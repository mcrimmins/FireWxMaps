# ==============================================================================
# CPC Global Daily Precipitation Downloader
#
# Purpose:
#   Download and subset CPC Global Unified Gauge-Based daily precipitation
#   data from NOAA PSL using the THREDDS NetCDF Subset Service (NCSS).
#
# Features:
#   - Server-side spatial subsetting
#   - Automatic retry/backoff for HTTP 429 and temporary server errors
#   - Local annual NetCDF caching
#   - Restartable downloads
#   - Combines annual files into a terra SpatRaster
#   - Writes a single multi-layer GeoTIFF
#
# Data:
#   CPC Global Unified Gauge-Based Analysis of Daily Precipitation
#   Variable: precip
#   Units: mm/day
#   Resolution: 0.5 degree
#
# R packages:
#   terra, curl
#
# Author: MAC
# Created: 2025-02-09
# Revised: 2026-08-18
# ==============================================================================

library(terra)
library(curl)

get_cpc_precip <- function(
    years = 1979:2023,
    bbox = c(-140, -60, 20, 60),   # xmin, xmax, ymin, ymax
    cache_dir = "./Data/CPC/cache",
    output_file = NULL,
    overwrite_cache = FALSE,
    pause = 2,
    max_tries = 6,
    initial_backoff = 5,
    verbose = TRUE
) {
  
  # --------------------------------------------------------------------------
  # Basic checks
  # --------------------------------------------------------------------------
  
  if (length(bbox) != 4)
    stop("bbox must be c(xmin, xmax, ymin, ymax).")
  
  if (!is.numeric(years) || length(years) == 0)
    stop("years must contain at least one year.")
  
  xmin <- bbox[1]
  xmax <- bbox[2]
  ymin <- bbox[3]
  ymax <- bbox[4]
  
  if (xmin >= xmax)
    stop("bbox xmin must be less than xmax.")
  
  if (ymin >= ymax)
    stop("bbox ymin must be less than ymax.")
  
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  
  # --------------------------------------------------------------------------
  # Internal function: download with retry/backoff
  # --------------------------------------------------------------------------
  
  download_retry <- function(url, destfile) {
    
    transient_codes <- c(408, 425, 429, 500, 502, 503, 504)
    partfile <- paste0(destfile, ".part")
    
    for (attempt in seq_len(max_tries)) {
      
      if (file.exists(partfile))
        unlink(partfile)
      
      result <- tryCatch({
        h <- curl::new_handle()
        
        curl::handle_setopt(
          h,
          followlocation = TRUE,
          timeout = 600,
          connecttimeout = 60
        )
        
        curl::curl_fetch_disk(
          url,
          partfile,
          handle = h
        )
      }, error = function(e) e)
      
      # Successful request
      if (!inherits(result, "error") &&
          result$status_code >= 200 &&
          result$status_code < 300) {
        
        if (file.exists(destfile))
          unlink(destfile)
        
        moved <- file.rename(partfile, destfile)
        
        if (!moved) {
          file.copy(partfile, destfile, overwrite = TRUE)
          unlink(partfile)
        }
        
        return(invisible(TRUE))
      }
      
      # Determine failure type
      if (inherits(result, "error")) {
        status <- NA_integer_
        error_msg <- conditionMessage(result)
      } else {
        status <- result$status_code
        error_msg <- paste("HTTP status", status)
      }
      
      # Stop immediately for non-transient HTTP errors
      if (!is.na(status) && !status %in% transient_codes) {
        unlink(partfile)
        stop("Download failed: ", error_msg)
      }
      
      # Stop after final retry
      if (attempt == max_tries) {
        unlink(partfile)
        stop(
          "Download failed after ",
          max_tries,
          " attempts: ",
          error_msg
        )
      }
      
      # Exponential backoff plus random jitter
      wait <- min(initial_backoff * 2^(attempt - 1), 120) +
        runif(1, 0, 2)
      
      if (verbose) {
        message(
          sprintf(
            "  Request failed (%s). Retrying in %.1f seconds...",
            error_msg,
            wait
          )
        )
      }
      
      Sys.sleep(wait)
    }
  }
  
  # --------------------------------------------------------------------------
  # Internal function: process one year
  # --------------------------------------------------------------------------
  
  get_one_year <- function(year) {
    
    if (verbose)
      message("Processing ", year, "...")
    
    base_url <- sprintf(
      paste0(
        "https://psl.noaa.gov/thredds/ncss/grid/",
        "Datasets/cpc_global_precip/precip.%d.nc"
      ),
      year
    )
    
    start_date <- sprintf("%04d-01-01T00:00:00Z", year)
    end_date <- sprintf("%04d-12-31T00:00:00Z", year)
    
    query <- c(
      var = "precip",
      north = ymax,
      south = ymin,
      west = xmin,
      east = xmax,
      horizStride = 1,
      time_start = start_date,
      time_end = end_date,
      timeStride = 1,
      addLatLon = "false",
      accept = "netcdf4"
    )
    
    query_string <- paste(
      names(query),
      vapply(
        as.character(query),
        curl::curl_escape,
        character(1)
      ),
      sep = "=",
      collapse = "&"
    )
    
    url <- paste0(base_url, "?", query_string)
    
    nc_file <- file.path(
      cache_dir,
      sprintf("CPC_precip_%d.nc", year)
    )
    
    # Download only if file is missing or overwrite requested
    if (!file.exists(nc_file) || overwrite_cache) {
      
      if (verbose)
        message("  Downloading subset...")
      
      download_retry(url, nc_file)
      Sys.sleep(pause)
      
    } else {
      
      if (verbose)
        message("  Using cached file.")
    }
    
    # Read annual NetCDF
    r <- tryCatch(
      terra::rast(nc_file, subds = "precip"),
      error = function(e) {
        stop(
          "Unable to read cached file ",
          nc_file,
          ": ",
          conditionMessage(e)
        )
      }
    )
    
    # Convert 0-360 longitude to -180 to 180 if necessary
    if (terra::xmin(r) >= 180) {
      
      e <- terra::ext(r)
      
      terra::ext(r) <- terra::ext(
        terra::xmin(e) - 360,
        terra::xmax(e) - 360,
        terra::ymin(e),
        terra::ymax(e)
      )
      
    } else if (terra::xmax(r) > 180) {
      
      r <- terra::rotate(r)
    }
    
    # Check expected number of daily layers
    expected_days <- as.integer(
      as.Date(sprintf("%04d-12-31", year)) -
        as.Date(sprintf("%04d-01-01", year))
    ) + 1
    
    if (terra::nlyr(r) != expected_days) {
      warning(
        "Year ",
        year,
        " contains ",
        terra::nlyr(r),
        " layers; expected ",
        expected_days,
        "."
      )
    }
    
    if (verbose)
      message("  ", terra::nlyr(r), " daily time steps retrieved.")
    
    return(r)
  }
  
  # --------------------------------------------------------------------------
  # Process all years
  # --------------------------------------------------------------------------
  
  results <- vector("list", length(years))
  names(results) <- years
  
  for (i in seq_along(years)) {
    
    yr <- years[i]
    
    results[[i]] <- tryCatch(
      get_one_year(yr),
      error = function(e) {
        warning(
          "Year ",
          yr,
          " failed: ",
          conditionMessage(e)
        )
        NULL
      }
    )
  }
  
  # --------------------------------------------------------------------------
  # Identify successful/failed years
  # --------------------------------------------------------------------------
  
  successful <- !vapply(results, is.null, logical(1))
  
  if (!any(successful))
    stop("No years were successfully retrieved.")
  
  failed_years <- years[!successful]
  
  if (length(failed_years) > 0 && verbose)
    message("Failed years: ", paste(failed_years, collapse = ", "))
  
  # --------------------------------------------------------------------------
  # Combine annual rasters
  # --------------------------------------------------------------------------
  
  good_results <- results[successful]
  r <- good_results[[1]]
  
  if (length(good_results) > 1) {
    for (i in 2:length(good_results)) {
      r <- c(r, good_results[[i]])
    }
  }
  
  if (!inherits(r, "SpatRaster"))
    stop("Annual results did not combine into a SpatRaster.")
  
  # --------------------------------------------------------------------------
  # Sort layers chronologically
  # --------------------------------------------------------------------------
  
  tt <- terra::time(r)
  
  if (length(tt) == terra::nlyr(r) && !all(is.na(tt)))
    r <- r[[order(tt)]]
  
  attr(r, "failed_years") <- failed_years
  
  # --------------------------------------------------------------------------
  # Summary
  # --------------------------------------------------------------------------
  
  if (verbose) {
    
    message(
      "Combined ",
      sum(successful),
      " years into ",
      terra::nlyr(r),
      " daily layers."
    )
    
    tt <- terra::time(r)
    
    if (length(tt) == terra::nlyr(r) && !all(is.na(tt))) {
      message(
        "Time range: ",
        min(tt, na.rm = TRUE),
        " to ",
        max(tt, na.rm = TRUE)
      )
    }
  }
  
  # --------------------------------------------------------------------------
  # Write combined GeoTIFF
  # --------------------------------------------------------------------------
  
  if (!is.null(output_file)) {
    
    dir.create(
      dirname(output_file),
      recursive = TRUE,
      showWarnings = FALSE
    )
    
    if (verbose)
      message("Writing GeoTIFF: ", output_file)
    
    terra::writeRaster(
      r,
      output_file,
      filetype = "GTiff",
      overwrite = TRUE
    )
    
    if (verbose)
      message("Saved: ", output_file)
  }
  
  return(r)
}


# ==============================================================================
# Download CPC precipitation
# ==============================================================================

cpc_precip <- get_cpc_precip(
  years = 1979:2024,
  bbox = c(-140, -60, 20, 60),
  cache_dir = "./Data/CPC/cache",
  output_file = "./Data/CPC/CPC_Global_precip_1979_2024_CONUS.tif",
  pause = 2,
  max_tries = 6
)


# ==============================================================================
# Check result
# ==============================================================================

cpc_precip
terra::nlyr(cpc_precip)
head(terra::time(cpc_precip))
tail(terra::time(cpc_precip))
attr(cpc_precip, "failed_years")