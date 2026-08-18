# ==============================================================================
# NCEP/DOE Reanalysis 2 Pressure-Level Data Downloader
#
# Purpose:
#   Download and subset NCEP/DOE Reanalysis 2 pressure-level data from the
#   NOAA Physical Sciences Laboratory (PSL) THREDDS server using the NetCDF
#   Subset Service (NCSS).
#
#   The workflow retrieves a user-defined:
#     - variable (e.g., geopotential height, relative humidity)
#     - pressure level
#     - UTC observation hour
#     - geographic bounding box
#     - range of years
#
#   Annual subsets are cached locally, combined into a terra SpatRaster,
#   and may optionally be written to NetCDF or another raster format.
#
# Features:
#   - Server-side spatial, vertical, and temporal subsetting
#   - Automatic retry with exponential backoff for HTTP 429/5xx errors
#   - Local caching to avoid repeated downloads
#   - Selection of pressure levels by value rather than array index
#   - Daily extraction from 6-hourly data at a specified UTC hour
#   - Automatic combination of annual files into a single time series
#
# Data source:
#   NOAA Physical Sciences Laboratory (PSL)
#   NCEP/DOE Reanalysis 2, pressure-level data
#   https://psl.noaa.gov/data/gridded/data.ncep.reanalysis2.pressure.html
#
# R packages:
#   terra
#   curl
#
# Example:
#   Retrieve daily 500-hPa geopotential height at 18 UTC for 1992-2024:
#
#   hgt500 <- get_r2_pressure(
#     var = "hgt",
#     years = 1992:2024,
#     level = 500,
#     hour_utc = 18,
#     bbox = c(-140, -60, 20, 60)
#   )
#
# Notes:
#   - This script accesses NCEP/DOE Reanalysis 2, not NARR.
#   - Pressure levels are specified directly in hPa (e.g., 500, 700).
#   - Valid synoptic hours for these 6-hourly files are 00, 06, 12, and 18 UTC.
#   - Local caching is recommended to minimize load on the PSL server.
#
# Author: MAC/ ChatGPT-High
# Created: 2025-02-22
# Revised: 2026-08-18
# ==============================================================================




library(terra)
library(curl)

get_r2_pressure <- function(
    var = "hgt",
    years = 1992:2024,
    level = 500,
    hour_utc = 18,
    bbox = c(-140, -60, 20, 60),   # xmin, xmax, ymin, ymax
    cache_dir = "./Data/Reanalysis2/cache",
    output_file = NULL,
    overwrite_cache = FALSE,
    pause = 2,
    max_tries = 6,
    initial_backoff = 5,
    verbose = TRUE
) {
  
  # ------------------------------------------------------------
  # Basic checks
  # ------------------------------------------------------------
  
  if (!hour_utc %in% c(0, 6, 12, 18)) {
    stop("hour_utc must be one of 0, 6, 12, or 18.")
  }
  
  if (length(bbox) != 4) {
    stop("bbox must be c(xmin, xmax, ymin, ymax).")
  }
  
  xmin <- bbox[1]
  xmax <- bbox[2]
  ymin <- bbox[3]
  ymax <- bbox[4]
  
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  
  # ------------------------------------------------------------
  # HTTP download with retry/backoff
  # ------------------------------------------------------------
  
  download_retry <- function(url, destfile) {
    
    transient_codes <- c(
      408, 425, 429,
      500, 502, 503, 504
    )
    
    partfile <- paste0(destfile, ".part")
    
    for (attempt in seq_len(max_tries)) {
      
      unlink(partfile)
      
      result <- tryCatch({
        
        h <- curl::new_handle()
        
        curl::handle_setopt(
          h,
          followlocation = TRUE,
          timeout = 600
        )
        
        curl::curl_fetch_disk(
          url,
          partfile,
          handle = h
        )
        
      }, error = function(e) e)
      
      # Successful HTTP request
      if (!inherits(result, "error") &&
          result$status_code >= 200 &&
          result$status_code < 300) {
        
        if (!file.rename(partfile, destfile)) {
          file.copy(
            partfile,
            destfile,
            overwrite = TRUE
          )
          unlink(partfile)
        }
        
        return(invisible(TRUE))
      }
      
      # Get error information
      if (inherits(result, "error")) {
        status <- NA_integer_
        error_msg <- conditionMessage(result)
      } else {
        status <- result$status_code
        error_msg <- paste("HTTP status", status)
      }
      
      # Do not retry non-transient HTTP errors
      if (!is.na(status) &&
          !status %in% transient_codes) {
        
        unlink(partfile)
        
        stop(
          "Download failed: ",
          error_msg
        )
      }
      
      # Final attempt failed
      if (attempt == max_tries) {
        
        unlink(partfile)
        
        stop(
          "Download failed after ",
          max_tries,
          " attempts: ",
          error_msg
        )
      }
      
      # Exponential backoff + random jitter
      wait <- min(
        initial_backoff * 2^(attempt - 1),
        120
      ) + runif(1, 0, 2)
      
      if (verbose) {
        message(
          sprintf(
            "Request failed (%s). Retrying in %.1f seconds...",
            error_msg,
            wait
          )
        )
      }
      
      Sys.sleep(wait)
    }
  }
  
  
  # ------------------------------------------------------------
  # Download/process one year
  # ------------------------------------------------------------
  
  get_one_year <- function(year) {
    
    if (verbose) {
      message("Processing ", year, "...")
    }
    
    # NCSS endpoint
    base_url <- sprintf(
      paste0(
        "https://psl.noaa.gov/thredds/ncss/grid/",
        "Datasets/ncep.reanalysis2/pressure/%s.%d.nc"
      ),
      var,
      year
    )
    
    # Start at requested UTC hour.
    # timeStride = 4 selects every fourth 6-hour time step,
    # giving one observation per day.
    query <- c(
      var = var,
      north = ymax,
      south = ymin,
      west = xmin,
      east = xmax,
      horizStride = 1,
      time_start = sprintf(
        "%04d-01-01T%02d:00:00Z",
        year,
        hour_utc
      ),
      time_end = sprintf(
        "%04d-12-31T%02d:00:00Z",
        year,
        hour_utc
      ),
      timeStride = 4,
      vertCoord = level,
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
    
    url <- paste0(
      base_url,
      "?",
      query_string
    )
    
    # Local cached file
    nc_file <- file.path(
      cache_dir,
      sprintf(
        "R2_%s_%dhPa_%02dZ_%d.nc",
        var,
        level,
        hour_utc,
        year
      )
    )
    
    # Only download if necessary
    if (!file.exists(nc_file) || overwrite_cache) {
      
      if (verbose) {
        message("  Downloading subset...")
      }
      
      download_retry(
        url,
        nc_file
      )
      
      # Deliberately avoid hammering PSL
      Sys.sleep(pause)
      
    } else {
      
      if (verbose) {
        message("  Using cached file.")
      }
    }
    
    
    # ----------------------------------------------------------
    # Read directly as SpatRaster
    # ----------------------------------------------------------
    
    r <- terra::rast(
      nc_file,
      subds = var
    )
    
    # Remove singleton vertical dimension.
    # Pressure level is already known from the function argument.
    terra::depth(r) <- NULL
    
    # ----------------------------------------------------------
    # Convert 0-360 longitude if necessary
    # ----------------------------------------------------------
    
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
    
    if (verbose) {
      message(
        "  ",
        terra::nlyr(r),
        " time steps retrieved."
      )
    }
    
    r
  }
  
  
  # ------------------------------------------------------------
  # Run all years
  # ------------------------------------------------------------
  
  results <- vector(
    "list",
    length(years)
  )
  
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
  
  
  # ------------------------------------------------------------
  # Identify failures
  # ------------------------------------------------------------
  
  successful <- !vapply(
    results,
    is.null,
    logical(1)
  )
  
  if (!any(successful)) {
    stop("No years were successfully retrieved.")
  }
  
  failed_years <- years[!successful]
  
  
  # ------------------------------------------------------------
  # Combine years
  # ------------------------------------------------------------
  
  good_results <- results[successful]
  
  # Start with first successful year
  r <- good_results[[1]]
  
  # Append remaining years
  if (length(good_results) > 1) {
    for (i in 2:length(good_results)) {
      r <- c(r, good_results[[i]])
    }
  }
  
  # Sort chronologically if time metadata are available
  tt <- terra::time(r)
  
  if (length(tt) == terra::nlyr(r) &&
      !all(is.na(tt))) {
    r <- r[[order(tt)]]
  }
  
  # Store failed years
  attr(r, "failed_years") <- failed_years
  
  
  # ------------------------------------------------------------
  # Save combined raster
  # ------------------------------------------------------------
  
  if (!is.null(output_file)) {
    
    dir.create(
      dirname(output_file),
      recursive = TRUE,
      showWarnings = FALSE
    )
    
    terra::writeRaster(
      r,
      output_file,
      filetype = "GTiff",
      overwrite = TRUE
    )
    
    if (verbose) {
      message("Saved: ", output_file)
    }
  }
  
  return(r)
}
###############################



###### EXAMPLES #################

hgt500 <- get_r2_pressure(
  var = "hgt",
  years = 1992:2024,
  level = 500,
  hour_utc = 18,
  bbox = c(-140, -60, 20, 60),
  cache_dir = "./Data/Reanalysis2/cache",
  output_file = "./Data/R2_hgt_500mb_1992_2024_CONUS.tif"
)

hgt700 <- get_r2_pressure(
  var = "hgt",
  years = 1992:2024,
  level = 700,
  hour_utc = 18,
  bbox = c(-140, -60, 20, 60),
  cache_dir = "./Data/Reanalysis2/cache",
  output_file = "./Data/R2_hgt_700mb_1992_2024_CONUS.tif"
)

hgt1000 <- get_r2_pressure(
  var = "hgt",
  years = 1992:2024,
  level = 1000,
  hour_utc = 18,
  bbox = c(-140, -60, 20, 60),
  cache_dir = "./Data/Reanalysis2/cache",
  output_file = "./Data/R2_hgt_1000mb_1992_2024_CONUS.tif"
)











