# ==============================================================================
# CPC Precipitation Percent-of-Average Calculator
#
# Purpose:
#   Calculate rolling precipitation totals and express them as percent of the
#   climatological average for each calendar day.
#
# Example:
#     30-day percent of average on March 15 =
#
#       30-day precip ending March 15
#       -------------------------------- * 100
#       mean 30-day precip ending March 15
#
# Features:
#   - Works directly with CPC daily precipitation GeoTIFFs
#   - User-defined rolling period (e.g., 14, 30, 60, 90 days)
#   - User-defined climatology period
#   - Handles leap years using month-day climatology groups
#   - Optional output-period subsetting
#   - Checks for continuous daily time series
#   - Masks percent-of-average where climatological precipitation is very low
#   - Masks extreme percent-of-average values
#   - Stores whole percentages as compact 12-bit unsigned integers
#   - Uses lossless ZSTD GeoTIFF compression
#
# R package:
#   terra
#
# Author: MAC
# Revised: 2026-08-18
# ==============================================================================

library(terra)

terraOptions(progress = 1)

calc_cpc_percent_avg <- function(
    precip,
    rolling_window = 30,
    climatology_period = NULL,
    output_period = NULL,
    output_file = NULL,
    start_date = NULL,
    min_clim_precip = 1,
    max_percent = 4094,
    overwrite = TRUE,
    verbose = TRUE
) {
  
  # --------------------------------------------------------------------------
  # Load precipitation
  # --------------------------------------------------------------------------
  
  if (inherits(precip, "character")) {
    if (!file.exists(precip))
      stop("Precipitation file does not exist: ", precip)
    r <- terra::rast(precip)
  } else if (inherits(precip, "SpatRaster")) {
    r <- precip
  } else {
    stop("precip must be a filename or SpatRaster.")
  }
  
  # --------------------------------------------------------------------------
  # Validate arguments
  # --------------------------------------------------------------------------
  
  if (length(rolling_window) != 1 ||
      !is.numeric(rolling_window) ||
      rolling_window < 2 ||
      rolling_window != round(rolling_window))
    stop("rolling_window must be an integer greater than 1.")
  
  rolling_window <- as.integer(rolling_window)
  
  if (terra::nlyr(r) < rolling_window)
    stop("Raster has fewer layers than the requested rolling window.")
  
  if (length(min_clim_precip) != 1 ||
      !is.numeric(min_clim_precip) ||
      is.na(min_clim_precip) ||
      min_clim_precip < 0)
    stop("min_clim_precip must be a non-negative number.")
  
  if (length(max_percent) != 1 ||
      !is.numeric(max_percent) ||
      is.na(max_percent) ||
      max_percent < 1 ||
      max_percent > 4094)
    stop("max_percent must be between 1 and 4094.")
  
  max_percent <- as.integer(max_percent)
  
  # --------------------------------------------------------------------------
  # Check time metadata
  # --------------------------------------------------------------------------
  
  tt <- terra::time(r)
  
  if (inherits(tt, "Date")) {
    dates <- tt
  } else if (inherits(tt, c("POSIXct", "POSIXlt"))) {
    dates <- as.Date(tt, tz = "UTC")
  } else if (!is.null(start_date)) {
    start_date <- as.Date(start_date)
    
    if (is.na(start_date))
      stop("Invalid start_date.")
    
    dates <- seq(
      start_date,
      by = "day",
      length.out = terra::nlyr(r)
    )
    
    terra::time(r) <- dates
  } else {
    stop(
      "Raster does not contain valid date metadata. ",
      "Supply start_date, e.g. start_date = '1979-01-01'."
    )
  }
  
  if (length(dates) != terra::nlyr(r) || anyNA(dates))
    stop("Invalid or incomplete time metadata.")
  
  # --------------------------------------------------------------------------
  # Sort chronologically and check time series
  # --------------------------------------------------------------------------
  
  ord <- order(dates)
  
  if (!identical(ord, seq_along(dates))) {
    r <- r[[ord]]
    dates <- dates[ord]
    terra::time(r) <- dates
  }
  
  if (anyDuplicated(dates))
    stop("Duplicate dates were found in the precipitation raster.")
  
  gaps <- which(as.integer(diff(dates)) != 1)
  
  if (length(gaps) > 0) {
    stop(
      "The precipitation time series is not continuous daily data. ",
      "First gap occurs between ",
      dates[gaps[1]],
      " and ",
      dates[gaps[1] + 1],
      "."
    )
  }
  
  if (verbose) {
    message("Input layers: ", terra::nlyr(r))
    message("Input period: ", min(dates), " to ", max(dates))
    message("Rolling window: ", rolling_window, " days")
  }
  
  # --------------------------------------------------------------------------
  # Calculate trailing rolling precipitation totals
  # --------------------------------------------------------------------------
  
  if (verbose)
    message("Calculating ", rolling_window, "-day rolling precipitation...")
  
  r_rolling <- terra::roll(
    r,
    n = rolling_window,
    fun = "sum",
    type = "to",
    circular = FALSE,
    na.rm = FALSE
  )
  
  terra::time(r_rolling) <- dates
  
  # First n-1 dates do not have a complete rolling window
  complete_window <- seq_along(dates) >= rolling_window
  
  # --------------------------------------------------------------------------
  # Define climatology period
  # --------------------------------------------------------------------------
  
  if (is.null(climatology_period)) {
    clim_idx <- which(complete_window)
  } else {
    if (length(climatology_period) != 2)
      stop("climatology_period must contain start and end dates.")
    
    clim_period <- as.Date(climatology_period)
    
    if (anyNA(clim_period))
      stop("Invalid climatology_period.")
    
    if (clim_period[1] > clim_period[2])
      stop("Climatology start date must precede end date.")
    
    clim_idx <- which(
      dates >= clim_period[1] &
        dates <= clim_period[2] &
        complete_window
    )
  }
  
  if (length(clim_idx) == 0)
    stop("No dates fall within the requested climatology period.")
  
  if (verbose) {
    message(
      "Climatology period: ",
      min(dates[clim_idx]),
      " to ",
      max(dates[clim_idx])
    )
  }
  
  # --------------------------------------------------------------------------
  # Calculate climatological mean by calendar day
  #
  # Month-day is used instead of numeric day-of-year so dates after Feb 28
  # remain aligned correctly between leap and non-leap years.
  # --------------------------------------------------------------------------
  
  calendar_keys <- format(
    seq(
      as.Date("2000-01-01"),
      as.Date("2000-12-31"),
      by = "day"
    ),
    "%m-%d"
  )
  
  clim_keys <- format(dates[clim_idx], "%m-%d")
  
  clim_calendar_index <- match(
    clim_keys,
    calendar_keys
  )
  
  if (verbose)
    message("Calculating climatological rolling precipitation means...")
  
  clim_mean <- terra::tapp(
    r_rolling[[clim_idx]],
    index = clim_calendar_index,
    fun = "mean",
    na.rm = TRUE
  )
  
  present_index <- sort(unique(clim_calendar_index))
  present_keys <- calendar_keys[present_index]
  
  if (terra::nlyr(clim_mean) != length(present_keys)) {
    stop(
      "Unexpected climatology layer count. Expected ",
      length(present_keys),
      " but received ",
      terra::nlyr(clim_mean),
      "."
    )
  }
  
  names(clim_mean) <- present_keys
  
  # --------------------------------------------------------------------------
  # Define requested output period
  # --------------------------------------------------------------------------
  
  if (is.null(output_period)) {
    out_idx <- seq_along(dates)
  } else {
    if (length(output_period) != 2)
      stop("output_period must contain start and end dates.")
    
    out_period <- as.Date(output_period)
    
    if (anyNA(out_period))
      stop("Invalid output_period.")
    
    if (out_period[1] > out_period[2])
      stop("Output start date must precede end date.")
    
    out_idx <- which(
      dates >= out_period[1] &
        dates <= out_period[2]
    )
  }
  
  if (length(out_idx) == 0)
    stop("No dates fall within the requested output period.")
  
  out_dates <- dates[out_idx]
  out_keys <- format(out_dates, "%m-%d")
  
  # --------------------------------------------------------------------------
  # Match output dates to climatological calendar days
  # --------------------------------------------------------------------------
  
  clim_match <- match(
    out_keys,
    present_keys
  )
  
  if (anyNA(clim_match)) {
    missing_keys <- unique(out_keys[is.na(clim_match)])
    
    stop(
      "The climatology does not contain the following calendar dates: ",
      paste(missing_keys, collapse = ", "),
      ". The climatology should normally include at least one leap year."
    )
  }
  
  # --------------------------------------------------------------------------
  # Calculate percent of average
  # --------------------------------------------------------------------------
  
  if (verbose)
    message("Calculating percent of average...")
  
  r_out <- r_rolling[[out_idx]]
  clim_daily <- clim_mean[[clim_match]]
  
  # Only calculate percent-of-average where the climatological rolling
  # precipitation total is at least min_clim_precip mm.
  percent_of_avg <- terra::ifel(
    clim_daily >= min_clim_precip,
    (r_out / clim_daily) * 100,
    NA
  )
  
  terra::time(percent_of_avg) <- out_dates
  
  names(percent_of_avg) <- paste0(
    "pctavg_",
    format(out_dates, "%Y%m%d")
  )
  
  # Explicitly set incomplete initial rolling periods to NA
  incomplete <- which(out_idx < rolling_window)
  
  if (length(incomplete) > 0)
    percent_of_avg[[incomplete]] <- NA
  
  # --------------------------------------------------------------------------
  # Convert to compact whole-percent product
  # --------------------------------------------------------------------------
  
  percent_of_avg <- round(percent_of_avg)
  
  # Mask extreme ratios so output remains within 12-bit integer range.
  # 0-4094 are data values; 4095 is reserved for NoData.
  percent_of_avg <- terra::ifel(
    percent_of_avg >= 0 &
      percent_of_avg <= max_percent,
    percent_of_avg,
    NA
  )
  
  if (verbose) {
    message("Output layers: ", terra::nlyr(percent_of_avg))
    message("Output period: ", min(out_dates), " to ", max(out_dates))
    message("Minimum climatological precipitation: ", min_clim_precip, " mm")
    message("Maximum retained percent of average: ", max_percent, "%")
  }
  
  # --------------------------------------------------------------------------
  # Write compact 12-bit GeoTIFF
  # --------------------------------------------------------------------------
  
  if (!is.null(output_file)) {
    dir.create(
      dirname(output_file),
      recursive = TRUE,
      showWarnings = FALSE
    )
    
    if (verbose)
      message("Writing compact 12-bit GeoTIFF: ", output_file)
    
    terra::writeRaster(
      percent_of_avg,
      output_file,
      filetype = "GTiff",
      datatype = "INT2U",
      NAflag = 4095,
      gdal = c(
        "NBITS=12",
        "COMPRESS=ZSTD",
        "ZSTD_LEVEL=22",
        "BIGTIFF=IF_SAFER"
      ),
      overwrite = overwrite
    )
    
    if (verbose)
      message("Saved: ", output_file)
  }
  
  return(percent_of_avg)
}


# ==============================================================================
# EXAMPLE: 14-day percent of average
# ==============================================================================

pct14 <- calc_cpc_percent_avg(
  precip = "./Data/CPC/CPC_Global_precip_1979_2024_CONUS.tif",
  rolling_window = 14,
  climatology_period = c("1979-01-01", "2024-12-31"),
  output_period = c("1992-01-01", "2024-12-31"),
  min_clim_precip = 1,
  max_percent = 4094,
  output_file = "./Data/CPC_Global_precip_14dyPercAvg_1992_2024_CONUS.tif"
)


pct90 <- calc_cpc_percent_avg(
  precip = "./Data/CPC/CPC_Global_precip_1979_2024_CONUS.tif",
  rolling_window = 90,
  climatology_period = c("1979-01-01", "2024-12-31"),
  output_period = c("1992-01-01", "2024-12-31"),
  min_clim_precip = 1,
  max_percent = 4094,
  output_file = "./Data/CPC_Global_precip_90dyPercAvg_1992_2024_CONUS.tif"
)


