#' Hash date time
#'
#' Hash dates according to the given temporal resolution.
#'
#' @param dates Date objects to be binned.
#' @param resolution Temporal resolution to be used. Either one of the following
#'   string literals: \code{"day"}, \code{"week"}, \code{"month"}, \code{"year"}
#'   or a date time format string e.g. \code{"\%Y-\%m-\%d"}.
#' @return character vector with unique characters for each bin.
hash_datetime <- function(dates, resolution = "week") {
  if(is.null(dates)) {
    return(NULL)
  }
  if(!inherits(dates, 'Date') && !inherits(dates, 'POSIXct')) {
    stop("dates should be valid Date objects")
  }

  formats <- list(day = "%Y-%m-%d", month = "%Y-%m", year = "%Y")
  fmtstring <- formats[[resolution]]
  if(resolution == "week") {
    # create week bins that work with the end/beginning of the year ("%Y-%V" does not work)
    refdate <- as.Date("0000-01-03") ## Monday 3 Jan in year 0 => reference day
    if(!all(dates >= refdate)) {
      stop("Some dates are before 0000-01-03")
    }
    bins <- as.character(floor(difftime(dates, refdate, units = "days") / 7))
  } else {
    if(is.null(fmtstring)) {
      fmtstring <- resolution
    }
    bins <- (format(dates, fmtstring))
    if(all(bins == sub('%', '', fmtstring))) {
      stop('Invalid datetime resolution')
    }
  }
  bins
}


#' Bin tracking data
#'
#' Aggregate tracking data with a specific spatial and temporal resolution
#'
#' @param data Tracking data to be aggregated with the columns
#'   \code{"organismID"}, \code{"decimalLongitude"}, \code{"decimalLatitude"},
#'   \code{"eventDate"} and optionally \code{"detections"}.
#' @param spatial Spatial resolution in meters to be used to aggregate the data
#'   (default \code{1000}).
#' @param temporal Temporal resolution to be used to aggregate the data (default
#'   \code{"week"}).
#' @return Data frame with columns \code{"organismID"}, \code{"decimalLongitude"},
#'   \code{"decimalLatitude"}, \code{"eventDateArrival"}, \code{"eventDateDeparture"}
#'   and \code{"detections"}.
#' @details The aggregation is done in a spatiotemporal grid based on the
#'   provided spatial and temporal resolution. The spatial aggregation is done
#'   on a Behrmann equal area grid with as cell size the specified spatial
#'   resolution in meters. The temporal aggregation accepts either on of the
#'   following string literals: \code{"day"}, \code{"week"}, \code{"month"},
#'   \code{"year"} or a date time format string e.g. \code{"\%Y-\%m-\%d"}
#' @export
bin_tracking_data <- function(data, spatial = 1000, temporal = 'week') {
  lonlat <- sp::CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')
  equalarea <- sp::CRS('+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0')

  if(!all(c('organismID', 'decimalLongitude', 'decimalLatitude', 'eventDate') %in% colnames(data))) {
    stop("The columns organismID, decimalLongitude, decimalLatitude and eventDate are mandatory")
  }

  if((!inherits(data$eventDate, 'Date') && !inherits(data$eventDate, 'POSIXct')) || anyNA(data$eventDate) || is.null(data$eventDate)) {
    stop("eventDate values should be valid Date objects")
  }

  if (!"detections" %in% names(data)) {
    data$detections <- rep(1, nrow(data))
  }

  grid <- raster::raster(xmn = -17367530, xmx = 17367530, ymn = -7342230, ymx = 7342230, crs = equalarea, resolution = spatial)
  pts <- sf::sf_project(lonlat@projargs, equalarea@projargs, as.matrix(data[,c("decimalLongitude", "decimalLatitude")]))
  cells <- raster::cellFromXY(grid, pts)
  data$ziptrackOrder <- seq_len(NROW(data))
  bins <- paste(cells, hash_datetime(data$eventDate, temporal))
  bin_groups <- rle(bins)
  bin_groups$values <- paste(bin_groups$values, seq_len(length(bin_groups$values)))
  bins <- inverse.rle(bin_groups)
  data %>%
    mutate(bins) %>%
    group_by(organismID, bins) %>%
    arrange(organismID, ziptrackOrder) %>%
    summarise(
      decimalLongitude = first(decimalLongitude),
      decimalLatitude = first(decimalLatitude),
      eventDateArrival = min(eventDate),
      eventDateDeparture = max(eventDate),
      detections = sum(detections)) %>%
    ungroup() %>%
    select_(., .dots = c("organismID", "decimalLongitude", "decimalLatitude", "eventDateArrival", "eventDateDeparture", "detections")) %>%
    arrange(organismID, eventDateArrival, eventDateDeparture)
}
