#' Hash date time
#'
#' Hash dates according to the given temporal resolution.
#'
#' @param dates Date objects to be binned.
#' @param resolution Temporal resolution to be used. Either one of the following
#'   string literals: \code{"day"}, \code{"week"}, \code{"month"}, \code{"year"}
#'   or a date time format string e.g. \code{"\%Y-\%m-\%d"}.
#' @return character vector with a unique characters for each bin.
hash_datetime <- function(dates, resolution = "week") {
  formats <- list(day = "%Y-%m-%d", month = "%Y-%m", year = "%Y")
  fmtstring <- formats[[resolution]]
  if(resolution == "week") {
    # create week bins that work with the end/beginning of the year ("%Y-%V" does not work)
    refdate <- lubridate::as_date("0000-01-03") ## Monday 3 Jan in year 0 => reference day
    if(any(dates <= refdate)) {
      stop("Some dates are before 0000-01-04")
    }
    bins <- as.character(floor(difftime(dates, refdate,units = "days") / 7))
  } else {
    if(is.null(fmtstring)) {
      fmtstring <- resolution
    }
    bins <- (format(dates, fmtstring))
  }
  bins
}


#'
#' @export
bin_tracking_data <- function(data, spatial = 1000, temporal = "%Y-%V") {
  lonlat <- sp::CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')
  equalarea <- sp::CRS('+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0')

  if (!"detections" %in% names(data)) {
    data$detections <- rep(1, nrow(data))
  }

  grid <- raster::raster(xmn = -17367530, xmx = 17367530, ymn = -7342230, ymx = 7342230, crs = equalarea, resolution = spatial)
  pts <- sf::sf_project(lonlat@projargs, equalarea@projargs, as.matrix(data[,c("decimalLongitude", "decimalLatitude")]))
  cells <- raster::cellFromXY(grid, pts)
  dates <- lubridate::as_datetime(data$eventDate)
  data %>%
    mutate(
      cells,
      time = hash_datetime(dates, temporal)) %>%
    group_by(cells, time, organismID) %>%
    arrange(organismID, eventDate) %>%
    summarise(
      decimalLongitude = first(decimalLongitude),
      decimalLatitude = first(decimalLatitude),
      eventDateArrival = min(eventDate),
      eventDateDeparture = max(eventDate),
      detections = sum(detections)) %>%
    ungroup() %>%
      select_(., .dots = c("organismID", "decimalLongitude", "decimalLatitude", "eventDateArrival", "eventDateDeparture", "detections"))
}
