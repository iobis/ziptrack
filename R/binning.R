#'
#' @export
bin_tracking_data <- function(data, resolution = 1000, temporal = "%Y-%m-%d") {
  lonlat <- CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')
  equalarea <- CRS('+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0')

  if (!"detections" %in% names(data)) {
    data$detections <- rep(1, nrow(data))
  }

  grid <- raster(xmn = -17367530, xmx = 17367530, ymn = -7342230, ymx = 7342230, crs = equalarea, resolution = resolution)
  pts <- sf::sf_project(lonlat@projargs, equalarea@projargs, as.matrix(data[,c("decimalLongitude", "decimalLatitude")]))
  cells <- cellFromXY(grid, pts)
  dates <- lubridate::as_datetime(data$eventDate)
  data %>%
    mutate(
      cells,
      time = format(dates, temporal)) %>%
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
