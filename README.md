# Binning and analysis of tracking data

The `bin_tracking_data` function allows you to combine multiple detections in the same spatial and temporal into one observation.

```R
d <- data.frame(organismID = "o1", decimalLongitude = c(0.00001, 0.00001), decimalLatitude = c(1.00001, 1.00001), eventDate = as.Date(c("2018-06-06", "2018-06-07")))
b <- bin_tracking_data(d, spatial = 1000, temporal = "week")
```
