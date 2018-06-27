
# based on a space-time gridding with a spatial resolution of 1km2 and a
# temporal resolution of 1 week. This means a new occurrence record should be
# created when an observed organism moves from one spatiotemporal cell to
# another. The arrival and departure date from this spatiotemporal cell will be
# saved as part of the aggregation. With respect to abiotic parameters, such as
# temperature, recorded together with the observations, the start and end values
# will be saved.


test_that("simple binning works without detections", {
  d <- data.frame(organismID='o1', decimalLongitude = c(0.00001, 0.00001), decimalLatitude = c(1.00001, 1.00001), eventDate = as.Date(c('2018-06-06', '2018-06-07')))
  b <- bin_tracking_data(d, spatial=1000, temporal="week")
  expect_equal(nrow(b), 1)
  expect_equal(b$detections, 2)
  expect_equal(b$organismID, as.factor('o1'))
  expect_equal(b$eventDateArrival, min(d$eventDate))
  expect_equal(b$eventDateDeparture, max(d$eventDate))
})

advanced_binning_test_data <- function() {
  # test data: organism1 3 times same cell then different cell 2 times then 1st cell 3 times then next week same cell
  # and organism2 records for the same period
  d <- data.frame(organismID='o1',
                  decimalLongitude = c(0.00001, 0.00002, 0.00003,
                                       0.50001, 0.50002,
                                       0.00001, 0.00002, 0.00003,
                                       0.00001),
                  decimalLatitude = c(1.00001, 1.00001, 1.00001,
                                      2.00001, 2.00001,
                                      1.00001, 1.00001, 1.00001,
                                      1.00001 ),
                  eventDate = as.Date(c('2018-06-05', '2018-06-06', '2018-06-06',
                                        '2018-06-06', '2018-06-06',
                                        '2018-06-07', '2018-06-07', '2018-06-07',
                                        '2018-06-12')))
  d <- rbind(d, data.frame(organismID='o2',
                           decimalLongitude = c(0.00002, 0.00001),
                           decimalLatitude = c(1.00001, 1.00002),
                           eventDate = as.Date(c('2018-06-06', '2018-06-07'))))
  d[order(d$eventDate),]
}

test_that("advanced binning works", {
  d <- advanced_binning_test_data()
  b <- bin_tracking_data(d, spatial=1000, temporal="week")
  expect_equal(nrow(b), 5)
  expect_equal(b$detections, c(3,2,3,1,2)) # first 4 are for organism 1, then 2 detections for organism 2

})

test_that("binning works with detections", {
  d <- advanced_binning_test_data()
  b1 <- bin_tracking_data(d, spatial=1000, temporal="week")
  d$detections <- rep(2, nrow(d))
  b2 <- bin_tracking_data(d, spatial=1000, temporal="week")
  expect_equal(b1$organismID, b2$organismID)
})

test_that("binning works with different spatial and temporal resolution", {
  d <- advanced_binning_test_data()
  b <- bin_tracking_data(d, spatial = 1000000, temporal="week")
  expect_equal(nrow(b), 3)
  b <- bin_tracking_data(d, spatial = 1000000, temporal="year")
  expect_equal(nrow(b), 2)
  b <- bin_tracking_data(d, spatial = 1000, temporal="year")
  expect_equal(nrow(b), 4)
  b <- bin_tracking_data(d, spatial = 1, temporal="day")
  expect_equal(nrow(b), nrow(d))
})

test_that("binning with invalid or missing eventDate fails", {
  d <- data.frame(organismID='o1', decimalLongitude = c(0.00001, 0.00001), decimalLatitude = c(1.00001, 1.00001), stringsAsFactors = FALSE)
  expect_error(bin_tracking_data(cbind(d, eventDate='2018-01-01')))
  expect_error(bin_tracking_data(cbind(d, eventDate=10102018)))
  expect_error(bin_tracking_data(cbind(d, eventDate=NA)))
})

test_that("hash datetime by day works", {
  dates <- lubridate::as_datetime(c('2018-01-01 12:33:44', '2018-01-01', '2017-12-31 12:23:00'))
  inherits(dates, 'Date')
  hash <- ziptrack:::hash_datetime(dates, resolution = "week")
  expect_equal(hash[1], hash[2])
  expect_false(hash[1] == hash[3])
})

test_that("hash datetime by month works", {
  dates <- lubridate::as_datetime(c('2018-01-01', '2018-01-08', '2018-02-01 12:23:00'))
  hash <- ziptrack:::hash_datetime(dates, resolution = "month")
  expect_equal(hash[1], hash[2])
  expect_false(hash[1] == hash[3])
})

test_that("hash datetime by year works", {
  dates <- lubridate::as_datetime(c('2018-01-01 12:33:44', '2018-01-08', '2017-01-01 12:23:00', '2017-12-31 12:23:00'))
  hash <- ziptrack:::hash_datetime(dates, resolution = "year")
  expect_equal(hash[1], hash[2])
  expect_false(hash[1] == hash[3])
  expect_false(hash[1] == hash[4])
})

test_that("hash datetime by week works", {
  dates <- lubridate::as_datetime(c('2018-01-01 12:33:44', '2018-01-06', '2017-01-12 12:23:00', '2017-12-31 12:23:00'))
  hash <- ziptrack:::hash_datetime(dates, resolution = "year")
  expect_equal(hash[1], hash[2])
  expect_false(hash[1] == hash[3])
  expect_false(hash[1] == hash[4])
})

test_that("hash datetime by format works", {
  dates <- lubridate::as_datetime(c('2018-01-01 12:33:44', '2017-03-01', '2018-01-02 12:33:44'))
  hash <- ziptrack:::hash_datetime(dates, resolution = "%d")
  expect_equal(hash[1], hash[2])
  expect_false(hash[1] == hash[3])
})

test_that("hash datetime by week throws exception before '0000-01-03'", {
  dates <- lubridate::as_datetime(c('0000-01-02 12:33:44'))
  expect_error(ziptrack:::hash_datetime(dates, resolution = 'week'))
})

test_that("hash datetime with invalid format fails", {
  dates <- lubridate::as_datetime(c('2018-01-01 12:33:44', '2017-03-01', '2018-01-02 12:33:44'))
  expect_error(ziptrack:::hash_datetime(dates, resolution = 'aeek'))
  expect_error(ziptrack:::hash_datetime(dates, resolution = '%o'))
})

test_that("hash datetime with non Date objects fails", {
  expect_error(ziptrack:::hash_datetime('2018-01-01'))
  expect_error(ziptrack:::hash_datetime(10102018))
  expect_error(ziptrack:::hash_datetime(NA))
})

test_that("hash datetime with NULL returns NULL", {
  dates <- NULL
  expect_true(is.null(ziptrack:::hash_datetime(dates)))
})
