
# based on a space-time gridding with a spatial resolution of 1km2 and a
# temporal resolution of 1 week. This means a new occurrence record should be
# created when an observed organism moves from one spatiotemporal cell to
# another. The arrival and departure date from this spatiotemporal cell will be
# saved as part of the aggregation. With respect to abiotic parameters, such as
# temperature, recorded together with the observations, the start and end values
# will be saved.


test_that("binning works without detections", {
  # d <- date.frame(decimalLongitude = 0, decimalLatitude = 1, eventDate = '2010-01-20')
  # bin_tracking_data(d)
})

test_that("binning works with detections", {

})

test_that("hash datetime by day works", {
  dates <- lubridate::as_datetime(c('2018-01-01 12:33:44', '2018-01-01', '2017-12-31 12:23:00'))
  hash <- ziptrack:::hash_datetime(dates, resolution = "week")
  expect_equal(hash[1], hash[2])
  expect_false(hash[1] == hash[3])
})

test_that("hash datetime by month works", {

})

test_that("hash datetime by year works", {

})

test_that("hash datetime by format works", {

})

test_that("hash datetime by week works", {

})

test_that("hash datetime by week throws exception before '0000-01-04'", {

})

test_that("hash datetime with invalid format fails", {

})
