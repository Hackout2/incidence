context("ISO8601 Date playing")

#Generate some dates
d <- as.Date("2001-01-01") + 0:6

test_that("wdaymon function",
          expect_that(wdaymon(d), equals(seq_len(7))))

test_that("monday function",
          expect_that(monday(d), equals(rep(as.Date("2001-01-01"),length(d)))))

test_that("firstOfMonth function",
          expect_that(firstOfMonth(d), equals(rep(as.Date("2001-01-01"),length(d)))))
