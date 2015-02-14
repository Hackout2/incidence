context("Test aggregation functionality in getIncidence")

library("data.table")
library("zoo")

#Extract the data.frame from the Hagelloch data
dt <- data.table(hagelloch.obk@individuals)
#Select which date column to do the aggregation by and make a column data
col <- "ERU"
dt$date <- dt[[col]]

#To aggregate by ISO weeks we need a function to aggregate by monday of the ISO week.
#Note: This might not work on windows.
all.equal(data.table::week(dt$date),as.numeric(format(dt$date,"%V")))
all.equal(lubridate::week(dt$date),as.numeric(format(dt$date,"%V")))
all.equal(lubridate::isoweek(dt$date),as.numeric(format(dt$date,"%V")))

#Weekly data based on ISOweek (list not necessary if the column is ok to be called monday)
incidence <- dt[!is.na(date), list(incidence = .N), by = list(date=monday(date))]

## create data.table of all dates, to fill in incince
dates <- data.table(date = seq.Date(monday(min(dt[, date])), monday(max(dt[, date])), by = "1 week"))

## merge
incidence.ts <- merge(incidence, dates, by = "date", all.y = TRUE)

## Convert NA's to zeroes
incidence.ts[is.na(incidence), incidence := 0]

#Compare output
ts <- get.incidence(hagelloch.obk,data="timeERU", where="records", interval=7,
                    from=head(dates[["date"]],n=1),to=tail(dates[["date"]],n=1),add.zero=FALSE)
all.equal(ts$date , incidence.ts[["date"]])

#Weekly time series (for all)
xts3 <- linelist2xts(formula=ERU ~ 1, dateProjFun=monday, data=hagelloch.obk@individuals)

#Checks between manual aggregation and automatic.
test_that("linelist2sts dates works",
          expect_that(all(ts$date == index(xts3)), equals(TRUE)))

test_that("linelist2sts counts works",
         expect_that(all(ts$incidence==xts3), equals(TRUE)))


