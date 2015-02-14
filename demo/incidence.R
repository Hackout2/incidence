#Small demo illustrating how to do manual aggregation using the data.table
#function. This should allow for more flexible grouping also.

library("data.table")
library("scales")
library("ggplot2")
library("OutbreakTools")
require("zoo")

#Daily time series (for all)
xts2 <- linelist2xts(formula=ERU ~ 1, dateProjFun=identity, data=hagelloch.obk@individuals)
#Weekly time series (for all)
xts3 <- linelist2xts(formula=ERU ~ 1, dateProjFun=monday, data=hagelloch.obk@individuals)
#Monthly time series (for all)
xts4 <- linelist2xts(formula=ERU ~ 1, dateProjFun=firstOfMonth, data=hagelloch.obk@individuals)
#Weekly time series per gender
xts5 <- linelist2xts(formula=ERU ~ SEX, dateProjFun=monday, data=hagelloch.obk@individuals)
#Show result (using zoo plotting functionality. There might be something newwer?)
plot(as.zoo(xts5),main="",type="h",xlab="Time of onset (week)")

#ggplot2 version.
autoplot(xts5) + scale_x_date(labels = date_format("%G-W%V"), xlab("Time of onset of rash (ISO week)")) +  scale_y_continuous(ylab("No. individuals"))

#Similar, but now using Thibaut's function.
plotIncidence(hagelloch.obk@individuals, dates="ERU", split.by="SEX")
#Now including the school class.
plotIncidence(hagelloch.obk@individuals, dates="ERU", split.by="SEX",fill.by="CL")

