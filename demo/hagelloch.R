library("sp")
library("xts")
library("surveillance")
library("animation")

#Show hagelloch data
data("hagelloch.obk")


#Dirty mapping of the households
hagellochLocations <- function() {
  #Locations
  location <- hagelloch.obk@individuals[,c("x.loc","y.loc")]
  locationFactor <- apply(location, 1, paste0, collapse="-")
  hagelloch.obk@individuals$locationFactor <- as.factor(locationFactor)
  xts <- linelist2xts(formula=ERU ~ locationFactor, dateProjFun=monday, data=hagelloch.obk@individuals)

  #Small fake map for the locations
  box <- 10*cbind(c(0,1,1,0,0),c(0,0,1,1,0))
  households <- list()
  uLoc <- unique(cbind(location,locationFactor))

  #Loop over all locations
  for (i in 1:nrow(uLoc)) {
    households[[i]] <- Polygons(list(Polygon(t(apply(box,1, function(x) x + as.numeric(uLoc[i,1:2]))))),ID=uLoc[i,3])
  }

  map <- SpatialPolygons(households)
  return(list(xts=xts,map=map))
}

#Compute incidence time series per
hagelloch <- hagellochLocations()
plot(hagelloch$map,axes=TRUE)

#Convert xts object to sts class
sts <- new("sts", observed=as.matrix(hagelloch$xts), epoch=as.numeric(index(hagelloch$xts)), epochAsDate=TRUE, map=hagelloch$map)
plot(sts, xlab="Date of rash (week)",
     xaxis.tickFreq = list("%V" = atChange, "%m" = atChange,"%G" = atChange),
     xaxis.labelFreq = list("%V" = atChange),
     xaxis.labelFormat = "%d-%b", type = observed ~ time, legend.opts=NULL)

#Do an animation
saveHTML(animate(sts, tps = 1:8, total.args = list()),
         title = "Evolution of the measles epidemic in Hagelloch",
         ani.width = 500, ani.height = 600)

###Demo using surveillance -- see http://arxiv.org/pdf/1411.0416.pdf
data("measlesWeserEms")
saveHTML(animate(measlesWeserEms, tps = 1:52, total.args = list()),
         title = "Evolution of the measles epidemic in the Weser-Ems region",
         ani.width = 500, ani.height = 600)
