#Small demo showing the aggregate and visualization functionality for the
#Hagelloch data.
library("sp")
library("xts")
library("surveillance")
library("animation")

#Load Hagelloch data.
data("hagelloch.obk")

#' Create a SpatialPolygons object representing the households.
#'
#' @note This function is merely a convenience function.
hagellochSpatialPolygons <- function() {
  #Locations and a factor representing the locations.
  location <- hagelloch.obk@individuals[,c("x.loc","y.loc")]
  locationFactor <- hagelloch.obk@individuals[,"locationFactor"]

  #Small box, which we are going to translate and scale.
  box <- 10*cbind(c(0,1,1,0,0),c(0,0,1,1,0))

  households <- list()
  #Loop over all households and create a polygon for each of it.
  uLoc <- unique(cbind(location,locationFactor))
  for (i in 1:nrow(uLoc)) {
    households[[i]] <- Polygons(list(Polygon(t(apply(box,1, function(x) x + as.numeric(uLoc[i,1:2]))))),ID=uLoc[i,3])
  }

  #Create SpatialPolygons object from the list of Polygons
  map <- SpatialPolygons(households)

  #Done
  return(map)
}

#Compute SpatialPolygons object for the Hagelloch households.
hhMap <- hagellochSpatialPolygons()
plot(hhMap,axes=TRUE,xlab="x (m)",ylab="y (m)",main="Hagelloch household locations")

#Convert linelist to a weekly time series using the linelist2xts function
xts <- linelist2xts(formula=ERU ~ locationFactor, dateProjFun=monday, data=hagelloch.obk@individuals)
dim(xts)

#Convert xts object to sts class.
#ToDo: Have this as a converter function in the surveillance package.
sts <- new("sts", observed=as.matrix(xts), epoch=as.numeric(index(xts)), epochAsDate=TRUE, map=hhMap)
#Show the sts -- see ?stsplot for details
plot(sts, xlab="Date of rash (week)",
     xaxis.tickFreq = list("%V" = atChange, "%m" = atChange,"%G" = atChange),
     xaxis.labelFreq = list("%V" = atChange),
     xaxis.labelFormat = "%d-%b", type = observed ~ time, legend.opts=NULL)

#Do an animation using surveillance package functionality.
#We show daily number of cases per household (no cumulative incidence)
#See http://arxiv.org/pdf/1411.0416.pdf for a more detailed description.
saveHTML(animate(sts, tps = 1:8, total.args = list()),
         title = "Evolution of the measles epidemic in Hagelloch",
         ani.width = 500, ani.height = 600)

