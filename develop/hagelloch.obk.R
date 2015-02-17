#' Script to generate Hagelloch data from the surveillance package.
#'
#' @author Michael Höhle
#' 


library("OutbreakTools")

hagelloch.obk <- (function() {
  #Use Hagelloch measles data (as available in the surveillance package) instead
  load(system.file("data","hagelloch.RData", package="surveillance"))

  #The variable PN contains the ID, use the OutbreakTools name 'individualID' instead
  names(hagelloch.df)[pmatch("PN",names(hagelloch.df))] <- "individualID"
  #Remove the individual, which must have gotten infected for other sources than from the outbreak
  diff(sort(hagelloch.df$ERU))
  hagelloch.df <- hagelloch.df[-which.max(hagelloch.df$ERU),]

  #Add location slot
  location <- hagelloch.df[,c("x.loc","y.loc")]
  hagelloch.df$locationFactor <- as.factor(apply(location, 1, paste0, collapse="-"))

  #Check.
  nrow(hagelloch.df)

  #Variables with date information in the Hagelloch data.set
  dateVars <- c("PRO", "ERU", "DEAD")
  records <- lapply(dateVars, function(varName) {
    data.frame(individualID=hagelloch.df$individualID, date=hagelloch.df[,varName])
  })
  #Give the list appropriate names (ensure names are not the same as in 'individuals')
  names(records) <- paste0("time",dateVars)

  #Create obkData object
  hagelloch.obk <- methods::new("obkData", individuals=hagelloch.df, records=records)

  #Done
  return(hagelloch.obk)
})()


######################################################################
# Function doing the work.
######################################################################

doIt <- function() {
  source("hagelloch.obk.R")
  save(file="../data/hagelloch.obk.RData", list=c("hagelloch.obk"))
}
