#' incidence: a package for visualisating incidence based on outbreak line lists.
#'
#' This package provides functions for computing and plotting incidence data from outbreak line lists.
#'
#'
#' @section Available functions include:
#'  \itemize{
#'  \item{\link{get.incidence2}: }{Extension of OutbreakTools's function \code{get.incidence}}
#'  \item{\link{linelist2xts}: }{Convert line lists data into xts objects (multivariate time series)}
#'  \item{\link{plotIncidence}: }{Plot incidence time series using histograms}
#'  \item{\link{mapIncidence}: }{Visualise incidence in space and time using animations}
#' }
##
#'
#' @docType package
#' @name incidence
NULL




#' #' #' Toy outbreak dataset
#'
#' This is a simplified version of Michael Hohle's dataset Hagelloch
#'
#' \itemize{
#'   \item dateOfOnset a vector of dates of symptom onset in Date format
#'   \item gender the gender of the patient
#'   \item complication an indication of possible clinical complications
#' }
#'
#' @format A data frame with 187 rows and 3 variables
#' @source Michael Hohle's dataset Hagelloch
#' @name toy
NULL


#' #' #' Zombie outbreak dataset
#'
#' This is a dataset simulating an outbreak with geo-localisation of the cases (6 localities)
#'
#' \itemize{
#'   \item x.coord a vector of x coordinates
#'   \item y.coord a vector of y coordinates
#'   \item date a vector of dates of recorded cases in Date format
#'   \item gender the gender of the patient
#' }
#'
#' @format A data frame with 154013 rows and 4 variables
#' @name zombie_outbreak
NULL
