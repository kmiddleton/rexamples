##' Convert Excel Dates to R Dates
##'
##' This function converts numeric Excel dates to R dates using the
##' appropriate offset for Excel's starting from 1899-12-30.
##' 
##' @title ExcelDateToRDate
##' 
##' @param x vector of Excel (numeric) dates
##' 
##' @return
##' 
##' @author Kevin Middleton
##'
##' @references See \url{http://markmail.org/message/zcydoj6nvtqe57xl}
##' 
ExcelDateToRDate <- function(x) {
  as.Date(x, origin = "1899-12-30")
}
