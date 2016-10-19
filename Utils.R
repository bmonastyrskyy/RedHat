# Author: Bohdan Monastyrskyy
# Date : 2016-08-27
# Description : collection of utils


# load library
load.lib <- function(l){
  if (! require(l, character.only = TRUE)){
    suppressWarnings(install.packages(l, dependencies = TRUE));
    if (require(l, character.only = TRUE)){
      return(TRUE);
    } else {
      stop(paste("The package", l , "hasn't been loaded"));
    }
  } else {
    return(TRUE);
  }
}

# read big data table
read.big.table <- function(f, ...){
  tmp<-read.table(file=f, nrows=100, stringsAsFactors=FALSE, ...)
  colClasses <- sapply(tmp, class)
  res <- read.table(file=f, stringsAsFactors=FALSE, colClasses = colClasses, ...)
  res
}