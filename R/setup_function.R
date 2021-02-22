# Function: reyes_setup
# Description: personal function for students in the course so that all
#              necessary functionality is incorporated.

ma482_setup <- function(){
  pkgs <- utils::installed.packages()
  dir <- pkgs[which(pkgs[, "Package"]=="reyes482"), "LibPath"]

  source(paste0(dir, "/reyes482/exec/ma482setup.R"))
}

