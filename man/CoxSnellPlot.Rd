% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/survival.R
\name{CoxSnellPlot}
\alias{CoxSnellPlot}
\title{Plot of Cox-Snell residuals.}
\usage{
CoxSnellPlot(fit)
}
\arguments{
\item{fit}{a \code{\link[survival]{coxph}} model object.}
}
\value{
a \code{\link[ggplot2]{ggplot}} object.
}
\description{
Create a plot of the Cox-Snell residuals for examining the goodness of fit
for a Cox PH model.
}
\examples{
library(survival)
test1 <- list(time = c(4,3,1,1,2,2,3),
              status = c(1,1,1,0,1,1,0),
              x = c(0,2,1,1,1,0,0),
              sex = c(0,0,0,0,1,1,1))

fit <- coxph(Surv(time, status) ~ x + sex, data = test1)
CoxSnellPlot(fit)

}
