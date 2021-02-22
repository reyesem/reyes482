# Functions helpful for survival analysis.

#' Plot of Cox-Snell residuals.
#'
#' Create a plot of the Cox-Snell residuals for examining the goodness of fit
#' for a Cox PH model.
#'
#' @param fit a \code{\link[survival]{coxph}} model object.
#'
#' @return a \code{\link[ggplot2]{ggplot}} object.
#'
#' @examples
#' library(survival)
#' test1 <- list(time = c(4,3,1,1,2,2,3),
#'               status = c(1,1,1,0,1,1,0),
#'               x = c(0,2,1,1,1,0,0),
#'               sex = c(0,0,0,0,1,1,1))
#'
#' fit <- coxph(Surv(time, status) ~ x + sex, data = test1)
#' CoxSnellPlot(fit)
#'
#' @importFrom rlang .data
#' @export
CoxSnellPlot <- function(fit){
  # Update model, if necessary
  fit <- update(fit, x = TRUE, y = TRUE)

  # Obtain Cox-Snell residuals from fit
  resi <- fit$y[, 2] - residuals(fit, type = "martingale")

  # Compute the cumulative hazard of these residuals
  Sfit <- survival::survfit(
    survival::Surv(time = resi, event = fit$y[, 2]) ~ 1)
  CumHaz <- cumsum(Sfit$n.event / Sfit$n.risk)

  plot.dat <- data.frame(resi = Sfit$time,
                         cumhaz = CumHaz)

  # Construct cumulative hazard vs. residuals
  p1 <- ggplot2::ggplot(data = plot.dat,
                        mapping = ggplot2::aes(y = .data$cumhaz,
                                               x = .data$resi)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, colour = "grey") +
    ggplot2::geom_point() +
    ggplot2::labs(x = "Cox-Snell Residuals",
                  y = "Estimated Cumulative Hazard Rates")

  return(p1)
}




#' Kaplain-Meier estimates.
#'
#' Compute Kaplan-Meier (product limit) estimates for survival data. This is a
#' wrapper to survfit in order to return a nicer set of data.
#'
#' @param form formula for describing a \code{Surv} object.
#' @param data optional dataset where variables appearing in form should be
#' located.
#' @param conflevel scalar. Confidence level (default = 0.95)
#'
#' @return dataframe containing the survival estimates.
#'
#' @examples
#' library(survival)
#' test1 <- list(time = c(4,3,1,1,2,2,3),
#'               status = c(1,1,1,0,1,1,0),
#'               x = c(0,2,1,1,1,0,0),
#'               sex = c(0,0,0,0,1,1,1))
#'
#' fit <- KMest(Surv(time, status) ~ sex, data = test1)
#'
#' @export
KMest <- function(form, data, conflevel = 0.95){
  # Use survfit to get the estimates
  if(missing(data)){
    fit <- survival::survfit(formula = form, conf.int = conflevel)
  } else if(!missing(data)){
    fit <- survival::survfit(formula = form, data = data, conf.int = conflevel)
  }

  # Obtain name of time in dataset
  survobj <- strsplit(as.character(form[2]), ",")[[1]][1]
  survobj <- strsplit(survobj, "\\(")[[1]][2]
  survobj <- strsplit(survobj, "=")[[1]][1]

  # Construct data
  dat <- data.frame(Survival = fit$surv,
                    UCL = fit$upper,
                    LCL = fit$lower)

  dat[,survobj] <- fit$time
  dat <- dat[, c(4, 1, 2, 3)]

  # Add grouping if available
  if(!is.null(fit$strata)){
    names(fit$strata) <- sapply(strsplit(names(fit$strata), "="), "[", 2)
    dat[, as.character(form[3])] <- rep(names(fit$strata), times = fit$strata)
  }

  return(dat)
}




#' Life table estimates.
#'
#' Compute life table estimates for survival data.
#'
#' @param nrisk vector. Number of subjects at risk at start of interval.
#' @param nevent vector. Number of events occuring during the interval.
#' @param ncensor vector. Number of subjects censored during interval.
#' @param data dataframe. Optional data set containing the variables.
#' @param conflevel scalar. Confidence level (default = 0.95).
#'
#' @return dataframe containing the survival estimates (has class LifeTable).
#'
#' @import stats
#' @export
LifeTable <- function(nrisk, nevent, ncensor, data, conflevel = 0.95){

  # Form Dataset
  if(!missing(data)){
    dat <- dplyr::select(data,
                         {{ nrisk }},
                         {{ nevent }},
                         {{ ncensor }})

    colnames(dat) <- c("nrisk", "nevent", "ncensor")
  } else if(missing(data)){
    dat <- data.frame(nrisk = nrisk,
                      nevent = nevent,
                      ncensor = ncensor)
  }


  # Compute Quantities of Interest
  dat <- within(dat, {
    # Mortality Rate
    mortality <- nevent/(nrisk - 0.5*ncensor)

    # Survival
    survival <- cumprod(1 - mortality)

    # SE
    se <- (mortality / (nevent * (1 - mortality)))
    se <- ifelse(is.nan(se), 0, se)
    se <- survival * sqrt(cumsum(se))

    # Confidence Limits
    LCL <- pmax(0, survival - qnorm(0.5*(1+conflevel))*se)
    UCL <- pmin(1, survival + qnorm(0.5*(1+conflevel))*se)
  })

  dat <- dat[, c(1, 2, 3, 8, 7, 6, 5, 4)]
  names(dat) <- c("Number at Risk","Number of Events","Number Censored",
                  "Mortality", "Survival", "Std. Error",
                  "Lower CL", "Upper CL")

  class(dat) <- c("LifeTable", "data.frame")
  return(dat)
}
