# Additional methods for common operations.

#' Confidence intervals for general linear hypothesis tests.
#'
#' Compute asymptotic confidence intervals for linear combinations of a
#' parameter vector
#' \deqn{\mathbf{K}\beta}
#'
#' @param object numeric matrix. The (r by p) K matrix specifying the r linear
#' combinations for which a confidence interval should be generated.
#' @param parm a specification of which parameters are to be given confidence
#' intervals, either a vector of numbers or a vector of names. If missing, all
#' parameters are considered.
#' @param level confidence level required (default = 0.95).
#' @param coef. numeric vector. Parameter estimates.
#' @param vcov. numeric matrix. Variance-covariance matrix for the parameter
#' estimates.
#' @param ... additional arguments (not currently implemented).
#'
#' @return A matrix (or vector) with columns giving lower and upper confidence
#' limits for each linear combination.
#'
#' @examples
#' carsmod <- lm(dist ~ speed, data = cars)
#' K <- matrix(c(1, 0, 0, 1, 1, 1), nrow = 3, ncol = 2, byrow = TRUE)
#'
#' confint(K, coef. = coef(carsmod), vcov. = vcov(carsmod))
#'
#' @export
confint.matrix <- function(object, parm, level = 0.95, coef., vcov., ...){
  Kb <- drop(object %*% coef.)
  KSK <- object %*% vcov. %*% t(object)

  parm <- seq_along(Kb)

  a <- (1 - level)/2
  a <- c(a, 1 - a)

  pct <- paste(format(100 * a, trim = TRUE, scientific = FALSE, digits = 3))

  fac <- stats::qnorm(a)

  ci <- array(NA, dim = c(length(parm), 2L), dimnames = list(parm, pct))
  ses <- sqrt(diag(KSK))[parm]
  ci[] <- Kb[parm] + ses %o% fac

  ci
}


# #' @importFrom broom glance
# #' @export
# broom::glance


# #' Glance at a model object.
# #'
# #' Construct a single row summary "glance" of a model fit.
# #'
# #' @param x model or other R object to convert to a single-row data frame
# #' @param ... additional arguments (not currently implemented).
# #'
# #' @return A one-row data frame
# #' @export
# glance.geeglm <- function(x, ...){
#   s <- geepack:::summary.geeglm(x)
#
#   data.frame(n = sum(s$clusz),
#              sigma = sqrt(s$geese$scale$estimate),
#              df.residual = s$df[2],
#              n.clusters = length(s$clusz),
#              max.cluster.size = max(s$clusz))
# }




#' Print LifeTable object.
#'
#' Prints a LifeTable object and returns it invisibly.
#'
#' @param x LifeTable object
#' @param ... additional arguments (not currently implemented).
#'
#'
#' @export
print.LifeTable <- function(x, ...){
  x[, -c(1:3)] <- round(x[, -c(1:3)], 3)
  print.data.frame(x, row.names = FALSE)

  invisible(x)
}



# #' Extract variance-covariance matrix from \code{geeglm} object.
# #'
# #' Returns the variance-covariance matrix of a \code{geeglm} object fit with
# #' the \code{geepack} algorithms.
# #'
# #' @param object \code{geeglm} object
# #' @param ... additional arguments (not currently implemented).
# #'
# #'
# #' @export
# vcov.geeglm <- function(object,...){
#   return(object$geese$vbeta)
# }



#' Summarize a dataset.
#'
#' Wrapper for \code{\link[skimr]{skim}} which returns numeric summaries for
#' variables in a dataset.
#'
#' @param data see \code{\link[skimr]{skim}}
#' @param ... see \code{\link[skimr]{skim}}.
#'
#' @export
skim_clean <- skimr::skim_with(
  numeric = skimr::sfl(hist = NULL),
  ts = skimr::sfl(line_graph = NULL),
  character = skimr::sfl(
    min = NULL,
    max = NULL,
    empty = NULL,
    n_unique = skimr::n_unique,
    whitespace = NULL,
    top_counts = NULL,
    counts = ~paste(names(skimr::sorted_count(.)),
                    skimr::sorted_count(.),
                    sep = ":", collapse = ", "),
    percs = ~paste(names(skimr::sorted_count(.)),
                   round(100*skimr::sorted_count(.)/skimr::n_complete(.), 1),
                   sep = ":", collapse = ", ")),
  factor = skimr::sfl(
    top_counts = NULL,
    counts = ~paste(names(skimr::sorted_count(.)),
                    skimr::sorted_count(.),
                    sep = ":", collapse = ", "),
    percs = ~paste(names(skimr::sorted_count(.)),
                   round(100*skimr::sorted_count(.)/skimr::n_complete(.), 1),
                   sep = ":", collapse = ", ")))

