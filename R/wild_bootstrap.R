# functions for implementing a wild bootstrap

#' Environment for bootstrapping.
#' @export
.reyesEnv <- new.env(parent = emptyenv())

#' Wild bootstrap.
#'
#' Implement a wild bootstrap for \code{lm} and \code{nls} objects. This is a
#' very rough approach based on the \code{\link[car]{Boot}} function.
#'
#' @param object a regression object of class "lm" or "nls".
#' @param f see \code{\link[car]{Boot}}
#' @param labels see \code{\link[car]{Boot}}
#' @param R see \code{\link[car]{Boot}}
#' @param ncores see \code{\link[car]{Boot}}
#' @param ... arguments passed to additional methods
#'
#' @return See \code{\link[boot]{boot}} for the returned value of the structure
#' returned by this function.
#'
#' @examples
#' m1 <- lm(dist ~ speed, data = cars)
#' betahat.boot <- WildBoot(m1, R = 199)
#'
#' confint(betahat.boot)
#'
#' @import stats
#' @export
WildBoot <- function(object, f = coef, labels = names(f(object)), R = 999,
                     ncores = 1, ...){
  UseMethod("WildBoot")
}




#' @describeIn WildBoot
#'
#' @import stats
#' @export
WildBoot.nls <- function(object, f = coef, labels = names(f(object)), R = 999,
                         ncores = 1, ...){
  f0 <- f(object)
  all.names <- all.vars(object$m$formula())
  param.names <- names(object$m$getPars())
  vars <- all.names[!(all.names %in% param.names)]
  obj <- update(object, data = na.omit(eval(object$data)[,
                                                         vars]), start = coef(object))

  if (!(requireNamespace("car")))
    stop("The 'car' package is missing")
  if (!(requireNamespace("boot")))
    stop("The 'boot' package is missing")
  if (length(labels) != length(f0))
    labels <- paste("V", seq(length(f0)), sep = "")
  opt <- options(show.error.messages = FALSE)

  # adjustment for wild boostrap patterned after residual bootstrap
  boot.f <- function(data, indices, .fn){
    res <- residuals(object)

    .n <- length(res)
    .delta1 <- sqrt((3/4) + (sqrt(17)/12))
    .delta2 <- sqrt((3/4) - (sqrt(17)/12))
    .v <- matrix(rnorm(2*.n), nrow=.n, ncol=2)

    .mammen <- (.delta1 + (.v[,1]/sqrt(2)))*(.delta2 + (.v[,2]/sqrt(2))) -
      (.delta1*.delta2)

    val <- fitted(object) + res*.mammen
    if (!is.null(object$na.action)){
      pad <- object$na.action
      attr(pad, "class") <- "exclude"
      val <- naresid(pad, val)
    }

    assign(".y.boot", val, envir = .reyesEnv)
    mod <- try(update(object, get(".y.boot", envir = .reyesEnv) ~
                        ., start = coef(object)))
    if (inherits(mod, "try-error")) {
      out <- .fn(object)
      out <- rep(NA, length(out))
    }
    else {
      out <- .fn(mod)
    }
    out
  }

  if (ncores <= 1) {
    parallel_env = "no"
    ncores = getOption("boot.ncpus", 1L)
  }
  else {
    if (.Platform$OS.type == "unix") {
      parallel_env = "multicore"
    }
    else {
      parallel_env = "snow"
    }
  }

  b <- boot::boot(data.frame(update(object, model = TRUE)$model),
                  boot.f, R, .fn = f, parallel = parallel_env, ncpus = ncores,
                  ...)
  colnames(b$t) <- labels
  if (exists(".y.boot", envir = .reyesEnv))
    remove(".y.boot", envir = .reyesEnv)
  if (exists(".boot.indices", envir = .reyesEnv))
    remove(".boot.indices", envir = .reyesEnv)
  options(opt)
  d <- dim(na.omit(b$t))[1]
  if (d != R)
    cat(paste("\n", "Number of bootstraps was", d, "out of",
              R, "attempted", "\n"))
  b
}



#' @describeIn WildBoot
#'
#' @import stats
#' @export
WildBoot.lm <- function (object, f = coef, labels = names(f(object)), R = 999,
                         ncores = 1, ...){
  if (!is.null(object$na.action))
    stop("The WildBoot function does not currently allow\n  missing values for lm or glm models.  Refit your model with rows \n  with missing values removed.  If you have a data frame called 'd', \n  then the argument data=na.omit(d) is likely to work.")

  if (!(requireNamespace("car")))
    stop("The 'car' package is missing")
  if (!(requireNamespace("boot")))
    stop("The 'boot' package is missing")
  f0 <- f(object)
  if (length(labels) != length(f0))
    labels <- paste0("V", seq_along(f0))

  # make alterations for wild bootstrap based on residual bootstrap in Boot.default
  boot.f <- function(data, indices, .fn){
    res <- residuals(object, type = "pearson")

    .n <- length(res)
    .delta1 <- sqrt((3/4) + (sqrt(17)/12))
    .delta2 <- sqrt((3/4) - (sqrt(17)/12))
    .v <- matrix(rnorm(2*.n), nrow=.n, ncol=2)

    .mammen <- (.delta1 + (.v[,1]/sqrt(2)))*(.delta2 + (.v[,2]/sqrt(2))) -
      (.delta1*.delta2)

    val <- fitted(object) + res*.mammen

    if (!is.null(object$na.action)) {
      pad <- object$na.action
      attr(pad, "class") <- "exclude"
      val <- naresid(pad, val)
    }
    assign(".y.boot", val, envir = .reyesEnv)
    mod <- update(object, get(".y.boot", envir = .reyesEnv) ~ .)
    out <- if (!is.null(object$qr) && (mod$qr$rank != object$qr$rank))
      f0 * NA
    else .fn(mod)
    out
  }

  nobs0 <- function(x, ...) {
    rval <- try(stats::nobs(x, ...), silent = TRUE)
    if (inherits(rval, "try-error") | is.null(rval))
      rval <- NROW(residuals(x, ...))
    return(rval)
  }
  n <- nobs0(object)
  dd <- data.frame(.zero = rep.int(0L, n))
  if (ncores <= 1) {
    parallel_env = "no"
    ncores = getOption("boot.ncpus", 1L)
  }
  else {
    if (.Platform$OS.type == "unix") {
      parallel_env = "multicore"
    }
    else {
      parallel_env = "snow"
    }
  }
  b <- boot::boot(dd, boot.f, R, .fn = f, parallel = parallel_env,
                  ncpus = ncores, ...)
  colnames(b$t) <- labels
  if (exists(".y.boot", envir = .reyesEnv))
    remove(".y.boot", envir = .reyesEnv)
  if (exists(".boot.indices", envir = .reyesEnv))
    remove(".boot.indices", envir = .reyesEnv)
  b
}
