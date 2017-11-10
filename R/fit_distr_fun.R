#' Fit a prob. distribution to data and return appropriate functions in the form pdist, ddist, rdist, qdist
#' 
#' Given the name of a family of 1-dimensional distributions, this function chooses a particular member 
#' of the family that fits the data and returns a function in the selected p, d, q, or r format.
#' `MASS::fitdistr()` is used to  estimate the parameters.
#' 
#' @param x A vector of numerical data
#' @param distfun A distribution function for the family of density desired, e.g. `pnorm`, `rgamma`. (see MASS:fitdistr for the 
#' distributions that are available)
#' what format the generated function should take.
#' @param start starting values for the parameters
#' @param ... Additional arguments to MASS::fitdistr()
#' 
#' @return  A function of one variable that acts like, say, pnorm, dnorm, qnorm, rnorm, but using the
#' fitted distribution
#' @export
fit_distr_fun <- function(data, formula, distr_fun, start, ... ) {
  # Eliminate the next line in favor of listing rlang as a dependency in the package description file
  if ( ! require(rlang) ) stop("Need to install rlang package")
  
  x <- eval_tidy(formula[[2]], data = data)

  distr_fun_name <- expr_text(enquo(distr_fun)[[2]])
  densfun <- gsub("$[pdqr]", "d", distr_fun_name)
  
  params <- analytic_distributions(x, densfun)
  if (is.null(params)) { 
    if (is.missing(start)) start <- get_start_params(x, densfun)
    params <- MASS::fitdistr(x, densfun, start, ...)
    
  }
  
  # set those parameters as the defaults for the appropriate arguments
  param_args <- as.list(params$estimate)
  orig_args <- formals(distr_fun)
  orig_args[names(param_args)] <- param_args
  formals(distr_fun) <- orig_args
  
  distr_fun
}


analytic_distributions <- function(x, distname) {
  n <- length(x)
  if (distname %in% c("dlnorm")) {
    if (any(x <= 0)) 
      stop("need positive values to fit a log-Normal")
    lx <- log(x)
    sd0 <- sqrt((n - 1)/n) * sd(lx)
    mx <- mean(lx)
    estimate <- c(mx, sd0)
    sds <- c(sd0/sqrt(n), sd0/sqrt(2 * n))
    names(estimate) <- names(sds) <- c("meanlog", "sdlog")
    vc <- matrix(c(sds[1]^2, 0, 0, sds[2]^2), ncol = 2, 
                 dimnames = list(names(sds), names(sds)))
    names(estimate) <- names(sds) <- c("meanlog", "sdlog")
    return(structure(list(estimate = estimate, sd = sds, 
                          vcov = vc, n = n, loglik = sum(dlnorm(x, mx, 
                                                                sd0, log = TRUE))), class = "fitdistr"))
  }
  if (distname == "dnorm") {
    sd0 <- sqrt((n - 1)/n) * sd(x)
    mx <- mean(x)
    estimate <- c(mx, sd0)
    sds <- c(sd0/sqrt(n), sd0/sqrt(2 * n))
    names(estimate) <- names(sds) <- c("mean", "sd")
    vc <- matrix(c(sds[1]^2, 0, 0, sds[2]^2), ncol = 2, 
                 dimnames = list(names(sds), names(sds)))
    return(structure(list(estimate = estimate, sd = sds, 
                          vcov = vc, n = n, loglik = sum(dnorm(x, mx, sd0, 
                                                               log = TRUE))), class = "fitdistr"))
  }
  if (distname == "poisson") {
    estimate <- mean(x)
    sds <- sqrt(estimate/n)
    names(estimate) <- names(sds) <- "lambda"
    vc <- matrix(sds^2, ncol = 1, nrow = 1, dimnames = list("lambda", 
                                                            "lambda"))
    return(structure(list(estimate = estimate, sd = sds, 
                          vcov = vc, n = n, loglik = sum(dpois(x, estimate, 
                                                               log = TRUE))), class = "fitdistr"))
  }
  if (distname == "dexp") {
    if (any(x < 0)) 
      stop("Exponential values must be >= 0")
    estimate <- 1/mean(x)
    sds <- estimate/sqrt(n)
    vc <- matrix(sds^2, ncol = 1, nrow = 1, dimnames = list("rate", 
                                                            "rate"))
    names(estimate) <- names(sds) <- "rate"
    return(structure(list(estimate = estimate, sd = sds, 
                          vcov = vc, n = n, loglik = sum(dexp(x, estimate, 
                                                              log = TRUE))), class = "fitdistr"))
  }
  if (distname == "dgeom") {
    estimate <- 1/(1 + mean(x))
    sds <- estimate * sqrt((1 - estimate)/n)
    vc <- matrix(sds^2, ncol = 1, nrow = 1, dimnames = list("prob", 
                                                            "prob"))
    names(estimate) <- names(sds) <- "prob"
    return(structure(list(estimate = estimate, sd = sds, 
                          vcov = vc, n = n, loglik = sum(dgeom(x, estimate, 
                                                               log = TRUE))), class = "fitdistr"))
  }
  
  NULL # signal that there was no analytic calculation
}

get_start_params <- function(x, distname) {
  if (distname == "dweibull") {
    if (any(x <= 0)) 
      stop("Weibull values must be > 0")
    lx <- log(x)
    m <- mean(lx)
    v <- var(lx)
    shape <- 1.2/sqrt(v)
    scale <- exp(m + 0.572/shape)
    start <- list(shape = shape, scale = scale)
    start <- start[!is.element(names(start), dots)]
  }
  if (distname == "dgamma") {
    if (any(x < 0)) 
      stop("gamma values must be >= 0")
    m <- mean(x)
    v <- var(x)
    start <- list(shape = m^2/v, rate = m/v)
    start <- start[!is.element(names(start), dots)]
    control <- list(parscale = c(1, start$rate))
  }
  if (distname == "dnbinom") {
    m <- mean(x)
    v <- var(x)
    size <- if (v > m) 
      m^2/(v - m)
    else 100
    start <- list(size = size, mu = m)
    start <- start[!is.element(names(start), dots)]
  }
  if (distname %in% c("dcauchy", "dlogis")) {
    start <- list(location = median(x), scale = IQR(x)/2)
    start <- start[!is.element(names(start), dots)]
  }
  if (distname == "t") {
    start <- list(m = median(x), s = IQR(x)/2, df = 10)
    start <- start[!is.element(names(start), dots)]
  }
  
  start
}
