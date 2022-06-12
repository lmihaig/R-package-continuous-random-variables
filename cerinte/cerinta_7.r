# 7) Crearea unei functii P care permite calculul diferitelor tipuri de probabilitati asociate
# unei variabile aleatoare continue(similar functiei P din pachetul discreteRV)

# Pachetul discreteRV folosit pentru inspiratie

# TODO: de facut si pentru joint distributions

"<.cRV" <- function(X, x) {
    if (class(X) != "cRV") stop("X is not a continuous random variable.")
    if (class(x) != "numeric") stop("x must be a numeric value")
  
    interv <- Intervals(
      matrix(
        c(-Inf, x),
        byrow = TRUE,
        ncol = 2
      ),
      closed = c(FALSE, FALSE),
      type = "R"
    )
  
    result <- attr(X, "cdf")
    class(result) <- "cRVresult"
    attr(result, "interval") <- interv
    
    return(result)
}

"<=.cRV" <- function(X, x) {
  if (class(X) != "cRV") stop("X is not a continuous random variable.")
  if (class(x) != "numeric") stop("x must be a numeric value")
  
  interv <- Intervals(
    matrix(
      c(-Inf, x),
      byrow = TRUE,
      ncol = 2
    ),
    closed = c(FALSE, TRUE),
    type = "R"
  )
  
  result <- attr(X, "cdf")
  class(result) <- "cRVresult"
  attr(result, "interval") <- interv
  
  return(result)
}

">.cRV" <- function(X, x) {
  if (class(X) != "cRV") stop("X is not a continuous random variable.")
  if (class(x) != "numeric") stop("x must be a numeric value")
  
  interv <- Intervals(
    matrix(
      c(x, Inf),
      byrow = TRUE,
      ncol = 2
    ),
    closed = c(FALSE, FALSE),
    type = "R"
  )
  
  result <- attr(X, "cdf")
  class(result) <- "cRVresult"
  attr(result, "interval") <- interv
  
  return(result)
}

">=.cRV" <- function(X, x) {
  if (class(X) != "cRV") stop("X is not a continuous random variable.")
  if (class(x) != "numeric") stop("x must be a numeric value")
  
  interv <- Intervals(
    matrix(
      c(x, Inf),
      byrow = TRUE,
      ncol = 2
    ),
    closed = c(TRUE, FALSE),
    type = "R"
  )
  
  result <- attr(X, "cdf")
  class(result) <- "cRVresult"
  attr(result, "interval") <- interv
  
  return(result)
}

"==.cRV" <- function(X, x) {
  if (class(X) != "cRV") stop("X is not a continuous random variable.")
  if (class(x) != "numeric") stop("x must be a numeric value")
  
  interv <- Intervals(
    matrix(
      c(x, x),
      byrow = TRUE,
      ncol = 2
    ),
    closed = c(TRUE, TRUE),
    type = "R"
  )
  
  result <- attr(X, "cdf")
  class(result) <- "cRVresult"
  attr(result, "interval") <- interv
  
  return(result)
}

#'
#' @param Xres The result of comparing a cRV with a numeric value
#' @param Yres The result of comparing a cRV with a numeric value
#' @examples
#' (X < 0.1) | (X > 0.2)
"|.cRVresult" <- function(Xres, Yres) {
  ORret <- Xres
  attr(ORret, "interval") <- interval_union(attr(Xres, "interval"),
                                                   attr(Yres, "interval"))
  return(ORret)
}

#'
#' @param Xres The result of comparing a cRV with a numeric value
#' @param Yres The result of comparing a cRV with a numeric value
#' @examples
#' (X > 0.1) & (X < 0.2)
"&.cRVresult" <- function(Xres, Yres) {
  ORret <- Xres
  attr(ORret, "interval") <- interval_intersection(attr(Xres, "interval"),
                                                   attr(Yres, "interval"))
  return(ORret)
}

#'
#' @examples
#' pdf1 <- function(x) { (0 <= x & x <= 1) * (3\*x^2) }
#' X <- cRV(pdf1)
#' Pr((X < 0.1) | (X > 0.2) | ((X > 0.1) & (X < 0.2)))
Pr <- function(cResult) {
  if (class(cResult) != "cRVresult") stop("Incorrect type for parameter")
  
  # calculate integral over interval using cdf
  calcFunction <- function(interv) {
    rvl <- 0
    if (interv[2] != -Inf)
      rvl <- cResult(interv[2])
    
    lvl <- 0
    if (interv[1] != -Inf)
      lvl <- cResult(interv[1])
    
    return(rvl - lvl)
  }
  
  sum(apply(attr(cResult, "interval"), 1, calcFunction))
}