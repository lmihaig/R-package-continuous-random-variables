# 4) Reprezentarea grafica a densitatii si a functiei de repartitie pentru diferite valori ale 
# parametrilor repartitiei. In cazul in care functia de repartitie nu este data intr-o forma 
# explicita(ex. repartitia normala) se accepta reprezentarea grafica a unei aproximari a 
# acesteia.

library(plotly)

#' Plot pdf or cdf of a unidimensional or a bidimensional random variable.
#' 
#' @name plot_fun
#' @param fun Can be a function taking one argument (x) or two arguments (x, y). Can be a vector of y values for unidimensional distributions, or a vector/matrix of z values for bidimensional distributions.
#' @param xDomain Domain of x values over which pdf/cdf is to be evaluated
#' @param yDomain Domain of y values over which pdf/cdf is to be evaluated. If empty/not provided, 'plot_fun()' assumes a unidimensional distribution.
#' @examples 
#' 
#' # UNIDIMENSIONAL RV EXAMPLES
#' 
#' # Using pdf provided by the user
#' pdf1 <- function(x) { (0 <= x & x <= 1) * (3\*x^2) }
#' plot_fun(pdf1, seq(-1, 2, 0.01))
#'
#' # Using cdf provided by the user
#' cdf1 <- function(x) { integral(pdf1, bound = list(x = c(-Inf, x)))$value }
#' plot_fun(cdf1, seq(-1, 2, 0.01))
#' 
#' # Using normal distribution
#' xDomain <- seq(-10, 10, 0.1)
#' plot_fun(dnorm(xDomain, mean = 0, sd = 1), xDomain)
#' plot_fun(pnorm(xDomain, mean = 0, sd = 1), xDomain)
#' 
#' # BIDIMENSIONAL RV EXAMPLES
#' 
#' # Using pdf provided by the user
#' pdf2 <- function(x, y) { (0 <= x & x <= 1 & 0 <= y & y <= 1) * (2 \* (1-x)) }
#' 
#' yDomain <- seq(-1, 1, 0.1)
#' xDomain <- seq(-1, 1, 0.1)
#' 
#' plot_fun(pdf2, xDomain, yDomain)
#' 
#' # Using cdf provided by the user
#' 
#' cdf2 <- function(x, y) { integral(pdf2, bounds = list(x = c(-Inf, x), y = c(-Inf, y)))$value }
#' plot_fun(cdf2, xDomain, yDomain)
#' 
#' # Using normal distribution
#' domain <- seq(-5, 5, 0.1)
#' value_pairs <- expand.grid(x = domain, y = domain)
#' 
#' library(mvtnorm)
#' 
#' plot_fun(dmvnorm(x = value_pairs), xDomain = domain, yDomain = domain)
#'
plot_fun <- function(fun, xDomain, yDomain = c()) {
  if (length(yDomain) == 0) {
    if (is.function(fun))
      fun <- sapply(xDomain, fun)
    
    plot(xDomain, fun, type="l", col="red", lwd = 3)
  }
  else {
    if (is.function(fun)) {
      xT = rep(xDomain, each=length(yDomain))
      yT = rep(yDomain, length(xDomain))
      
      fun <- mapply(fun, xT, yT)
    }
    
    if (!is.matrix(fun))
      fun <- matrix(fun, ncol = length(xDomain), byrow=TRUE)
    
    fig <- plot_ly(x = xDomain, y = yDomain, z = fun)
    fig <- fig %>% add_surface()
    
    fig
  }
}
