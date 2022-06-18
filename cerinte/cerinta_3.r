# 3) Crearea unui obiect de tip variabila aleatoare continua pornind de la o densitate de 
# probabilitate introdusa de utilizator. Functia trebuie sa aiba optiunea pentru variabile 
# aleatoare unidimensionale si respectiv bidimensionale.

library(cubature)
library(calculus)

#' Check if a function can be the pdf of a random variable with a joint distribution
#' 
#' @name is_joint_pdf
#' @param func  Function to be tested
#' @return A boolean value representing wether func is the pdf of a bidimensional random variable
is_joint_pdf <- function(func) {
  if (!is.function(func)) stop("Parameter func has to be a function.")
  if (length(formals(func)) != 2) return(FALSE)
  
  xs <- rep(seq(-1e+6, 1e+6, len = 5000), each=5000)
  ys <- rep(seq(-1e+6, 1e+6, len = 5000), 5000)
  
  if (all(func(xs, ys) >= 0)) {
    integ <- integral(func, bounds = list(x=c(-Inf,Inf), y=c(-Inf,Inf)), vectorize = TRUE)
    if (abs(integ$value - 1) <= integ$error) return(TRUE)
  }
  
  return(FALSE)
}


get_cdf_from_pdf <- function(pdf) {
  if (length(formals(pdf)) == 1) {
    return(function(x) {
      integrate(pdf, lower=-Inf, upper=x)$value
    })
  }
  else if (length(formals(pdf)) == 2) {
    return(function(x, y) {
      integral(pdf, bounds = list(x=c(-Inf, x), y=c(-Inf, y)))$value
    })
  }
}

#' Create a continuous random variable starting from a probability density function
#' 
#' @name cRV
#' @param pdf Probability density function. Can either be a univariate or a bivariate pdf. Must allow vectorization.
#' @return Continuous random variable
#' @examples 
#' # Define a valid pdf
#' pdf1 <- function(x) { (0 <= x & x <= 1) * (3\*x^2) }
#' 
#' # Create a unidimensional continuous random variable with pdf1
#' X <- cRV(pdf1)
#' 
#' # Define a valid pdf with two parameters
#' pdf2 <- function(x) { (0 <= x & x <= 1 & 0 <= y & y <= 1) * (2 \* (1-x)) }
#' 
#' # Create a bidimensional continuous random variable with pdf2
#' Y <- cRV(pdf2)
cRV <- function(pdf) {
  if (!is.function(pdf)) stop("PDF must be a function.")
  
  # Check if function takes one or two parameters
  if (length(formals(pdf)) == 1) {
    if (!is_pdf(pdf)) stop("Provided function is not a valid PDF.")
  }
  else if (length(formals(pdf)) == 2) {
    if (!is_joint_pdf(f)) stop("Provided function is not a valid PDF.")
  }
  else stop("Provide a function that takes either one or two variables.")
  
  class(pdf) <- "cRV"
  attr(pdf, "pdf") <- pdf
  attr(pdf, "cdf") <- get_cdf_from_pdf(pdf)
  
  return(pdf)
}



# Obs:
#   Pachetul discreteRV folosit ca inspiratie