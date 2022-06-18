# 11) Pornind de la densitatea comuna a doua variabile aleatoare continue, construirea 
# densitatilor marginale si a densitatilor conditionate.

#' Get marginal distribution for X from a bivariate pdf
#' @param pdf must be a probability density fonction for a bivariate random variable
#' @examples 
#' pdf2 <- function(x, y) { (0 <= x & x <= 1 & 0 <= y & y <= 1) \* (2/3 \* (x + 2\*y)) }
#' f_X <- X_marginal_dist(pdf2)
#' f_X(0.4)
X_marginal_dist <- function(pdf) {
  if (length(formals(pdf)) != 2)
    stop('Pdf must take two arguments.')
  if (!is_joint_pdf(pdf))
    stop('Parameter pdf must be a joint pdf.')
  
  function(x) {
    new_f <- function(y) { pdf(x, y) }
    integral(new_f, bound = list(y= c(-Inf, Inf)))$value
  }
}

#' Get marginal distribution for X from a bivariate pdf
#' @param pdf must be a probability density fonction for a bivariate random variable
#' @examples 
#' pdf2 <- function(x, y) { (0 <= x & x <= 1 & 0 <= y & y <= 1) \* (2/3 \* (x + 2\*y)) }
#' f_Y <- Y_marginal_dist(pdf2)
#' f_Y(0.2)
Y_marginal_dist <- function(pdf) {
  if (length(formals(pdf)) != 2)
    stop('Pdf must take two arguments.')
  if (!is_joint_pdf(pdf))
    stop('Parameter pdf must be a joint pdf.')
  
  function(y) {
    new_f <- function(x) { pdf(x, y) }
    integral(new_f, bound = list(x= c(-Inf, Inf)))$value
  }
}

#' Get a conditional distribution from a bivariate pdf
#' @param pdf must be a probability density fonction for a bivariate random variable
#' @param x fixed value of X. Providing x will result in returning f(Y | X = x)
#' @param y fixed value of Y. Providing y will result in returning f(X | Y = x)
#' @examples 
#' pdf2 <- function(x, y) { (0 <= x & x <= 1 & 0 <= y & y <= 1) \* (2/3 \* (x + 2\*y)) }
#' 
#' # Conditional distribution of X given Y
#' f_XY <- cond_distribution(pdf2, y = 0.3)
#' f_XY(0.6)
#' 
#' # Conditional distribution of Y given X
#' f_YX <- cond_distribution(pdf2, x = 0.1)
#' f_YX(0.75)
cond_distribution <- function(pdf, x = NULL, y = NULL) {
  if ((is.null(x) && is.null(y)) || !(is.null(x) || is.null(y))) {
    stop('Must either provide a value for X or a value for Y.')
  }
  
  if (!is_joint_pdf(pdf))
    stop('Parameter pdf must be a joint pdf.')
  
  if (is.null(x)) {
    # conditional given y f(X | Y = y)
    mdist <- Y_marginal_dist(pdf)
    denom <- mdist(y)
    
    function(x) {
      pdf(x, y) / denom
    }
  }
  else {
    # conditional given x: f(Y | X = x)
    mdist <- X_marginal_dist(pdf)
    denom <- mdist(x)
    
    function(y) {
      pdf(x, y) / denom
    }
  }
}