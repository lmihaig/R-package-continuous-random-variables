# 5) Calculul mediei, dispersiei si a momentelor initiale si centrate pana la ordinul 4(daca
# exista). Atunci cand unul dintre momente nu exista, se va afisa un mesaj corespunzator
# catre utilizator.



#' Returns the expectation (expected value, first moment, mean, average) for an object of type cRV
#'
#' @name expectation
#' @param cRV Is the continuous random variable for which we want to find the expectation.
#' @return The value of the integral to determine the expectation
#' @examples
#' rv <- cRV(function(x) {(0 <= x & x <= 1) * (3\*x^2)})
#' expectation(rv)
expectation <- function(cRV) {
    if (class(cRV) != "cRV") {
        warning("Expected cRV object")
    }
    tryCatch(
        {
            func <- cRV$pdf
            new_func <- function(x) {
                x * func(x)
            }
            return(integrate(new_func, lower = -Inf, upper = Inf)$value)
        },
        error = function(e) {
            warning("Expectation not found")
            warning(e$message)
            return(NULL)
        }
    )
}


#' Returns the variance for an object of type cRV
#'
#' @name variance
#' @param cRV Is the continuous random variable for which we want to find the variance.
#' @return The value of the integral to determine the variance
#' @examples
#' rv <- cRV(function(x) {(0 <= x & x <= 1) * (3\*x^2)})
#' variance(rv)
variance <- function(cRV) {
    if (class(cRV) != "cRV") {
        warning("Expected cRV object")
    }
    tryCatch(
        {
            func <- cRV$pdf
            new_func <- function(x) {
                ((x - expectation(func)^2) * func(x))
            }
            return(integrate(new_func, lower = -Inf, upper = Inf)$value)
        },
        error = function(e) {
            warning("Variance not found")
            warning(e$message)
            return(NULL)
        }
    )
}


#' Finds the fourth degree initial moments (if they exist) for an object of type cRV
#'
#' @name initial_moments
#' @param cRV Is the continuous random variable for which we want to find the initial moments
#' @return A list containing the first four initial moments
#' @examples
#' rv <- cRV(function(x) {(0 <= x & x <= 1) * (3\*x^2)})
#' initial_moments(rv)
initial_moments <- function(cRV) {
    # will return a list containing the 4 moments (if they exist)
    if (class(cRV) != "cRV") {
        warning("Expected cRV object")
    }
    moments_list <- list()
    for (i in 1:4) {
        tryCatch(
            {
                func <- cRV$pdf
                new_func <- function(x) {
                    (x^i) * func(x)
                }
                moments_list <- append(
                    moments_list,
                    integrate(new_func, lower = -Inf, upper = Inf)$value
                )
            },
            error = function(e) {
                warning("Moment not found for i=", i)
                warning(e$message)
            }
        )
    }
    return(moments_list)
}

#' Finds the fourth degree central moments (if they exist) for an object of type cRV
#'
#' @name central_moments
#' @param cRV Is the continuous random variable for which we want to find the central moments
#' @return A list containing the first four central moments
#' @examples
#' rv <- cRV(function(x) {(0 <= x & x <= 1) * (3\*x^2)})
#' central_moments(rv)
central_moments <- function(cRV) {
    # will return a list containing the 4 moments (if they exist)\
    if (class(cRV) != "cRV") {
        warning("Expected cRV object")
    }
    moments_list <- list()
    for (i in 1:4) {
        tryCatch(
            {
                func <- cRV$pdf
                new_func <- function(x) {
                    (x - expectation(func))^i * func(x)
                }
                moments_list <- append(
                    moments_list,
                    integrate(new_func, lower = -Inf, upper = Inf)$value
                )
            },
            error = function(e) {
                warning("Moment not found for i=", i)
                warning(e$message)
            }
        )
    }
    return(moments_list)
}

# BONUS
factorial_moments <- function(cRV) {
    # will return a list containing the 4 moments (if they exist)
    if (class(cRV) != "cRV") {
        warning("Expected cRV object")
    }
    moments_list <- list()
    for (i in 1:4) {
        tryCatch(
            {
                func <- cRV$pdf
                new_func <- function(x) {
                    (factorial(x) / factorial(x - i)) * func(x)
                }
                moments_list <- append(
                    moments_list,
                    integrate(new_func, lower = -Inf, upper = Inf)$value
                )
            },
            error = function(e) {
                warning("Moment not found for i=", i)
                warning(e$message)
            }
        )
    }
    return(moments_list)
}
