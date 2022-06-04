# 5) Calculul mediei, dispersiei si a momentelor initiale si centrate pana la ordinul 4(daca
# exista). Atunci cand unul dintre momente nu exista, se va afisa un mesaj corespunzator
# catre utilizator.

expectation <- function(func) {
    tryCatch(
        {
            new_func <- function(x) {
                x * func(x)
            }
            return(integrate(new_func, lower = -Inf, upper = Inf)$value)
        },
        error = function(e) {
            warning("Average not found")
            warning(e$message)
            return(NULL)
        }
    )
}

deviation <- function(func) {
    tryCatch(
        {
            new_func <- function(x) {
                ((x - expectation(func)) * func(x))
            }
            return(integrate(new_func, lower = -Inf, upper = Inf)$value)
        },
        error = function(e) {
            warning("Dispersion not found")
            warning(e$message)
            return(NULL)
        }
    )
}

initial_moments <- function(func) {
    # will return a list containing the 4 moments (if they exist)
    moments_list <- list()
    for (i in 1:4) {
        tryCatch(
            {
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


central_moments <- function(func) {
    # will return a list containing the 4 moments (if they exist)
    moments_list <- list()
    for (i in 1:4) {
        tryCatch(
            {
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
factorial_moments <- function(func) {
    # will return a list containing the 4 moments (if they exist)
    moments_list <- list()
    for (i in 1:4) {
        tryCatch(
            {
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