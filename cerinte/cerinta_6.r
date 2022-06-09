# 6) Calculul mediei si dispersiei unei variabile aleatoare g(X), unde X are o repartitie
# continua cunoscuta iar g este o functie continua precizata de utilizator.

expected_value <- function(g, func, lower = -Inf, upper = Inf) {
    tryCatch(
        {
            new_func <- function(x) {
                g(x) * func(x)
            }
            return(integrate(new_func, lower = lower, upper = upper)$value)
        },
        error = function(e) {
            warning("Mean not found")
            warning(e$message)
            return(NULL)
        }
    )
}


variance <- function(g, func, lower = -Inf, upper = Inf) {
    tryCatch(
        {
            exp_val <- expected_value(g, func, lower, upper)
            new_func <- function(x) {
                (g(x) - exp_val)^2 * func(x)
            }
            return(integrate(new_func, lower = lower, upper = upper)$value)
        },
        error = function(e) {
            warning("Variance not found")
            warning(e$message)
            return(NULL)
        }
    )
}
