# 2) Verificarea daca o functie introdusa de utilizator este densitate de probabilitate.

is_pdf <- function(func) {
    tryCatch(
        {
            if (!is.function(func)) stop("Parameter func has to be a function.")
            # generate a faux (-Inf, Inf) interval
            xs <- seq(-1e+6, 1e+6, len = 10000)

            # function must be positive
            if (all(func(xs) >= 0)) {

                # integral must be equal to 1 (with 1^-10 tolerance)
                integral <- integrate(func, lower = -Inf, upper = Inf)
                if (abs(integral$value - 1) < 1e-10) {
                    return(TRUE)
                }
            }
            return(FALSE)
        },
        error = function(e) {
            warning(e$message)
            return(FALSE)
        }
    )
}

# EX:
# f <- function(x) ifelse(x >= -1 & x <= 1, 1 - abs(x), 0)
# is_pdf(f)