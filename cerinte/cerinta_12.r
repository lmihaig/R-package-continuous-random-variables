# 12)Construirea sumei si diferentei a doua variabile aleatoare continue
# independente(folositi formula de convolutie)

#' Calculates the difference of two objects of type cRV
#'
#' @name difCRV
#' @param cRV1 The first continous random variable
#' @param cRV2 The second continous random variable
#' @return The difference of the two continous random variables
difCRV <- function(cRV1, cRV2) {
    if (class(cRV1) != "cRV" || class(cRV2) != "cRV") {
        warning("Expected cRV object")
    }
    fun1 <- attr(cRV1, "pdf")
    fun2 <- attr(cRV2, "pdf")
    function(t) {
        integrate(
            f = function(r) {
                fun1(r) * fun2(t - r)
            },
            lower = -Inf,
            upper = Inf
        )$value
    }
}

#' Calculates the sum of two objects of type cRV
#'
#' @name sumCRV
#' @param cRV1 The first continous random variable
#' @param cRV2 The second continous random variable
#' @return The sum of the two continous random variables
sumCRV <- function(cRV1, cRV2) {
    if (class(cRV1) != "cRV" || class(cRV2) != "cRV") {
        warning("Expected cRV object")
    }
    fun1 <- attr(cRV1, "pdf")
    fun2 <- attr(cRV2, "pdf")
    function(t) {
        integrate(
            f = function(r) {
                fun1(r) * fun2(r - t)
            },
            lower = -Inf,
            upper = Inf
        )$value
    }
}
