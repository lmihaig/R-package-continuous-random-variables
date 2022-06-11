# 12)Construirea sumei si diferentei a doua variabile aleatoare continue
# independente(folositi formula de convolutie)


sumCRV <- function(cRV1, cRV2) {
    function(t) {
        integrate(
            f = function(r) {
                cRV1$pdf(r) * cRV2$pdf(t - r)
            },
            lower = -Inf,
            upper = Inf
        )$value
    }
}

sumCRV <- function(cRV1, cRV2) {
    function(t) {
        integrate(
            f = function(r) {
                cRV1$pdf(r) * cRV2$pdf(r - t)
            },
            lower = -Inf,
            upper = Inf
        )$value
    }
}
