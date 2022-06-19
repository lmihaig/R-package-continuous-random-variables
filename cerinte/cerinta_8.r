# 8) Afisarea unei "fise de sinteza" care sa contina informatii de baza despre respectiva
# repartitie(cu precizarea sursei informatiei!). Relevant aici ar fi sa precizati pentru ce e
# folosita in mod uzual acea repartitie, semnificatia parametrilor, media, dispersia etc.


normal <- function() {
    x <- "Normal distribution (Gaussian distribution), for a single such quantity; the most commonly used absolutely continuous distribution.
    Applications: Linear growth (e.g. errors, offsets)
    Notation: N(mu, sigma^2)
    Parameters: mu -> mean (location); sigma^2 -> varaince (squared scale)
    PDF: 1/(sigma*sqrt(2pi))*e^(-1/2 * ((x-mu)/sigma)^2)
    CDF: 1/2*[1+erf((x-mu)/(sigma*sqrt(2))]
    Mean: mu
    Median: mu
    Variance: sigma^2"
}


pareto <- function() {
    x <- "Pareto distribution, for a single such quantity whose log is exponentially distributed; the prototypical power law distribution
    Applications: Exponential growth (e.g. prices, incomes, populations)
    Parameters: xm -> scale; alpha -> shape
    PDF: alpha*xm^alpha/x^(alpha+1)
    CDF: 1 - (xm - x)^alpha
    Mean: Inf, alpha<=1; alpha*xm/(alpha-1), alpha>1
    Median: xm*2^(1/alpha)
    Variance: Inf, alpha<=2; xm^2*alpha/((alpha-1)^2(alpha-2)), alpha>2"
}

uniform <- function() {
    x <- "Continuous uniform distribution, for absolutely continuously distributed values
    Applications: Uniformly distributed quantities
    Notation: U(a,b)
    Parameters: -Inf < a < b < Inf
    PDF: 1/(b-a), a < x < b; 0, otherwise
    CDF: 0, x<a; (x-a)/(b-a), a < x < b; 1, x>b
    Mean: 1/2*(a+b)
    Variance: 1/12 * (b-a)^2"
}

bernoulli <- function() {
    x <- "Bernoulli distribution, for the outcome of a single Bernoulli trial (e.g. success/failure, yes/no)
    Applications: Bernoulli trials (yes/no events, with a given probability)
    Parameters: 0 <= p <= 1; q = 1-p
    PMF: q = 1-p, k=0; p, k=1
    CDF: 0, k<0; 1-p, 0<=k<1; 1, k>=1
    Mean: p
    Variance: pq"
}

hypergeometric <- function() {
    x <- "Hypergeometric distribution, for the number of positive occurrences (e.g. successes, yes votes, etc.) given a fixed number of total occurrences, using sampling without replacement
    Applications: Sampling schemes over a finite population yes/no events, with a given probability
    Parameters: 0 <= N < Inf; 0 <= K < N; 0 <= n < N
    PMF: kCK * (N-K)C(n-k)/(NCn)
    Mean: n*K/N
    Variance: n*K/N * (N-K)/N * (N-n)/(N-1)
    "
}

multinomial <- function() {
    x <- "Multinomial distribution, for the number of each type of categorical outcome, given a fixed number of total outcomes; a generalization of the binomial distribution
    Applications: Categorical outcomes (events with K possible outcomes)
    Parameters: n > 0, number of trials; k > 0, number of mutually exclusive events; p1....pn event probabilities
    PMF: n!/(x1!*...*xk!) *p1^x1 *...* pk^xk
    Mean: E(Xi) = npi
    Variance: Var(Xi) = npi(1-pi)"
}

i_poisson <- function() {
    x <- "Poisson distribution, for the number of occurrences of a Poisson-type event in a given period of time
    Applications: Poisson process (events that occur independently with a given rate)
    Notation: Pois(lambda)
    Parameters: 0 < lambda < Inf, rate
    PMF: lambda^k * e^-lambda / k!
    Mean: lambda
    Variance: lambda"
}

exponential <- function() {
    x <- "Exponential distribution, for the time before the next Poisson-type event occurs
    Applications: The exponential distribution occurs naturally when describing the lengths of the inter-arrival times in a homogeneous Poisson process.
    Parameters: lambda > 0 , rate
    PDF: lamdba*e^(-lambda*x)
    CDF: 1- e ^(-lamdba*x)
    Mean: 1/lamdba
    Median: ln(2)/lambda
    Variance: 1/lambda^2"
}

rayleigh <- function() {
    x <- "Rayleigh distribution, for the distribution of vector magnitudes with Gaussian distributed orthogonal components. Rayleigh distributions are found in RF signals with Gaussian real and imaginary components.
    Applications: Absolute values of vectors with normally distributed components
    Parameters: sigma > 0, scale
    PDF: x/sigma^2 * e^(-x^2/2sigma^2)
    Mean: sigma*sqrt(pi/2)
    Median: sigma*sqrt(2*ln(2))
    Variance: (4-pi)/2 * sigma^2"
}

chisquared <- function() {
    x <- "Chi-squared distribution, the distribution of a sum of squared standard normal variables; useful e.g. for inference regarding the sample variance of normally distributed samples
    Applications: Normally distributed quantities operated with sum of squares
    Notation: X^2(k)
    Parameters: k != 0, degrees of freedom
    Mean: k
    Variance: 2k"
}

info <- function() {
    message("   1.Normal
            2.Pareto
            3.Uniform
            4.Bernoulli
            5.Hypergeometric
            6.Multinomial
            7.Poisson
            8.Exponential
            9.Rayleigh
            10.Chi-squared
    ")
    option <- as.numeric(readline("Enter choice: "))
    x <- switch(option,
        normal(),
        pareto(),
        uniform(),
        bernoulli(),
        hypergeometric(),
        multinomial(),
        i_poisson(),
        exponential(),
        rayleigh(),
        chisquared()
    )
    message(x)
    message("Source: Curs + https://en.wikipedia.org/wiki/Probability_distribution")
}
