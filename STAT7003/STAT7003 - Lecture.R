# STAT 7003 R code (yr 2015-16 sem 1)
library(manipulate)

# Ex 2.6 - Solve equation of integration
integrand <- function(x, c)
    c * (2 * x - x ^ 2)
eq <-
    function(c) {
        integrate(integrand,
                  c = c,
                  lower = 0,
                  upper = 2)$value - 1
    }
solution <- uniroot(eq, c(0, 1))
integrate(integrand,
          c = solution,
          lower = 0.5,
          upper = 1)$value

# Ex 2.13 Double integration
integrate(function(y) {
    sapply(y, function(y) {
        integrate(function(x)
            4 * x * (1 - y), 0, 1)$value
    })
}, 0, 1)

# Ex 3.2 Expected value
f <- function(x)
    3 / 2 * x ^ 2 + x
integrand <-  function(x)
    x * f(x)
integrate(integrand, 0, 1)$value

# 4.1 Uniform distribution
manipulate({
    x <- seq(a, b, length.out = 100)
    f <- function(x)
        1 / (b - a)
    plot(
        x,
        dunif(x, a, b),
        type = "l",
        xlim = c(0, 30),
        ylim = c(0, 1)
    )
    text(0.1,
         1,
         labels = sprintf("Mean: %0.2f", (a + b) / 2),
         adj = 0)
    text(0.1,
         0.9,
         labels = sprintf("Variance: %0.2f", (b - a) ^ 2 / 12),
         adj = 0)
    text(
        0.1,
        0.8,
        labels = sprintf(
            "1st cen moment: %0.2f",
            integrate(function(x)
                x * f(x), a, b)$value
        ),
        adj = 0
    )
    text(
        0.1,
        0.7,
        labels = sprintf(
            "2nd cen moment: %0.2f",
            integrate(function(x)
                x ^ 2 * f(x), a, b)$value -
                integrate(function(x)
                    x * f(x), a, b)$value ^ 2
        ),
        adj = 0
    )
},
a = slider(0, 10),
b = slider(20, 30))

# 4.2 Binomial Distribution
manipulate({
    x <- 0:n
    f <- function(x)
        choose(n,x) * p ^ x * (1-p) ^ (n-x)
    plot(
        x,
        dbinom(x, n, p),
        type = "h",
        xlim = c(0, n),
        ylim = c(0, 0.5)
    )
},
n = slider(0, 100, step = 10),
p = slider(0, 1, step = 0.1))

# 4.3 Negative Binominal Distribution
manipulate({
    x <- 0:40
    f <- function(x)
        p * (1-p) ^ (x - 1)
    plot(
        x,
        dgeom(x, p),
        type = "h",
        xlim = c(0, 40),
        ylim = c(0, 0.5)
    )
},
p = slider(0.1, 1, step = 0.1))

# 4.4 Hypereometric Distribution
manipulate({
    x <- 0:min(n,m)
    plot(
        x,
        dhyper(x, m, N-m, n),
        type = "h",
        xlim = c(0, N),
        ylim = c(0, 1)
    )
},
N = slider(1, 100),
m = slider(1, 100),
n = slider(1, 100))

# 4.5 Poisson
manipulate({
    x <- 0:100
    plot(
        x,
        dpois(x, lambda),
        type = "h",
        xlim = c(0, 100),
        ylim = c(0, 1)
    )
},
lambda = slider(1, 100))

# Check out equivalence of different parameterization
dpois(10,10)
sum(dpois(0:10,5) *  dpois(10:0,5))

# 4.6 Exponential