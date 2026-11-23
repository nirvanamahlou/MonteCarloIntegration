install.packages("ggplot2")
library(ggplot2)

dens <- function(x) dexp(x)
rnd  <- function(n) rexp(n)
fun  <- function(x) sin(x^2)

L <- integrate(function(x) fun(x)*dens(x),
               0, Inf,
               rel.tol    = 1e-12,
               subdivisions = 2e5)$value
cat("L =", L, "\n")

pr   <- function(x) dexp(x, rate = 0.8)
prnd <- function(n) rexp(n, rate = 0.8)

Ns  <- c(200, 2000, 20000, 200000)
out <- data.frame(N=integer(),
                  method=character(),
                  est=numeric(),
                  err=numeric(),
                  stringsAsFactors=FALSE)

for (n in Ns) {
  x  <- rnd(n)
  e1 <- mean(fun(x))
  out <- rbind(out,
               data.frame(N=n, method="MC", est=e1, err=abs(e1-L)))
  
  y  <- prnd(n)
  w  <- dens(y)/pr(y)
  e2 <- mean(fun(y)*w)
  out <- rbind(out,
               data.frame(N=n, method="IS", est=e2, err=abs(e2-L)))
}

print(out)

ggplot(out, aes(log10(N), est, colour=method)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept=L, linetype="dashed") +
  labs(x="log10(N)", y="Estimate",
       title="MC vs IS for ∫ e^{-x} sin(x^2) dx") +
  theme_minimal()

