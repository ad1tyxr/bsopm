compute_d1_d2 <- function(S0, K, T, r, sigma) {
  d1 <- (log(S0 / K) + (r + 0.5 * sigma^2) * T) / (sigma * sqrt(T))
  d2 <- d1 - sigma * sqrt(T)
  list(d1 = d1, d2 = d2)
}

# Quick test
print(compute_d1_d2(S0 = 100, K = 100, T = 1, r = 0.05, sigma = 0.2))
