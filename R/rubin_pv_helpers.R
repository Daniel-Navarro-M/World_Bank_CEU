# =============================================================================
# Rubin PV combining helpers
# =============================================================================
# These functions pool results across multiple plausible values (PVs) using
# Rubin's rules:
#   q_bar = average of point estimates
#   W     = average of within-imputation variances (here: mean(se^2))
#   B     = variance of point estimates across PVs
#   T     = W + (1 + 1/M) * B
#   se    = sqrt(T)
#
# Conventions:
# - `est` is a vector of point estimates (length = number of PVs).
# - `se`  is a vector of standard errors corresponding to each PV estimate.
# - `m`   is the effective number of PVs used for the (1+1/m) factor. By
#         default it is `length(est)`, which matches the project's existing code.

rubin_combine <- function(est, se, m = length(est)) {
  q_bar <- mean(est, na.rm = TRUE)
  W <- mean(se^2, na.rm = TRUE)
  B <- if (m > 1) stats::var(est, na.rm = TRUE) else 0
  T <- W + (1 + 1 / m) * B
  list(beta = q_bar, se = sqrt(T))
}

