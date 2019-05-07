#' Obtain the confidence interval for a given sample correlations
#' @param r Sample correlation
#' @param n Sample size
#' @param level Confidence level (0 to 1), default .95
#' @param show.message Show console message describing CI
#' @export
ci.r <- function(r, n, level = .95, show.message = TRUE) {
  alpha_level_half = (1 - level)/2
  LLz <- r_to_z(r) - qnorm(1 -alpha_level_half) * r_to_z_se(n)
  ULz <- r_to_z(r) + qnorm(1 -alpha_level_half) * r_to_z_se(n)
  LL <- z_to_r(LLz)
  UL <- z_to_r(ULz)

  if (show.message == TRUE) {
    message(sprintf("Sample correlation: %1.2f", r))
    message(sprintf("%1.0f%% CI lower population correlation: %1.2f", level*100, LL))
    message(sprintf("%1.0f%% CI upper population correlation: %1.2f", level*100, UL))
  }

  return(c(LL, UL))
}

#' Obtain the range of sample correlations for a given population correlation and sample size
#' @param pop.r Population correlation
#' @param n Sample size
#' @param level Confidence level (0 to 1), default .95
#' @export
pop.r.sampling.bounds <- function(pop.r, n, level = .95) {
  r_values <- ci.r(r= pop.r, n = n, level = level, show.message = FALSE)

  alpha_half_percent <- ((1 - level)/2)*100

  message(sprintf("Population correlation: %1.2f", pop.r))
  message(sprintf("Range: 95%% sample correlations between %1.2f and %1.2f", r_values[1], r_values[2]))
  message(sprintf("%1.1f%% of sample correlations greater than %1.2f", alpha_half_percent, r_values[2]))
  message(sprintf("%1.1f%% of sample correlations less than %1.2f", alpha_half_percent, r_values[1]))

  return(r_values)
}



