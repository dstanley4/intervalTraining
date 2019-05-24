#' Obtain the confidence interval for a given sample correlations
#' @param r Sample correlation
#' @param n Sample size
#' @param level Confidence level (0 to 1), default .95
#' @param show_message Show console message describing CI
#' @export
ci.r <- function(r, n, level = .95, show_message = TRUE) {
  alpha_level_half = (1 - level)/2
  LLz <- r_to_z(r) - qnorm(1 -alpha_level_half) * r_to_z_se(n)
  ULz <- r_to_z(r) + qnorm(1 -alpha_level_half) * r_to_z_se(n)
  LL <- z_to_r(LLz)
  UL <- z_to_r(ULz)

  if (show_message == TRUE) {
    message(sprintf("Sample correlation: %1.2f", r))
    message(sprintf("%1.0f%% CI lower population correlation: %1.2f", level*100, LL))
    message(sprintf("%1.0f%% CI upper population correlation: %1.2f", level*100, UL))
  }

  return(c(LL, UL))
}

#' Obtain the range of sample correlations for a given population correlation and sample size
#' @param pop_r Population correlation
#' @param n Sample size
#' @param level Confidence level (0 to 1), default .95
#' @export
pop_r_sampling_bounds <- function(pop_r, n, level = .95) {
  r_values <- ci_r(r= pop_r, n = n, level = level, show_message = FALSE)

  alpha_half_percent <- ((1 - level)/2)*100

  message(sprintf("Population correlation: %1.2f", pop_r))
  message(sprintf("Range: 95%% sample correlations between %1.2f and %1.2f", r_values[1], r_values[2]))
  message(sprintf("%1.1f%% of sample correlations greater than %1.2f", alpha_half_percent, r_values[2]))
  message(sprintf("%1.1f%% of sample correlations less than %1.2f", alpha_half_percent, r_values[1]))

  return(r_values)
}


#' Obtain the range of sample correlations for a given population correlation and sample size
#' @param pop_r Population correlation
#' @param n Sample size
#' @param level Confidence level (0 to 1), default .95
#' @export
bounds.r <- function(samples, level = .95) {

  alpha_half <- ((1 - level)/2)

  r <- sort(samples$r)
  LLcount <- round(alpha_half*length(r)) + 1
  ULcount <- round((1-alpha_half)*length(r))
  sampling_lower_bound <- r[LLcount]
  sampling_upper_bound <- r[ULcount]


  K <- length(r)
  n <- samples$n[1]
  pop.r <- samples$pop.r[1]


  cat(sprintf("Population correlation: %1.2f\nN = %1.0f\nNumber of samples = %1.0f\n", pop.r, n, K))
  cat(sprintf("Range sample r: %1.2f to %1.2f\n", range(r)[1], range(r)[2]))
  cat(sprintf("95%% sample r: %1.2f to %1.2f\n", sampling_lower_bound, sampling_upper_bound))
  cat(sprintf("95%% width sample r: %1.2f\n", abs(sampling_upper_bound - sampling_lower_bound)))
  cat(sprintf("%1.1f%% of sample r >  %1.2f\n", alpha_half*100, sampling_upper_bound))
  cat(sprintf("%1.1f%% of sample r <  %1.2f\n", alpha_half*100, sampling_lower_bound))

  return("-----------------------")
}




