#' #' Create simulated data
#' #' @param n Population cor-value
#' #' @param mu Population cor-value
#' #' @param sd Population cor-value
#' #' @param cormatrix Population cor-value
#' #' @param empirical Population corvalue
#' #' @return Data frame with desired properties
#' #' @export
#' mvrnorm_cor <- function(n = 1, mu, sd, cormatrix, empirical = FALSE) {
#'
#'      sdmatrix <- sd %o% sd
#'
#'      if (!all(dim(sdmatrix) == dim(cormatrix))) {
#'           print("Error: sd list and cormatrix not compatible")
#'           return(-1)
#'      }
#'
#'      Sigma <- cormatrix * sdmatrix
#'
#'      dataout <- MASS::mvrnorm(n = n,
#'                               mu = mu,
#'                               Sigma = Sigma,
#'                               empirical = empirical)
#'
#'  return(dataout)
#' }
