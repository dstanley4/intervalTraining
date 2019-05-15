#' Calculate a number of sample correlations based on a specified population correlation
#' @param pop_r Population correlation. Do not use pop.data is you provide this value.
#' @param n Sample size for all samples If you use n, do not use n.min or n.max.
#' @param number_samples Number of samples to obtain
#' @param number_decimals Number of decimals to report in returned data frame
#' @return Data frame with sample correlations
#' @examples
#' get_r_samples(pop_r = .35, n = 100)
#' @export
get.r.samples <- function(pop_r = NA, n, number_samples = 10, number_decimals = 2) {

     Sigma <- diag(2)
     Sigma[1,2] <- pop_r
     Sigma[2,1] <- pop_r
     mu <- c(0,0)
     K <- number_samples
     data_samples <- get_true_scores_with_sampling_error_cov(Sigma, mu, n, K)

     rs <- rep(NA,number_samples)
     dfs <- rep(NA,number_samples)
     ts <- rep(NA,number_samples)
     in_interval <- rep(NA, number_samples)
     ps <- rep(NA,number_samples)
     LLs <- rep(NA,number_samples)
     ULs <- rep(NA,number_samples)
     pi_LLs <- rep(NA,number_samples)
     pi_ULs <- rep(NA,number_samples)
     pi_in_interval <- rep(NA, number_samples)
     ci_as_pi_in_interval <- rep(NA, number_samples)
     for (i in 1:number_samples) {
          x <- data_samples$x_true[,i]
          y <- data_samples$y_true[,i]
          r_info <- stats::cor.test(x,y)
          rs[i] <- round(r_info$estimate,number_decimals+1)
          LLs[i] <- round(r_info$conf.int[1],number_decimals)
          ULs[i] <- round(r_info$conf.int[2],number_decimals)
          in_interval[i] <- is_value_in_interval(pop_r, c(r_info$conf.int[1], r_info$conf.int[2]))
          ts[i] <- round(r_info$statistic,number_decimals)
          dfs[i] <- round(r_info$parameter,number_decimals)
          ps[i] <- round(r_info$p.value,5)
          pi_info <- predictionInterval::pi.r(r = rs[i], n = n, rep.n = n)
          pi_LLs[i] <- round(pi_info$lower_prediction_interval, number_decimals)
          pi_ULs[i] <- round(pi_info$upper_prediction_interval, number_decimals)
          if (i > 1) {
               pi_in_interval[i-1] <- is_value_in_interval(rs[i], c(pi_LLs[i-1], pi_ULs[i-1]))
               ci_as_pi_in_interval[i-1]  <- is_value_in_interval(rs[i], c(LLs[i-1], ULs[i-1]))
          }
     }
     xx<-1:number_samples
     sample_number <- xx
     data.out <- data.frame(sample_number, pop_r = pop_r, n = n, r =  rs, ci_LL = LLs, ci_UL = ULs, pi_LL = pi_LLs, pi_UL = pi_ULs, ci_captures_pop_r = in_interval, ci_captures_next_r = ci_as_pi_in_interval, pi_captures_next_r = pi_in_interval, p = ps)
     rownames(data.out) <- NULL


     pi_in_interval[i] <- is_value_in_interval(pop_r, c(r_info$conf.int[1], r_info$conf.int[2]))

     data.out <- tibble::as_tibble(data.out)


     return(data.out)
}



get_true_scores_with_sampling_error_cov <- function(Sigma, mu, n, K) {
     # This is the MASS::mvrnorm routine structured to eliminate
     # redundant calculations when repeated K times

     #Matrix approach for fast bivariate simulations

     p <- length(mu)
     eS <- eigen(Sigma, symmetric = TRUE)
     ev <- eS$values

     Xmulti <-  eS$vectors %*% diag(sqrt(pmax(ev, 0)), p)

     xs <- matrix(NA,n,K)
     ys <- matrix(NA,n,K)
     for (i in 1:K) {
          X <- matrix(rnorm(p * n), n)
          X2 <- Xmulti %*% t(X)
          X2 <- t(X2)
          xs[,i] <- X2[,1]
          ys[,i] <- X2[,2]
     }

     output <- list()
     output$x_true <- xs
     output$y_true <- ys
     return(output)
}


is_value_in_interval <- function(value, interval) {
        is_in_interval <- FALSE
        check_interval<-findInterval(value,sort(interval),rightmost.closed = TRUE)
        if (check_interval==1) {
                is_in_interval <- TRUE
        }
        return(is_in_interval)
}



#' Calculate the percentage of rows that are true in a column
#' @param x the column to be examined
#' @return The percent equal to TRUE
#' @export
percent.true <- function(x) {
     sum_TRUE <- sum(x, na.rm = TRUE)
     sum_length <- sum(!is.na(x))
     return(sum_TRUE/sum_length*100)
}



