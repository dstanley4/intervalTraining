#remember to account for missing data

StatSamplingDistributionOutline <- ggplot2::ggproto("StatSamplingDistributionOutline", ggplot2::Stat,
                              required_aes = c("x", "y"),
                              compute_group = function(data, scales, level, fill, pop.r, n, pi.LL, pi.UL) {
                                calc_distribution_path_data(data,scales)
                              }
)

StatSamplingDistributionCapture <- ggplot2::ggproto("StatSamplingDistributionCapture", ggplot2::Stat,
                             required_aes = c("x", "y"),

                             compute_group = function(data, scales, level, fill, pop.r, n, pi.LL, pi.UL) {
                               calc_distribution_polygon_data(data, scales)
                             }

)


StatIntervalPI <- ggplot2::ggproto("StatSamplingDistributionCapture", ggplot2::Stat,
                                   required_aes = c("x", "y"),
                                   compute_group = function(data, scales, level, fill, pop.r, n, pi.LL, pi.UL, height = .15, size = .8) {
                                     calc_pi_interval_data(data, scales)
                                   }

)

StatIntervalCenter<- ggplot2::ggproto("StatSamplingDistributionCapture", ggplot2::Stat,
                                   required_aes = c("x", "y"),
                                   compute_group = function(data, scales, level, fill) {
                                     return(data)
                                   }

)

#remove fill and colour from above functions?


#' This is the documentation for stat_catseye
#' @param mapping asdfasd
#' @param data adfasd
#' @param geom asdfasd
#' @param position asdfasd
#' @param na.rm adsfasdf
#' @param show.legend asdfads
#' @param inherit.aes adfasd
#' @param pop.r asdf
#' @param n asdf
#' @param pi.LL asdf
#' @param pi.UL asdf
#' @param ... adfasdf
#' @export
stat_piplot <- function(mapping = NULL, data = NULL, geom = "polygon",
                         position = "identity", na.rm = FALSE, show.legend = NA,
                         inherit.aes = TRUE, pop.r, n, pi.LL, pi.UL, ...) {

  list(
    ggplot2::layer(
      stat = StatSamplingDistributionOutline, data = data, mapping = mapping, geom = "path",
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, ...)
    ),
    ggplot2::layer(
      stat = StatSamplingDistributionCapture, data = data, mapping = mapping, geom = geom,
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, ...)
    ),
    ggplot2::layer(
      stat = StatIntervalPI, data = data, mapping = mapping, geom = "errorbarh",
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, ...)
    ),
    ggplot2::layer(
      stat = StatIntervalCenter, data = data, mapping = mapping, geom = "point",
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, ...)
    )
  )
}


r_to_z <- function(r) {
  zvalue <- atanh(r)
  return(zvalue)
}

z_to_r <- function(z) {
  rvalue <- tanh(z)
  return(rvalue)
}

r_to_z_se <- function(N) {
  se_out <-  1 / sqrt(N-3)
  return(se_out)
}


r_pdf_from_zs <- function(z_values, dist_m, dist_se) {
  r_x <- z_to_r(z_values)
  pdf_each_z <- dnorm(x = z_values, mean = dist_m, sd = dist_se)
  pdf_out <- data.frame(r = r_x, pdf = pdf_each_z)
  return(pdf_out)
}

path_density_points <-function(pop.r, n, number.points  = 100) {
  dist_m <- r_to_z(pop.r)
  dist_se <- r_to_z_se(n)

  z_sampling_lower_end <-qnorm(.001, mean = dist_m, sd = dist_se)
  z_sampling_upper_end <-qnorm(.999, mean = dist_m, sd = dist_se)
  z_x <- seq(z_sampling_lower_end, z_sampling_upper_end, length.out = number.points)

  density_values_out <- r_pdf_from_zs(z_values = z_x, dist_m = dist_m, dist_se = dist_se)

  n_rows <- nrow(density_values_out)
  first_row <- density_values_out[1,]
  last_row <- density_values_out[n_rows,]
  first_row[1,2] <- 0
  last_row[1,2] <- 0
  density_values_out <- rbind(first_row, density_values_out, last_row, first_row)


  return(density_values_out)
}

polygon_density_points <- function(pop.r, n, LL, UL) {
  dist_m <- r_to_z(pop.r)
  dist_se <- r_to_z_se(n)

  z_LL <- r_to_z(LL)
  z_UL <- r_to_z(UL)
  number.points <- 20
  z_x <- seq(z_LL, z_UL, length.out = number.points)
  pdf_out <- r_pdf_from_zs(z_values = z_x, dist_m = dist_m, dist_se = dist_se)


  n_rows <- nrow(pdf_out)
  first_row <- pdf_out[1,]
  last_row <- pdf_out[n_rows,]
  first_row[1,2] <- 0
  last_row[1,2] <- 0
  pdf_out <- rbind(first_row, pdf_out, last_row)

  return(pdf_out)
}

path_polygon_data <- function(pop.r, n, LL, UL, y, scalefactor = .60) {
  path_df <- path_density_points(pop.r = pop.r, n = n)
  polygon_df <- polygon_density_points(pop.r = pop.r, n = n, LL = LL, UL = UL)
  path_y_max <- max(path_df$pdf)
  path_df$pdf <- path_df$pdf / path_y_max * scalefactor
  polygon_df$pdf <- polygon_df$pdf / path_y_max * scalefactor

  path_df$pdf <- path_df$pdf + y + .15
  polygon_df$pdf <- polygon_df$pdf + y + .15

  output <- list()
  output$path <- path_df
  output$polygon <- polygon_df

  return(output)
}



calc_distribution_path_data <- function(data, scales) {
  sample.r <- data$x[1] # pop.r
  y <- data$y[1] # vertical position
  group <- data$group[1]
  PANEL <- data$PANEL[1]

  path_data = path_polygon_data(pop.r = data$pop.r[1], n = data$n[1], LL = data$pi.LL[1], UL = data$pi.UL[1], y = y)$path

  df_out <- data.frame(x = path_data$r, y = path_data$pdf, PANEL = PANEL, group = group)

  #print("df out path")
  #print(df_out)
  return(df_out)
}

calc_distribution_polygon_data <- function(data, scales) {
  sample.r <- data$x[1] # pop.r
  y <- data$y[1] # vertical position
  group <- data$group[1]
  PANEL <- data$PANEL[1]

  polygon_data = path_polygon_data(pop.r = data$pop.r[1], n = data$n[1], LL = data$pi.LL[1], UL = data$pi.UL[1], y = y)$polygon

  df_out <- data.frame(x = polygon_data$r, y = polygon_data$pdf, PANEL = PANEL, group = group)

  print("df out polygon")
  print(df_out)
  return(df_out)
}

calc_pi_interval_data <- function(data, scales) {
  print("pi whisker data")
  print(data)
  df_out <- data.frame(x = data$x[1], y = data$y[1], xmin = data$pi.LL, xmax = data$pi.UL, height = .15, size = .5, PANEL = data$PANEL[1], group = data$group[1])
  print("pi out")
  print(df_out)
  return(df_out)
}

