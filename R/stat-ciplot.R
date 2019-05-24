#remember to account for missing data

StatSamplingDistributionLL <- ggplot2::ggproto("StatSamplingDistributionOutline", ggplot2::Stat,
                                               required_aes = c("x", "y"),
                                               compute_group = function(data, scales, level, fill, pop.r, n, ci.LL, ci.UL) {
                                                 calc_distribution_ciLL(data, scales)
                                               }
)

StatSamplingDistributionUL <- ggplot2::ggproto("StatSamplingDistributionOutline", ggplot2::Stat,
                                               required_aes = c("x", "y"),
                                               compute_group = function(data, scales, level, fill, pop.r, n, ci.LL, ci.UL) {
                                                 calc_distribution_ciUL(data,scales)
                                               }
)


StatIntervalCI <- ggplot2::ggproto("StatSamplingDistributionCapture", ggplot2::Stat,
                                   required_aes = c("x", "y"),
                                   compute_group = function(data, scales, level, fill, pop.r, n, ci.LL, ci.UL, height = .15, size = 1) {
                                     calc_ci_interval_data(data, scales, height, size)
                                   }

)


StatDistCenterUL<- ggplot2::ggproto("StatDistCenterUL", ggplot2::Stat,
                                    required_aes = c("x", "y"),
                                    compute_group = function(data, scales, level, fill, pop.r, n, ci.LL, ci.UL, height = .15, size = .8) {
                                      calc_distribution_centerUL(data, scales)
                                    }

)


StatDistCenterLL<- ggplot2::ggproto("StatDistCenterLL", ggplot2::Stat,
                                    required_aes = c("x", "y"),
                                    compute_group = function(data, scales, level, fill, pop.r, n, ci.LL, ci.UL, height = .15, size = .8) {
                                      calc_distribution_centerLL(data, scales)
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
#' @param ... adfasdf
#' @export
stat_ciplot <- function(mapping = NULL, data = NULL, geom = "path",
                        position = "identity", na.rm = FALSE, show.legend = NA,
                        inherit.aes = TRUE, pop.r, n, ci.LL, ci.UL, ...) {

  list(
    ggplot2::layer(
      stat = StatSamplingDistributionLL, data = data, mapping = mapping, geom = geom,
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, ...)
    ),
    ggplot2::layer(
      stat = StatSamplingDistributionUL, data = data, mapping = mapping, geom = geom,
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, ...)
    ),
    ggplot2::layer(
      stat = StatIntervalCI, data = data, mapping = mapping, geom = "errorbarh",
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, ...)
    ),
    ggplot2::layer(
      stat = StatDistCenterUL, data = data, mapping = mapping, geom = geom,
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, ...)
    ),
    ggplot2::layer(
      stat = StatDistCenterLL, data = data, mapping = mapping, geom = geom,
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


#####

calc_distribution_ciLL <- function(data, scales) {
  sample.r <- data$x[1] # pop.r
  y <- data$y[1] # vertical position
  group <- data$group[1]
  PANEL <- data$PANEL[1]

  path_other <- path_polygon_data(pop.r = data$ci.UL[1],
                                n = data$n[1],
                                LL = data$ci.LL[1],
                                UL = data$ci.UL[1],
                                y = y,
                                scale_to_other = FALSE)$path

  other_pdf_max <- max(path_other$pdf)
  print("LL other_pdf_max")
  print(other_pdf_max)


  path_data <- path_polygon_data(pop.r = data$ci.LL[1],
                                n = data$n[1],
                                LL = data$ci.LL[1],
                                UL = data$ci.UL[1],
                                y = y,
                                other_pdf_max = other_pdf_max)$path

  df_out <- data.frame(x = path_data$r,
                       y = path_data$pdf,
                       PANEL = PANEL,
                       group = group)

  #print("df out path")
  #print(df_out)
  return(df_out)
}

calc_distribution_ciUL <- function(data, scales) {
  sample.r <- data$x[1] # pop.r
  y <- data$y[1] # vertical position
  group <- data$group[1]
  PANEL <- data$PANEL[1]

  path_other <- path_polygon_data(pop.r = data$ci.LL[1],
                                  n = data$n[1],
                                  LL = data$ci.LL[1],
                                  UL = data$ci.UL[1],
                                  y = y,
                                  scale_to_other = FALSE)$path

  other_pdf_max <- max(path_other$pdf)
  print("UL other_pdf_max")
  print(other_pdf_max)

  path_data = path_polygon_data(pop.r = data$ci.UL[1],
                                n = data$n[1],
                                LL = data$ci.LL[1],
                                UL = data$ci.UL[1],
                                y = y,
                                other_pdf_max = other_pdf_max)$path

  df_out <- data.frame(x = path_data$r,
                       y = path_data$pdf,
                       PANEL = PANEL,
                       group = group)

  #print("df out path")
  #print(df_out)
  return(df_out)
}
#
#
# calc_distribution_ciUL <- function(data, scales) {
#   sample.r <- data$x[1] # pop.r
#   y <- data$y[1] # vertical position
#   group <- data$group[1]
#   PANEL <- data$PANEL[1]
#
#   other_pdf_max = path_polygon_data(pop.r = data$ci.LL[1], n = data$n[1], LL = data$ci.LL[1], UL = data$ci.UL[1], y = y)$pdf_max
#   path_data = path_polygon_data(pop.r = data$ci.UL[1], n = data$n[1], LL = data$ci.LL[1], UL = data$ci.UL[1], y = y)$path
#
#   df_out <- data.frame(x = path_data$r, y = path_data$pdf, PANEL = PANEL, group = group)
#
#   #print("df out path")
#   #print(df_out)
#   return(df_out)
# }


calc_distribution_centerUL <- function(data, scales) {
  sample.r <- data$x[1] # pop.r
  y <- data$y[1] # vertical position
  group <- data$group[1]
  PANEL <- data$PANEL[1]

  path_data = path_polygon_data(pop.r = data$ci.UL[1],
                                n = data$n[1],
                                LL = data$ci.LL[1],
                                UL = data$ci.UL[1],
                                y = y)$path

  y_value_max <- max(path_data$pdf)
  x_value <- data$ci.UL[1]

  df_out <- data.frame(x = c(x_value, x_value),
                       y = c(y_value_max, y + .15),
                       PANEL = PANEL,
                       group = group)

  #print("df out path")
  #print(df_out)
  return(df_out)
}

calc_distribution_centerLL <- function(data, scales) {
  sample.r <- data$x[1] # pop.r
  y <- data$y[1] # vertical position
  group <- data$group[1]
  PANEL <- data$PANEL[1]

  path_data = path_polygon_data(pop.r = data$ci.LL[1],
                                n = data$n[1],
                                LL = data$ci.LL[1],
                                UL = data$ci.UL[1],
                                y = y)$path

  y_value_max <- max(path_data$pdf)
  x_value <- data$ci.LL[1]

  df_out <- data.frame(x = c(x_value, x_value),
                       y = c(y_value_max, y + .15),
                       PANEL = PANEL,
                       group = group)

  #print("df out path")
  #print(df_out)
  return(df_out)
}


calc_ci_interval_data <- function(data, scales, height, size) {
  #print("ci whisker data")
  #print(data)
  df_out <- data.frame(x = data$x[1], y = data$y[1],
                       xmin = data$ci.LL[1],
                       xmax = data$ci.UL[1],
                       height = height,
                       size = size,
                       PANEL = data$PANEL[1],
                       group = data$group[1])
  #print("ci out")
  #print(df_out)
  return(df_out)
}

