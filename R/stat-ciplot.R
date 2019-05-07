#' This is the documentation for stat_ciplot (confidence interval plot)
#' @param mapping Set of aesthetic mappings created by aes() or aes_(). If specified and inherit.aes = TRUE (the default), it is combined with the default mapping at the top level of the plot. You must supply mapping if there is no plot mapping
#' @param data The data to be displayed in this layer
#' @param position Position adjustment, either as a string, or the result of a call to a position adjustment function.
#' @param na.rm If FALSE, the default, missing values are removed with a warning. If TRUE, missing values are silently removed
#' @param show.legend logical. Should this layer be included in the legends? NA, the default, includes if any aesthetics are mapped. FALSE never includes, and TRUE always includes. It can also be a named logical vector to finely select the aesthetics to display
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than combining with them. This is most useful for helper functions that define both data and aesthetics and shouldn't inherit behaviour from the default plot specification, e.g. borders().
#' @param pop.r Population correlation
#' @param n Sample size
#' @param ci.LL Confidence interval lower limit
#' @param ci.UL Confidence interval upper limit
#' @param ... Other arguments
#' @export
stat_ciplot <- function(mapping = NULL, data = NULL,
                        position = "identity", na.rm = FALSE, show.legend = NA,
                        inherit.aes = TRUE, pop.r, n, ci.LL, ci.UL, ...) {

  list(
    ggplot2::layer(
      stat = StatSamplingDistributionLL, data = data, mapping = mapping, geom = "path",
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, ...)
    ),
    ggplot2::layer(
      stat = StatSamplingDistributionUL, data = data, mapping = mapping, geom = "path",
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, ...)
    ),
    ggplot2::layer(
      stat = StatIntervalCI, data = data, mapping = mapping, geom = "errorbarh",
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, ...)
    ),
    ggplot2::layer(
      stat = StatDistCenterUL, data = data, mapping = mapping, geom = "path",
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, ...)
    ),
    ggplot2::layer(
      stat = StatDistCenterLL, data = data, mapping = mapping, geom = "path",
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




#####

calc_distribution_ciLL <- function(data, scales) {
  sample.r <- data$x[1] # pop.r
  y <- data$y[1] # vertical position
  group <- data$group[1]
  PANEL <- data$PANEL[1]

  path_data = path_polygon_data(pop.r = data$ci.LL[1], n = data$n[1], LL = data$ci.LL[1], UL = data$ci.UL[1], y = y)$path

  df_out <- data.frame(x = path_data$r, y = path_data$pdf, PANEL = PANEL, group = group)

  #print("df out path")
  #print(df_out)
  return(df_out)
}

calc_distribution_ciUL <- function(data, scales) {
  sample.r <- data$x[1] # pop.r
  y <- data$y[1] # vertical position
  group <- data$group[1]
  PANEL <- data$PANEL[1]

  path_data = path_polygon_data(pop.r = data$ci.UL[1], n = data$n[1], LL = data$ci.LL[1], UL = data$ci.UL[1], y = y)$path

  df_out <- data.frame(x = path_data$r, y = path_data$pdf, PANEL = PANEL, group = group)

  #print("df out path")
  #print(df_out)
  return(df_out)
}


calc_distribution_ciUL <- function(data, scales) {
  sample.r <- data$x[1] # pop.r
  y <- data$y[1] # vertical position
  group <- data$group[1]
  PANEL <- data$PANEL[1]

  path_data = path_polygon_data(pop.r = data$ci.UL[1], n = data$n[1], LL = data$ci.LL[1], UL = data$ci.UL[1], y = y)$path

  df_out <- data.frame(x = path_data$r, y = path_data$pdf, PANEL = PANEL, group = group)

  #print("df out path")
  #print(df_out)
  return(df_out)
}


calc_distribution_centerUL <- function(data, scales) {
  sample.r <- data$x[1] # pop.r
  y <- data$y[1] # vertical position
  group <- data$group[1]
  PANEL <- data$PANEL[1]

  path_data = path_polygon_data(pop.r = data$ci.UL[1], n = data$n[1], LL = data$ci.LL[1], UL = data$ci.UL[1], y = y)$path
  y_value_max <- max(path_data$pdf)
  x_value <- data$ci.UL[1]

  df_out <- data.frame(x = c(x_value, x_value), y = c(y_value_max, y + .15), PANEL = PANEL, group = group)

  #print("df out path")
  #print(df_out)
  return(df_out)
}

calc_distribution_centerLL <- function(data, scales) {
  sample.r <- data$x[1] # pop.r
  y <- data$y[1] # vertical position
  group <- data$group[1]
  PANEL <- data$PANEL[1]

  path_data = path_polygon_data(pop.r = data$ci.LL[1], n = data$n[1], LL = data$ci.LL[1], UL = data$ci.UL[1], y = y)$path
  y_value_max <- max(path_data$pdf)
  x_value <- data$ci.LL[1]

  df_out <- data.frame(x = c(x_value, x_value), y = c(y_value_max, y + .15), PANEL = PANEL, group = group)

  #print("df out path")
  #print(df_out)
  return(df_out)
}


calc_ci_interval_data <- function(data, scales, height, size) {
  print("ci whisker data")
  print(data)
  df_out <- data.frame(x = data$x[1], y = data$y[1], xmin = data$ci.LL[1], xmax = data$ci.UL[1], height = height, size = size, PANEL = data$PANEL[1], group = data$group[1])
  print("ci out")
  print(df_out)
  return(df_out)
}

