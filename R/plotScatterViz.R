#' Scater Plot
#'
#' @name plotScatterViz
#'
#' @details A simple plotting utility for visualization of two numerical
#'          variables. Provided two numerical
#'          variable present in \code{sample_data} of a
#'          \code{\link{phyloseq-class}} object, a scatter plot is returned.
#'
#' @param x \code{\link{phyloseq-class}} object.
#'
#' @param x_numeric A column in \code{sample_data} to compare that is a numeric
#'
#' @param y_numeric A column in \code{sample_data} to compare that is a numeric
#'
#' @param color_var A column in \code{sample_data} to color points
#'
#' @param point_size \code{geom_point}. Default is 3
#'
#' @param point_opacity \code{ggplot2} alpha value. Default is 0.5
#'
#' @param point_shape \code{ggplot2} point shape value. Default is 21
#'
#' @param side_panel Logical. If side density to plot. Default is TRUE
#'
#' @param side_panel_scale \code{ggside} ggside.panel.scale requires a scalar numeric.
#'                         default is 0.3
#'
#' @return a \code{ggplot} object.
#'
#' @examples
#' library(biomeUtils)
#' library(biomeViz)
#' library(dplyr)
#' library(microbiome)
#' library(ggplot2)
#' ps <- FuentesIliGutData
#' plotScatterViz(ps, x_numeric = "age", y_numeric = "BMI",
#'                color_var = "ILI") +
#'                scale_fill_manual(values=c("steelblue", "brown3", "grey70"))+
#'                scale_fill_manual(values=c("steelblue", "brown3", "grey70"))
#' @author Sudarshan A. Shetty
#' @references
#' Shetty SA (2021). Data visualization for microbiome analytics.
#' \url{https://github.com/microsud/biomeViz}
#'
#' @export
NULL

plotScatterViz <- function(x,
                           x_numeric = NULL,
                           y_numeric = NULL,
                           color_var = NULL,
                           point_size = 3,
                           point_opacity = 0.5,
                           point_shape = 21,
                           side_panel=TRUE,
                           side_panel_scale = 0.3){

  p <- getSampleTibble(x) |>
    ggplot2::ggplot(ggplot2::aes_string(x_numeric, y_numeric,fill=color_var))

  p <- p +
    ggplot2::geom_point(size=point_size,
                        alpha=point_opacity,
                        shape=point_shape) +
    theme_biomViz()

  if(side_panel){
    p <- p +
      ggside::geom_xsidedensity(ggplot2::aes_string(fill=color_var,color= color_var),
                                alpha = point_opacity) +
      ggside::geom_ysidedensity(ggplot2::aes_string(fill=color_var,color= color_var),
                                alpha = point_opacity)+
      theme(ggside.panel.scale = side_panel_scale) +
      ggside::scale_ysidex_continuous(guide = ggplot2::guide_axis(angle = 90), minor_breaks = NULL)
  }
  return(p)

}
