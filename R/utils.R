#' Standard ggplot2 theme for RIVM Toolbox
#'
#' @name Themes
#'
#' @details A standard theme for ggplot2 to produce similar aesthetics for
#'          RIVM Toolbox.
#'
#' @param base_size Base size
#' @param base_family Base family
#'
#' @importFrom ggplot2 element_text element_rect element_blank theme_bw theme
#'   rel %+replace%
#' @importFrom grid unit
#'
#' @examples
#' library(biomeUtils)
#' library(biomeViz)
#' library(microbiome)
#' library(ggplot2)
#' library(dplyr)
#'
#' ps <- FuentesIliGutData %>%
#'   microbiome::aggregate_taxa("Genus") %>%
#'   microbiome::transform("compositional")
#'
#' plotTaxaRaincloud(ps,
#'                   taxa ="Bacteroides",
#'                   group_samples_by = "ILI",
#'                   opacity = 0.25,
#'                   shape_point = 21) + # combine with ggplot2 for improvements
#'   labs(y = "Relative abundance",
#'        x = "",
#'        subtitle = "Raincloud plot") +
#'   theme_biomViz_minimal()+
#'   scale_fill_biomeViz_summer() +
#'   scale_colour_biomeViz_summer()
#'

NULL

#' @rdname Themes
#' @aliases theme_biomViz
#' @export
theme_biomViz <- function(base_size = 11, base_family = "") {
  half_line <- base_size/2
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      panel.grid = element_blank() ,
      axis.ticks.length = unit(half_line / 2.2, "pt"),
      strip.background = element_rect(fill = NA, colour = NA),
      strip.text.x = element_text(colour = "#303030", size = rel(1.2), face = "bold"),
      strip.text.y = element_text(colour = "#303030", size = rel(1.2), face = "bold"),
      axis.text = element_text(colour="#303030", size = rel(1)),
      axis.title = element_text(colour = "grey10", size = rel(1.2)),
      legend.title = element_text(colour = "#303030", size = rel(1.2), face = "bold", hjust=0),
      panel.border = element_rect(fill = NA, colour = "#181818", size = rel(0.9)),
      legend.key.size = unit(1.2, "lines"),
      legend.text = element_text(size = rel(1.2), colour = "#303030"),
      legend.key = element_rect(colour = NA, fill = NA),
      legend.background = element_rect(colour = NA, fill = NA),
      plot.title = element_text(colour = "#303030", size = rel(1.5),hjust = 0),
      plot.subtitle = element_text(colour = "#303030", size = rel(1),hjust = 0),
      plot.margin=unit(c(10,5,5,5),"mm")
    )
}


#' @rdname Themes
#' @aliases theme_biomViz_minimal
#' @importFrom ggplot2 element_text element_rect element_blank theme_minimal theme
#'   rel %+replace% element_line
#' @export
theme_biomViz_minimal <- function(base_size = 11, base_family = "") {
  half_line <- base_size/2
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      panel.grid.major = element_line(colour="#f0f0f0"),
      panel.grid.minor = element_blank(),
      axis.ticks.length = unit(half_line / 2.2, "pt"),
      axis.ticks = element_line(colour = "#303030")  ,
      strip.background = element_rect(fill = NA, colour = NA),
      strip.text.x = element_text(colour = "#303030", size = rel(1.2), face = "bold"),
      strip.text.y = element_text(colour = "#303030", size = rel(1.2), face = "bold"),
      axis.text = element_text(colour="#303030", size = rel(1)),
      axis.title = element_text(colour = "grey10", size = rel(1.2)),
      axis.line = element_line(colour="#303030"),
      legend.title = element_text(colour = "#303030", size = rel(1.2), face = "bold", hjust=0),
      panel.border = element_blank(),
      legend.key.size = unit(1.2, "lines"),
      legend.text = element_text(size = rel(1.2), colour = "#303030"),
      legend.key = element_rect(colour = NA, fill = NA),
      legend.background = element_rect(colour = NA, fill = NA),
      plot.title = element_text(colour = "#303030", size = rel(1.5),hjust = 0),
      plot.subtitle = element_text(colour = "#303030", size = rel(1),hjust = 0)
    )
}

#' @rdname Themes
#' @aliases theme_biomViz_bw
#' @importFrom ggplot2 element_text element_rect element_blank theme_minimal theme
#'   rel %+replace% element_line
#' @export
theme_biomViz_bw <- function(base_size = 11, base_family = "") {
  half_line <- base_size/2
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(panel.background = element_rect(fill = "#F8F8F8"),
      panel.grid.major = element_line(colour= "#DCDCDC"),
      panel.grid.minor = element_blank(),
      axis.ticks.length = unit(half_line / 2.2, "pt"),
      axis.ticks = element_line(colour = "#303030")  ,
      strip.background = element_rect(fill = "#F8F8F8", colour = "#303030"),
      strip.text.x = element_text(colour = "#303030", size = rel(1), face = "bold"),
      strip.text.y = element_text(colour = "#303030", size = rel(1), face = "bold", angle = -90),
      axis.text = element_text(colour="#303030", size = rel(1)),
      axis.title = element_text(colour = "grey10", size = rel(1.2)),
      axis.line = element_line(colour="#303030"),
      legend.title = element_text(colour = "#303030", size = rel(1.2), face = "bold", hjust=0),
      panel.border = element_blank(),
      legend.key.size = unit(1.2, "lines"),
      legend.text = element_text(size = rel(1.2), colour = "#303030"),
      legend.key = element_rect(colour = NA, fill = NA),
      legend.background = element_rect(colour = NA, fill = NA),
      plot.title = element_text(colour = "#303030", size = rel(1.5),hjust = 0, vjust = 1),
      plot.subtitle = element_text(colour = "#303030", size = rel(1),hjust = 0, vjust = 1),
      plot.margin = unit(c(1, 1, 0.5, 0.5), "lines")
    )
}




#' @rdname Themes
#' @aliases scale_fill_biomeViz_summer
#' @importFrom scales manual_pal
#' @importFrom ggplot2 discrete_scale
#' @export
scale_fill_biomeViz_summer <- function(...){
  discrete_scale("fill","biomeViz_summer",manual_pal(values = c("#E56997","#BD97CB",
                                                                "#66D2D6","#F5631A","#FFA303",
                                                                "#444440","#07BB9C","#4120A9",
                                                                "#FEC8A7","#116530","#D7D4DD")), ...)

}

#' @rdname Themes
#' @aliases scale_colour_biomeViz_summer
#' @importFrom scales manual_pal
#' @importFrom ggplot2 discrete_scale
#' @export
scale_colour_biomeViz_summer <- function(...){
  discrete_scale("colour","biomeViz_summer",manual_pal(values = c("#E56997","#BD97CB",
                                                                  "#66D2D6","#F5631A","#FFA303",
                                                                  "#444440","#07BB9C","#4120A9",
                                                                  "#FEC8A7", "#116530","#D7D4DD")), ...)

}



