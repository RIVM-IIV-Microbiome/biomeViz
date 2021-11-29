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
#'   scale_fill_biomeViz(palette = "viz3") +
#'   scale_color_biomeViz(palette = "viz3")
#'

NULL

#' @rdname Themes
#' @aliases theme_biomViz
#' @importFrom ggplot2 element_text element_rect element_blank theme_bw theme margin
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
      plot.title = element_text(colour = "#303030", size = rel(1.2),hjust = 0),
      plot.subtitle = element_text(colour = "#303030", size = rel(1),hjust = 0),
      plot.margin=unit(c(10,5,5,5),"mm")
    )
}


#' @rdname Themes
#' @aliases theme_biomViz_minimal
#' @importFrom ggplot2 element_text element_rect element_blank theme_minimal theme
#'   rel %+replace% element_line margin
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
      plot.title = element_text(colour = "#303030", size = rel(1.2),hjust = 0, margin=margin(0,0,10,0)),
      plot.subtitle = element_text(colour = "#303030", size = rel(1),hjust = 0, margin=margin(0,0,10,0)),
      plot.margin=unit(c(10,5,5,5),"mm")
    )
}

#' @rdname Themes
#' @aliases theme_biomViz_bw
#' @importFrom ggplot2 element_text element_rect element_blank theme_minimal theme
#'   rel %+replace% element_line margin
#' @export
theme_biomViz_bw <- function(base_size = 11, base_family = "") {
  half_line <- base_size/2
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(panel.background = element_rect(fill = "#F8F8F8", color = "#303030"),
      panel.grid.major = element_line(colour= "#DCDCDC"),
      panel.grid.minor = element_blank(),
      axis.ticks.length = unit(half_line / 2.2, "pt"),
      axis.ticks = element_line(colour = "#303030")  ,
      strip.background = element_rect(fill = "#F8F8F8", colour = "#303030"),
      strip.text.x = element_text(colour = "#303030", size = rel(1), face = "bold"),
      strip.text.y = element_text(colour = "#303030", size = rel(1), face = "bold", angle = -90),
      axis.text = element_text(colour="#303030", size = rel(1)),
      axis.title = element_text(colour = "grey10", size = rel(1.2)),
      #axis.line = element_line(colour="#303030"),
      legend.title = element_text(colour = "#303030", size = rel(1.2), face = "bold", hjust=0),
      panel.border = element_blank(),
      legend.key.size = unit(1.2, "lines"),
      legend.text = element_text(size = rel(1.2), colour = "#303030"),
      legend.key = element_rect(colour = NA, fill = NA),
      legend.background = element_rect(colour = NA, fill = NA),
      plot.title = element_text(colour = "#303030", size = rel(1.2),hjust = 0, vjust = 1,margin=margin(0,0,10,0)),
      plot.subtitle = element_text(colour = "#303030", size = rel(1),hjust = 0, vjust = 1, margin=margin(0,0,10,0)),
      plot.margin=unit(c(10,5,5,5),"mm")
    )
}



# Themes setup from https://drsimonj.svbtle.com/creating-corporate-colour-palettes-for-ggplot2
# https://twitter.com/drsimonj
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

#' Function to extract biomeViz colors as hex codes
#'
#' @param ... Character names of biomeViz_colors
#' @rdname Themes
#' @aliases biomeViz_cols
#' @export
biomeViz_cols <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (biomeViz_colors)

  biomeViz_colors[cols]
}

#' List of biomeViz colors
#' @rdname Themes
#' @export
biomeViz_colors <- c(
  `darkgreen` = "#3d6721",
  `beigeyellow` = "#e9c46a",
  `beigeorange` = "#f4a261",
  `beigebrown` = "#936639",
  `beigetblue` = "#457b9d",
  `beigegreen` = "#b5e48c",
  `beigegrey` = "#adb5bd")

#biomeViz_cols("beigegreen")


#' biomeViz palettes
#' @rdname Themes
#' @export
biomeViz_palettes <- list(
  `summer`  = biomeViz_cols("beigegreen","beigebrown","beigeyellow","beigeorange","beigetblue","beigegrey"),
  `summer3`  = biomeViz_cols("beigetblue","beigeyellow","beigeorange"),
  `viz3` = biomeViz_cols("darkgreen","beigebrown","beigetblue")
)

#' Return function to interpolate a biomeViz color palette
#'
#' @param palette Character name of palette in biomeViz_palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#' @importFrom grDevices colorRampPalette
#' @rdname Themes
#' @export
biomeViz_pal <- function(palette = "summer", reverse = FALSE, ...) {
  pal <- biomeViz_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  colorRampPalette(pal, ...)
}

#biomeViz_pal("summer")
#' Color scale constructor for biomeViz colors
#'
#' @param palette Character name of palette in biomeViz_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#'
#' @importFrom ggplot2 discrete_scale scale_color_gradientn
#' @rdname Themes
#' @aliases scale_color_biomeViz
#' @export
scale_color_biomeViz <- function(palette = "summer", discrete = TRUE, reverse = FALSE, ...) {
  pal <- biomeViz_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("colour", paste0("biomeViz_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

#' Fill scale constructor for biomeViz colors
#'
#' @param palette Character name of palette in biomeViz_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
#'
#' @importFrom ggplot2 discrete_scale scale_fill_gradientn
#' @rdname Themes
#' @aliases scale_fill_biomeViz
#' @export
scale_fill_biomeViz <- function(palette = "summer", discrete = TRUE, reverse = FALSE, ...) {
  pal <- biomeViz_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("biomeViz_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}
