#' Standard ggplot2 theme for RIVM Toolbox
#'
#' @name theme_biomViz
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
#'   theme_biomViz()+
#'   scale_fill_manual(values = c("#3d6721", "#a86826", "#006c89"), guide = "none") +
#'   scale_color_manual(values = c("#3d6721", "#a86826", "#006c89"), guide = "none") +
#'   theme(plot.subtitle = element_text(margin = margin(t = 5, b = 10)),
#'         plot.margin = margin(10, 25, 10, 25))
#'
#' @export
NULL
theme_biomViz <- function(base_size = 11, base_family = "") {
  half_line <- base_size/2
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks.length = unit(half_line / 2.2, "pt"),
      strip.background = element_rect(fill = NA, colour = NA),
      strip.text.x = element_text(colour = "grey30"),
      strip.text.y = element_text(colour = "grey30"),
      axis.text = element_text(colour = "grey10", size = rel(0.9)),
      axis.title = element_text(colour = "grey10", size = rel(0.9)),
      legend.title = element_text(colour = "grey30", size = rel(0.9)),
      panel.border = element_rect(fill = NA, colour = "grey70", size = rel(0.9)),
      legend.key.size = unit(0.9, "lines"),
      legend.text = element_text(size = rel(0.7), colour = "grey30"),
      legend.key = element_rect(colour = NA, fill = NA),
      legend.background = element_rect(colour = NA, fill = NA),
      plot.title = element_text(colour = "grey10", size = rel(1)),
      plot.subtitle = element_text(colour = "grey10", size = rel(.85))
    )
}
#pcoa.plot + theme_biomViz()
