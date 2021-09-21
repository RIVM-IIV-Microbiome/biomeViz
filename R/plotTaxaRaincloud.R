#' Creates a Raincloud plot
#'
#' @name plotTaxaRaincloud
#'
#' @details A Rain Cloud plotting wrapper for \code{\link{phyloseq-class}} object.
#'
#' @param x \code{\link{phyloseq-class}} object.
#'
#' @param taxa Feature to plot.
#'
#' @param group_samples_by A column in \code{sample_data} to compare abundances.
#'
#' @param opacity Alpha value.
#'
#' @param shape_point Point shape.
#'
#' @return a \code{ggplot2} object.
#'
#' @examples
#' library(biomeUtils)
#' library(biomeViz)
#' library(microbiome)
#' library(dplyr)
#'
#' ps <- FuentesIliGutData %>%
#'        microbiome::aggregate_taxa("Genus") %>%
#'        microbiome::transform("compositional")
#'
#' # Select taxa and group to compare
#'
#' plotTaxaRaincloud(ps,
#'                   taxa ="Bacteroides",
#'                   group_samples_by = "ILI",
#'                   opacity = 0.25,
#'                   shape_point = 21)
#'
#' @author Sudarshan A. Shetty
#' @references
#' Shetty SA (2021). Data visualization for microbiome analytics.
#' \url{https://github.com/microsud/biomeViz}
#'
#' Scherer Cédric (2021). A Step-by-Step tutorial as supplement to my talk “ggplot
#' Wizardry: My Favorite Tricks and Secrets for Beautiful Plot in R” at OutlierConf 2021.
#' \url{https://z3tt.github.io/OutlierConf2021/}
#'
#'
#' @export
plotTaxaRaincloud <- function(x, taxa = NULL,
                              group_samples_by = NULL,
                              opacity = 0.25,
                              shape_point = 21){


  if(length(group_samples_by) >1){
    stop("Only one grouping from sample_data is supported")
  }

  # Prep data
  prep.abund <- .prep_data(x, taxa, group_samples_by)

  # Plot
  p <- ggplot2::ggplot(prep.abund,
                  ggplot2::aes_string(group_samples_by,
                                      "Abundance",
                                      color=group_samples_by)) +
    ggdist::stat_halfeye(
      ggplot2::aes_string(fill=group_samples_by),
      adjust = .5,
      width = .3,
      .width = 0,
      justification = -.3,
      point_colour = NA,
      alpha=opacity) +
    ggplot2::geom_boxplot(
      ggplot2::aes_string(fill=group_samples_by),
      width = .1,
      outlier.shape = NA,
      alpha=opacity) +
    gghalves::geom_half_point(
      ggplot2::aes_string(fill=group_samples_by),
      side = "l",
      range_scale = .4,
      alpha = opacity,
      shape = shape_point) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none")

  if(length(taxa) > 1){
    p <- p + ggplot2::facet_wrap(~FeatureID) +
      ggplot2::theme(legend.position = "none")
  }

  return(p)

}

#' @importFrom phyloseq taxa_names sample_variables
#' @importFrom biomeUtils getAbundanceTibble getTaxaTibble getSampleTibble
.prep_data <- function(x, taxa, group_samples_by) {

  FeatureID <- NULL

  if(is.null(taxa) | is.null(group_samples_by)) {
    stop("Please specify taxa and or group_samples_by")
  }
  if ( !is(x, "phyloseq") ){
    stop("input must be an phyloseq object.")
  }

  if(!group_samples_by %in% sample_variables(x)){
    stop("Please check taxa and or group_samples_by are present in input")
  }

  prep.abund <- getAbundanceTibble(x, select_rows = taxa) %>%
    tidyr::pivot_longer(-FeatureID,
                        names_to = "SampleID",
                        values_to = "Abundance") %>%
    dplyr::left_join(getTaxaTibble(x, select_rows = taxa),
                     by = "FeatureID") %>%
    dplyr::left_join(getSampleTibble(x, select_cols = group_samples_by),
                     by = "SampleID")

  return(prep.abund)
}
