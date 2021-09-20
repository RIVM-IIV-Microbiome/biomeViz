#' Creates a Heatmap using `TidyHeatmap` and `ComplexHeatmap`
#'
#' @name plotTidyHeatmap
#'
#' @details A TidyHeatmap plotting wrapper for \code{\link{phyloseq-class}} object.
#'
#' @param x \code{\link{phyloseq-class}} object.
#'
#' @param select_taxa Features to plot.
#'
#' @param group_samples_by A column in \code{sample_data} to group tiles.
#'
#' @param add_taxa_label Logical. Default is TRUE.
#'
#' @param ... Arguments forwarded to tidyHeatmap::heatmap and ComplexHeatmap::Heatmap.
#'
#' @return a \code{heatmap} object.
#'
#' @examples
#' library(biomeUtils)
#' library(biomeViz)
#' library(microbiome)
#' library(dplyr)
#'
#' ps <- FuentesIliGutData %>%
#'          microbiome::transform("compositional")
#' select_taxa <- findTopTaxa(ps, top= 10, method="mean")
#'
#' plotTidyHeatmap(ps, select_taxa = select_taxa,
#'                 group_samples_by = "ILI",
#'                 add_taxa_label = FALSE,
#'                 cluster_rows = FALSE,
#'                 .scale = "none",
#'                 transform = NULL,
#'                 palette_value = c("red", "white", "blue"))
#'
#' @author Sudarshan A. Shetty
#'
#' @references
#' Shetty SA (2021). Data visualization for microbiome analytics.
#' \url{https://github.com/microsud/biomeViz}
#'
#' @importFrom dplyr %>% case_when
#' @importFrom biomeUtils getSampleTibble getTaxaTibble getAbundanceTibble
#' @importFrom rlang sym
#' @importFrom stringr str_c
#' @importFrom phyloseq sample_variables
#'
#' @export
plotTidyHeatmap <- function(x,
                            select_taxa = NULL,
                            group_samples_by = NULL,
                            add_taxa_label = TRUE,
                            ...){

  FeatureID <- Features <- SampleID <- n <- num <- NULL
    sample_variables <- value <- NULL

  if(is.null(select_taxa) | is.null(group_samples_by)) {
    stop("Please specify select_rows and or group_samples_by")
  }
  if ( !is(x, "phyloseq") ){
    stop("input must be an phyloseq object.")
  }

  # Make first data
  plot.data <- x %>%
    getAbundanceTibble(select_rows = select_taxa) %>%
    tidyr::pivot_longer(-FeatureID, names_to = "SampleID")

  if(add_taxa_label){

    tax_df <- getTaxaTibble(x) %>%
      #mutate(Species = split_species(Species, n = num_species)) %>%
      dplyr::mutate(Features =
               case_when(
                 is.na(Class)  ~ str_c(FeatureID, ":p_", Phylum),
                 is.na(Order)  ~ str_c(FeatureID, ":c_", Class),
                 is.na(Family)  ~ str_c(FeatureID, ":o_", Order),
                 is.na(Genus)   ~ str_c(FeatureID, ":f_", Family),
                 is.na(Species) ~ str_c(FeatureID, ":g_", Genus),
                 TRUE ~ str_c(FeatureID, ":" , Genus, " ", Species)
               )
      ) %>%
      dplyr::filter(FeatureID %in% select_taxa)

    plot.data <- plot.data %>%
      dplyr::left_join(tax_df, by = "FeatureID")

  } else {

    plot.data <- plot.data %>%
      dplyr::left_join(getTaxaTibble(x, select_rows = select_taxa),
                by = "FeatureID") %>%
      dplyr::rename(Features = "FeatureID")

  }

  if( !group_samples_by %in% sample_variables(x) ) {
    stop("group_samples_by value not found in sample_data")
  }
  plot.data <- plot.data %>%
    dplyr::left_join(getSampleTibble(x), by = "SampleID") %>%
    dplyr::group_by(!!sym(group_samples_by))

  p <- plot.data %>%
    tidyHeatmap::heatmap(
      .column = SampleID,
      .row = Features,
      .value = value,
      ...)

  return(p)
}
