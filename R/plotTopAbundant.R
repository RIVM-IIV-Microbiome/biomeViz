#' Plot Top Abundant Taxa
#'
#' @name plotTopAbundant
#'
#' @details Plot the features with the highest abundance in all samples,
#'          at a user specific taxonomic level.
#'
#' @param x \code{phyloseq} object
#'
#' @param taxa_level Taxonomic level at which features are selected. Abundances
#'                   are summed up for each of the taxa at this level and then top
#'                   are selected.
#'
#' @param top Numeric value, how many top \code{taxa_level} to return. Default
#'            top = 100
#'
#' @param ... Options to pass ggplot2::geom_jitter()
#'
#' @examples
#'
#' library(biomeUtils)
#' library(biomeViz)
#' p <- plotTopAbundant(SprockettTHData,
#'                      taxa_level = "Family",
#'                      top=10L,
#'                      alpha = 0.25,
#'                      shape = 124,
#'                      color="black",
#'                      fill = "black",
#'                      size=3,
#'                      width = 0.02)
#' print(p)
#'
#' @return A ggplot2 object.
#'
#' @author Sudarshan A. Shetty
#'
#' @references
#' Shetty SA (2021). Data visualization for microbiome analytics.
#' \url{https://github.com/microsud/biomeUtils}
#'
#' @importFrom tibble tibble
#' @importFrom dplyr left_join group_by summarise arrange pull mutate_if filter %>% desc
#' @importFrom ggplot2 ggplot geom_jitter theme_minimal ylab xlab aes
#' @importFrom biomeUtils getTaxaTibble
#' @importFrom stats reorder
#' @importFrom rlang sym
#' @importFrom phyloseq rank_names
#'
#' @export

plotTopAbundant <- function(x,
                            taxa_level = "Family",
                            top=10L,
                            ...){


  # Global vars
  mean_abnd <- taxa_counts <- percent_of_total <- NULL
  if(!taxa_level %in% rank_names(x)){
    stop("Please provide a valid taxa_level")
  }

  .check_qc_input(x)

  # calculate taxa qc

  tax.qc <- biomeUtils::calculateQC(x)
  tax.qc <- tax.qc$TaxaQC
  # add taxonomy
  tax.qc <- tax.qc %>%
    dplyr::left_join(biomeUtils::getTaxaTibble(x, column_id = "taxa"))

  top_levels <- tax.qc %>%
    dplyr::group_by(!!sym(taxa_level)) %>%
    dplyr::summarise(mean_abnd = sum(taxa_counts)) %>%
    dplyr::arrange(desc(mean_abnd)) %>%
    dplyr::pull(!!sym(taxa_level))

  if(top > length(top_levels)){
    message(paste0("Top value is higher than available
                   plotting all availble taxa within ", taxa_level))
    top_levels <- length(top_levels)
  } else{

    top_levels <- top_levels[1:top]

  }


  #prep for plot
  tax.qc <- tax.qc %>%
    dplyr::filter(!!sym(taxa_level) %in% top_levels) %>%
    dplyr::mutate_if(is.character, ~(ifelse(is.na(.), paste0("Unclassified_", taxa_level ), .))) %>%
    #dplyr::mutate(Group = ifelse(is.na({{ taxa_level }}), "Unclassified", !!sym(taxa_level))) %>%
    dplyr::arrange(desc(percent_of_total))

  plot.tax.qc <- ggplot2::ggplot(tax.qc,
                                 aes(percent_of_total,
                                     reorder(!!sym(taxa_level), percent_of_total))) +
    ggplot2::geom_jitter(...) + ##124
    ggplot2::theme_minimal() +
    ggplot2::ylab(taxa_level) +
    ggplot2::xlab("% of total reads")

  return(plot.tax.qc)

}



#' @importFrom phyloseq sample_sums nsamples ntaxa
.check_qc_input <- function(x){
  ## Must be a phyloseq object
  if ( !is(x, "phyloseq") ){
    stop("input must be an phyloseq object.")
  }

  ## the input must have few samples
  if ( nsamples(x) < 1 ){
    stop("input must have at least one sample")
  }

  if ( ntaxa(x) < 1 ) {
    stop("input must have at least one taxa")
  }

  ## See what versions of the expression data are available in the object
  if(any(sample_sums(x)==1) | any(.check_decimal(sample_sums(x)))){
    stop("Data must be counts")
  }

}

#' @keywords internal
#' check decimal
#' \url{https://www.rdocumentation.org/packages/schoolmath/versions/0.4/topics/is.decimal}
.check_decimal <- function(x){

  start <- 1
  end <- length(x)+1
  while(start<end){
    y <- x[start]

    test <- floor(y)
    if(y==test){
      if(start==1){
        result=FALSE
      }else{
        result<- c(result,FALSE)
      }

    }else{
      if(start==1){
        result=TRUE
      }else{
        result <- c(result,TRUE)
      }
    }
    start <- start+1
  }

  return(result)
}


