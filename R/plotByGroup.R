#' Visualize one categorical column and one numeric column
#'
#' @name plotByGroup
#'
#' @details A simple plotting utility for visualization of one categorical
#'          and numerical variable. Provided a categorical variable and numerical
#'          variable present in \code{sample_data} of a
#'          \code{\link{phyloseq-class}} object, a comparative visualization
#'          can be done.
#'
#' @param x \code{\link{phyloseq-class}} object.
#'
#' @param x.factor A column in \code{sample_data} to compare that is a factor
#'
#' @param y.numeric A column in \code{sample_data} to compare that is a numeric
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
#' # calculate Shannon diversity using microbiome::diversity and add it to
#' # the sample_data in phyloseq using biomeUtils::mutateSampleData()
#' ps <- mutateSampleData(ps,
#'                        Shannon = microbiome::diversity(ps, "shannon")[,1])
#' plotByGroup(ps,
#'             x.factor="ILI",
#'             y.numeric = "Shannon") +
#'          geom_point(aes(color=ILI),
#'                     position = position_jitter(width = 0.1)) +
#'          scale_color_manual(values=c("steelblue", "brown3", "grey70"))
#'
#' @author Sudarshan A. Shetty
#'
#' @references
#' Shetty SA (2021). Data visualization for microbiome analytics.
#' \url{https://github.com/microsud/biomeViz}
#'
#' @importFrom dplyr %>%
#' @importFrom biomeUtils getSampleTibble
#' @importFrom rlang sym
#'
#' @export

plotByGroup <- function(x,
                        x.factor = NULL,
                        y.numeric = NULL){

  if(is.null(x.factor) | is.null(y.numeric)) {
    stop("Please specify x.factor and y.numeric")
  }

  data.plot <- x %>%
    getSampleTibble()

  vars.use <- c(x.factor,y.numeric)
  if( !vars.use[1] %in% colnames(data.plot) | !vars.use[2] %in% colnames(data.plot)) {
    stop("variables not found in sample_data")
  }

  # check
  .check_factor_numeric(data.plot,
                        x.factor = x.factor,
                        y.numeric = y.numeric)

  nsamples.data <- .get_nsamples_by_group(x, x.factor)

  data.plot <- data.plot %>%
    dplyr::left_join(nsamples.data, by = x.factor) %>%
    dplyr::mutate(myaxis = paste0(!!sym(x.factor), "\n", "n=", num))



  p <- data.plot %>%
    ggplot2::ggplot(ggplot2::aes_string(x="myaxis",
                                        y=y.numeric)) +
    ggplot2::geom_boxplot(width=0.3,
                          color="#191919",
                          alpha=0.2,
                          outlier.shape = NA) +
    xlab("") +
    theme_minimal()
  return(p)
}


# Helper
#' @importFrom rlang sym
#' @importFrom dplyr %>% pull
.check_factor_numeric <- function(data.plot, x.factor, y.numeric){

  if(is.null(x.factor) | is.null(y.numeric)) {
    stop("Please specify x.factor and y.numeric")
  }

  cat.vals  <- data.plot %>%
    pull(!!sym(x.factor))
  num.vals  <- data.plot %>%
    pull(!!sym(y.numeric))
  if(!is.factor(cat.vals) | !is.numeric(num.vals)) {
    stop("x.factor must be a factor and y.numeric must be numeric values")
  }

}

#' @importFrom biomeUtils getSampleTibble
.get_nsamples_by_group <- function(x, x.factor) {

  if(!x.factor %in% phyloseq::sample_variables(x)) {
    stop("x.factor variable is not present in",
         "sample_data")
  }

  n <- num <- NULL
  nsamples.data <- x %>%
    getSampleTibble() %>%
    dplyr::group_by(!!sym(x.factor)) %>%
    dplyr::summarize(num=n())
  return(nsamples.data)
}

