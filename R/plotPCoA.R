#' PCoA plot
#'
#' @name plotPCoA
#'
#' @details A Principal Coordinates Analysis for \code{\link{phyloseq-class}} object.
#'          To visualize similarities or dissimilarities between samples in 2D ordination.
#'          This function extends the \code{phyloseq} ordination plots to include
#'          taxa that correlate with choosen axis and plots them along with a
#'          side boxplot for comparing inter-sample variation within groups.
#'
#' @param x \code{\link{phyloseq-class}} object.
#'
#' @param group_var A column in \code{sample_data} to compare. This is also the
#'                  variable used to colour.
#'
#' @param ord_method Ordination method, currently tested PCoA.
#'
#' @param dist_method Distance method, currently tested Bray-Curtis.
#'
#' @param seed Random seed number \code{set.seed}.
#'
#' @param cor_method Correlation method. Default is \code{Spearman}.
#'
#' @param padj_cutoff Cut-off for multiple testing. Default is 0.05.
#'
#' @param padj_method Method for multiple testing. Default is fdr.
#'
#' @param arrows Logical. If arrows for taxa with correlation to axis are to be
#'               plotted. Default is TRUE.
#'
#' @param label_col Color of labels. Default is grey30.
#'
#' @param plot_centroids Logical. To plot centroids or not. Default is TRUE.
#'
#' @param add_side_box Logical. To plot side boxplots or not. Default is TRUE.
#'
#' @param axis_plot Which axis to plot. Default is first two.
#'
#' @param point_shape Shape of the points. Default is 21.
#'
#' @param point_alpha Opacity of points. Default is 0.5
#'
#' @param verbose Logical. Messages to print. Default is TRUE.
#'
#' @param ... Additional arguments to pass to vegan's adonis function.
#'
#' @return a \code{ggplot2} object.
#'
#' @examples
#' library(biomeUtils)
#' library(dplyr)
#' library(ggside)
#' ps <- FuentesIliGutData %>%
#'   microbiome::transform("compositional") %>%
#'   mutateTaxaTable(FeatureID = taxa_names(FuentesIliGutData))
#' plotPCoA(x =ps,
#'          group_var = "ILI",
#'          ord_method = "PCoA",
#'          dist_method = "bray",
#'          seed = 1253,
#'          cor_method = "spearman",
#'          verbose = TRUE,
#'          padj_cutoff = 0.05,
#'          padj_method = "fdr",
#'          arrows = TRUE,
#'          label_col = "grey30",
#'          plot_centroids = TRUE,
#'          add_side_box = TRUE,
#'          axis_plot = c(1:2),
#'          point_shape = 21,  # point_shape
#'          point_alpha = 0.5) +
#'   scale_color_manual(values = c("#3d6721", "#a86826", "#006c89")) +
#'   scale_fill_manual(values = c("#3d6721", "#a86826", "#006c89"))
#'
#' @author Sudarshan A. Shetty
#' @references
#' Shetty SA (2021). Data visualization for microbiome analytics.
#' \url{https://github.com/microsud/biomeViz}
#'
#' @export
NULL
plotPCoA <- function(x,
                     group_var,
                     ord_method,
                     dist_method,
                     seed,
                     cor_method = "spearman",
                     padj_cutoff = 0.05,
                     padj_method = "fdr",
                     arrows = TRUE,
                     label_col = "grey30",
                     plot_centroids = TRUE,
                     add_side_box = TRUE,
                     axis_plot = c(1:2),
                     point_shape = 21,  # point_shape
                     point_alpha = 0.5,
                     verbose = TRUE,
                     ...){

  id.type <- FeatureID <- Axis.1 <- Axis.2 <- NULL
  ord.dat <- .get_ord_info(x,
                           group_var,
                           ord_method,
                           dist_method,
                           seed,
                           verbose= verbose,
                           ...)

  ordplot.dat.all <- phyloseq::plot_ordination(x,
                                               ord.dat$ordDF,
                                               justDF = TRUE,
                                               type = "split")

  # get samples ordination
  ordplot.dat.samples <- subset(ordplot.dat.all, id.type =="Samples")

  # get sample centroids
  axis.a <- colnames(ordplot.dat.samples)[1]
  axis.b <- colnames(ordplot.dat.samples)[1]

  centroids.samples <- ordplot.dat.samples %>%
    dplyr::group_by(!!rlang::sym(group_var)) %>%
    dplyr::summarise(centroid.axis.1 = mean(!!rlang::sym(axis.a)),
                     centroid.axis.2 = mean(!!rlang::sym(axis.b)))

  # Get axis 1 and 2 variation
  evals1 <- round(ord.dat$ordDF$values$Eigenvalues[1] / sum(ord.dat$ordDF$values$Eigenvalues) * 100, 2)
  evals2 <- round(ord.dat$ordDF$values$Eigenvalues[2] / sum(ord.dat$ordDF$values$Eigenvalues) * 100, 2)

  # Get Taxa PC correlation
  tax.pc.cor.dat <- .pc_taxa_correlation(x,
                                         ordplot.dat.samples= ordplot.dat.samples,
                                         cor_method,
                                         axis_plot,
                                         padj_cutoff,
                                         padj_method)


  ordplot.dat.taxa <- subset(ordplot.dat.all, id.type == "Taxa")

  #head(ordplot.dat.taxa)
  names(tax.pc.cor.dat) <- NULL
  axis.a.tax <- as.data.frame(tax.pc.cor.dat[1])$rowname
  axis.b.tax <- as.data.frame(tax.pc.cor.dat[2])$rowname

  .check_taxa_axis(axis.a.tax,axis.b.tax)

  select_tax <- c(axis.a.tax,axis.b.tax)

  ordplot.dat.taxa <- ordplot.dat.taxa %>%
    dplyr::filter(FeatureID %in% select_tax)

  axis_a_names <- colnames(ordplot.dat.samples)[1]
  axis_b_names <- colnames(ordplot.dat.samples)[2]
  # start plot
  p <- ggplot2::ggplot(ordplot.dat.samples,
                       ggplot2::aes_string(x = axis_a_names, y = axis_b_names)) +
    ggplot2::geom_point(ggplot2::aes_string(color = group_var,
                                            fill = group_var),
                        shape= point_shape,  # point_shape
                        alpha= point_alpha)  # point_alpha

  if(add_side_box){

    p  <- p +
      ggside::geom_xsideboxplot(ggplot2::aes_string(y = axis_a_names,
                                                    color= group_var,
                                                    fill = group_var),
                                shape = point_shape,  # point_shape
                                alpha = point_alpha,
                                orientation = "y") +
      ggside::geom_ysideboxplot(ggplot2::aes_string(x = axis_a_names,
                                                    color = group_var,
                                                    fill = group_var),
                                shape= point_shape,  # point_shape
                                alpha = point_alpha,
                                orientation = "x") +
      ggside::scale_ysidex_continuous(guide = ggplot2::guide_axis(angle = 90),
                                      minor_breaks = NULL) +
      ggside::scale_xsidey_discrete() +
      ggside::scale_ysidex_discrete()

  }

  if(arrows){
    p <- p +
      ggplot2::geom_segment(data=ordplot.dat.taxa,
                            ggplot2::aes(x=0, xend=Axis.1*4,
                                         y=0, yend=Axis.2*4),
                            colour = label_col,              # arrow_color
                            size=0.5,                        # arrow_size
                            arrow = ggplot2::arrow(length= ggplot2::unit(1, "mm"),
                                                   type="closed")) +
      ggrepel::geom_label_repel(data=ordplot.dat.taxa,
                                ggplot2::aes(x=Axis.1*4, y=Axis.2*4, # scale_vars
                                             label=FeatureID), # taxa_label
                                colour=label_col,     # label_color
                                #label.padding=.1,
                                size=3,               # label_size
                                max.overlaps = 100000,
                                alpha = 0.75,         # label_opacity
                                box.padding = 0.80,
                                point.padding = 0.5,
                                na.rm=TRUE)
  }

  p + ggplot2::labs(x = paste0("PCoA (", evals1, "%)"),
                    y = paste0("PCoA (", evals2, "%)"))
}

# Get ordination

.get_ord_info <- function(x,
                          group_var = NULL,
                          ord_method = "PCoA",
                          dist_method ="bray",
                          seed = 123,
                          verbose = verbose,
                          ...){

  if(is.null(group_var) || !any(sample_variables(x) %in% group_var)){
    stop("Provide a valid grouping variable")
  }

  if (!is(x, "phyloseq")){
    stop("Ïnput x must be a phyloseq object")
  }

  ord <- phyloseq::ordinate(x, method = ord_method, distance = dist_method)

  dist.mat <- phyloseq::distance(x, method = dist_method)

  group.levs <- microbiome::meta(x)[,group_var]

  if(is.na(seed) || is.null(seed)){
    stop("Provide a random number, see `set.seed`")
  }
  set.seed(seed)
  if (verbose){
    message(paste0("Random number for permutation analysis ...\n", seed))
  }

  permanova.res <- vegan::adonis(dist.mat~ group.levs, ...)

  return(list(ordDF = ord, permanovaRes = permanova.res))
}


# Taxa PC correlation

.pc_taxa_correlation <- function(x,
                                 ordplot.dat.samples= ordplot.dat.samples,
                                 cor_method = cor_method,
                                 axis_plot = axis_plot,
                                 padj_cutoff = padj_cutoff,
                                 padj_method = padj_method){

  # select axis
  pcs <- ordplot.dat.samples[,axis_plot]
  pc.cor.taxa <- NULL
  for (i in colnames(pcs)){

    suppressWarnings(
      correlate.pcs <- apply(microbiome::abundances(x),1,function(x)
      stats::cor.test(x, pcs[,i], method = cor_method))
    )

    pc.corr <- sapply(correlate.pcs,"[[",4)
    pc.pval <- sapply(correlate.pcs,"[[",3)
    pc.padj <- stats::p.adjust(pc.pval, method = padj_method)

    #otu.cor.pc <- abundances(x)[which(pc.padj < alpha & abs(pc.corr) >= quantile(abs(pc.corr),0.95,na.rm = T)),]
    sig.corr <- pc.corr[which(pc.padj < padj_cutoff & abs(pc.corr) >= stats::quantile(abs(pc.corr),0.95,na.rm = T))]
    sig.p <- pc.padj[which(pc.padj < padj_cutoff & abs(pc.corr) >= stats::quantile(abs(pc.corr),0.95,na.rm = T))]

    taxa.sig.axis.1 <- tibble::tibble(rowname=names(sig.corr),
                                      corr = sig.corr,
                                      p.adj = sig.p)

    taxa.sig.axis.1$rowname <- gsub(".rho$", "",taxa.sig.axis.1$rowname)
    pc.cor.taxa[[i]] <- taxa.sig.axis.1

  }
  return(pc.cor.taxa)
}


.check_taxa_axis <- function(axis.a.tax,axis.b.tax){
  if (length(axis.a.tax) == 0 && length(axis.b.tax) == 0){
    warning("Both of the choosen axis in `axis_plot` have no taxa satisfying criteria to plot")
  }

  if (length(axis.a.tax) == 0){
    warning("First of the choosen axis in `axis_plot` has no taxa satisfying criteria to plot")
  }

  if (length(axis.b.tax) == 0){
    warning("Second of the choosen axis in `axis_plot` has no taxa satisfying criteria to plot")
  }
}
