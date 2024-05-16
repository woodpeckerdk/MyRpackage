
#' @title Enhance UMAP
#' @description
#' Further beautify plot based on UMAP
#'     this is a test function
#'
#' @param obj Seurat object \code{\link[base]{sum}}
#'
#' @return A new style of plot with arrows
#' @export
#'
#' @examples + d_umap(obj)
#' \donttest{
#' + d_umap(obj)}
#'
d_umap <- function(obj = NULL){
  df <- obj[["umap"]]@cell.embeddings %>% as.data.frame()
  arrow_len = 0.2
  xmin <- min(df$umap_1)
  xmax <- max(df$umap_1)
  xend <- xmin+(xmax - xmin)*arrow_len
  ymin <- min(df$umap_2)
  ymax <- max(df$umap_2)
  yend <- ymin + (ymax - ymin)*arrow_len
  d_arrow <- list(
    theme_classic(),
    theme(axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.line = element_blank(),
          legend.position = "none"),
    scale_color_hue(h=c(0,360),h.start = 90,c = 100,l = 60,direction = 1),
    annotate("segment",
             x = xmin - (ymax - ymin) * 0.05, xend = xend,
             y = ymin - (ymax - ymin) * 0.05, yend = ymin - (ymax - ymin) * 0.05,
             linewidth = 1.5,
             arrow = arrow(length = unit(0.15, "inches"))),
    annotate("segment",
             x = xmin - (ymax - ymin) * 0.05, xend = xmin - (ymax - ymin) * 0.05,
             y = ymin - (ymax - ymin) * 0.05, yend = yend,
             linewidth = 1.5,
             arrow = arrow(length = unit(0.15, "inches"))),
    annotate("text",
             x = xmin + (ymax - ymin) * 0.05,
             y = ymin - (ymax - ymin) * 0.1,
             label = "umap_1",
             size = 5),
    annotate("text",
             x = xmin - (ymax - ymin) * 0.1,
             y = ymin + (ymax - ymin) * 0.05,
             label = "umap_2",
             size = 5,
             angle = 90))
  return(d_arrow)}
#' @title Enhance UMAP
#' @author dk
#'
#' @param obj obj Seurat object
#'
#' @return A new style of plot with arrows
#' @export
#'
#' @examples + d_tsne(obj)
#' \donttest{
#' + d_tsne(obj)}
#'
d_tsne <- function(obj = NULL){
  df <- obj[["tsne"]]@cell.embeddings %>% as.data.frame()
  arrow_len = 0.2
  xmin <- min(df$tSNE_1)
  xmax <- max(df$tSNE_1)
  xend <- xmin+(xmax - xmin)*arrow_len
  ymin <- min(df$tSNE_2)
  ymax <- max(df$tSNE_2)
  yend <- ymin + (ymax - ymin)*arrow_len
  d_arrow <- list(
    theme_classic(),
    theme(axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.line = element_blank(),
          legend.position = "none"),
    scale_color_hue(h=c(0,360),h.start = 90,c = 100,l = 60,direction = 1),
    annotate("segment",
             x = xmin - (ymax - ymin) * 0.05, xend = xend,
             y = ymin - (ymax - ymin) * 0.05, yend = ymin - (ymax - ymin) * 0.05,
             linewidth = 1.5,
             arrow = arrow(length = unit(0.15, "inches"))),
    annotate("segment",
             x = xmin - (ymax - ymin) * 0.05, xend = xmin - (ymax - ymin) * 0.05,
             y = ymin - (ymax - ymin) * 0.05, yend = yend,
             linewidth = 1.5,
             arrow = arrow(length = unit(0.15, "inches"))),
    annotate("text",
             x = xmin + (ymax - ymin) * 0.05,
             y = ymin - (ymax - ymin) * 0.1,
             label = "tsne_1",
             size = 5),
    annotate("text",
             x = xmin - (ymax - ymin) * 0.1,
             y = ymin + (ymax - ymin) * 0.05,
             label = "tsne_2",
             size = 5,
             angle = 90))
  return(d_arrow)}
