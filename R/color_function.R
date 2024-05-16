#' Browse Discrete color
#'
#' @description
#' Use a bar chart to display your discrete colors.
#'
#' @param mycolor Your custom colors(a vector).
#'
#' @return A ggplot2 object.
#' @export
#'
#' @examples browse_color_discrete(mycolor)
#' \donttest{
#' browse_color_discrete(mycolor)}
browse_color_discrete <- function(mycolor){
  color_len <- seq_along(mycolor)
  x <- LETTERS[color_len]
  y <- rep(1,length(color_len))
  df <- data.frame(x,y)
  ggplot(df)+
    geom_bar(aes(x,y,fill = x),stat = "identity")+
    scale_fill_manual(values = mycolor)+
    theme_classic()+
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "none")
}
#' Browse gradient color
#'
#' @param mycolor Your custom colors(a vector)
#'
#' @return A ggplot2 object.
#' @export
#'
#' @examples browse_color_gradient(mycolor)
#' \donttest{
#' browse_color_gradient(mycolor)}
browse_color_gradient <- function(mycolor){
  df <- data.frame(
    x = seq(1,100,1))
  ggplot(df) +
    geom_tile(aes(1,x,fill = x),width = 0.5) +
    scale_fill_gradientn(colors = mycolor)+
    scale_x_continuous(limits = c(0,2))+
    theme_classic()+
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "none")
}
