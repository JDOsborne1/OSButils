
# Slope Chart -------------------------------------------------------------

#' generate slope plot
#'
#' @param dataset The source dataset
#' @param changes_in The variable to show changes in
#' @param for_the The variable to split those changes by
#' @param across The two timeframes for the changes to occur
#'
#' @return
#' @export
#'
#' @examples
osb_SlopePlot <- function(dataset, changes_in, for_the, across){

  ggplot2::ggplot(data = dataset,  ggplot2::aes(x = {{across}}, y={{changes_in}}, colour={{for_the}}, group = {{for_the}})) +
    ggplot2::geom_line(ggplot2::aes(color = {{for_the}}), alpha = 1, size = 1) +
    ggplot2::geom_point(ggplot2::aes(color = {{for_the}}), alpha = 1, size = 3) +
    ggrepel::geom_text_repel(
      data = dataset %>% dplyr::filter({{across}} == levels({{across}})[1]),
      ggplot2::aes(label = paste0({{for_the}}, " : ", round({{changes_in}}))) ,
      hjust = "left",
      fontface = "bold",
      size = 4,
      point.padding = 0.4,
      nudge_x = -.5,
      direction = "x"
    ) +
    ggrepel::geom_text_repel(data = dataset %>% dplyr::filter({{across}} == levels({{across}})[2]),
                    ggplot2::aes(label = paste0({{for_the}}, " : ", round({{changes_in}}))),
                    hjust = "right",
                    fontface = "bold",
                    size = 4,
                    nudge_x = .5,
                    direction = "y") +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.line.y = ggplot2::element_blank()
      , axis.text.y = ggplot2::element_blank()
      , axis.ticks.y = ggplot2::element_blank()
      , legend.position = "none"
    )
}

# Dot Plot ----------------------------------------------------------------


#' generate dot plot
#'
#' @param dataset The source dataset
#' @param changes_in The variable to show changes in
#' @param for_the The variable to split those changes by
#' @param across The two timeframes for the changes to occur
#' @param formatter A formatter function to control the labels
#'
#' @return
#' @export
#'
#' @examples
osb_DotPlot <- function(dataset, changes_in , for_the, across, formatter = function(x) x ) {
  ggplot2::ggplot(dataset, ggplot2::aes(x = {{for_the}})) +
    ggplot2::geom_point(
      ggplot2::aes(y = {{changes_in}}, color = {{across}})
      , size = 2

    ) +
    ggplot2::geom_line(
      ggplot2::aes(group = {{for_the}}, y = {{changes_in}})
      , alpha = 0.3
    ) +
    ggplot2::geom_text(
      data = dplyr::filter(dataset, {{across}} == levels({{across}})[1])
      , ggplot2::aes(y = {{changes_in}}, label = formatter({{changes_in}}))
      , hjust = -.3
    ) +
    ggplot2::geom_text(
      data = dplyr::filter(dataset, {{across}} == levels({{across}})[2])
      , ggplot2::aes(y = {{changes_in}}, label = formatter({{changes_in}}))
      , hjust = 1.3
    ) +
    ggplot2::coord_flip() +
    ggplot2::theme_classic() +
    ggplot2::scale_y_continuous(
      limits = c(
        min(dplyr::pull(dataset, {{changes_in}})) * 0.9
        , max(dplyr::pull(dataset, {{changes_in}})) * 1.1)
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank()
      , axis.ticks.x = ggplot2::element_blank()
      , axis.line.x = ggplot2::element_blank()
    )
}

# Drift Plot --------------------------------------------------------------


#' generate drift plot
#'
#' @param dataset The source dataset
#' @param changes_in The variable to show changes in
#' @param for_the The variable to split those changes by
#' @param across The two timeframes for the changes to occur
#' @param formatter A formatter function to control the labels
#'
#'
#' @return
#' @export
#'
#' @examples
osb_DriftPlot <- function(dataset, changes_in , for_the, across, formatter = function(x) x ) {
  data_values <- dataset %>%
    dplyr::pull({{across}}) %>%
    levels()

  tidyr::pivot_wider(
    dataset
    , names_from = {{across}}
    , values_from = {{changes_in}}
    )

  print(data_values)
}

