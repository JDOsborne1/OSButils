
# Slope Chart -------------------------------------------------------------

#' generate slope plot
#'
#' @param dataset
#' @param changes_in
#' @param for_the
#' @param across
#'
#' @return
#' @export
#'
#' @examples
osb_SlopePlot <- function(dataset, changes_in, for_the, across){

  ggplot(data = dataset,  aes(x = {{across}}, y={{changes_in}}, colour={{for_the}}, group = {{for_the}})) +
    geom_line(aes(color = {{for_the}}), alpha = 1, size = 1) +
    geom_point(aes(color = {{for_the}}), alpha = 1, size = 3) +
    geom_text_repel(
      data = dataset %>% filter({{across}} == levels({{across}})[1]),
      aes(label = paste0({{for_the}}, " : ", round({{changes_in}}))) ,
      hjust = "left",
      fontface = "bold",
      size = 4,
      point.padding = 0.4,
      nudge_x = -.5,
      direction = "x"
    ) +
    geom_text_repel(data = dataset %>% filter({{across}} == levels({{across}})[2]),
                    aes(label = paste0({{for_the}}, " : ", round({{changes_in}}))),
                    hjust = "right",
                    fontface = "bold",
                    size = 4,
                    nudge_x = .5,
                    direction = "y") +
    theme_classic() +
    theme(
      axis.line.y = element_blank()
      , axis.text.y = element_blank()
      , axis.ticks.y = element_blank()
      , legend.position = "none"
    )
}

# Dot Plot ----------------------------------------------------------------


#' generate dot plot
#'
#' @param dataset
#' @param changes_in
#' @param for_the
#' @param across
#' @param formatter
#'
#' @return
#' @export
#'
#' @examples
osb_DotPlot <- function(dataset, changes_in , for_the, across, formatter = function(x) x ) {
  ggplot(dataset, aes(x = {{for_the}})) +
    geom_point(
      aes(y = {{changes_in}}, color = {{across}})
      , size = 2

    ) +
    geom_line(
      aes(group = {{for_the}}, y = {{changes_in}})
      , alpha = 0.3
    ) +
    geom_text(
      data = filter(dataset, {{across}} == levels({{across}})[1])
      , aes(y = {{changes_in}}, label = formatter({{changes_in}}))
      , hjust = -.3
    ) +
    geom_text(
      data = filter(dataset, {{across}} == levels({{across}})[2])
      , aes(y = {{changes_in}}, label = formatter({{changes_in}}))
      , hjust = 1.3
    ) +
    coord_flip() +
    theme_classic() +
    scale_y_continuous(
      limits = c(
        min(pull(dataset, {{changes_in}})) * 0.9
        , max(pull(dataset, {{changes_in}})) * 1.1)
    ) +
    theme(
      axis.text.x = element_blank()
      , axis.ticks.x = element_blank()
      , axis.line.x = element_blank()
    )
}

# Drift Plot --------------------------------------------------------------


#' generate drift plot
#'
#' @param dataset
#' @param changes_in
#' @param for_the
#' @param across
#' @param formatter
#'
#' @return
#' @export
#'
#' @examples
osb_DriftPlot <- function(dataset, changes_in , for_the, across, formatter = function(x) x ) {
  data_values <- dataset %>% pull({{across}}) %>% levels()
  pivot_wider(dataset, names_from = {{across}}, values_from = {{changes_in}})
  print(data_values)
}

