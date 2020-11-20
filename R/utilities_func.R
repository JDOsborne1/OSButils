#' Developer Comments
#'
#' @description A function which can be called in the middle of pipe pipelines
#'   to optionally print a comment at that step. Reliant on either an object
#'   called running_mode, or a config variable of the same name
#'
#'   Intended to be used in function with:
#'
#'   a) lots of piped steps abstracted as functions, which need some labelling.
#'
#'   b) lots of piped steps which are prone to failure, as a means to
#'   identifying where in a pipeline a failure occured.
#'
#'
#' @param input The data flow as it would go into the next step
#' @param comment The comment to print when prompted
#'
#' @return The same data flow, unchanged
#' @export
#'
#' @examples
devCom <- function(input, comment){
  if(isTRUE(config::get('running_mode') == "Dev") | get0("running_mode", envir = .GlobalEnv, ifnotfound = "Dev") == "Dev"){
    print(comment)
  }
  return(input)
}


#' One Stop Data Type Formatting
#'
#' A function to perform the routine data type transformations at the beggining of a data flow in one easy go.
#' Most data is read into R as character data, but most useful data we work with is not character data,
#' this package allows you to direct R to transform all the columns suiting a regular expression you define to
#' be transformed as either date, numeric or factor data.
#'
#' @param dfin The input dataframe
#' @param dateform The format of any dates in the dataframe as a string
#' @param datereg The regular expression to determine if a column name justifies treating it as a date
#' @param numreg The regular expression to determine if a column name justifies treating it as numeric
#' @param flagreg The regular expression to determine if a column name justifies treating it as a factor
#'
#' @return The dataframe with the appropriate data type transformations applied to the columns
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
data_type_format <- function(dfin, dateform = "%Y-%m-%d", datereg = "date", numreg = "value", flagreg = "flag") {
  dfout <- dfin %>%
    dplyr::mutate_at(dplyr::vars(dplyr::matches(datereg)),dplyr::funs(as.Date(as.character(.data),format=dateform)))%>%
    dplyr::mutate_at(dplyr::vars(dplyr::matches(numreg)),dplyr::funs(as.numeric))%>%
    dplyr::mutate_at(dplyr::vars(dplyr::matches(flagreg)),dplyr::funs(as.factor))
  return(dfout)
}



#' Distinctness checker
#'
#' @param df Source dataframe
#' @param on The NSE variable which you want to check the distinctiveness of
#'
#' @return
#' @export
#'
#' @examples
is.distinct <- function(df, on) {
  dplyr::distinct(df, {{on}}) %>% nrow() == nrow(df)
}

#' show the indistinct values
#'
#' @param df Source Dataframe
#' @param ... The variable, or variables which you want to view all indistinct combinations of.
#'
#' @return
#' @export
#' @importFrom rlang .data
#'
#' @examples
showIndistinct <- function(df, ...){
  df %>%
    dplyr::add_count(...) %>%
    dplyr::filter(.data$n > 1) %>%
    dplyr::arrange(...) %>%
    utils::View()
}


#' Spoof a dependency
#'
#' @description  This is a convinience function designed for use with {drake}.
#'   It will detect the dependencies automatically from function calls. In cases
#'   where background tasks, like a SQL query, are dependencies, then this can
#'   be used to flag that so the code is executed in the correct order.
#'
#'
#' @param a_tibble A table
#' @param a_dependency Another variable which you want to show as a dependency
#'
#' @return
#' @export
#'
#' @examples
utilDepend <- function(a_tibble, a_dependency){
  a_tibble
}

#' Get the range of a numeric vector
#'
#' @param a_vect a numeric vector
#'
#' @return
#' @export
#'
#' @examples
utilRange <- function(a_vect){
  max(a_vect, na.rm = T) - min(a_vect, na.rm = T)
}



#' Monitored Left Join
#'
#' @description This is currently /extremely/ slow, likely due to the extra
#'   steps injected in the middle, and the fact that no backend efficiencies in
#'   left_join() are applicable.
#'
#' @param x The LHS object
#' @param y The RHS object
#' @param by the joining column(s) between x & y
#' @param ... Additional argiments to be passed to dplyr::nest_join
#'
#' @return
#' @export
#' @importFrom rlang .data
#' @examples
left_join_monitor <- function(x, y, by, ...){
  nest_output <- dplyr::nest_join(x, y, by, ...)

  nest_output %>%
    dplyr::mutate(numrows = purrr::map(y, nrow)) %>%
    dplyr::select(.data$numrows) %>%
    tidyr::unnest(cols = c(.data$numrows)) %>%
    dplyr::pull(.data$numrows) %>%
    {sum(.data)/length(.data)} %>%
    {print(glue::glue("Dropout Rate = {scales::percent(.data)}"))}

  nest_output %>%
      tidyr::unnest(cols = c(y), keep_empty = TRUE)
}
