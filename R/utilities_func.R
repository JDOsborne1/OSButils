#' Developer Comments
#'
#' @description A function which can be called in the middle of pipe pipelines to optionally print a comment at that step. Reliant on either an object called running_mode, or a config variable of the same name
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


#' Step incrementer and assigner
#'
#' @param val the object to step
#'
#' @return the value of that object, the object has been incremented in the background
#'
#' @examples
printAndIncrementStep <- function(val){
  name <- deparse(substitute(val))
  value <- get0(name, envir = .GlobalEnv, ifnotfound = 1)
  assign(name, value + 1, envir = .GlobalEnv)
  return(value)
}

#' Step title printer
#'
#' @param string The title of the step
#'
#' @return NULL The function instead prints the step title along with a timestamp
#' @export
#'
#' @examples
#' @examples
#' @examples
printStepTitle <- function(string){
  print(paste0("Step ", printAndIncrementStep(analysis_step), ": ", string, " - ", strftime(Sys.time(),"%H:%M")))
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
#' @examples
data_type_format <- function(dfin, dateform = "%Y-%m-%d", datereg = "date", numreg = "value", flagreg = "flag") {
  dfout <- dfin %>%
    dplyr::mutate_at(dplyr::vars(dplyr::matches(datereg)),dplyr::funs(as.Date(as.character(.),format=dateform)))%>%
    dplyr::mutate_at(dplyr::vars(dplyr::matches(numreg)),dplyr::funs(as.numeric))%>%
    dplyr::mutate_at(dplyr::vars(dplyr::matches(flagreg)),dplyr::funs(as.factor))
  return(dfout)
}



#' Add caveats function
#'
#' @param new_caveat The added caveat to put into the caveats folder
#'
#' @return None, assigns the caveat to the environment
#' @export
#'
#' @examples
addCaveats <- function(new_caveat){
  get0("caveats", envir = .GlobalEnv, ifnotfound = "") %>%
    c(new_caveat) %>%
    {assign("caveats", ., envir = .GlobalEnv)}
}

#' Distinctness checker
#'
#' @param df
#' @param on
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
#' @param df
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
showIndistinct <- function(df, ...){
  df %>%
    dplyr::add_count(...) %>%
    dplyr::filter(n > 1) %>%
    dplyr::arrange(...) %>%
    utils::View()
}


#' spoof a dependency
#'
#' @param a_tibble
#' @param a_dependency
#'
#' @return
#' @export
#'
#' @examples
utilDepend <- function(a_tibble, a_dependency){
  a_tibble
}

#' get range of a vector
#'
#' @param a_vect
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
#' @param x The LHS object
#' @param y The RHS object
#' @param by the joining column(s) between x & y
#' @param ... Additional argiments to be passed to dplyr::nest_join
#'
#' @return
#' @export
#'
#' @examples
left_join_monitor <- function(x, y, by, ...){
  nest_output <- dplyr::nest_join(x, y, by, ...)

  nest_output %>%
    dplyr::mutate(numrows = purrr::map(y, nrow)) %>%
    dplyr::select(numrows) %>%
    tidyr::unnest(cols = c(numrows)) %>%
    dplyr::pull(numrows) %>%
    {sum(.)/length(.)} %>%
    {print(glue::glue("Dropout Rate = {scales::percent(.)}"))}

  nest_output %>%
      tidyr::unnest(cols = c(y), keep_empty = TRUE)
}
