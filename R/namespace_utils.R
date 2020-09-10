#' Get the currently used namespace
#'
#' @description Function to get the used namespace and prepend it to the
#'   function name.
#'
#'   When it is used on a function with no defintion, or a function which is a
#'   part of the base packages, it will simply return the input unchanged.
#'
#' @param .function_name
#'
#' @return
#' @export
#'
#' @examples
get_used_namespace <- function(.function_name){
  if (length(findFunction(.function_name)) == 0){
    print("Function is not currently defined anywhere")
    return(.function_name)
  } else if (is.null(attr(findFunction(.function_name)[[1]], "name"))){
    print("Function environment has no name, likely a base function")
    return(.function_name)
  } else {
    paste0(gsub("^.*:", "", attr(methods::findFunction(.function_name)[[1]], "name")), "::", .function_name)
  }
}


#' Prepend Namespace
#'
#' @return
#' @export
#'
#' @examples
prepend_namespace <- function(){
  # Gets The active Documeent
  ctx <- rstudioapi::getActiveDocumentContext()

  # Checks that a document is active
  if (!is.null(ctx)) {

    # Extracts selection as a string
    selected_text <- ctx$selection[[1]]$text

    # modify string
    selected_text <- get_used_namespace(selected_text)

    # replaces selection with string
    rstudioapi::modifyRange(ctx$selection[[1]]$range, selected_text)
  }

}
