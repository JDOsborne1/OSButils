#' Index Caster Function
#'
#' @description A function to cast an inde as an measure of its deviation from a
#' standard, circumventing the issue of fractional changes in negative indicies.
#'
#' @param .index The supplied index value to be converted
#'
#' @export
#' @examples
#'      util_IndexCaster(50)
#'
#'      util_IndexCaster(200)
#'
util_IndexCaster <- function(.index){
  if(.index >= 1){
    output <- .index - 1
  } else {
    output <- -1 *((1/.index) - 1)
  }
  output
}
