util_IndexCaster <- function(.index){
  if(.index >= 1){
    output <- .index - 1
  } else {
    output <- -1 *((1/.index) - 1)
  }
  output
}
