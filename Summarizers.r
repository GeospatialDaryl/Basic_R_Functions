colMeaner <- function(inputCol){
  inputL <-  ! is.na(inputCol)
  mean(inputCol[inputL]) 
}