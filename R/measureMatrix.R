#' Measure to DF
#' 
#' @param m matrix
#' @export
measureMatrixToDf <- function(m) {
  m[] <- ifelse(trimws(m) == "", NA, m)
  df <- as.data.frame(m, stringsAsFactors = FALSE)
  df <- df[colSums(!is.na(df)) > 0] # this removes empty columns and also their titles
  #df <- na.omit(df)
  df[] <- lapply(df, coerceIfNumeric)
  df
}

coerceIfNumeric <- function(x) {
  if (all(!is.na(as.numeric(na.omit(x))))) as.numeric(x)
  else x
}

getMode <- function(x){
  names(sort(-table(x)))[1]
}
