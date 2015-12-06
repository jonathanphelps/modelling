trimCity <- function(x) {
  return(gsub("UK/","",x))
}

trimWales <- function(x) {
  return(gsub("-Wales","",x))
}