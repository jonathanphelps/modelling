isGoogleSource <- function(x){
  ifelse(grep("Google",x)==1,return(1),return(0))
}

isBingSource <- function(x){
  ifelse(grep("Bing",x)==1,return(1),return(0))
}


trimPercentSymbol <- function(x){
  return(gsub('%','',x))
}

makeColNamesUserFriendly <- function(ds) {
  # FIXME: Repetitive.
  
  # Convert any number of consecutive dots to a single space.
  colnames(ds) <- gsub(x = colnames(ds),
                       pattern = "(\\.)+",
                       replacement = " ")
  
  # Drop the spaces.
  colnames(ds) <- gsub(x = colnames(ds),
                       pattern = " ",
                       replacement = "")
  
  colnames(ds) <- gsub(x = colnames(ds),
                       pattern = "-",
                       replacement = "")
  
  colnames(ds) <- gsub(x = colnames(ds),
                       pattern = "/",
                       replacement = "")
  
  colnames(ds) <- gsub(x = colnames(ds),
                       pattern = "#",
                       replacement = "")
  
  return(ds)
}

prepareImpressionColumns <- function(df){
  col.names <- colnames(df)
  ix1 <- pmatch('SearchImprshare',col.names,nomatch = NA)
  if(!is.na(ix1)) {
    col.names[ix1] <- 'impression.share'
    colnames(df) <- col.names
    df$impression.share <- sapply(df$impression.share,trimPercentSymbol)
    col.names <- colnames(df)
    df$impression.share <- as.numeric(df$impression.share)
    NonNAindex <- which(!is.na(df$impression.share))
    firstNonNA <- min(NonNAindex)
    df <- df[firstNonNA:nrow(df),]
    col.names[pmatch(metric,col.names)] <- 'shown.Impressions'
    colnames(df) <- col.names
    df[,eval(metric)] <- round(df$shown.Impressions / (as.double(df$impression.share)/100))
    df[,eval(metric)] <-  forecast::na.interp(df[,eval(metric)])
  }
  return(df)
}

subsetDF <- function(df,select.columns){
  return(df[,eval(select.columns)] %>% tbl_df %>% arrange(Day))
}
