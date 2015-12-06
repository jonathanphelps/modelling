# Pads a time series (represented as a data frame) with missing periods with 0s
# Assumes first column is date and second is the series of observations


fillSeries <- function(raw.data,timescale="day") {
  
  colnames(raw.data) <- c("time","observations")
  
  # Convert the time column to a date column.
  # Accessing a column is done by using the '$' sign
  # like so: raw.data$time.
  raw.data$time <- as.Date(raw.data$time)
  
  # sort the data by time. The [*,] selects all rows that 
  # match the specified condition - in this case an order function
  # applied to the time column.
  sorted.data <- raw.data[order(raw.data$time),]
  
  # Find the length of the dataset
  data.length <- length(sorted.data$time)
  
  # Find min and max. Because the data is sorted, this will be 
  # the first and last element.
  time.min <- sorted.data$time[1]
  time.max <- sorted.data$time[data.length]
  
  # generate a time sequence with 1 month intervals to fill in
  # missing dates
  all.dates <- seq(time.min, time.max, by=timescale)
  
  # Convert all dates to a data frame. Note that we're putting 
  # the new dates into a column called "time" just like the 
  # original column. This will allow us to merge the data.
  all.dates.frame <- data.frame(list(time=all.dates))
  
  # Merge the two datasets: the full dates and original data
  merged.data <- merge(all.dates.frame, sorted.data, all=T)
  
  # The above merge set the new observations to NA.
  # To replace those with a 0, we must first find all the rows
  # and then assign 0 to them.
  merged.data$observations[which(is.na(merged.data$observations))] <- NA
  return(merged.data)
}