fillNA <- function(activity){
  ## make a copy of activity
  activityNoNA <- activity
  ## get the indice of missing values 
  NAindex <- which(is.na(activity$steps))
  ## get the sum of steps occurred by dates
  sumOfStepsByDate <- melt(tapply(activity$steps,activity$date, sum, na.rm=T))
  names(sumOfStepsByDate) <- c('dates','steps')
  ## only loop through the missing values, instead of all rows of activity
  for (i in 1:length(NAindex)){
    index <- NAindex[i]
    dateOftheMissingValue <- activity[index]$date
    temp <- sumOfStepsByDate[(sumOfStepsByDate$date == dateOftheMissingValue),]
    sumoftheSteps <- temp$steps
    ## there are 288 intervals in 1 day, the mean is the sum / no. intervals
    activityNoNA[index]$steps <- sumoftheSteps / 288
  }
  activityNoNA
}