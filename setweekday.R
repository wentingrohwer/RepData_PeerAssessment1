setWeekend <- function(activityWeekday){
  copy <-activityWeekday
  for(i in 1:length(activityWeekday)){
    if (activityWeekday[i] %in% c("Monday","Tuesday","Wednesday","Thursday","Friday")){
      copy[i] <- 'weekday'
      }
    if (activityWeekday[i] %in% c("Saturday","Sunday")){
      copy[i] <- 'weekend'
    }
  }
  copy
}
