# Function to return all the page view data in a data frame

library(zoo)
library(lubridate)

#==================================================================================================

prepare_cdi_data <- function(){
  
  path <- "data/pageviews/"
  cdi_file_list <- list.files(path = path)
  num.months <- length(cdi_file_list)
  
  cdi_df <- data.frame()
  
  # Read data from the files
  for(i in 1:length(cdi_file_list)){
    cdi_report <- read.csv(file = paste0(path, cdi_file_list[i]),
                           skip = 10, stringsAsFactors = FALSE)[,1:2]
    
    # Cut off the final row with the total
    row.to.remove <- as.integer(rownames(tail(cdi_report, n=1)))
    cdi_report <- cdi_report[-c(row.to.remove),]
    
    # Assign column names
    colnames(cdi_report) <- c("date", "views")
    
    # Add new data to cdi_df each loop
    cdi_df <- rbind(cdi_df, cdi_report)
  }
  
  cdi_df$date <- as.Date(cdi_df$date, format = "%m/%d/%y")  # Format the date
  cdi_df$weekDay <- wday(cdi_df$date, label=TRUE)           # Add the day of the week
  cdi_df$month <- month(cdi_df$date, label=TRUE)            # Add the month
  cdi_df$views <- gsub(",", "", cdi_df$views)               # Remove any commas in the views
  cdi_df$views <- as.integer(cdi_df$views)                  # Format views as an integer
  
  cdi_df <- cdi_df[-c(1:2),]  # Remove 01 and 02 Sep 2014 (0 views)
  num.days <- dim(cdi_df)[1]
  rownames(cdi_df) <- seq(1:num.days) # Rename the rows to start at 1
  
  cdi_df$rank <- rank(cdi_df$views, ties.method = "min")  # Rank the views; ties get the low value
  cdi_df$percRank <- (cdi_df$rank - 1)/(num.days - 1)     # Calculate precentile rank
  cdi_df$rolling7DaySum <- rollsum(cdi_df$views, 7, fill = c(NA,NA,NA), align = "right") # Calculate the rolling 7 day sum
  
  # Clamp the data to the 0.1 to 0.9 range
  cdi_df$percRank <- replace(cdi_df$percRank, cdi_df$percRank>0.9, 0.9)
  cdi_df$percRank <- replace(cdi_df$percRank, cdi_df$percRank<0.1, 0.1)
  
  return(cdi_df)
}