# Function to plot the page views by quarter

library(ggplot2)
library(dplyr)

#==================================================================================================

quarterPlot <- function(cdi_df){
  
  # Set up a data frame with total views per quarter starting with Q4 2014
  cdi_df %>% mutate(qtr = as.yearqtr(date)) %>% count(qtr, wt = views) %>% data.frame() -> qtr_df
  qtr_df <- qtr_df[-c(1),]
  colnames(qtr_df)[1:2] <- c("yearQtr", "views")
  qtr_df$year <- format(qtr_df$yearQtr, format = "%Y")
  qtr_df$qtr <- format(qtr_df$yearQtr, format = "Q%q")
  qtr_df$qtr <- factor(qtr_df$qtr)
  levels(qtr_df$qtr) <- c("Q1 (JFM)", "Q2 (AMJ)", "Q3 (JAS)", "Q4 (OND)")
  
  #Make the quarter plot
  g <- ggplot(qtr_df, aes(x = year, y = views)) +
    geom_bar(stat = "identity") +
    facet_wrap(~qtr, ncol = 4) +
    theme_bw(base_size = 15) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0, unit = "pt")),
          plot.title = element_text(color="black", size=20, face = "bold"),
          panel.grid.major.x = element_blank()) +
    labs(y = "Page Views",
         title = "Data.gov/climate Quarterly Page Views",
         subtitle = "October 2014 - September 2018") +
    scale_y_continuous(breaks = c(0,10000,20000,30000), labels = c(0, "10K", "20K", "30K"))
  
  return(g)
}