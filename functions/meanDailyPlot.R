# Function to calculate the mean daily page views each month

#==================================================================================================

meanDailyPlot <- function(cdi_df){
  
  #Get years
  cdi_df$year <- year(cdi_df$date)
  
  #Aggregate views on months and year and get mean
  cdi_means <- data.frame(aggregate(views ~ month + year , cdi_df , mean))
  cdi_means <- cdi_means[-c(4),]
  cdi_means$year <- as.factor(cdi_means$year)
  
  g <- ggplot() + geom_line(data = cdi_means,
                            aes(x = month, y = views, group = year, color = year),
                            size = 1) +
    scale_x_discrete(limits=c("Jan","Feb","Mar","Apr","May","Jun",
                              "Jul","Aug","Sep","Oct","Nov","Dec")) +
    scale_color_brewer(palette = "Set1") +
    geom_point(data = cdi_means, aes(x = month, y = views, color = year),
               size = 3, shape = 21, fill = "white", stroke = 2.5) +
    theme_bw(base_size = 15) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          panel.grid = element_blank(),
          axis.title = element_blank(),
          plot.title = element_text(color="black", size=16, face = "bold"),
          panel.border = element_blank(),
          legend.title = element_blank(),
          legend.key.width = unit(2, "line")) +
    labs(title = "Data.gov/climate Mean Daily Page Views",
         subtitle = "Ending November 2018") +
    guides(color = guide_legend(reverse = TRUE))
  
  return(g) 
}