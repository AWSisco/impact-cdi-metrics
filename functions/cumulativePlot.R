# Function to calculate annual cumulative page views

library(plyr)
library(ggplot2)

#==================================================================================================

cumulativePlot <- function(cdi_df){
  
  cdi_df <- subset(cdi_df, year(cdi_df$date)>2014)
  
  cdi_df$date <- as.Date(cdi_df$date)
  cumu <- ddply(cdi_df,.(year(date)),transform, cumViews = cumsum(views))
  
  month <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","")
  start <- c(0,32,60,91,121,152,182,213,244,274,305,335,365)
  end <- c(32,60,91,121,152,182,213,244,274,305,335,365,410)
  color <- c("light","dark","light","dark","light","dark",
             "light","dark","light","dark","light","dark","light")
  
  rect_df <- data.frame(month, start, end, color)
  
  g <- ggplot(rect_df) + geom_rect(aes(xmin = start, xmax = end, fill = color),
                                   ymin = -Inf, ymax = Inf, alpha = 0.2,
                                   data = rect_df) +
    geom_step(data= cumu, aes(x = yday(date), y = cumViews, color = factor(year(date))), size = 1) +
    scale_x_continuous(breaks = c((32+0)/2,
                                  (60+32)/2,
                                  (91+60)/2,
                                  (121+91)/2,
                                  (152+121)/2,
                                  (182+152)/2,
                                  (213+182)/2,
                                  (244+213)/2,
                                  (274+244)/2,
                                  (305+274)/2,
                                  (335+305)/2,
                                  (365+335)/2),
                       labels = c("Jan","Feb","Mar","Apr","May","Jun",
                                  "Jul","Aug","Sep","Oct","Nov","Dec"),
                       expand = c(0,0),
                       limits = c(0,410)) +
    scale_y_continuous(expand = c(0,0), limits = c(0,130000),
                       labels = c(0, "25K","50K","75K","100K","125K")) +
    labs(title = "Data.gov/climate Cumulative Page Views",
         subtitle = "Ending November 2018") +
    theme_grey(base_size = 15) +
    theme(legend.position = "none",
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title = element_blank(),
          axis.line = element_line(),
          panel.grid.major.y = element_line(color = "grey60"),
          panel.grid.minor.y = element_line(color = "grey80"),
          plot.title = element_text(size=14, face = "bold"),
          plot.subtitle = element_text(size = 12)) +
    scale_fill_manual(values = c("grey65","grey100")) +
    scale_color_brewer(palette = "Set1") +
    geom_point(data = subset(cumu, yday(cumu$date)==365|cumu$date=="2018-11-30"),
               aes(x = yday(date), y = cumViews), size = 3, color = c("#e41a1c","#377eb8","#4daf4a","#984ea3")) +
    geom_text(data = subset(cumu, yday(cumu$date)==365|cumu$date=="2018-11-30"),
              aes(x = yday(date) + 5, y = cumViews, label = year(date)), hjust = 0,
              color = c("#e41a1c","#377eb8","#4daf4a","#984ea3"),
              fontface = "bold",
              size = 6)
  return(g)
}