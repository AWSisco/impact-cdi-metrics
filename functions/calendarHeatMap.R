#' Calendar Heatmap
# 
#' Creates a color coded calendar to visualize time series data.
#' 
#' @param dates       A vector containing the dates in `Date` format.
#' @param values      A vector containing the corresponding values as numeric.
#' @param title       Main plot title (optional).
#' @param subtitle    Main plot subtitle (optional).
#' @param legendtitle Legend title (optional).
#'   
#' @return ggplot object

#==================================================================================================

calendarHeatMap <- function(dates, values, title = "", subtitle = "", legendtitle = ""){
  
  # Parameter checks
  if(missing(dates)){
    stop("Need to specify a dates vector.")
  }
  if(missing(values)){
    stop("Need to specify a values vector.")
  }
  if(!is.Date(dates)){
    stop("Dates vector need to be in Date format.")
  }
  if(length(dates) != length(values)){
    stop("Dates and values need to have the same length.")
  }
  
  my_theme <- function() {
    
    # Colors
    color.background = "white"
    color.text = "black"
    
    # Begin construction of chart
    theme_bw(base_size=15) + # base_size is the base font size
      
      # Format background colors
      theme(panel.background = element_rect(fill=color.background, color=color.background)) +
      theme(plot.background  = element_rect(fill=color.background, color=color.background)) +
      theme(panel.border     = element_rect(color=color.background)) +
      theme(strip.background = element_rect(fill=color.background, color=color.background)) +
      
      # Format the grid
      theme(panel.grid.major = element_blank()) +
      theme(panel.grid.minor = element_blank()) +
      theme(axis.ticks       = element_blank()) +
      
      # Format the legend
      theme(legend.position = "right") +
      theme(legend.text = element_text(size = 10, color = "black")) +
      theme(legend.title = element_text(size = 10, color = "black")) +
      
      # Format title and axis labels
      theme(plot.title       = element_text(color="black", size=20, face = "bold")) +
      theme(axis.title.x     = element_text(size=14, color="black", face = "bold")) +
      theme(axis.title.y     = element_text(size=14, color="black", vjust=1.25)) +
      theme(axis.text.x      = element_text(size=10, hjust = 0.5, color = "black")) +
      theme(axis.text.y      = element_text(size=10, color = "black")) +
      theme(strip.text       = element_text(face = "bold")) + 
      
      # Plot margins
      theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
  }
  
  # Create empty calendar
  min.date <- as.Date(paste(format(min(dates), "%Y"),"-1-1",sep = "")) # Gets the min year
  max.date <- as.Date(paste(format(max(dates), "%Y"),"-12-31", sep = "")) # Gets the max year
  df <- data.frame(date = seq(min.date, max.date, by="days"), value = NA)
  
  # Fill in values
  df$value[match(dates, df$date)] <- values
  
  df$year  <-  as.factor(format(df$date, "%Y")) # 4-digit year
  df$month <- as.numeric(format(df$date, "%m")) # Decimal month
  df$doy   <- as.numeric(format(df$date, "%j")) # Decinal day of year
  df$dow <- as.numeric(format(df$date, "%w")) #Decimal weekday (0 = Sunday)
  df$woy <- as.numeric(format(df$date, "%U")) #Decimal week of year (starting on Sunday)
  
  df$dowmapped <- ordered(df$dow, levels = 6:0)
  levels(df$dowmapped) <- rev(c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))
  
  g <- ggplot(df, aes(woy, dowmapped, fill = value)) + 
    geom_tile(colour = "darkgrey") + 
    facet_wrap(~year, ncol = 1) + # Facet for years
    coord_fixed(xlim = c(-1, 54), ylim = c(0,8)) +
    scale_x_continuous(expand = c(0,0), breaks = c(1.5, 5.5, 10, 14.5, 18.5, 23, 27.5, 31.5, 36, 40.5, 44.5, 49),
                       labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
    scale_y_discrete(expand = c(0,0)) +
    my_theme() + 
    theme(legend.key = element_rect(color = "black")) +
    scale_fill_distiller(palette = "Purples",
                         direction = 1,
                         na.value = "grey80",
                         guide = guide_colorbar(barwidth = unit(6, units = "mm"),
                                                barheight = unit(75, units = "mm"),
                                                direction = "vertical",
                                                ticks = FALSE),
                         breaks=c(0.1,0.5,0.9),
                         labels = c("10th", "50th", "90th")) +
    labs(x = NULL, 
         y = NULL, 
         title = title, 
         subtitle = subtitle,
         fill = legendtitle)
  
  my.lines<-data.frame(x=numeric(), 
                       y=numeric(), 
                       xend=numeric(), 
                       yend=numeric(), 
                       year=character())
  
  for(years in levels(df$year)){
    df.subset <- df[df$year == years,]
    
    y.start <- df.subset$dow[1] # First day to plot on y-axis (0:6)
    x.start <- df.subset$woy[1] # First week to plot on x-axis (1:53)
    
    x.top.left <- ifelse(y.start == 0, x.start - 0.5, x.start + 0.5) # If starting on Sunday, draw top left at Sunday Week 1, else draw top left at Sunday Week 2.
    y.top.left <- 7.5 # Very top will always be 7.5
    x.top.right <- df.subset$woy[nrow(df.subset)] + 0.5 # Based on last week of year being 52 or 53, determine far right extent.
    y.top.right <- 7.5 # Very top will always be 7.5
    
    x.mid.left01 <- x.start - 0.5 # Middle left horizontal starts here on the x-axis
    y.mid.left01 <- 7.5 - y.start # Middle left horizontal starts here on the y-axis
    x.mid.left02 <- x.start + 0.5 # Middle left horizontal ends here on the x-axis
    y.mid.left02 <- 7.5 - y.start # Middle left horizontal ends here on the y-axis
    
    x.bottom.left <- x.start - 0.5 # Very bottom will always be 0.5
    y.bottom.left <- 0.5 # Very bottom will always be 0.5
    x.bottom.right <- ifelse(max(df.subset$woy > 52) == 6, df.subset$woy[nrow(df.subset)] + 0.5, df.subset$woy[nrow(df.subset)] - 0.5) # Bottom right will either be 52.5 or 53.5.
    y.bottom.right <- 0.5 # Very bottom will always be 0.5
    
    my.lines<-rbind(my.lines,
                    data.frame(x    = c(x.top.left, x.bottom.left, x.mid.left01, x.top.left, x.bottom.left), 
                               y    = c(y.top.left, y.bottom.left, y.mid.left01, y.top.left, y.bottom.left),
                               xend = c(x.top.right, x.bottom.right, x.mid.left02, x.mid.left02, x.mid.left01), 
                               yend = c(y.top.right, y.bottom.right, y.mid.left02, y.mid.left02, y.mid.left01), 
                               year = years))
    
    # Lines to separate months
    for (j in 1:12)  {
      df.subset.month <- max(df.subset$doy[df.subset$month == j]) # Find max day of month
      x.month <- df.subset$woy[df.subset.month] # Last week of year in the month
      y.month <- df.subset$dow[df.subset.month] # Last day of week in the month
      
      x.top.mid <- x.month + 0.5 # Top right extent of month
      y.top.mid <- 7.5 # Top extent of month
      
      x.mid.mid01 <- x.month - 0.5
      y.mid.mid01 <- 7.5 - y.month - 1
      x.mid.mid02 <- x.month + 0.5
      y.mid.mid02 <- 7.5 - y.month - 1
      
      x.bottom.mid <- ifelse(y.month == 6, x.month + 0.5, x.month - 0.5) # If last day of month is Sunday, then right extent of month is equal at top and bottom.
      y.bottom.mid <- 0.5 # Bottom extent of month
      
      my.lines<-rbind(my.lines,
                      data.frame(x    = c(x.top.mid, x.mid.mid01, x.mid.mid01), 
                                 y    = c(y.top.mid, y.mid.mid01, y.mid.mid01),
                                 xend = c(x.mid.mid02, x.mid.mid02, x.bottom.mid), 
                                 yend = c(y.mid.mid02, y.mid.mid02, y.bottom.mid), 
                                 year = years))
    }
  }
  
  month.check.ind <- seq(from = 8, to = 41, by = 3)
  delete.ind <- vector()
  
  for(i in 1:length(month.check.ind)){
    if(sum(as.logical(my.lines[month.check.ind[i],1:4]==my.lines[month.check.ind[i]-1,1:4])==TRUE)==4){
      delete.ind <- append(delete.ind, c(month.check.ind[i]-1, month.check.ind[i]))
    }
  }
  
  my.lines <- my.lines[-c(delete.ind),]
  
  # Add lines
  g <- g + geom_segment(data=my.lines, aes(x,y,xend=xend, yend=yend), lineend = "square", color = "black", inherit.aes=FALSE)
  
  return(g)
}