# Function to visualize the GeoPlatform Resilience community portfilio

library(RJSONIO)
library(ggplot2)
library(png)
library(grid)

#==================================================================================================

gpPortfolioPlot <- function(){
  
  #Read the faceted information from the GeoPlatform Resilience community
  api_request <- fromJSON("https://ual.geoplatform.gov/api/items?usedBy=4eebc494059beab9fda54cb078927ddc&includeFacet=types&size=1")
  
  #Create a data frame of the 5 object types we care about
  cdi_objects <- data.frame(matrix(unlist(api_request[[3]][[1]][[3]][1:5]), nrow = 5, byrow = T)[,2:3], stringsAsFactors = FALSE)
  colnames(cdi_objects) <- c("object", "count") #Name the columns of the data frame
  cdi_objects$count <- as.integer(cdi_objects$count) #Change the counts from character to integer data
  
  #Make the plot
  obj_plot <- ggplot(cdi_objects, aes(x = object, y = count, fill = object)) +
    geom_bar(stat = "identity", width = 0.8) +
    scale_x_discrete(limits = c("dcat:Dataset", "regp:Service", "Layer", "Map", "Gallery"), #Put things in custom order L to R
                     labels = c("Datasets", "Services", "Layers", "Maps", "Galleries")) +
    scale_y_continuous(expand = c(0,0),
                       limits = c(0, max(cdi_objects$count) + 100),
                       breaks = seq(0,1200,by=200)) + #Customize the y axis breaks on this line
    scale_fill_manual(values = c("Map"           = rgb(247,64,64,maxColorValue = 255),
                                 "Gallery"       = rgb(250,187,64,maxColorValue = 255),
                                 "Layer"         = rgb(197,226,237,maxColorValue = 255),
                                 "regp:Service"  = rgb(91,160,64,maxColorValue = 255),
                                 "dcat:Dataset"  = rgb(71,67,170,maxColorValue = 255))) +
    theme(legend.position   = "none",
          axis.title.x      = element_blank(),
          axis.title.y      = element_blank(),
          panel.background  = element_blank(),
          axis.line         = element_line(size = 1),
          axis.ticks        = element_line(size = 1),
          axis.ticks.length = unit(0.15, "cm"),
          plot.margin       = unit(c(5.5,5.5,40.5,5.5), "pt"), #Adjust the bottom margin to ensure room for icons
          plot.title        = element_text(color="black", size=16, face = "bold"),
          plot.subtitle     = element_text(color="black", size=12),
          axis.text.x       = element_text(size=12, color="black"),
          axis.text.y       = element_text(size=12, color="black")) +
    geom_text(aes(x = object, #Add the actual value above each bar
                  y = count + 15,
                  label = cdi_objects$count,
                  fontface = "bold"),
              size = 6,
              vjust = 0) +
    labs(title = "GeoPlatform.gov Resilience Community Objects",
         subtitle = paste("Registered as of", format(Sys.Date(), "%B %d, %Y"))) + #Pull the current date in this line
    annotate("text", x = 4.5, y = cdi_objects[1,2] + 15, #Annotate the total objects used by the Resilience community
             label = paste("Total:", sum(cdi_objects$count)),
             size = 6,
             fontface = "bold",
             vjust = 0)
  
  obj_plot
  
  #Add the high resolution PNG icons
  
  for(i in 1:5){
    png_names <- c("dataset", "service", "layer", "map", "gallery")
    stage_png <- readPNG(paste0("icons/", png_names[i], ".png"))
    create_grob <- rasterGrob(stage_png)
    obj_plot <- last_plot() + annotation_custom(create_grob, xmin = i-0.5, xmax = i+0.5, ymin = -210, ymax = -100)
  }
  
  # Turn off clipping
  gt <- ggplot_gtable(ggplot_build(obj_plot))
  gt$layout$clip[gt$layout$name == 'panel'] <- 'off'
  grid.draw(gt)
  
  return(gt)

}
