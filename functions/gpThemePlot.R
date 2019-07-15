# Function to visualize the GeoPlatform Resilience community datasets, services, and layers by theme

library(reshape2)
library(ggplot2)
library(RJSONIO)
library(jpeg)
library(grid)

#==================================================================================================

gpThemePlot <- function(){
  
  datasets <- fromJSON("https://ual.geoplatform.gov/api/datasets?usedBy=4eebc494059beab9fda54cb078927ddc&includeFacet=themes&size=1")
  services <- fromJSON("https://ual.geoplatform.gov/api/services?usedBy=4eebc494059beab9fda54cb078927ddc&includeFacet=themes&size=1")
  layers <- fromJSON("https://ual.geoplatform.gov/api/layers?usedBy=4eebc494059beab9fda54cb078927ddc&includeFacet=themes&size=1")
  
  theme_id <- c("76b32145a6840fe5c42749db95f68861", #Arctic
                "f2c97bcff320c51c22b617d26fed8a04", #Coastal Flooding
                "10530dd4a8fba9980892d151b6757b4d", #Ecossytem Vulnerability
                "28fcf97a5a660eee256ebd54e3e9b1c8", #Energy Infrastructure
                "117232a5ac5f2ff01a74ab184c428d26", #Food Resilience
                "1473456c14c8d8fad888226ead372046", #Human Health
                "ed8ec3c29371490df8593f92ea5a145a", #Transportation
                "b1f6fd94d5028fa5a0ccf972aa9f6c8a", #Tribal Nations
                "cc835d56b2ca9a632694e34d419acdd9"  #Water
  )
  
  theme_df <- data.frame(theme_id = theme_id, datasets= NA, services = NA, layers = NA, stringsAsFactors=FALSE)   
  
  for(i in 1:9){
    dat_index <- grep(theme_id[i], datasets[[3]][[1]][[3]])
    ser_index <- grep(theme_id[i], services[[3]][[1]][[3]])
    lay_index <- grep(theme_id[i], layers[[3]][[1]][[3]])
    
    if(length(dat_index)==0){
      dat_count <- 0
    } else{
      dat_count <- datasets[[3]][[1]][[3]][[dat_index]][['count']]
    }
    
    if(length(ser_index)==0){
      ser_count <- 0
    } else{
      ser_count <- services[[3]][[1]][[3]][[ser_index]][['count']]
    }
    
    if(length(lay_index)==0){
      lay_count <- 0
    } else{
      lay_count <- layers[[3]][[1]][[3]][[lay_index]][['count']]
    }
    
    theme_df[i,2:4] <- c(dat_count, ser_count, lay_count)
  }
  
  theme_melt <- melt(data = theme_df, id = "theme_id")
  
  labels <- c("Arctic", "Coastal\nFlooding", "Ecosystem\nVulnerability", "Energy\nInfrastructure",
              "Food\nResilience", "Human\nHealth", "Transportation", "Tribal\nNations", "Water")
  
  obj_plot <- ggplot(theme_melt, aes(x = theme_id, y = value)) +
    geom_bar(aes(fill = variable), stat = "identity", position = "dodge", color = "grey20") +
    scale_x_discrete(limits = theme_id,
                     labels = labels) +
    scale_y_continuous(expand = c(0,0),
                       limits = c(0, max(theme_melt$value) + 50),
                       breaks = seq(0,500,by=100)) +
    scale_fill_manual(values = c("layers"   = rgb(197,226,237,maxColorValue = 255),
                                 "services"  = rgb(91,160,64,maxColorValue = 255),
                                 "datasets" = rgb(71,67,170,maxColorValue = 255)),
                      labels = c(" Datasets          ", " Services          ", " Layers")) +
    theme_grey(base_size = 14) +
    theme(axis.title.x      = element_blank(),
          axis.title.y      = element_blank(),
          legend.title      = element_blank(),
          axis.line         = element_line(size = 0.75, color = "grey20"),
          axis.ticks.length = unit(0.15, "cm"),
          plot.margin       = unit(c(5.5,5.5,60.5,5.5), "pt"),
          plot.title        = element_text(color="black", size=18, face = "bold"),
          plot.subtitle     = element_text(color="black", size=14),
          #legend.spacing.x = unit(0.5, "cm"),
          legend.position = c(0.75, 1.075),
          legend.box.background = element_rect(color = "transparent"),
          legend.direction = "horizontal") +
    labs(title = "Resilience Community Theme Portfolio",
         subtitle = paste("Registered as of", format(Sys.Date(), "%B %d, %Y"))) +
    geom_text(aes(y = value + 3, label = value, group = variable),
              position = position_dodge(width= 0.9),
              vjust = 0,
              hjust = 0.5, 
              size = 3,
              color = "grey20")
  
  obj_plot
  
  #Add hi res JPEG icons
  for(i in 1:9){
    jpg_names <- c("arctic",
                   "coastal",
                   "eco",
                   "energy",
                   "food",
                   "health",
                   "transportation",
                   "tribal",
                   "water")
    
    stage_jpg <- readJPEG(paste0("icons/icon_", jpg_names[i], ".jpg"))
    create_grob <- rasterGrob(stage_jpg)
    obj_plot <- last_plot() + annotation_custom(create_grob, xmin = i-1, xmax = i+1, ymin = -125, ymax = -55)
  }
  
  # Turn off clipping
  gt <- ggplot_gtable(ggplot_build(obj_plot))
  gt$layout$clip[gt$layout$name == 'panel'] <- 'off'
  grid.draw(gt)
  
  return(gt)
  
}