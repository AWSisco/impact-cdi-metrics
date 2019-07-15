# SET UP
source("functions/prepare_cdi_data.R")
source("functions/cumulativePlot.R")
source("functions/meanDailyPlot.R")
source("functions/calendarHeatMap.R")
source("functions/quarterPlot.R")
source("functions/gpPortfolioPlot.R")
source("functions/gpThemePlot.R")

#==================================================================================================
# GET THE DATA
cdi_df <- prepare_cdi_data()

# Option to write cdi_df to a local CSV
# write.csv(cdi_df, file = "cdi_df.csv", row.names = FALSE, quote = FALSE)

#==================================================================================================
# CUMULATIVE PAGE VIEWS
cumulativePlot(cdi_df)

ggsave(filename = "cumulativePlot.png", path = "figures", plot = last_plot(),
       dpi = 300, width = 7, height = 5.5, units = "in")

#==================================================================================================
# MEAN DAILY PAGE VIEWS
meanDailyPlot(cdi_df)

ggsave(filename = "meanDailyPlot.png", path = "figures", plot = last_plot(), 
       dpi = 300, width = 11, height = 4, units = "in")

#==================================================================================================
# CALENDAR HEAT MAP
calendarHeatMap(cdi_df$date, cdi_df$percRank,
                title = "Climate Data Initiative",
                subtitle = "Data.gov/climate Daily Page Views",
                legendtitle = "Percentile")

ggsave(filename = "calendarHeatMap_nov2018.png", path = "figures", plot = last_plot(),
       dpi = 300, width = 9, height = 8, units = "in")

#==================================================================================================
# QUARTERLY PAGE VIEWS
quarterPlot(cdi_df)

ggsave(filename = "quarterPlot.png", path = "figures", plot = last_plot(),
       dpi = 300, width = 11.5, height = 3.25, units = "in")

#==================================================================================================
# GEOPLATFORM PORTFOLIO PLOT

geo_port <- gpPortfolioPlot()

png("figures/gpPortfolioPlot.png", width = 2400, height = 1600, res = 300)
grid.draw(geo_port)
dev.off()

#==================================================================================================
# GEOPLATFORM THEME PLOT

geo_theme <- gpThemePlot()

png("figures/gpThemePlot.png", width = 4000, height = 2000, res = 300)
grid.draw(geo_theme)
dev.off()
