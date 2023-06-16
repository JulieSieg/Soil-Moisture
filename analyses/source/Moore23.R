# Data from Moore23

#info about the data:
#Soil moisture potential at 10 cm as measured with a gypsum block
#Note that the the values in this field are shown as positive values, but in reality are negative; 
#thus,greater magnitudes represent drier soils.

#get libraries needed
library(ggplot2)
library(gridExtra)
library(stringr)
library(tidyverse)
library(lubridate)
library(dplyr)
library(tidyr)
library(patchwork)

#set working directory
rm(list=ls(all=TRUE))
path <- '//wsl.localhost/Ubuntu/home/julie/Soil Moisture/Soil-Moisture/data/SEV LTER/Moore23'

setwd(path)
getwd()

data <- read.csv('Sevilleta_LTER_Hourly_Meteorological_Data_2010_2014.csv')
#print(colnames(data))
      
#the dataframe includes all meterological data collected - only need soil moisture
# subset to just get soil moisture data -> "Moisture_10_cm"       "Moisture_30_cm"
data <- subset(data, select = c(StationID, Date_Time, Year, Moisture_10_cm, Moisture_30_cm))

#combine all moisture data into one column by depth
long_data <- data %>%
  gather(key = "Depth", value = "value", Moisture_10_cm, Moisture_30_cm) %>%
  mutate(Depth = ifelse(Depth == "Moisture_10_cm", "10", "30"))

#remove NA values
long_data <- na.omit(long_data)

#convert all Station ID into character as opposed to numerics
long_data$StationID <- as.character(long_data$StationID)

#convert Datetime into plottable format
long_data$Date_Time <- as.POSIXct(long_data$Date_Time)


# Create an empty list to store the plots
plots <- list()

plotData <- function(d,y) {
  
  # Create an empty list to store the plots
  plots <- list()
  
    # Iterate through the Station IDs 
  for (site in unique(d$StationID)) {
    
    # Subset the data for the current year
    subset_data <- subset(d, StationID == site)
    
    # # Check if the subset data has enough date range
    # if (length(subset_data$datetime) < 2) {
    #   warning(paste("Insufficient data for Station", site, ". Skipping plot generation."))
    #   next  # Skip to the next iteration
    # }
    
    
    # Create the plot for the current year
    plot <- ggplot(subset_data, aes(x = Date_Time, y = value, color = factor(Depth))) +
      geom_line(linewidth = 0.8) +
      labs(x = "time", y = "Soil Moisture Potential", color = "depth (cm)", title = paste("Station", "", site)) +
      scale_color_discrete(labels = c("30", "5", "10", "20", "40")) +
      guides(color = guide_legend(override.aes = list(size = 3))) +
      theme_minimal() + 
      scale_x_datetime(date_labels = "%b")
    
    # Add the plot to the list
    plots[[site]] <- plot
    
  }
    
    # Combine the plots using patchwork and add a common legend
    combined_plots <- wrap_plots(plots, nrow = 4, ncol = 2)
    combined_plots <- combined_plots + plot_layout(guides = 'collect')
    
    # Add a title to the combined plots
    combined_plots <- combined_plots + plot_annotation(title = paste("Soil Moisture in SEV LTER", y),
                                                       theme = theme(plot.title = element_text(hjust = 0.5)))
    
    
    # Save combined_plots as PNG
    ggsave(paste("Moore23plot", y,".png"), plot = combined_plots, width = 10, height = 8, dpi = 300)
    
  
}



# create a for loop to plot each year seperately using the plotting function 
for (year in unique(long_data$Year)){
  
  subset_year <- subset(long_data, Year == year) 
  
  plotData(subset_year, year) #pass parameters subset_year and year into the plotData function created above 
  
}
