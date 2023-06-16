#Data from Andrus22 

#Soil moisture continuous

#get libraries needed
library(ggplot2)
library(gridExtra)
library(dplyr)



#set working directory
rm(list=ls(all=TRUE))
path <- 'C:/Users/julie/Documents/Soil Moisture/Andrus22'

setwd(path)
getwd()

#get the downloaded data
soilMoisture <- read.csv('soilmoisture_continuous_micromet.ra.data.csv')


#SM = soilmoisture in m^3 taken continuously during 2018 and 2019 
#mmt_num = the measurement number. multiple measurements were made at each site
#location: Niwot Ridge, Colorado 
#this is a test with how to make plots, etc
#the location is wrong - not all data on the site is from the SEV LTER - consider this a test run
#(I later realized everything would be easier if I used functions - for future reference)

#check on all of the variables
unique(soilMoisture$local_site)
summary(soilMoisture$local_site)
head(soilMoisture$local_site)
str(soilMoisture$local_site)

count <- sum(soilMoisture$local_site == "MRS8")
print(count)


summary(soilMoisture$DateTime)
head(soilMoisture$DateTime)
str(soilMoisture$DateTime)

summary(soilMoisture$mmt_num)
head(soilMoisture$mmt_num)
str(soilMoisture$mmt_num)

summary(soilMoisture$SM)
head(soilMoisture$SM)
str(soilMoisture$SM)

# Create a function to plot the data

plotData <- function(d,n){
  
  # Create an empty list to store the plots
  plots <- list()
  
  # Iterate through the local sites in sm18$local_site
  for (site in unique(d$local_site)) {
    # Subset the data for the current local site
    subset_data <- subset(d, local_site == site)
    
    # Create the plot for the current local site
    plot <- ggplot(subset_data, aes(x = DateTime, y = SM, color = factor(mmt_num))) +
      geom_point(size = 0.8) +
      geom_line(linewidth = 0.5) + 
      labs(x = "2018", y = "Soil Moisture (m^3)", color = "mmt", title = site) +
      scale_color_discrete(labels = c("1", "2", "3", "4", "5")) +
      guides(color = guide_legend(override.aes = list(size = 3))) +
      theme_minimal()
    
    # Add the plot to the list
    plots[[site]] <- plot
  }
  
  # Arrange the plots with labels by site 
  combined_plots <- grid.arrange(grobs = plots, nrow = 2, ncol = 2, top = "Soil Moisture by Local Site")
  
  # Save combined_plots as PNG
  ggsave(paste("PlotAndrus22", n,".png"), plot = combined_plots, width = 10, height = 8, dpi = 300)
  
}

# Convert 'time' column to proper date-time format
soilMoisture$DateTime <- as.POSIXct(soilMoisture$DateTime)

# Subset the data frame for just 2018
sm18 <- subset(soilMoisture, format(DateTime, "%Y") == "2018")

n18 <- "2018"

plotData(sm18, n18)

# Subset the data frame for just 2019
sm19 <- subset(soilMoisture, format(DateTime, "%Y") == "2019")

n19 <- "2019"

plotData(sm19, n19)

#### Mean all of the measurement numbers for each time value

#create function for plotting the means
plotDataMean <- function(d,n){
  
  # Create an empty list to store the plots
  plots <- list()
  
  # Iterate through the local sites in sm18$local_site
  for (site in unique(d$local_site)) {
    # Subset the data for the current local site
    subset_data <- subset(d, local_site == site)
    
    # Create the plot for the current local site
    plot <- ggplot(subset_data, aes(x = DateTime, y = SM)) +
      geom_point(size = 0.8, color = "#0072B2") +
      geom_line(linewidth = 0.5, color = "#0072B2") + 
      labs(x = "2018", y = "Soil Moisture (m^3)", title = site) +
      scale_color_discrete(labels = c("1", "2", "3", "4", "5")) +
      guides(color = guide_legend(override.aes = list(size = 3))) +
      theme_minimal()
    
    # Add the plot to the list
    plots[[site]] <- plot
  }
  
  # Arrange the plots with labels by site 
  combined_plots <- grid.arrange(grobs = plots, nrow = 2, ncol = 2, top = "Soil Moisture by Local Site")
  
  # Save combined_plots as PNG
  ggsave(paste("PlotAndrus22", n,".png"), plot = combined_plots, width = 10, height = 8, dpi = 300)
  
}


# Group the data by DateTime and calculate the mean of SM values
sm18mean <- sm18 %>%
  group_by(DateTime, local_site) %>%
  summarise(SM = mean(SM))

n18mean <- "mean 2018"

plotDataMean(sm18mean, n18mean)

sm19mean <- sm19 %>%
  group_by(DateTime, local_site) %>%
  summarise(SM = mean(SM, na.rm = TRUE))

n19mean <- "mean 2019"

plotDataMean(sm19mean, n19mean)

