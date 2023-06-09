#Data from Andrus22 

#Soil moisture continuous

#get libraries needed
library(ggplot2)
library(gridExtra)


#set working directory
rm(list=ls(all=TRUE))
path <- 'C:/Users/julie/Documents/Soil Moisture/Andrus22'

setwd(path)
getwd()

#get the downloaded data
soilMoisture <- read.csv('soilmoisture_continuous_micromet.ra.data.csv')

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

#SM = soilmoisture in m^3 taken continuously during 2018 and 2019 
#mmt_num = the measurement number. multiple measurements were made at each site
#location: Niwot Ridge, Colorado 
#this is a test with how to make plots, etc
#the location is wrong - not all data on the site is from the SEV LTER


# Convert 'time' column to proper date-time format
soilMoisture$DateTime <- as.POSIXct(soilMoisture$DateTime)

# Subset the data frame for just 2018
sm18 <- subset(soilMoisture, format(DateTime, "%Y") == "2018")

# Split the soilMoisture dataframe by site 
split_data <- split(sm18, sm18$local_site)

# Print the names of the resulting data frames
print(names(split_data))

for (variable_value in names(split_data)) {
  assign(variable_value, split_data[[variable_value]])
}


#### Create plots of each study site for 2018 and arrange them together

# Create an empty list to store the plots
plots <- list()

# Iterate through the local sites in sm18$local_site
for (site in unique(sm18$local_site)) {
  # Subset the data for the current local site
  subset_data <- subset(sm18, local_site == site)
  
  # Create the plot for the current local site
  plot <- ggplot(subset_data, aes(x = DateTime, y = SM, color = factor(mmt_num))) +
    geom_point(size = 0.8) +
    labs(x = "2018", y = "Soil Moisture (m^3)", color = "mmt", title = site) +
    scale_color_discrete(labels = c("1", "2", "3", "4", "5")) +
    guides(color = guide_legend(override.aes = list(size = 3))) +
    theme_minimal()
  
  # Add the plot to the list
  plots[[site]] <- plot
}

# Arrange the plots with labels by site 
grid.arrange(grobs = plots, nrow = 2, ncol = 2, top = "Soil Moisture by Local Site")


####Repeat above code for 2019


# Subset the data frame for just 2018
sm19 <- subset(soilMoisture, format(DateTime, "%Y") == "2019")

# Split the soilMoisture dataframe by site 
split_data19 <- split(sm19, sm19$local_site)

# Print the names of the resulting data frames
print(names(split_data19))

for (variable_value in names(split_data19)) {
  assign(variable_value, split_data19[[variable_value]])
}


#### Create plots of each study site for 2018 and arrange them together

# Create an empty list to store the plots
plots <- list()

# Iterate through the local sites in sm18$local_site
for (site in unique(sm19$local_site)) {
  # Subset the data for the current local site
  subset_data <- subset(sm19, local_site == site)
  
  # Create the plot for the current local site
  plot <- ggplot(subset_data, aes(x = DateTime, y = SM, color = factor(mmt_num))) +
    geom_point(size = 0.8) +
    labs(x = "2019", y = "Soil Moisture (m^3)", color = "mmt", title = site) +
    scale_color_discrete(labels = c("1", "2", "3", "4", "5")) +
    guides(color = guide_legend(override.aes = list(size = 3))) +
    theme_minimal()
  
  # Add the plot to the list
  plots[[site]] <- plot
}

# Arrange the plots with labels by site 
grid.arrange(grobs = plots, nrow = 2, ncol = 2, top = "Soil Moisture by Local Site")

