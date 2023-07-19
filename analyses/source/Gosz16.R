#Data from Gosz16
#Time Domain Reflectometry at the Sevilleta National Wildlife Refuge, New Mexico (1996-2005)

#Soil moisture continuous

#get libraries needed
library(ggplot2)
library(gridExtra)
library(stringr)
library(tidyverse)
library(lubridate)
library(dplyr)
library(tidyr)
library(patchwork)
library(cowplot)

#set working directory
rm(list=ls(all=TRUE))
path <- 'C:/Users/julie/Documents/Soil Moisture/Gosz16'

setwd(path)
getwd()

###### make a dataframe
#get the downloaded data - it is in txt format - write as a data frame, split each variable into its own column

dat <- readLines("sev078_tdr_12092011.txt")
dat = as.data.frame(do.call(rbind, strsplit(dat, split=" {2,10}")), stringsAsFactors=FALSE)
names <- c("Station_ID", "Year", "Jul_Day", "Hour", "w1_30", "w1_5", "w1_10", 
           "w1_20", "w1_40", "w2_30", "w2_5", "w2_10", "w2_20", "w2_40", "w3_30", "w3_5", "w3_10", "w3_20", "w3_40")
dat_sep <- dat %>%
  separate(V1, into = paste0(names), sep = ",", remove = TRUE)

#remove the first row (repeated column names)
dat_sep <- dat_sep[-1, ]

#make everything numeric
#dat_sep <- dat_sep %>%
#  mutate_all(as.numeric)   # this line of code caused the date conversion to fail 


#check on all of the variables
summary(dat_sep)


#### Convert Julian date and hour to POSIXct
dat_sep$Jul_Day <- as.numeric(dat_sep$Jul_Day)
dat_sep$Hour <- as.numeric(dat_sep$Hour)
dat_sep$Year <- as.numeric(dat_sep$Year)

dat_sep$Month <- NA  # Create an empty column to store the month
dat_sep$Day <- NA    # Create an empty column to store the day


# Convert Julian date and year to date object
dat_sep$date <- as.Date(paste0(dat_sep$Year, formatC(dat_sep$Jul_Day, width = 3, flag = "0")), format = "%Y%j")

# Extract month and day from the date
dat_sep$Month <- format(dat_sep$date, "%m")
dat_sep$Day <- format(dat_sep$date, "%d")

#now convert data + hour to POSIXct
dat_sep$datetime <- as.POSIXct(paste(dat_sep$date, formatC(dat_sep$Hour, width = 2, flag = "0"), sep = " "), format = "%Y-%m-%d %H")



#convert -999 values into NAs (from dataset, -999 was used as a placeholder for NA)
dat_sep <- dat_sep %>%
  mutate_all(~replace(., . == -999, NA))


# check the number of stations/sites 
# unique(dat_sep$Station_ID)



### Experiment with subsetting the df

# Subset dataframe for w1
df_w1 <- dat_sep[, grepl("w1|datetime|Year|Station_ID", names(dat_sep))]

# Subset dataframe for w2
df_w2 <- dat_sep[, grepl("w2|datetime|Year|Station_ID", names(dat_sep))]

# Subset dataframe for w3
df_w3 <- dat_sep[, grepl("w3|datetime|Year|Station_ID", names(dat_sep))]


### Create plots of all stations per year
# The for loop cycles through the data 

#set up a for loop that calls the plotdata function so as to create a new plot for each year

plotData <- function(d,y){ #d = subsetted data for that year, y = year #
  
  # Convert datetime to POSIXct format
  d$datetime <- as.POSIXct(d$datetime)
  
  ## make all the values be in one column (long data > wide data)
  long_data <- gather(d, depth, value, -datetime, - Year, -Station_ID)
  long_data$value <- as.numeric(long_data$value)
  long_data$Year <- as.numeric(long_data$Year)
  
  
  # Create an empty list to store the plots
  plots <- list()
  
  # Iterate through the Station IDs 
  for (site in unique(long_data$Station_ID)) {
    if (site != 1) {
    
    # Subset the data for the current year
      subset_data <- subset(long_data, Station_ID == site)
      
      # Check if the subset data has enough date range
      if (length(subset_data$datetime) < 2) {
        warning(paste("Insufficient data for Station", site, ". Skipping plot generation."))
        next  # Skip to the next iteration
      }
      
     # Create the plot for the current year
      plot <- ggplot(subset_data, aes(x = datetime, y = value, color = factor(depth))) +
        geom_line(linewidth = 0.8) +
        labs(x = "time", y = "Soil Moisture", color = "depth (cm)", title = paste("Station", "", site)) +
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
  combined_plots <- combined_plots + plot_annotation(title = paste("Soil Moisture in SEV LTER (Plot C) during", y,"by Station ID"),
                                                     theme = theme(plot.title = element_text(hjust = 0.5)))
  
  
  # Save combined_plots as PNG
  ggsave(paste("plots_w3", y,".png"), plot = combined_plots, width = 10, height = 8, dpi = 300)
  
  }}


# create a for loop to plot each year seperately using the plotting function 
for (year in unique(df_w3$Year)){
  
  subset_year <- subset(df_w3, Year == year) 
  
  plotData(subset_year, year) #pass parameters subset_year and year into the plotData function created above 
  
}


### Create plots of just Station 40 showing each year 
# subset df_w1

w1_40 <- df_w1[which(df_w1$Station_ID == "40"),]


plotStation40 <- function(d){ #d = subsetted data for that year, y = year #
  
  # Convert datetime to POSIXct format
  d$datetime <- as.POSIXct(d$datetime)
  
  ## make all the values be in one column (long data > wide data)
  long_data <- gather(d, depth, value, -datetime, - Year, -Station_ID)
  long_data$value <- as.numeric(long_data$value)
  long_data$Year <- as.numeric(long_data$Year)
  
  #View(long_data)
  
  # Create an empty list to store the plots
  plots <- list()
  
  # Iterate through the Station IDs 
  for (year in unique(long_data$Year)) {
    
      # Subset the data for the current year
      subset_data <- subset(long_data, Year == year)
      
      # Check if the subset data has enough date range
      # Check if the subset data has enough data points
      if (nrow(subset_data) < 2) {
        warning(paste("Insufficient data in year", year, ". Skipping plot generation."))
        next  # Skip to the next iteration
      }
      
      # Create the plot for the current year
      plot <- ggplot(subset_data, aes(x = datetime, y = value, color = factor(depth))) +
        geom_point(size = 0.2, na.rm = TRUE) +
        labs(x = "time", y = "Soil Moisture", color = "depth (cm)", title = paste(year)) +
        scale_color_discrete(labels = c("30", "5", "10", "20", "40")) +
        guides(color = guide_legend(override.aes = list(size = 3))) +
        theme_minimal() + 
        scale_x_datetime(date_labels = "%b") + 
        scale_y_continuous(limits = c(0.0, 0.3))
      
      # Add the plot to the list
      plots[[as.character(year)]] <- plot
      
    }
  
    # Combine the plots using patchwork and add a common legend
    #combined_plots <- plot_grid(plotlist = plots, nrow = 2, ncol = 5)  
    combined_plots <- wrap_plots(plots, nrow = 3, ncol = 4)
    combined_plots <- combined_plots + plot_layout(guides = 'collect')
    
    # Add a title to the combined plots
    combined_plots <- combined_plots + plot_annotation(title = paste("Soil Moisture in SEV LTER (Pit A) Station 40"),
                                                       theme = theme(plot.title = element_text(hjust = 0.5)))
    print(combined_plots)
    
    # Save combined_plots as PNG
    ggsave(paste("plots_w1byYear.png"), plot = combined_plots, width = 12, height = 8, dpi = 300)
    
  }

plotStation40(w1_40)


### make a plot of the data meaned across the depths

## make all the values be in one column (long data > wide data)
w1_40 <- gather(w1_40, depth, value, -datetime, - Year, -Station_ID)
w1_40$value <- as.numeric(w1_40$value)
w1_40$Year <- as.numeric(w1_40$Year)

split_df <- w1_40 %>%
  separate(depth, into = c("w1", "depth"), sep = "_", convert = TRUE)

split_df$value[which(split_df$value == "NaN")] <- NA

mean_df <- split_df %>%
  group_by(datetime, Year, Station_ID) %>%
  summarize(mean_value = mean(value, na.rm = TRUE))

plotMean <- function(d){ 
  
  # Convert datetime to POSIXct format
  d$datetime <- as.POSIXct(d$datetime)

  # Create an empty list to store the plots
  plots <- list()
  
  # Iterate through the Station IDs 
  for (year in unique(d$Year)) {
    
    # Subset the data for the current year
    subset_data <- subset(d, Year == year)
    
    # Check if the subset data has enough date range
    # Check if the subset data has enough data points
    if (nrow(subset_data) < 2) {
      warning(paste("Insufficient data in year", year, ". Skipping plot generation."))
      next  # Skip to the next iteration
    }
    
    # Create the plot for the current year
    plot <- ggplot(subset_data, aes(x = datetime, y = mean_value)) +
      geom_point(size = 0.2, na.rm = TRUE, color =  "#3366CC") +
      labs(x = "time", y = "Soil Moisture", title = paste(year)) +
      guides(color = guide_legend(override.aes = list(size = 3))) +
      theme_minimal() + 
      scale_x_datetime(date_labels = "%b") + 
      scale_y_continuous(limits = c(0.0, 0.3))
    
    # Add the plot to the list
    plots[[as.character(year)]] <- plot
    
  }
  
  # Combine the plots using patchwork and add a common legend
  #combined_plots <- plot_grid(plotlist = plots, nrow = 2, ncol = 5)  
  combined_plots <- wrap_plots(plots, nrow = 3, ncol = 4)
  combined_plots <- combined_plots + plot_layout(guides = 'collect')
  
  # Add a title to the combined plots
  combined_plots <- combined_plots + plot_annotation(title = paste("Mean Soil Moisture in SEV LTER (Pit A) Station 40"),
                                                     theme = theme(plot.title = element_text(hjust = 0.5)))
 
  # Save combined_plots as PNG
  ggsave(paste("MeanPlots_w1byYear.png"), plot = combined_plots, width = 12, height = 8, dpi = 300)
  
  
  
}

plotMean(mean_df)
#stacked_df <- dat_sep %>%
#  gather(key, value, starts_with("w")) %>%
#  separate(key, into = c("source", "depth"), sep = "_") %>%
#  mutate(depth = as.numeric(depth)) %>%
#  arrange(source, depth)
