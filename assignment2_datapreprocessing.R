library(ggplot2)
library(tidyverse)
data <- read.csv(file.choose())
head(data)
colnames(data)
View(data)
## data cleaning ###
######### remove variables with >90% NA values #########
# Step 1: Calculate the proportion of missing values for each column
missing_prop <- colMeans(is.na(data))

# Step 2: Identify columns with >90% missing values
columns_to_remove <- names(data)[missing_prop > 0.9]

# Step 3: Remove the identified columns from the data frame
data <- data[, !names(data) %in% columns_to_remove]
colnames(data)

########### check for null values and handle them#########
# Calculate the count of null values in each column
null_count <- colSums(is.na(data))

# Display the null value counts
print(null_count)
dim(data)
# Remove rows with missing values
data <- na.omit(data)
########## rename variables to suitable format #########
colnames(data)<-c("Crash_ID","Crash_Date","Crash_Time","Severity","Crash_Type","Pedestrians_Injured","Pedestrian_loss","Reported_location",
                  "Location")
colnames(data)
View(data)
########## check and change all date to same type ########## 
data$Crash_Date <- format(data$Crash_Date, format = "%d-%m-%Y")
head(data$Crash_Date)
################# change time variable to a suitable format ##########
data$Crash_Time <- format(data$Crash_Time, format = "%H:%M:%S")
head(data$Crash_Time)
write.csv(data, file = "cleaned_data.csv", row.names = FALSE)
getwd()
######## create new var's - latitude and longutude ##########
data$Latitude <- as.numeric(str_extract_all(data$Location, "-?\\d+\\.\\d+") %>% sapply(`[`, 1))
data$Longitude <- as.numeric(str_extract_all(data$Location, "-?\\d+\\.\\d+") %>% sapply(`[`, 2))
View(data)

####### split the date month year ################
# Split the "Date" column into day, month, and year components
date_components <- str_split(data$Crash_Date, "-")

# Create new columns for day, month, and year
data$Day <- as.integer(sapply(date_components, function(x) x[1]))
data$Month <- as.integer(sapply(date_components, function(x) x[2]))
data$Year <- as.integer(sapply(date_components, function(x) x[3]))
View(data)
######## convert all to upper from reported_location ########
data$Reported_location <- toupper(data$Reported_location)
########################### visualisation ##############################################
# data is from 2012 to 2021 #######
####################### top 3 suburbs ##################
attach(data)
library(dplyr)

crash_counts <- data %>%
  group_by(Reported_location, Year) %>%
  summarise(crash_count = n()) %>%
  ungroup()

# Summarize the total crashes for each suburb across all years
suburb_totals <- crash_counts %>%
  group_by(Reported_location) %>%
  summarise(total_crashes = sum(crash_count)) %>%
  arrange(desc(total_crashes))

# Get the top 3 suburbs with the most crashes
top_3_suburbs <- suburb_totals %>%
  head(3)

# Print the top 3 suburbs
print(top_3_suburbs)

filtered_crash_counts <- crash_counts %>%
  filter(Reported_location %in% top_3_suburbs$Reported_location)
filtered_crash_counts
# Create a line plot
ggplot(filtered_crash_counts, aes(x = Year, y = crash_count, color = Reported_location)) +
  geom_line() +
  labs(
    title = "Crashes Over the Years",
    x = "Year",
    y = "Crash Count",
    color = "Suburb"
  ) +
  theme_minimal()


####################### crash time to find peak hours ##############
# Combine Pedestrians_Injured and Pedestrian_loss to get total crashes
data$Total_Crashes <- data$Pedestrians_Injured + data$Pedestrian_loss

# Create a bar plot to show total crashes at each hour
ggplot(data, aes(x = as.numeric(format(Crash_Time, "%H")), fill = factor(as.numeric(format(Crash_Time, "%H")))))+
  geom_bar() +
  labs(x = "Hour of the Day", y = "Total Crashes") +
  scale_x_continuous(breaks = 0:23) +
  ggtitle("Total Crashes by Hour of the Day") +
  scale_fill_discrete(name = "Hour") +
  theme_minimal()
library(plotly)

color_palette <- rainbow(24)
plot <- data %>%
  group_by(Hour = as.numeric(format(Crash_Time, "%H"))) %>%
  summarise(Total_Crashes = sum(Total_Crashes)) %>%
  plot_ly(x = ~Hour, y = ~Total_Crashes, type = 'bar',marker = list(color = color_palette)) %>%
  layout(
    xaxis = list(title = "Hour of the Day"),
    yaxis = list(title = "Total Crashes"),
    title = "Total Crashes by Hour of the Day"
  )

# Show the Plotly plot
plot

########################################################



















