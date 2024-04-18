#Importing the data 
csv_file=read.csv("weatherHistory.csv")
#printing top 5 data 
head(csv_file,100)
tail(csv_file)
#printg all the information about our dataset 
print(str(csv_file))
#Finding the null values 
print(colSums(is.na(csv_file)))
#Finding the duplicate vlaues from the dataset 
print(sum(duplicated(csv_file)))
#removing the duplicate values from the dataset 
csv_file=unique(csv_file)
#Printing the stastical summary 
summary(csv_file)
install.packages("psych")
library("psych")+

#removing unwanted vlaues form a formatted.date Modyfi 
csv_file$Formatted.Date = substr(csv_file$Formatted.Date, start = 1, stop = 11)
head(csv_file)
print(csv_file$Formatted.Date)
#changing the format of the date column 
csv_file$Formatted.Date=as.Date(csv_file$Formatted.Date,format="%Y-%m-%d")
head(csv_file)
#How does temperature vary over time?
library("ggplot2")+
# Plot humidity over time
ggplot(csv_file, aes(x =Formatted.Date )) +
  geom_line(aes(y = Humidity, color = "Humidity")) +
  labs(title = "Humidity over time ",x = "Time", y = "Humidity", color = "Variable") +
  scale_color_manual(values = c("Humidity" = "skyblue")) 
#-------------------------------------------------------------------------------
# Plot temperature over time
ggplot(csv_file, aes(x =Formatted.Date )) +
  geom_line(aes(y = Temperature..C., color = "Humidity")) +
  labs(title="Temperature over time",x = "Time", y = "Temperature", color = "Variable") +
  scale_color_manual(values = c("Humidity" = "orange")) 

#-----------------------------------------------
#Importing 
library(dplyr)+

#--------------------------------------------------------------------------------
#Temperature distribution 
#What is the distribution of temperature and apparent temperature?

# Create a histogram for Temperature
ggplot(csv_file, aes(x = Temperature..C.)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(x = "Temperature", y = "Frequency", title = "Distribution of Temperature")

# Create a histogram for Apparent_Temperature
ggplot(csv_file, aes(x = Apparent.Temperature..C.)) +
  geom_histogram(binwidth = 1, fill = "lightgreen", color = "black") +
  labs(x = "Apparent Temperature", y = "Frequency", title = "Distribution of Apparent Temperature")

#Are there any outliers in temperature or apparent temperature?
# Create a boxplot for the Temperature column
boxplot(csv_file$Temperature..C., main = "Boxplot of Temperature",col="skyblue")

# Identify outliers using boxplot statistics
outliers = boxplot(csv_file$Temperature..C., plot = FALSE)$out
print(outliers)
#------------------------------------------------------------------------
#creating a New column where we will store only month & year

csv_file$month_year = format(csv_file$Formatted.Date, "%Y-%m")
head(csv_file)
#creating a New column where we will store only Year
csv_file$year = format(csv_file$Formatted.Date, "%Y")
head(csv_file)
# Added season column 
library("dplyr")+
install.packages("lubridate")
library(lubridate)
csv_file$Season =case_when(
  month(csv_file$Formatted.Date) %in% c(12, 01, 02) ~ "Winter",
  month(csv_file$Formatted.Date) %in% c(03, 04, 05) ~ "Spring",
  month(csv_file$Formatted.Date) %in% c(06, 07, 08) ~ "Summer",
  month(csv_file$Formatted.Date) %in% c(09, 10, 11) ~ "Fall",
  TRUE ~ NA_character_
)
head(csv_file)
#groping season and year by their temperature using Groupby function
result1 = csv_file %>%
  group_by(Season,year) %>%
  summarize(.groups = "keep",Mean_Temperature = mean(Temperature..C., na.rm = TRUE,))
head(result1) 
tail(result1)

#Are there any seasonal trends in temperature or weather conditions?
#creating barplot to find seasonal trends with 
ggplot(result1, aes(x = factor(year), y = Mean_Temperature, fill = Season)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Season-wise Temperature", x = "Year", y = "Mean Temperature") +
  scale_fill_manual(values = c("Winter" = "blue", "Spring" = "green", "Summer" = "orange", "Fall" = "red")) +
  theme_minimal()
#How do temperature and apparent temperature vary with other variables like humidity or wind speed?
# Load necessary libraries
library(ggplot2)


# Scatter plot: Temperature vs. Humidity
ggplot(csv_file, aes(x = Humidity, y =Temperature..C. )) +
  geom_point(color="skyblue",size=2) + 
  geom_smooth(method = "lm",se=FALSE,color="black")
  labs(x = "Humidity", y = "Temperature", title = "Temperature vs. Humidity")

# Scatter plot: Apparent Temperature vs. Humidity
ggplot(data, aes(x = Humidity, y = Apparent_Temperature)) +
  geom_point() +
  labs(x = "Humidity", y = "Apparent Temperature", title = "Apparent Temperature vs. Humidity")

# Scatter plot: Temperature vs. Wind Speed
ggplot(csv_file, aes(x = Wind.Speed..km.h., y = Temperature..C.)) +
  geom_point(color="orange",size=0.5) +
  geom_smooth(method = "lm",se=FALSE,color="black")
  labs(x = "Wind Speed", y = "Temperature", title = "Temperature vs. Wind Speed")
#What are the different types of precipitation recorded in the dataset?
precipitation=unique(csv_file$Precip.Type)
print(precipitation)
#How often does each type of precipitation occur?
#Grouping the data according to Precip type
grp=csv_file%>%
  group_by(Precip.Type)%>%
  summarize(total_num=n(),)
print(grp)
#visualizing the precip type occurrences 
ggplot(grp,aes(x=Precip.Type,y=total_num))+
  geom_bar(stat="identity",fill="skyblue",color="black")+
  labs(title="Occurrences of each precip type",x="Precip type",y="Occurrences")

#Is there any correlation between precipitation type and other weather variables?
#encoding the precipty to find correlation between other weather varibale 
csv_file$Encoded_precip=as.numeric(factor(csv_file$Precip.Type))
head(csv_file)
csv_file$Encoded_summary=as.numeric(factor(csv_file$Summary))
csv_file$Encoded_daily=as.numeric(factor(csv_file$Daily.Summary))
head(csv_file)
subset_cr=csv_file[,c("Encoded_precip","Encoded_summary","Encoded_daily")]
Cr1=cor(subset_cr)
Cr1
#visualising the heat map for 

# Assuming correlation_matrix is your correlation matrix
# Generate heatmap
heatmap(Cr1, 
        col = colorRampPalette(c("blue", "white", "red"))(100), 
        symm = TRUE, 
        margins = c(5, 10))
ggplot(data=Cr1,aes(x=))

#What are the most common daily summaries recorded in the dataset?
grp_common=csv_file%>%
  group_by(Daily.Summary)%>%
  summarize(Total_summary=n())

sorted_data <- grp_common[order(-grp_common$Total_summary), ]
print(sorted_data)
