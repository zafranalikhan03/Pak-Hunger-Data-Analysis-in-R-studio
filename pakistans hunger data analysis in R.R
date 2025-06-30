##############################################################
########################## Phase 1 ###########################
##############################################################

library(readxl)
pakistan_hunger_data_raw <- read_excel("D:/Files/6th Semester/DAR/data final project/pakistan_hunger_data_raw.xlsx")
View(pakistan_hunger_data_raw)

head(pakistan_hunger_data_raw)
str(pakistan_hunger_data_raw)
dim(pakistan_hunger_data_raw)
summary(pakistan_hunger_data_raw)



###############################################################
########################### Phase 3 ###########################
###############################################################

############# Step#1 Checking missing values #############

colSums(is.na(pakistan_hunger_data_raw))

############# Checking data types #############
str(pakistan_hunger_data_raw)

############## Basic summary to detect outliers or strange values #############

summary(pakistan_hunger_data_raw)

############## Quick view of data #############
head(pakistan_hunger_data_raw)


################### Step#2 - Mean imputation for missing values ###################

# For Population_Under_Poverty

pakistan_hunger_data_raw$Population_Under_Poverty[is.na(pakistan_hunger_data_raw$Population_Under_Poverty)] <-
  round(mean(pakistan_hunger_data_raw$Population_Under_Poverty, na.rm = TRUE), 2)


# For Access_to_Clean_Water
pakistan_hunger_data_raw$Access_to_Clean_Water[is.na(pakistan_hunger_data_raw$Access_to_Clean_Water)] <-
  round(mean(pakistan_hunger_data_raw$Access_to_Clean_Water, na.rm = TRUE), 2)

# For Food_Production_Index
pakistan_hunger_data_raw$Food_Production_Index[is.na(pakistan_hunger_data_raw$Food_Production_Index)] <-
  round(mean(pakistan_hunger_data_raw$Food_Production_Index, na.rm = TRUE), 2)

# Confirming no more missing values
colSums(is.na(pakistan_hunger_data_raw))



############ Step#3 -  Boxplots for each numeric column to detect outliers ###########
# Boxplots for each numeric column to detect outliers

numeric_cols <- pakistan_hunger_data_raw[, sapply(pakistan_hunger_data_raw, is.numeric)]
par(mfrow = c(2, 3))  

# set layout for multiple plots
for (col in names(numeric_cols)) {
  boxplot(numeric_cols[[col]], main = col, col = "lightblue")
}
par(mfrow = c(1, 1))  



# IQR-based detection function
detect_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR_val
  upper <- Q3 + 1.5 * IQR_val
  return(which(x < lower | x > upper))
}

# Applying to specific columns
outliers_pop_under_pov <- detect_outliers(pakistan_hunger_data_raw$Population_Under_Poverty)
outliers_pop_under_pov

outliers_Malnutrition_Rate <- detect_outliers(pakistan_hunger_data_raw$Malnutrition_Rate)
outliers_Malnutrition_Rate

outliers_Food_Insecurity <- detect_outliers(pakistan_hunger_data_raw$Food_Insecurity)
outliers_Food_Insecurity

outliers_Access_to_Clean_Water <- detect_outliers(pakistan_hunger_data_raw$Access_to_Clean_Water)
outliers_Access_to_Clean_Water

outliers_Food_Production_Index <- detect_outliers(pakistan_hunger_data_raw$Food_Production_Index)
outliers_Food_Production_Index

outliers_Children_Underweight <- detect_outliers(pakistan_hunger_data_raw$Children_Underweight)
outliers_Children_Underweight


############## Step#4 - Data Transformation  #################

#Creating new variable of hunger severity index

pakistan_hunger_data_raw$Hunger_Severity_Index <- rowMeans(
  pakistan_hunger_data_raw[, c("Malnutrition_Rate", "Food_Insecurity", "Children_Underweight")]
)
pakistan_hunger_data_raw$Hunger_Severity_Index


##########Scaling Columns ##########

pakistan_hunger_data_raw$Access_to_Clean_Water_Scaled <- scale(pakistan_hunger_data_raw$Access_to_Clean_Water)
pakistan_hunger_data_raw$Food_Production_Index_Scaled <- scale(pakistan_hunger_data_raw$Food_Production_Index)


########### Convert City and Year columns to factors (categorical data)
pakistan_hunger_data_raw$City <- as.factor(pakistan_hunger_data_raw$City)
pakistan_hunger_data_raw$Year <- as.factor(pakistan_hunger_data_raw$Year)

############ Saving the Transformed Datset #############

write.csv(pakistan_hunger_data_raw, "pakistan_hunger_data_cleaned.csv", row.names = FALSE)

pakistan_hunger_data_cleaned <- read_excel("D:/Files/6th Semester/DAR/data final project/pakistan_hunger_data_cleaned.xlsx")
View(pakistan_hunger_data_cleaned)




###############################################################
################ Phase 2 Descriptive Statistics ###############
###############################################################

# Load required packages

# For describe() function
install.packages("psych")
library(psych)

# For skewness() and kurtosis()
install.packages("e1071")
library(e1071)

# For skim() function
install.packages("skimr")
library(skimr)

# For data wrangling
library(dplyr)       


# View the first few rows
head(pakistan_hunger_data_cleaned)

# summary of Data
summary(pakistan_hunger_data_cleaned)

#Describe function for detailed statistics
describe(pakistan_hunger_data_cleaned)


skim(pakistan_hunger_data_cleaned)

############################################################
#################### Exploratory Data Analysis #############
############################################################
install.packages("ggplot2")
install.packages("tidyverse")
library(ggplot2)
library(tidyr)
install.packages("plotly")
install.packages("dplyr")
library(plotly)
library(dplyr)

#### Population under poverty Histogram #########

plot_ly(data = pakistan_hunger_data_cleaned, x = ~Population_Under_Poverty, type = "histogram",
        marker = list(color = "skyblue")) %>%
  layout(title = "Distribution of Population Under Poverty",
         xaxis = list(title = "Poverty (%)"),
         yaxis = list(title = "Count"))
######### Malnutrition Box Plot #####
plot_ly(data = pakistan_hunger_data_cleaned, y = ~Malnutrition_Rate, type = "box",
        boxpoints = "all", jitter = 0.3, pointpos = -1.8,
        marker = list(color = "orange")) %>%
  layout(title = "Boxplot of Malnutrition Rate",
         yaxis = list(title = "Malnutrition (%)"))

#### Denisty plot for food insecurity #########

# First calculate density
food_density <- density(pakistan_hunger_data_cleaned$Food_Insecurity, na.rm = TRUE)

# Create data frame for plotting
food_df <- data.frame(x = food_density$x, y = food_density$y)

# Plot
plot_ly(food_df, x = ~x, y = ~y, type = 'scatter', mode = 'lines',
        line = list(color = 'darkred')) %>%
  layout(title = "Density Plot of Food Insecurity",
         xaxis = list(title = "Food Insecurity (%)"),
         yaxis = list(title = "Density"))

#### Box plot for Access to clean water ########
plot_ly(data = pakistan_hunger_data_cleaned, y = ~Access_to_Clean_Water, type = "box",
        boxpoints = "all", jitter = 0.3,
        marker = list(color = "lightgreen")) %>%
  layout(title = "Boxplot of Access to Clean Water",
         yaxis = list(title = "Access (%)"))

##### Histogram food production index ######
plot_ly(data = pakistan_hunger_data_cleaned, x = ~Food_Production_Index, type = "histogram",
        marker = list(color = "purple")) %>%
  layout(title = "Distribution of Food Production Index",
         xaxis = list(title = "Food Production Index"),
         yaxis = list(title = "Frequency"))

######## Children Underweight #####
plot_ly(data = pakistan_hunger_data_cleaned, y = ~Children_Underweight, type = "box",
        boxpoints = "all", jitter = 0.3,
        marker = list(color = "salmon")) %>%
  layout(title = "Boxplot of Underweight Children",
         yaxis = list(title = "Underweight (%)"))


########### Hunger severity Index ############
hunger_density <- density(pakistan_hunger_data_cleaned$Hunger_Severity_Index, na.rm = TRUE)
hunger_df <- data.frame(x = hunger_density$x, y = hunger_density$y)

plot_ly(hunger_df, x = ~x, y = ~y, type = 'scatter', mode = 'lines',
        line = list(color = 'blue')) %>%
  layout(title = "Density Plot of Hunger Severity Index",
         xaxis = list(title = "Hunger Index"),
         yaxis = list(title = "Density"))

######## Categorical Variables Year and City #########

###City###
# Create bar chart for City
city_counts <- as.data.frame(table(pakistan_hunger_data_cleaned$City))
colnames(city_counts) <- c("City", "Count")

plot_ly(data = city_counts, x = ~City, y = ~Count, type = "bar",
        marker = list(color = "skyblue")) %>%
  layout(title = "Frequency of Records by City",
         xaxis = list(title = "City", tickangle = -45),
         yaxis = list(title = "Count"),
         margin = list(b = 120))

### year ###
year_counts <- as.data.frame(table(pakistan_hunger_data_cleaned$Year))
colnames(year_counts) <- c("Year", "Count")

plot_ly(year_counts, labels = ~Year, values = ~Count, type = "pie",
        textinfo = "label+percent", insidetextorientation = 'radial') %>%
  layout(title = "Distribution of Records by Year")
################### Bivariate Analysis ####################

#### scatter plots for numeric to numeric #####

plot_ly(
  data = pakistan_hunger_data_cleaned,
  x = ~Food_Production_Index,
  y = ~Children_Underweight,
  type = 'scatter',
  mode = 'markers',
  marker = list(color = 'orange', size = 10),
  text = ~paste("City:", City, "<br>Year:", Year)
) %>%
  layout(
    title = "Scatter Plot: Food Production Index vs Hunger Severity Index",
    xaxis = list(title = "Food Production Index"),
    yaxis = list(title = "Children Underweight")
  )


########### Numeric vs. Categorical ############

## Box plot Mulnutrition Rate across years ##
plot_ly(
  data = pakistan_hunger_data_cleaned,
  x = ~factor(Year),
  y = ~Malnutrition_Rate,
  type = "box",
  boxpoints = "all",
  jitter = 0.3,
  pointpos = -1.8,
  marker = list(color = 'red')
) %>%
  layout(
    title = "Box Plot: Malnutrition Rate Across Years",
    xaxis = list(title = "Year"),
    yaxis = list(title = "Malnutrition Rate (%)")
  )

############### Violin Plot Access to clean water by City ###############
plot_ly(
  data = pakistan_hunger_data_cleaned,
  x = ~City,
  y = ~Access_to_Clean_Water,
  type = "violin",
  box = list(visible = TRUE),
  meanline = list(visible = TRUE),
  line = list(color = "blue")
) %>%
  layout(
    title = "Violin Plot: Access to Clean Water by City",
    xaxis = list(title = "City"),
    yaxis = list(title = "Access to Clean Water (%)")
  )

## 4. Categorical vs Categorical – Stacked Bar Chart ##

#### City vs. Year stacked bar chart ####

library(dplyr)

city_year_count <- pakistan_hunger_data_cleaned %>%
  count(City, Year)

plot_ly(
  data = city_year_count,
  x = ~City,
  y = ~n,
  color = ~factor(Year),
  type = 'bar'
) %>%
  layout(
    barmode = 'stack',
    title = "Stacked Bar Chart: City vs Year",
    xaxis = list(title = "City"),
    yaxis = list(title = "Number of Records")
  )

## Categorical vs Categorical – Grouped Bar Chart ############
############ City vs. year ##############
plot_ly(
  data = city_year_count,
  x = ~City,
  y = ~n,
  color = ~factor(Year),
  type = 'bar'
) %>%
  layout(
    barmode = 'group',
    title = "Grouped Bar Chart: City vs Year",
    xaxis = list(title = "City"),
    yaxis = list(title = "Number of Records")
  )

#### Multivariate Analysis ####
##Visualization 1: Scatter Plot with Color Encoding ###

plot_ly(
  data = pakistan_hunger_data_cleaned,
  x = ~Food_Production_Index,
  y = ~Hunger_Severity_Index,
  type = 'scatter',
  mode = 'markers',
  color = ~factor(Year),
  marker = list(size = 10),
  text = ~paste("City:", City, "<br>Year:", Year)
) %>%
  layout(
    title = "Scatter Plot with Color Encoding: Food Production vs Hunger Severity across Years",
    xaxis = list(title = "Food Production Index"),
    yaxis = list(title = "Hunger Severity Index"),
    legend = list(title = list(text = "Year"))
  )

##Visualization 2: Bubble Plot (Size Encoding)
plot_ly(
  data = pakistan_hunger_data_cleaned,
  x = ~Population_Under_Poverty,
  y = ~Hunger_Severity_Index,
  size = ~Access_to_Clean_Water,
  type = 'scatter',
  mode = 'markers',
  marker = list(sizemode = 'diameter'),
  text = ~paste("City:", City, "<br>Water Access:", Access_to_Clean_Water)
) %>%
  layout(
    title = "Bubble Plot: Poverty vs Hunger, Bubble Size = Water Access",
    xaxis = list(title = "Population Under Poverty (%)"),
    yaxis = list(title = "Hunger Severity Index")
  )

##Visualization 3: Faceted Scatter Plot by City
library(ggplot2)
library(plotly)

# ggplot object with facet
p <- ggplot(pakistan_hunger_data_cleaned, aes(x = Food_Production_Index, y = Malnutrition_Rate)) +
  geom_point(color = 'darkgreen') +
  facet_wrap(~City) +
  labs(title = "Faceted Plot: Food Production vs Malnutrition Rate by City",
       x = "Food Production Index",
       y = "Malnutrition Rate (%)")
p
# Convert to interactive plotly object
ggplotly(p)


###################################################
########### Phase#5 Regression Analysis ###########
###################################################

regression_model <- lm(Hunger_Severity_Index ~ Malnutrition_Rate + Children_Underweight  , data = pakistan_hunger_data_cleaned)
summary(regression_model)

cor(pakistan_hunger_data_cleaned$Malnutrition_Rate, pakistan_hunger_data_cleaned$Children_Underweight)

### Diagnostic PLots ###
regression_model <- lm(Hunger_Severity_Index ~ Malnutrition_Rate + Children_Underweight, data = pakistan_hunger_data_cleaned)
par(mfrow = c(2, 2))
plot(regression_model)


