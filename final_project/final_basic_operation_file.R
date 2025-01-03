# Load necessary libraries
install.packages("plot3D")


library(tidyverse)  # for data manipulation and visualization

# Load the dataset
data <- read.csv("C:\\Users\\HP\\Downloads\\Updated_Crop_recom_with_location.csv")

# View the first and last few rows
head(data)
tail(data)

# Summary statistics
summary(data)

# Structure of the dataset
str(data)

# Check for missing values
colSums(is.na(data))

#Data Type Conversions
#Ensure columns are in the correct format. For instance, label and Location should be factors:
data$label <- as.factor(data$label)
data$Location <- as.factor(data$Location)

#Handle Outliers
#Outliers can distort analysis. Use boxplots to visualize them:


boxplot(data$N, main="Nutrient N")
boxplot(data$P, main="Nutrient P")

#feature engeenering 

#Categorizing Temperature
#Temperature values might influence crops differently based on the range they fall into. Let’s categorize temperature into "Low", "Moderate", and "High" ranges.


data$temperature_category <- cut(
  data$temperature,
  breaks = c(-Inf, 15, 25, Inf),
  labels = c("Low", "Moderate", "High")
)

#Categorizing Rainfall
#Similarly, categorizing rainfall can help distinguish between dry, moderate, and high rainfall areas.


data$rainfall_category <- cut(
  data$rainfall,
  breaks = c(-Inf, 100, 200, Inf),
  labels = c("Low", "Moderate", "High")
)

#PH Level Categories
#Soil pH affects the type of crops that grow well in an area. We can classify ph as "Acidic", "Neutral", and "Alkaline".


data$ph_category <- cut(
  data$ph,
  breaks = c(-Inf, 5.5, 7.5, Inf),
  labels = c("Acidic", "Neutral", "Alkaline")
)

#Creating Interaction Features
#Interaction terms can reveal hidden relationships between variables. For instance, the combined effect of temperature and humidity might influence crop growth.

data$temperature_humidity_interaction <- data$temperature * data$humidity

#Another interaction term could be between N, P, and K, as these nutrients jointly influence soil fertility.


data$N_P_K_interaction <- data$N * data$P * data$K

#Location-Based Features
#Since crop suitability varies by region, it may be useful to add region-based binary (dummy) variables to represent each location. This technique is called one-hot encoding.


data <- data %>% mutate(across(Location, as.factor))
data <- data %>% mutate(across(Location, ~ as.numeric(. == Location)))

#Relative Humidity Categorization
#Relative humidity levels can be classified into categories, as different levels might be beneficial for specific crops.


data$humidity_category <- cut(
  data$humidity,
  breaks = c(-Inf, 40, 70, Inf),
  labels = c("Low", "Moderate", "High")
)
# Aggregate Statistics by Crop Type
#Calculate statistics such as average temperature, rainfall, and ph per crop type, and then add these as new features in your dataset. This could help capture the typical environmental conditions under which each crop grows best.


crop_averages <- data %>%
  group_by(label) %>%
  summarise(avg_temperature = mean(temperature),
            avg_rainfall = mean(rainfall),
            avg_ph = mean(ph))

# Join these average values back to the original data
data <- data %>%
  left_join(crop_averages, by = "label")

#Temporal Features 
#If there’s any temporal data (e.g., seasonality for crop yield), create features based on time, such as month or season. You’d likely have separate date columns, but you could create these if you had a "season" column:

data$season <- factor(ifelse(
  data$temperature >= 30, "Summer",
  ifelse(data$temperature <= 15, "Winter", "Monsoon")
))

#Soil Nutrient Ratios
#The balance between nutrients N, P, and K can influence soil health. Ratios like N/P, P/K, and N/K can be used to better understand nutrient composition.

data$N_to_P_ratio <- data$N / (data$P + 1)
data$P_to_K_ratio <- data$P / (data$K + 1)
data$N_to_K_ratio <- data$N / (data$K + 1)

#Clustering for Similar Environments 
#If your dataset includes distinct environmental conditions, you can use clustering techniques like K-means to create "environment clusters." Each observation is then assigned a cluster label representing similar environmental conditions.

library(cluster)

# Selecting relevant variables for clustering
clustering_data <- data %>% select(N, P, K, temperature, humidity, ph, rainfall)

# Apply K-means clustering
set.seed(123)
kmeans_result <- kmeans(clustering_data, centers = 3)
data$environment_cluster <- kmeans_result$cluster

#Log Transformations for Highly Skewed Variables
#If variables like rainfall have a long tail, consider log-transforming them to reduce skewness. This transformation can be helpful for models sensitive to normally distributed data.

data$log_rainfall <- log1p(data$rainfall)  # log1p handles log(0) cases by adding 1

#Label Encoding for Crop Types
#For machine learning, you may need to convert label (crop type) into numeric form. Label encoding assigns a unique number to each crop type.


data$label_encoded <- as.numeric(factor(data$label))

#Stastical analysis
#Descriptive Statistics
# Mean and Median
mean(data$temperature)
median(data$temperature)

# Mode function
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
get_mode(data$temperature)

# Standard deviation
sd(data$temperature)

#Correlation Analysis

cor(data %>% select_if(is.numeric), use="complete.obs")

#Basic Visualizations
#Bar Plot (e.g., Crop counts by Location):


ggplot(data, aes(x=Location, fill=label)) + 
  geom_bar() + 
  theme(axis.text.x = element_text(angle=90, hjust=1)) +
  labs(title="Crop Distribution by Location", x="Location", y="Count")


#Scatter Plot (e.g., Temperature vs. Humidity):

ggplot(data, aes(x=temperature, y=humidity, color=label)) +
  geom_point() +
  labs(title="Temperature vs Humidity by Crop Type")


#Line Plot (e.g., Rainfall over Observations):


ggplot(data, aes(x=1:nrow(data), y=rainfall, color=label)) +
  geom_line() +
  labs(title="Rainfall Across Observations", x="Observations", y="Rainfall")

#Advanced Visualizations
#Heatmap (e.g., Correlation between Variables):


# Take a random sample of 200 rows
set.seed(50)# Setting seed for reproducibility
sample_data <- data %>% sample_n(50)

corr <- cor(sample_data %>% select_if(is.numeric))
# Load ggcorrplot library
library(ggcorrplot)

# Create the correlation plot
ggcorrplot(corr, lab = TRUE, lab_size = 3, tl.cex = 10)


#Treemap (e.g., Crop Distribution):


library(treemap)
treemap(data, index="label", vSize="N", vColor="Location", title="Treemap of Crop Distribution")


#Interactive Visualizations

library(plotly)
p <- ggplot(data, aes(x=temperature, y=humidity, color=label)) + geom_point()
ggplotly(p)


#Machine learning Model

# Install and load caret and randomForest
if(!require(caret)) install.packages("caret")
if(!require(randomForest)) install.packages("randomForest")

library(caret)
library(randomForest)
# Split the data
set.seed(123)
trainIndex <- createDataPartition(data$label, p = 0.8, list = FALSE)
dataTrain <- data[trainIndex, ]
dataTest <- data[-trainIndex, ]

# Set up cross-validation
train_control <- trainControl(method = "cv", number = 5)

# Train the random forest model
model <- train(
  label ~ ., 
  data = dataTrain, 
  method = "rf", 
  trControl = train_control
)

# Print model summary
print(model)

# Check variable importance
varImpPlot(model$finalModel)

# Make predictions on test data
predictions <- predict(model, dataTest)

# Calculate accuracy
confusionMatrix(predictions, dataTest$label)


#3d visualization

# Load the dataset
data <- read.csv("C:\\Users\\HP\\Desktop\\crop_recom_project\\Updated_Crop_recom_with_location.csv")  # Replace with your actual file path

# View the first few rows to check the structure
head(data)


library(scatterplot3d)

# Create a 3D scatter plot
scatterplot3d(
  data$N,
  data$P,
  data$K,
  pch = 19,
  color = "blue",
  xlab = "N",
  ylab = "P",
  zlab = "K",
  main = "3D Scatter Plot"
)


library(rgl)

# Create a 3D scatter plot using plot3d
plot3d(
  data$N,
  data$P,
  data$K,
  col = "red",
  size = 5,
  type = "s",
  xlab = "N",
  ylab = "P",
  zlab = "K",
  main = "3D Scatter Plot"
)


library(plot3D)

# Create sequences for N and P
N_seq <- seq(from = 0, to = 140, by = 5)
P_seq <- seq(from = 5, to = 145, by = 5)

# Create a grid for N and P
N_matrix <- matrix(
  rep(N_seq, each = length(P_seq)),
  nrow = length(P_seq),
  ncol = length(N_seq),
  byrow = TRUE
)
P_matrix <- matrix(rep(P_seq, length(N_seq)),
                   nrow = length(P_seq),
                   ncol = length(N_seq))

# Create Z using an example function (N + P in this case)
Z <- outer(N_seq, P_seq, function(n, p) n + p)

# Check the dimensions of Z
dim(Z)  # Should return 30 x 30

# Plot the 3D surface
surf3D(
  x = N_matrix,
  y = P_matrix,
  z = Z,
  colkey = TRUE,
  col = terrain.colors(30),
  xlab = "N",
  ylab = "P",
  zlab = "K",
  main = "3D Surface Plot"
)


# Load plotly
library(plotly)

# Assuming 'data' is your dataset
# Replace this with your actual data if needed
data <- data.frame(
  N = sample(0:140, 30, replace = TRUE),   # Sample data for N
  P = sample(5:145, 30, replace = TRUE),   # Sample data for P
  K = sample(1:100, 30, replace = TRUE)    # Sample data for K
)

# Create the 3D Mesh Plot using plotly
mesh3d_plot <- plot_ly(data, 
                       x = ~N,  # Replace 'N' with the desired column for x-axis
                       y = ~P,  # Replace 'P' with the desired column for y-axis
                       z = ~K,  # Replace 'K' with the desired column for z-axis
                       type = 'mesh3d', 
                       intensity = ~K,  # Use 'K' for intensity coloring
                       colorscale = 'Jet', opacity = 0.6) %>%
  layout(scene = list(
    xaxis = list(title = 'N'),
    yaxis = list(title = 'P'),
    zaxis = list(title = 'K')
  ),
  title = "3D Mesh Plot")

# Display the plot
mesh3d_plot


#pair_plot
library(GGally)
ggpairs(data[, c("N", "P", "K")], title = "Pair Plot")


# Box plot of 'N' by 'P'
boxplot(
  data$N ~ data$P,
  col = rainbow(length(unique(data$P))),
  main = "Box Plot of N by P",
  xlab = "P (Parameter)",
  ylab = "N (Another Parameter)"
)

# Histogram of 'K'
hist(
  data$K,
  col = "skyblue",
  main = "Histogram of K",
  xlab = "K",
  ylab = "Frequency"
)

