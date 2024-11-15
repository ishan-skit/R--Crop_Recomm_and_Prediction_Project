# Load necessary libraries
install.packages("tidyverse")


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

#rshiny
# Install packages if they are not already installed
if (!require(shiny)) install.packages("shiny")
if (!require(caret)) install.packages("caret")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(randomForest)) install.packages("randomForest")
if (!require(dplyr)) install.packages("dplyr")

library(shiny)
library(caret)
library(ggplot2)
library(randomForest)
library(dplyr)

# Define the User Interface
ui <- fluidPage(
  
  # CSS for enhanced colorful styling
  tags$style(HTML("
    body {
      background: linear-gradient(to right, #2c3e50, #4ca1af);
      font-family: Arial, sans-serif;
      color: #34495e;
    }
    .title {
      text-align: center;
      font-size: 32px;
      font-weight: bold;
      color: #ecf0f1;
      margin-bottom: 20px;
      text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.5);
    }
    .sidebar {
      background: #f1c40f;
      padding: 20px;
      border-radius: 10px;
      box-shadow: 3px 3px 6px rgba(0, 0, 0, 0.2);
      color: #2c3e50;
    }
    .sidebar h4 {
      font-size: 20px;
      color: #2c3e50;
    }
    .btn-custom {
      background: #e74c3c;
      color: white;
      border: none;
      font-weight: bold;
      transition: background 0.3s ease;
    }
    .btn-custom:hover {
      background: #c0392b;
      cursor: pointer;
    }
    .main-panel {
      padding: 15px;
      background: #ecf0f1;
      border-radius: 10px;
      box-shadow: 3px 3px 6px rgba(0, 0, 0, 0.2);
    }
    .tab-content {
      margin-top: 20px;
      color: #34495e;
    }
    .tabsetPanel .nav-tabs>li>a {
      color: #2980b9;
      font-weight: bold;
    }
    .tabsetPanel .nav-tabs>li>a:hover {
      color: #1abc9c;
    }
  ")),
  
  # Page title
  div(class = "title", "Crop Recommendation Analysis and Prediction"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    sidebarPanel(
      class = "sidebar",
      h4("Exploratory Data Analysis"),
      
      # Exploratory Data Analysis (EDA) options
      selectInput("var1", "Select Variable for X-axis", choices = NULL),
      selectInput("var2", "Select Variable for Y-axis", choices = NULL),
      selectInput("plot_type", "Select Plot Type", choices = c("Scatter Plot" = "scatter", "Box Plot" = "box")),
      actionButton("plot", "Generate Plot", class = "btn-custom"),
      
      hr(),
      
      # Modeling options
      h4("Crop Prediction Model"),
      selectInput("target", "Select Target Variable", choices = c("label")),
      actionButton("train_model", "Train Random Forest Model", class = "btn-custom"),
      verbatimTextOutput("model_summary")
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      class = "main-panel",
      tabsetPanel(
        id = "tabsetPanel",
        tabPanel("Data Overview", tableOutput("data")),
        tabPanel("Summary Statistics", verbatimTextOutput("summary")),
        tabPanel("Visualization", plotOutput("plot")),
        tabPanel("Model Results", verbatimTextOutput("model_results"))
      )
    )
  )
)

# Define the Server Logic
server <- function(input, output, session) {
  
  # Load the dataset from the specified path
  data <- reactive({
    read.csv("C:\\Users\\HP\\Downloads\\Updated_Crop_recom_with_location.csv", stringsAsFactors = TRUE)
  })
  
  # Update variable choices based on loaded data
  observeEvent(data(), {
    updateSelectInput(session, "var1", choices = names(data()))
    updateSelectInput(session, "var2", choices = names(data()))
    updateSelectInput(session, "target", choices = "label")
  })
  
  # Display data overview
  output$data <- renderTable({
    head(data(), 10)
  })
  
  # Display summary statistics
  output$summary <- renderPrint({
    summary(data())
  })
  
  # Render Plot based on user inputs
  output$plot <- renderPlot({
    req(input$var1, input$var2, input$plot_type, data())
    if (input$plot_type == "scatter") {
      ggplot(data(), aes_string(x = input$var1, y = input$var2, color = "label")) +
        geom_point(size = 3, alpha = 0.7) + 
        theme_minimal() + 
        labs(title = "Scatter Plot")
    } else if (input$plot_type == "box") {
      ggplot(data(), aes_string(x = input$var1, y = input$var2)) +
        geom_boxplot(fill = "#3498db", alpha = 0.7) + 
        theme_minimal() + 
        labs(title = "Box Plot")
    }
  })
  
  # Train and Display Model
  model <- reactiveVal()
  
  observeEvent(input$train_model, {
    req(input$target, data())
    train_control <- trainControl(method = "cv", number = 5)
    model_rf <- train(label ~ ., data = data() %>% select(-Location),
                      method = "rf",
                      trControl = train_control)
    model(model_rf)
    output$model_summary <- renderPrint({
      model_rf
    })
  })
  
  # Display Model Results
  output$model_results <- renderPrint({
    req(model())
    model()$results
  })
}

# Run the Shiny App
shinyApp(ui = ui, server = server)
