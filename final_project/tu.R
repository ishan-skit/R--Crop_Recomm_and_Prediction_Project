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
library(shiny)
library(caret)
library(ggplot2)
library(randomForest)
library(dplyr)

# Define the User Interface
ui <- fluidPage(
  # Custom CSS Styling remains the same
  tags$style(HTML("
    body {
      background: url('image1.jpg') no-repeat center center fixed;
      background-size: cover;
      font-family: 'Roboto', sans-serif;
      color: #ffffff;
      margin: 0;
    }
    .visualization-panel {
      color: black;
    }
    .summary-panel{
    color: black;
    font-weight: 800;
    }
    .title {
      text-align: center;
      font-size: 50px;
      font-weight: bold;
      color: #f39c12;
      margin-bottom: 20px;
      text-shadow: 4px 4px 8px rgba(0, 0, 0, 0.6);
    }
    .home-page {
      padding: 30px;
      margin-top: 20px;
      border-radius: 12px;
      background: rgba(0, 0, 0, 0.6);
      box-shadow: 4px 4px 12px rgba(0, 0, 0, 0.6);
      overflow-y: auto;
      max-height: 800px;
    }
    .content-section {
      font-size: 18px;
      margin-top: 20px;
      line-height: 1.8;
    }
    .btn-shortcut {
      display: inline-block;
      background: #27ae60;
      color: white;
      font-size: 16px;
      font-weight: bold;
      padding: 10px 15px;
      margin: 10px 10px 20px;
      border-radius: 8px;
      text-align: center;
      text-decoration: none;
      transition: background 0.3s ease, transform 0.3s, box-shadow 0.3s;
      box-shadow: 2px 2px 5px rgba(0, 0, 0, 0.3);
      cursor: pointer;
    }
    .btn-shortcut:hover {
      background: #1e8449;
      transform: translateY(-5px) scale(1.05);
      box-shadow: 4px 4px 8px rgba(0, 0, 0, 0.4);
    }
    .overview-panel {
      color: black;
    }
    .model-panel {
      color: black;
      background: linear-gradient(145deg, #d5e1f2, #c7d8ea);
      border-radius: 15px;
      padding: 20px;
      box-shadow: 5px 5px 15px rgba(0, 0, 0, 0.4), -5px -5px 15px rgba(255, 255, 255, 0.3);
    }
    .model-panel h3, .model-panel h4, .model-panel p, .model-panel .btn-custom {
      color: black;
    }
    .btn-custom {
      background: #2980b9;
      color: white;
      padding: 10px 15px;
      border-radius: 8px;
      font-weight: bold;
      transition: all 0.3s ease;
    }
    .btn-custom:hover {
      background: #21618c;
      transform: translateY(-5px);
      box-shadow: 3px 3px 10px rgba(0, 0, 0, 0.3);
    }
  ")),
  
  # Tabset Panel with conditional panels
  uiOutput("mainUI")
)

# Define the Server Logic
server <- function(input, output, session) {
  
  # User data
  users <- reactiveVal(data.frame(username = "admin", password = "admin123"))
  logged_in <- reactiveVal(FALSE)
  user_data <- reactiveVal(NULL)
  
  # Main UI output with authentication check
  output$mainUI <- renderUI({
    tabsetPanel(
      id = "tabsetPanel",
      
      # Login/Register Tab - Always visible
      tabPanel("Login/Register",
               div(class = "home-page",
                   h2("Login/Register", class = "title"),
                   tabsetPanel(
                     tabPanel("Login",
                              textInput("login_username", "Username"),
                              passwordInput("login_password", "Password"),
                              actionButton("login_btn", "Login", class = "btn-shortcut"),
                              if(logged_in()) div(style="color: green;", "Logged in successfully!")
                     ),
                     tabPanel("Register",
                              textInput("register_username", "Username"),
                              passwordInput("register_password", "Password"),
                              actionButton("register_btn", "Register", class = "btn-shortcut")
                     )
                   )
               )
      ),
      
      # Conditional panels - Only visible when logged in
      if(logged_in()) {
        tabPanel("Home",
                 div(class = "home-page",
                     h2("Welcome to the Crop Recommendation and Prediction System", class = "title"),
                     div(tags$img(src = "new.png", height = "200px", width = "300px", alt = "Something went wrong", deleteFile = FALSE), style = "text-align: center"),
                     div(class = "content-section",
                         p("Key Features Include:"),
                         tags$ul(
                           tags$li("Real-time crop recommendations based on user-input environmental conditions."),
                           tags$li("Data visualization tools that allow users to explore trends, patterns, and correlations."),
                           tags$li("Predictive models that forecast crop performance under different scenarios.")
                         ),
                         p("Navigate through the platform using the shortcuts below:"),
                         actionButton("go_summary", "Summary Statistics", class = "btn-shortcut"),
                         actionButton("go_model", "Model Training", class = "btn-shortcut"),
                         actionButton("go_visualization", "Data Visualization", class = "btn-shortcut"),
                         actionButton("go_overview", "Data Overview", class = "btn-shortcut"),
                         actionButton("go_upload", "Upload Dataset", class = "btn-shortcut"),
                         actionButton("logout_btn", "Logout", class = "btn-shortcut")
                     )
                 )
        )
      },
      
      if(logged_in()) {
        tabPanel("Upload Dataset",
                 div(class = "home-page",
                     h2("Upload Dataset"),
                     fileInput("dataset", "Choose CSV File", accept = ".csv"),
                     tableOutput("uploaded_data")
                 )
        )
      },
      
      if(logged_in()) {
        tabPanel("Summary Statistics", 
                 div(class = "summary-panel", 
                     h2("Summary Statistics"),
                     tableOutput("summary_stats")
                 )
        )
      },
      
      if(logged_in()) {
        tabPanel("Model Training", 
                 sidebarLayout(
                   sidebarPanel(
                     div(class = "model-panel",
                         h3("Train a Random Forest Model"),
                         selectInput("target", "Select Target Variable", choices = NULL),
                         actionButton("train_model", "Train Model", class = "btn-custom"),
                         uiOutput("prediction_inputs"),
                         actionButton("make_prediction", "Predict Crop", class = "btn-custom"),
                         verbatimTextOutput("prediction_result")
                     )
                   ),
                   mainPanel(
                     div(class = "model-panel",
                         h4("Model Training Summary"),
                         verbatimTextOutput("model_summary"),
                         h4("Trained Model Results"),
                         verbatimTextOutput("model_results")
                     )
                   )
                 )
        )
      },
      
      if(logged_in()) {
        tabPanel("Data Visualization", 
                 div(class = "visualization-panel", 
                     h2("Data Visualization"),
                     selectInput("plot_type", "Select Plot Type", 
                                 choices = c("Scatter Plot", "Box Plot", "Line Graph", "Pie Chart")),
                     selectInput("x_var", "Select X Variable", choices = NULL),
                     selectInput("y_var", "Select Y Variable", choices = NULL),
                     actionButton("generate_plot", "Generate Plot", class = "btn-shortcut"),
                     plotOutput("plot_output")
                 )
        )
      },
      
      if(logged_in()) {
        tabPanel("Data Overview", 
                 div(class = "overview-panel", 
                     h2("Dataset Overview"),
                     tableOutput("data_overview")
                 )
        )
      }
    )
  })
  
  # Authentication logic
  observeEvent(input$login_btn, {
    valid_user <- users() %>% 
      filter(username == input$login_username, password == input$login_password)
    if (nrow(valid_user) > 0) {
      logged_in(TRUE)
      showNotification("Login successful", type = "message")
      updateTabsetPanel(session, "tabsetPanel", selected = "Home")
    } else {
      showNotification("Invalid login", type = "error")
    }
  })
  
  # Logout logic
  observeEvent(input$logout_btn, {
    logged_in(FALSE)
    showNotification("Logged out successfully", type = "message")
    updateTabsetPanel(session, "tabsetPanel", selected = "Login/Register")
  })
  
  observeEvent(input$register_btn, {
    users(rbind(users(), data.frame(username = input$register_username, password = input$register_password)))
    showNotification("Registration successful", type = "message")
  })
  
  # Dataset upload
  observeEvent(input$dataset, {
    req(input$dataset)
    user_data(read.csv(input$dataset$datapath))
    updateSelectInput(session, "target", choices = names(user_data()))
    updateSelectInput(session, "x_var", choices = names(user_data()))
    updateSelectInput(session, "y_var", choices = names(user_data()))
  })
  output$uploaded_data <- renderTable({ user_data() })
  
  # Summary statistics
  output$summary_stats <- renderTable({ req(user_data()); summary(user_data()) })
  
  # Navigation from Home page
  observeEvent(input$go_summary, { updateTabsetPanel(session, "tabsetPanel", selected = "Summary Statistics") })
  observeEvent(input$go_model, { updateTabsetPanel(session, "tabsetPanel", selected = "Model Training") })
  observeEvent(input$go_visualization, { updateTabsetPanel(session, "tabsetPanel", selected = "Data Visualization") })
  observeEvent(input$go_overview, { updateTabsetPanel(session, "tabsetPanel", selected = "Data Overview") })
  observeEvent(input$go_upload, { updateTabsetPanel(session, "tabsetPanel", selected = "Upload Dataset") })
  
  # Data visualization
  output$plot_output <- renderPlot({
    req(user_data(), input$plot_type, input$x_var)
    data <- user_data()
    if (input$plot_type == "Scatter Plot") {
      req(input$y_var)
      ggplot(data, aes_string(x = input$x_var, y = input$y_var)) + geom_point()
    } else if (input$plot_type == "Box Plot") {
      req(input$y_var)
      ggplot(data, aes_string(x = input$x_var, y = input$y_var)) + geom_boxplot()
    } else if (input$plot_type == "Line Graph") {
      req(input$y_var)
      ggplot(data, aes_string(x = input$x_var, y = input$y_var)) + geom_line()
    } else if (input$plot_type == "Pie Chart") {
      data_summary <- data %>% group_by(!!sym(input$x_var)) %>% summarize(total = n())
      ggplot(data_summary, aes(x = "", y = total, fill = !!sym(input$x_var))) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y") +
        labs(fill = input$x_var, y = "Count", x = "") +
        theme_void()
    }
  })
  
  # Data overview
  output$data_overview <- renderTable({ req(user_data()); head(user_data()) })
  
  # Model training and prediction
  rf_model <- reactiveVal(NULL)
  
  observeEvent(input$train_model, {
    req(input$target)
    data <- user_data()
    target_var <- input$target
    if (is.numeric(data[[target_var]]) || is.integer(data[[target_var]])) {
      model <- randomForest(as.formula(paste(target_var, "~ .")), data = data)
      rf_model(model)
      output$model_summary <- renderPrint({ summary(model) })
      output$model_results <- renderPrint({ model })
    } else if (is.factor(data[[target_var]]) || is.character(data[[target_var]])) {
      data[[target_var]] <- as.factor(data[[target_var]])
      model <- randomForest(as.formula(paste(target_var, "~ .")), data = data)
      rf_model(model)
      output$model_summary <- renderPrint({ summary(model) })
      output$model_results <- renderPrint({ model })
    } else {
      showNotification("Target variable must be numeric or a factor.", type = "error")
    }
  })
  
  output$prediction_inputs <- renderUI({
    req(rf_model())
    lapply(names(user_data()), function(var) {
      numericInput(paste0("pred_", var), var, value = 0)
    })
  })
  
  observeEvent(input$make_prediction, {
    req(rf_model())
    inputs <- sapply(names(user_data()), function(var) input[[paste0("pred_", var)]])
    prediction <- predict(rf_model(), as.data.frame(t(inputs)))
    output$prediction_result <- renderPrint({ paste("Predicted Crop:", prediction) })
  })
}

# Run the App
shinyApp(ui, server)