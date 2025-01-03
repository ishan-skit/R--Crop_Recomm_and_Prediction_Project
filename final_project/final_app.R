library(shiny)
library(caret)
library(ggplot2)
library(randomForest)
library(dplyr)
library(googleAuthR)
library(httr)
library(jsonlite)

# Initialize Google Authentication
options(googleAuthR.scopes.selected = c(
  "https://www.googleapis.com/auth/userinfo.email",
  "https://www.googleapis.com/auth/userinfo.profile"
))

# Set your Google OAuth credentials
options(googleAuthR.client_id = "320433890459-51iqi8k28vs01cqgk7u8bic1prrs9pja.apps.googleusercontent.com")
options(googleAuthR.client_secret = "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")
options(googleAuthR.webapp.redirect_uri = "http://127.0.0.1:5418")

# Custom Google Auth UI function
googleAuthUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(
      class = "google-auth-container",
      actionButton(
        inputId = ns("google_login"),
        label = span(
          icon("google"), 
          "Sign in with Google"
        ),
        class = "google-login-button"
      )
    )
  )
}

# Google Authentication Module
googleAuthServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    auth_token <- reactiveVal(NULL)
    user_info <- reactiveVal(NULL)
    
    observeEvent(input$google_login, {
      tryCatch({
        withProgress(message = 'Authenticating with Google...', {
          token <- gar_auth()
          if (!is.null(token)) {
            auth_token(token)
            
            # Fetch user profile
            profile <- GET(
              "https://www.googleapis.com/oauth2/v1/userinfo",
              config(token = token)
            )
            
            if (status_code(profile) == 200) {
              user_data <- fromJSON(rawToChar(profile$content))
              user_info(user_data)
              showNotification("Successfully signed in with Google", type = "message")
            }
          }
        })
      }, error = function(e) {
        showNotification(
          paste("Google authentication failed:", e$message),
          type = "error"
        )
      })
    })
    
    return(list(
      token = auth_token,
      user_info = user_info
    ))
  })
}

# Define UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      /* Your existing styles remain the same */
      .login-container {
        background: rgba(255, 255, 255, 0.9);
        border-radius: 20px;
        padding: 30px;
        box-shadow: 0 8px 32px rgba(0, 0, 0, 0.1);
        backdrop-filter: blur(10px);
        max-width: 1200px;
        margin: 40px auto;
      }
      /* ... [Keep all your existing CSS styles] ... */
      .login-header {
        text-align: center;
        margin-bottom: 30px;
      }
      .login-header h1 {
        color: #2c3e50;
        font-size: 2.5em;
        font-weight: 700;
        text-transform: uppercase;
        margin-bottom: 15px;
        text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.2);
      }
      .login-content {
        display: grid;
        grid-template-columns: 1fr 1fr;
        gap: 40px;
        align-items: start;
      }
      .login-form {
        padding: 20px;
        background: linear-gradient(145deg, #ffffff, #f0f0f0);
        border-radius: 15px;
        box-shadow: 5px 5px 15px rgba(0, 0, 0, 0.1);
      }
      .platform-info {
        color: #2c3e50;
        line-height: 1.6;
        padding: 20px;
      }
      .features-list {
        margin: 20px 0;
        padding-left: 20px;
      }
      .features-list li {
        margin: 15px 0;
        padding-left: 25px;
        position: relative;
      }
      .features-list li:before {
        content: '→';
        position: absolute;
        left: 0;
        color: #27ae60;
      }
      body {
        background: url('agri.jpg') no-repeat center center fixed;
        background-size: cover;
        font-family: 'Roboto', sans-serif;
        color: #ffffff;
        margin: 0;
      }
      .visualization-panel {
        color: black;
      }
      .summary-panel {
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
      
      .google-auth-container {
        margin: 20px 0;
        width: 100%;
      }
      
      /* Enhanced Google login button styles */
      .google-login-button {
        background-color: #4285f4;
        color: white;
        padding: 12px 24px;
        border-radius: 4px;
        border: none;
        cursor: pointer;
        width: 100%;
        font-size: 16px;
        font-weight: 500;
        display: flex;
        align-items: center;
        justify-content: center;
        transition: all 0.3s ease;
        margin-top: 20px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.25);
      }
      
      .google-login-button:hover {
        background-color: #357abd;
        box-shadow: 0 4px 8px rgba(0,0,0,0.3);
        transform: translateY(-1px);
      }
      
      .google-login-button .fa {
        margin-right: 12px;
        font-size: 18px;
      }
    "))
  ),
  
  uiOutput("mainUI")
)

# Server Logic
server <- function(input, output, session) {
  # Initialize reactive values
  users <- reactiveVal(data.frame(username = "admin", password = "admin123"))
  logged_in <- reactiveVal(FALSE)
  user_data <- reactiveVal(NULL)
  
  # Initialize Google Auth
  google_auth <- googleAuthServer("google_auth")
  
  # Watch for Google authentication
  observe({
    req(google_auth$user_info())
    logged_in(TRUE)
    updateTabsetPanel(session, "tabsetPanel", selected = "Home")
  })
  
  # Main UI Output
  output$mainUI <- renderUI({
    tabsetPanel(
      id = "tabsetPanel",
      
      # Login/Register Tab
      tabPanel("Login/Register",
               div(class = "login-container",
                   div(class = "login-header",
                       h1("Crop Recommendation and Prediction System")
                   ),
                   div(class = "login-content",
                       div(class = "login-form",
                           tabsetPanel(
                             tabPanel("Login",
                                      textInput("login_username", "Username"),
                                      passwordInput("login_password", "Password"),
                                      actionButton("login_btn", "Login", class = "btn-shortcut"),
                                      tags$hr(),
                                      googleAuthUI("google_auth")
                             ),
                             tabPanel("Register",
                                      textInput("register_username", "Username"),
                                      passwordInput("register_password", "Password"),
                                      actionButton("register_btn", "Register", class = "btn-shortcut")
                             )
                           )
                       ),
                       div(class = "platform-info",
                           # ... [Keep your existing welcome content] ...
                           h2("Welcome to the Crop Recommendation System"),
                           p("This platform combines advanced data science and machine learning techniques to help farmers, researchers, and agricultural enthusiasts make informed decisions about crop selection."),
                           h3("Features & Services:"),
                           tags$ul(class = "features-list",
                                   tags$li("Personalized Crop Recommendations"),
                                   tags$li("Crop Yield Predictions"),
                                   tags$li("Weather-Based Recommendations"),
                                   tags$li("Resource Optimization"),
                                   tags$li("Data Visualization"),
                                   tags$li("User-Friendly Interface"),
                                   tags$li("Data-Driven Insights"),
                                   tags$li("Expert Guidance")
                           ),
                           p("Get started by logging in or registering to unlock all features!")
                       )
                   )
               )
      ),
      
      # Conditional panels for logged-in users
      if(logged_in()) {
        tabPanel("Home",
                 div(class = "home-page",
                     # ... [Keep your existing home page content] ...
                     h2("Welcome to the Crop Recommendation and Prediction System", class = "title"),
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
      
      # ... [Keep all your other conditional panels] ...
      
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
  
  # Logout handler
  observeEvent(input$logout_btn, {
    if (!is.null(google_auth$token())) {
      gar_auth_service(reset = TRUE)
    }
    logged_in(FALSE)
    showNotification("Logged out successfully", type = "message")
    updateTabsetPanel(session, "tabsetPanel", selected = "Login/Register")
  })
  
  # ... [Keep all your existing server logic for dataset handling,
  #      visualization, model training, etc.] ...
  
  observeEvent(input$register_btn, {
    users(rbind(users(), data.frame(username = input$register_username, 
                                    password = input$register_password)))
    showNotification("Registration successful", type = "message")
  })
  
  
  # Dataset handling
  observeEvent(input$dataset, {
    req(input$dataset)
    user_data(read.csv(input$dataset$datapath))
    updateSelectInput(session, "target", choices = names(user_data()))
    updateSelectInput(session, "x_var", choices = names(user_data()))
    updateSelectInput(session, "y_var", choices = names(user_data()))
  })
  
  # ... [Keep all your existing outputs and reactive expressions] ...
  output$uploaded_data <- renderTable({ user_data() })
  output$summary_stats <- renderTable({ req(user_data()); summary(user_data()) })
  output$data_overview <- renderTable({ req(user_data()); head(user_data()) })
  
  # Navigation
  observeEvent(input$go_summary, { 
    updateTabsetPanel(session, "tabsetPanel", selected = "Summary Statistics") 
  })
  observeEvent(input$go_model, { 
    updateTabsetPanel(session, "tabsetPanel", selected = "Model Training") 
  })
  observeEvent(input$go_visualization, { 
    updateTabsetPanel(session, "tabsetPanel", selected = "Data Visualization") 
  })
  observeEvent(input$go_overview, { 
    updateTabsetPanel(session, "tabsetPanel", selected = "Data Overview") 
  })
  observeEvent(input$go_upload, { 
    updateTabsetPanel(session, "tabsetPanel", selected = "Upload Dataset") 
  })
  
  # Visualization
  output$plot_output <- renderPlot({
    req(user_data(), input$plot_type, input$x_var)
    data <- user_data()
    
    if (input$plot_type == "Scatter Plot") {
      req(input$y_var)
      ggplot(data, aes_string(x = input$x_var, y = input$y_var)) + 
        geom_point() +
        theme_minimal()
    } else if (input$plot_type == "Box Plot") {
      req(input$y_var)
      ggplot(data, aes_string(x = input$x_var, y = input$y_var)) + 
        geom_boxplot() +
        theme_minimal()
    } else if (input$plot_type == "Line Graph") {
      req(input$y_var)
      ggplot(data, aes_string(x = input$x_var, y = input$y_var)) + 
        geom_line() +
        theme_minimal()
    } else if (input$plot_type == "Pie Chart") {
      data_summary <- data %>% 
        group_by(!!sym(input$x_var)) %>% 
        summarize(total = n())
      ggplot(data_summary, aes(x = "", y = total, fill = !!sym(input$x_var))) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y") +
        labs(fill = input$x_var, y = "Count", x = "") +
        theme_void()
    }
  })
  
  # Model Training and Prediction
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

# Run the application
shinyApp(
  ui = ui,
  server = server,
  options = list(
    host = "127.0.0.1",
    port = 5418
  )
)
