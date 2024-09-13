# Pollen App: A Comprehensive Allergy Management Tool
# ===================================================

# 1. Setup and Configuration
# --------------------------

# 1.1 Load Required Libraries
# These libraries provide essential functionality for data manipulation,
# UI components, authentication, visualization, and database interactions.

library(config)
config <- config::get()
library(shiny)
library(dplyr)
library(httr)
library(jsonlite)
library(shinyjs)
library(shinyWidgets)
library(ggplot2)
library(DT)
library(reshape2)
library(plotly)
library(leaflet)
library(mongolite)
library(tidyr)
library(lubridate)


# 1.2 Source Helper Functions and Configuration
# These files contain custom functions and settings used throughout the app.
source("functions.R")  # Custom functions for data processing and UI generation
source("config.R")     # Configuration settings (e.g., API keys, database connections)

# 1.3 Load Plant Glossary Data
# This dataset contains information about various pollen-producing plants.
plant_gloss <- read.csv("plant_gloss.csv", 
                          stringsAsFactors = FALSE)

# 1.4 Configure Authentication
options(gargle_oauth_email = TRUE)

# 2. UI Definition
# ----------------

ui <- fluidPage(
  # 2.1 Enable shinyjs for Dynamic UI Manipulation
  useShinyjs(),
  
  # 2.2 Include CSS and JavaScript Resources
  # These resources enhance the app's appearance and functionality.
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$script(src = "https://code.jquery.com/ui/1.12.1/jquery-ui.min.js"),
    tags$link(rel = "stylesheet", href = "https://code.jquery.com/ui/1.12.1/themes/base/jquery-ui.css"),
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0-beta3/css/all.min.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "introjs.css"),
    tags$script(src = "intro.min.js"),
    
    # Custom JavaScript function for tab switching
    tags$script(HTML("
      function switchToTab(tabName) {
        var tabLink = $('a[data-bs-toggle=\"tab\"][data-value=\"' + tabName + '\"]');
        if (tabLink.length) {
          tabLink.tab('show');
          $(document).trigger('shiny:tab:switched', tabName);
        } else {
          console.error('Tab not found:', tabName);
        }
      }
    ")),
    
    # JavaScript to enable draggable panels
    tags$script(HTML("
      $(document).ready(function() {
        $('.draggable-panel').draggable({
          containment: '.map-container',
          cursor: 'move'
        });
      });
    "))
  ),
  
  # 2.3 Home Page
  # This is the initial landing page for the app.
  div(id = "home_page", class = "fullscreen-bg",
      div(class = "jumbotron text-center",
          h1("pollen app"),
          p("explore pollen levels for any location and manage your allergies"),
          actionButton("enter_app", 
                       HTML('<i class="fas fa-seedling"></i>'), 
                       class = "btn-lg btn-primary")
      )
  ),
  
  # 2.4 Login Form
  # Secure user authentication interface.
  hidden(
    div(id = "login_page",
        div(class = "container login-container",
            div(class = "login-box",
                h2("Please Login", class = "text-center mb-4"),
                textInput("username", 
                          label = tags$label(
                            icon("user"), 
                            "Username"
                          )
                ),
                passwordInput("password", 
                              label = tags$label(
                                icon("lock-open"), 
                                "Password"
                              )
                ),
                actionButton("login", 
                             HTML('<i class="fa-solid fa-right-to-bracket"></i>'), 
                             class = "btn-primary btn-block"),
                uiOutput("auth_status")
            )
        )
    )
  ),
  
  # 2.5 Main App Content
  # The core functionality of the app, hidden until user authentication.
  hidden(
    div(id = "app_content",
        navbarPage(
          # 2.5.1 Navbar with App Title
          title = div(class = "navbar-custom",
                      div(class = "navbar-title", "pollen app")
          ),
          id = "navbar",
          inverse = FALSE,
          collapsible = TRUE,
          
          # Header with welcome message and user controls
          header = div(
            class = "container",
            div(
              class = "jumbotron jumbotron-welcome",
              div(class = "row align-items-center",
                  div(class = "col-md-8",
                      h2("Welcome to the Pollen App"),
                      div(class = "welcome-controls",
                          actionButton("start_tour", "Start Tour", class = "btn-sm btn-primary")
                      )
                  ),
                  div(class = "col-md-4",
                      div(class = "user-controls",
                          tags$span(class = "fa fa-user user-icon"),
                          textOutput("user_info", inline = TRUE),
                          actionButton("logout", "Logout", icon = icon("sign-out-alt"), class = "btn-sm")
                      )
                  )
              )
            )
          ),
          
          # 2.5.2 Pollen Map Tab
          # Interactive map displaying pollen levels and related information.
          tabPanel("Pollen Map",
                   id = "pollen-map-tab", value = "pollen-map-tab",
                   div(class = "container",
                       div(class = "jumbotron",
                           fluidRow(
                             # Input panel for location and date selection
                             column(width = 4,
                                    wellPanel(id = "input_panel",
                                              div(id = "input_main",
                                                  textInput("place", "Enter Location", "Zagreb Croatia"),
                                                  dateInput("date", "Select Date", min = Sys.Date(), max = Sys.Date() + 4, autoclose = TRUE),
                                                  div(id = "buttons",
                                                      actionBttn("submit", HTML('<i class="fas fa-seedling"></i> Get Data'), class = "unite"),
                                                      actionBttn("reset", HTML('<i class="fas fa-undo"></i> Reset'), class = "unite"),
                                                      actionBttn("save_data", HTML('<i class="fas fa-save"></i> Save to My Log'), class = "unite")
                                                  ),
                                                  div(id = "switches",
                                                      materialSwitch("health_switch", "Health Recommendations", value = TRUE, status = "primary"),
                                                      materialSwitch("plant_switch", "Plant Info", value = TRUE, status = "primary")
                                                  )
                                              )
                                    ),
                                    # Plant information panel
                                    conditionalPanel(
                                      condition = "input.plant_switch == true",
                                      div(id = "plant_panel", class = "panel panel-default",
                                          div(class = "panel-body",
                                              h4(HTML('<i class="fas fa-seedling"></i> Plant Information')),
                                              htmlOutput("plant_info")
                                          )
                                      )
                                    )
                             ),
                             
                             # Map and health information panel
                             column(width = 8,
                                    div(class = "map-container",
                                        leafletOutput("pollenMap", width = "100%", height = "100%"),
                                        div(id = "health_panel", class = "draggable-panel",
                                            h4(HTML('<i class="fas fa-heartbeat"></i> Health Recommendations')),
                                            textOutput("health_recommendations")
                                        )
                                    )
                             )
                           )
                       )
                   )
          ),
          
          # 2.5.3 My Log Tab
          # Personal allergy diary and data visualization.
          tabPanel("My Log",
                   id = "my-log-tab", value = "my-log-tab",
                   div(class = "container", 
                       div(class = "jumbotron",
                           uiOutput("data_encouragement"),
                           fluidRow(
                             column(6,
                                    div(class = "panel panel-default",
                                        div(class = "panel-heading",
                                            h3(class = "panel-title", "Pollen and Symptoms Data")
                                        ),
                                        div(class = "panel-body",
                                            uiOutput("pollen_log_or_message"),
                                            br(),
                                            div(id = "log_buttons",
                                                actionButton("reset_log", label = tagList(icon("redo"), "Reset Log")),
                                                downloadButton("download_csv", "Download CSV"),
                                                downloadButton("download_pdf", "Download PDF")
                                            )
                                        )
                                    )
                             ),
                             column(6,
                                    div(class = "panel panel-default",
                                        div(class = "panel-heading",
                                            h3(class = "panel-title", "Average Symptom Intensity")
                                        ),
                                        div(class = "panel-body",
                                            uiOutput("bar_chart_or_message")
                                        )
                                    ),
                                    div(class = "panel panel-default mt-4",
                                        div(class = "panel-heading",
                                            h3(class = "panel-title", "Pollen Levels and Symptoms Over Time")
                                        ),
                                        div(class = "panel-body",
                                            uiOutput("multi_line_chart_or_message")
                                        )
                                    )
                             )
                           )
                       )
                   )
          ),
          
          # 2.5.4 Plant Glossary Tab
          # Comprehensive guide to pollen-producing plants.
          tabPanel("Plant Glossary",
                   id = "plant-glossary-tab", value = "plant-glossary-tab",
                   div(class = "container mt-4",
                       div(class = "jumbotron p-4",
                           fluidRow(
                             column(6,
                                    uiOutput("plant_image")
                             ),
                             br(),
                             br(),
                             column(6,
                                    pickerInput("plant_select", "Select a Plant",
                                                choices = sort(unique(plant_gloss$plant_name)),
                                                options = list(`live-search` = TRUE)),
                                    uiOutput("plant_gloss")
                             )
                           )
                       )
                   )
          ),
          
          # 2.5.5 About Tab
          # Information about the app and its features.
          tabPanel("About",
                   id = "about-tab", value = "about-tab",
                   div(class = "container mt-4",
                       div(class = "jumbotron p-4",
                           create_about_tab_content()
                       )
                   )
          ) 
        )
    )
  )
)

# 3. Server Logic
# ---------------

server <- function(input, output, session) {
  
  # 3.1 Reactive Values
  # These values store the app's state and user data.
  current_user <- reactiveVal(NULL)  # Stores the current user's username
  pollen_data <- reactiveVal()       # Stores the current pollen data
  selected_type <- reactiveVal("GRASS")  # Stores the currently selected pollen type
  pollen_log_data <- reactiveVal(reset_pollen_log_data())  # Stores the user's pollen log data
  
  # 3.2 Authentication and Navigation
  # Handles user login, logout, and initial app navigation.
  
  # Show login page when user enters the app
  observeEvent(input$enter_app, {
    shinyjs::hide("home_page")
    shinyjs::show("login_page")
  })
  
  # Handle user login
  observeEvent(input$login, {
    if (authenticate_user(input$username, input$password)) {
      current_user(input$username)
      shinyjs::hide("login_page")
      shinyjs::show("app_content")
      output$auth_status <- renderUI(NULL)
      
      # Load user's saved data
      user_log <- load_user_data(input$username, mongo_conn)
      if (!is.null(user_log)) {
        pollen_log_data(user_log)
      }
    } else {
      output$auth_status <- renderUI({
        div(style = "color: red;", "Invalid username or password.")
      })
    }
  })
  
  # Handle user logout
  observeEvent(input$logout, {
    current_user(NULL)
    shinyjs::hide("app_content")
    shinyjs::show("home_page")
    pollen_log_data(reset_pollen_log_data())
  })
  
  # Display current user information
  output$user_info <- renderText({
    paste(current_user())
  })
  
  # 3.3 UI Controls
  # Manages the visibility and behavior of UI elements.
  
  # Toggle health panel visibility
  observeEvent(input$health_switch, {
    shinyjs::toggle("health_panel", condition = input$health_switch)
  })
  
  # Toggle plant panel visibility
  observeEvent(input$plant_switch, {
    shinyjs::toggle("plant_panel", condition = input$plant_switch)
  })
  
  # 3.4 Pollen Map Functionality
  # Handles pollen data retrieval, map updates, and related operations.
  
  # Initialize and render the pollen map
  output$pollenMap <- renderLeaflet({
    initialize_map(pollen_data(), selected_type())
  })
  
  # Load initial pollen data when the app starts
  observe({
    if (is.null(pollen_data())) {
      data <- load_initial_pollen_data()
      if (!is.null(data)) {
        pollen_data(data)
        selected_type("GRASS")
        update_map(data, "GRASS")
        update_info_panel_output(data, "GRASS")
      }
    }
  })
  
  # Handle pollen type selection from map popups
  observeEvent(input$popup_button, {
    selected_type(input$popup_button)
    update_popups(pollen_data(), input$popup_button)
    update_info_panel_output(pollen_data(), input$popup_button)
  }, ignoreInit = TRUE)
  
  # Handle form submission for new location and date
  observeEvent(input$submit, {
    req(input$place)
    data <- handle_form_submission(input$place, input$date)
    if (!is.null(data)) {
      pollen_data(data)
      selected_type("GRASS")
      update_map(data, selected_type())
      update_info_panel_output(data, selected_type())
    }
  })
  
  # Reset form and map to initial state
  observeEvent(input$reset, {
    reset_form(session)
    selected_type("GRASS")
    data <- load_initial_pollen_data()
    if (!is.null(data)) {
      pollen_data(data)
      update_map(data, selected_type())
      update_info_panel_output(data, selected_type())
    }
  })
  
  # Helper function to update health recommendations and plant info
  update_info_panel_output <- function(data, selected) {
    info <- update_info_panel(data, selected)
    output$health_recommendations <- renderText(info$health_text)
    output$plant_info <- renderText(info$plant_text)
  }
  
  # 3.5 Data Logging Functionality
  # Handles saving pollen data to user's log and managing symptoms.
  
  # Open modal for adding symptoms when saving data
  observeEvent(input$save_data, {
    showModal(modalDialog(
      title = "Add Symptoms",
      div(id = "symptoms-container",
          pickerInput("symptoms", "Select symptoms:",
                      choices = c("No Symptoms","Sneezing", "Runny Nose", "Itchy Eyes", "Coughing", 
                                  "Wheezing", "Shortness of Breath", "Skin Rash", "Headache", "Fatigue"),
                      multiple = TRUE, options = list(`actions-box` = TRUE))
      ),
      uiOutput("symptom_severity_inputs"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_data_with_symptoms", "Save Data with Symptoms")
      )
    ))
  })
  
  # Render severity inputs for selected symptoms
  output$symptom_severity_inputs <- renderUI({
    req(input$symptoms)
    create_symptom_severity_inputs(input$symptoms)
  })
  
  # Save pollen data with symptoms to user's log
  observeEvent(input$save_data_with_symptoms, {
    data <- pollen_data()
    
    if (!is.null(data) && length(input$symptoms) > 0) {
      selected_date <- as.Date(input$date)
      existing_log <- pollen_log_data()
      
      if (any(!is.na(existing_log$Date) & existing_log$Date == selected_date)) {
        showNotification("Data for the selected date already exists in the log. Please select a different date.", type = "error")
        return()
      }
      
      symptom_severities <- sapply(input$symptoms, function(symptom) {
        input[[paste0("severity_", make.names(symptom))]]
      })
      
      new_log_entries <- create_new_log_entries(data, selected_date, input$place, input$symptoms, symptom_severities)
      new_log_entries <- do.call(rbind, new_log_entries)
      
      updated_log <- rbind(existing_log, new_log_entries)
      pollen_log_data(updated_log)
      
      save_user_data(current_user(), updated_log, mongo_conn)
      
      removeModal()
      showNotification("Pollen data with symptoms and severity saved to My Log", type = "message")
    } else {
      showNotification("Please select at least one symptom", type = "error")
    }
  })
  
  # 3.6 My Log Tab Functionality
  # Manages the display and interaction with user's logged data.
  
  # Check if user has any logged data
  has_data <- reactive({
    nrow(pollen_log_data()) > 0
  })
  
  # Check if user has sufficient data for visualization
  has_sufficient_data <- reactive({
    data <- pollen_log_data()
    nrow(data) >= 5 && length(unique(data$Date)) >= 3
  })
  
  # Generate encouragement message based on logged data
  output$data_encouragement <- renderUI({
    generate_data_encouragement(pollen_log_data())
  })
  
  # Render pollen log table or message if no data
  output$pollen_log_or_message <- renderUI({
    generate_pollen_log_content(has_data(), pollen_log_data())
  })
  
  # Render the pollen log data table
  output$pollen_log <- renderDataTable({
    req(has_data())
    data <- format_log_data_for_display(pollen_log_data())
    data$Date <- as.Date(data$Date)  # Ensure Date is in Date format
    data
  }, rownames = FALSE,
  options = list(
    paging = FALSE,
    searching = TRUE,
    scrollX = TRUE,
    scrollY = "700px",
    scrollCollapse = TRUE,
    dom = 'ftiS',
    columnDefs = list(
      list(targets = '_all', className = 'dt-center')
    ),
    autoHeight = TRUE,
    pageLength = -1
  ))
  
  # Toggle visibility of log-related buttons
  observe({
    toggle_log_buttons(session, has_data())
  })
  
  # Render bar chart or message if insufficient data
  output$bar_chart_or_message <- renderUI({
    generate_chart_content(has_sufficient_data(), "bar_chart")
  })
  
  # Generate and render bar chart
  output$bar_chart <- renderPlotly({
    req(has_sufficient_data())
    generate_bar_chart(pollen_log_data())
  })
  
  # Render multi-line chart or message if insufficient data
  output$multi_line_chart_or_message <- renderUI({
    generate_chart_content(has_sufficient_data(), "multi_line_chart")
  })
  
  # Generate and render multi-line chart
  output$multi_line_chart <- renderPlotly({
    req(has_sufficient_data())
    generate_multi_line_chart(pollen_log_data())
  })
  
  # Handle log reset functionality
  observeEvent(input$reset_log, {
    showModal(modalDialog(
      title = "Confirm Reset",
      "Are you sure you want to reset your log? This action cannot be undone.",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_reset", "Confirm Reset", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_reset, {
    pollen_log_data(reset_pollen_log_data())
    save_user_data(current_user(), pollen_log_data(), mongo_conn)
    removeModal()
    showNotification("Log has been reset", type = "message")
  })
  
  # Handle CSV download
  output$download_csv <- downloadHandler(
    filename = function() {
      paste("pollen_log_export_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(pollen_log_data(), file, row.names = FALSE)
    }
  )
  
  # Handle PDF report generation and download
  output$download_pdf <- downloadHandler(
    filename = function() {
      "pollen_log_export.pdf"
    },
    content = function(file) {
      rmarkdown::render("pollen_report_template.Rmd",
                        output_file = file,
                        params = list(data = pollen_log_data(), for_pdf = TRUE))
    }
  )
  
  # 3.7 Plant Glossary Functionality
  # Manages the display of plant information in the Plant Glossary tab.
  
  # React to plant selection
  selected_plant <- reactive({
    req(input$plant_select)
    plant_gloss[plant_gloss$plant_name == input$plant_select, ]
  })
  
  # Render plant image
  output$plant_image <- renderUI({
    req(selected_plant())
    get_plant_image(selected_plant()$plant_name)
  })
  
  # Render plant information
  output$plant_gloss <- renderUI({
    req(selected_plant())
    create_plant_glossary_ui(selected_plant())
  })
  
  
  # 3.8 App Tour Functionality
  # Manages the interactive tour of the app.
  
  observeEvent(input$start_tour, {
    setup_and_start_tour(pollen_log_data())
  })
  
} 
# 4. Run the Application
# ----------------------
shinyApp(ui = ui, server = server)