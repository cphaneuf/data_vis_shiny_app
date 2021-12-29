#######################################################
### Shiny App for Simple, Exploratory Data Analysis ###
#######################################################

# Developer: Camille Phaneuf (cphaneuf@umich.edu)
# Last updated: 12/28/21
# Modified from: https://github.com/MatePocs/rshiny_apps

# Run Shiny web application by clicking the 'Run App' button above.
# For more information: http://shiny.rstudio.com/

###########################
### Prepare Application ###
###########################

# Load necessary packages
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(data.table)
library(ggplot2)

###################################
### Initialize Global Variables ###
###################################

# Plot colors

# Choice options
none_selected <- 'none selected'

#############
### Pages ###
#############

# Welcome page
welcome_page <- tabPanel(
    title = "Welcome",
    
    # Display user name greeting
    textInput("user_name", "What is your name?", "JANE DOE"),
    textOutput("user_name_greeting")
)

# About page
about_page <- tabPanel(
    title = "About",
    titlePanel("About"),
    "This browser-based application allows users to examine relationships among specified variables of interest from a dataset of their choosing. It was developed by Camille Phaneuf in the Affective Neuroscience and Development Lab at Harvard University.",
    # br(),
    # FIX THIS TO BE A HYPERLINK
    "Thank you to github user @MatePocs for their tremendous Shiny App resource (https://github.com/MatePocs/rshiny_apps), upon which much of this was built.",
    imageOutput("lab_logo")
)

# Analysis page
analysis_page <- tabPanel(
    title = "Analysis",
    titlePanel("Analysis"),
    sidebarLayout(
        sidebarPanel(
            title = "Inputs",
            fileInput("csv_input", "Select .csv file to import. This .csv file should be stored in data_vis_shiny_app/data.", accept = ".csv"),
            selectInput("con_num_var", "Which continuous numeric variable would you like to examine?", choices = c(none_selected)),
            selectInput("dis_num_var", "Which discrete numeric variable would you like to examine?", choices = c(none_selected)),
            selectInput("ord_cat_var", "Which ordinal categoric variable would you like to examine?", choices = c(none_selected)),
            selectInput("nom_cat_var", "Which nominal categoric variable would you like to examine?", choices = c(none_selected)),
            br(),
            actionButton("run_button", "Run simple, exploratory data analysis.", icon = icon("play"))
        ),
        mainPanel(
            tabsetPanel(
                tabPanel(
                    title = "Continuous Numeric Visualizations",
                    plotOutput("hist"),
                    sliderInput("bins", "Number of bins:", min = 1, max = 50, value = 30) # FIX THIS TO BE DYNAMIC ACCORDING TO COLUMN SELECTION
                    # plotOutput("scatter") IMPLEMENT THIS
                ),
                tabPanel(
                    title = "Discrete Numeric Visualizations"
                ),
                tabPanel(
                    title = "Ordinal Categoric Visualizations"
                ),
                tabPanel(
                    title = "Nominal Categoric Visualizations"
                )
            )
        )
    )
)

######################
### User Interface ###
######################

# Define UI for application that allows users to examine relationships among specified variables of interest
ui <- navbarPage(
    title = "Exploratory Data Analyzer",
    theme = shinytheme('united'),
    welcome_page,
    analysis_page,
    about_page
)

##############
### Server ###
##############

# Define server logic required to process user input
server <- function(input, output) {

    # Define user name greeting
    output$user_name_greeting <- renderText({
        paste0("Welcome to your simple, exploratory data analysis, ", input$user_name, "!")
    })
    
    # Define ANDL logo image for display
    output$lab_logo <- renderImage({
        # List containing the image filename
        list(src = 'andl_logo.png',
             contentType = 'image/png',
             width = 200,
             height = 200,
             alt = "Affective Neuroscience and Development Lab logo.")
    }, deleteFile=FALSE)
    
    # Require that a csv file be read-in to application 
    data_input <- reactive({
        req(input$csv_input)
        fread(input$csv_input$datapath)
    })
    
    # Update choice options for continuous numeric variables based on data file read-in
    observeEvent(data_input(),{
        choices <- c(none_selected, names(data_input()))
        updateSelectInput(inputId = "con_num_var", choices = choices)
    })
    
    con_num_var <- eventReactive(input$run_button, input$con_num_var)

    # Histogram for continuous numeric variables
    hist <- eventReactive(input$run_button,{
        # FIX PLOT AESTHETICS
        ggplot(data = data_input(),
               aes_string(x = con_num_var())) +
            geom_histogram(bins = 5) # FIX THIS BY MAKING BIN NUMBER DYNAMIC
    })
    output$hist <- renderPlot(hist())
}

# Run the application 
shinyApp(ui = ui, server = server)

# Resources used:
# https://towardsdatascience.com/how-to-build-a-data-analysis-app-in-r-shiny-143bee9338f7
# https://github.com/MatePocs/rshiny_apps/blob/main/data_analyser/app.R
# https://shiny.rstudio.com/articles/images.html
# https://stackoverflow.com/questions/50218614/shiny-selectinput-to-select-all-from-dropdown
# https://shiny.rstudio.com/articles/layout-guide.html
# https://rstudio.github.io/shinythemes/
