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

# Plot colors and theme
andl_red <- "#DD292A"
andl_burgundy <- "#970505"
andl_pink <- "#F59596"
andl_orange <- "#FF871E"
color_scheme <- scale_color_manual(values = c(andl_red, andl_burgundy, 
                                              andl_pink, andl_orange))
plot.theme <- theme(title = element_text(size = 18, vjust = 2, face = "bold"),
                              plot.title = element_text(hjust = .5), # center plot title (where applicable)
                              axis.title.x = element_text(size = 18, vjust = -0.5),
                              axis.title.y = element_text(size = 18, vjust = 1.5),
                              axis.text.x = element_text(size = 12, colour = "black", vjust = -0.5),
                              axis.text.y = element_text(size = 12, colour = "black"),
                              legend.title = element_blank(), # removes legend title
                              legend.text = element_text(size = 12),
                              legend.position = "bottom",
                              legend.key = element_rect(fill = "transparent", colour = NA),
                              strip.background = element_rect(fill = "grey90"),
                              strip.text.x = element_text(size = 12, colour = "black"), # format vertical facet header text (where applicable)
                              strip.text.y = element_text(size = 12, colour = "black"), # format horizontal facet header text (where applicable)
                              # text = element_text(family = "Ubuntu"),
                              panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(),
                              panel.background = element_blank(), 
                              axis.line = element_line(colour = "black"))

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
    "Thank you to Github user @MatePocs for their ", 
        tags$a(href = "https://github.com/MatePocs/rshiny_apps", "tremendous Shiny App resource"),
        ", upon which much of this was built.",
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
        df = data_input()
        var = con_num_var()
        num_bins <- length(df[[1]]) / 5
        mean_var <- mean(df[[var]])
        ggplot(data = df,
               aes_string(x = var)) +
            geom_histogram(bins = num_bins, color = andl_burgundy, fill = andl_pink) +
            geom_vline(aes(xintercept = mean_var), color = andl_burgundy, linetype = "dashed", size = 1) +
            plot.theme
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
# http://www.sthda.com/english/wiki/ggplot2-histogram-plot-quick-start-guide-r-software-and-data-visualization
