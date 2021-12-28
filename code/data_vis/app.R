#######################################################
### Shiny App for Simple, Exploratory Data Analysis ###
#######################################################

# Developer: Camille Phaneuf (cphaneuf@umich.edu)
# Last updated: 12/27/21

# Run Shiny web application by clicking the 'Run App' button above.
# For more information: http://shiny.rstudio.com/

# Load necessary packages
library(shiny)

# Read in data
setwd("/Users/camillephaneuf/Desktop/ANDL/HCPD/data_vis_shiny_app/code/data_vis")
data <- read.csv('../../data/data.csv')

# Define UI for application that does XYZ
ui <- fluidPage(
    
    # Application title
    titlePanel("Exploratory Data Analysis"),
    h3('Welcome to this Shiny App created by cphaneuf!'),
    
    # Display user name greeting
    textInput("user_name", "What is your name?", "JANE DOE"),
    textOutput("user_name_greeting"),
    h3('\n'),
    
    # Display data name greeting
    textInput("data_name", "What is the name of the .csv data file that you will be visualizing today? This .csv file should be stored in data_vis_shiny_app/data.", "EXAMPLE.csv"),
    textOutput("data_name_greeting"),
    
    # Visualize continuous numeric variables
    h3('Visualize Continuous Numeric Variables'),
    
    # Interactive histograms
    h4('Histograms'),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    ),
    
    # Interactive scatterplots
    h4('Scatterplots'),
    
    selectInput('x_var', 'Select x', names(data), 'age'),
    selectInput('y_var', 'Select y', names(data), 'task_1'),
    #numericInput('something', 'Somthing', 3),
    #plotOutput("func")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
#server <- function(input, output, session) {
    
    # Define user name greeting
    output$user_name_greeting <- renderText({
        paste0("Welcome to your simple, exploratory data analysis, ", input$user_name, "!")
    })
    
    # Define data name greeting
    output$data_name_greeting <- renderText({
        #data <- read.csv('../../data/data.csv')
        paste0("Thanks! Let's get started with ", input$data_name, " then.")
    })
    
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
    
    output$func <- renderPlot({
        plot(x_var, y_var)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
