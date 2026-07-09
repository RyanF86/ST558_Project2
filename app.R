#################################################
#  ST 558                                       #
#  Project #2                                   #
#  Ryan Friedman                                #
#  7/13/2026                                    #
#################################################

library(tidyverse)
library(gt) # for static summary tables
library(fmsb) # for radarchart()
library(shiny)
library(shinyalert)
library(DT) # for interactive data tables

# helpers.R contains names of variables, useful functions, and other items
# also helpers.R reads CombinedData.rds
source("helpers.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

  titlePanel("Tennis Data Exploration"),
  sidebarLayout(
    sidebarPanel(
      h3("Choose a subset of the data:"),
      radioButtons(inputId = "tour_subset", label = "Tour", choiceNames = c("All", "Men's Tour", "Women's Tour"), choiceValues =  c("all", "M", "W"), selected = "all"),
      radioButtons(inputId = "handed_subset", label = "Handed", choiceNames = c("All", "Right-Handed", "Left-Handed"), choiceValues =  c("all", "R", "L"), selected = "all"),    
      checkboxGroupInput(inputId = "surface_subset",
                         label = "Surface",
                         choiceNames = c("Hard", "Clay", "Grass"),
                         choiceValues = c("Hard", "Clay", "Grass"),
                         selected = c("Hard", "Clay", "Grass")),
      checkboxGroupInput(inputId = "round_subset",
                         label = "Round",
                         choices = round_group_levels,
                         selected = round_group_levels),
      br(),
      # choose two numeric variables, with dynamic UI sliders
      selectizeInput(inputId = "num_subset_first", label = "First Numeric Variable", choices = subset_vars, selected = "year"),
      uiOutput("slider_first"),
      selectizeInput(inputId = "num_subset_second", label = "Second Numeric Variable", choices = subset_vars, selected = "total_pts"),
      uiOutput("slider_second"),
      br(),
      # action button to get the sample
      actionButton("sample_btn","Subset the Data!")
    ),
    mainPanel(
      tabsetPanel(
        id = "main_tabs",
        
        tabPanel("About",

        ),
        
        tabPanel("Data Download",
                 DT::dataTableOutput("data_table"),
                 downloadButton("download_data", "Download Data")
        ),
        
        tabPanel("Data Exploration",
                 tabsetPanel(
                   tabPanel("Categorical Summaries",
                            selectizeInput(inputId = "categorical_first", label = "First Categorical Variable", choices = categorical_vars, selected = "tour"),
                            selectizeInput(inputId = "categorical_second", label = "Second Categorical Variable", choices = c("None", categorical_vars), selected = "None"),
                            # Display Bar Graph (for 1 categorical)
                            plotOutput("bar_one"),
                            # Display Side-By-Side Bar Graph (for 2 categorical)
                            plotOutput("bar_two")
                            # Display Contingency Table (for 1 categorical)
                            # Display Two-Way Contingency Table (for 2 categorical)
                            ),
                   tabPanel("Numeric Summaries",
                            selectizeInput(inputId = "numeric_first", label = "First Numeric Variable", choices = numeric_vars, selected = "year"),
                            selectizeInput(inputId = "numeric_second", label = "Second Numeric Variable", choices = c("None", numeric_vars), selected = "None"),
                            selectizeInput(inputId = "numeric_group_by", label = "Group By Variable", choices = c("None", categorical_vars), selected = "None"),
                            # Display Box Plot (for 1 numerical)
                            plotOutput("box"),
                            # Display Histogram (for 1 numerical)
                            plotOutput("histogram"),
                            # Display Scatter Plot (for 2 numerical)
                            plotOutput("scatter"),
                            # Display Numeric Summary
                            ),
                   
                   tabPanel("Player Comparison", # since there are so many options for the player dropdown, this will be updated on the server side insead
                            selectizeInput(inputId = "first_player", label = "First Player", choices = NULL),
                            selectizeInput(inputId = "second_player", label = "Second Player", choices = NULL)
                            # Check for either player with n=0
                            # Display Radar Chart
                            # Display Player Metrics Table
                            # Warning if n is small
                            )
                            
                 )
        )
      )
    )
  )
)

#initialize mysubset with everything
mysubset <- CombinedData

# Define server logic
server <- function(input, output, session) {
  
  # Dynamic UI sliders
  output$slider_first <- renderUI({
    rng <- slider_ranges[[input$num_subset_first]] # gets min/max/step from the slider_ranges list in helpers.R
    sliderInput(inputId = "range_first",
                label = paste("Range for", names(subset_vars)[subset_vars == input$num_subset_first]), # label lookup from subset_vars in helpers.R
                min = rng$min,
                max = rng$max,
                value = c(rng$min, rng$max), # selection starts at full range
                step = rng$step) 
  })
  output$slider_second <- renderUI({
    rng <- slider_ranges[[input$num_subset_second]]
    sliderInput(inputId = "range_second",
                label = paste("Range for", names(subset_vars)[subset_vars == input$num_subset_second]),
                min = rng$min,
                max = rng$max,
                value = c(rng$min, rng$max),
                step = rng$step)
  })
  
  # Create subset on sample_btn
  mysubset <- eventReactive(input$sample_btn,{
    
    # Radio inputs
    if(input$tour_subset == "all") {tour_vals = c("M", "W")}
    else {tour_vals = input$tour_subset}
    if(input$handed_subset == "all") {handed_vals = c("R", "L")}
    else {handed_vals = input$handed_subset}    
    
    # subset 
    mysubset <- CombinedData |>
      filter(tour %in% tour_vals,
             handed %in% handed_vals,
             surface %in% input$surface_subset, # checkbox input for surface
             round_group %in% input$round_subset, # checkbox input for round_group
             .data[[input$num_subset_first]]  >= input$range_first[1],
             .data[[input$num_subset_first]]  <= input$range_first[2],
             .data[[input$num_subset_second]] >= input$range_second[1],
             .data[[input$num_subset_second]] <= input$range_second[2]
             )

  })
  
  #
  output$data_table <- renderDataTable(mysubset())
  
  # Update selectize for player selection, done on server due to qty of items.
  updateSelectizeInput(session, "first_player",
                       choices = eligible_players,
                       selected = "Roger Federer",
                       server = TRUE,
                       options = list(maxOptions = 2000))
  
  updateSelectizeInput(session, "second_player",
                       choices = c("None", eligible_players),
                       selected = "None",
                       server = TRUE,
                       options = list(maxOptions = 2000))

}

# Run the application 
shinyApp(ui = ui, server = server)
