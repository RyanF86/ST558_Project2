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
      actionButton("corr_sample","Get a Sample!")
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
                            selectizeInput(inputId = "categorical_second", label = "Second Categorical Variable", choices = c("None", categorical_vars), selected = "None")
                            ),
                   tabPanel("Numeric Summaries",
                            selectizeInput(inputId = "numeric_first", label = "First Numeric Variable", choices = numeric_vars, selected = "year"),
                            selectizeInput(inputId = "numeric_second", label = "Second Numeric Variable", choices = c("None", numeric_vars), selected = "None"),
                            selectizeInput(inputId = "numeric_group_by", label = "Group By Variable", choices = c("None", categorical_vars), selected = "None"),                           
                            ),
                   
                   tabPanel("Player Comparison",
                            selectizeInput(inputId = "first_player", label = "First Player", choices = eligible_players, selected = "Roger Federer"))
                   
                 )
        )
      )
    )
  )
)



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

}

# Run the application 
shinyApp(ui = ui, server = server)
