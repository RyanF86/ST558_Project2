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
                            conditionalPanel(
                              condition = "input.categorical_second == 'None' | input.categorical_second == input.categorical_first",
                              plotOutput("bar_one")
                              ),
                            # Display Side-By-Side Bar Graph (for 2 categorical)
                            conditionalPanel(
                              condition = "input.categorical_second != 'None' && input.categorical_second != input.categorical_first",
                              plotOutput("bar_two")
                              ),
                            # Display Contingency Table (for 1 categorical)
                            conditionalPanel(
                              condition = "input.categorical_second == 'None' | input.categorical_second == input.categorical_first",
                              gt_output("contingency_one")
                              ),
                            # Display Two-Way Contingency Table (for 2 categorical)
                            conditionalPanel(
                              condition = "input.categorical_second != 'None'",
                              gt_output("contingency_two")
                              )
                            ),
                   tabPanel("Numeric Summaries",
                            selectizeInput(inputId = "numeric_first", label = "First Numeric Variable", choices = numeric_vars, selected = "year"),
                            selectizeInput(inputId = "numeric_second", label = "Second Numeric Variable", choices = c("None", numeric_vars), selected = "None"),
                            selectizeInput(inputId = "numeric_group_by", label = "Group By Variable", choices = c("None", categorical_vars), selected = "None"),
                            # Display Box Plot (for 1 numerical), optional side-by-side
                            conditionalPanel(
                              condition = "input.numeric_second == 'None' | input.numeric_second == input.numeric_first",
                              plotOutput("box")
                            ),
                            # Display Histogram (for 1 numerical), optional faceting
                            conditionalPanel(
                              condition = "input.numeric_second == 'None' | input.numeric_second == input.numeric_first",
                              plotOutput("histogram")
                            ),                            
                            # Display Scatter Plot (for 2 numerical), optional coloring
                            conditionalPanel(
                              condition = "input.numeric_second != 'None' && input.numeric_second != input.numeric_first",
                              plotOutput("scatter")
                            ),                            

                            # Display Numeric Summary
                            gt_output("numeric_summary")
                            
                            ),
                   
                   tabPanel("Player Comparison", # since there are so many options for the player dropdown, this will be updated on the server side instead
                            selectizeInput(inputId = "first_player", label = "First Player", choices = NULL), # see updateSelectizeInput() on server
                            selectizeInput(inputId = "second_player", label = "Second Player", choices = NULL), # see updateSelectizeInput() on server
                            plotOutput("radar"), # display radar chart
                            textOutput("radar_warning"), # display warnings, if applicable
                            gt_output("player_table") # display player table
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
  
  # Data Table
  output$data_table <- renderDataTable(mysubset())
  
  # Download Data
  output$download_data <- downloadHandler(
    filename = "download.csv",
    content = function(file) {
      # Write the dataset to the `file` that will be downloaded
      write.csv(mysubset(), file)
    }
  )  
  
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
  
  # Bar Plot, Single Variable
  output$bar_one <- renderPlot({
    req(input$categorical_second == "None" | input$categorical_second == input$categorical_first) # proceed if second variable is none -OR- both variables are the same
    ggplot(data = mysubset() |> drop_na(.data[[input$categorical_first]]), 
           aes(x = .data[[input$categorical_first]], 
               fill = .data[[input$categorical_first]])) +
      geom_bar() +
      labs(x = var_labels[[input$categorical_first]], # reference corresponding label
           title = paste("Count of Charted Matches by", var_labels[[input$categorical_first]])) + 
      theme(plot.title = element_text(hjust = 0.5), # centering the title
            legend.position = "none") # remove redundant legend
  })
  
  # Side-By-Side Bar Plot
  output$bar_two <- renderPlot({
    req(input$categorical_second != "None" && input$categorical_second != input$categorical_first) # proceed if second variable is selected -AND- both variables are different
    ggplot(data = mysubset() |> drop_na(.data[[input$categorical_first]], .data[[input$categorical_second]]), 
           aes(x = .data[[input$categorical_first]], 
               fill = .data[[input$categorical_second]])) +
    geom_bar(position = "dodge") +
      labs(x = var_labels[[input$categorical_first]], 
           title = paste("Count of Charted Matches by", var_labels[[input$categorical_first]], "and", var_labels[[input$categorical_second]])) + 
      scale_fill_discrete(var_labels[[input$categorical_second]]) +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  # One-Way Contingency Table
  output$contingency_one <- render_gt({
    req(input$categorical_second == "None" | input$categorical_second == input$categorical_first) # proceed if second variable is none -OR- both variables are the same
    mysubset() |>
      drop_na(.data[[input$categorical_first]]) |>
      group_by(.data[[input$categorical_first]]) |>
      summarize(count = n()) |>
      set_names(c(var_labels[[input$categorical_first]], "Count")) |> # uses the corresponding label      
      gt() |>
      tab_header(title = paste("Count by", var_labels[[input$categorical_first]]))
  })
  
  # Two-Way Contingency Table
  output$contingency_two <- render_gt({
    req(input$categorical_second != "None" && input$categorical_second != input$categorical_first) # proceed if second variable is selected -AND- both variables are different
    mysubset() |>
      drop_na(.data[[input$categorical_first]], .data[[input$categorical_second]]) |>
      group_by(.data[[input$categorical_first]], .data[[input$categorical_second]]) |>
      summarize(count = n(), .groups = "drop") |>
      pivot_wider(names_from = !!sym(input$categorical_second), values_from = count) |>
      gt(rowname_col = input$categorical_first) |>
      tab_header(title = paste(var_labels[[input$categorical_second]], "by", var_labels[[input$categorical_first]]))
  }) 

  # Box Plot (w/ side-by-side option)
  output$box <- renderPlot({
    req(input$numeric_second == "None" | input$numeric_second == input$numeric_first)  # proceed if second variable is none -OR- both variables are the same
    boxdata <- mysubset() |> drop_na(.data[[input$numeric_first]]) # drop na here, then use boxdata for both options
    if (input$numeric_group_by == "None") { # simple box plot with no group by
      ggplot(boxdata, aes(y = .data[[input$numeric_first]])) +
        geom_boxplot() +
        labs(y = var_labels[[input$numeric_first]],
             title = var_labels[[input$numeric_first]]) +
        theme(plot.title = element_text(hjust = 0.5))
    } else { # group by results in side-by-side box plot
      ggplot(boxdata |> drop_na(.data[[input$numeric_group_by]]), 
             aes(x = .data[[input$numeric_group_by]], 
                 y = .data[[input$numeric_first]], 
                 fill = .data[[input$numeric_group_by]])) +
        geom_boxplot() +
        labs(x = var_labels[[input$numeric_group_by]],
             y = var_labels[[input$numeric_first]],
             title = paste(var_labels[[input$numeric_first]], "by", 
                           var_labels[[input$numeric_group_by]])) +
        theme(plot.title = element_text(hjust = 0.5),
              legend.position = "none")
    }
  })

  # Histogram (w/ faceting option)
  output$histogram <- renderPlot({
    req(input$numeric_second == "None" | input$numeric_second == input$numeric_first)  # proceed if second variable is none -OR- both variables are the same
    histdata <- mysubset() |> drop_na(.data[[input$numeric_first]]) # drop na here, then use boxdata for both options 
    if (input$numeric_group_by == "None") { # simple histogram, no faceting
      ggplot(histdata, aes(x = .data[[input$numeric_first]])) +
        geom_histogram(bins = 30) +
        labs(x = var_labels[[input$numeric_first]],
             title = var_labels[[input$numeric_first]]) +
        theme(plot.title = element_text(hjust = 0.5))
    } else { # group by results in faceted histogram
      ggplot(histdata |> drop_na(.data[[input$numeric_group_by]]), 
             aes(x = .data[[input$numeric_first]], 
                 fill = .data[[input$numeric_group_by]])) +
        geom_histogram(bins = 30, alpha = 0.5) + 
        facet_wrap(vars(.data[[input$numeric_group_by]])) +
        labs(x = var_labels[[input$numeric_first]],
             fill = var_labels[[input$numeric_group_by]],
             title = var_labels[[input$numeric_first]],
             subtitle = paste("Faceted by", var_labels[[input$numeric_group_by]])) + 
        theme(plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5),
              legend.position = "none")
    }
  })
  
  # Scatter Plot (w/ coloring option)
  output$scatter <- renderPlot({
    req(input$numeric_second != "None", input$numeric_second != input$numeric_first)  # proceed if second variable is selected -AND- both variables are different
    scatterdata <- mysubset() |> drop_na(.data[[input$numeric_first]], .data[[input$numeric_second]]) # drop na here, then use boxdata for both options 
    if (input$numeric_group_by == "None") { # simple scatter, no coloring
      ggplot(scatterdata, 
             aes(x = .data[[input$numeric_second]], 
                 y = .data[[input$numeric_first]])) +
        geom_point(alpha = 0.5, size = 0.9) +
        labs(x = var_labels[[input$numeric_second]],
             y = var_labels[[input$numeric_first]],
             title = paste(var_labels[[input$numeric_first]], "vs", 
                           var_labels[[input$numeric_second]])) +
        theme(plot.title = element_text(hjust = 0.5))
    } else { # group by results in colored scatter
      ggplot(scatterdata |> drop_na(.data[[input$numeric_group_by]]), 
             aes(x = .data[[input$numeric_second]], 
                 y = .data[[input$numeric_first]], 
                 color = .data[[input$numeric_group_by]])) +
        geom_point(alpha = 0.5, size = 0.9) +
        labs(x = var_labels[[input$numeric_second]],
             y = var_labels[[input$numeric_first]],
             color = var_labels[[input$numeric_group_by]],
             title = paste(var_labels[[input$numeric_first]], "vs", 
                           var_labels[[input$numeric_second]], "by",
                           var_labels[[input$numeric_group_by]])) +
        theme(plot.title = element_text(hjust = 0.5))
    }
  })
  
  # Numeric Summary Table
  output$numeric_summary <- render_gt({
    # num_vars will contain of list of which numeric variables to summarize (1 or 2)
    num_vars <- input$numeric_first
    if (input$numeric_second != "None" && input$numeric_second != input$numeric_first) {
      num_vars <- c(num_vars, input$numeric_second)
    }
    numdata <- mysubset()
    if (input$numeric_group_by != "None") { # apply grouping only if a group variable is selected
      numdata <- numdata |> 
        drop_na(.data[[input$numeric_group_by]]) |>
        group_by(.data[[input$numeric_group_by]])
    }
    summary_tbl <- numdata |>
      summarize(across(.cols = all_of(num_vars),
                       list(mean   = ~ mean(.x, na.rm = TRUE),
                            median = ~ median(.x, na.rm = TRUE),
                            sd     = ~ sd(.x, na.rm = TRUE),
                            Q1     = ~ quantile(.x, 0.25, na.rm = TRUE),
                            Q3     = ~ quantile(.x, 0.75, na.rm = TRUE)),
                       .names = "{.fn}.{.col}"),
                .groups = "drop") |> # removes grouping if present
      pivot_longer(cols = contains("."),
                   names_to = c("Statistic", "Variable"),
                   names_sep = "\\.") |>
      pivot_wider(names_from = Statistic, values_from = value) |>
      mutate(Variable = var_labels[Variable])   # convert column names to display labels
    # build the gt, grouping only if applicable
    if (input$numeric_group_by != "None") {
      summary_tbl |>
        gt(groupname_col = input$numeric_group_by, rowname_col = "Variable") |>
        fmt_number(decimals = 3)
    } else {
      summary_tbl |>
        gt(rowname_col = "Variable") |>
        fmt_number(decimals = 3)
    }
  })
  
  # Get player match counts for use later in output$radar and output$radar_warning
  player_counts <- reactive({
    playercountdata <- mysubset()
    n_first  <- sum(playercountdata$player == input$first_player, na.rm = TRUE) # grab sum of first player's matches
    n_second <- if (input$second_player == "None") NA else # 
      sum(playercountdata$player == input$second_player, na.rm = TRUE) # grab sum of second player's matches
    list(first = n_first, second = n_second)
  })
  
  # Radar Chart
  output$radar <- renderPlot({
    req(input$first_player) # proceed if first player has been chosen
    counts <- player_counts()
    validate(
      need(counts$first > 0, # check if there is no data for first player
           paste(input$first_player, "has no charted matches under the current filters.")) 
    )
    if (input$second_player != "None") { # if second player has been chosen
      validate(
        need(counts$second > 0, # check if there is no data for second player
             paste(input$second_player, "has no charted matches under the current filters."))
      )
    }
    # Get data for the radar chart, using function defined in helpers.R
    radardata <- build_radar_data(mysubset(), input$first_player, input$second_player)
    # Build the radar chart
    par(mar = c(1, 1, 1, 1)) # narrow margins for less white space
    radarchart(radardata,
               vlcex = 0.8, # scales label text size
               pcol = c("red", "blue"), # applies colors
               pfcol = c(adjustcolor("red", alpha.f = 0.3), # applies fill colors with transparency
                         adjustcolor("blue", alpha.f = 0.3)), 
               plwd = 2, # line width
               plty = 1, # solid lines
               cglcol = "grey", # grid line color
               cglty = 1, # solid grid line
               cglwd = 0.5, # grid line width
               title = "Mean Metrics Across All Charted Matches")
    legend("topright", # build legend in top right
           legend = rownames(radardata)[-c(1, 2)],   # drop max/min rows
           col = c("red", "blue"), # legend colors
           lwd = 4, # legend line width
           bty = "n", # remove legend border box
           cex = 0.8) # scale legend text size
  })
  
  # Warning for small radar chart sample sizes (n < 5)
  output$radar_warning <- renderText({
    req(input$first_player) # proceed if first player has been chosen
    counts <- player_counts()
    msgs <- character(0) # msgs initialized
    if (counts$first > 0 && counts$first < 5) {
      msgs <- c(msgs, paste0(input$first_player, " has only ", counts$first, 
                             " charted match(es) under the current filters — interpret with caution."))
    }
    if (input$second_player != "None" && !is.na(counts$second) && 
        counts$second > 0 && counts$second < 5) {
      msgs <- c(msgs, paste0(input$second_player, " has only ", counts$second, 
                             " charted match(es) under the current filters — interpret with caution."))
    }
    msgs # assembles and returns the message
  })
  
  # Player Data Table
  output$player_table <- render_gt({
    req(input$first_player) # proceed if first player has been chosen
    build_radar_table(mysubset(), input$first_player, input$second_player) |> # function from helpers.R
      gt() |>
      fmt_number(columns = -c(Player, `Charted Matches`), decimals = 3) |> # round to 3 decimals for select columns
      tab_header(title = "Mean Player Metrics",
                 subtitle = "Across All Charted Matches")
  })
  
}



# Run the application 
shinyApp(ui = ui, server = server)
