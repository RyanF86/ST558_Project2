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
library(DT) # for interactive data tables
library(shinycssloaders) # for loading spinners

# helpers.R contains names of variables, useful functions, and other items
# also helpers.R reads CombinedData.rds
source("helpers.R")

# Define UI 
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
      selectizeInput(inputId = "num_subset_second", label = "Second Numeric Variable", choices = subset_vars, selected = "unforced"),
      uiOutput("slider_second"),
      br(),
      # action button to get the sample
      actionButton("sample_btn","Subset the Data!")
    ),
    mainPanel(
      tabsetPanel(
        id = "main_tabs",
        
        tabPanel("About",
                 
                 br(),
                 p("This application is used for exploring professional tennis match data."),
                 h2("About the Data:"),
                 br(),
                 #insert images
                 div(style = "display: flex; gap: 10px; justify-content: center;",
                     img(src = "ATP-Logo.png", height = "100px"),
                     img(src = "WTA-Logo.png", height = "100px")
                 ),
                 br(),
                 p("The dataset for this analysis comes from the Tennis Match Charting Project (MCP), available at the ",
                   a("Match Charting Project", # creating hyperlink
                     href = "https://github.com/JeffSackmann/tennis_MatchChartingProject",
                     target = "_blank"), # opens in new tab
                   " GitHub repository. The MCP aims to build a detailed public record of professional tennis matches (ATP and WTA Tours).",
                   "MCP data covers over 11,000 men's and women's singles matches including both match summary statistics and shot-by-shot detail. ",
                   "The project is open to the public both for accessing the data and for contributing new charted matches. This analysis uses only the match summary data, retrieved on 7/6/2026.  ",
                   "Note:  This is not a comprehensive dataset of all professional tennis matches. ",
                   "Because charting is done by individual volunteer contributors, the data tends to over-represent the biggest tournaments and the most popular players."
                 ),
                 
                 h3("Description of Variables:"),
                 p("Most variable names are self-explanatory to anyone familiar with the basics of tennis, so they are not all listed here. A few points are worth clarifying:"),
                 tags$ul( # bullet list
                   tags$li(
                     tags$b("Percentage variables"), 
                     " were created to normalize metrics across match length. Fifteen aces in a short match means something different than fifteen aces in a long one, so raw counts can mislead. For example, ",
                     tags$b("Ace %"), " is the share of a player's service points that were aces, and ",
                     tags$b("Forehand Winner %"), " is the share of total points that ended with the player hitting a forehand winner."
                   ),
                   tags$li(
                     tags$b("Aggression %"), 
                     " is a custom metric created for this analysis (not a standard tennis statistic). It is the sum of Winner % and Unforced Error %. A higher value indicates a more aggressive style."
                   ),
                   tags$li(
                     tags$b("Forehand Effectiveness %"), " and ", tags$b("Backhand Effectiveness %"), 
                     " are also custom metrics. Each representing Forehand/Backhand Winner % minus its Unforced Error %. A positive value means the shot produces more winners than errors."
                   )
                 ),
                 
                 h3("How to Use:"),
                 tags$ol( # numbered list
                   tags$li(
                     "Use the ", tags$b("sidebar"), " to subset the data. Select the desired categorical levels and numeric ranges, then click the ", 
                     tags$b("Subset the Data!"), " button to pull a subset from the 23,230 match summary results. All tabs will reflect the current subset and update when this button is clicked."
                   ),
                   tags$li(
                     "The ", tags$b("Data Download"), " tab displays a table of the subset. Click column headers to sort ascending or descending, and use the search box to filter further. Click the ", 
                     tags$b("Download Data"), " button at the bottom to save the table as a .csv file."
                   ),
                   tags$li(
                     "The ", tags$b("Data Exploration"), " tab investigates the subset in more depth, across three sub-tabs:",
                     tags$ul( # bullet list
                       tags$li(
                         tags$b("Categorical Summaries"), 
                         " — select one or two categorical variables to display bar charts and a counts table."
                       ),
                       tags$li(
                         tags$b("Numeric Summaries"), 
                         " — select one or two numeric variables, plus an optional categorical variable to group by. With one variable, a box plot and histogram are shown.  With two variables, a scatter plot is shown. A summary table of center and spread is shown below the plots."
                       ),
                       tags$li(
                         tags$b("Player Comparison"), 
                         " — select one or two players to display a radar chart and summary table of their average performance metrics across the subset."
                       )
                     )
                   )
                 ),
                 
                 h3("Other Notes:"),
                 tags$ul(
                   tags$li(
                     "The ", 
                     a("Isner–Mahut match at the 2010 Wimbledon Championships", 
                       href = "https://en.wikipedia.org/wiki/Isner%E2%80%93Mahut_match_at_the_2010_Wimbledon_Championships",
                       target = "_blank"),
                     " is an extreme outlier due to its length. The match had 980 total points, compared to 569 for the next-longest charted match. For practical reasons, the maximum ranges of the Total Points, Winners, and Aces sliders do not extend far enough to include it. The match can still appear in a subset if Year and Unforced Errors are used as the two numeric subsetting variables."
                   ),
                   tags$li(
                     "The Player Comparison tab averages each metric across all of a player's matches within the current subset. Averages built on only a few matches can be misleading. The number of charted matches per player is shown in the table below the radar chart, and a warning appears when a player has fewer than five."
                   ),
                   tags$li(
                     "Because the Match Charting Project is crowdsourced, the data over-represents the biggest tournaments and the most popular players. All summaries describe the charted sample, not the full population of professional tennis matches."
                   )
                 )                 
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
                              withSpinner(plotOutput("bar_one"))
                              ),
                            # Display Side-By-Side Bar Graph (for 2 categorical)
                            conditionalPanel(
                              condition = "input.categorical_second != 'None' && input.categorical_second != input.categorical_first",
                              withSpinner(plotOutput("bar_two"))
                              ),
                            # Display Contingency Table (for 1 categorical)
                            conditionalPanel(
                              condition = "input.categorical_second == 'None' | input.categorical_second == input.categorical_first",
                              br(),
                              gt_output("contingency_one")
                              ),
                            # Display Two-Way Contingency Table (for 2 categorical)
                            conditionalPanel(
                              condition = "input.categorical_second != 'None'",
                              br(),
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
                              withSpinner(plotOutput("box"))
                            ),
                            # Display Histogram (for 1 numerical), optional faceting
                            conditionalPanel(
                              condition = "input.numeric_second == 'None' | input.numeric_second == input.numeric_first",
                              br(),
                              withSpinner(plotOutput("histogram"))
                            ),                            
                            # Display Scatter Plot (for 2 numerical), optional coloring
                            conditionalPanel(
                              condition = "input.numeric_second != 'None' && input.numeric_second != input.numeric_first",
                              withSpinner(plotOutput("scatter"))
                            ),                            

                            # Display Numeric Summary
                            br(),
                            gt_output("numeric_summary")
                            ),
                   
                   tabPanel("Player Comparison", # since there are so many options for the player dropdown, this will be updated on the server side instead
                            selectizeInput(inputId = "first_player", label = "First Player", choices = NULL), # see updateSelectizeInput() on server
                            selectizeInput(inputId = "second_player", label = "Second Player", choices = NULL), # see updateSelectizeInput() on server
                            withSpinner(plotOutput("radar")), # display radar chart
                            br(),
                            uiOutput("radar_warning"), # display warnings, if applicable
                            br(),
                            gt_output("player_table") # display player table
                            )
                            
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
                step = rng$step,
                sep = "") # removes thousands comma
  })
  output$slider_second <- renderUI({
    rng <- slider_ranges[[input$num_subset_second]]
    sliderInput(inputId = "range_second",
                label = paste("Range for", names(subset_vars)[subset_vars == input$num_subset_second]),
                min = rng$min,
                max = rng$max,
                value = c(rng$min, rng$max),
                step = rng$step,
                sep = "") # removes thousands comma
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
  output$data_table <- DT::renderDataTable({
    tabledata <- mysubset()
    DT::datatable(tabledata, colnames = unname(var_labels[names(tabledata)])) |> # use var_labels for the column names
      DT::formatRound(columns = grep("_pct$", names(tabledata)), digits = 3) # cap at 3 decimals
  })
  
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
              legend.position = "none") # no legend needed
    }
  })

  # Histogram (w/ faceting option)
  output$histogram <- renderPlot({
    req(input$numeric_second == "None" | input$numeric_second == input$numeric_first)  # proceed if second variable is none -OR- both variables are the same
    histdata <- mysubset() |> drop_na(.data[[input$numeric_first]]) # drop na here, then use boxdata for both options 
    if (input$numeric_group_by == "None") { # simple histogram, no faceting
      ggplot(histdata, aes(x = .data[[input$numeric_first]])) +
        geom_histogram(bins = 30) + # 30 bin histogram
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
              legend.position = "none") # no legend needed
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
    numdata <- mysubset() # numdata passed below containing the subset
    validate(need(nrow(numdata) > 0, "No data matches the current filters.")) # confirm the subset has content, otherwise the table generates with "NaN" and "Inf" 
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
                            min    = ~ min(.x, na.rm = TRUE),
                            Q1     = ~ quantile(.x, 0.25, na.rm = TRUE),
                            Q3     = ~ quantile(.x, 0.75, na.rm = TRUE),
                            max    = ~ max(.x, na.rm = TRUE)),
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
               title = "Mean Metrics Across Match Subset")
    legend("topright", # build legend in top right
           legend = rownames(radardata)[-c(1, 2)],   # drop max/min rows
           col = c("red", "blue"), # legend colors
           lwd = 4, # legend line width
           bty = "n", # remove legend border box
           cex = 0.8) # scale legend text size
  })
  
  # Warning for small radar chart sample sizes (n < 5)
  output$radar_warning <- renderUI({
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
    
    if (length(msgs) == 0) return(NULL)   # nothing shows when no warning
    div(style = "background-color: #FFF3CD; padding: 8px; border-radius: 4px; border: 1px solid #FFE69C;", # adds yellow highlight
        HTML(paste(msgs, collapse = "<br>"))) 
  })
  
  # Player Data Table
  output$player_table <- render_gt({
    req(input$first_player) # proceed if first player has been chosen
    build_radar_table(mysubset(), input$first_player, input$second_player) |> # function from helpers.R
      gt() |>
      fmt_number(columns = -c(Player, `Charted Matches`), decimals = 3) |> # round to 3 decimals for select columns
      tab_header(title = "Mean Player Metrics",
                 subtitle = "Across Match Subset")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
