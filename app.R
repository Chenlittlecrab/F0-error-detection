library(shiny)
library(ggplot2)
library(tidyverse)
library(dplyr)

#### this script allows loading and plotting of each utterance produced by speaker gd67 ############
#### this version allows the user to modify the limits of x and y axis

# Load your data and define df_utr
setwd("/Users/chenz/Documents/CUNY/QP1/Stats/modeling/pitch_range_data/shiny_tutorial/f0_attempt4_manual/data/test_data")


df_utr <- read.csv("10_sample_err_detect.csv") %>%
  filter(Filename == "gd69_a_67.mat") %>%
  filter(strF0 < 500) %>% 
  rename(f0_manual = f0_keep)
head(df_utr)



# Define UI
ui <- fluidPage(
  
  fluidRow(
    
    column(width = 4,
      sliderInput(
        inputId = "x_lim",
        label = "Set min and max for x axis",
        value = c(0, 3000),
        min = 0,
        max = 6300,
        step = 100
      )
    ),
    
    column(width = 4,
      sliderInput(
      inputId = "y_lim",
      label = "set min and max for y axis",
      value = c(0, 500),
      min = 0,
      max = 500,
      step = 2
      )
    )
    
    

  ),
  
  fluidRow(
    column(width = 12,
           plotOutput("plot1", height = 300,
                      click = "plot1_click",
                      brush = brushOpts(
                        id = "plot1_brush"
                      ),
           ),
           actionButton("exclude_toggle", "Toggle points"),
           actionButton("exclude_reset", "Reset")
    )
  ),
  
   
    # add an action button to download the file
  
    actionButton(inputId = "write_csv", 
                 label = "download the CSV file")
  )


server <- function(input, output) {

  
  # For storing which rows have been excluded
  vals <- reactiveValues(
    keeprows = rep(TRUE, nrow(df_utr))
  )
  
  # set xlim as reactive
  x_val <- reactive({
    input$x_lim
  })
  
  y_val <- reactive({
    input$y_lim
  })
  
  output$plot1 <- renderPlot({
    
    # Plot the kept and excluded points as two separate data sets
    keep    <- df_utr[ vals$keeprows, , drop = FALSE]
    exclude <- df_utr[!vals$keeprows, , drop = FALSE]
    
    ggplot(keep, aes(t_ms, strF0, color = f0_manual)) + geom_point() +
      geom_point(data = exclude, shape = 21, fill = NA, color = "black", alpha = 0.25) +
      theme_minimal() +
      coord_cartesian(xlim = x_val(), ylim = y_val()) +
      labs(title = "gd43_a_10")
  })
  
  # Toggle points that are clicked
  observeEvent(input$plot1_click, {
    res <- nearPoints(df_utr, input$plot1_click, allRows = TRUE)
    
    vals$keeprows <- xor(vals$keeprows, res$selected_)
  })
  
  # Toggle points that are brushed, when button is clicked
  observeEvent(input$exclude_toggle, {
    res <- brushedPoints(df_utr, input$plot1_brush, allRows = TRUE)
    
    vals$keeprows <- xor(vals$keeprows, res$selected_)
  })
  
  # Reset all points
  observeEvent(input$exclude_reset, {
    vals$keeprows <- rep(TRUE, nrow(df_utr))
  })
  
  # a dataframe flagging the errors
  df_clean <- eventReactive(
    c(input$exclude_toggle,
      input$exclude_reset,
      input$plot1_click),
    {
      keep <- df_utr[ vals$keeprows, , drop = FALSE] %>% 
        add_column(f0_keep = TRUE)
      exclude <- df_utr[!vals$keeprows, , drop = FALSE] %>% 
        add_column(f0_keep = FALSE)
      bind_rows(keep, exclude)
      
    }
    
  )
  
  # download the subfile (this is for future use)
  # later I may write this as download ONLY when
  # changes have been made to the subfile()
  observeEvent(input$write_csv,{
    csv_name <- unique(df_utr$Filename)
    filename <- paste0(
      "data/test_data/",
      substring(csv_name, 1, nchar(csv_name)-4), ".csv")
    write_csv(df_clean(), filename)
  })
  
  
}

shinyApp(ui, server)
