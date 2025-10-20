#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
#install.packages("shiny")

library(shiny)
library(dplyr)
library(ggplot2)

###############################################################################
# Sample retail sales data
set.seed(123)
retail_data <- data.frame(
  Date = rep(seq.Date(as.Date("2023-01-01"), as.Date("2023-01-31"), by = "day"), 3),
  Category = rep(c("Electronics", "Clothing", "Groceries"), each = 31),
  Sales = round(runif(31*3, min = 1000, max = 5000), 0) # random uniform data
)

# UI
ui <- fluidPage(
  titlePanel("Retail Sales Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("category", 
                  "Select Product Category:",
                  choices = unique(retail_data$Category),
                  selected = "Electronics")
    ),
    
    mainPanel(
      h4("Summary Statistics"),
      verbatimTextOutput("summary"),
      
      h4("Sales Trend Plot"),
      plotOutput("salesPlot")
    )
  )
)

# Server: Define server logic required to plot
server <- function(input, output) {
  
  filtered_data <- reactive({
    retail_data %>%
      filter(Category == input$category)
  })
  
  output$summary <- renderPrint({
    summary(filtered_data()$Sales)
  })
  
  output$salesPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = Date, y = Sales)) +
      geom_line(color = "steelblue", size = 1) +
      geom_point(color = "darkblue") +
      labs(title = paste("Sales Trend -", input$category),
           x = "Date", y = "Sales (USD)") +
      theme_minimal()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

###############################################################################
# Varsity Tutors experience
#install.packages("readr")
library(readr)
df <- read_csv("varsitytutors.csv")
df

# install.packages("lubridate")
library(lubridate)

# Combine into POSIXct datetime
df$datetime <- make_datetime(
  year = df$year,
  month = df$month,
  day = df$day,
  hour = df$hr,
  min = df$min,
  sec = 0
)

# UI
ui <- fluidPage(
  titlePanel("Varsity Tutors: Lesson Duration Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("subject", 
                  "Select Subject:",
                  choices = unique(df$subject),
                  selected = "Calculus 2")
    ),
    
    mainPanel(
      h4("Summary Statistics"),
      verbatimTextOutput("summary"),
      
      h4("Lesson Duration Trend Plot"),
      plotOutput("durationPlot")
    )
  )
)

# Server: Define server logic required to plot
server <- function(input, output) {
  
  filtered_data <- reactive({
    df %>%
      filter(subject == input$subject)
  })
  
  output$summary <- renderPrint({
    summary(filtered_data()$duration)
  })
  
  output$durationPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = datetime, y = duration)) +
      geom_line(color = "steelblue", size = 1) +
      geom_point(color = "darkblue") +
      labs(title = paste("Lesson Duration vs Time Trend -", input$subject),
           x = "date", y = "duration (min)") +
      theme_minimal()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
