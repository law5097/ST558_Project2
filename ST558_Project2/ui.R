library(shiny)

# Define UI for the application
shinyUI(fluidPage(
  titlePanel("ST 558 Project 2"),
  
  tabsetPanel(
    id = "tabs",
    tabPanel("About",
             h3("About the App"),
             p("This app allows users to query and summarize data from the U.S. Government Revenue Collections API."),
             p("The app consists of three tabs: About, Data Download, and Data Exploration."),
             img(src = "logo.png", height = "100px")
    ),
    tabPanel("Data Download",
             fluidRow(
               column(6,
                      selectInput("electronic_category_desc", "Electronic Category Description",
                                  choices = c("Electronic Settlement", "Fully Electronic - All", 
                                              "Fully Electronic - FS", "Non-Electronic")),
                      selectInput("channel_type_desc", "Channel Type Description",
                                  choices = c("Bank", "Internet", "Mail", "Over-the-Counter (OTC)")),
                      selectInput("tax_category_desc", "Tax Category Description",
                                  choices = c("IRS Non-Tax", "IRS Tax", "Non-Tax")),
                      numericInput("record_calendar_year", "Calendar Year", value = 2020, min = 2000, max = 2024),
                      numericInput("record_calendar_month", "Calendar Month", value = 1, min = 1, max = 12),
                      numericInput("rows", "Number of Rows to Return", value = 1000, min = 1),
                      actionButton("query_data", "Query Data"),
                      downloadButton("download_data", "Download Data")
               ),
               column(6,
                      h4("Sample Data"),
                      dataTableOutput("sample_data_table")
               )
             ),
             dataTableOutput("data_table")
    ),
    tabPanel("Data Exploration",
             selectInput("summary_var", "Variable to Summarize", choices = c("tax_category_desc", "channel_type_desc")),
             selectInput("plot_type", "Plot Type", choices = c("Histogram", "Boxplot", "Line Plot", "Heatmap")),
             plotOutput("plot")
    )
  )
))



