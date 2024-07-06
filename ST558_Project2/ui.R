library(shiny)

# Define UI for the application
shinyUI(fluidPage(
  
  # App title
  titlePanel("ST 558 Project 2"),
  
  # Create tabs
  tabsetPanel(
    id = "tabs",
    
    # Create the about tab
    tabPanel("About",
             h3("About the App"),
             p("This app allows users to query and summarize data from the U.S. Government Revenue Collections API."),
             p("The app consists of three tabs: About, Data Download, and Data Exploration."),
             img(src = "logo.png", height = "100px")
    ),
    
    # Create the data download tab
    tabPanel("Data Download",
             fluidRow(
               column(2,
                      # Drop down options for electronic category description
                      selectInput(
                        "electronic_category_desc", 
                        "Electronic Category Description",
                        choices = c("Electronic Settlement", "Fully Electronic - All", 
                                    "Fully Electronic - FS", "Non-Electronic")
                      ),
                      
                      # Drop down options for channel type description
                      selectInput(
                        "channel_type_desc", 
                        "Channel Type Description",
                        choices = c("Mail", "Bank", "Internet", "Over-the-Counter (OTC)")
                      ),
                      
                      # Drop down options for tax category description
                      selectInput(
                        "tax_category_desc", 
                        "Tax Category Description",
                        choices = c("IRS Non-Tax", "IRS Tax", "Non-Tax")
                      ),
                      
                      # Input for calendar year range
                      sliderInput("record_calendar_year", "Calendar Year Range", min = 2000, max = 2024, value = c(2000, 2024), step = 1),
                      
                      # Input for calendar month range
                      sliderInput("record_calendar_month", "Calendar Month Range", min = 1, max = 12, value = c(1, 12), step = 1),
                      
                      # Input for number of rows
                      numericInput("rows", "Number of Rows to Return", value = 1000, min = 1),
                      
                      # Column selection checkboxes
                      checkboxGroupInput("columns", "Select Columns to Display", 
                                         choices = c("Electronic Category Description" = "electronic_category_desc", 
                                                     "Channel Type Description" = "channel_type_desc", 
                                                     "Tax Category Description" = "tax_category_desc", 
                                                     "Record Fiscal Year" = "record_fiscal_year", 
                                                     "Record Calendar Year" = "record_calendar_year", 
                                                     "Record Calendar Month" = "record_calendar_month"),
                                         selected = c("electronic_category_desc", "channel_type_desc", "tax_category_desc", "record_fiscal_year", "record_calendar_year", "record_calendar_month")),
                      # Message about mandatory columns
                      p("Note: 'record_date' and 'net_collections_amt' are always selected.")
                      ,
                      # Button to query the data
                      actionButton("query_data", "Query Data"),
                      
                      # Button to download the data
                      downloadButton("download_data", "Download Data")
               ),
               column(10,
                      # Show data table
                      dataTableOutput("data_table")
               )
             )
    ),
    
    # Create the data exploration tab
    tabPanel("Data Exploration",
             selectInput("summary_var", "Variable to Summarize", choices = c("Net Collections Amount" = "net_collections_amt", "Count of Records" = "count")),
             selectInput("plot_type", "Plot Type", choices = c("Histogram", "Boxplot", "Line Plot", "Heatmap")),
             plotOutput("plot")
    )
  )
))





