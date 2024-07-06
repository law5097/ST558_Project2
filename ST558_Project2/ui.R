# ==================================================================================
# ui.R code
# ==================================================================================

# Define UI for the application
shinyUI(fluidPage(
  
  # App title
  titlePanel("ST 558 Project 2 - Lee Worthington"),
  
  # Create tabs
  tabsetPanel(
    id = "tabs",
    
    # Create the about tab
    tabPanel(
      "About",
      img(src = "logo.png", height = "100px"),  # Correct path
      
      h3("About the App"),
      p("This app allows users to:"),
      tags$ul(
        tags$li("Query U.S. Government Revenue Collections API"),
        tags$li("Define the filters sent to the API"),
        tags$li("Define the columns returned by the API"),
        tags$li("Download the data as a csv"),
        tags$li("Generate summary plots/statistics based on the selected data")
      ),
      p("Please note the following:"),
      tags$ul(
        tags$li("Because this app allows the user to query the API freely, certain combinations of filters may return an error as there is no data that meet the given criteria"),
        tags$li("Similarly, some combinations of filters may only have data in certain years"),
        tags$li("There is a 10000 record limit on the api, so if you try to return everything you'll only get the first 10000 records")
      ),
      
      h3("Data source"),
      p("Here's a brief description of the data from the treasury department:"),
      tags$ul(
        tags$li("The U.S. Government Revenue Collections dataset provides a daily overview of federal revenue collections such as individual and corporate income tax deposits, customs duties, fees for government service, fines, and loan repayments. These collections can be made through either electronic or non-electronic transactions by mail, internet, bank, or over-the-counter channels.")
      ),
      
      p("More information about this data can be found in the link below"),
      tags$ul(
        tags$li("https://fiscaldata.treasury.gov/datasets/revenue-collections-management/u-s-government-revenue-collections")
      ),
      
      h3("About the tabs"),
      p("There are 3 different tabs in this app, with the following purposes:"),
      tags$ul(
        tags$li("The about tab is here to give some introductionary info about the app, as well as links to resources to learn more about the data"),
        tags$li("The data selection tab allows you to query the API live with filters you specify, as well as download the data as a csv file. Any filters selected here will also apply to the plots in the data exploration tab."),
        tags$li("The data exploration tab provides some graphical summaries of the data you selected, the plots that are available will depend on which of the two data points you'd like to plot"),
      ),
    ),
    
    # Create the Data Selection tab
    tabPanel("Data Selection",
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
                        choices = c("Non-Tax", "IRS Non-Tax", "IRS Tax")
                      ),
                      
                      # Input for calendar year range
                      sliderInput("record_calendar_year", "Calendar Year Range", min = 2000, max = 2024, value = c(2000, 2024), step = 1),
                      
                      # Input for calendar month range
                      sliderInput("record_calendar_month", "Calendar Month Range", min = 1, max = 12, value = c(1, 12), step = 1),
                      
                      # Input for number of rows
                      numericInput("rows", "Number of Rows to Return (max 10000)", value = 10000, min = 1, max = 10000),
                      
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
                      p("Note: 'record_date' and 'net_collections_amt' are always selected."),
                      
                      # Button to download the data
                      downloadButton("download_data", "Download Data")
               ),
               column(10,
                      # Note about no data being displayed
                      p("Note: If no data is displayed or an error is returned, it means no data meets the filter criteria."),
                      
                      # Show data table
                      dataTableOutput("data_table")
               )
             )
    ),
    
    # Create the data exploration tab
    tabPanel("Data Exploration",
             selectInput("summary_var", "Variable to Summarize", choices = c("Net Collections Amount" = "net_collections_amt", "Count of Records" = "count")),
             selectInput("plot_type", "Plot Type", choices = c("Histogram", "Boxplot", "Line Plot", "Heatmap")),
             plotOutput("plot"),
             h4("Numeric Summaries"),
             verbatimTextOutput("numeric_summary"),
             h4("Contingency Tables"),
             verbatimTextOutput("contingency_table")
    )
  )
))



