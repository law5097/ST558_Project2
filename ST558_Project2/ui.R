# ==================================================================================
# ui.R code
# ==================================================================================

# Load shiny
library(shiny)

# Fluid page code
shinyUI(fluidPage(
  
  # Background color
  tags$style(HTML("
    body {
      background-color: #fcfcfc;
    }
  ")),
  
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
        tags$li("Generate summary plots/statistics based on user selections")
      ),
      p("Please note the following:"),
      tags$ul(
        tags$li("Because this app allows the user to query the API freely, certain combinations of filters may return an error as there is no data that meet the given criteria"),
        tags$li("Similarly, some combinations of filters may only have data in certain years"),
        tags$li("There is a 10000 record limit on the api, so if you try to return everything you'll only get the first 10000 records"),
        tags$li("There are some additional variables available in the source data that I did not include in this app, such as the IDs")
      ),
      
      h3("Data source"),
      p("Here's a brief description of the data from the treasury department:"),
      tags$ul(
        tags$li("The U.S. Government Revenue Collections dataset provides a daily overview of federal revenue collections such as individual and corporate income tax deposits, customs duties, fees for government service, fines, and loan repayments. These collections can be made through either electronic or non-electronic transactions by mail, internet, bank, or over-the-counter channels."),
        tags$li("More information about this data can be found in this link: https://fiscaldata.treasury.gov/datasets/revenue-collections-management/u-s-government-revenue-collections")
      ),
      
      h3("About the tabs"),
      p("There are 3 different tabs in this app, with the following purposes:"),
      tags$ul(
        tags$li("The about tab is here to give some introductionary info about the app, as well as links to resources to learn more about the data"),
        tags$li("The data selection tab allows you to query the API live with filters you specify, as well as download the data as a csv file. Any filters selected here will also apply to the plots in the data exploration tab."),
        tags$li("The data exploration tab provides some graphical summaries of the data you selected, the plots that are available will depend on which of the two data points you'd like to plot and the histogram can be faceted"),
      )
    ),
    
    # Create the Data Selection tab
    tabPanel(
      "Data Selection",
      fluidRow(
        column(
          2,
          # Drop down options for electronic category description
          checkboxGroupInput(
            "electronic_category_desc", 
            "Electronic Category Description",
            choices = c("Electronic Settlement", "Fully Electronic - All", 
                        "Fully Electronic - FS", "Non-Electronic")
          ),
          
          # Drop down options for channel type description
          checkboxGroupInput(
            "channel_type_desc", 
            "Channel Type Description",
            choices = c("Mail", "Bank", "Internet", "Over-the-Counter (OTC)")
          ),
          
          # Drop down options for tax category description
          checkboxGroupInput(
            "tax_category_desc", 
            "Tax Category Description",
            choices = c("IRS Non-Tax", "IRS Tax", "Non-Tax")
          ),
          
          # Input for calendar year range
          sliderInput("record_calendar_year", "Calendar Year Range", min = 2000, max = 2024, value = c(2000, 2024), step = 1),
          
          # Input for calendar month range
          sliderInput("record_calendar_month", "Calendar Month Range", min = 1, max = 12, value = c(1, 12), step = 1),
          
          # Input for number of rows
          numericInput("rows", "Number of Rows to Return (max 10000)", value = 500, min = 1, max = 10000),
          
          # Column selection checkboxes
          checkboxGroupInput(
            "columns", 
            "Select Columns", 
            choices = c("Electronic Category Description" = "electronic_category_desc", 
                        "Channel Type Description" = "channel_type_desc", 
                        "Tax Category Description" = "tax_category_desc", 
                        "Record Calendar Year" = "record_calendar_year", 
                        "Record Calendar Month" = "record_calendar_month"),
            selected = c("electronic_category_desc", "channel_type_desc", "tax_category_desc", "record_calendar_year", "record_calendar_month")),
          
          # Message about mandatory columns
          p("Note: 'record_date' and 'net_collections_amt' are always selected."),
          
          # Button to download the data
          downloadButton("download_data", "Download Data")
        ),
        column(
          
          # Width
          10,
          
          # Note about no data being displayed
          p("Note: It may take a second for the data to load. If no data is displayed or an error is returned it means no data meets the filter criteria or there's a problem with the API host, try changing your filters or restarting R to fix this"),
          
          # Show data table
          tableOutput("data_table")
        )
      )
    ),
    
    # Create the data exploration tab
    tabPanel(
      "Data Exploration",
      fluidRow(
        
        # Variable selection
        column(4,
               selectInput("summary_var", "Variable to Summarize", choices = c("Net Collections Amount" = "net_collections_amt", "Count of Records" = "count"))
        ),
        
        # Dynamic UI for plot type, changes based on select variable
        column(4,
               uiOutput("plot_type_ui") 
        ),
        
        # Selection of secondary variable
        column(4,
               selectInput("contingency_var", "Variable to Partition by", 
                           choices = c("Electronic Category Description" = "electronic_category_desc", 
                                       "Channel Type Description" = "channel_type_desc", 
                                       "Tax Category Description" = "tax_category_desc",
                                       "Record Calendar Year" = "record_calendar_year",
                                       "Record Calendar Month" = "record_calendar_month"))
        )
      ),
      
      # Condition y-axis choices based on plot type and variable selection
      conditionalPanel(
        condition = "input.plot_type == 'Heat map'",
        uiOutput("y_axis_var_ui")
      ),
      
      # Give a faceting option for the histogram
      conditionalPanel(
        condition = "input.plot_type == 'Histogram'",
        checkboxInput("facet_histogram", "Facet by selected partition variable", value = FALSE)
      ),
      
      # Generate plot
      plotOutput("plot"),
      
      # Drop down for summary type selection and second variable selection
      fluidRow(
        
        # Main drop down
        column(4,
               h4("Summary Tables"),
               selectInput("summary_type", "Type of Summary", choices = c("Mean/SD" = "mean_sd", "Percentiles" = "percentiles", "Contingency Table" = "contingency_table"))
        ),
        
        # Dynamic drop down dependent on summary selection
        column(4,
               h4("."),
               uiOutput("second_var_ui")  
        )
      ),
      
      # Output selected summary type
      verbatimTextOutput("summary_output")
    )
  )
))

