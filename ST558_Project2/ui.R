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
        tags$li("Query the U.S. Government Revenue Collections API"),
        tags$li("Define the filters to modify the API endpoint and retrieve data"),
        tags$li("Select columns and rows from the data"),
        tags$li("Download the data as a csv"),
        tags$li("Generate summary plots/statistics based on user selections")
      ),
      p("Please note the following:"),
      tags$ul(
        tags$li("Because this app allows the user to query the API freely, certain combinations of filters may return an error as there is no data that meet the given criteria"),
        tags$li("Similarly, some combinations of filters may only have data in certain years, certain quarters, etc"),
        tags$li("There is a 10000 record limit on the API, so if you try to query more than that you'll only get the first 10000 records"),
        tags$li("There are some additional variables available in the source data that I did not include in this app, such as the IDs")
      ),
      
      h3("Data source"),
      p("Here's a brief description of the data from the treasury department and a link to learn more:"),
      tags$ul(
        tags$li("The U.S. Government Revenue Collections dataset provides a daily overview of federal revenue collections such as individual and corporate income tax deposits, customs duties, fees for government service, fines, and loan repayments. These collections can be made through either electronic or non-electronic transactions by mail, internet, bank, or over-the-counter channels."),
        tags$li("Each row in the data represents the net collections amount for the associated electronic category, channel type, and tax category on the given date"),
        tags$li(a("https://fiscaldata.treasury.gov/datasets/revenue-collections-management/u-s-government-revenue-collections", 
                  href = "https://fiscaldata.treasury.gov/datasets/revenue-collections-management/u-s-government-revenue-collections", 
                  target = "_blank"))
      ),
      
      h3("About the tabs"),
      tags$ul(
        tags$li(tags$b("About tab: "), "gives some introductory info about the app, as well as links to resources to learn more about the data"),
        tags$li(
          tags$b("Data Selection tab: "), "allows you to query the API in real time with different filters that modify the API endpoint that is sent in the request, any filters you apply here will also determine the data available in the data exploration tab.",
          tags$ul(
            tags$li(tags$b("API & Row Filters (these will modify the API endpoint and filter the data)"),
                    tags$ul(
                      tags$li("Electronic Category Description - Transaction type"),
                      tags$li("Channel Type Description - The collections program reporting the data to the collections information repository (CIR)"),
                      tags$li("Tax Category Description - The category of tax"),
                      tags$li("Calendar Year Range - The calendar year associated with record date"),
                      tags$li("Calendar Month Range - The calendar month associated with record date"),
                      tags$li("Number of rows - The # of rows to request from the API")
                    )
            ),
            tags$li(tags$b("Other Fields (you can select which of these to download as a csv, but they will not modify the API request)"),
                    tags$ul(
                      tags$li("Record Date - The date that data was published"),
                      tags$li("Net Collections Amount - The dollar amount of the transaction"),
                      tags$li("Record Fiscal Year - The fiscal year associated with record"),
                      tags$li("Record Fiscal Quarter - The fiscal quarter associated with record"),
                      tags$li("Record Calendar Quarter - The calendar quarter associated with record date"),
                      tags$li("Record Calendar Day - The calendar day associated with record date")
                    )
            )
          )
        ),
        tags$li(
          tags$b("Data Exploration tab: "), 
          "provides some graphical summaries of the data you selected, the plots that are available will depend on which of the data points you'd like to plot and the histogram can be faceted",
          tags$ul(
            tags$li("The collection amounts within the database, as well as the record counts, can be plotted and summarized"),
            tags$li("Every field except for the record_date can be used as a factor for any of the plots and summaries")
          )
        )
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
          numericInput("rows", "Number of Rows to Return (max 10000)", value = 1000, min = 1, max = 10000),
          
          # Column selection checkboxes
          checkboxGroupInput(
            "columns", 
            "Select Columns to Download", 
            choices = c("Electronic Category Description" = "electronic_category_desc", 
                        "Channel Type Description" = "channel_type_desc", 
                        "Tax Category Description" = "tax_category_desc", 
                        "Record Calendar Year" = "record_calendar_year", 
                        "Record Calendar Month" = "record_calendar_month",
                        "Record Fiscal Year" = "record_fiscal_year",
                        "Record Fiscal Quarter" = "record_fiscal_quarter",
                        "Record Calendar Quarter" = "record_calendar_quarter",
                        "Record Calendar Day" = "record_calendar_day"),
            selected = c("electronic_category_desc", "channel_type_desc", "tax_category_desc", "record_calendar_year", "record_calendar_month", "record_fiscal_year", "record_fiscal_quarter", "record_calendar_quarter", "record_calendar_day")),
          
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
        column(
          4,
          selectInput("summary_var", "Variable to Summarize", choices = c("Net Collections Amount" = "net_collections_amt", "Count of Records" = "count"))
        ),
        
        # Dynamic UI for plot type, changes based on select variable
        column(
          4,
          uiOutput("plot_type_ui") 
        ),
        
        # Selection of variable to partition plots on
        column(
          4,
          selectInput(
            "contingency_var", 
            "Variable to Partition by", 
            choices = c(
              "Electronic Category Description" = "electronic_category_desc", 
              "Channel Type Description" = "channel_type_desc", 
              "Tax Category Description" = "tax_category_desc",
              "Record Calendar Year" = "record_calendar_year",
              "Record Calendar Month" = "record_calendar_month",
              "Record Fiscal Year" = "record_fiscal_year",
              "Record Fiscal Quarter" = "record_fiscal_quarter",
              "Record Calendar Quarter" = "record_calendar_quarter",
              "Record Calendar Day" = "record_calendar_day"
            )
          )
        )
      ),
      
      # Conditional panels for faceting options
      conditionalPanel(
        condition = "input.plot_type == 'Histogram'",
        checkboxInput("facet_histogram", "Facet by selected partition variable", value = FALSE)
      ),
      conditionalPanel(
        condition = "input.plot_type == 'Box plot'",
        checkboxInput("facet_boxplot", "Facet by selected partition variable", value = FALSE)
      ),
      conditionalPanel(
        condition = "input.plot_type == 'Line plot'",
        checkboxInput("facet_lineplot", "Facet by selected partition variable", value = FALSE)
      ),
      
      # Condition y-axis choices based on plot type and variable selection
      conditionalPanel(
        condition = "input.plot_type == 'Heat map'",
        uiOutput("y_axis_var_ui")
      ),
      
      # Generate plot
      plotOutput("plot"),
      
      # Drop down for summary type selection and second variable selection
      fluidRow(
        
        # Main drop down
        column(4,
               h4("Summaries (of the variable you selected to summarize above)"),
               selectInput("summary_type", "Type of Summary", choices = c("Mean/SD/Variance" = "mean_sd", "Percentiles" = "percentiles", "Contingency Table" = "contingency_table"))
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

