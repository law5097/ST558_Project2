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
                        choices = c("IRS Non-Tax", "IRS Tax", "Non-Tax")
                      ),
                      
                      # Input for calendar year range
                      sliderInput("record_calendar_year", "Calendar Year Range", min = 2000, max = 2024, value = c(2000, 2024), step = 1),
                      
                      # Input for calendar month range
                      sliderInput("record_calendar_month", "Calendar Month Range", min = 1, max = 12, value = c(1, 12), step = 1),
                      
                      # Input for number of rows
                      numericInput("rows", "Number of Rows to Return (max 10000)", value = 1000, min = 1, max = 10000),
                      
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
             plotOutput("plot")
    )
  )
))


# ==================================================================================
# api notes
# ==================================================================================

# problems
# 1 api seems to fail if you don't pass filters in this order: calendar year > calendar month > electronic category > tax category > channel type
# no documentation on this order that i could find, had to infer from testing urls
# also when passing amonth you need to pass 8 as 08, 1 as 01, etc

# using this data
# https://fiscaldata.treasury.gov/datasets/revenue-collections-management/u-s-government-revenue-collections

# working url filters @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v2/revenue/rcm?filter=electronic_category_desc:eq:Fully%20Electronic%20-%20All,channel_type_desc:eq:Bank,tax_category_desc:eq:IRS%20Tax,record_calendar_year:eq:2004,record_calendar_month:eq:10&format=json&page[number]=1&page[size]=1000

# ==================================================================================
# load libraries
# ==================================================================================

library(shiny)
library(dplyr)
library(ggplot2)
library(scales)
library(httr)
library(jsonlite)
library(DT)
library(purrr)

# ==================================================================================
# define api function
# ==================================================================================

# define api function
get_revenue_collections_data <- function(record_date = NULL, electronic_category_desc = NULL, channel_type_desc = NULL, tax_category_desc = NULL, 
                                         record_fiscal_year = NULL, record_calendar_year = NULL, record_calendar_month = NULL, 
                                         format = "json", page_number = 1, page_size = 1000) {
  
  # Set base URL and endpoint
  base_url <- 'https://api.fiscaldata.treasury.gov/services/api/fiscal_service/'
  end_point <- 'v2/revenue/rcm'
  full_url <- paste0(base_url, end_point)
  
  # URL Encoding Function
  url_encode <- function(value) {
    return(URLencode(as.character(value), reserved = TRUE))
  }
  
  # Function to put the filters in a string format the API expects
  build_filter <- function(field, value) {
    if (!is.null(value) && value != "") {
      return(paste0(field, ":eq:", url_encode(value)))
    }
    return(NULL)
  }
  
  build_range_filter <- function(field, values) {
    if (!is.null(values) && length(values) == 2) {
      gte_value <- values[1]
      lte_value <- values[2]
      if (field == "record_calendar_month") {
        gte_value <- sprintf("%02d", as.numeric(gte_value))
        lte_value <- sprintf("%02d", as.numeric(lte_value))
      }
      return(c(
        paste0(field, ":gte:", url_encode(gte_value)),
        paste0(field, ":lte:", url_encode(lte_value))
      ))
    }
    return(NULL)
  }
  
  # Create filters in the specified order
  filters <- c(
    build_range_filter("record_calendar_year", record_calendar_year),
    build_range_filter("record_calendar_month", record_calendar_month),
    build_filter("electronic_category_desc", electronic_category_desc),
    build_filter("tax_category_desc", tax_category_desc),
    build_filter("channel_type_desc", channel_type_desc)
  )
  
  # Flatten the list of filters
  filters <- unlist(filters)
  
  # Remove NULL values from filters
  filters <- filters[!sapply(filters, is.null)]
  
  # Combine the filters into a single string separated by commas
  filter_string <- if (length(filters) > 0) {
    paste(filters, collapse = ",")
  } else {
    ""
  }
  
  # Construct the full URL with query parameters
  query_string <- paste0("filter=", filter_string, "&format=", format, "&page[number]=", page_number, "&page[size]=", page_size)
  full_query_url <- paste0(full_url, "?", query_string)
  
  # debugging @@@@@@@@@@@@@@@@@@@@@
  print(full_query_url)
  
  # Get the data from the API
  url_data <- httr::GET(full_query_url)
  
  # Response check
  if (status_code(url_data) != 200) {
    stop("Failed to retrieve data: ", status_code(url_data), " - ", content(url_data, "text"))
  }
  
  # Parse data as tibble
  data <- url_data |>
    httr::content(as = "text") |>
    fromJSON(flatten = TRUE, simplifyDataFrame = TRUE) |>
    pluck("data") |>
    as_tibble()
  
  # Print a glimpse of the data for debugging
  print(head(data))
  
  # Return the results
  return(data)
}

# ==================================================================================
# define server logic
# ==================================================================================

shinyServer(function(input, output, session) {
  
  # Reactive expression to fetch data based on user inputs
  fetch_data <- reactive({
    data <- get_revenue_collections_data(
      record_date = input$record_date,
      electronic_category_desc = input$electronic_category_desc,
      channel_type_desc = input$channel_type_desc,
      tax_category_desc = input$tax_category_desc,
      record_fiscal_year = input$record_fiscal_year,
      record_calendar_year = c(input$record_calendar_year[1], input$record_calendar_year[2]),
      record_calendar_month = c(input$record_calendar_month[1], input$record_calendar_month[2]),
      page_number = 1, 
      page_size = input$rows
    )
    
    # Convert to numeric for plots
    data <- data |>
      mutate(
        net_collections_amt = as.numeric(net_collections_amt),
        record_calendar_year = as.integer(record_calendar_year),
        record_calendar_month = as.integer(record_calendar_month)
      )
    
    # Select only the columns chosen by the user
    selected_columns <- c("record_date", "net_collections_amt", input$columns)
    data <- data %>% select(all_of(selected_columns))
    
    return(data)
  })
  
  # Update plot types based on summary variable
  observeEvent(input$summary_var, {
    if (input$summary_var == "net_collections_amt") {
      updateSelectInput(session, "plot_type", choices = c("Histogram", "Boxplot", "Line Plot", "Heatmap"))
    } else if (input$summary_var == "count") {
      updateSelectInput(session, "plot_type", choices = c("Line Plot", "Heatmap"))
    }
  })
  
  # Render data table without "Show # entries" and "Search" options
  output$data_table <- renderDataTable({
    datatable(fetch_data(), options = list(dom = 't', pageLength = 20))  # 't' removes the table control elements
  })
  
  # Download handler
  output$download_data <- downloadHandler(
    filename = function() {
      paste("revenue_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(fetch_data(), file, row.names = FALSE)
    }
  )
  
  # Render plot based on user input
  output$plot <- renderPlot({
    data <- fetch_data()
    
    # Extract date range and format as "Month YYYY"
    date_range_year <- range(data$record_calendar_year, na.rm = TRUE)
    date_range_month <- range(data$record_calendar_month, na.rm = TRUE)
    min_date <- paste(month.name[date_range_month[1]], date_range_year[1])
    max_date <- paste(month.name[date_range_month[2]], date_range_year[2])
    date_range_text <- paste(min_date, "to", max_date)
    
    if (input$summary_var == "count") {
      data <- data %>%
        mutate(count = 1)
    }
    
    if (input$plot_type == "Histogram") {
      ggplot(data, aes_string(x = input$summary_var)) +
        geom_histogram(bins = 25, fill = "lightgreen", color = "black", alpha = 0.5) +
        labs(title = paste("Histogram of", input$summary_var, "between", date_range_text), x = input$summary_var, y = "Count") +
        scale_x_log10(labels = if (input$summary_var == "net_collections_amt") dollar else identity) +
        theme_minimal()
    } else if (input$plot_type == "Boxplot") {
      ggplot(data, aes_string(x = "as.factor(record_calendar_year)", y = input$summary_var, fill = "as.factor(record_calendar_year)")) +
        geom_boxplot(alpha = 0.5) +
        labs(title = paste("Distribution of", input$summary_var, "by Year between", date_range_text), x = "Year", y = input$summary_var) +
        scale_y_log10(labels = if (input$summary_var == "net_collections_amt") dollar else identity) +
        theme_minimal() +
        theme(legend.position = "none")
    } else if (input$plot_type == "Line Plot") {
      ggplot(data, aes_string(x = "as.factor(record_calendar_year)", y = input$summary_var, color = "tax_category_desc", group = "tax_category_desc")) +
        stat_summary(fun = sum, geom = "line") +
        labs(title = paste(input$summary_var, "by Year and Tax Category between", date_range_text), x = "Year", y = input$summary_var) +
        scale_y_continuous(labels = if (input$summary_var == "net_collections_amt") dollar else identity) +
        theme_minimal()
    } else if (input$plot_type == "Heatmap") {
      heatmap_data <- data %>%
        group_by(record_calendar_year, record_calendar_month) %>%
        summarize(total_value = sum(!!sym(input$summary_var), na.rm = TRUE), .groups = "drop")
      
      ggplot(heatmap_data, aes(x = as.factor(record_calendar_year), y = as.factor(record_calendar_month), fill = total_value)) +
        geom_tile(alpha = 0.5) +
        geom_text(aes(label = if (input$summary_var == "net_collections_amt") scales::dollar(total_value) else total_value), color = "black", size = 3) +
        labs(title = paste("Heatmap of", input$summary_var, "by Year and Month between", date_range_text), x = "Year", y = "Month", fill = input$summary_var) +
        scale_fill_gradient(low = "lightgreen", high = "darkgreen", labels = if (input$summary_var == "net_collections_amt") dollar else identity) +
        theme_minimal()
    }
  })
  
  # Sync the main panel tabs with the sidebar tabs
  observeEvent(input$tabs, {
    updateTabsetPanel(session, "main_tabs", selected = input$tabs)
  })
})
