library(shiny)
library(dplyr)
library(ggplot2)
library(scales)
library(httr)
library(jsonlite)
library(DT)

# ==================================================================================
# define api function
# ==================================================================================

# define function with 6 filter options
get_revenue_collections_data <- function(record_date = NULL, electronic_category_desc = NULL, channel_type_desc = NULL, tax_category_desc = NULL, 
                                         record_fiscal_year = NULL, record_calendar_year = NULL, record_calendar_month = NULL, 
                                         format = "json", page_number = 1, page_size = 1000) {
  
  # set base url and endpoint
  base_url <- 'https://api.fiscaldata.treasury.gov/services/api/fiscal_service/'
  end_point <- 'v2/revenue/rcm'
  full_url <- paste0(base_url, end_point)
  
  # function to put the filters in a string format the API expects
  build_filter <- function(field, value) {
    if (!is.null(value)) {
      return(paste0(field, ":eq:", URLencode(as.character(value))))
    }
    return(NULL)
  }
  
  # create query parameters
  filters <- c(
    build_filter("record_date", record_date),
    build_filter("electronic_category_desc", electronic_category_desc),
    build_filter("channel_type_desc", channel_type_desc),
    build_filter("tax_category_desc", tax_category_desc),
    build_filter("record_fiscal_year", record_fiscal_year),
    build_filter("record_calendar_year", record_calendar_year),
    build_filter("record_calendar_month", record_calendar_month)
  )
  
  # Remove NULL values from filters
  filters <- filters[!sapply(filters, is.null)]
  
  # Combine the filters into a single string
  filter_string <- if (length(filters) > 0) {
    paste(filters, collapse = "&filter=")
  } else {
    ""
  }
  
  # Construct the full URL with query parameters
  query_string <- paste0("filter=", filter_string, "&format=", format, "&page[number]=", page_number, "&page[size]=", page_size)
  full_query_url <- paste0(full_url, "?", query_string)
  print(full_query_url)
  
  # Get the data from the API
  url_data <- httr::GET(full_query_url)
  
  # Response check
  if (status_code(url_data) != 200) {
    stop("Failed to retrieve data: ", status_code(url_data), " - ", content(url_data, "text"))
  }
  
  # Parse data as tibble
  data <- url_data %>%
    httr::content(as = "text") %>%
    fromJSON(flatten = TRUE, simplifyDataFrame = TRUE) %>%
    pluck("data") %>%
    as_tibble()
  
  # Return the results
  return(data)
}

# ==================================================================================
# define server logic
# ==================================================================================

# Define server logic
shinyServer(function(input, output, session) {
  
  # Reactive expression to fetch sample data based on user inputs
  sample_data <- reactive({
    get_revenue_collections_data(
      record_date = NULL,
      electronic_category_desc = input$electronic_category_desc,
      channel_type_desc = input$channel_type_desc,
      tax_category_desc = input$tax_category_desc,
      record_fiscal_year = NULL,
      record_calendar_year = input$record_calendar_year,
      record_calendar_month = input$record_calendar_month,
      page_number = 1, 
      page_size = 10
    )
  })
  
  # Render sample data table
  output$sample_data_table <- renderDataTable({
    datatable(sample_data())
  })
  
  # Reactive expression to fetch data based on user inputs
  fetch_data <- eventReactive(input$query_data, {
    get_revenue_collections_data(
      record_date = NULL,
      electronic_category_desc = input$electronic_category_desc,
      channel_type_desc = input$channel_type_desc,
      tax_category_desc = input$tax_category_desc,
      record_fiscal_year = NULL,
      record_calendar_year = input$record_calendar_year,
      record_calendar_month = input$record_calendar_month,
      page_number = 1, 
      page_size = input$rows
    )
  })
  
  # Render data table
  output$data_table <- renderDataTable({
    datatable(fetch_data())
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
    
    if (input$plot_type == "Histogram") {
      ggplot(data, aes_string(x = input$summary_var)) + 
        geom_histogram(fill = "blue", bins = 30) + 
        theme_minimal()
    } else if (input$plot_type == "Boxplot") {
      ggplot(data, aes_string(x = input$summary_var, y = "some_numeric_column")) + 
        geom_boxplot(fill = "blue") + 
        theme_minimal()
    } else if (input$plot_type == "Line Plot") {
      ggplot(data, aes_string(x = "some_time_column", y = input$summary_var, group = 1)) + 
        geom_line() + 
        theme_minimal()
    } else if (input$plot_type == "Heatmap") {
      ggplot(data, aes_string(x = "some_category", y = input$summary_var, fill = "some_value")) + 
        geom_tile() + 
        theme_minimal()
    }
  })
  
  # Sync the main panel tabs with the sidebar tabs
  observeEvent(input$tabs, {
    updateTabsetPanel(session, "main_tabs", selected = input$tabs)
  })
})


------------------------------------------------------------------------------------------- backup 2
------------------------------------------------------------------------------------------- backup 2
------------------------------------------------------------------------------------------- backup 2
library(shiny)
library(dplyr)
library(ggplot2)
library(scales)
library(httr)
library(jsonlite)
library(DT)


# ==================================================================================
# define api function
# ==================================================================================

# Define function with 6 filter options
get_revenue_collections_data <- function(record_date = NULL, electronic_category_desc = NULL, channel_type_desc = NULL, tax_category_desc = NULL, 
                                         record_fiscal_year = NULL, record_calendar_year = NULL, record_calendar_month = NULL, 
                                         format = "json", page_number = 1, page_size = 1000) {
  
  # Set base URL and endpoint
  base_url <- 'https://api.fiscaldata.treasury.gov/services/api/fiscal_service/'
  end_point <- 'v2/revenue/rcm'
  full_url <- paste0(base_url, end_point)
  
  # Function to put the filters in a string format the API expects
  build_filter <- function(field, value) {
    if (!is.null(value) && value != "") {
      return(paste0(field, ":eq:", URLencode(as.character(value))))
    }
    return(NULL)
  }
  
  # Create query parameters
  filters <- c(
    build_filter("record_date", record_date),
    build_filter("electronic_category_desc", electronic_category_desc),
    build_filter("channel_type_desc", channel_type_desc),
    build_filter("tax_category_desc", tax_category_desc),
    build_filter("record_fiscal_year", record_fiscal_year),
    build_filter("record_calendar_year", record_calendar_year),
    build_filter("record_calendar_month", record_calendar_month)
  )
  
  # Print filters for debugging
  print(filters)
  
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
  print(full_query_url) # debugging @@@@@@@@@@@@@@@@@@@@@
  
  # Print the full query URL for debugging
  print(full_query_url)
  
  # Get the data from the API
  url_data <- httr::GET(full_query_url)
  
  # Response check
  if (status_code(url_data) != 200) {
    stop("Failed to retrieve data: ", status_code(url_data), " - ", content(url_data, "text"))
  }
  
  # Parse data as tibble
  data <- url_data %>%
    httr::content(as = "text") %>%
    fromJSON(flatten = TRUE, simplifyDataFrame = TRUE) %>%
    pluck("data") %>%
    as_tibble()
  
  # Print a glimpse of the data for debugging
  print(head(data))
  
  # Return the results
  return(data)
}


# ==================================================================================
# define server logic
# ==================================================================================

# Define server logic
shinyServer(function(input, output, session) {
  
  # Reactive expression to fetch data based on user inputs
  fetch_data <- reactive({
    get_revenue_collections_data(
      record_date = input$record_date,
      electronic_category_desc = input$electronic_category_desc,
      channel_type_desc = input$channel_type_desc,
      tax_category_desc = input$tax_category_desc,
      record_fiscal_year = input$record_fiscal_year,
      record_calendar_year = input$record_calendar_year,
      record_calendar_month = input$record_calendar_month,
      page_number = 1, 
      page_size = input$rows
    )
  })
  
  # Render data table without "Show # entries" and "Search" options
  output$data_table <- renderDataTable({
    datatable(fetch_data(), options = list(dom = 't'))  # 't' removes the table control elements
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
    
    if (input$plot_type == "Histogram") {
      ggplot(data, aes_string(x = input$summary_var)) + 
        geom_histogram(fill = "blue", bins = 30) + 
        theme_minimal()
    } else if (input$plot_type == "Boxplot") {
      ggplot(data, aes_string(x = input$summary_var, y = "some_numeric_column")) + 
        geom_boxplot(fill = "blue") + 
        theme_minimal()
    } else if (input$plot_type == "Line Plot") {
      ggplot(data, aes_string(x = "some_time_column", y = input$summary_var, group = 1)) + 
        geom_line() + 
        theme_minimal()
    } else if (input$plot_type == "Heatmap") {
      ggplot(data, aes_string(x = "some_category", y = input$summary_var, fill = "some_value")) + 
        geom_tile() + 
        theme_minimal()
    }
  })
  
  # Sync the main panel tabs with the sidebar tabs
  observeEvent(input$tabs, {
    updateTabsetPanel(session, "main_tabs", selected = input$tabs)
  })
})



# working url filters @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v2/revenue/rcm?filter=electronic_category_desc:eq:Fully%20Electronic%20-%20All,channel_type_desc:eq:Bank,tax_category_desc:eq:IRS%20Tax,record_calendar_year:eq:2004,record_calendar_month:eq:10&format=json&page[number]=1&page[size]=1000
