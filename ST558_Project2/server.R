# ==================================================================================
# api notes
# ==================================================================================

# using this data
# https://fiscaldata.treasury.gov/datasets/revenue-collections-management/u-s-government-revenue-collections

# api problems
# 1 api seems to fail if you don't pass filters in this order: calendar year > calendar month > electronic category > tax category > channel type
# 2 no documentation on this order that i could find, had to infer from testing urls
# 3 also when passing a month you need to pass 8 as 08, 1 as 01, etc

# working api filters @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# https://api.fiscaldata.treasury.gov/services/api/fiscal_service/v2/revenue/rcm?filter=electronic_category_desc:eq:Fully%20Electronic%20-%20All,channel_type_desc:eq:Bank,tax_category_desc:eq:IRS%20Tax,record_calendar_year:eq:2004,record_calendar_month:eq:10&format=json&page[number]=1&page[size]=1000

# ==================================================================================
# API function
# ==================================================================================

# Load libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(scales)
library(httr)
library(jsonlite)
library(tidyr)
library(DT)
library(purrr)

# Define API function
get_revenue_collections_data <- function(
    record_date = NULL, 
    electronic_category_desc = NULL, 
    channel_type_desc = NULL, 
    tax_category_desc = NULL, 
    record_calendar_year = NULL, 
    record_calendar_month = NULL, 
    format = "json", 
    page_number = 1, 
    page_size = 1000){
  
  # Set base URL and endpoint
  base_url <- 'https://api.fiscaldata.treasury.gov/services/api/fiscal_service/'
  end_point <- 'v2/revenue/rcm'
  full_url <- paste0(base_url, end_point)
  
  # baseR URL encoding function - this puts the text in a url friendly format, such as removing spaces
  url_encode <- function(value) {
    return(URLencode(as.character(value), reserved = TRUE))
  }
  
  # Function to put the filters in a string format the API expects
  build_filter <- function(field, value, operator = "eq") {
    
    # Check if there's any value 
    if (!is.null(value) && length(value) > 0) {
      
      # If there's more than a single value passed, wrap it in IN
      if (length(value) > 1) {
        return(paste0(field, ":in:(", paste(url_encode(value), collapse = ","), ")"))
      } 
      # Otherwise just paste the single value that was passed with the default operator, eq
      else {
        return(paste0(field, ":", operator, ":", url_encode(value)))
      }
    }
    
    # Return nada if there's no value
    return(NULL)
  }
  
  # Function to put the RANGE filters in a string format the API expects
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
  
  # Take the given inputs and put them in the necessary format, using my functions above
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
  
  # Print for debugging @@@@@@@@@@@@@@@@@@@@@
  # print(full_query_url)
  
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
  
  # Print for debugging @@@@@@@@@@@@@@@@@@@@@
  # print(head(data))
  
  # Return the results
  return(data)
}

# ==================================================================================
# server.R code
# ==================================================================================

# Define server logic
shinyServer(function(input, output, session){
  
  # Define a named vector for clean names and corresponding column names
  clean_names <- c(
    "Electronic Category Description" = "electronic_category_desc",
    "Channel Type Description" = "channel_type_desc",
    "Tax Category Description" = "tax_category_desc",
    "Record Calendar Year" = "record_calendar_year",
    "Record Calendar Month" = "record_calendar_month"
  )
  
  # Reactive expression to fetch data based on user inputs in the UI using my function get_revenue_collections_data
  fetch_data <- reactive({
    data <- get_revenue_collections_data(
      record_date = input$record_date,
      electronic_category_desc = input$electronic_category_desc,
      channel_type_desc = input$channel_type_desc,
      tax_category_desc = input$tax_category_desc,
      record_calendar_year = c(input$record_calendar_year[1], input$record_calendar_year[2]),
      record_calendar_month = c(input$record_calendar_month[1], input$record_calendar_month[2]),
      page_number = 1, 
      page_size = input$rows
    )
    
    # Convert data to numeric for plots and handle non-finite values
    data <- data |>
      mutate(
        net_collections_amt = as.numeric(net_collections_amt),
        record_calendar_year = as.factor(record_calendar_year),
        record_calendar_month = as.factor(record_calendar_month)
      ) |>
      filter(!is.na(net_collections_amt) & net_collections_amt > 0)
    
    # Print for debugging @@@@@@@@@@@@@@@@@@@@@
    # print(head(data))
    
    # return results
    return(data)
  })
  
  # Reactive expression to fetch data for the table based on user-selected columns
  fetch_table_data <- reactive({
    data <- fetch_data()
    
    # Select only the columns chosen by the user + mandatory defaults
    selected_columns <- c("record_date", "net_collections_amt", input$columns)
    data <- data |> select(all_of(selected_columns))
    
    # Print for debugging @@@@@@@@@@@@@@@@@@@@@
    # print(head(data))
    
    # return selection
    return(data)
  })
  
  # Update plot types based on summary variable while preserving current selection if possible
  observeEvent(input$summary_var, {
    
    # Get initial plot type
    current_plot_type <- input$plot_type
    
    # Define plots and summaries for net_collections_amt
    if (input$summary_var == "net_collections_amt") {
      plot_types <- c("Histogram", "Box plot", "Line plot", "Heat map")
      updateSelectInput(session, "plot_type", choices = plot_types, selected = if (!is.null(current_plot_type) && current_plot_type %in% plot_types) current_plot_type else "Histogram")
      updateSelectInput(session, "summary_type", choices = c("Mean/SD" = "mean_sd", "Percentiles" = "percentiles", "Contingency Table" = "contingency_table"))
    } 
    
    # Define plots and summaries for count data
    else if (input$summary_var == "count") {
      plot_types <- c("Line plot", "Heat map")
      updateSelectInput(session, "plot_type", choices = plot_types, selected = if (!is.null(current_plot_type) && current_plot_type %in% plot_types) current_plot_type else "Heat map")
      updateSelectInput(session, "summary_type", choices = c("Contingency Table" = "contingency_table"))
    }
  })
  
  # Render table
  output$data_table <- renderTable({
    
    # Check data is available
    req(fetch_table_data())  
    
    # Return results
    fetch_table_data()  
  })
  
  # Download handler is a shiny function to enable file downloads, here im outputing as csv
  output$download_data <- downloadHandler(
    filename = function() {
      paste("revenue_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(fetch_table_data(), file, row.names = FALSE)
    }
  )
  
  # Generate dynamic UI for plot type depending on the input variable
  output$plot_type_ui <- renderUI({
    
    # If the selection variable is amount, return these plots
    if (input$summary_var == "net_collections_amt") {
      selectInput("plot_type", "Plot Type", choices = c("Histogram", "Box plot", "Line plot", "Heat map"), selected = "Histogram")
    } else if (input$summary_var == "count") {
      selectInput("plot_type", "Plot Type", choices = c("Line plot", "Heat map"), selected = "Heat map")
    }
  })
  
  # Generate dynamic UI for y-axis variable in heat map
  output$y_axis_var_ui <- renderUI({
    req(input$contingency_var)
    
    # List of all potential variables for y-axis with clean names
    y_axis_choices <- setdiff(names(clean_names), names(clean_names)[clean_names == input$contingency_var])
    
    selectInput("y_axis_var", "Y-axis Variable", choices = y_axis_choices)
  })
  
  # Generate dynamic UI for second variable in contingency table
  output$second_var_ui <- renderUI({
    
    # If the user selects contigency table option show the options below
    req(input$summary_type == "contingency_table")
    
    # List of all potential second variables with clean names
    second_var_choices <- setdiff(names(clean_names), input$contingency_var)
    
    # Define selection box
    selectInput("second_var", "Second Variable (Optional)", choices = c("", second_var_choices))
  })
  
  # Render plot based on user input
  output$plot <- renderPlot({
    
    # Get data
    data <- fetch_data()
    
    # Ensure plot type is selected
    req(input$plot_type)
    req(input$summary_var)
    
    # Extract date range and format as "Month YYYY"
    date_range_year <- range(as.numeric(as.character(data$record_calendar_year)), na.rm = TRUE)
    date_range_month <- range(as.numeric(as.character(data$record_calendar_month)), na.rm = TRUE)
    min_date <- paste(month.name[date_range_month[1]], date_range_year[1])
    max_date <- paste(month.name[date_range_month[2]], date_range_year[2])
    date_range_text <- paste(min_date, "to", max_date)
    
    # Define all the different plots a user can see based on the selected variables
    if (input$summary_var == "count") {
      data <- data |>
        mutate(count = 1)
    }
    
    # Define histogram code
    if (input$plot_type == "Histogram") {
      p <- ggplot(data, aes_string(x = input$summary_var, fill = input$contingency_var)) +
        geom_histogram(bins = 25, alpha = 0.5, position = "stack") +
        labs(title = paste("Histogram of", input$summary_var, "between", date_range_text), x = input$summary_var, y = "Count") +
        scale_x_log10(labels = if (input$summary_var == "net_collections_amt") dollar else identity) +
        theme_minimal()
      
      if (input$facet_histogram) {
        p <- p + facet_wrap(as.formula(paste("~", input$contingency_var)))
      }
      
      print(p)
    } 
    
    # Define box plot code
    else if (input$plot_type == "Box plot") {
      ggplot(data, aes_string(x = input$contingency_var, y = input$summary_var, fill = input$contingency_var)) +
        geom_boxplot(alpha = 0.5) +
        labs(title = paste("Box plots of", input$summary_var, "by", input$contingency_var, "between", date_range_text), x = input$contingency_var, y = input$summary_var) +
        scale_y_log10(labels = if (input$summary_var == "net_collections_amt") dollar else identity) +
        theme_minimal() +
        theme(legend.position = "none")
    } 
    
    # Define line plot code
    else if (input$plot_type == "Line plot") {
      if (input$contingency_var == "record_calendar_year") {
        ggplot(data, aes(x = as.factor(record_calendar_year), y = !!sym(input$summary_var), group = 1)) +
          stat_summary(fun = sum, geom = "line") +
          labs(title = paste(input$summary_var, "by Year between", date_range_text), x = "Year", y = input$summary_var) +
          scale_y_continuous(labels = if (input$summary_var == "net_collections_amt") dollar else identity) +
          theme_minimal()
      } else {
        ggplot(data, aes_string(x = "as.factor(record_calendar_year)", y = input$summary_var, color = input$contingency_var, group = input$contingency_var)) +
          stat_summary(fun = sum, geom = "line") +
          labs(title = paste("Line plot of", input$summary_var, "by Year and", input$contingency_var, "between", date_range_text), x = "Year", y = input$summary_var) +
          scale_y_continuous(labels = if (input$summary_var == "net_collections_amt") dollar else identity) +
          theme_minimal()
      }
    } 
    
    # Define heat map code
    else if (input$plot_type == "Heat map") {
      y_axis <- clean_names[input$y_axis_var]
      heatmap_data <- data |>
        group_by(!!sym(input$contingency_var), !!sym(y_axis)) |>
        summarize(total_value = sum(!!sym(input$summary_var), na.rm = TRUE), .groups = "drop")
      
      ggplot(heatmap_data, aes_string(x = input$contingency_var, y = y_axis, fill = "total_value")) +
        geom_tile(alpha = 0.5) +
        geom_text(aes(label = if (input$summary_var == "net_collections_amt") scales::dollar(total_value) else total_value), color = "black", size = 3) +
        labs(title = paste("Heat map of", input$summary_var, "by", input$contingency_var, "and", y_axis, "between", date_range_text), x = input$contingency_var, y = y_axis, fill = input$summary_var) +
        scale_fill_gradient(low = "lightgreen", high = "darkgreen", labels = if (input$summary_var == "net_collections_amt") dollar else identity) +
        theme_minimal()
    }
  })
  
  # Create contingency and summary tables based on user selection
  output$summary_output <- renderPrint({
    data <- fetch_data()
    
    req(input$summary_type)
    req(input$contingency_var)
    
    if (input$summary_type == "mean_sd") {
      summary_data <- data |>
        group_by(!!sym(input$contingency_var)) |>
        summarise(
          mean_amt = mean(net_collections_amt, na.rm = TRUE),
          sd_amt = sd(net_collections_amt, na.rm = TRUE)
        )
    } else if (input$summary_type == "percentiles") {
      summary_data <- data |>
        group_by(!!sym(input$contingency_var)) |>
        summarise(
          percentile_25 = quantile(net_collections_amt, 0.25, na.rm = TRUE),
          percentile_50 = median(net_collections_amt, na.rm = TRUE),
          percentile_75 = quantile(net_collections_amt, 0.75, na.rm = TRUE),
          percentile_100 = quantile(net_collections_amt, 1, na.rm = TRUE)
        )
    } else if (input$summary_type == "contingency_table") {
      if (input$second_var != "") {
        summary_data <- data |>
          count(!!sym(input$contingency_var), !!sym(clean_names[input$second_var])) |>
          spread(key = !!sym(clean_names[input$second_var]), value = n, fill = 0)
      } else {
        summary_data <- data |>
          count(!!sym(input$contingency_var))
      }
    }
    
    # Print summaries
    print(summary_data)
  })
  
  # Sync the main panel tabs with the sidebar tabs
  observeEvent(input$tabs, {
    updateTabsetPanel(session, "main_tabs", selected = input$tabs)
  })
})

