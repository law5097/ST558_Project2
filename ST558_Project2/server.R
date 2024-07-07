
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
# server.R code
# ==================================================================================

# Load libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(scales)
library(httr)
library(jsonlite)
library(DT)
library(purrr)

# Define API function
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
  build_filter <- function(field, value, operator = "eq") {
    if (!is.null(value) && length(value) > 0) {
      if (length(value) > 1) {
        return(paste0(field, ":in:(", paste(url_encode(value), collapse = ","), ")"))
      } else {
        return(paste0(field, ":", operator, ":", url_encode(value)))
      }
    }
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
  
  # Create filters in the order the api needs
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
  
  # Debugging @@@@@@@@@@@@@@@@@@@@@
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
  
  # Print for debugging @@@@@@@@@@@@@@@@@@@@@
  print(head(data))
  
  # Return the results
  return(data)
}

# Define server logic
shinyServer(function(input, output, session){
  
  # Reactive expression to fetch data based on user inputs in the UI
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
        record_calendar_year = as.factor(record_calendar_year),  # Convert to factor for plotting
        record_calendar_month = as.integer(record_calendar_month)
      )
    
    # Print for debugging
    print(head(data))
    
    # return results
    return(data)
  })
  
  # Reactive expression to fetch data for the table based on user-selected columns
  fetch_table_data <- reactive({
    data <- fetch_data()
    
    # Select only the columns chosen by the user + mandatory defaults
    selected_columns <- c("record_date", "net_collections_amt", input$columns)
    data <- data |> select(all_of(selected_columns))
    
    # Print for debugging
    print(head(data))
    
    # return selection
    return(data)
  })
  
  # Update plot types based on summary variable while preserving current selection if possible
  observeEvent(input$summary_var, {
    
    # Get initial plot type
    current_plot_type <- input$plot_type
    
    # Define plots and summaries for net_collections_amt
    if (input$summary_var == "net_collections_amt") {
      plot_types <- c("Histogram", "Boxplot", "Line Plot", "Heatmap")
      updateSelectInput(session, "plot_type", choices = plot_types, selected = if (!is.null(current_plot_type) && current_plot_type %in% plot_types) current_plot_type else plot_types[1])
      updateSelectInput(session, "summary_type", choices = c("Mean/SD" = "mean_sd", "Percentiles" = "percentiles", "Contingency Table" = "contingency_table"))
    } 
    # Define plots and summaries for count
    else if (input$summary_var == "count") {
      plot_types <- c("Line Plot", "Heatmap")
      updateSelectInput(session, "plot_type", choices = plot_types, selected = if (!is.null(current_plot_type) && current_plot_type %in% plot_types) current_plot_type else plot_types[1])
      updateSelectInput(session, "summary_type", choices = c("Contingency Table" = "contingency_table"))
    }
  })
  
  # Render table
  output$data_table <- renderTable({
    req(fetch_table_data())  # Ensure data is available
    fetch_table_data()  # Simply return the data frame
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
  
  # Render plot based on user input
  output$plot <- renderPlot({
    data <- fetch_data()
    
    # Extract date range and format as "Month YYYY"
    date_range_year <- range(as.numeric(as.character(data$record_calendar_year)), na.rm = TRUE)
    date_range_month <- range(data$record_calendar_month, na.rm = TRUE)
    min_date <- paste(month.name[date_range_month[1]], date_range_year[1])
    max_date <- paste(month.name[date_range_month[2]], date_range_year[2])
    date_range_text <- paste(min_date, "to", max_date)
    
    # Define all the different plots a user can see based on the selected variables
    if (input$summary_var == "count") {
      data <- data |>
        mutate(count = 1)
    }
    
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
    } else if (input$plot_type == "Boxplot") {
      ggplot(data, aes_string(x = input$contingency_var, y = input$summary_var, fill = input$contingency_var)) +
        geom_boxplot(alpha = 0.5) +
        labs(title = paste("Distribution of", input$summary_var, "by", input$contingency_var, "between", date_range_text), x = input$contingency_var, y = input$summary_var) +
        scale_y_log10(labels = if (input$summary_var == "net_collections_amt") dollar else identity) +
        theme_minimal() +
        theme(legend.position = "none")
    } else if (input$plot_type == "Line Plot") {
      if (input$contingency_var == "record_calendar_year") {
        ggplot(data, aes(x = as.factor(record_calendar_year), y = !!sym(input$summary_var), group = 1)) +
          stat_summary(fun = sum, geom = "line") +
          labs(title = paste(input$summary_var, "by Year between", date_range_text), x = "Year", y = input$summary_var) +
          scale_y_continuous(labels = if (input$summary_var == "net_collections_amt") dollar else identity) +
          theme_minimal()
      } else {
        ggplot(data, aes_string(x = "as.factor(record_calendar_year)", y = input$summary_var, color = input$contingency_var, group = input$contingency_var)) +
          stat_summary(fun = sum, geom = "line") +
          labs(title = paste(input$summary_var, "by Year and", input$contingency_var, "between", date_range_text), x = "Year", y = input$summary_var) +
          scale_y_continuous(labels = if (input$summary_var == "net_collections_amt") dollar else identity) +
          theme_minimal()
      }
    } else if (input$plot_type == "Heatmap") {
      y_axis <- if (input$contingency_var == "record_calendar_year") "record_calendar_month" else input$y_axis_var
      heatmap_data <- data |>
        group_by(!!sym(input$contingency_var), !!sym(y_axis)) |>
        summarize(total_value = sum(!!sym(input$summary_var), na.rm = TRUE), .groups = "drop")
      
      ggplot(heatmap_data, aes_string(x = input$contingency_var, y = y_axis, fill = "total_value")) +
        geom_tile(alpha = 0.5) +
        geom_text(aes(label = if (input$summary_var == "net_collections_amt") scales::dollar(total_value) else total_value), color = "black", size = 3) +
        labs(title = paste("Heatmap of", input$summary_var, "by", input$contingency_var, "and", y_axis, "between", date_range_text), x = input$contingency_var, y = y_axis, fill = input$summary_var) +
        scale_fill_gradient(low = "lightgreen", high = "darkgreen", labels = if (input$summary_var == "net_collections_amt") dollar else identity) +
        theme_minimal()
    }
  })
  
  # Create contingency and summary tables based on user selection
  output$summary_output <- renderPrint({
    data <- fetch_data()
    
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
      summary_data <- data |>
        count(!!sym(input$contingency_var))
    }
    
    # Print summaries
    print(summary_data)
  })
  
  # Generate dynamic UI for plot type
  output$plot_type_ui <- renderUI({
    if (input$summary_var == "net_collections_amt") {
      selectInput("plot_type", "Plot Type", choices = c("Histogram", "Boxplot", "Line Plot", "Heatmap"))
    } else if (input$summary_var == "count") {
      selectInput("plot_type", "Plot Type", choices = c("Line Plot", "Heatmap"))
    }
  })
  
  # Sync the main panel tabs with the sidebar tabs
  observeEvent(input$tabs, {
    updateTabsetPanel(session, "main_tabs", selected = input$tabs)
  })
})
