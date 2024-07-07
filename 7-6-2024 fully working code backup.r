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
                      p("Note: If no data is displayed or an error is returned, it means no data meets the filter criteria or there's a problem with the API host. Try changing your filters or reloading to fix this"),
                      
                      # Show data table
                      dataTableOutput("data_table")
               )
             )
    ),
    
    # Create the data exploration tab
    tabPanel("Data Exploration",
             selectInput("summary_var", "Variable to Summarize", choices = c("Net Collections Amount" = "net_collections_amt", "Count of Records" = "count")),
             uiOutput("plot_type_ui"),  # Dynamic UI for plot type
             selectInput("contingency_var", "Variable to Partition by", 
                         choices = c("Electronic Category Description" = "electronic_category_desc", 
                                     "Channel Type Description" = "channel_type_desc", 
                                     "Tax Category Description" = "tax_category_desc",
                                     "Record Calendar Year" = "record_calendar_year")),
             conditionalPanel(
               condition = "input.plot_type == 'Heatmap' && input.contingency_var != 'record_calendar_year'",
               selectInput("y_axis_var", "Y-axis Variable", choices = c("Year" = "record_calendar_year", "Month" = "record_calendar_month"))
             ),
             conditionalPanel(
               condition = "input.plot_type == 'Histogram'",
               checkboxInput("facet_histogram", "Facet by selected variable", value = FALSE)
             ),
             plotOutput("plot"),
             h4("Summary Type"),
             selectInput("summary_type", "Type of Summary", choices = c("Mean/SD" = "mean_sd", "Percentiles" = "percentiles", "Contingency Table" = "contingency_table")),
             verbatimTextOutput("summary_output")
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

# Define server logic
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
        record_calendar_year = as.factor(record_calendar_year),  # Convert to factor for plotting
        record_calendar_month = as.integer(record_calendar_month)
      )
    
    return(data)
  })
  
  # Reactive expression to fetch data for the table based on user-selected columns
  fetch_table_data <- reactive({
    data <- fetch_data()
    
    # Select only the columns chosen by the user
    selected_columns <- c("record_date", "net_collections_amt", input$columns)
    data <- data |> select(all_of(selected_columns))
    
    return(data)
  })
  
  # Update plot types based on summary variable
  observeEvent(input$summary_var, {
    if (input$summary_var == "net_collections_amt") {
      updateSelectInput(session, "plot_type", choices = c("Histogram", "Boxplot", "Line Plot", "Heatmap"))
      updateSelectInput(session, "summary_type", choices = c("Mean/SD" = "mean_sd", "Percentiles" = "percentiles", "Contingency Table" = "contingency_table"))
    } else if (input$summary_var == "count") {
      updateSelectInput(session, "plot_type", choices = c("Line Plot", "Heatmap"))
      updateSelectInput(session, "summary_type", choices = c("Contingency Table" = "contingency_table"))
    }
  })
  
  # Render data table without "Show # entries" and "Search" options
  output$data_table <- renderDataTable({
    datatable(fetch_table_data(), options = list(dom = 't', pageLength = 20))  # 't' removes the table control elements
  })
  
  # Download handler
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
  
  # Generate summaries based on user input
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
          p25 = quantile(net_collections_amt, 0.25, na.rm = TRUE),
          median_amt = median(net_collections_amt, na.rm = TRUE),
          p75 = quantile(net_collections_amt, 0.75, na.rm = TRUE),
          p100 = quantile(net_collections_amt, 1, na.rm = TRUE)
        )
    } else if (input$summary_type == "contingency_table") {
      summary_data <- data |>
        count(!!sym(input$contingency_var))
    }
    
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

