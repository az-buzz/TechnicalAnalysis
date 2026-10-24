# BDA400 Assignment 6: Technical Analysis using R, Visualization Phase
# Name: Kessar Atchia

# Install packages if missing (uncomment if running for the first time)
# install.packages(c("shiny", "ggplot2", "quantmod"))

library(shiny)
library(ggplot2)
library(quantmod)

# Define UI for the application
ui <- fluidPage(
  titlePanel("Dynamic Portfolio Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("stock_symbol", "Stock Symbol:", value = "AAPL"),
      dateRangeInput("date_range", "Select Date Range:", 
                     start = "2023-01-01", end = "2023-07-01"),
      
      selectInput("time_frame", "Select Time Frame:", 
                  choices = c("Daily", "Weekly", "Monthly")),
      
      checkboxGroupInput("technical_indicators", "Overlay Technical Indicators:",
                         choices = c("Moving Averages", "RSI", "MACD")),
      
      actionButton("update", "Update Chart")
    ),
    
    mainPanel(
      plotOutput("stock_chart", height = "600px")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Reactive event to fetch data only when update button is clicked
  stock_data <- eventReactive(input$update, {
    req(input$stock_symbol)
    
    # Fetch historical stock data using quantmod
    data <- getSymbols(input$stock_symbol, src = "yahoo", 
                       from = input$date_range[1], 
                       to = input$date_range[2], 
                       auto.assign = FALSE)
    
    # Adjust for selected time frame
    if (input$time_frame == "Weekly") {
      data <- to.weekly(data, indexAt = "lastof", name = input$stock_symbol)
    } else if (input$time_frame == "Monthly") {
      data <- to.monthly(data, indexAt = "lastof", name = input$stock_symbol)
    }
    
    return(data)
  }, ignoreNULL = FALSE)
  
  output$stock_chart <- renderPlot({
    raw_data <- stock_data()
    req(raw_data)
    
    # Convert quantmod xts object to data frame for ggplot2
    df <- data.frame(Date = index(raw_data), coredata(raw_data))
    # Rename columns dynamically based on the symbol
    colnames(df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
    
    # --- Implementing Trading Rules ---
    # Moving Average Crossover Trading Rule
    df$short_ma <- as.numeric(SMA(df$Close, n = 20))
    df$long_ma <- as.numeric(SMA(df$Close, n = 50))
    
    # Generate signals based on MA crossover
    df$Signal <- ifelse(df$short_ma > df$long_ma, "Buy", 
                 ifelse(df$short_ma < df$long_ma, "Sell", "Hold"))
    
    # To prevent chart clutter, only annotate when the signal changes (the actual crossover)
    df$Signal_Change <- c(NA, diff(as.numeric(as.factor(df$Signal))))
    df$Annotation <- ifelse(!is.na(df$Signal_Change) & df$Signal_Change != 0, df$Signal, NA)
    
    # --- Visualizing Stock Data ---
    # Base ggplot
    p <- ggplot(data = df, aes(x = Date, y = Close)) +
      geom_line(color = "black", linewidth = 1) +
      theme_minimal() +
      labs(title = paste(input$stock_symbol, "Stock Price & Trading Signals"),
           x = "Date", y = "Closing Price")
    
    # --- Overlay Technical Indicators ---
    if ("Moving Averages" %in% input$technical_indicators) {
      # Add Moving Averages
      p <- p + geom_line(aes(y = short_ma, color = "20-Day SMA"), linetype = "dashed", linewidth = 1) +
               geom_line(aes(y = long_ma, color = "50-Day SMA"), linetype = "dashed", linewidth = 1) +
               scale_color_manual(name = "Indicators", values = c("20-Day SMA" = "blue", "50-Day SMA" = "red"))
    }
    
    if ("RSI" %in% input$technical_indicators) {
      # Add RSI (Scaled to fit the price chart for overlay visualization)
      df$rsi_val <- as.numeric(RSI(df$Close, n = 14))
      scale_factor <- max(df$Close, na.rm = TRUE) / 100
      p <- p + geom_line(aes(y = rsi_val * scale_factor), color = "purple", alpha = 0.5, linewidth = 1)
    }
    
    if ("MACD" %in% input$technical_indicators) {
      # Add MACD (Shifted to overlay on the price chart)
      macd_res <- MACD(df$Close, nFast = 12, nSlow = 26, nSig = 9, maType = "EMA")
      df$macd_val <- as.numeric(macd_res[,1])
      p <- p + geom_line(aes(y = macd_val + mean(df$Close, na.rm=TRUE)), color = "green", alpha = 0.5, linewidth = 1)
    }
    
    # --- Annotate the bars on the chart ---
    p <- p + geom_text(aes(label = Annotation, color = Annotation), 
                       vjust = -1.5, size = 5, fontface = "bold", na.rm = TRUE) +
             scale_color_manual(values = c("Buy" = "darkgreen", "Sell" = "red", "Hold" = "grey", 
                                           "20-Day SMA" = "blue", "50-Day SMA" = "red"), 
                                limits = force)
    
    # Return the ggplot object
    print(p)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
