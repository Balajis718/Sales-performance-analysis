
# Load required packages
library(shiny)
library(tidyverse)
library(lubridate)

# Sample UI and server for dashboard
ui <- fluidPage(
  titlePanel("Sales Overview Dashboard (Task 2a)"),
  sidebarLayout(
    sidebarPanel(
      selectInput("product_filter", "Select Product:", choices = NULL, multiple = TRUE),
      selectInput("country_filter", "Select Country:", choices = NULL, multiple = TRUE)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Sales by Product", plotOutput("productPlot")),
        tabPanel("Sales Over Time", plotOutput("timePlot")),
        tabPanel("Customer Category", plotOutput("categoryPlot")),
        tabPanel("Sales by Region", plotOutput("regionPlot")),
        tabPanel("Heatmap", plotOutput("heatmapPlot"))
      )
    )
  )
)

server <- function(input, output, session) {
  # Load your data here (replace with actual CSV file or reactive input)
  # sales_data <- read_csv("your_sales_data.csv")

  # Simulated placeholder data for structure (remove in real app)
  sales_data <- tibble(
    Product = rep(c("Base", "Enhanced", "Workstation"), each = 100),
    Customer_Category = rep(c("Retail", "Wholesale", "Public", "Education"), 75),
    Sticker_Price = runif(300, 500, 1500),
    Discount = runif(300, 0.05, 0.2),
    Promo_Campaign = sample(c("yes", "no"), 300, replace = TRUE),
    Quantity = sample(1:10, 300, replace = TRUE),
    Time = sample(seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "week"), 300, replace = TRUE),
    Country = sample(c("UK", "Germany", "France", "UK&EU"), 300, replace = TRUE)
  )

  # Revenue Calculation
  sales_data <- sales_data %>%
    mutate(
      promo_discount = if_else(Promo_Campaign == "yes", 0.05, 0),
      total_discount = Discount + promo_discount,
      unit_price = Sticker_Price * (1 - total_discount),
      sales_revenue = unit_price * Quantity
    )

  updateSelectInput(session, "product_filter", choices = unique(sales_data$Product), selected = unique(sales_data$Product))
  updateSelectInput(session, "country_filter", choices = unique(sales_data$Country), selected = unique(sales_data$Country))

  filtered_data <- reactive({
    sales_data %>%
      filter(Product %in% input$product_filter, Country %in% input$country_filter)
  })

  output$productPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = Product, y = sales_revenue, fill = Product)) +
      geom_bar(stat = "summary", fun = "sum") +
      labs(title = "Sales Revenue by Product Type", y = "Revenue", x = "Product") +
      theme_minimal()
  })

  output$timePlot <- renderPlot({
    ggplot(filtered_data(), aes(x = Time, y = sales_revenue)) +
      geom_line(stat = "summary", fun = "sum", color = "steelblue") +
      labs(title = "Sales Revenue Over Time", x = "Time", y = "Revenue") +
      theme_minimal()
  })

  output$categoryPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = Customer_Category, y = sales_revenue, fill = Product)) +
      geom_bar(stat = "summary", fun = "sum", position = "dodge") +
      labs(title = "Sales by Customer Category", x = "Category", y = "Revenue") +
      theme_minimal()
  })

  output$regionPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = Country, y = sales_revenue, fill = Country)) +
      geom_bar(stat = "summary", fun = "sum") +
      labs(title = "Sales by Market Region", x = "Country", y = "Revenue") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })

  output$heatmapPlot <- renderPlot({
    heatmap_data <- filtered_data() %>%
      group_by(Customer_Category, Product) %>%
      summarise(Total_Sales = sum(sales_revenue)) %>%
      ungroup()

    ggplot(heatmap_data, aes(x = Product, y = Customer_Category, fill = Total_Sales)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "lightblue", high = "darkblue") +
      labs(title = "Heatmap: Product Sales by Customer Category") +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)
