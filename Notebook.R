# clear environment
rm(list = ls())

# load libraries
library(tidyverse)
library(ggplot2)
library(RSQLite)
library(shiny)
library(RColorBrewer)
library(bslib)
library(extrafont)
library(ggchicklet)
library(crosstalk)
library(plotly)

font_import()
loadfonts()

# set file name where .db file is
filename <- "retail_store.db"

# initiate driver
driver <- RSQLite::dbDriver("SQLite")
# connect to database
db <- RSQLite::dbConnect(driver, dbname = filename)

# read individual tables
RSQLite::dbReadTable(db, "categories") |>
  as_tibble() -> cats

RSQLite::dbReadTable(db, "products") |>
  as_tibble() -> prod

RSQLite::dbReadTable(db, "customers") |>
  as_tibble() -> cust

RSQLite::dbReadTable(db, "orders") |>
  as_tibble() -> ords

RSQLite::dbReadTable(db, "order_details") |>
  as_tibble() -> dets

## Data Cleaning ###############################################################

## categories
head(cats)

## products
head(prod)
summary(prod)

## customers
head(cust)
summary(cust)

# turn gender, city, region and segment into factors
cust |>
  mutate(
    Gender = as_factor(Gender),
    City = as_factor(City),
    Region = as_factor(Region),
    CustomerSegment = as_factor(CustomerSegment)
  ) -> cust

# checking that factors work
levels(cust$Gender)
levels(cust$City)
levels(cust$Region)
levels(cust$CustomerSegment)

# turn sign up date into date object
cust |>
  mutate(
    SignUpDate = as_date(SignUpDate)
  ) -> cust

## orders
head(ords)
summary(ords)

# turn date and time into a single datetime object
ords |>
  mutate(
    OrderDateTime = paste(OrderDate, OrderTime),
    OrderDatetime = as_datetime(OrderDateTime)
  ) |>
  select(OrderID, CustomerID, OrderDatetime) -> ords

## order details
head(dets)
summary(dets)

# turn invalid dates into NA and make valid dates into datetimes
dets |>
  mutate(
    ReturnDateTime = paste(ReturnDate, ReturnTime),
    ReturnDateTime = as_datetime(ReturnDateTime),
    ReturnDateTime = na_if(year(ReturnDateTime), 9999)
  ) |>
  select(!c(ReturnDate, ReturnTime)) -> dets

# add columns for isdiscounted,  profit, and profit per unit
dets |>
  mutate(
    IsDiscounted = DiscountRate > 0,
    Profit = UnitPrice - UnitCost,
    ProfitPerUnit = Profit / Quantity
  ) -> dets

## Shiny Objects ###############################################################

## cust

# creating some ggplot templates
cust |>
  ggplot(aes(x = fct_infreq(Gender), fill = Gender)) +
  geom_bar() +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  xlab("Gender") +
  ylab("Frequency") +
  guides(fill = FALSE)

cust |>
  ggplot(aes(x = fct_relevel(CustomerSegment, "Standard", "Premium", "VIP"), fill = CustomerSegment)) +
  geom_bar() +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  xlab("Segment") +
  ylab("Frequency") +
  guides(fill = FALSE)

cust |>
  ggplot(aes(x = SignUpDate)) +
  geom_freqpoly()

cust |>
  mutate(
    SignUpDate = floor_date(SignUpDate, "month")
  ) |>
  count(SignUpDate) |>
  ggplot(aes(x = SignUpDate, y = n, fill = as_factor(year(SignUpDate)))) +
  geom_col() +
  scale_x_date(date_labels = "%Y-%b", date_breaks = "3 months") +
  guides(fill = FALSE) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Month", y = "Sign Ups", title = "Sign Ups Time Series")

## ords

ords |>
  mutate(
    OrderDatetime = floor_date(OrderDatetime, "month")
  ) |>
  count(OrderDatetime) |>
  ggplot(aes(x = OrderDatetime, y = n, fill = as_factor(year(OrderDatetime)))) +
  geom_col() +
  scale_x_date(date_labels = "%Y-%b", date_breaks = "3 months") +
  guides(fill = FALSE) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Month", y = "Orders", title = "Orders Time Series")

ords |>
  mutate(
    OrderDatetime = floor_date(OrderDatetime, "month")
  ) |>
  count(OrderDatetime) |>
  ggplot(aes(x = OrderDatetime, y = n)) +
  geom_line(linewidth = 0.75) +
  scale_x_date(date_labels = "%Y-%b", date_breaks = "3 months") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Time", y = "Orders", title = "Orders Time Series") +
  geom_vline(xintercept = dmy(paste0("1-1-", as.character(c(year(min(ords$OrderDatetime)):year(max(ords$OrderDatetime)))))),
             linetype = "dashed") +
  ylim(0, max(ords |>
                mutate(
                  OrderDatetime = as_date(floor_date(OrderDatetime, "month"))
                ) |>
                count(OrderDatetime) |> summarise(x = max(n))) + 100)

ords |>
  mutate(
    OrderDatetime = as_date(floor_date(OrderDatetime, "month"))
  ) |>
  count(OrderDatetime) |>
  summarise(x = max(n))

ui <- fluidPage(
  plotOutput("plot", click = "plot_click"),
  textOutput("info")
)

server <- function(input, output){
  clicked_point <- reactiveVal(NULL)
  
  observeEvent(input$plot_click, {
    np <- nearPoints(ords |>
                       mutate(
                         OrderDatetime = as_date(floor_date(OrderDatetime, "month"))
                       ) |>
                       count(OrderDatetime), input$plot_click, xvar = "OrderDatetime", yvar = "n", threshold = 10, maxpoints = 1)
    if (nrow(np) > 0) {
      clicked_point(np)
    } else {
      clicked_point(NULL)
    }
  })
  
  output$plot <- renderPlot({
    p <- ords |>
      mutate(
        OrderDatetime = as_date(floor_date(OrderDatetime, "month"))
      ) |>
      count(OrderDatetime) |>
      ggplot(aes(x = OrderDatetime, y = n)) +
      geom_line(linewidth = 0.75) +
      geom_point() +
      scale_x_date(date_labels = "%Y-%b", date_breaks = "3 months") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = "Time", y = "Orders", title = "Orders Time Series") +
      geom_vline(xintercept = dmy(paste0("1-1-", as.character(c(year(min(ords$OrderDatetime)):year(max(ords$OrderDatetime)))))),
                 linetype = "dashed") +
      ylim(0, max(ords |>
                    mutate(
                      OrderDatetime = as_date(floor_date(OrderDatetime, "month"))
                    ) |>
                    count(OrderDatetime) |> summarise(x = max(n))) + 100)
    
    if(!is.null(clicked_point())) {
      cp <- clicked_point()
      p <- p + geom_label(
        data = cp,
        aes(x = OrderDatetime, y = n,
            label = paste0("Month: ", format(OrderDatetime, "%b %Y"), "\nOrders: ", n)),
        nudge_y = 50,
        fill = "white", color = "black", label.size = 0.4
      )
    }
    p
  })
  
  output$info <- renderText({
    "Click on a point to see orders"
  })
}

shinyApp(ui, server)

year(as.POSIXct(ords$OrderDatetime, origin = "1970-01-01", tz = "UTC"))

ui_in <- page_fixed(
  dateRangeInput(
    inputId = "daterange",
    label = "Select Date Range",
    start = as_date(floor_date(min(ords$OrderDatetime))),
    end = as_date(floor_date(max(ords$OrderDatetime)))
  ),
  plotOutput("plot"),
  verbatimTextOutput("datetype")
)

server_in <- function(input, output){
  output$plot <- renderPlot({
    ords |>
      mutate(
        OrderDatetime = as_date(floor_date(OrderDatetime, "month"))
      ) |>
      count(OrderDatetime) |>
      ggplot(aes(x = OrderDatetime, y = n)) +
      geom_line(linewidth = 0.75) +
      geom_point() +
      xlim(input$daterange[[1]], input$daterange[[2]]) +
      scale_x_date(date_labels = "%Y-%b", date_breaks = "3 months") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = "Time", y = "Orders", title = "Orders Time Series") +
      geom_vline(xintercept = dmy(paste0("1-1-", as.character(c(year(min(ords$OrderDatetime)):year(max(ords$OrderDatetime)))))),
                 linetype = "dashed") +
      ylim(0, max(ords |>
                    mutate(
                      OrderDatetime = as_date(floor_date(OrderDatetime, "month"))
                    ) |>
                    count(OrderDatetime) |> summarise(x = max(n))) + 100)
    })
  
  output$datetype <- renderPrint({typeof(input$daterange[[1]])})
}

shinyApp(ui_in, server_in)

mUd7hwLtdEvLgeh3

write_csv(cats, "categories.csv")
write_csv(cust, "customers.csv")
write_csv(dets, "order_details.csv")
write_csv(ords, "orders.csv")
write_csv(prod, "products.csv")

categorical_colours <- c("#2DD4BF", "#FBBF24", "#F0A8C4", "#7FC7F2", "#F4A261", 
                         "#6C7BC2", "#8FBC94", "#E76F51", "#B79CF0", "#9CA0A8")

cats <- read_csv("https://retail-data-api.onrender.com/details")

cats[c(3, 4, 5, 6, 12, 13, 25)] -> cats

head(cats)

colnames(cats)[c(5:7)] <- c("OrderDate", "OrderTime", "CategoryName")

cats |>
  mutate(
    UnitProfit = UnitPrice - UnitCost,
    TotalPrice = UnitPrice * Quantity,
    TotalCost = UnitCost * Quantity,
    TotalProfit = TotalPrice - TotalCost,
    IsDiscounted = DiscountRate > 0
  ) -> cats

cats |>
  group_by(CategoryName) |>
  summarise(
    Profit = sum(TotalProfit)
  ) |>
  mutate(
    x_pos = as.numeric(factor(CategoryName))
  ) |>
  ggplot(aes(x = CategoryName, y = Profit, fill = CategoryName)) +
  geom_chicklet(radius = grid::unit(6, "pt"), color = NA) +
  geom_rect(
    aes(
      xmin = x_pos - 0.45, xmax = x_pos + 0.45,
      ymin = 0, ymax = Profit * 0.5  # covers just the bottom slice of each bar
    )) +
  scale_fill_manual(values = categorical_colours) +
  guides(fill = FALSE) +
  theme_minimal() +
  RD_theme_c() +
  labs(x = NULL, y = "Profit", title = "Profit by Category")

RD_theme_c <- function() {
  theme(text = element_text(family = "Manrope"),
        panel.background = element_rect(fill = "#1A1D22", colour = NA),
        plot.background = element_rect(fill = "#1A1D22", colour = NA),
        axis.text.x = element_text(angle = -45, color = "#9CA0A8", hjust = 0, margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")),
        axis.text.y = element_text(color = "#9CA0A8"),
        axis.title.y = element_text(color = "#9CA0A8", family = "Garamond", size = 16),
        axis.title.x = element_text(color = "#9CA0A8", family = "Garamond", size = 16),
        title = element_text(color = "#EDEDED", family = "Baskerville Old Face", size = 20, face = "bold"),
        panel.grid.major.x = element_line(colour = NA),
        panel.grid.minor.y = element_line(colour = "#2A2D33", linewidth = 0.5),
        panel.grid.major.y = element_line(colour = "#2A2D33", linewidth = 0.1)
  )
}

prod <- read_csv("https://retail-data-api.onrender.com/products")

colnames(prod)[c(5:8)] <- c("OrderDate", "OrderTime", "CategoryName", "ProductName")

prod |>
  mutate(
    UnitProfit = UnitPrice - UnitCost,
    TotalPrice = UnitPrice * Quantity,
    TotalCost = UnitCost * Quantity,
    TotalProfit = TotalPrice - TotalCost
  ) |>
  group_by(ProductName) |>
  summarise(
    Profit = sum(TotalProfit),
    Cost = sum(TotalCost),
    Price = sum(TotalPrice)
  ) -> prod_discrete

shared_df <- SharedData$new(prod_discrete)

ggplot(shared_df, aes(x = ProductName)) +
  geom_bar()

dets <- read_csv("https://retail-data-api.onrender.com/details")
shared_dets <- SharedData$new(dets)

test <- read_csv("https://retail-data-api.onrender.com/CatTest")

read_csv("https://retail-data-api.onrender.com/products")
read_csv("https://retail-data-api.onrender.com/categories")
read_csv("https://retail-data-api.onrender.com/customers")
read_csv("https://retail-data-api.onrender.com/orders")