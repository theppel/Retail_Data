# clear environment
rm(list = ls())

# load libraries
library(tidyverse)
library(ggplot2)
library(RSQLite)
library(shiny)
library(RColorBrewer)
library(bslib)

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

df <- data.frame(x = 1:20, y = cumsum(rnorm(20)))

ui_test <- fluidPage(
  plotOutput("plot", click = "plot_click"),
  verbatimTextOutput("info")
)

server_test <- function(input, output) {
  output$plot <- renderPlot({
    ggplot(df, aes(x, y)) +
      geom_line() +
      geom_point()  # nearPoints() works best with actual point geoms
  })
  
  output$info <- renderPrint({
    np <- nearPoints(df, input$plot_click, xvar = "x", yvar = "y", threshold = 10, maxpoints = 1)
    if (nrow(np) == 0) {
      "Click on a point on the line to see coordinates"
    } else {
      np
    }
  })
}

shinyApp(ui_test, server_test)

year(as.POSIXct(ords$OrderDatetime, origin = "1970-01-01", tz = "UTC"))

