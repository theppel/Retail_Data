# clear environment
rm(list = ls())

# load libraries
library(tidyverse)
library(ggplot2)
library(lme4)
library(lmerTest)
library(RSQLite)

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

paste(ords$OrderDate, ords$OrderTime)

# turn date and time into a sinfle datetime object
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

# add columns for isdiscounted and profit, and profit per unit
dets |>
  mutate(
    IsDiscounted = DiscountRate > 0,
    Profit = UnitPrice - UnitCost,
    ProfitPerUnit = Profit / Quantity
  ) -> dets