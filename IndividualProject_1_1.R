customers = read.csv("../Data/customers.csv")
str(customers)

products = read.csv("../Data/products.csv")
str(products)

transactions = read.csv("../Data/transactions -- January 2020.csv")
str(transactions)

views = read.csv("../Data/views -- January 2020.csv")
str(views)

##Question 1 - During the first 7 days of the month of January, which product had the most views? Enter
## the products identifier.
library(dplyr)

# Filter views for the first 7 days of January
start_date <- as.POSIXct("2020-01-01T00:00:00", format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
end_date <- as.POSIXct("2020-01-07T23:59:59", format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")

filtered_views <- views %>%
  filter(as.POSIXct(time) >= start_date & as.POSIXct(time) <= end_date)

# Count views for each product
product_views <- filtered_views %>%
  group_by(product_id) %>%
  summarize(views = n())

# Find the product with the most views
product_with_most_views <- product_views %>%
  arrange(desc(views)) %>%
  head(1)

# Print the product with the most views
product_with_most_views

##--------------------

# Convert 'time' column to POSIXct format
views$time <- as.POSIXct(views$time, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")

# Define the start and end timestamps for the first 7 days of January
start_date <- as.POSIXct("2020-01-01T00:00:00", format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
end_date <- as.POSIXct("2020-01-07T23:59:59", format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")

# Filter views for the first 7 days of January
filtered_views <- views[views$time >= start_date & views$time <= end_date, ]

# Count views for each product
product_views <- table(filtered_views$product_id)

# Find the product with the most views
most_viewed_product <- names(product_views[which.max(product_views)])

# Print the product with the most views
most_viewed_product

#Answer - hZmcs0gqq3PNAWCx

##Question 2 - Looking only at coats, identify the product with the most views for the whole month of January.
## How many times was this product viewed?

# Convert 'time' column to POSIXct format
views$time <- as.POSIXct(views$time, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")

# Define the start and end timestamps for the entire month of January
start_date <- as.POSIXct("2020-01-01T00:00:00", format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
end_date <- as.POSIXct("2020-01-31T23:59:59", format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")

# Filter views for the entire month of January and category "coats"
january_coat_views <- views[views$time >= start_date & views$time <= end_date & views$product_id %in% products$product_id[products$category == "coat"], ]

# Count views for each coat product
coat_product_views <- table(january_coat_views$product_id)
coat_product_views

# Find the coat product with the most views
most_viewed_coat_product <- names(coat_product_views[which.max(coat_product_views)])

# Determine how many times the most viewed coat product was viewed
views_count <- max(coat_product_views)

# Print the most viewed coat product and its view count
most_viewed_coat_product
views_count

##Answer - Most viewed "coat" product: 120JD3Voq3D23Nx7 with 697 views

##Question 3 - During the month of January, calculate the total revenue for shoes. Enter your answer
#as a number rounded to two decimal places.

# Convert 'time' column in transactions and views to POSIXct format
transactions$time <- as.POSIXct(transactions$time, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")

# Define the start and end timestamps for the entire month of January
start_date <- as.POSIXct("2020-01-01T00:00:00", format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
end_date <- as.POSIXct("2020-01-31T23:59:59", format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")

# Filter transactions for the entire month of January and category "shoes"
january_shoe_transactions <- transactions[
  transactions$time >= start_date & transactions$time <= end_date &
    transactions$product_id %in% products$product_id[products$category == "shoes"],
]

# Calculate total revenue for shoes in January
total_revenue_january_shoes <- sum(january_shoe_transactions$price * january_shoe_transactions$quantity)
total_revenue_january_shoes

# Round the total revenue to two decimal places
total_revenue_january_shoes <- sprintf("%.2f", total_revenue_january_shoes)

# Print the total revenue for shoes in January
total_revenue_january_shoes

##Answer - 3,555,392.49

##Question 4 - Among customers with at least one transaction, what was the average spending per customer
#during the month of January? Calculate this value to two decimal places.

# Convert 'time' column in transactions to POSIXct format
transactions$time <- as.POSIXct(transactions$time, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")

# Define the start and end timestamps for the entire month of January
start_date <- as.POSIXct("2020-01-01T00:00:00", format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
end_date <- as.POSIXct("2020-01-31T23:59:59", format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")

# Filter transactions for the entire month of January
january_transactions <- transactions[
  transactions$time >= start_date & transactions$time <= end_date,
]

# Find customers with at least one transaction in January
customers_with_transactions <- unique(january_transactions$customer_id)

# Calculate total spending for each customer
customer_total_spending <- sapply(customers_with_transactions, function(customer_id) {
  # Filter transactions for the current customer
  customer_transactions <- january_transactions[january_transactions$customer_id == customer_id, ]
  
  # Calculate total spending for the customer
  total_spending <- sum(customer_transactions$price * customer_transactions$quantity, na.rm = TRUE)
  
  return(total_spending)
})

# Calculate the average spending per customer
average_spending_per_customer <- mean(customer_total_spending, na.rm = TRUE)

# Round the average spending to two decimal places
average_spending_per_customer <- round(average_spending_per_customer, 2)

# Print the average spending per customer in January
average_spending_per_customer

##Answer - 495.91

##Question 5 - What percent of the total revenue in the month of January came from female (F) employees?
#Express this number as a percentage (on a scale between 0 to 100) that is rounded to one decimal place.

# Convert 'time' column in transactions to POSIXct format
transactions$time <- as.POSIXct(transactions$time, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")

# Define the start and end timestamps for the entire month of January
start_date <- as.POSIXct("2020-01-01T00:00:00", format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
end_date <- as.POSIXct("2020-01-31T23:59:59", format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")

# Filter transactions for the entire month of January
january_transactions <- transactions[
  transactions$time >= start_date & transactions$time <= end_date,
]

# Calculate total revenue for all transactions in January
total_revenue_january <- sum(january_transactions$price * january_transactions$quantity)

# Calculate total revenue for transactions made by female (F) customers in January
female_transactions <- january_transactions[january_transactions$customer_id %in% customers$customer_id[customers$gender == "F"], ]
total_revenue_female <- sum(female_transactions$price * female_transactions$quantity)

# Calculate the percentage of total revenue that came from female customers
percentage_revenue_from_female <- (total_revenue_female / total_revenue_january) * 100

# Round the percentage to one decimal place
percentage_revenue_from_female <- round(percentage_revenue_from_female, 1)

# Print the percentage of total revenue from female customers in January
percentage_revenue_from_female

#Answer - 50.8%

##Question 6 - Using Linear Regression, what is the increase in monthly average spending associated with an
#extra ten thousand dollars of income? Use only the January data to build the model. Make sure your model
#also includes age, gender and region as inputs to adjust for. Make sure to round your answer to 2 
#decimal places.

# Convert 'time' column in transactions to POSIXct format
transactions$time <- as.POSIXct(transactions$time, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")

# Define the start and end timestamps for the entire month of January
start_date <- as.POSIXct("2020-01-01T00:00:00", format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
end_date <- as.POSIXct("2020-01-31T23:59:59", format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")

# Filter transactions for the entire month of January
january_transactions <- transactions[
  transactions$time >= start_date & transactions$time <= end_date,
]

# Calculate total spending for each customer ID
total_spending <- january_transactions %>%
  group_by(customer_id) %>%
  summarize(total_spending = sum(price * quantity))

# Merge total spending with customer data (age, gender, income, region)
january_data <- left_join(customers, total_spending, by = "customer_id")

# Load necessary libraries if not already loaded
# install.packages("lmtest")  # Uncomment and run if you haven't installed the 'lmtest' package
library(lmtest)

# Build a linear regression model
model <- lm(total_spending ~ income + age + gender + region, data = january_data)

# Extract the coefficient for income from the summary
summary_result <- summary(model)
income_coefficient <- summary_result$coefficients["income", "Estimate"]

# Calculate the increase in monthly average spending associated with an extra ten thousand dollars of income
increase_in_spending <- income_coefficient * 10000  # 10,000 dollars

# Round the result to two decimal places
increase_in_spending <- round(increase_in_spending, 2)

# Print the increase in monthly average spending
increase_in_spending

##Answer - 11.02

##Question 7 - Among customers who viewed at least 1 product during the month of January, what percentage
# of them also made at least one purchase in January? Calculate this value as a percentage between 
# 0 and 100.

# Convert 'time' columns to POSIXct format
transactions$time <- as.POSIXct(transactions$time, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
views$time <- as.POSIXct(views$time, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")

# Define the start and end timestamps for the entire month of January
start_date <- as.POSIXct("2020-01-01T00:00:00", format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
end_date <- as.POSIXct("2020-01-31T23:59:59", format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")

# Filter views and transactions for the entire month of January
january_views <- views[
  views$time >= start_date & views$time <= end_date,
]
january_transactions <- transactions[
  transactions$time >= start_date & transactions$time <= end_date,
]

# Find customers who viewed at least one product in January
viewed_customers <- unique(january_views$customer_id)

# Find customers who made at least one purchase in January
purchased_customers <- unique(january_transactions$customer_id)

# Calculate the intersection of viewed and purchased customers
intersection_customers <- length(intersect(viewed_customers, purchased_customers))

# Calculate the percentage of customers in the intersection
percentage_intersection <- (intersection_customers / length(viewed_customers)) * 100

# Round the result to two decimal places
percentage_intersection <- round(percentage_intersection, 1)

# Print the percentage of customers who both viewed and purchased in January
percentage_intersection

##Answer - 52.6%

##Question 8 - Consider customers who are at least 35 years old and less than 50 year old and viewed at 
# least one product during the month of January. Within this group, what is the median number of
# unique products viewed per customer?

library(dplyr)

# Convert 'time' column in views to POSIXct format
views$time <- as.POSIXct(views$time, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")

# Define the start and end timestamps for the entire month of January
start_date <- as.POSIXct("2020-01-01T00:00:00", format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
end_date <- as.POSIXct("2020-01-31T23:59:59", format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")

# Filter views for the entire month of January
january_views <- views %>%
  filter(time >= start_date, time <= end_date)

# Filter customers who are at least 35 and less than 50 years old
filtered_customers <- customers %>%
  filter(age >= 35, age < 50)

# Find customers who viewed at least one product in January
viewed_customers <- january_views %>%
  distinct(customer_id)

# Calculate the median number of unique products viewed per customer
median_unique_products_viewed <- january_views %>%
  filter(customer_id %in% viewed_customers$customer_id) %>%
  distinct(customer_id, product_id) %>%
  group_by(customer_id) %>%
  summarize(unique_products_viewed = n()) %>%
  summarise(median_unique_products = median(unique_products_viewed))

# Print the median number of unique products viewed per customer
median_unique_products_viewed$median_unique_products

##Answer - 8

##Question 9 - In the month of January, what is the correlation between a user's total page views 
# and total spending? Look only at customers with at least 1 view for the month. 
# For customers without a transaction, include their spending as zero. 
# Use only customers who viewed at least 1 product in January. Enter your answer as a decimal number
# between -1 and 1, rounded to 2 decimal places.

# Convert 'time' columns in views and transactions to POSIXct format
views$time <- as.POSIXct(views$time, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
transactions$time <- as.POSIXct(transactions$time, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")

# Define the start and end timestamps for the entire month of January
start_date <- as.POSIXct("2020-01-01T00:00:00", format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
end_date <- as.POSIXct("2020-01-31T23:59:59", format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")

# Filter views and transactions for the entire month of January
january_views <- views[
  views$time >= start_date & views$time <= end_date,
]
january_transactions <- transactions[
  transactions$time >= start_date & transactions$time <= end_date,
]

# Find customers who viewed at least one product in January
viewed_customers <- unique(january_views$customer_id)

# Calculate total spending for each customer
total_spending <- january_transactions %>%
  filter(customer_id %in% viewed_customers) %>%
  group_by(customer_id) %>%
  summarize(total_spending = sum(price * quantity))

# Calculate total page views for each customer
total_page_views <- january_views %>%
  filter(customer_id %in% viewed_customers) %>%
  group_by(customer_id) %>%
  summarize(total_page_views = n())

# Combine total page views and total spending into a single data frame
customer_data <- merge(total_page_views, total_spending, by = "customer_id", all = TRUE)

# Replace missing spending values with 0
customer_data[is.na(customer_data$total_spending), "total_spending"] <- 0

# Calculate the correlation between total page views and total spending
correlation <- cor(customer_data$total_page_views, customer_data$total_spending, use = "complete.obs")

# Round the correlation to two decimal places
correlation <- round(correlation, 2)

# Print the correlation
correlation

#Answer - 0.82

##Question 10 - Which customer purchased the largest number of coats during the month of January?
#Enter the customer's identifier as exact text with no leading or trailing spaces.

# Convert 'time' column in transactions to POSIXct format
transactions$time <- as.POSIXct(transactions$time, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")

# Define the start and end timestamps for the entire month of January
start_date <- as.POSIXct("2020-01-01T00:00:00", format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
end_date <- as.POSIXct("2020-01-31T23:59:59", format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")

# Filter transactions for the entire month of January and only for coats
january_coats_transactions <- transactions[
  transactions$time >= start_date & transactions$time <= end_date & transactions$product_id %in% products$product_id[products$category == "coat"],
]

# Group transactions by customer and calculate the total quantity of coats purchased
customer_coats_summary <- january_coats_transactions %>%
  group_by(customer_id) %>%
  summarize(total_coats_purchased = sum(quantity))

# Find the customer with the largest quantity of coats purchased
customer_with_most_coats <- customer_coats_summary %>%
  filter(total_coats_purchased == max(total_coats_purchased)) %>%
  select(customer_id)

# Extract the customer identifier
largest_coats_customer_id <- customer_with_most_coats$customer_id

# Print the customer identifier
largest_coats_customer_id

##Answer - tDezWLIaLmm8






