#Question 1 - During the first 7 days of the month of January, what were the 10 most viewed products? 
# Show the results in a table with the product's identifier, category, and count of the number of views.
library(data.table)
library(DT)
library(dplyr)
library(lmtest)

# Split the 'time' column into 'date' and 'time' columns
transactions$date <- as.Date(substring(transactions$time, 1, 10))
transactions$time <- format(as.POSIXct(transactions$time, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"), format = "%H:%M:%S")

# Split the 'time' column into 'date' and 'time' columns
views$date <- as.Date(substring(views$time, 1, 10))
views$time <- format(as.POSIXct(views$time, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"), format = "%H:%M:%S")

# Define the start and end timestamps for the first 7 days of January
start_date <- "2020-01-01"
end_date <-   "2020-01-07"

# Create a data.table for 'views'
views_dt <- as.data.table(views)

# Filter views for the first 7 days of January
first_week_views <- views_dt[date >= start_date & date <= end_date]

# Count the number of views for each product
product_views_count <- first_week_views[, .(views_count = .N), by = product_id]

# Select the top 10 most viewed products
top_10_viewed_products <- product_views_count[order(-views_count)][1:10]

# Join with the 'products' data to get product information
top_10_viewed_products <- merge(top_10_viewed_products, products, by = "product_id")

# Sort the top 10 viewed products in descending order
top_10_viewed_products <- top_10_viewed_products[order(-views_count)]

# Display the results using DT
datatable(top_10_viewed_products, 
          options = list(pageLength = 10, scrollX = TRUE))


#####################################
#Question 2 - During the whole month, what were the 10 most viewed products for each category?  
#Show the results in separate tables by category. Include only the product's identifier and the
# count of the number of views.

# Define the start and end date for the entire month of January
start_date <- "2020-01-01"
end_date <- "2020-01-31"

# Create a data.table for 'views'
views_dt <- as.data.table(views)

# Filter views for the entire month of January
january_views <- views_dt[date >= start_date & date <= end_date]

# Join 'january_views' with 'products' to include category information
january_views_with_category <- merge(january_views, products, by = "product_id")

# Count the number of views for each product in each category
category_views_count <- january_views_with_category[, .(views_count = .N), by = .(category, product_id)]
category_views_count[1:10]

# Define a function to get the top N viewed products for each category
get_top_n_viewed <- function(category_data, n = 10) {
  return(head(category_data[order(-views_count)], n))
}

# Get the top 10 most viewed products for each category
top_10_viewed_by_category <- category_views_count[, get_top_n_viewed(.SD,10), by = category]
top_10_viewed_by_category

# Display the results in separate tables by category
unique_categories <- unique(top_10_viewed_by_category$category)
unique_categories

display_Top_10(unique_categories,top_10_viewed_by_category)

for (unique_category in unique_categories) {
  cat("Category:", unique_category, "\n")
  category_data <- top_10_viewed_by_category[unique_category == top_10_viewed_by_category$category, ][1:10]
  print(category_data)
  cat("\n")
}

cat("Category:", "Shirt", "\n")
category_data <- top_10_viewed_by_category[top_10_viewed_by_category$category == "shirt",][1:10]
print(category_data)

cat("Category:", "Pants", "\n")
category_data <- top_10_viewed_by_category[top_10_viewed_by_category$category == "pants",][1:10]
print(category_data)

cat("Category:", "Coat", "\n")
category_data <- top_10_viewed_by_category[top_10_viewed_by_category$category == "coat",][1:10]
print(category_data)

cat("Category:", "Shoes", "\n")
category_data <- top_10_viewed_by_category[top_10_viewed_by_category$category == "shoes",][1:10]
print(category_data)

cat("Category:", "Hat", "\n")
category_data <- top_10_viewed_by_category[top_10_viewed_by_category$category == "hat",][1:10]
print(category_data)

###################################################
#Question 3 -  What was the total revenue for each category during the month? 
# Show the results in a single table sorted in decreasing order.

# Create a data.table for 'transactions' and 'products'
transactions_dt <- as.data.table(transactions)
products_dt <- as.data.table(products)

# Filter transactions for the entire month of January
january_transactions <- transactions_dt[date >= start_date & date <= end_date]

# Perform an inner join between 'transactions_dt' and 'products_dt' on 'product_id'
merged_dt <- january_transactions[products_dt, on = "product_id"]

# Calculate revenue for each transaction
merged_dt[, revenue := price * quantity]

# Sum revenue by category and sort in decreasing order
category_revenue <- merged_dt[, .(total_revenue = sum(revenue)), by = category]

# Format total_revenue with commas
category_revenue[, total_revenue := format(total_revenue, big.mark = ",", scientific = FALSE)]

# Sort in descending order
category_revenue <- category_revenue[order(-total_revenue)]

# Print the results
print(category_revenue)

#################################################
#Question 4 - Among customers with at least one transaction, show the average, median, 
# and standard deviation of monthly spending per customer.

#Feel it's incorrect
# Create data.tables for 'transactions' and 'products'
transactions_dt <- as.data.table(transactions)

# Filter transactions for the entire month of January
january_transactions <- transactions_dt[date >= start_date & date <= end_date]

# Calculate monthly spending per customer
monthly_spending <- january_transactions[, .(total_spending = sum(price * quantity)), by = .(customer_id, year_month = format(time, "%Y-%m"))]

# Filter out customers with no transactions in January
customers_with_transactions <- monthly_spending[, .N, by = customer_id][N > 0]$customer_id

# Calculate average, median, and standard deviation of monthly spending for eligible customers
customer_stats <- monthly_spending[customer_id %in% customers_with_transactions, 
                                   .(average = mean(total_spending), 
                                     median = median(total_spending), 
                                     stddev = sd(total_spending)), 
                                   by = customer_id]
customer_stats

# Calculate the overall average, median, and standard deviation
overall_stats <- customer_stats[, .(average = mean(average), median = median(median), stddev = sd(stddev))]

# Print the results
print(overall_stats)

#Right approach
# Create a data.table for 'transactions'
transactions_dt <- as.data.table(transactions)

# Filter transactions for the entire month of January
january_transactions <- transactions_dt[date >= start_date & date <= end_date]

# Calculate monthly spending per customer
customer_stats <- january_transactions[, .(average = mean(price * quantity), 
                                           median = median(price * quantity), 
                                           stddev = if (.N > 1) sd(price * quantity) else NA_real_), 
                                       by = customer_id]

# Print the results
print(customer_stats)

#2nd approach for overall data

# Calculate total spending for each customer and count the number of transactions
customer_spending_count <- transactions_for_period[, .(spending = sum(price * quantity), count = .N), by = customer_id]

# Filter customers with at least one transaction
customers_with_transactions <- customer_spending_count[count >= 1]

# Calculate the average, median, and standard deviation of spending
result <- customers_with_transactions[, .(
  Average = round(mean(spending), 2),
  Median = round(median(spending), 2),
  Standard_Deviation = round(sd(spending), 2)
)]

# Print the results
cat("The average spending is: $", result$Average, "\n")
cat("The median spending is: $", result$Median, "\n")
cat("The standard deviation is: $", result$Standard_Deviation, "\n")

###########################################################
#Question 5 - What percent of the monthly revenue was attributed to each category of gender? 
# Show the amount of revenue and the percentage for each group.

# Create data.tables for 'transactions', 'products', 'customers'
transactions_dt <- as.data.table(transactions)
products_dt <- as.data.table(products)
customers_dt <- as.data.table(customers)

# Filter transactions for the entire month of January
january_transactions <- transactions_dt[date >= start_date & date <= end_date]

# Perform inner joins to combine 'january_transactions', 'products', 'customers', and 'views'
merged_dt <- january_transactions[products_dt, on = "product_id"]
merged_dt <- merged_dt[customers_dt, on = "customer_id"]

# Calculate revenue for each transaction
merged_dt[, revenue := price * quantity]

# Sum revenue by gender and category
gender_category_revenue <- merged_dt[, .(total_revenue = sum(revenue)), by = .(gender, category)]

# Filter out rows where 'category' is NA
gender_category_revenue <- gender_category_revenue[!is.na(category)]

# Calculate the total revenue for the entire month
total_monthly_revenue <- sum(gender_category_revenue$total_revenue, na.rm = TRUE)

# Calculate the percentage of revenue for each group
gender_category_revenue[, percentage := (total_revenue / total_monthly_revenue) * 100]

# Sort revenue by gender
setorder(gender_category_revenue, gender)

# Format total_revenue with commas
gender_category_revenue[, total_revenue := format(total_revenue, big.mark = ",", scientific = FALSE)]

# Round the percentage column to 2 decimal places
gender_category_revenue[, percentage := round.numerics(percentage, 2)]

# Print the results
print(gender_category_revenue) ##category gender wise

##gender wise (categories combined)

# Perform left joins to combine transactions_for_period, 'products', 'customers'
merged_dt <- transactions_for_period[products_dt, on = "product_id", nomatch = 0L]
merged_dt <- merged_dt[customers_dt, on = "customer_id", nomatch = 0L]

# Calculate revenue for each transaction
merged_dt[, revenue := price * quantity]

# Sum revenue by gender and category
gender_category_revenue <- merged_dt[, .(total_revenue = sum(revenue)), by = .(gender)]

# Calculate the total revenue for the selected month
total_monthly_revenue <- sum(gender_category_revenue$total_revenue, na.rm = TRUE)

# Calculate the percentage of revenue for each group
gender_category_revenue[, percentage := (total_revenue / total_monthly_revenue) * 100]

# Round the percentage column to 2 decimal places
gender_category_revenue[, percentage := round(percentage, 2)]

# Filter rows for 'male', 'female', and 'other' genders
desired_genders <- c('M', 'F', 'Other')
gender_category_revenue <- gender_category_revenue[gender %in% desired_genders]

# Format total_revenue with commas
gender_category_revenue[, total_revenue := format(total_revenue, big.mark = ",", scientific = FALSE)]

# Round the percentage column to 2 decimal places
gender_category_revenue[, percentage := round.numerics(percentage, 1)]

# Print the results
print(gender_category_revenue)



#####################################################################
#Question 6 - Using linear regression, what is the increase in monthly average spending associated 
# with an extra ten thousand dollars of income?  
# Make sure your model also includes age, gender, and region as inputs to adjust for.

# Create data.tables for 'transactions'
transactions_dt <- as.data.table(transactions)

# Filter transactions for the entire month of January
january_transactions <- transactions_dt[date >= start_date & date <= end_date]

# Calculate total spending for each customer ID
total_spending <- january_transactions[,.(total_spending = sum(price * quantity)), by = customer_id]

# Merge total spending with customer data (age, gender, income, region)
january_data <- left_join(customers, total_spending, by = "customer_id")

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

#Using a function
lm_IncreaseSpend_WithIncome <- function(extra_income){
 
  # Filter transactions for the entire month of January
  transactions_for_period <- transactions_dt[date >= start_date & date <= end_date]
  
  # Calculate total spending for each customer ID
  total_spending <- transactions_for_period[,.(total_spending = sum(price * quantity)), by = customer_id]
  
  # Merge total spending with customer data (age, gender, income, region)
  customer_spend_data <- left_join(customers, total_spending, by = "customer_id")
  
  # Build a linear regression model
  model <- lm(total_spending ~ income + age + gender + region, data = customer_spend_data)
  
  # Extract the coefficient for income from the summary
  summary_result <- summary(model)
  income_coefficient <- summary_result$coefficients["income", "Estimate"]
  
  # Calculate the increase in monthly average spending associated with an extra ten thousand dollars of income
  increase_in_spending <- income_coefficient * extra_income  # 10,000 dollars
  
  # Round the result to two decimal places
  increase_in_spending <- round.numerics(increase_in_spending, 2)
  
  # Print the increase in monthly average spending
  return(increase_in_spending)
}

increase_in_spending2 = lm_IncreaseSpend_WithIncome(10000)
increase_in_spending2

################################################################
#Question 7 - Among customers who viewed at least 1 product, how many had at least one purchase 
# during the month?  Show the total number of customers with a view, the total who made a purchase,
# and the percentage of customers with a view who made a purchase.

# Filter views for customers who viewed at least 1 product during January
january_views <- views_dt[date >= start_date & date <= end_date]
viewed_customers <- unique(january_views$customer_id)

# Filter transactions for customers who made at least 1 purchase during January
january_transactions <- transactions_dt[date >= start_date & date <= end_date]
purchased_customers <- unique(january_transactions$customer_id)

# Calculate the total number of customers with a view and customers who made a purchase
total_customers_with_view <- length(viewed_customers)

total_customers_with_purchase <- length(purchased_customers)

# Calculate the percentage of customers with a view who made a purchase
percentage_with_purchase <- (total_customers_with_purchase / total_customers_with_view) * 100

# Create a data.table to display the results
result_dt <- data.table(
  Total_Customers_With_View = total_customers_with_view,
  Total_Customers_With_Purchase = total_customers_with_purchase,
  Percentage_With_Purchase = percentage_with_purchase
)

# Format the total_customers_with_view and total_customers_with_purchase with commas
result_dt[, `:=`(
  Total_Customers_With_View = format(Total_Customers_With_View, big.mark = ",", scientific = FALSE),
  Total_Customers_With_Purchase = format(Total_Customers_With_Purchase, big.mark = ",", scientific = FALSE)
)]

# Round the Percentage_With_Purchase column to 2 decimal places
result_dt[, Percentage_With_Purchase := round.numerics(Percentage_With_Purchase, 2)]

# Print the results
print(result_dt)


###########################################################################
#Question 8 - Now let's look at the viewing habits in different age groups, 
# including 18-34, 35-49, 50-64, and 65+.  Within each group, what were the mean, median,
# and standard deviation for the number of unique products viewed per customer?

## Merge 'views' with 'customers' to get age information
#views_with_age <- views_dt[customers_dt, on = "customer_id"]
#
## Create age groups
##views_with_age[, age_group := cut(age, breaks = c(18, 34, 49, 64, Inf), labels = c("18-34", "35-49", "50-64", "65+"))]
#views_with_age[, age_group := cut(age, breaks = c(18, 34, 49, 64, Inf), labels = c("18-34", "35-49", "50-64", "65+"), include.lowest = TRUE, right = FALSE)]
#views_with_age[is.na(age_group), age_group := "Unknown"]

views_with_age <- create_age_groups()

# Calculate the number of unique products viewed per customer within each age group
unique_products_per_customer <- views_with_age[, .(Unique_Products_Viewed = length(unique(product_id))), by = .(age_group, customer_id)]

# Calculate mean, median, and standard deviation for each age group
age_group_stats <- unique_products_per_customer[, .(Mean = mean(Unique_Products_Viewed, na.rm = TRUE),
                                                    Median = median(Unique_Products_Viewed, na.rm = TRUE),
                                                    StdDev = sd(Unique_Products_Viewed, na.rm = TRUE)),
                                                by = age_group]
# Sort age_group_stats in ascending order of age_group
setorder(age_group_stats, age_group)

# Round off Mean, Median, and StdDev to 2 decimal places
age_group_stats[, `:=`(
  Mean = round(Mean, 2),
  Median = round(Median, 2),
  StdDev = round(StdDev, 2)
)]

# Print the results
print(age_group_stats)

####Different Approach
# Calculate unique product counts per customer
product_count <- views_for_period[, .(views_count = uniqueN(product_id)), by = customer_id]

# Merge product_count with customers and create an 'age_group' column
customer_with_products <- merge(product_count, customers[, c("age", "customer_id")], by = "customer_id", all = TRUE)
customer_with_products[, age_group := cut(age, breaks = c(18, 34, 49, 64, Inf), labels = c('18-34', '35-49', '50-64', '65+'))]

# Filter out rows with NA in 'views_count' and 'age_group'
customer_with_products <- customer_with_products[!is.na(views_count) & !is.na(age_group)]

# Calculate mean, median, and standard deviation of 'views_count' by 'age_group'
result <- customer_with_products[, .(
  mean = round(mean(views_count), 2),
  median = round(median(views_count), 2),
  std = round(sd(views_count), 2)
), by = age_group]

# Print the results
print(result)


###########################################################################
#Question 9 - What is the correlation between a user's total page views and total spending?  
# Look only at customers with at least 1 view for the month.  For customers without a transaction,
# include their spending as zero.

## Convert 'time' columns to POSIXct format
#views$time <- as.POSIXct(views$time, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
#transactions$time <- as.POSIXct(transactions$time, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
#
## Define the start and end timestamps for the entire month of January
#start_date <- as.POSIXct("2020-01-01T00:00:00", format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
#end_date <- as.POSIXct("2020-01-31T23:59:59", format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
#
## Filter views and transactions for the entire month of January
#january_views <- views[views$time >= start_date & views$time <= end_date, ]
#january_transactions <- transactions[transactions$time >= start_date & transactions$time <= end_date, ]
#
## Find customers who viewed at least one product in January
#viewed_customers <- unique(january_views$customer_id)
#
## Calculate total spending for each customer
#total_spending <- january_transactions[, sum(price * quantity), by = customer_id]
#
## Calculate total page views for each customer
#total_page_views <- january_views[customer_id %in% viewed_customers, .N, by = customer_id]
#
## Merge total page views and total spending
#customer_data <- merge(total_page_views, total_spending, by = "customer_id", all = TRUE)
#
## Replace missing spending values with 0
#customer_data[is.na(customer_data$V2), "V2"] <- 0
#
## Calculate the correlation between total page views and total spending
#correlation <- cor(customer_data$N, customer_data$V2, use = "complete.obs")
#
## Round the correlation to two decimal places
#correlation <- round(correlation, 2)
#
## Print the correlation
#correlation

# Filter views for the entire month of January
january_views <- views_dt[date >= start_date & date <= end_date]

# Filter transactions for the entire month of January
january_transactions <- transactions_dt[date >= start_date & date <= end_date]

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

round.numerics <- function(x, digits){
  if(is.numeric(x)){
    x <- round(x = x, digits = digits)
  }
  return(x)
}

# Round the correlation to two decimal places
correlation <- round.numerics(correlation, 2)

# Print the correlation
correlation

######################################################################
#Question 10 - Which customer(s) purchased the largest number of coats during the month?  
# In the event of a tie, include all of the users who reached this value, listed in increasing
# sorted order.  Show their identifiers and total volume of coats purchased during the month.

# Filter transactions for the entire month of January
january_transactions <- transactions_dt[date >= start_date & date <= end_date,]

# Merge the 'products' table to check for the "coat" product
january_transactions <- january_transactions[products_dt, on = "product_id"]

# Calculate the total volume of coats purchased by each customer
coats_purchased <- january_transactions[category == "coat", .(total_coats = sum(quantity)), by = customer_id]

# Find the largest number of coats purchased
max_coats <- max(coats_purchased$total_coats)

# Filter customers who purchased the largest number of coats
top_coat_customers <- coats_purchased[total_coats == max_coats, .(customer_id, total_coats)]

# Sort the top customers by customer_id in increasing order
top_coat_customers <- top_coat_customers[order(customer_id)]

# Print the result
print(top_coat_customers)



#######################################
#Testing functions

# 2. Define a function to get the top N viewed products for each category
get_top_n_viewed <- function(category_data, n) {
  return(head(category_data[order(-views_count)], n))
}

# 4. Define a function to create age groups in the dataset
create_age_groups <- function() {
  
  # Merge 'views' with 'customers' to get age information
  views_with_age <- views_dt[customers_dt, on = "customer_id"]
  
  # Create age groups
  views_with_age[, age_group := cut(age, breaks = c(18, 34, 49, 64, Inf), labels = c("18-34", "35-49", "50-64", "65+"), include.lowest = TRUE, right = FALSE)]
  views_with_age[is.na(age_group), age_group := "Unknown"]
  
  return(views_with_age)
}

# 5. Define a function to display Top 10 most viewed products for every unique category
display_Top_10 <- function(unique_categories, category_data){
  
  for (unique_category in unique_categories) {
    cat("Category:", unique_category, "\n")
    top10_category_data <- category_data[unique_category == top_10_viewed_by_category$category, ][1:10]
    print(top10_category_data)
    cat("\n")
  }
}


















