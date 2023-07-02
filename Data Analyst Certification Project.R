#import the necessary libraries
install.packages("magrittr")
library(magrittr)
library(dplyr)
install.packages("stringr")
library(stringr)
library(tidyr)
library(ggplot2)
library(scales)
install.packages("corrplot")
library(corrplot)
install.packages("corrplot")
library(corrplot)

##Getting the Data in the workspace
product_sales <- product_sales_pens_and_printers
summary(product_sales)

## To begin this project, I needed to inspect the data to make sure it was ready for analysis
#inspecting the data
#Email sales method
email_sales_method <- product_sales %>%
  filter(product_sales$sales_method == "Email") %>%
  count()
email_sales_method
#email and call sales method
email_and_call_sales_method <- product_sales %>%
  filter(product_sales$sales_method == "Email + Call") %>%
  count()
email_and_call_sales_method
#Call sales method
call_sales_method <- product_sales %>%
  filter(product_sales$sales_method == "Call") %>%
  count()
call_sales_method

# How the Data Originally Appeared(Before Editing) ------------------------

## Week: values were 1-6, as expected
## Sales Metric: There were spelling errors, need to fix
## Customer ID: Random set of numbers and letters for each customer, as expected
##NB Sold: Range of 7-16, reasonable amount to be expected
## Revenue: Had NA values where there should've been a positive value, need to fix
##Years as Customer: Had two rows where the years as customer exceeded the number of years the company has existed, need to fix
## State: Had the 50 states in the U.S., as expected


# Data Cleaning and Validation --------------------------------------------
## One of the first things that I noticed was that there were spelling errors when entering the type of sales method. 
## For instance, some of the entries were entered as "em" or "Emailail"

#cleaning the data
product_sales$sales_method <- product_sales$sales_method %>%
  str_replace_all("em", "Email")

product_sales$sales_method <- product_sales$sales_method %>%
  str_replace_all("call", "Call")

product_sales$sales_method <- product_sales$sales_method %>%
  str_replace_all("Emailail", "Email")

#remove the customers where the years_as_customer is not possible
product_sales <- product_sales[-c(13743, 13802), ]

#under the revenue column, there were 1074 NA's. Given that I am unable to accurately estimate what the revenue would be for those rows, I will need to delete them. 
product_sales <- drop_na(product_sales)
summary(product_sales)


# Analyzing the Data ------------------------------------------------------
#The goal is now to find out which sales method worked the best

#creating the same variables with the NA's removed
email_sales_method <- product_sales %>%
  filter(product_sales$sales_method == "Email") %>%
  count()
email_sales_method 
#call method 
call_sales_method <- product_sales %>%
  filter(product_sales$sales_method == "Call") %>%
  count()
call_sales_method
#email and call method
email_and_call_sales_method <- product_sales %>%
  filter(product_sales$sales_method == "Email + Call") %>%
  count()
email_and_call_sales_method


#Creating a graphic to demonstrate the sales methods
ggplot(product_sales, aes(x = `sales_method`)) +
  geom_histogram(`stat`= "count", fill = "red")
##this graphic shows that the email only method was by far the most popular

#Creating a correlation matrix graphic
numeric_variables <- which(sapply(product_sales, is.numeric))
num_var_names <- names(numeric_variables)

all_numeric_variables <- product_sales[, numeric_variables]

#finding the correlation between the numeric variables
numeric_vars_correlation <- cor(all_numeric_variables, us = "pairwise.complete.obs")
numeric_vars_correlation

#Create a heatmap for the correlation matrix
corrplot(numeric_vars_correlation, order = "AOE", method = "color")

#findings from the correlation matrix
#One interesting finding is that there is actually a negative correlation between time as a customer and revenue
## As expected, the greater the number of site visits, the greater the revenue

#Revenue per Sales method graphic
ggplot(product_sales, aes(x = `sales_method`, y = `revenue`)) +
  geom_col(fill = "blue")+
  scale_y_continuous(labels = label_comma())

#Next I wanted to see the revenue per sales method
#Revenue by call sales method
revenue_by_call_method <- product_sales %>%
  select(revenue) %>%
  filter(product_sales$sales_method == "Call") %>%
  sum()
revenue_by_call_method

#revenue by email sales method
revenue_by_email_method <- product_sales %>%
  select(revenue) %>%
  filter(product_sales$sales_method == "Email") %>%
  sum()
revenue_by_email_method

#revenue by email and call method
revenue_by_email_and_call_method <- product_sales %>%
  select(revenue) %>%
  filter(product_sales$sales_method == "Email + Call") %>%
  sum()
revenue_by_email_and_call_method
 
##The table above shows that the revenue was greatest for the email sales method

#Getting a count of how long they have been customers
ggplot(product_sales, aes(x = `years_as_customer`)) +
  geom_boxplot()
##As you can see, there are far more new customers than long time customers

#Individual Order Revenue Distribution
ggplot(product_sales, aes(x = `sales_method`, y = `revenue`))+
  geom_violin()+
  facet_wrap(~`week`)
##What is interesting about this graph is that the range of individual purchases was highest for the email and call method every week.

#Calculating average revenue per sales method used
avg_revenue_by_call <- revenue_by_call_method/call_sales_method
avg_revenue_by_call

#average revenue by email
avg_revenue_by_email <- revenue_by_email_method/email_sales_method
avg_revenue_by_email

#average revenue by email and call
avg_revenue_by_email_and_call <- revenue_by_email_and_call_method/email_and_call_sales_method
avg_revenue_by_email_and_call

##This information is important because it tells us the average revenue generated per sales method. The highest was for the email and call method

#Total Sales Per Week
ggplot(data = product_sales, aes(x = `week`, y = `revenue`)) +
  geom_col(fill = "green")+
  scale_y_continuous(label = label_comma())
##As we can see, the most revenue was generated during the first week of the promotion

#inspecting difference in revenue over time for each method 
ggplot(product_sales, aes(x = `week`, y = `revenue`)) +
  geom_col(fill = "blue")+
  facet_wrap(~`sales_method`)
##As you can see, the email method started out as the most effective, but lost its effectiveness later on in the sales promotion



# Conclusion --------------------------------------------------------------


##When finishing up the analysis of the sales data, I was able to come to a couple of conclusions
##1. A focus on increasing the revenue of the long term customers. With your most loyal customers, you would want them to spend the greatest amount of money. 
##The above point is important because it shows that your product is superior and that your customers are satisfied. 

##2. Another conclusion that I came to was that the email method was the most effective. 
##My main reasoning behind this conclusion was that the email method was by far the most time efficient of the sales methods. 
