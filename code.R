
set.seed(2020)

if(!require(groupdata2)) install.packages("groupdata2", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(recommenderlab)) install.packages("recommenderlab", repos = "http://cran.us.r-project.org")
if(!require(recosystem)) install.packages("recosystem", repos = "http://cran.us.r-project.org")
if(!require(tinytex)) install.packages("tinytex", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(ggcorrplot)) install.packages("ggcorplot", repos = "http://cran.us.r-project.org")


options(digits = 4)


### NYC property sales
### https://www.kaggle.com/new-york-city/nyc-property-sales
### https://www.kaggle.com/new-york-city/nyc-property-sales/download

### reading the data from the git repository - where I have downloaded it
prices <- read_csv(url("https://raw.githubusercontent.com/AmiDavid/NYC-real-esate-prices-algorithm/master/nyc-rolling-sales.csv"))


#### golssary of terms https://www1.nyc.gov/assets/finance/downloads/pdf/07pdf/glossary_rsf071607.pdf

#### Exploring the data

head(prices)


### some houses are sold for 0, 1, 10 or NA.
prices %>% 
  dplyr::count(prices$`SALE PRICE` < 1000)



#### I removed the houses sold for less than 1000 and the NA's
prices <- prices %>% 
  dplyr::filter(prices$`SALE PRICE` > 1000)

#### checking the building class category

prices %>% 
  group_by(`BUILDING CLASS CATEGORY`) %>% 
  summarize(n = n())

#### grouping the neighberhood
prices %>% 
  ggplot(aes(NEIGHBORHOOD)) +
  geom_bar()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#### the neighberhoods with the most sales
prices %>% 
  group_by(NEIGHBORHOOD) %>% 
  summarize(n = n(), mean_sale_price = mean(as.numeric(`SALE PRICE`)/ 1000)) %>% 
  arrange(desc(n))

### the tax class 
prices %>% 
  group_by(prices$`TAX CLASS AT TIME OF SALE`) %>% 
    summarize(mean_sale_price = mean(as.numeric(`SALE PRICE`) / 1000), n = n())
    
### plotting the tax class
prices %>% 
  group_by(prices$`TAX CLASS AT TIME OF SALE`) %>% 
  ggplot(aes(`TAX CLASS AT TIME OF SALE`, fill = `TAX CLASS AT TIME OF SALE`)) + 
  geom_bar(color = "black")
  

### showing building class and price
prices %>% 
  ggplot(aes(prices$`TAX CLASS AT PRESENT`, as.numeric(prices$`SALE PRICE`))) +
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(trans = "log2") +
  xlab("Tax class at present") +
  ylab("price sold")


### showing connection between size and price

prices %>% 
  filter((prices$'GROSS SQUARE FEET') > 0) %>% 
  ggplot(aes(as.numeric(`GROSS SQUARE FEET`), as.numeric(`SALE PRICE`))) +
  geom_point()+
  scale_y_continuous(trans = "log2") +
  scale_x_continuous(trans = "log2") + 
  xlab("Gross Square FEET") +
  ylab("Price Sold")


### showing connection between size and price coloring the tax class at moment of sale

prices %>% 
  filter((prices$'GROSS SQUARE FEET') > 0) %>% 
  ggplot(aes(as.numeric(`GROSS SQUARE FEET`), as.numeric(`SALE PRICE`), color = `TAX CLASS AT TIME OF SALE`)) +
  geom_point()+
  scale_y_continuous(trans = "log2") +
  scale_x_continuous(trans = "log2") +
  xlab("Gross Square FEET") +
  ylab("Price Sold")


### in addition, some properties have changed classification since the time
prices %>% 
  mutate(changed_tax_class = str_detect(prices$`TAX CLASS AT PRESENT`,as.character(prices$`TAX CLASS AT TIME OF SALE`))) %>% 
  group_by(changed_tax_class) %>% 
  summarize(n = n())


### year built of the building
prices %>% 
  filter(prices$`YEAR BUILT` > 0) %>% 
  ggplot(aes(as.numeric(`YEAR BUILT`))) +
  geom_bar() + 
  xlim(1800, max(as.numeric(prices$`YEAR BUILT`)))+
  xlab("Year Built")


### connection between the year built and the average price
prices %>% 
  group_by(`YEAR BUILT`) %>% 
  filter(`YEAR BUILT` > 1800) %>% 
  summarize(mean_sale_price = mean(as.numeric(`SALE PRICE`) / 1000), n = n())


prices %>% 
  group_by(`YEAR BUILT`) %>% 
  filter(`YEAR BUILT` > 1800) %>% 
  summarize(mean_sale_price = mean(as.numeric(`SALE PRICE`) / 1000), n = n()) %>% 
  ggplot(aes(`YEAR BUILT`, mean_sale_price)) +
  geom_point()+
  xlab("Year Built") +
  ylab("Mean Sale Price")


### 5 boroughs of NYC: Manhattan, Brooklyn, Queens, The Bronx and Staten Island

prices %>% 
  mutate(BOROUGH = ifelse(BOROUGH == 1, "Manhattan", 
                          ifelse(BOROUGH == 2, "Brooklyn",
                                 ifelse(BOROUGH == 3, "Queens", 
                                        ifelse(BOROUGH == 4, "The Bronx", "Staten Island"))))) %>% 
  
  group_by(`BOROUGH`) %>% 
  summarize(mean_sale_price = mean(as.numeric(`SALE PRICE`) / 1000), n = n())


### bourugh info
prices %>% 
  ggplot(aes(as.factor(prices$BOROUGH), as.numeric(prices$`SALE PRICE`))) +
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(trans = "log2") +
  xlab("Borough") +
  ylab("price sold") +
  scale_x_discrete(labels = c("Manhattan", "Brooklyn", "Queens", "The Bronx", "Staten Island")) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))


### plotting the boroughs
prices %>% 
  mutate(BOROUGH = ifelse(BOROUGH == 1, "Manhattan", 
                          ifelse(BOROUGH == 2, "Brooklyn",
                                 ifelse(BOROUGH == 3, "Queens", 
                                        ifelse(BOROUGH == 4, "The Bronx",
                                               ifelse(BOROUGH == 5, "Staten Island", 0)))))) %>% 
  group_by(prices$BOROUGH) %>% 
  ggplot(aes(BOROUGH, fill = as.factor(`BOROUGH`))) +
  geom_bar(color = "black")


### some weeks have more deals than others

prices %>% 
  mutate(`SALE DATE` = as_datetime(`SALE DATE`),
         `SALE DATE` = round_date(`SALE DATE`, unit = "week",
                                  week_start = getOption("lubridate.week.start", 7))) %>% 
  group_by(`SALE DATE`) %>% 
  ggplot(aes(`SALE DATE`)) +
  geom_bar() +
  ylab("Number of Sales")


### tidying the data

prices$`SALE DATE` <- as_datetime(prices$`SALE DATE`)
prices$`SALE DATE` <- round_date(prices$`SALE DATE`, unit = "week",
                         week_start = getOption("lubridate.week.start", 7))


### Creating a correlation matrix - by the explanations in http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization
### And https://towardsdatascience.com/how-to-create-a-correlation-matrix-with-too-many-variables-309cc0c0a57


corr_simple <- function(data=prices,sig=0.5){
  #convert data to numeric in order to run correlations
  #convert to factor first to keep the integrity of the data - each value will become a number rather than turn into NA
  df_cor <- data %>% mutate_if(is.character, as.factor)
  df_cor <- df_cor %>% mutate_if(is.factor, as.numeric)
}

prices_numerical <- corr_simple(prices)
prices_numerical$`SALE DATE` <- as.numeric(prices_numerical$`SALE DATE`)
prices_numerical <- prices_numerical %>% 
  select(-`EASE-MENT`, -`X1`)


cormat <- round(cor(prices_numerical, use = "na.or.complete"), 2)
ggcorrplot(cormat)

### Reorder the correlation matrix

reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

# Reorder the correlation matrix
cormat <- reorder_cormat(cormat)
cormat

### We can see from the correlation matrix that the sale price is correlated mainly with gross sq. feet, altough there are a lot of na's to really learn something.
### I'm gonna remove highly correlated variables
prices_numerical <- prices_numerical %>% 
  select(-BOROUGH, -`GROSS SQUARE FEET`, -`BUILDING CLASS AT PRESENT`, -`TAX CLASS AT PRESENT`)


### We see that some columns have some NA's
colSums(is.na(prices))

### removing Easement and apartemnt number
prices <- prices %>% 
  select(-`EASE-MENT`, -`APARTMENT NUMBER`)


#### creating train set, test set and validation set
validation_index <- createDataPartition(y = prices$BOROUGH, times = 1, p = 0.1, list = FALSE)
validation_set <- prices[validation_index,]
prices_set <- prices[-validation_index,]

### creating test set from the prices_set
test_index <- createDataPartition(y = prices_set$BOROUGH, times = 1, p = 0.1, list = FALSE)
test_set <- prices_set[test_index,]
train_set <- prices_set[-test_index,]
train_set <- corr_simple(train_set)
test_set <- corr_simple(test_set)


###RMSE function
RMSE <- function(real_sale_price, predicted_price){
  sqrt(mean((real_sale_price - predicted_price) ^ 2))
}

MSE <- function(real_sale_price, predicted_price){
  mean((real_sale_price - predicted_price) ^ 2)
}

MAE <- function(real_sale_price, predicted_price){
  mean(abs(real_sale_price - predicted_price))
}


#### Naive linear model - just the average
mu_hat_average <- mean(train_set$`SALE PRICE`)
tibble(mu_hat_average = mu_hat_average)

### building and evaluation table

evaluation_results <- tibble(method = "Simple average", RMSE = RMSE(test_set$`SALE PRICE`, mu_hat_average),
                             MSE = MSE(test_set$`SALE PRICE`, mu_hat_average), 
                             MAE = MAE(test_set$`SALE PRICE`, mu_hat_average))

evaluation_results

### adding gross square feet 
gsf <- train_set %>% 
  summarize(gsf = mean(train_set$`SALE PRICE` - mu_hat_average))

predicted_price <- test_set %>% 
  mutate(pred = mu_hat_average + gsf)


### The first model is trying to predict based only on columns with no na's
train_set_numeric <- corr_simple(train_set)
test_set_numeric <- corr_simple(test_set)

glm_train <- train(`SALE PRICE` ~ ., method = "glm",
                               data = train_set_numeric, na.action = na.omit)

y_hat_glm <- predict(glm_train, test_set_numeric, type = "raw")

RMSE(test_set_numeric$`SALE PRICE`, y_hat_glm)


### since it takes a very long time I have filtered the data to only staten Island

manhattan_train <- train_set_numeric %>% 
  filter(BOROUGH == 5) %>% 
  select(`TAX CLASS AT TIME OF SALE`, BLOCK, `ZIP CODE`, LOT, `YEAR BUILT`, `BUILDING CLASS AT TIME OF SALE`, `SALE DATE`, `SALE PRICE`)
manhattan_test <- test_set_numeric %>% 
  filter(BOROUGH == 5) %>% 
  select(`TAX CLASS AT TIME OF SALE`, BLOCK, `ZIP CODE`, LOT, `YEAR BUILT`, `BUILDING CLASS AT TIME OF SALE`, `SALE DATE`, `SALE PRICE`)

first_model_manhattan <- train(`SALE PRICE` ~ ., method = "rf",
                     data = manhattan_train)

y_hat_manhattan_rf <- predict(first_model_manhattan, manhattan_test)

RMSE(manhattan_test$`SALE PRICE`, y_hat_manhattan_rf)


manhattan_test <- manhattan_test %>% 
  mutate(predict = y_hat_manhattan_rf,
         gap = (predict - `SALE PRICE`)^2)

