
set.seed(2020)
options(digits = 3)

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
