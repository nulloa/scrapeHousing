##### Libraries #####
# library(textreadr)
library(tidyverse)
library(rvest)
library(RSelenium)

##### Get Data #####
# This set of code tells me the number of search pages to make the code general as possible
zillow_url <- "https://www.zillow.com/chula-vista-ca/sold/house,townhouse_type/?searchQueryState=%7B%22pagination%22%3A%7B%7D%2C%22usersSearchTerm%22%3A%22Chula%20Vista%2C%20CA%22%2C%22mapBounds%22%3A%7B%22west%22%3A-117.14297635009765%2C%22east%22%3A-116.90608364990234%2C%22south%22%3A32.55339406311664%2C%22north%22%3A32.71010272519207%7D%2C%22regionSelection%22%3A%5B%7B%22regionId%22%3A51405%2C%22regionType%22%3A6%7D%5D%2C%22isMapVisible%22%3Atrue%2C%22filterState%22%3A%7B%22sort%22%3A%7B%22value%22%3A%22days%22%7D%2C%22ah%22%3A%7B%22value%22%3Atrue%7D%2C%22mf%22%3A%7B%22value%22%3Afalse%7D%2C%22con%22%3A%7B%22value%22%3Afalse%7D%2C%22land%22%3A%7B%22value%22%3Afalse%7D%2C%22apa%22%3A%7B%22value%22%3Afalse%7D%2C%22manu%22%3A%7B%22value%22%3Afalse%7D%2C%22apco%22%3A%7B%22value%22%3Afalse%7D%2C%22rs%22%3A%7B%22value%22%3Atrue%7D%2C%22fsba%22%3A%7B%22value%22%3Afalse%7D%2C%22nc%22%3A%7B%22value%22%3Afalse%7D%2C%22cmsn%22%3A%7B%22value%22%3Afalse%7D%2C%22fore%22%3A%7B%22value%22%3Afalse%7D%7D%2C%22isListVisible%22%3Atrue%2C%22mapZoom%22%3A12%7D"

numberoftiles =40 # This is the number of tiles of listings that show up on Zillow. 

zillow_pg <- read_html(zillow_url)

# count the total results and set the page count
zillow_cnt <- zillow_pg %>% html_nodes('.result-count') %>% html_text() %>% str_remove(' results') %>% gsub("[^0-9.-]", "", .) %>% as.numeric()
zillow_pg_cnt = ceiling(zillow_cnt /numberoftiles)



## Now to construct a dataframe to loop over the number of search pages. 
res_all <- NULL
for (i in 1:zillow_pg_cnt) {
  pg <- read_html(str_c('https://www.zillow.com/chula-vista-ca/sold/house,townhouse_type/', i, '_p/?searchQueryState=%7B%22pagination%22%3A%7B%22currentPage%22%3A2%7D%2C%22usersSearchTerm%22%3A%22Chula%20Vista%2C%20CA%22%2C%22mapBounds%22%3A%7B%22west%22%3A-117.14297635009765%2C%22east%22%3A-116.90608364990234%2C%22south%22%3A32.55339406311664%2C%22north%22%3A32.71010272519207%7D%2C%22regionSelection%22%3A%5B%7B%22regionId%22%3A51405%2C%22regionType%22%3A6%7D%5D%2C%22isMapVisible%22%3Atrue%2C%22filterState%22%3A%7B%22sort%22%3A%7B%22value%22%3A%22days%22%7D%2C%22ah%22%3A%7B%22value%22%3Atrue%7D%2C%22mf%22%3A%7B%22value%22%3Afalse%7D%2C%22con%22%3A%7B%22value%22%3Afalse%7D%2C%22land%22%3A%7B%22value%22%3Afalse%7D%2C%22apa%22%3A%7B%22value%22%3Afalse%7D%2C%22manu%22%3A%7B%22value%22%3Afalse%7D%2C%22apco%22%3A%7B%22value%22%3Afalse%7D%2C%22rs%22%3A%7B%22value%22%3Atrue%7D%2C%22fsba%22%3A%7B%22value%22%3Afalse%7D%2C%22nc%22%3A%7B%22value%22%3Afalse%7D%2C%22cmsn%22%3A%7B%22value%22%3Afalse%7D%2C%22fore%22%3A%7B%22value%22%3Afalse%7D%7D%2C%22isListVisible%22%3Atrue%2C%22mapZoom%22%3A12%7D'))
  res_pg <- tibble(
    address= pg %>% html_nodes(".list-card-info a") %>% html_text(),
    pricelisted = pg %>% html_nodes(".list-card-price") %>% html_text(),
    solddate = pg %>% html_nodes(".list-card-variable-text") %>% html_text() %>% 
      str_remove('Sold '),
    bedrooms = pg %>% html_nodes(".list-card-details li:nth-child(1)") %>% 
      html_text() %>% str_remove(' bd.*') %>% as.numeric(),
    bathrooms = pg %>% html_nodes(".list-card-details li:nth-child(2)") %>% 
      html_text() %>% str_remove(' ba') %>% as.numeric(),
    squarefoot = pg %>% html_nodes(".list-card-details li:nth-child(3)") %>% 
      html_text() %>% str_remove(' sqft'),
    typeofsale = pg %>% html_nodes(".list-card-type") %>%
      html_text(), 
    zillowcomments = pg %>% html_nodes(".list-card-top") %>%
      html_text(),
    zillow_link = pg %>% html_nodes(".list-card-info a") %>% html_attr('href')
  ) 
  
  
  res_all <- res_all %>% bind_rows(res_pg)
  
}




# Have to scroll through the entire zillow page to show the table I want, for 
# this, I need to use RSelenium to "scroll" the page before scraping it.

## opens the driver
rD <- rsDriver(browser="firefox", port=4444L)
remDr <- rD[["client"]]

## navigates to the correct page
remDr$navigate('https://www.zillow.com/homedetails/1437-Max-Ave-Chula-Vista-CA-91911/17154209_zpid/')

## scrolls to the bottom of the table
remDr$executeScript("window.scrollTo(0,document.body.scrollHeight);")
Sys.sleep(1)
remDr$executeScript("window.scrollTo(0,document.body.scrollHeight);")
Sys.sleep(1)
remDr$executeScript("window.scrollTo(0,document.body.scrollHeight);")
Sys.sleep(1)
remDr$executeScript("window.scrollTo(0,document.body.scrollHeight);")
Sys.sleep(1)
remDr$executeScript("window.scrollTo(0,document.body.scrollHeight);")
Sys.sleep(1)
remDr$executeScript("window.scrollTo(0,document.body.scrollHeight);")
Sys.sleep(1)
## get the entire page source that's been loaded
html <- remDr$getPageSource()[[1]]
## read in the page source
indHome <- read_html(html)

indHome %>% html_element(css='.cQFnnD') %>%
  html_table()



## close driver
remDr$close()
rD$server$stop()



##### Cleaning #####

# Cleaning the file - Zillow_DF
zillow_df <- res_all %>%
  mutate(pricelisted = as.numeric(str_replace_all(pricelisted,"[^0-9]*",""))) %>%
  mutate(squarefoot = as.numeric(str_replace(squarefoot,",","")))

# create new dataset without missing data
zillow_df <- na.omit(zillow_df)


##### Visualization #####
# 1) Number of Observations :
nrow(zillow_df)

zillow_df %>% select(1:5) %>% tail()

## Outlier in Price

boxplot(pricelisted, main="Boxplot of House Prices", 
        ylab = "House Price Listed ($)", yaxt = "n", 
        col = "gold", medcol = "red", boxlty = 0, axes=FALSE,
        whisklty = 1,  staplelwd = 4, outpch = 8, outcex = 1)
axis(2, at = seq(0, max(pricelisted), 70000), las = 2, cex.axis=0.5)


## Relationship between Price and Squarefoot. 
plot(pricelisted ~ squarefoot, 
     pch=15,  col = rgb((1:7)/7, 0, 0)[as.factor(bedrooms)],
     yaxt = "n", xaxt = "n", axes = FALSE, main="", 
     ylab="House Prices Listed ($)",
     xlab = "Square Foot Of Houses")
axis(2, at = seq(0, max(pricelisted), 125000), las = 2, cex.axis=0.5)
axis(1,at = seq(300, max(squarefoot), 500), las=2, cex.axis = 0.6 )
legend("bottomright", legend = paste("Bedrooms", 1:7), 
       col = rgb((1:7)/7, 0, 0), pch = 15)
title("Here is a simple plot of Square footage vs. price \ngrouped by number of bedrooms", cex.main = 1,   font.main= 2, col.main=rgb(1, 0, 0) ) 








