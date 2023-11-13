##################################################################
### This code is a combination of online tutorials and my own work
#################################################################
### 2023-11-13 
### Scraping data from Amazon Brazil


library(rvest)
library(tidyverse)

scrape_amazon <- function(ASIN, page_num){
  url_reviews <- paste0("#someAmazonpage",page_num)
  doc <- read_html(url_reviews) # Assign results to `doc`
  
  # Review Title
  #concatenate the titles from br and non-br reviews
  c(
    #br review
    doc %>% 
      html_nodes("[class='a-size-base a-link-normal review-title a-color-base review-title-content a-text-bold']") %>%
      html_text(),
    
    #non-br reviews
    doc %>% 
      html_nodes("[class='a-size-base review-title a-color-base review-title-content a-text-bold']") %>%
      html_text()
  ) -> review_title
  
  # Review Text
  doc %>% 
    html_nodes("[class='a-size-base review-text review-text-content']") %>%
    html_text() -> review_text
  
  # Number of stars in review
  #concatenate the stars from br and non-br reviews
  c(
    #br stars
    doc %>%
      html_nodes("[data-hook='review-star-rating']") %>%
      html_text(), 
    
    #non-br stars
    doc %>%
      html_nodes("[data-hook='cmps-review-star-rating']") %>%
      html_text() 
  ) -> review_star
  
 # Date 
  doc %>%
    html_nodes("#cm_cr-review_list .review-date") %>%
    html_text() -> review_date
  
  #format
  doc %>% 
    html_nodes(".review-format-strip") %>% 
    html_text() -> review_format
  

  # Return a tibble
  tibble(review_title,
         review_text,
         review_star,
         review_date,
         review_format,
         page = page_num) %>% return()
}

#You can then run this function to extract a nice, clean table of reviews:
scrape_amazon(ASIN = "#ASINnumberGoesHere", page_num = 10) %>%
head()

ASIN <- "#here" # Specify ASIN
page_range <- 1:10 # Let's say we want to scrape pages 1 to 10
   
# Create a table that scrambles page numbers using `sample()`
# Randomize pages
match_key <- tibble(n = page_range,
                       key = sample(page_range,length(page_range)))
   
lapply(page_range, function(i){
     j <- match_key[match_key$n==i,]$key
     message("Getting page ",i, " of ",length(page_range), "; Actual: page ",j) # Progress bar
     Sys.sleep(3) # Take a three second break
     if((i %% 3) == 0){ # After every three scrapes... take another two second break
    message("Taking a break...") 
       Sys.sleep(2) # Take an additional two second break
     }
     scrape_amazon(ASIN = ASIN, page_num = j) # Scrape
   }) -> output_list



###change for each product 
SomeMeaningfulTitle <- output_list
merged_data <- reduce(SomeMeaningfulTitle, rbind) ####Jonathan's solution  #works perfectly

### add info about each product/category
prod <- c("#prodName") ### substitute #prodName for the name of the product you chose
prod_price <- c("100.60") ### add price
prod_category <- c("foods") ### add category
country <- c("Brazil") ### add country 
data_new <- data.frame(merged_data, prod, prod_price, prod_category, country)

### write data to disk
write.csv(data_new, "/your/path/here", row.names=FALSE)

