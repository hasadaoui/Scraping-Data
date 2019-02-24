# Scraping indeed webpage using R --------------------------------------
library(tidyverse)  #General-purpose data wrangling
library(rvest)      #Parsing of HTML/XML files
library(stringr)    #String manipulation
library(rebus)      #Verbose regular expressions
library(lubridate)  #Eases DateTime manipulation

# Best method to scraping Data -----------------------------------------
url = 'https://www.indeed.fr/cmp/IBM/reviews'

get_last_page = function(html){
  
  pages_data <- html %>% 
    # The '.' indicates the class
    html_nodes('.cmp-Pagination-link') %>% 
    # Extract the raw text as a list
    html_text()                   
  
  # The second to last of the buttons is the one
  pages_data[(length(pages_data)-1)] %>%            
    # Take the raw string
    unname() %>%                                     
    # Convert to number
    as.numeric()   
} 
first_page <- read_html(url)
(latest_page_number <- get_last_page(first_page))

# On fait des changements à partir de cette ligne
latest_page_number = 8
list_of_pages <- c(url,str_c(url,'?start=',seq(20,100,20)))

# Extract the Information of One Page
get_reviews <- function(html){
  html %>% 
    # The relevant tag
    html_nodes('.cmp-review-title') %>%      
    html_text() %>% 
    # Trim additional white space
    str_trim() %>%                       
    # Convert the list into a vector
    unlist()                             
}
get_reviews(read_html(url))

get_reviewer_names <- function(html){
  html %>% 
    html_nodes('.cmp-reviewer-job-title') %>% 
    html_text() %>% 
    str_trim() %>% 
    unlist()
}
get_reviewer_names(read_html(url))
get_reviewer_location <- function(html){
  html %>% 
    html_nodes('.cmp-reviewer-job-location') %>% 
    html_text() %>% 
    str_trim() %>% 
    unlist()
}
get_reviewer_location(read_html(url))
get_review_dates <- function(html){
  
  html %>% 
    html_nodes('.cmp-review-date-created') %>% 
    html_text() %>% 
    str_trim() %>% 
    unlist()  
  
}
get_review_dates(read_html(url))
get_review_description <- function(html){
  
  html %>% 
    html_nodes('.cmp-review-description') %>% 
    html_text() %>% 
    str_trim() %>% 
    unlist()  
  
}
get_review_description(read_html(url))
get_star_rating <- function(html){
  
  html %>% 
    html_nodes(xpath = '//meta[@itemprop="ratingValue"]') %>% 
    html_attr('content') %>%
    as.numeric() %>% 
    unlist()  
}
get_star_rating(read_html(url))

## Optionnel
get_positive_review <- function(html){
  
  html %>% 
    html_nodes('.cmp-review-pro-text') %>% 
    html_text() %>% 
    str_trim() %>% 
    unlist()  
}
get_positive_review(read_html(url))
get_negative_review <- function(html){
  
  html %>% 
    html_nodes('.cmp-review-con-text') %>% 
    html_text() %>% 
    str_trim() %>% 
    unlist()  
}
get_negative_review(read_html(url))


# Get table -------------
get_data_table <- function(html, company_name){
  
  # Extract the Basic information from the HTML
  reviews <- get_reviews(read_html(url))
  reviewer_names <- get_reviewer_names(read_html(url))
  reviewer_location<- get_reviewer_location(read_html(url))
  dates <- get_review_dates(read_html(url))
  review_description<- get_review_description(read_html(url))
  ratings <- get_star_rating(read_html(url))
  pos= get_positive_review(read_html(url))
  neg = get_negative_review(read_html(url))
  
  # Combine into a tibble
  combined_data <- tibble(reviewer = reviewer_names,
                          job = reviewer_names,
                          location = reviewer_location,
                          date = dates,
                          rating = ratings,
                          review_title = reviews,
                          desc = review_description) 
  
  # Tag the individual data with the company name
  combined_data %>% 
    mutate(company = company_name) %>% 
    select(company, job, location,date,rating,review_title,desc) #reviewer,
}
get_data_table(url, 'IBM')

get_data_from_url <- function(html, company_name){
  get_data_table(html, company_name)
}
get_data_from_url(url, 'IBM')


###########################
# Positive reviews
pos = NULL
neg = NULL
for( i in 2:5){
  pos = cbind(pos,get_positive_review(read_html(list_of_pages[i])))
  neg = cbind(neg,get_negative_review(read_html(list_of_pages[i])))
}
pos = cbind(pos,get_positive_review(read_html(list_of_pages[1])))
neg = cbind(neg,get_negative_review(read_html(list_of_pages[1])))

###########################

scrape_write_table <- function(url, company_name){
  
  # Read first page
  first_page <- read_html(url)
  
  # Extract the number of pages that have to be queried
  latest_page_number <- get_last_page(first_page)
  
  # Generate the target URLs
  list_of_pages <- c(url,str_c(url,'?start=',seq(20,100,20)))
  
  #list_of_pages <- c(url,str_c(url, '?fcountry=ALL&start=',seq(20,200,20)))
  
  # Apply the extraction and bind the individual results back into one table, 
  # which is then written as a tsv file into the working directory
  list_of_pages %>% 
    # Apply to all URLs
    map(get_data_from_url, company_name) %>%  
    # Combine the tibbles into one tibble
    bind_rows() %>%                           
    # Write a tab-separated file
    write_tsv(str_c(company_name,'.tsv')) 
  
}
scrape_write_table(url, 'IBM_FR')

df <- read_tsv('C:/Users/sadaouih/Desktop/Scraping DATA/IBM_fr.tsv')
tail(df, 5)