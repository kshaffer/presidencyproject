library(rvest)
library(tidyverse)
library(lubridate)


# function to scrape and parse the title, date, and content of a page on 
# The American Presidency Project, http://www.presidency.ucsb.edu/
# takes the pageID, not an entire URL:
# XXXXXX in http://www.presidency.ucsb.edu/ws/index.php?pid=XXXXXX

deglaze <- function(page_id) {
  page_data <- read_html(paste('http://www.presidency.ucsb.edu/ws/index.php?pid=', page_id, sep = ''))
  page_title <- page_data %>%
    html_node('title') %>%
    html_text()
  page_date <- page_data %>%
    html_node('.docdate') %>%
    html_text() %>%
    mdy() %>%
    as.character()
  page_text <- page_data %>%
    html_nodes('p') %>%
    html_text() %>%
    paste(collapse = ' ')
  return(as_tibble(cbind(title = page_title, date = page_date, text = page_text)))
}


# function to extract text from hyperlink (as character)

extract_link_title <- function(anchor_tag) {
  return(unlist(strsplit(unlist(strsplit(anchor_tag, '>'))[2], '<'))[1])
}


# function to extract target URL from hyperlink (as character)

extract_link_page_id <- function(anchor_tag) {
  return(c(unlist(strsplit(unlist(strsplit(anchor_tag, '?pid='))[2], '\">'))[1]))
}


# function to extract president's name from title

extract_president <- function(title) {
  return(unlist(strsplit(title, ':'))[1])
}
