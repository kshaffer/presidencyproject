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


# declare a list of pages to deglaze

pages <- c()


# scrape and parse the pages, store as tibble called pages_data

pages_data <- t(mapply(deglaze, pages)) %>%
  as_tibble() %>%
  unnest() %>%
  mutate(date = ymd(date))

