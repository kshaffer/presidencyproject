# Presidency Project
Code to scrape and parse documents in [The American Presidency Project](http://www.presidency.ucsb.edu/) using [R](https://www.r-project.org/) and [rvest](https://cran.r-project.org/web/packages/rvest/index.html).

## Instructions

The essential functions are contained in ```functions.R```. Run this script first, or include it with

    source('functions.R')

to use those functions in your own scripts. See ```examples.R``` for examples of how to use the scraper.

## Primary functions

Each content page on TAPP has a URL formatted:

    http://www.presidency.ucsb.edu/ws/index.php?pid=000000

where ```000000``` is a unique page ID number.

```deglaze(page_id)``` will scrape the page corresponding to the page ID number, and return a one-row tibble containing the page title, date, and main text.

```deglaze()``` can process a list of page IDs and return a single tibble with one row per page:

    # declare list of page IDs (accepts both integers and characters)
    pages <- c(100001, 100002, 100003, 100004)
    
    # scrape and parse the pages, store as tibble called pages_data
    pages_data <- t(mapply(deglaze, pages)) %>%
      as_tibble() %>%
      unnest() %>%
      mutate(date = ymd(date))

You can also extract page IDs from a search results page (```extract_link_title``` and ```extract_link_page_id``` are functions declared in ```functions.R```):

    links <- read_html('http://www.presidency.ucsb.edu/executive_orders.php?year=2009&Submit=DISPLAY') %>%
      html_nodes('a') %>%
      as.character() %>%
      as_tibble() %>%
      mutate(title = mapply(extract_link_title, value),
         page_id = mapply(extract_link_page_id, value)) %>%
      select(title, page_id)

See ```examples.R``` for an example of how to combine multiple pages of search results automatically.