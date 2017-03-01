source('functions.R')

# this example creates a tibble of all of Barack Obama's executive orders by doing:
# 1. download the HTML for search-result pages
# 2. extract links that match the format of TAPP content, as opposed to other pages or external links
# 3. extract title text and page ID (URL component) from the anchor tag of each link
# 4. use deglaze() to scrape corresponding page for each page ID
# 5. return a tibble containing the president's name, the page title, the page ID, and the text of the executive order

exec_links <- read_html('http://www.presidency.ucsb.edu/executive_orders.php?year=2009&Submit=DISPLAY') %>%
  html_nodes('a') %>%
  as.character() %>%
  as_tibble() %>%
  full_join(read_html('http://www.presidency.ucsb.edu/executive_orders.php?year=2010&Submit=DISPLAY') %>%
              html_nodes('a') %>%
              as.character() %>%
              as_tibble()) %>%
  full_join(read_html('http://www.presidency.ucsb.edu/executive_orders.php?year=2011&Submit=DISPLAY') %>%
              html_nodes('a') %>%
              as.character() %>%
              as_tibble()) %>%
  full_join(read_html('http://www.presidency.ucsb.edu/executive_orders.php?year=2012&Submit=DISPLAY') %>%
              html_nodes('a') %>%
              as.character() %>%
              as_tibble()) %>%
  full_join(read_html('http://www.presidency.ucsb.edu/executive_orders.php?year=2013&Submit=DISPLAY') %>%
              html_nodes('a') %>%
              as.character() %>%
              as_tibble()) %>%
  full_join(read_html('http://www.presidency.ucsb.edu/executive_orders.php?year=2014&Submit=DISPLAY') %>%
              html_nodes('a') %>%
              as.character() %>%
              as_tibble()) %>%
  full_join(read_html('http://www.presidency.ucsb.edu/executive_orders.php?year=2015&Submit=DISPLAY') %>%
              html_nodes('a') %>%
              as.character() %>%
              as_tibble()) %>%
  full_join(read_html('http://www.presidency.ucsb.edu/executive_orders.php?year=2016&Submit=DISPLAY') %>%
              html_nodes('a') %>%
              as.character() %>%
              as_tibble()) %>%
  full_join(read_html('http://www.presidency.ucsb.edu/executive_orders.php?year=2017&Submit=DISPLAY') %>%
              html_nodes('a') %>%
              as.character() %>%
              as_tibble()) %>%
  unique() %>%
  filter(grepl('../ws/index.php?pid=', value, fixed = TRUE)) %>%
  mutate(title = mapply(extract_link_title, value),
         page_id = mapply(extract_link_page_id, value)) %>%
  select(title, page_id)

obama_orders <- mapply(deglaze, exec_links$page_id) %>%
  t() %>%
  as_tibble() %>%
  unnest() %>%
  mutate(president = mapply(extract_president, title)) %>%
  filter(president == 'Barack Obama')