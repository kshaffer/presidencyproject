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

write_csv(obama_orders, 'data/obama_executive_orders.csv')

# this example creates a tibble of all of Donald Trump's executive orders

trump_exec_links <- read_html('http://www.presidency.ucsb.edu/executive_orders.php?year=2017&Submit=DISPLAY') %>%
  html_nodes('a') %>%
  as.character() %>%
  as_tibble() %>%
  unique() %>%
  filter(grepl('../ws/index.php?pid=', value, fixed = TRUE)) %>%
  mutate(title = mapply(extract_link_title, value),
         page_id = mapply(extract_link_page_id, value)) %>%
  select(title, page_id)

trump_orders <- mapply(deglaze, trump_exec_links$page_id) %>%
  t() %>%
  as_tibble() %>%
  unnest() %>%
  mutate(president = mapply(extract_president, title)) %>%
  filter(president == 'Donald J. Trump')

write_csv(trump_orders, 'data/trump_executive_orders_2017-03-01.csv')


# And George W. Bush's executive orders

exec_links <- read_html('http://www.presidency.ucsb.edu/executive_orders.php?year=2009&Submit=DISPLAY') %>%
  html_nodes('a') %>%
  as.character() %>%
  as_tibble() %>%
  full_join(read_html('http://www.presidency.ucsb.edu/executive_orders.php?year=2008&Submit=DISPLAY') %>%
              html_nodes('a') %>%
              as.character() %>%
              as_tibble()) %>%
  full_join(read_html('http://www.presidency.ucsb.edu/executive_orders.php?year=2007&Submit=DISPLAY') %>%
              html_nodes('a') %>%
              as.character() %>%
              as_tibble()) %>%
  full_join(read_html('http://www.presidency.ucsb.edu/executive_orders.php?year=2006&Submit=DISPLAY') %>%
              html_nodes('a') %>%
              as.character() %>%
              as_tibble()) %>%
  full_join(read_html('http://www.presidency.ucsb.edu/executive_orders.php?year=2005&Submit=DISPLAY') %>%
              html_nodes('a') %>%
              as.character() %>%
              as_tibble()) %>%
  full_join(read_html('http://www.presidency.ucsb.edu/executive_orders.php?year=2004&Submit=DISPLAY') %>%
              html_nodes('a') %>%
              as.character() %>%
              as_tibble()) %>%
  full_join(read_html('http://www.presidency.ucsb.edu/executive_orders.php?year=2003&Submit=DISPLAY') %>%
              html_nodes('a') %>%
              as.character() %>%
              as_tibble()) %>%
  full_join(read_html('http://www.presidency.ucsb.edu/executive_orders.php?year=2002&Submit=DISPLAY') %>%
              html_nodes('a') %>%
              as.character() %>%
              as_tibble()) %>%
  full_join(read_html('http://www.presidency.ucsb.edu/executive_orders.php?year=2001&Submit=DISPLAY') %>%
              html_nodes('a') %>%
              as.character() %>%
              as_tibble()) %>%
  unique() %>%
  filter(grepl('../ws/index.php?pid=', value, fixed = TRUE)) %>%
  mutate(title = mapply(extract_link_title, value),
         page_id = mapply(extract_link_page_id, value)) %>%
  select(title, page_id)

gwb_orders <- mapply(deglaze, exec_links$page_id) %>%
  t() %>%
  as_tibble() %>%
  unnest() %>%
  mutate(president = mapply(extract_president, title)) %>%
  filter(president == 'George W. Bush')

write_csv(gwb_orders, 'data/gwb_executive_orders.csv')


# this example creates a tibble containing all press briefings from the Trump administration
# (January 21, 2017, to the present)

trump_brief_links <- read_html('http://www.presidency.ucsb.edu/press_briefings.php?year=2017&Submit=DISPLAY') %>%
  html_nodes('a') %>%
  as.character() %>%
  as_tibble() %>%
  unique() %>%
  filter(grepl('../ws/index.php?pid=', value, fixed = TRUE)) %>%
  mutate(title = mapply(extract_link_title, value),
         page_id = mapply(extract_link_page_id, value)) %>%
  select(title, page_id)

trump_briefings <- mapply(deglaze, trump_brief_links$page_id) %>%
  t() %>%
  as_tibble() %>%
  unnest() %>%
  filter(date >= '2017-01-21')
