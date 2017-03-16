source('functions.R')
library(tidytext)
library(stringr)

# executive orders

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

write_csv(trump_orders, 'data/trump_executive_orders.csv')


# press briefings

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

write_csv(trump_briefings, 'data/trump_press_briefings.csv')


# weekly radio addresses

trump_radio_links <- read_html('http://www.presidency.ucsb.edu/satradio.php?year=2017&Submit=DISPLAY') %>%
  html_nodes('a') %>%
  as.character() %>%
  as_tibble() %>%
  unique() %>%
  filter(grepl('../ws/index.php?pid=', value, fixed = TRUE)) %>%
  mutate(title = mapply(extract_link_title, value),
         page_id = mapply(extract_link_page_id, value)) %>%
  select(title, page_id)

trump_radio <- mapply(deglaze, trump_radio_links$page_id) %>%
  t() %>%
  as_tibble() %>%
  unnest() %>%
  filter(date >= '2017-01-21')

write_csv(trump_radio, 'data/trump_radio_addresses.csv')


# presidential news conferences

trump_news_conference_links <- read_html('http://www.presidency.ucsb.edu/news_conferences.php?year=2017&Submit=DISPLAY') %>%
  html_nodes('a') %>%
  as.character() %>%
  as_tibble() %>%
  unique() %>%
  filter(grepl('../ws/index.php?pid=', value, fixed = TRUE)) %>%
  mutate(title = mapply(extract_link_title, value),
         page_id = mapply(extract_link_page_id, value)) %>%
  select(title, page_id)

trump_news_conferences <- mapply(deglaze, trump_news_conference_links$page_id) %>%
  t() %>%
  as_tibble() %>%
  unnest() %>%
  filter(date >= '2017-01-21')

write_csv(trump_news_conferences, 'data/trump_news_conferences.csv')


# presidential proclamations

trump_proclamation_links <- read_html('http://www.presidency.ucsb.edu/proclamations.php?year=2017&Submit=DISPLAY') %>%
  html_nodes('a') %>%
  as.character() %>%
  as_tibble() %>%
  unique() %>%
  filter(grepl('../ws/index.php?pid=', value, fixed = TRUE)) %>%
  mutate(title = mapply(extract_link_title, value),
         page_id = mapply(extract_link_page_id, value)) %>%
  select(title, page_id)

trump_proclamations <- mapply(deglaze, trump_proclamation_links$page_id) %>%
  t() %>%
  as_tibble() %>%
  unnest() %>%
  filter(date >= '2017-01-21')

write_csv(trump_proclamations, 'data/trump_proclamations.csv')


# statements of administration policy

trump_policy_links <- read_html('http://www.presidency.ucsb.edu/saps.php?year=2017&Submit=DISPLAY') %>%
  html_nodes('a') %>%
  as.character() %>%
  as_tibble() %>%
  unique() %>%
  filter(grepl('../ws/index.php?pid=', value, fixed = TRUE)) %>%
  mutate(title = mapply(extract_link_title, value),
         page_id = mapply(extract_link_page_id, value)) %>%
  select(title, page_id)

trump_policy_statements <- mapply(deglaze, trump_policy_links$page_id) %>%
  t() %>%
  as_tibble() %>%
  unnest() %>%
  filter(date >= '2017-01-21')

write_csv(trump_policy_statements, 'data/trump_policy_statements.csv')


# inauguration and SOTU

page_ids <- c('120000','123408')

trump_major_speeches <- mapply(deglaze, page_ids) %>%
  t() %>%
  as_tibble() %>%
  unnest()

write_csv(trump_major_speeches, 'data/trump_major_speeches.csv')
