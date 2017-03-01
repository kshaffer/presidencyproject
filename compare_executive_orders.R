source('functions.R')
library(tidytext)
library(igraph)
library(ggraph)
library(scales)
library(stringr)

trump_orders <- read_csv('data/trump_executive_orders_2017-03-01.csv')
obama_orders <- read_csv('data/obama_executive_orders.csv')
gwb_orders <- read_csv('data/gwb_executive_orders.csv')

exec_stop_words <- as_tibble(cbind(word = c('section', 'sec', '1', '2', '3', '4', '5', '6', '7', '8', '9'),
                                   lexicon = 'exec orders'))

# tidy words
tidy_trump <- trump_orders %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(exec_stop_words)

tidy_obama <- obama_orders %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(exec_stop_words)

tidy_gwb <- gwb_orders %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(exec_stop_words)

# tidy bigrams
tidy_trump_bigrams <- trump_orders %>%
  unnest_tokens(bigram, text, token = 'ngrams', n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% c(stop_words$word, exec_stop_words$word)) %>%
  filter(!word2 %in% c(stop_words$word, exec_stop_words$word)) %>%
  unite(bigram, word1, word2, sep = ' ')

tidy_obama_bigrams <- obama_orders %>%
  unnest_tokens(bigram, text, token = 'ngrams', n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% c(stop_words$word, exec_stop_words$word)) %>%
  filter(!word2 %in% c(stop_words$word, exec_stop_words$word)) %>%
  unite(bigram, word1, word2, sep = ' ')

tidy_gwb_bigrams <- gwb_orders %>%
  unnest_tokens(bigram, text, token = 'ngrams', n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% c(stop_words$word, exec_stop_words$word)) %>%
  filter(!word2 %in% c(stop_words$word, exec_stop_words$word)) %>%
  unite(bigram, word1, word2, sep = ' ')


# tidy trigrams
tidy_trump_trigrams <- trump_orders %>%
  unnest_tokens(trigram, text, token = 'ngrams', n = 3) %>%
  separate(trigram, c("word1", "word2", 'word3'), sep = " ") %>%
  filter(!word1 %in% c(stop_words$word, exec_stop_words$word)) %>%
  filter(!word2 %in% c(stop_words$word, exec_stop_words$word)) %>%
  filter(!word3 %in% c(stop_words$word, exec_stop_words$word)) %>%
  unite(trigram, word1, word2, word3, sep = ' ')

tidy_obama_trigrams <- obama_orders %>%
  unnest_tokens(trigram, text, token = 'ngrams', n = 3) %>%
  separate(trigram, c("word1", "word2", 'word3'), sep = " ") %>%
  filter(!word1 %in% c(stop_words$word, exec_stop_words$word)) %>%
  filter(!word2 %in% c(stop_words$word, exec_stop_words$word)) %>%
  filter(!word3 %in% c(stop_words$word, exec_stop_words$word)) %>%
  unite(trigram, word1, word2, word3, sep = ' ')

tidy_gwb_trigrams <- gwb_orders %>%
  unnest_tokens(trigram, text, token = 'ngrams', n = 3) %>%
  separate(trigram, c("word1", "word2", 'word3'), sep = " ") %>%
  filter(!word1 %in% c(stop_words$word, exec_stop_words$word)) %>%
  filter(!word2 %in% c(stop_words$word, exec_stop_words$word)) %>%
  filter(!word3 %in% c(stop_words$word, exec_stop_words$word)) %>%
  unite(trigram, word1, word2, word3, sep = ' ')


# most common words
tidy_obama %>%
  count(word, sort = TRUE) %>%
  filter(n > 400) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = word)) +
  geom_bar(stat = 'identity') +
  theme(legend.position="none") +
  xlab('word') +
  ylab('count') +
  ggtitle('Most common words in Obama executive orders') +
  coord_flip()

tidy_gwb %>%
  count(word, sort = TRUE) %>%
  filter(n > 300) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = word)) +
  geom_bar(stat = 'identity') +
  theme(legend.position="none") +
  xlab('word') +
  ylab('count') +
  ggtitle('Most common words in G.W. Bush executive orders') +
  coord_flip()

tidy_trump %>%
  count(word, sort = TRUE) %>%
  filter(n > 40) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = word)) +
  geom_bar(stat = 'identity') +
  theme(legend.position="none") +
  xlab('word') +
  ylab('count') +
  ggtitle('Most common words in Trump executive orders through Feb 28, 2017') +
  coord_flip()

# most common bigrams
tidy_trump_bigrams %>%
  count(bigram, sort = TRUE) %>%
  filter(n > 15) %>%
  mutate(bigram = reorder(bigram, n)) %>%
  ggplot(aes(bigram, n, fill = bigram)) +
  geom_bar(stat = 'identity') +
  theme(legend.position="none") +
  xlab('bigram') +
  ylab('count') +
  ggtitle('Most common bigrams in Trump executive orders through Feb 28, 2017') +
  coord_flip()

tidy_obama_bigrams %>%
  count(bigram, sort = TRUE) %>%
  filter(n > 100) %>%
  mutate(bigram = reorder(bigram, n)) %>%
  ggplot(aes(bigram, n, fill = bigram)) +
  geom_bar(stat = 'identity') +
  theme(legend.position="none") +
  xlab('bigram') +
  ylab('count') +
  ggtitle('Most common bigrams in Obama executive orders
          ') +
  coord_flip()

tidy_gwb_bigrams %>%
  count(bigram, sort = TRUE) %>%
  filter(n > 100) %>%
  mutate(bigram = reorder(bigram, n)) %>%
  ggplot(aes(bigram, n, fill = bigram)) +
  geom_bar(stat = 'identity') +
  theme(legend.position="none") +
  xlab('bigram') +
  ylab('count') +
  ggtitle('Most common bigrams in G.W. Bush executive orders') +
  coord_flip()

# most common trigrams
tidy_trump_trigrams %>%
  count(trigram, sort = TRUE) %>%
  filter(n > 5) %>%
  mutate(trigram = reorder(trigram, n)) %>%
  ggplot(aes(trigram, n, fill = trigram)) +
  geom_bar(stat = 'identity') +
  theme(legend.position="none") +
  xlab('trigram') +
  ylab('count') +
  ggtitle('Most common trigrams in Trump executive orders through Feb 28, 2017') +
  coord_flip()

tidy_obama_trigrams %>%
  count(trigram, sort = TRUE) %>%
  filter(n > 40) %>%
  mutate(trigram = reorder(trigram, n)) %>%
  ggplot(aes(trigram, n, fill = trigram)) +
  geom_bar(stat = 'identity') +
  theme(legend.position="none") +
  xlab('trigram') +
  ylab('count') +
  ggtitle('Most common trigrams in Obama executive orders') +
  coord_flip()

tidy_gwb_trigrams %>%
  count(trigram, sort = TRUE) %>%
  filter(n > 30) %>%
  mutate(trigram = reorder(trigram, n)) %>%
  ggplot(aes(trigram, n, fill = trigram)) +
  geom_bar(stat = 'identity') +
  theme(legend.position="none") +
  xlab('trigram') +
  ylab('count') +
  ggtitle('Most common trigrams in G.W. Bush executive orders') +
  coord_flip()


# bigram network graph
set.seed(2017)
a <- grid::arrow(type = 'closed', length = unit(.1, 'inches'))

bigram_graph <- tidy_gwb_bigrams %>%
  count(bigram, sort = TRUE) %>%
  mutate(bigram = reorder(bigram, n)) %>%
  separate(bigram, c('from', 'to'), sep = ' ') %>%
  filter(n >= 20) %>%
  graph_from_data_frame()

ggraph(bigram_graph, layout = 'fr') +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
  geom_node_point(color = 'lightblue', size = 3) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void() +
  ggtitle('Network of two-word phrases occurring 20 or more times in G.W. Bush executive orders')


# compare corpora
tidy_presidents <- tidy_trump %>%
  full_join(tidy_obama) %>%
  full_join(tidy_gwb) %>%
  unique() %>%
  filter(str_detect(word, "[a-z]"))

obama_percent <- tidy_presidents %>%
  filter(president == 'Barack Obama') %>%
  count(word) %>%
  transmute(word, obamafreq = n / sum(n))

frequency <- tidy_presidents %>%
  filter(president == 'Donald J. Trump') %>%
  count(word) %>%
  mutate(other = n / sum(n)) %>%
  left_join(obama_percent) %>%
  ungroup()

ggplot(frequency, aes(x = other, y = obamafreq, color = abs(obamafreq - other))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  theme(legend.position="none") +
  labs(y = 'Obama', x = 'Trump')

frequency <- tidy_presidents %>% 
  group_by(president) %>% 
  count(word, sort = TRUE) %>% 
  left_join(tidy_presidents %>% 
              group_by(president) %>% 
              summarise(total = n())) %>%
  mutate(freq = n/total)

frequency <- frequency %>% 
  select(president, word, freq) %>% 
  spread(president, freq) 

word_ratios <- tidy_presidents %>%
  filter(str_detect(word, "[a-z]")) %>%
  count(word, president) %>%
  filter(sum(n) >= 10) %>%
  spread(president, n, fill = 0) %>%
  ungroup() %>%
  mutate_each(funs((. + 1) / sum(. + 1)), -word) %>%
  mutate(bush_obama_ratio = log(`George W. Bush` / `Barack Obama`),
         obama_trump_ratio = log(`Barack Obama` / `Donald J. Trump`)) %>%
  arrange(desc(obama_trump_ratio))

word_ratios %>% 
  arrange(abs(obama_trump_ratio))

word_ratios %>%
  filter(!word %in% c('barack', 'obama', 'donald', 'trump', 'bushthe', 'a.m')) %>%
  group_by(obama_trump_ratio < 0) %>%
  top_n(15, abs(obama_trump_ratio)) %>%
  ungroup() %>%
  mutate(word = reorder(word, obama_trump_ratio)) %>%
  ggplot(aes(word, obama_trump_ratio, fill = obama_trump_ratio < 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ylab("Log odds ratio (Obama/Trump)") +
  ggtitle('Words that most uniquely define the text of an administration\'s executive orders') +
  scale_fill_discrete(name = "", labels = c("Obama", "Trump"))

word_ratios %>%
  filter(!word %in% c('barack', 'obama', 'george', 'bush', 'bushthe', 'a.m')) %>%
  group_by(bush_obama_ratio < 0) %>%
  top_n(15, abs(bush_obama_ratio)) %>%
  ungroup() %>%
  mutate(word = reorder(word, bush_obama_ratio)) %>%
  ggplot(aes(word, bush_obama_ratio, fill = bush_obama_ratio < 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ylab("Log odds ratio (Bush/Obama)") +
  ggtitle('Words that most uniquely define the text of an administration\'s executive orders') +
  scale_fill_discrete(name = "", labels = c("Bush", "Obama"))

tidy_president_bigrams <- tidy_trump_bigrams %>%
  full_join(tidy_obama_bigrams) %>%
  full_join(tidy_gwb_bigrams) %>%
  unique()

frequency <- tidy_president_bigrams %>% 
  group_by(president) %>% 
  count(bigram, sort = TRUE) %>% 
  left_join(tidy_president_bigrams %>% 
              group_by(president) %>% 
              summarise(total = n())) %>%
  mutate(freq = n/total)

frequency <- frequency %>% 
  select(president, bigram, freq) %>% 
  spread(president, freq) 

bigram_ratios <- tidy_president_bigrams %>%
  filter(str_detect(bigram, "[a-z]")) %>%
  count(bigram, president) %>%
  filter(sum(n) >= 10) %>%
  spread(president, n, fill = 0) %>%
  ungroup() %>%
  mutate_each(funs((. + 1) / sum(. + 1)), -bigram) %>%
  mutate(bush_obama_ratio = log(`George W. Bush` / `Barack Obama`),
         obama_trump_ratio = log(`Barack Obama` / `Donald J. Trump`)) %>%
  arrange(desc(obama_trump_ratio))

bigram_ratios %>% 
  arrange(abs(obama_trump_ratio))

bigram_ratios %>%
  filter(!bigram %in% c('barack', 'obama', 'donald', 'trump', 'bushthe', 'a.m')) %>%
  group_by(obama_trump_ratio < 0) %>%
  top_n(15, abs(obama_trump_ratio)) %>%
  ungroup() %>%
  mutate(bigram = reorder(bigram, obama_trump_ratio)) %>%
  ggplot(aes(bigram, obama_trump_ratio, fill = obama_trump_ratio < 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ylab("Log odds ratio (Obama/Trump)") +
  ggtitle('Bigrams that most uniquely define the text of an administration\'s executive orders') +
  scale_fill_discrete(name = "", labels = c("Obama", "Trump"))

bigram_ratios %>%
  filter(!bigram %in% c('barack', 'obama', 'george', 'bush', 'bushthe', 'a.m')) %>%
  group_by(bush_obama_ratio < 0) %>%
  top_n(15, abs(bush_obama_ratio)) %>%
  ungroup() %>%
  mutate(bigram = reorder(bigram, bush_obama_ratio)) %>%
  ggplot(aes(bigram, bush_obama_ratio, fill = bush_obama_ratio < 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ylab("Log odds ratio (Bush/Obama)") +
  ggtitle('Bigrams that most uniquely define the text of an administration\'s executive orders') +
  scale_fill_discrete(name = "", labels = c("Bush", "Obama"))
