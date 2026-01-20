#### Reading in data ==============



library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(tidytext)
library(ggplot2)
library(forcats)
library(tibble)
library(scales)
library(here)
library(gt)
library(webshot2)

file_a <- "Week_2/texts/A07594__Circle_of_Commerce.txt"
file_b <- "Week_2/texts/B14801__Free_Trade.txt"


text_a <- read_file(file_a)
text_b <- read_file(file_b)

texts <- tibble(
  doc_title = c("Text A", "Text B"),
  text = c(text_a, text_b)
)

texts <- texts %>% mutate(n_chars = nchar(text))



tokens <- texts %>% unnest_tokens(word, text) %>% mutate(word = str_to_lower(word))

diagnostics <- tokens %>% group_by(doc_title) %>% summarise(n_word_tokens = n(),
                                                                   n_word_types = n_distinct(word))


corpus_diagnostics <- texts %>% select(doc_title, n_chars) %>% left_join(diagnostics, by = "doc_title")



corpus_diagnostics_table <- corpus_diagnostics %>% gt()


#I would say no, they are not similar in length. Text A has nearly 30% more words. 
#Additionally, its vocabulary seems to be larger. Text A has 4773 unique words 
#compared to text B. This is possibly demonstrated by Text A have nearly twice as many 
#characters despite having only a third more total token count. Given that their
# raw counts differ substantially, we would expect for this pattern to presist
#after our preprocessing procedures.


texts <- texts %>% select(doc_title, text)


data("stop_words")

# Add our own project-specific stopwords (you can, and will, expand this list later)
custom_stopwords <- tibble(
  word = c(
    "vnto", "haue", "doo", "hath", "bee", "ye", "thee"
  )
)

all_stopwords <- bind_rows(stop_words, custom_stopwords) %>%
  distinct(word)

word_counts <- texts %>%
  unnest_tokens(word, text) %>%
  mutate(word = str_to_lower(word)) %>%
  anti_join(all_stopwords, by = "word") %>%
  count(doc_title, word, sort = TRUE)

doc_lengths <- word_counts %>% group_by(doc_title) %>% summarise(document_word_sum = sum(n))


new_merge <- word_counts %>% left_join(doc_lengths, by = "doc_title")

word_counts_normalized <- new_merge %>% mutate(relative_freq = n / document_word_sum)

comparing_trade_frequency <- word_counts_normalized %>% filter(word == "trade")


comparing_trade_frequency %>% gt()



# Plotting the relative frequency


plot_n_words <- 20  # you can change this as needed

# Select the most frequent words overall
word_comparison_tbl <- word_counts_normalized %>%
  pivot_wider(
    names_from = doc_title,
    values_from = relative_freq,
    values_fill = 0
  ) %>%
  mutate(max_n = pmax(`Text A`, `Text B`)) %>%
  arrange(desc(max_n))

word_plot_data <- word_comparison_tbl %>%
  slice_head(n = plot_n_words) %>%
  pivot_longer(
    cols = c(`Text A`, `Text B`),
    names_to = "doc_title",
    values_to = "relative_freq"
  ) %>%
  mutate(word = fct_reorder(word, relative_freq, .fun = max))

ggplot(word_plot_data, aes(x = relative_freq, y = word)) + 
  geom_col() +
  facet_wrap(~ doc_title, scales = "free_x") +
  labs(
    title = "Most frequent words (stopwords removed)",
    subtitle = paste0(
      "Top ", plot_n_words,
      " words by maximum frequency across both texts"
    ),
    x = "Word frequency",
    y = NULL
  ) +
  theme_minimal()

#Question 1: Does Text A or Text B use "trade" more proportionally? And 
#how does this compare to what the raw counts suggested?


#Response 1: We can observe that although in Text A the word "trade" is uttered 47 more times
#than in Text B, when we take into account frequency, we find that the word 
#"trade" is more common in Text B than Text A (0.0231 vs 0.0158)


#Question 2: We normalized by dividing each word count by the total words in that document (after
#stopword removal). How would your results change if you normalized by the original
#document length (before stopword removal)? Would this be better or worse, and why? 
#[This is a harder question than it would seem at first! Review the lecture notes].

#Response 2: The style of writing would have a large impact on the relative frequencies.
#Rather than calculating the frequency of a word like "trade" relative to other topics 
#and subjects, whether "trade" was more common in one text compared to another 
# would depend on whether either had a disproportionate number of filler words. 
#It would seem that removing stopwords helps illustrate patterns that are more
#meaningful to us. Not whether "trade" is used more frequently relative to "it"
# "and" "the" or other stop words, but whether "trade" is used more relative to
#"exchange."

