# Wordcloud of tweets between 2. and 9. Decembre
library(quanteda)
library(stopwords)
library(topicmodels)
library(tidytext)
library(quanteda.textplots)

#Count the number of characters
tweets$tweet_length <- nchar(tweets$Tweet)

#Specify minimum text length (number of characters)
tweets_cloud <- subset(tweets, nchar(Tweet) > 200)

# Tweets between 02.12 and 09.12
tweets_filtered <- tweets_cloud[as.Date(tweets_cloud$tweet_date) >= as.Date("2022-12-01") & as.Date(tweets_cloud$tweet_date) <= as.Date("2022-12-09"),]

# Define additional stopwords
extended_stopwords <- c("one", "like", "get", "use", "can", "#chatgpt", "chatgpt", "#ai", "ai")

# Transform words into tokens, select basic text preprocessing steps
tokens <- tokens(tweets_filtered$Tweet,
                 remove_punct = TRUE,
                 remove_symbols = TRUE,
                 remove_numbers = TRUE,
                 remove_url = TRUE,
                 remove_separators = TRUE)

# remove stopwords
tokens <- tokens_select(tokens, pattern = c(stopwords("english"), extended_stopwords), selection = "remove")

# transform to lowercase
tokens <- tokens_tolower(tokens)

# Stem all words
tokens <-tokens_wordstem(tokens)

# Create n-grams of any length
tokens <- tokens_ngrams(tokens, n = 1:2)

# Create Document-feature-matrix
myDfm <-dfm(tokens)


# Create LDA model (specify number of topics)
tweets_lda <- LDA(myDfm, k = 3, control = list(seed = 1111))
topics <- as.data.frame(terms(tweets_lda, 50))

# Convert into tidy-format to visualize results 
tweets_lda_td <- tidy(tweets_lda)

# Extract top-terms per topic 
top_terms <- tweets_lda_td %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Visualize top-terms and their loadings (can you assign topic labels based on this information?) 
top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

# Link results to metadata
tmResult <- posterior(tweets_lda)
theta <- tmResult$topics
lda_results <- cbind(tweets_filtered, theta)
rm (theta, tweets_lda,tweets_lda_td,tmResult,top_terms,tokens)



library (ldatuning) 
### Calculate different metrics to estimate the most preferable number of topics for LDA model
## Be aware: The procedure is computation intensive 
# ldatuning uses parallelism, specify the correct number of CPU cores in mc.core parameter to archive best performance

# Calculate selected metrics
result <- FindTopicsNumber(
  myDfm,
  topics = seq(from = 2, to = 10, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE)

# plot results
FindTopicsNumber_plot(result)

# Create a wordcloud from your Document-Feature-Matrix
textplot_wordcloud(myDfm,
                   min_size = 0.5,
                   max_size = 4,
                   min_count = 10,
                   max_words = 50,
                   color = "darkblue")

