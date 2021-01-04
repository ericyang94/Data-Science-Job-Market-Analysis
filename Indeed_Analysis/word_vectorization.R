library(tidyverse)
library(tm)
library(text2vec)

df <- read.csv('Indeed_Analysis/listings_unitedstates_cleaned.csv')

# plot years of experience
experience <- unlist(str_extract_all(unlist(df$Description), "\\-*[0-9]+\\.*[0-9]*"))
experience <- as.numeric(experience)
experience <- abs(experience)
experience <- experience[(experience > 0) & (experience < 10) & (experience%%1 == 0)]
experience <- data.frame(years = experience)

ggplot(experience, aes(x = years, fill = "red"))+
  geom_bar()+
  labs(title = "Most Common Years of Experience Desired")+
  xlab("Years of Expereience")+
  ylab("Frequency")+
  theme(legend.position = "none")

# clean up descriptions for analysis
df$Description <- enc2utf8(df$Description)
df$Description = tolower(df$Description)
df$Description = removeWords(df$Description, stopwords("english"))
df$Description = removePunctuation(df$Description)
df$Description = removeNumbers(df$Description)
df$Description = str_trim(df$Description)

# create unigrams and bigrams
it = itoken(df$Description, tolower, word_tokenizer)
words <- create_vocabulary(it, ngram = c(1L, 2L))
words <- prune_vocabulary(words, term_count_min = 20)




