install.packages("RColorBrewer")
install.packages("wordcloud2")
library(tidyverse)
library(scales)
library(forcats)
library(wordcloud2)
library(tidytext)
library(lubridate)
library(quanteda)
library(tokenizers)
install.packages("usethis")
library(devtools)
devtools::install_github("lchiffon/wordcloud2")

listing<- listings_unitedstates_cleaned

#Extracting sentences which have the word experience
sentence<- tokenize_sentences(listing$Description)
theSentences <- tokens(sentence,what="sentence")
experience<- data.frame(grep("experience",theSentences,value=TRUE))
experience<- data.frame(number=c(1:nrow(experience)),description= grep("experience",theSentences,value=TRUE))

#List of stop words
stop_word_created<- data.frame(c("experience","analysis","data","science","complex","environment","preferred","qualifications","required","development","design","relevant","develop","tools","including","ability","business","scientist","quantitative","related","learning","e.g"),c("NA"))
colnames(stop_word_created)<- c("word","lexicon")
stop_words<- rbind(stop_words,stop_word_created)

##Word count
experience %>%
  unnest_tokens(word,description) %>%
  count(word) %>% anti_join(stop_words) %>% arrange(desc(n)) %>% top_n(50) %>%
  ggplot(aes(x=fct_reorder(word,n),y=n)) + geom_bar(stat='identity') + 
  coord_flip() + theme_bw()+
  labs(title='Top 50 Words for experience',
       x='Count',
       y= 'Word')

##Word cloud
topWords <- experience %>%
  unnest_tokens(word,description) %>%
  count(word) %>% anti_join(stop_words) %>%
  arrange(desc(n)) %>% top_n(200) 
colnames(topWords)<- c('word','freq')

letterCloud(topWords, word = "DATA",color='random-light' , backgroundColor="black")

wordcloud2(topWords, size=0.5, shape= 'star')

##TF-IDF Analysis
exp<- experience %>%
  unnest_tokens(word,description) %>%
  count(number,word)

minLength <- 20  # focus on long reviews 
exp_long <- exp %>%
  group_by(number) %>%
  summarize(length = sum(n)) %>%
  filter(length >= minLength)

expTFIDF <- exp %>%
  filter(number %in% exp_long$number) %>%
  bind_tf_idf(word,number,n) %>%
  group_by(number) %>%
  arrange(desc(tf_idf)) %>%
  slice(1:20) %>% # get top 10 words in terms of tf-idf
  ungroup() %>%
  mutate(xOrder=n():1)

n <- 50
plot.df <- expTFIDF %>%
  filter(number %in% exp_long$number[1:n])

plot.df %>%
  ggplot(aes(x=xOrder,y=tf_idf,fill=factor(number))) + 
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ number,scales='free') +
  scale_x_continuous(breaks = plot.df$xOrder,
                     labels = plot.df$word,
                     expand = c(0,0)) + 
  coord_flip()+
  labs(x='Word',
       y='TF-IDF',
       title = 'Top TF-IDF Words in Experience',
       subtitle = paste0('Based on first ', 
                         n,
                         ' job descriptions'))+
  theme(legend.position = "none")

#Creating categories for positions
listing <- listing %>%
  mutate(category= ifelse(grepl('Junior|Jr|Jr.|Early|Entry',listing$Title, ignore.case = TRUE),"Junior DS",
                   ifelse(grepl('Senior|Sr.|Sr|Head|Manager',listing$Title, ignore.case = TRUE),"Senior DS",
                   ifelse(grepl('Intern|Internship|Summer',listing$Title, ignore.case = TRUE),"Intern DS","DS"))))

table(listing$category)

list_cat<- listing %>%
  select(Description,category) %>%
  filter(str_detect(Description, "Experience")) %>%
  unnest_tokens(word,Description) %>%
  count(category,word) 

list_cat %>%
  anti_join(stop_words) %>%
  group_by(category) %>%
  top_n(20) %>%
  ggplot(aes(x=reorder_within(word,n,category),
             y=n,
             fill=category)) + 
  geom_bar(stat='identity') + 
  coord_flip() +
  scale_x_reordered() +
  facet_wrap(~category,scales = 'free',nrow=1) + 
  theme_bw() + 
  theme(legend.position = "none")+
  labs(title = 'Top Words by Category',
       subtitle = 'Stop words removed',
       x = 'Word',
       y = 'Count')
