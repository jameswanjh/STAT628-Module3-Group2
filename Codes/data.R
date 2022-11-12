library("rjson")
library(tidytext)
library(tidyr)
library("tm")
library("SnowballC")
library("wordcloud")
library(ggplot2)
library(tidyverse)
library(scales)

# read the original json data in as dataframes
business <- jsonlite::stream_in(file("business.json"),pagesize = 1000)
review <- jsonlite::stream_in(file("review.json"),pagesize = 10000)

# select only "ice cream" category
icecream = business[grepl('Ice Cream', business$categories), ]
write.csv(icecream, "icecream.csv")

# select reviews of ice-cream business
icecream_review=review%>%filter(business_id%in% icecream$business_id)
write.csv(icecream_review, "icecream_review.csv")


review = read.csv("Data/icecream_review.csv")
text = tibble(line = 1:151330, text = review$text, stars = review$stars, useful = review$useful, funny = review$funny, cool = review$cool)
words = text %>%
  unnest_tokens(word, text)
data(stop_words)
words <- words %>%
  anti_join(stop_words)
word_freq = words %>%
  count(line, stars, useful, funny, cool, word)
colnames(word_freq)[1] = "review"
colnames(word_freq)[7] = "count"

flavor = c('banana', 'bubblegum', 'butter', 'butterscotch', 'pecan', 'chocolate', 
           'cheese', 'cake', 'cherry', 'cookie', 'coffee', 'cinnamon', 'apple', 
           'candy', 'vanilla', 'eggnog', 'grape', 'tea', 'lucuma', 'mamey', 'mango',
           'maple', 'mint', 'neapolitan', 'pistachio', 'peanut', 'blueberry', 
           'raspberry', 'marshmallows', 'strawberry', 'watermelon', 'melon', 'ube', 
           'taro', 'matcha', 'caramel', 'tiramisu', 'rum', 'praline', 'berry', 
           'sugar', 'chili', 'chilli', 'cranberry', 'cucumber', 'hazelnut', 'kiwi',
           'honey', 'lavendar', 'lemon', 'lime', 'lychee', 'mocha', 'oreo', 'passionfruit',
           'peach', 'peppermint', 'plum', 'toffee', 'sherbet', 'rose', 'pumpkin',
           'pear', 'pineapple', 'salt', 'almond', 'yogurt')
flavor_words = word_freq %>% 
  filter(word %in% flavor) %>% 
  pivot_wider(names_from=word,values_from=count, values_fill = 0)
freq_flavors = as.matrix(colSums(flavor_words[, -c(1:5)]))
names(freq_flavors) = flavor
sort(freq_flavors, decreasing = TRUE)

type = c('cone', 'sundae', 'shakes', 'shake', 'smoothie', 'slush', 'waffle', 'sherbet', 'sorbet', 'yogurt', 'gelato')
type_words = word_freq %>% 
  filter(word %in% type) %>% 
  pivot_wider(names_from=word,values_from=count, values_fill = 0)
freq_types = as.matrix(colSums(type_words[, -c(1:5)]))
names(freq_types) = type

dev.new(width = 1000, height = 1000, unit = "px")
wordcloud(rownames(freq_flavors), 
          freq_flavors, min.freq =3, scale=c(10, .5),
          random.order = FALSE,
          random.color = TRUE)

flavors = word_freq %>% filter(word %in% flavor)
dev.new(width = 1000, height = 1000, unit = "px")
flavors %>% ggplot(aes(x=stars,color=word,fill=word)) + geom_histogram(aes(y = stat(density) * 0.5), binwidth = 0.5) + 
  scale_y_continuous(labels = percent) + theme(
                        legend.position="none",
                        panel.spacing = unit(0.1, "lines"),
                        strip.text.x = element_text(size = 8)
                      ) + xlab("") +
                      ylab("Proportion ") +
                      facet_wrap(~word)


wordcloud(rownames(freq_types), 
          freq_types, min.freq =3, scale=c(10, .5),
          random.order = FALSE,
          random.color = TRUE)
types = word_freq %>% filter(word %in% type)
dev.new(width = 1000, height = 1000, unit = "px")
types%>%ggplot(aes(x=stars,color=word,fill=word))+geom_histogram(aes(y = ..density../sum(..density..))) + theme(
                        legend.position="none",
                        panel.spacing = unit(0.1, "lines"),
                        strip.text.x = element_text(size = 8)
                      ) +xlab("") +
                      ylab("Proportion ") +
                      facet_wrap(~word)
types %>% ggplot(aes(x=stars,color=word,fill=word))+geom_histogram(aes(y = stat(density) * 0.5), binwidth = 0.5) + 
  scale_y_continuous(labels = percent) + 
  theme(
  legend.position="none",
  panel.spacing = unit(0.1, "lines"),
  strip.text.x = element_text(size = 8)
) + xlab("") +
  ylab("Proportion ") +
  facet_wrap(~word)

review%>%ggplot(aes(x=stars))+geom_histogram(aes(y = ..count../sum(..count..)))
