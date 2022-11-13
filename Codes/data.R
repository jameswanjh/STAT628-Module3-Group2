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



# read cleaned reviews file of ice-cream business in
review = read.csv("Data/icecream_review.csv")

# filter the fields we need
text = tibble(line = 1:151330, text = review$text, stars = review$stars, useful = review$useful, funny = review$funny, cool = review$cool)

# tokenize each review into single words
words = text %>% unnest_tokens(word, text)

# eliminate stop words
data(stop_words)
words <- words %>% anti_join(stop_words)

# count word appearance in each review
word_freq = words %>% count(line, stars, useful, funny, cool, word)
# change column names to make more sense
colnames(word_freq)[1] = "review"
colnames(word_freq)[7] = "count"

# flavor list searched online
flavor = c('banana', 'bubblegum', 'butter', 'butterscotch', 'pecan', 'chocolate', 
           'cheese', 'cake', 'cherry', 'cookie', 'coffee', 'cinnamon', 'apple', 
           'candy', 'vanilla', 'eggnog', 'grape', 'tea', 'mamey', 'mango',
           'maple', 'mint', 'neapolitan', 'pistachio', 'peanut', 'blueberry', 
           'raspberry', 'marshmallows', 'strawberry', 'watermelon', 'melon', 'ube', 
           'taro', 'matcha', 'caramel', 'tiramisu', 'rum', 'praline', 'berry', 
           'sugar', 'chili', 'chilli', 'cranberry', 'cucumber', 'hazelnut', 'kiwi',
           'honey', 'lavendar', 'lemon', 'lime', 'lychee', 'mocha', 'oreo', 'passionfruit',
           'peach', 'peppermint', 'plum', 'toffee', 'sherbet', 'rose', 'pumpkin',
           'pear', 'pineapple', 'salt', 'almond', 'yogurt', 'coconut')
# type list searched online
type = c('cone', 'sundae', 'shakes', 'shake', 'smoothie', 'slush', 'waffle', 'sherbet', 'sorbet', 'yogurt', 'gelato')

# show the word appearance related to flavors & types in each review
flavor_words = word_freq %>% 
  filter(word %in% flavor) %>% 
  pivot_wider(names_from=word,values_from=count, values_fill = 0)
type_words = word_freq %>% 
  filter(word %in% type) %>% 
  pivot_wider(names_from=word,values_from=count, values_fill = 0)

# count and sort the total word appearances related to each flavor & type in all reviews as a whole
flavors_total = as.matrix(sort(colSums(flavor_words[, -c(1:5)]), decreasing = TRUE))
names(flavors_total) = flavor
colnames(flavors_total)[1] = "total"

types_total = as.matrix(sort(colSums(type_words[, -c(1:5)]), decreasing = TRUE))
names(types_total) = type
colnames(types_total)[1] = "total"


# visualize the comparison of word appearances related to each flavor & type using wordcloud
# Note: Please enlarge the canvas window manually if the most frequent words are not shown in the graph
dev.new(width = 1000, height = 1000, unit = "px")
wordcloud(rownames(flavors_total), 
          flavors_total, min.freq = 3, scale=c(10, .5),
          random.order = FALSE,
          random.color = TRUE)

dev.new(width = 1000, height = 1000, unit = "px")
wordcloud(rownames(types_total), 
          types_total, min.freq = 3, scale=c(10, .5),
          random.order = FALSE,
          random.color = TRUE)



# visualize the distribution of star ratings for each flavor & type
flavors = word_freq %>% filter(word %in% flavor)
dev.new(width = 1000, height = 1000, unit = "px")
flavors %>% ggplot(aes(x=stars,color=word,fill=word)) + 
  geom_histogram(aes(y = stat(density) * 0.5), binwidth = 0.5) + 
  scale_y_continuous(labels = percent) + 
  theme(legend.position="none", panel.spacing = unit(0.1, "lines"), strip.text.x = element_text(size = 8)) + 
  xlab("Stars") +
  ylab("Proportion") +
  facet_wrap(~word)

types = word_freq %>% filter(word %in% type)
dev.new(width = 1000, height = 1000, unit = "px")
types %>% ggplot(aes(x=stars,color=word,fill=word)) + 
  geom_histogram(aes(y = stat(density) * 0.5), binwidth = 0.5) + 
  scale_y_continuous(labels = percent) + 
  theme(legend.position="none", panel.spacing = unit(0.1, "lines"), strip.text.x = element_text(size = 8)) + 
  xlab("Stars") +
  ylab("Proportion") +
  facet_wrap(~word)

# show the distribution of star ratings of all reviews
dev.new(width = 1000, height = 1000, unit = "px")
review %>% ggplot(aes(x=stars)) + 
  geom_histogram(aes(y = ..count../sum(..count..))) + 
  xlab("Stars") + 
  ylab("Proportion")
