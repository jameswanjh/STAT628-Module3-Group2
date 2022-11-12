library("rjson")
library(dplyr)
library(tidytext)
library(tidyr)

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
text = tibble(line = 1:151330, text = review$text)
words = text %>%
  unnest_tokens(word, text)
data(stop_words)
words <- words %>%
  anti_join(stop_words)
freq_words = words %>%
  count(line, word)
colnames(freq_words)[1] = "review"
colnames(freq_words)[3] = "count"
freq_words %>% pivot_wider(names_from = word, values_from = count, values_fill = 0)
flavor = c('banana', 'bubblegum', 'butter', 'butterscotch', 'pecan', 'chocolate', 
           'cheese', 'cake', 'cherry', 'cookie', 'coffee', 'cinnamon', 'apple', 
           'candy', 'vanilla', 'eggnog', 'grape', 'tea', 'lucuma', 'mamey', 'mango',
           'maple', 'mint', 'neapolitan', 'pistachio', 'peanut', 'blueberry', 
           'raspberry', 'marshmallows', 'strawberry', 'watermelon', 'melon', 'ube', 
           'taro', 'matcha', 'caramel', 'tiramisu', 'rum', 'praline', 'berry', 
           'sugar', 'chili', 'chilli', 'cranberry', 'cucumber', 'hazelnut', 'kiwi',
           'honey', 'lavendar', 'lemon', 'lime', 'lychee', 'mocha', 'oreo', 'passionfruit',
           'peach', 'peppermint', 'plum', 'toffee', 'sherbet', 'rose', 'pumpkin',
           'pear', 'pineapple', 'salt', 'custard', 'almond', 'yogurt')
flavor_words = freq_words %>% 
  filter(word %in% flavor) %>% 
  pivot_wider(names_from=word,values_from=count, values_fill = 0)
freq_flavors = colSums(flavor_words[, -1])
names(freq_flavors) = flavor
sort(freq_flavors, decreasing = TRUE)
