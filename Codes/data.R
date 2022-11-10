library("rjson")
library(dplyr)
business <- jsonlite::stream_in(file("business.json"),pagesize = 1000)
review <- jsonlite::stream_in(file("review.json"),pagesize = 10000)
icecream = business[grepl('Ice Cream', business$categories), ]
write.csv(icecream, "icecream.csv")
icecream_review=review%>%filter(business_id%in% icecream$business_id)
write.csv(icecream_review, "icecream_review.csv")
