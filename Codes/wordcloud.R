library("tm")
library("SnowballC")
library("wordcloud")


text=as.matrix(c(214,1233,23,314),nrow=4)
rownames(text)=c('apple','banana','grasp','peach')
dev.new(width = 1000, height = 1000, unit = "px")

wordcloud(rownames(text), 
          text, min.freq =3, scale=c(5, .2),
          random.order = FALSE,
          random.color = FALSE,
          colors= c("indianred1","indianred2","indianred3","indianred"))
