library(tidyverse)
library(MASS)
library(ggplot2)

review=read.csv('./Data/type_word_freq_per_review.csv')
review1=review%>%
  mutate(zscore=(stars-mean(stars))/sd(stars),shake=shake+shakes)%>%
  dplyr::select(zscore,cone,shake,sundae,waffle,slush)%>%
  filter(cone+shake+sundae+waffle+slush>0)%>%
  mutate(cone=(cone>0),shake=(shake>0),
         sundae=(sundae>0),waffle=(waffle>0),slush=(slush>0))

## ANOVA
lm1=lm(zscore~.,review1)
summary(lm1)

## To recover coefficients from model based on z scores
mean=mean(review$stars)
sd=sd(review$stars)
lm1$coefficients*sd+mean