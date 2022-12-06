library(tidyverse)
library(MASS)
library(ggplot2)
library(EnvStats)

## business2 is what we use.
business=read.csv('./Data/icecream.csv')
business1=business%>%dplyr::select(business_id,name,city,stars,
                            review_count,attributes.BikeParking,
                            attributes.BusinessAcceptsCreditCards,
                            attributes.RestaurantsTakeOut,
                            attributes.WheelchairAccessible,
                            attributes.OutdoorSeating,
                            attributes.HasTV,
                            attributes.BusinessParking)%>%na.omit()

## make attributes to be dummy variables
business2=business1%>%
  mutate(attributes.BikeParking=(attributes.BikeParking==TRUE),
         attributes.BusinessAcceptsCreditCards=(attributes.BusinessAcceptsCreditCards==TRUE),
         attributes.RestaurantsTakeOut=(attributes.RestaurantsTakeOut==TRUE),
         attributes.WheelchairAccessible=(attributes.WheelchairAccessible==TRUE),
         attributes.OutdoorSeating=(attributes.OutdoorSeating==TRUE),
         attributes.HasTV=(attributes.HasTV==TRUE),
         attributes.BusinessParking=(attributes.BusinessParking%in%c("{'garage': True",
                                                                     "{u'valet': True")))
         #attributes.GoodForKids=(attributes.GoodForKids==TRUE))


## try to fit a linear model
lm1=lm(stars~attributes.BikeParking+
         attributes.BusinessAcceptsCreditCards+
         attributes.RestaurantsTakeOut+
         attributes.WheelchairAccessible+
         attributes.OutdoorSeating+
         attributes.HasTV,
       business2)
summary(lm1)

aov1=aov(stars~attributes.BikeParking+
           attributes.BusinessAcceptsCreditCards+
           attributes.RestaurantsTakeOut+
           attributes.WheelchairAccessible+
           attributes.OutdoorSeating+
           attributes.HasTV,
         business2)
summary(aov1)


aov2=aov(stars~attributes.BikeParking+attributes.BusinessAcceptsCreditCards+
           attributes.OutdoorSeating,business2)
summary(aov2)

lm2=lm(stars~attributes.BikeParking+attributes.BusinessAcceptsCreditCards+
         attributes.OutdoorSeating,business2)
summary(lm2)
plot(lm2)

lm3=polr(as.factor(stars)~attributes.BikeParking+attributes.BusinessAcceptsCreditCards+
           attributes.OutdoorSeating,business2)
summary(lm3)

## EDA
### BikeParking
business2%>%
  ggplot(aes(x=attributes.BikeParking,y=stars,
             fill=attributes.BikeParking))+
  geom_boxplot()+ 
  scale_x_discrete(name ="BikeParking",label=c('FALSE'='No','TRUE'='Yes'))+
  scale_fill_discrete(name ="BikeParking",label=c('FALSE'='No','TRUE'='Yes'))+
  theme_classic()

### RestaurantsTakeOut
business2%>%
  ggplot(aes(x=attributes.RestaurantsTakeOut,y=stars,
             fill=attributes.RestaurantsTakeOut))+
  geom_boxplot()+ 
  scale_x_discrete(name ="TakeOut",label=c('FALSE'='No','TRUE'='Yes'))+
  scale_fill_discrete(name ="TakeOut",label=c('FALSE'='No','TRUE'='Yes'))+
  theme_classic()

### HasTV
business2%>%
  ggplot(aes(x=attributes.HasTV,y=stars,
             fill=attributes.HasTV))+
  geom_boxplot()+ 
  stat_n_text() + 
  scale_x_discrete(name ="HasTV",label=c('FALSE'='No','TRUE'='Yes'))+
  scale_fill_discrete(name ="HasTV",label=c('FALSE'='No','TRUE'='Yes'))+
  theme_classic()

### BusinessParking
business2%>%
  ggplot(aes(x=attributes.BusinessParking,y=stars,
             fill=attributes.BusinessParking))+
  geom_boxplot()+ 
  scale_x_discrete(name ="BusinessParking",label=c('FALSE'='No','TRUE'='Yes'))+
  scale_fill_discrete(name ="BusinessParking",label=c('FALSE'='No','TRUE'='Yes'))+
  theme_classic()

### WheelchairAccessible
business2%>%
  ggplot(aes(x=attributes.WheelchairAccessible,y=stars,
             fill=attributes.WheelchairAccessible))+
  geom_boxplot()+ 
  stat_n_text() +   
  scale_x_discrete(name ="WheelchairAccessible",label=c('FALSE'='No','TRUE'='Yes'))+
  scale_fill_discrete(name ="WheelchairAccessible",label=c('FALSE'='No','TRUE'='Yes'))+
  theme_classic()



## Decision Tree
library(rpart)
library(rpart.plot)
set.seed(1234)

Tree1 <- rpart(as.factor(stars)~attributes.BikeParking+
                 attributes.RestaurantsTakeOut+
                 attributes.BusinessParking,business2,
               method='class',
               control=rpart.control(minsplit=nrow(business2)%/%100,cp=0.001)) 
pruned_Tree1<- prune(Tree1, cp=0.005 )
rsq.rpart(pruned_Tree1)
rpart.plot(pruned_Tree1)

importance=data.frame(name=names(Tree1$variable.importance),
                             importance=Tree1$variable.importance)
importance%>%ggplot()+
  geom_bar(aes(y=reorder(name,importance),x=importance,fill=name),
           stat='identity')+theme_bw()
