library(tidymodels)
library(tidyverse)
library(parsnip)
library(dbarts)
library(stacks)
library(ranger)
library(rpart)
library(glmnet)
library(poissonreg)
library(patchwork)
library(GGally)
library(skimr)
library(DataExplorer)
library(vroom)
library(forecast)
sample <- "sampleSubmission.csv"
test <- "test.csv"
train <- "train.csv"
sample1 <- vroom(sample)
test1 <- vroom(test)
train1 <- vroom(train)
train1
StoreItem <- train1 %>% 
  filter(store==3,item==3)
plot1 <- StoreItem %>% 
  ggplot(mapping=aes(x=date,y=sales))+
  geom_line() +
  geom_smooth(se=FALSE)

plot2 <- StoreItem %>% 
  pull(sales) %>% 
forecast::ggAcf(.)
plot2
plot3 <- StoreItem %>% 
  pull(sales) %>% 
  forecast::ggAcf(., lag.max=2*365)
plot3

StoreItem2 <- train1 %>% 
  filter(store==4,item==4)
plot4 <- StoreItem2 %>% 
  ggplot(mapping=aes(x=date,y=sales))+
  geom_line() +
  geom_smooth(se=FALSE)

plot5 <- StoreItem2 %>% 
  pull(sales) %>% 
  forecast::ggAcf(.)
plot2
plot6 <- StoreItem2 %>% 
  pull(sales) %>% 
  forecast::ggAcf(., lag.max=2*365)
plot3

(plot1 + plot2+plot3) / (plot4 + plot5 + plot6)
