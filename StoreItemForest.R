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

test <- "/kaggle/input/demand-forecasting-kernels-only/test.csv"
train <- "/kaggle/input/demand-forecasting-kernels-only/train.csv"

test1 <- vroom(test)
train1 <- vroom(train)
train1 <- train1 %>%
  mutate(sales=log(sales))
for(s in 1:10){
  for(i in 1:50){
    train2 <- train1 %>% 
      filter(store==s,item==i)
    test2 <- test1 %>% 
      filter(store==s,item==i)
    my_recipe <- recipe(sales~.,data=train2) %>% 
      step_date(date, features="dow") %>% 
      step_date(date, features="month") %>% 
      step_date(date, features="year") %>% 
      step_date(date, features="doy") %>% 
      step_date(date, features="decimal") %>% 
      step_range(date_doy,min=0, max=pi) %>% 
      step_mutate(sinDOY=sin(date_doy))
    my_mod_for <- rand_forest(mtry = 8,
                              min_n = 40,
                              trees=250) %>% 
      set_engine("ranger") %>% 
      set_mode("regression")
    
    preg_wf_for <- workflow() %>% 
      add_recipe(my_recipe) %>% 
      add_model(my_mod_for)
    
    final_wf_forest <- 
      preg_wf_for %>% 
      fit(data=train2)
    
    predict <- final_wf_forest %>% 
      predict(new_data=test2)
    predict <- exp(predict)
    kaggle_submission <- predict %>% 
      bind_cols(., test2) %>% 
      select(id, .pred) %>% 
      rename(sales=.pred)
    
    if(s==1 & i==1){
      all_preds <- kaggle_submission
    } else {
      all_preds <- bind_rows(all_preds, kaggle_submission)
    }
    
  }
}

vroom_write(x=all_preds, file="./submission.csv", delim=",")

