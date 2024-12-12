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
library(modeltime)
library(timetk)
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


#Forest
StoreItem <- train1 %>% 
  filter(store==6,item==40)
StoreItem <- train1 %>%
  mutate(sales=log(sales))
StoreItem
my_recipe <- recipe(sales~.,data=StoreItem) %>% 
  step_date(date, features="dow") %>% 
  step_date(date, features="month") %>% 
  step_date(date, features="year") %>% 
  step_date(date, features="doy") %>% 
  step_date(date, features="decimal") %>% 
  step_range(date_doy,min=0, max=pi) %>% 
  step_mutate(sinDOY=sin(date_doy))
baked <- prep(my_recipe)
spuds <- bake(baked, new_data = StoreItem)
spuds$sinDOY

my_mod_for <- rand_forest(mtry = tune(),
                          min_n = tune(),
                          trees=500) %>% 
  set_engine("ranger") %>% 
  set_mode("regression")

preg_wf_for <- workflow() %>% 
  add_recipe(my_recipe) %>% 
  add_model(my_mod_for)

grid_of_tuning_params_for <- grid_regular(mtry(range=c(1,8)), min_n(), levels = 6)
folds_for <- vfold_cv(StoreItem, v = 5,repeats=1)

CV_results_for <- preg_wf_for %>% 
  tune_grid(resamples=folds_for,
            grid=grid_of_tuning_params_for,
            metrics=metric_set(smape))
CV_results_for
bestTune_for <- CV_results_for %>% 
  select_best(metric="smape")
summary(CV_results_for)
bestTune_for
print(collect_metrics(CV_results_for), n=80)
final_wf_forest <- 
  forest_wf %>% 
  finalize_workflow(bestTune_forest) %>% 
  fit(data=mycleandata)

predict <- final_wf_forest %>% 
  predict(new_data=test1)
#SArima

library(timetk)

train2 <- train1 %>% filter(store==4, item==4)
cv_split <- time_series_split(train2,assess = "3 months", cumulative = TRUE)
cv_split %>% 
  tk_time_series_cv_plan() %>% 
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)
test2 <- test1 %>% 
  filter(store==4, item==4)
arima_recipe <- recipe(sales~.,data=train2) %>% 
  step_date(date, features="dow") %>% 
  step_date(date, features="month") %>% 
  step_date(date, features="year") %>% 
  step_date(date, features="doy") %>% 
  step_date(date, features="decimal") %>% 
  step_range(date_doy,min=0, max=pi) %>% 
  step_mutate(sinDOY=sin(date_doy))

arima_model <- arima_reg(seasonal_period = 5,
                         non_seasonal_ar = 5,
                         non_seasonal_ma = 5,
                         seasonal_ar = 2,
                         seasonal_ma = 2,
                         non_seasonal_differences = 10,
                         seasonal_differences = 10) %>% 
  set_engine("auto_arima")

arima_wf <- workflow() %>% 
  add_recipe(arima_recipe) %>% 
  add_model(arima_model) %>% 
  fit(data=training(cv_split))
cv_results <- modeltime_calibrate(arima_wf,
                                  new_data=testing(cv_split))
plot3 <- cv_results %>% 
  modeltime_forecast(new_data=testing(cv_split),
                     actual_data = training(cv_split)) %>% 
  plot_modeltime_forecast(.interactive=FALSE)

fullfit <- cv_results %>% 
  modeltime_refit(data=train2)

plot4 <- fullfit %>% modeltime_forecast(
  new_data = test2,
  actual_data = train2) %>% 
  plot_modeltime_forecast(.interactive=FALSE)

plotly::subplot(plot1,plot3,plot2,plot4,nrows=2)
fullfit + fullfit2
fullfit
fullfit2

#Prophet

train2 <- train1 %>% filter(store==4, item==4)
cv_split <- time_series_split(train2,assess = "3 months", cumulative = TRUE)
cv_split %>% 
  tk_time_series_cv_plan() %>% 
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)
test2 <- test1 %>% 
  filter(store==4, item==4)
prophet_recipe <- recipe(sales~.,data=train2) %>% 
  step_date(date, features="dow") %>% 
  step_date(date, features="month") %>% 
  step_date(date, features="year") %>% 
  step_date(date, features="doy") %>% 
  step_date(date, features="decimal") %>% 
  step_range(date_doy,min=0, max=pi) %>% 
  step_mutate(sinDOY=sin(date_doy))

prophet_model <- prophet_reg() %>% 
  set_engine(engine = "prophet")
prophet_wf <- workflow() %>% 
  add_recipe(prophet_recipe) %>% 
  add_model(prophet_model) %>% 
  fit(data=training(cv_split))
cv_results <- modeltime_calibrate(prophet_wf,
                                  new_data=testing(cv_split))
cv_results
plot3 <- cv_results %>% 
  modeltime_forecast(new_data=testing(cv_split),
                     actual_data = training(cv_split)) %>% 
  plot_modeltime_forecast(.interactive=FALSE)

fullfit <- cv_results %>% 
  modeltime_refit(data=train2)

plot4 <- fullfit %>% modeltime_forecast(
  new_data = test2,
  actual_data = train2) %>% 
  plot_modeltime_forecast(.interactive=FALSE)
predict <- fullfit %>% modeltime_forecast(
  new_data = test2,
  actual_data = train2) 
predict
plot4
plotly::subplot(plot1,plot3,plot2,plot4,nrows=2)
fullfit + fullfit2
fullfit
fullfit2
