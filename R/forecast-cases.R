
#--------------- intro ---------------------------


#using Vector Autoregression (VAR), we will create a forecast of covid new cases in Canada

# this is not really a good way to forecast the spread of a disease: it's meant to forecast macroeconomic variables
#but it will be fun!

# in this analysis, we make three basic assumptions
# 1 the rate of growth of cases today is related to the rate of growth in cases yesterday, in a given country
# pretty self explanatory
# 2 the rate of growth of cases today in a given country is related to the rate of growth of cases yesterday in 
#   ANOTHER country
# less obvious, but in a globalized world not crazy
# 3 the rate of growth of cases of COVID 19 is not affected by anything else
#completely ridiculous



#------------------ preamble --------------------

library(tidyverse)



ts_packages <- c("vars", "tidyquant", "timetk", "sweep", "forecast")

# install.packages(ts_packages)

#lapply(ts_packages, library, character_only=TRUE)
pacman::p_load(
  "vars", 
  "tidyquant", 
  "timetk", 
  "sweep", 
  "forecast"
)

# ----------------- load and clean data ------------------------


covid_req <- httr::GET("https://api.covid19api.com/all")

covid_list <- httr::content(covid_req)

dat_covid <- bind_rows(covid_list %>% map(as_tibble))



dat_covid_clean <- dat_covid %>% 
  
  janitor::clean_names() %>% 
  mutate(
    date = as.Date(date)
  ) %>% 
  
  arrange(
    date
  ) %>% 
  
  group_by(
    country, date, status
  ) %>% 
  
  summarize(
    cases = sum(cases)
  ) %>% 
  
  ungroup()

# check data


test_plot <- dat_covid_clean %>% 
  filter(country %in% "Canada") %>% 
  filter(status %in% "confirmed") %>% 
  ggplot(aes(x=date, y=cases)) 

test_plot + geom_point(stat="identity")  

#looks good 


# ---------------- ts data ---------------------


# lets set up our data for time series analysis 

# lets consider the top few countries in terms of cases in our analysis as of today

dat_covid_clean %>% 
  group_by(country) %>% 
  filter(status %in% "confirmed") %>% 
  filter(date >= Sys.Date() - 1) %>% 
  arrange(-cases)

# somewhat arbitrary - looks like there's a dropoff after france

# we'll pick the top 7 Countries plus Canada

var_countries <- c("China", "Italy", "US", "Spain", "Germany", "Iran", "France", "Canada")


dat_covid_ts_wide <- dat_covid_clean %>% 
  filter(country %in% var_countries) %>% 
  group_by(country) %>% 
  pivot_wider(names_from= status, values_from = cases) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  # i think one of the ts packages also has a function called 'select', which causes problems
  dplyr::select(country, date, confirmed)




# prepping for time series: one column per country ; each country is a series

dat_covid_ts_widest <- dat_covid_ts_wide %>% 
  pivot_wider(names_from = country, values_from = confirmed)

dat_covid_ts_widest

#that looks right


#------------------ setting up to forecast -------------------


# we need what's called 'stationary data;' essentially, the mean of the data should be constant over time

# lets look 

ggplot(dat_covid_ts_widest, aes(x=date, y=Canada)) + 
  geom_line()

# definitely not stationary - this is a exponential trend 

# let's try taking the first difference 

dat_covid_lag <- dat_covid_ts_widest %>% 
  mutate_at(
    vars(Canada, China, France, Germany, Iran, Italy, Spain, US), 
    .funs = ~. - lag(.)
  )

ggplot(dat_covid_lag, aes(x=date, y=Canada)) + 
  geom_line()

# not quite there

dat_covid_lag2 <- dat_covid_lag %>% 
  mutate_at(
    vars(Canada, China, France, Germany, Iran, Italy, Spain, US), 
    .funs = ~. - lag(.)
  )

ggplot(dat_covid_lag2, aes(x=date, y=Canada)) + 
  geom_line()

# not great but it'll have to do 

# it's mean stationary but clearly the variance is increasing over time 
# we could manipulate the series more to make it look better, but this will also make it harder to interpret

# since we've taken two lags, we now have two empty rows 

dat_covid_lag2 <- dat_covid_lag2 %>% 
  drop_na()


#------------------ training model -------------------------


# let's pick a period of time to 'train' our model 

# arbitrarily, let's pick one week ago 

train <- dat_covid_lag2 %>% 
  filter(date <= Sys.Date()-7)

hold_out <- dat_covid_lag2 %>% 
  filter(date > Sys.Date()-7)

#------ Let's try an AR(1) model------------------

# Canada's covid acceleration rate today is a function only of its acceleration rate yesterday

ar_model <- ar(train$Canada, na.action = na.omit)

ar_model

# let's test our model 


ar_prediction <- predict(ar_model, n.ahead = 7)

ar_prediction

ggplot(cbind(hold_out[1:7, c("date", "Canada")], as.data.frame(ar_prediction)), 
       aes(x=date)) + 
  geom_ribbon(aes(ymin = pred - se, ymax = pred + se), alpha = 0.25, fill = scales::muted("green")) +
  geom_line(aes(y = pred), lty = 2) +
  geom_line(aes(y = Canada)) +
  scale_y_continuous()


# not a very good prediction 
# we may need a different model 


#-------------------- Let's try fitting a VAR ----------------------------

# Canada's acceleration of covid is related to acceleration rates in other countries 

# let's try it with US and China, and a lag of 2 

# training the model 

var_model1 <- vars::VAR(train %>% dplyr::select(Canada, China, US), p=2)

#calling vars package explicitly because there's another function in a package loaded also called 'VAR"

var_model1

# testing on holdout data 

var_prediction1 <- predict(var_model1, n.ahead=7)

var_prediction1

ggplot(
  cbind(hold_out[1:7,c("date", "Canada")], as.data.frame(var_prediction1$fcst$Canada)), 
  aes(x = date)
) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.25, fill = scales::muted("purple")) +
  geom_line(aes(y = fcst), lty = 2) +
  geom_line(aes(y = Canada)) +
  scale_y_continuous()

# not as bad as the AR(1)

# let's try imposing a restiction on the variance-covariance matrix


var_model2 <- restrict(var_model1, method = "ser")

var_prediction2 <- predict(var_model2, n.ahead=7)

ggplot(
  cbind(hold_out[1:7,c("date", "Canada")], as.data.frame(var_prediction2$fcst$Canada)), 
  aes(x = date)
) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.25, fill = scales::muted("yellow")) +
  geom_line(aes(y = fcst), lty = 2) +
  geom_line(aes(y = Canada)) +
  scale_y_continuous()

# not really any difference 

# let's try with a longer lag

var_model3 <- vars::VAR(train %>% dplyr::select(Canada, China, US, France, Germany, Italy, Iran), p=5)

var_prediction3 <- predict(var_model3, n.ahead=7)

ggplot(
  cbind(hold_out[1:7,c("date", "Canada")], as.data.frame(var_prediction3$fcst$Canada)), 
  aes(x = date)
) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.25, fill = scales::muted("red")) +
  geom_line(aes(y = fcst), lty = 2) +
  geom_line(aes(y = Canada)) +
  scale_y_continuous()


# try with more countries, different lags, if time 


#-----------forecasting the growth rate of cases next week-----------

# we can check next wednesday and see if we were right 

# pick the best forecast model and generate a prediction 

# then we can try to plot it in a fun way 





