
# LOAD PACKAGES -----------------------------------------------------------

pacman::p_load(
  "feasts",
  "tidyverse", 
  "tsibble", 
  "fable", 
  "lubridate"
)

# GET UPDATED DATA --------------------------------------------------------

covid_req <- httr::GET("https://api.covid19api.com/all")

covid_list <- httr::content(covid_req)

dat_covid <- bind_rows(covid_list %>% map(as_tibble))


# PREPARE TIMESERIES OF CONFIRMED CASES -----------------------------------

dat_covid_clean <- dat_covid %>% 
  
  janitor::clean_names() %>% 
  mutate(
    date = as.Date(date),
    country = str_remove(country, "Mainland ")
  ) %>% 
  # Arrange by date
  arrange(
    # ___
  ) %>% 
  
  # Group by country, date, and status
  group_by(
    # ___
  ) %>% 
  
  # use summarize to calculate the sum of cases by the groups
  summarize(
    # ____
  ) %>% 
  
  ungroup()

# identify the most infected countries
dat_covid_clean %>% 
  filter(status == "confirmed", date == max(date)) %>% 
  arrange(-cases)

# filter data and convert to tsibble
dat_covid_ts <- dat_covid_clean %>% 
  filter(
    # keep some of the most infected countries + canada
    country %in% c(
      #___
    ), 
    # keep only confirmed cases
    status == #___, 
    # keep only dates with 20 or more cases
    cases >= #___
  ) %>% 
  select(
    # drop the status column
    #___
  ) %>% 
  as_tsibble(index = date, key = country) %>% 
  fill_gaps() %>% 
  tidyr::fill(cases)

# use the autoplot function on dat_covid_ts
autoplot(# ___)


# CHECK OUT THE DIFFERENCES -----------------------------------------------

dat_covid_ts_diff <- dat_covid_ts %>% 
  group_by(country) %>% 
  mutate(
    diff_1 = difference(cases, lag = 1, differences = 1),
    diff_2 = difference(cases, lag = 1, differences = 2),
    log_diff_1 = difference(log(cases), lag = 1, differences = 1), 
    log_diff_2 = difference(log(cases), lag = 1, differences = 2)
  ) %>% 
  ungroup()

dat_covid_ts_diff %>% 
  ggplot(aes(date, diff_1, group = country, color = country)) + 
  geom_path()

dat_covid_ts_diff %>% 
  ggplot(aes(date, diff_2, group = country, color = country)) + 
  geom_path()

dat_covid_ts_diff %>% 
  ggplot(aes(date, log_diff_1, group = country, color = country)) + 
  geom_path() + 
  facet_wrap(~country)

# Plot log_diff_2 over time, facetted by country
dat_covid_ts_diff %>% 
  ggplot(
    aes(
      x = #___, 
      y = #___
    )
  ) + 
  geom_path() + 
  facet_wrap(
    #___
  )

# SEPARATE INTO TRAINING AND TEST DATA ------------------------------------

dat_covid_ts_train <- dat_covid_ts %>% 
  filter(
    # keep observations recorded more than a week ago
    date < #___
  )

dat_covid_ts_test <- dat_covid_ts %>% 
  filter(
    # Keep observations recorded a week ago or sooner
    date >= #___
  )



# TEST MODEL(s) -----------------------------------------------------------

dat_covid_fit <- dat_covid_ts_train %>% 
  model(
    # fit a var model to cases
    var = VAR(#___),
    
    # test additional models by uncommenting the following lines
    # ets = ETS(box_cox(cases, 0)), 
    # var_log = VAR(log(cases + 1)), 
    # nnet = NNETAR(box_cox(cases, 0)),
    # arima = ARIMA(log(cases))
  )

dat_covid_fit %>% 
  accuracy()

dat_covid_fc <- dat_covid_fit %>% 
  forecast(h = "1 week")

dat_covid_fc %>% 
  accuracy(dat_covid_ts)

dat_covid_fc %>% 
  autoplot(dat_covid_ts) + 
  scale_x_date(limits = c(as.Date("2020-03-01"), NA)) + 
  # scale_y_log10(labels = scales::comma) + 
  theme_minimal()


# PREDICT FUTURE CASES ----------------------------------------------------

# Where will the cases be in a week?
dat_covid_fc_final <- dat_covid_ts %>% 
  model(
    # fit a var model to cases
    # ___
  ) %>% 
  forecast(
    # forecast a week ahead
    # ___
  )


dat_covid_fc_final %>% 
  autoplot(
    # ___
  ) + 
  scale_x_date(limits = c(as.Date("2020-03-01"), NA)) + 
  theme_minimal()

dat_covid_fc_final %>% 
  filter(date == max(date))




