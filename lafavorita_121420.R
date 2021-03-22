# Fouad Yared, Stat 790: Case Seminar on Time Series
# Forecasting Future Sales at La Favorita, 12-14-20

library(TSA)
library(ggplot2)
library(tidyverse)
library(data.table)
library(scales)
library(fpp2)
library(arrow)
library(lubridate)
library(urca)
library(hts)
library(fable)
library(tsibble)
library(tsibbledata)
library(feasts)
library(forecast)

# helpful data.table material
# https://atrebas.github.io/post/2019-03-03-datatable-dplyr/

setwd("C:/Users/fouad/Documents/misc/fouad/hunter/stat 790 case seminar/favorita-grocery-sales-forecasting")

train <- fread("train.csv")
test <- fread("test.csv")

names(items)
items <- fread("items.csv")
transactions <- fread("transactions.csv")
stores <- fread("stores.csv")
unique(stores$store_nbr)

sample_submission <- fread("sample_submission.csv")
oil <- fread("oil.csv")
holiday_events <- fread("holidays_events.csv")

daysOpen_store1 <- fread("DatesStore1Opened_PerMonth.csv")

#### below: only store 1
# filter data.table
train_storeOne <-
  train[store_nbr == 1]

# quickly join data.table
train_storeOne_wItemFamily <-
  items[train_storeOne, on=c("item_nbr" = "item_nbr")]

#### below: stores 1 to 10
# filter data.table
train_storesOneToTen <-
  train[store_nbr %in% c(1,2,3,4,5,
                         6,7,8,9,10)]

# join ALL Stores to other data
train_StoresOneToTen_wItemFamily <-
  items[train_storesOneToTen, 
        on=c("item_nbr" = "item_nbr")]
train_StoresOneToTen_wItemFamily[,head(train_StoresOneToTen_wItemFamily)]
train_StoresOneToTen_wItemFamily[,sum(is.na(table(onpromotion)))]

#### below: stores 1 to 10
# filter data.table
train_storesOneToTwo <-
  train[store_nbr %in% c(1,2)]

# join ALL Stores to other data
train_StoresOneToTwo_wItemFamily <-
  items[train_storesOneToTwo, 
        on=c("item_nbr" = "item_nbr")]
train_StoresOneToTen_wItemFamily[,head(train_StoresOneToTen_wItemFamily)]
train_StoresOneToTen_wItemFamily[,sum(is.na(table(onpromotion)))]

# quick summary statistics
items[,head(items)]
test[,head(test)]
transactions[,head(transactions)]
stores[,head(stores)]
sample_submission[,head(sample_submission)]
oil[,head(oil)]
holiday_events[,head(holiday_events)]


train_storeOne_wItemFamily[,table(family)]
train_storeOne_wItemFamily[,table(store_nbr)]

########### plotting it

# used to gather days there were sales in store 1
transactions_x1 <-
  transactions %>% 
  filter(store_nbr=="1") %>% 
  mutate(month = month(date), 
         year = year(date), 
         quarter = quarter(date)) %>% 
  group_by(date) %>% 
  summarize(n=n())

# write.csv(transactions_x1, "transactions_x1.csv")

# aggregated by day
train_storeOne_wItemFamily_x0 <-
  train_storeOne_wItemFamily %>% 
  mutate(date = as.Date(date), 
         month = month(date), 
         year = year(date)) %>% 
  group_by(date, store_nbr) %>%
  summarize(unit_sales1 = sum(unit_sales))

# aggregated by day
train_storeOne_wItemFamily_x0A <-
  train_storeOne_wItemFamily %>% 
  mutate(date = as.Date(date), 
         month = month(date), 
         year = year(date)) %>% 
  group_by(date, store_nbr, family) %>%
  summarize(unit_sales1 = sum(unit_sales))

# aggregated by day, store, family
train_storeOne_wItemFamily_x0_family <-
  train_storeOne_wItemFamily %>% 
  mutate(date = as.Date(date), 
         month = month(date), 
         year = year(date)) %>% 
  group_by(date, store_nbr, family) %>%
  summarize(unit_sales1 = sum(unit_sales))

# aggregated by month, year
# View(train_storeOne_wItemFamily_x1)

# adding month-year component to look at residuals
train_storeOne_wItemFamily_x1 <-
  train_storeOne_wItemFamily %>% 
  mutate(date = as.Date(date), 
         month = month(date), 
         year = year(date)) %>% 
  group_by(month, year, store_nbr) %>%
  summarize(unit_sales1 = sum(unit_sales))

train_storeOne_wItemFamily_x1_Year <-
  train_storeOne_wItemFamily %>% 
  mutate(date = as.Date(date), 
         month = month(date), 
         year = year(date)) %>% 
  group_by(year, store_nbr) %>%
  summarize(unit_sales1 = sum(unit_sales))

ggplot(data = train_storeOne_wItemFamily_x1_Year, 
       aes(x=year, y=unit_sales1)) +
  geom_bar(stat="identity") +
  ggtitle("Number of Transactions in Store 1 in 2013") +
  scale_x_date(date_labels = "%b %d")

# monthly sales by year below (bar plot)
gridExtra::grid.arrange(a,b,c, nrow=1, ncol=3)

a<-ggplot(data = train_storeOne_wItemFamily_x1_Year, 
       aes(x=year, y=unit_sales1, 
           group = year, fill=as.factor(year))) +
  geom_bar(stat="identity") +
  geom_text(aes(label=scales::comma(unit_sales1)), vjust=-0.4) +
  ggtitle("Overall sales by year in Store 1") +
  theme(legend.position="bottom")

b<-ggplot(data = train_storeOne_wItemFamily_x1, 
       aes(x=month, y=unit_sales1, 
           group = year, fill=as.factor(year))) +
  geom_bar(stat="identity") +
  ggtitle("Overall sales by month for each year in Store 1") +
  theme(legend.position="bottom") +
  facet_wrap(~ year)+ 
  scale_y_continuous(labels = comma)

c<-ggplot(data = train_storeOne_wItemFamily_x1, 
          aes(x=year, y=unit_sales1, 
              group = year, fill=as.factor(year))) +
  geom_bar(stat="identity") +
  ggtitle("Comparing monthly sales by year in Store 1") +
  theme(legend.position="bottom") +
  facet_wrap(~ month)+ 
  scale_y_continuous(labels = comma)

################################
# ACF, PACF, EACF of original data
# this uses the daily sums 
par(mfrow=c(2,2))
acf(train_storeOne_wItemFamily_x0$unit_sales1, 
    main = "Autocorrelation of Total Unit Sales at 1st Store")
pacf(train_storeOne_wItemFamily_x0$unit_sales1, 
     main = "Partial Autocorrelation of Total Unit Sales at 1st Store")
eacf(train_storeOne_wItemFamily_x0$unit_sales1,
     ar.max = 7, 
     ma.max = 7)
# The Daily sales data shows a high weekly correlation

# ACF, PACF, EACF of difference of data (for non-stationary series)
acf(diff(train_storeOne_wItemFamily_x0$unit_sales1), 
    main = "First Difference of Autocorrelation of Total Unit Sales at 1st Store",
    xaxp=c(0,60, 30))
pacf(diff(train_storeOne_wItemFamily_x0$unit_sales1), 
     main = "First Difference of Partial Autocorrelation of Total Unit Sales at 1st Store",
     xaxp=c(0,60, 30))

# should look into whether Box-Ljung can be used on non-diff
# Box-Ljung test shows that the daily sales is a random number
Box.test(train_storeOne_wItemFamily_x0$unit_sales1, 
         lag=7, type="Ljung-Box")
# Box-Ljung test shows that the daily change in sales is a random number
Box.test(diff(train_storeOne_wItemFamily_x0$unit_sales1), lag=10, type="Ljung-Box")

# transforming variable with boxcox
BoxCox.ar(train_storeOne_wItemFamily$unit_sales)

trans_x0 <-
  BoxCox.ar(y=train_storeOne_wItemFamily_x0$unit_sales1)

trans_x0_value <- trans_x0$mle

y_x0 <- train_storeOne_wItemFamily_x0$unit_sales1

train_storeOne_wItemFamily_x0$y_x0_transformed <- 
  ((train_storeOne_wItemFamily_x0$unit_sales1^trans_x0_value-1)/trans_x0_value)

# using transformed variable in acf, etc
acf(train_storeOne_wItemFamily_x0$y_x0_transformed, 
    main = "Autocorrelation of Total Unit Sales at 1st Store")
pacf(train_storeOne_wItemFamily_x0$y_x0_transformed, 
     main = "Partial Autocorrelation of Total Unit Sales at 1st Store")
eacf(train_storeOne_wItemFamily_x0$y_x0_transformed,
     ar.max = 7, 
     ma.max = 7)

train_storeOne_wItemFamily_x0[,1]
train_x0 <- as.ts(train_storeOne_wItemFamily_x0$unit_sales1, 
                  start )
arima_test_x1 <-
  arima(train_x0, 
        order=c(1,0,0))
arima_test_x1
?arima
?tslm

library(seasonal)
ts_x1 %>% seas(x11="") -> fit
autoplot(fit) +
  ggtitle("Decomposition of Monthly Sales data, Overall")

fit %>% seasonal() %>% ggsubseriesplot() + ylab("Seasonal") + 
  ggtitle("Seasonal Sub-series plot Monthly Sales data, Overall")

stl(ts_x1)
?seas
?ets
autoplot(fc_x0) # too many levels in hierarchy
####
# Unit root test determines objectively whether 
## differencing is required.

# non-stationary (since test-stat >> 2.5pct)
train_storeOne_wItemFamily_x0$unit_sales1 %>% 
  ur.kpss() %>% 
  summary()

# stationary (since test-stat < 2.5pct)
train_storeOne_wItemFamily_x0$unit_sales1 %>% 
  diff %>% 
  ur.kpss() %>% 
  summary()

Box.test(train_storeOne_wItemFamily_x0$unit_sales1)
Box.test(diff(train_storeOne_wItemFamily_x0$unit_sales1),lag=7)

# number of differences appropriate
ndiffs(train_storeOne_wItemFamily_x0$unit_sales1)
# 1

# number of differences appropriate
nsdiffs(train_storeOne_wItemFamily_x0$unit_sales1)
# non-seasonal data

fit <- 
  auto.arima(train_storeOne_wItemFamily_x0$unit_sales1,
           seasonal = FALSE)
fit

fit_x2 <- 
  auto.arima(train_storeOne_wItemFamily_x0$unit_sales1,
             seasonal = FALSE,
             approximation = FALSE)
fit_x2

checkresiduals(fit)
autoplot(forecast(fit))

fit %>% 
  forecast(10) %>% 
  autoplot(include=80)

ggAcf(train_storeOne_wItemFamily_x0$unit_sales1)
ggPacf(train_storeOne_wItemFamily_x0$unit_sales1)
ggtsdisplay(train_storeOne_wItemFamily_x0$unit_sales1)

(fit2 <- 
    Arima(train_storeOne_wItemFamily_x0$unit_sales1, 
          order=c(1,0,0)))
fit2
fit

fit_000 <- 
  Arima(train_storeOne_wItemFamily_x0$unit_sales1, 
        order=c(0,0,0))
fit_000

fit_100 <- 
  Arima(train_storeOne_wItemFamily_x0$unit_sales1, 
        order=c(1,0,0))
fit_100


fit_200 <- 
    Arima(train_storeOne_wItemFamily_x0$unit_sales1, 
          order=c(2,0,0))
fit_200

fit_010 <- 
  Arima(train_storeOne_wItemFamily_x0$unit_sales1, 
        order=c(0,1,0))
fit_010

fit_001 <- 
  Arima(train_storeOne_wItemFamily_x0$unit_sales1, 
        order=c(0,0,1))
fit_001

fit_011 <- 
  Arima(train_storeOne_wItemFamily_x0$unit_sales1, 
        order=c(0,1,1))
fit_011

fit_110 <- 
  Arima(train_storeOne_wItemFamily_x0$unit_sales1, 
        order=c(1,1,0))
fit_110

fit_101 <- 
  Arima(train_storeOne_wItemFamily_x0$unit_sales1, 
        order=c(1,0,1))
fit_101

fit_111 <- 
  Arima(train_storeOne_wItemFamily_x0$unit_sales1, 
        order=c(1,1,1))
fit_111

fit_211 <- 
  Arima(train_storeOne_wItemFamily_x0$unit_sales1, 
        order=c(2,1,1))
fit_211

fit_201 <- 
  Arima(train_storeOne_wItemFamily_x0$unit_sales1, 
        order=c(2,0,1))
fit_201

fit_210 <- 
  Arima(train_storeOne_wItemFamily_x0$unit_sales1, 
        order=c(2,1,0))
fit_210

train_storeOne_wItemFamily_x0$unit_sales1 %>% 
  ets() %>% 
  forecast() %>% 
  autoplot()

fit.ets <- 
    ets(train_storeOne_wItemFamily_x0$unit_sales1 )
fit.ets

fit
# auto.arima() function is better than ets() function here

fit_x2 <- 
  auto.arima(train_storeOne_wItemFamily_x0$unit_sales1,
             seasonal = FALSE,
             approximation = FALSE)

a1 <- fit_x2 %>% 
  forecast(h=7) %>% 
  accuracy()

a2 <- fit.ets %>% 
  forecast(h=7) %>% 
  accuracy()

a1[,c("RMSE","MAE","MAPE","MASE")]
a2[,c("RMSE","MAE","MAPE","MASE")]

# hierarchical/disaggregated time series
# aka group time series (eg, separated for categories)
train_storeOne_wItemFamily_x0A

tourism.hts <- hts(visnights, characters = c(3, 5))
tourism.hts %>% aggts(levels=0:1) %>%
  autoplot(facet=TRUE) +
  xlab("Year") + ylab("millions") + 
  ggtitle("Visitor nights")

storeOne_wFamily <- hts(train_storeOne_wItemFamily_x0A, 
                   characters = c(1, 32))
?hts

storeOne_wFamily <- hts(train_storeOne_wItemFamily_x0A$store_nbr, 
                        train_storeOne_wItemFamily_x0A$family)

train_storeOne_wItemFamily_x0A$family
tourism.hts %>% aggts(levels=0:1) %>%
  autoplot(facet=TRUE) +
  xlab("Year") + ylab("millions") + 
  ggtitle("Visitor nights")

train_storeOne_wItemFamily[,table(family)]

prison.gts <- 
  gts(train_storeOne_wItemFamily_x0A/1e3, 
      characters = c(3,1,9),
      gnames = c("family"))
prison.gts <- 
  gts(prison/1e3, characters = c(3,1,9),
      gnames = c("State", "Gender", "Legal",
                 "State*Gender", "State*Legal",
                 "Gender*Legal"))

### fable package
train_storeOne_withFamily_TS <-
  as_tsibble(train_storeOne_wItemFamily_x0_family,
           index=date,
           key=family)

### Shows trend line for all families in store 1
train_storeOne_withFamily_TS %>%
  mutate(
    Day = lubridate::wday(date, label = TRUE),
    Weekend = (Day %in% c("Sun", "Sat"))
  ) %>%
  ggplot(aes(x = date, 
             y = unit_sales1)) +
  geom_line(aes(col = family)) +
  facet_wrap(family ~ .) +
  theme(legend.position = "bottom")

train_storeOne_withFamily_TS_FG <-
  tsibble::fill_gaps(.data = train_storeOne_withFamily_TS,
                   .full = TRUE)

train_storeOne_withFamily_TS_FG2 <-
  train_storeOne_withFamily_TS_FG %>% 
  mutate(store_nbr = replace_na(store_nbr,1),
         unit_sales1 = replace_na(unit_sales1,0))

names(train_storeOne_withFamily_TS_FG2)

ets_model <-
  train_storeOne_withFamily_TS_FG2 %>% 
  model(ETS(unit_sales1))

# fitted model
ets_model %>% fitted
# residual model
ets_model %>% residuals
# add variables from broom package
aug1 <- ets_model %>% augment
names(aug1)
tidy1 <- ets_model %>% tidy
glance1 <- ets_model %>% glance
names(glance1)

# extract components
comp1 <- 
  train_storeOne_withFamily_TS_FG2 %>% 
  model(ETS(unit_sales1)) %>% 
  components()
comp1
plot(comp1)

md_decomp <- 
  UKLungDeaths %>%
  STL(mdeaths ~ season(window = 12))
md_decomp

fore1 <-
  train_storeOne_withFamily_TS_FG2 %>% 
  model(ETS(unit_sales1)) %>% 
  forecast()

# # using a weekly sesonality for the data
# ts_x0 <- 
#   ts(train_storeOne_wItemFamily_x0$unit_sales1, 
#      start=c(2013,1), frequency =365.25)
# # need to aggregate by month to get correct values
# ts_x1 <- 
#   ts(train_storeOne_wItemFamily_x1$unit_sales_sum, 
#      start=c(2013, 1), frequency =12)
# ts_x0 <- 
#   ts(train_storeOne_wItemFamily_x0$unit_sales1, 
#      start=c(2013,1), frequency =365.25)

gglagplot(ts_x1)

ggAcf(train_storeOne_wItemFamily_x1$unit_sales_sum)
acf(train_storeOne_wItemFamily_x1$unit_sales_sum)
#### hierarchical
hier_train1 <- 
  train_storeOne_withFamily_TS_FG2 %>%
  aggregate_key(family, sales = sum(unit_sales1))

ncol(hier_train1)

hier_train1 %>%
  filter(is_aggregated(family)) %>%
  autoplot(sales) +
  ylab("Trips ('000)") +
  ggtitle("Australian tourism: national total and states") +
  facet_wrap(vars(family),scales = "free_y", nrow=c(2,1)) +
  theme(legend.position = "none")

tourism_hts %>%
  filter(is_aggregated(Region)) %>%
  autoplot(Trips) +
  ylab("Trips ('000)") +
  ggtitle("Australian tourism: national total and states") +
  facet_wrap(vars(State), scales = "free_y", ncol = `3`, nrow=2) +
  theme(legend.position = "none")

##### bottom-up forecasts
hier_train1 <- 
  train_storeOne_withFamily_TS_FG2 %>%
  aggregate_key(family, sales = sum(unit_sales1))

fcasts_family <- 
  hier_train1 %>%
  filter(!is_aggregated(family)) %>%
  model(ets = ETS(sales)) %>%
  forecast()

# Sum bottom-level forecasts to get top-level forecasts
fcasts_aggregatedFamily <- 
  fcasts_family %>%
  summarise(sales = sum(sales), 
            .mean = mean(sales))

fcasts_family_model <-
  fcasts_family %>%
  model(ets = ETS(sales)) %>%
  reconcile(bu = bottom_up(ets)) %>%
  # forecast()

forecast(x)
fcasts_family_model

### 11.4 forecasting Austrailian domestic tourism
hier_train1 <- 
  train_storeOne_withFamily_TS_FG2 %>%
  aggregate_key(family, sales = sum(unit_sales1))
train_storeOne_withFamily_TS_FG2$date

# set lambda=0 to get only positive values in forecast
# this has reconcile too..?


forecast(hts_obj2, 
         h=10, 
         method="wls",
         fmethod="arima")
?reconsile

fit_x3 <- 
  hier_train1 %>%
  model(arima = ARIMA(sales))

model(ets = ETS(train_storeOne_withFamily_TS$unit_sales1), 0.3) 
arima = ARIMA(log(family))
snaive = SNAIVE(family)

fit_x1B <- 
  hier_train1 %>%
  model(lm = ARIMA(sales ~ family))

sales_to2016 <- 
  hier_train1 %>%
  filter(year(date) <= 2016)

sales_2017 <- 
  hier_train1 %>%
  filter(year(date) == 2017)

fit_x1 <- 
  hier_train1 %>%
  filter(year(date) <= 2016) %>%
  model(
    nn_x1 = NNETAR(sales, lambda=0)
  ) %>%
  reconcile(
    bu_x6 = bottom_up(nn_x1),
    ols_x6 = min_trace(nn_x1, method = "ols"),
    mint_x6 = min_trace(nn_x1, method = "mint_shrink")
  )

fit_x1 <- 
  hier_train1 %>%
  filter(year(date) <= 2016) %>%
  model(
    # base = ETS(sales, lambda=0), 
    #     arima_x1 = ARIMA(sales, ic="aic", lambda=0),
    #     arima_x1B = ARIMA(sales~0+pdq(0,1,1)+PDQ(0,1,1), lambda=0),
    #     auto_regressive_x1 = AR(sales, ic="aic", lambda=0),
    #     croston_x1 = croston(sales, lambda=0),
    #     avg_x1 = MEAN(sales, lambda=0),
        nn_x1 = NNETAR(sales, lambda=0),
        # random_walk_x1 = RW(sales, lambda=0), 
        # TSLM(sales, lambda=0),
        # 
        # arima_x2 = ARIMA(diff(sales), ic="aic", lambda=0),
        # arima_x2B = ARIMA(diff(sales)~0+pdq(0,1,1)+PDQ(0,1,1), lambda=0),
        # auto_regressive_x2 = AR(diff(sales), ic="aic", lambda=0),
        # croston_x2 = croston(diff(sales), lambda=0),
        # avg_x2 = MEAN(diff(sales), lambda=0),
        # nn_x2 = NNETAR(diff(sales), lambda=0),
        # random_walk_x2 = RW(diff(sales), lambda=0), 
        # TSLM(diff(sales), lambda=0)
        # 
        ) %>%
  reconcile(
    # bu = bottom_up(base),
    # ols = min_trace(base, method = "ols"),
    # mint = min_trace(base, method = "mint_shrink"),
    # 
    # bu_x1 = bottom_up(arima_x1),
    # ols_x1 = min_trace(arima_x1, method = "ols"),
    # mint_x1 = min_trace(arima_x1, method = "mint_shrink"),
    # 
    # bu_x2 = bottom_up(arima_x1B),
    # ols_x2 = min_trace(arima_x1B, method = "ols"),
    # mint_x2 = min_trace(arima_x1B, method = "mint_shrink"),
    # 
    # bu_x3 = bottom_up(auto_regressive_x1),
    # ols_x3 = min_trace(auto_regressive_x1, method = "ols"),
    # mint_x3 = min_trace(auto_regressive_x1, method = "mint_shrink"),
    # 
    # bu_x4 = bottom_up(croston_x1),
    # ols_x4 = min_trace(croston_x1, method = "ols"),
    # mint_x4 = min_trace(croston_x1, method = "mint_shrink"),
    # 
    # bu_x5 = bottom_up(avg_x1),
    # ols_x5 = min_trace(avg_x1, method = "ols"),
    # mint_x5 = min_trace(avg_x1, method = "mint_shrink"),
    # 
    bu_x6 = bottom_up(nn_x1),
    ols_x6 = min_trace(nn_x1, method = "ols"),
    mint_x6 = min_trace(nn_x1, method = "mint_shrink")
    
    # bu_x7 = bottom_up(random_walk_x1),
    # ols_x7 = min_trace(random_walk_x1, method = "ols"),
    # mint_x7 = min_trace(random_walk_x1, method = "mint_shrink"),
    # 
    # bu_x8 = bottom_up(TSLM),
    # ols_x8 = min_trace(TSLM, method = "ols"),
    # mint_x8 = min_trace(TSLM, method = "mint_shrink"),
    # 
    # bu_x9 = bottom_up(arima_x2),
    # ols_x9 = min_trace(arima_x2, method = "ols"),
    # mint_x9 = min_trace(arima_x2, method = "mint_shrink"),
    # 
    # bu_x10 = bottom_up(arima_x2B),
    # ols_x10 = min_trace(arima_x2B, method = "ols"),
    # mint_x10 = min_trace(arima_x2B, method = "mint_shrink"),
    # 
    # bu_x11 = bottom_up(auto_regressive_x2),
    # ols_x11 = min_trace(auto_regressive_x2, method = "ols"),
    # mint_x11 = min_trace(auto_regressive_x2, method = "mint_shrink"),
    # 
    # bu_x12 = bottom_up(croston_x2),
    # ols_x12 = min_trace(croston_x2, method = "ols"),
    # mint_x12 = min_trace(croston_x2, method = "mint_shrink"),
    # 
    # bu_x13 = bottom_up(avg_x2),
    # ols_x13 = min_trace(avg_x2, method = "ols"),
    # mint_x13 = min_trace(avg_x2, method = "mint_shrink"),
    # 
    # bu_x14 = bottom_up(nn_x2),
    # ols_x14 = min_trace(nn_x2, method = "ols"),
    # mint_x14 = min_trace(nn_x2, method = "mint_shrink"),
    # 
    # bu_x15 = bottom_up(random_walk_x2),
    # ols_x15 = min_trace(random_walk_x2, method = "ols"),
    # mint_x15 = min_trace(random_walk_x2, method = "mint_shrink"),
    # 
    # bu_x16 = bottom_up(TSLM),
    # ols_x16 = min_trace(TSLM, method = "ols"),
    # mint_x16 = min_trace(TSLM, method = "mint_shrink")
    # 
  )


?fable::ARIMA
# finds the best ARIMA model. lowest aic, aicc, or bic
?fable::ETS 
# exponential smoothing. 
# ets() error, trend, seasonality of model
# model params chosen automatically
?bottom_up 
# totals aggregated values for TOTAL based on parts
?min_trace
# forecast the error variance of h-step ahead base forecasts
## params: ols (least weighted squares)
## params: mint_shrink (sample covariance matrix is shrunk)

# fitted values from model
fable::fitted(fit_x1)
forecast(fit_x1, new_data=sales_2017)

fc_x1 <- fit_x1 %>% forecast(h = "7 months")

# below: very good! figures are all positive
fc_x1 %>%
  filter(!is_aggregated(family)) %>%
  autoplot(
    hier_train1 %>% filter(year(date) >= 2013),
    level = NULL
  ) +
  ylab("Trips ('000)") +
  facet_wrap(vars(family), scales = "free_y")

fc_x1 %>%
  filter(is_aggregated(family)) %>%
  autoplot(
    hier_train1 %>% filter(year(date) >= 2013),
    level = NULL
  ) +
  ylab("Trips ('000)") +
  facet_wrap(vars(family), scales = "free_y")

fc_x1 %>%
  filter(!is_aggregated(family)) %>%
  accuracy(
    data = hier_train1,
    measures = list(rmse = RMSE, 
                    mase = MASE,
                    mape = MAPE,
                    mae = MAE)
  ) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), 
            mase = mean(mase),
            # mape = mean(mape, na.rm=T),
            mae = mean(mae)
            )

# arima models tend to perform worse than base model...
fc_x1 %>%
  filter(is_aggregated(family)) %>%
  accuracy(
    data = hier_train1,
    measures = list(rmse = RMSE, 
                    mase = MASE,
                    mape = MAPE,
                    mae = MAE)
  ) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), 
            mase = mean(mase),
            # mape = mean(mape, na.rm=T),
            mae = mean(mae)
  )

### could compare accuracy of total 
# calcuated using bottom-up (from categories)
# to overall total
# 
# fit <- hier_train1 %>%
#   filter(year(date) <= 2016) %>%
#   model(base = ETS(sales)) %>%
#   reconcile(
#     bottom_up = bottom_up(base))

# may want to show only bottom_up or MinT as they overlap
fit_x2 <- hier_train1 %>%
  filter(year(date) <= 2016) %>%
  model(base = ETS(sales)) %>%
  reconcile(
    bottom_up = bottom_up(base), 
    MinT = min_trace(base, method = "mint_shrink")
    )

# ,
#     MinT = min_trace(base, method = "mint_shrink")
#   )

fc_x2 <- fit_x2 %>% forecast(h = "7 months")

# overall plots confidence interval for next 7 months
fc_x2 %>%
  filter(
    is_aggregated(family)
  ) %>%
  autoplot(hier_train1, alpha = 0.7, level = 95) +
  ylab("Number of prisoners ('000)") +
  ggtitle("Australian prison population (total)")

# category plots confidence interval for next 7 months
fc_x2 %>%
  filter(
    !is_aggregated(family) & family=="GROCERY I"
  ) %>%
  autoplot(hier_train1, alpha = 0.7, level = 95) +
  ylab("Number of prisoners ('000)") +
  ggtitle("Australian prison population (total)")

#######################
# forecast accuracy across groups, post-reconcilation
# reconciled data has same accuracy as pre-recon
fc_x2 %>%
  filter(
    is_aggregated(family)
  ) %>%
  accuracy(data = hier_train1, measures = list(
    rmse = RMSE,
    mase = MASE,
    crps = CRPS,
    ss = skill_score(CRPS)
  )) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse),
            mase = mean(mase), 
            sspc = mean(ss) * 100)

##################
forecast(hier_train1, method="bu", fmethod="arima")




# haven't figured out how to plot forecast...

accuracy(f=fore1)
accuracy(f=fore1, measures = list(MASE))

# p <- ggplot(aes(x=date, y=as.numeric(unit_sales1)), 
#             data=fore1)
# p <- p + geom_line()
# p + geom_forecast(h=8)

ggplot(data= fore1, 
       aes(x=date, y=unit_sales1,
           group=family)) +
  geom_forecast(fore1)

train_storeOne_withFamily_TS %>%
  model(ets = ETS(unit_sales1), 0.3) 

model(ets = ETS(train_storeOne_withFamily_TS$unit_sales1), 0.3) 
    arima = ARIMA(log(family),
    snaive = SNAIVE(family)
  ) %>%
  forecast(h = "2 years") %>% 
  autoplot(filter(train_storeOne_wItemFamily_x0A, 
                  year(Month) > 2015), level = NULL)

aus_retail %>%
  filter(
    State %in% c("New South Wales", "Victoria"),
    Industry == "Department stores"
  ) %>% 
  model(
    ets = ETS(box_cox(Turnover, 0.3)),
    arima = ARIMA(log(Turnover)),
    snaive = SNAIVE(Turnover)
  ) %>%
  forecast(h = "2 years") %>% 
  autoplot(filter(aus_retail, year(Month) > 2010), level = NULL)

#######
y <- hts(vn, nodes=list(4,c(2,2,2,2)))
y <- hts(train_storeOne_wItemFamily_x0A, 
         nodes=list(1,4))

print(htseg1)
hts_obj <- hts(train_storeOne_wItemFamily_x0A)
print(hts(train_storeOne_wItemFamily_x0A))
summary(hts(train_storeOne_wItemFamily_x0A))
plot(htseg1, levels = 1)

aggts1 <- 
  aggts(hts_obj)
plot(hts_obj, levels=1)

train_storeOne_wItemFamily_x0A[,summary()]
train_storeOne_wItemFamily_x0A$

###
fitX <- 
  auto.arima(train_storeOne_wItemFamily_x0A$unit_sales1,
             seasonal = FALSE)
fitX

# hierarchical models
hts_obj2 <- hts(train_storeOne_wItemFamily_x0A)
print(hts_obj2)
summary(hts_obj2)
hts(hts_obj2, characters = c(1,4))
hts_obj3 <-
  hts(y=train_storeOne_wItemFamily_x0A, 
    bnames=colnames(train_storeOne_wItemFamily_x0A))
summary(hts_obj3)
# levels 0, 1

# hts_obj2 %>% aggts(levels=0:1) %>%
#   autoplot(facet=FALSE) +
#   xlab("Year") + ylab("millions") + 
#   ggtitle("Visitor nights")
# 
# plot(hts_obj2)
# 
# as_tibble(train_storeOne_wItemFamily_x0A) %>%
#   gather(family) %>%
#   mutate(Date = rep(time(train_storeOne_wItemFamily_x0A), 
#                     NCOL(train_storeOne_wItemFamily_x0A))) %>%
#   ggplot(aes(x=Date, y=value, group=family, 
#              colour=family)) +
#   geom_line() +
#   facet_grid(State~., scales="free_y") +
#   xlab("Year") + ylab("millions") +
#   ggtitle("Visitor nights by Zone") +
#   scale_colour_manual(values = cols)


forecast(hts_obj2, 
         h=10, 
         method="wls",
         fmethod="arima")

groupTS_1 <-
  gts(train_storeOne_wItemFamily_x0A)

?forecast
forecast(groupTS_1,fmethod="arima" )
forecast(prison.gts, method="bu", fmethod="arima")

get_groups(groupTS_1)

train_storeOne_wItemFamily_x0_family[,table(family)]

forecast(groupTS_1, 
         h=10, 
         method="comb",
         fmethod="arima")

tourism.hts <- 
  hts(train_storeOne_wItemFamily_x0_family, 
      characters = c(0, 1))

summary(hts_obj2)


#############################################
#############################################
# 
# fit_x1 <- 
#   hier_train1 %>%
#   filter(year(date) <= 2016) %>%
#   model(
#     nn_x1 = NNETAR(sales, lambda=0)
#   )%>%
#   reconcile(
#     bu = bottom_up(nn_x1),
#     ols = min_trace(nn_x1, method = "ols"),
#     mint = min_trace(nn_x1, method = "mint_shrink")
#   )
# 
# fc_x1 <- fit_x1 %>% forecast(h = "7 months")
# 
# acc_full_x1 <-
#   fc_x1 %>%
#   filter(is_aggregated(family)) %>%
#   accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
#   group_by(.model) %>%
#   summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)
# 
# acc_family_x1 <-
#   fc_x1 %>%
#   filter(!is_aggregated(family)) %>%
#   accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
#   group_by(.model) %>%
#   summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)

#####

fit_x0 <- 
  hier_train1 %>%
  filter(year(date) <= 2016) %>%
  model(
    base = ETS(sales)
  )%>%
  reconcile(
    bu = bottom_up(base),
    ols = min_trace(base, method = "ols"),
    mint = min_trace(base, method = "mint_shrink")
  )

fit_x2_Best <- 
  hier_train1 %>%
  filter(year(date) <= 2016) %>%
  model(
    arima_x1 = ARIMA(sales, ic="aic")
  )%>%
  reconcile(
    bu = bottom_up(arima_x1),
    ols = min_trace(arima_x1, method = "ols"),
    mint = min_trace(arima_x1, method = "mint_shrink")
  )

fit_x2A <- 
  hier_train1 %>%
  filter(year(date) <= 2016) %>%
  model(
    arima_x2 = ARIMA(sales ~ pdq(p=0:2, d=0:1, q=0:1), 
                     ic="aic")
  )%>%
  reconcile(
    bu = bottom_up(arima_x2),
    ols = min_trace(arima_x2, method = "ols"),
    mint = min_trace(arima_x2, method = "mint_shrink")
  )

fit_x2B <- 
  hier_train1 %>%
  filter(year(date) <= 2016) %>%
  model(
    arima_x1B = ARIMA(sales~0+pdq(0,0,1)+PDQ(0,0,0))
  )%>%
  reconcile(
    bu = bottom_up(arima_x1B),
    ols = min_trace(arima_x1B, method = "ols"),
    mint = min_trace(arima_x1B, method = "mint_shrink")
  )

fit_x2C <- 
  hier_train1 %>%
  filter(year(date) <= 2016) %>%
  model(
    arima_x1B = ARIMA(sales~0+pdq(0,1,0)+PDQ(0,0,0))
  )%>%
  reconcile(
    bu = bottom_up(arima_x1B),
    ols = min_trace(arima_x1B, method = "ols"),
    mint = min_trace(arima_x1B, method = "mint_shrink")
  )

fit_x2D <- 
  hier_train1 %>%
  filter(year(date) <= 2016) %>%
  model(
    arima_x1B = ARIMA(sales~0+pdq(1,0,0)+PDQ(0,0,0))
  )%>%
  reconcile(
    bu = bottom_up(arima_x1B),
    ols = min_trace(arima_x1B, method = "ols"),
    mint = min_trace(arima_x1B, method = "mint_shrink")
  )

fit_x2E <- 
  hier_train1 %>%
  filter(year(date) <= 2016) %>%
  model(
    arima_x1B = ARIMA(sales~0+pdq(1,1,0)+PDQ(0,0,0))
  )%>%
  reconcile(
    bu = bottom_up(arima_x1B),
    ols = min_trace(arima_x1B, method = "ols"),
    mint = min_trace(arima_x1B, method = "mint_shrink")
  )

fit_x2F <- 
  hier_train1 %>%
  filter(year(date) <= 2016) %>%
  model(
    arima_x1B = ARIMA(sales~0+pdq(1,0,1)+PDQ(0,0,0))
  )%>%
  reconcile(
    bu = bottom_up(arima_x1B),
    ols = min_trace(arima_x1B, method = "ols"),
    mint = min_trace(arima_x1B, method = "mint_shrink")
  )

fit_x2G <- 
  hier_train1 %>%
  filter(year(date) <= 2016) %>%
  model(
    arima_x1B = ARIMA(sales~0+pdq(0,1,1)+PDQ(0,0,0))
  )%>%
  reconcile(
    bu = bottom_up(arima_x1B),
    ols = min_trace(arima_x1B, method = "ols"),
    mint = min_trace(arima_x1B, method = "mint_shrink")
  )

fit_x2H <- 
  hier_train1 %>%
  filter(year(date) <= 2016) %>%
  model(
    arima_x1B = ARIMA(sales~0+pdq(1,1,1)+PDQ(0,0,0))
  )%>%
  reconcile(
    bu = bottom_up(arima_x1B),
    ols = min_trace(arima_x1B, method = "ols"),
    mint = min_trace(arima_x1B, method = "mint_shrink")
  )

fit_x2I <- 
  hier_train1 %>%
  filter(year(date) <= 2016) %>%
  model(
    arima_x1B = ARIMA(sales~0+pdq(2,0,0)+PDQ(0,0,0))
  )%>%
  reconcile(
    bu = bottom_up(arima_x1B),
    ols = min_trace(arima_x1B, method = "ols"),
    mint = min_trace(arima_x1B, method = "mint_shrink")
  )

fit_x2J <- 
  hier_train1 %>%
  filter(year(date) <= 2016) %>%
  model(
    arima_x1B = ARIMA(sales~0+pdq(2,0,1)+PDQ(0,0,0))
  )%>%
  reconcile(
    bu = bottom_up(arima_x1B),
    ols = min_trace(arima_x1B, method = "ols"),
    mint = min_trace(arima_x1B, method = "mint_shrink")
  )

fit_x2K <- 
  hier_train1 %>%
  filter(year(date) <= 2016) %>%
  model(
    arima_x1B = ARIMA(sales~0+pdq(2,1,0)+PDQ(0,0,0))
  )%>%
  reconcile(
    bu = bottom_up(arima_x1B),
    ols = min_trace(arima_x1B, method = "ols"),
    mint = min_trace(arima_x1B, method = "mint_shrink")
  )


fit_x2L <- 
  hier_train1 %>%
  filter(year(date) <= 2016) %>%
  model(
    arima_x1B = ARIMA(sales~0+pdq(2,1,1)+PDQ(0,0,0))
  )%>%
  reconcile(
    bu = bottom_up(arima_x1B),
    ols = min_trace(arima_x1B, method = "ols"),
    mint = min_trace(arima_x1B, method = "mint_shrink")
  )


fit_x3 <- 
  hier_train1 %>%
  filter(year(date) <= 2016) %>%
  model(
    auto_regressive_x1 = AR(sales, ic="aic", lambda=0)
  )%>%
  reconcile(
    bu = bottom_up(auto_regressive_x1),
    ols = min_trace(auto_regressive_x1, method = "ols"),
    mint = min_trace(auto_regressive_x1, method = "mint_shrink")
  )

fit_x4 <- 
  hier_train1 %>%
  filter(year(date) <= 2016) %>%
  model(
    croston_x1 = CROSTON(sales)) %>%
  reconcile(
    bu = bottom_up(croston_x1),
    ols = min_trace(croston_x1, method = "ols"),
    mint = min_trace(croston_x1, method = "mint_shrink")
  )

fit_x5 <- 
  hier_train1 %>%
  filter(year(date) <= 2016) %>%
  model(
    avg_x1 = MEAN(sales, lambda=0)
  )%>%
  reconcile(
    bu = bottom_up(avg_x1),
    ols = min_trace(avg_x1, method = "ols"),
    mint = min_trace(avg_x1, method = "mint_shrink")
  )

fit_x7 <- 
  hier_train1 %>%
  filter(year(date) <= 2016) %>%
  model(
    random_walk_x1 = RW(sales, lambda=0)
  )%>%
  reconcile(
    bu = bottom_up(random_walk_x1),
    ols = min_trace(random_walk_x1, method = "ols"),
    mint = min_trace(random_walk_x1, method = "mint_shrink")
  )
?RW
fit_x8 <- 
  hier_train1 %>%
  filter(year(date) <= 2016) %>%
  model(
    tslm_x1 = TSLM(sales)
  )%>%
  reconcile(
    bu = bottom_up(tslm_x1),
    ols = min_trace(tslm_x1, method = "ols"),
    mint = min_trace(tslm_x1, method = "mint_shrink")
  )


residuals(fit_x8, type=c("innovation","regression"))
a2 <- residuals(fit_x2_Best, type=c("innovation","regression"))
qqnorm(a2$.resid)
gghistogram(a2$.resid) + ggtitle("Histogram of residuals")
ggAcf(a2$.resid) + ggtitle("ACF of residuals")
Box.test(a2$.resid, lag=10, fitdf=0)
checkresiduals(fit_x8)
checkresiduals(a2$.resid)

?checkresiduals
a2 <- residuals(fit_x2_Best, type=c("innovation","regression"))
ggAcf(a2$.resid) + ggtitle("ACF of residuals")
checkresiduals(a2$.resid, main="ddd")

a2A <- residuals(fit_x2A, type=c("innovation","regression"))
ggAcf(a2A$.resid) + ggtitle("ACF of residuals")
checkresiduals(a2A$.resid, main="ddd")

a0 <- residuals(fit_x0, type=c("innovation","regression"))
ggAcf(a0$.resid) + ggtitle("ACF of residuals")
checkresiduals(a0$.resid, main="ddd")

# fit_x9_Best <- 
#   hier_train1 %>%
#   filter(year(date) <= 2016) %>%
#   model(
#     arima_x2 = ARIMA(diff(sales), 
#                      ic="aic")
#   )%>%
#   reconcile(
#     bu = bottom_up(arima_x2),
#     ols = min_trace(arima_x2, method = "ols"),
#     mint = min_trace(arima_x2, method = "mint_shrink")
#   )

# fit_x9A <- 
#   hier_train1 %>%
#   filter(year(date) <= 2016) %>%
#   model(
#     arima_x2 = ARIMA(diff(sales) ~ pdq(p=0:1, d=0:1, q=0:1), 
#                      ic="aic")
#   )%>%
#   reconcile(
#     bu = bottom_up(arima_x2),
#     ols = min_trace(arima_x2, method = "ols"),
#     mint = min_trace(arima_x2, method = "mint_shrink")
#   )

# fit_x9B <- 
#   hier_train1 %>%
#   filter(year(date) <= 2016) %>%
#   model(
#     arima_x1B = ARIMA(diff(sales)~0+pdq(0,0,1)+PDQ(0,0,0))
#   )%>%
#   reconcile(
#     bu = bottom_up(arima_x1B),
#     ols = min_trace(arima_x1B, method = "ols"),
#     mint = min_trace(arima_x1B, method = "mint_shrink")
#   )
# 
# fit_x9C <- 
#   hier_train1 %>%
#   filter(year(date) <= 2016) %>%
#   model(
#     arima_x1B = ARIMA(diff(sales)~0+pdq(0,1,0)+PDQ(0,0,0))
#   )%>%
#   reconcile(
#     bu = bottom_up(arima_x1B),
#     ols = min_trace(arima_x1B, method = "ols"),
#     mint = min_trace(arima_x1B, method = "mint_shrink")
#   )
# 
# fit_x9D <- 
#   hier_train1 %>%
#   filter(year(date) <= 2016) %>%
#   model(
#     arima_x1B = ARIMA(diff(sales)~0+pdq(1,0,0)+PDQ(0,0,0))
#   )%>%
#   reconcile(
#     bu = bottom_up(arima_x1B),
#     ols = min_trace(arima_x1B, method = "ols"),
#     mint = min_trace(arima_x1B, method = "mint_shrink")
#   )
# 
# fit_x9E <- 
#   hier_train1 %>%
#   filter(year(date) <= 2016) %>%
#   model(
#     arima_x1B = ARIMA(diff(sales)~0+pdq(1,1,0)+PDQ(0,0,0))
#   )%>%
#   reconcile(
#     bu = bottom_up(arima_x1B),
#     ols = min_trace(arima_x1B, method = "ols"),
#     mint = min_trace(arima_x1B, method = "mint_shrink")
#   )
# 
# fit_x9F <- 
#   hier_train1 %>%
#   filter(year(date) <= 2016) %>%
#   model(
#     arima_x1B = ARIMA(diff(sales)~0+pdq(1,0,1)+PDQ(0,0,0))
#   )%>%
#   reconcile(
#     bu = bottom_up(arima_x1B),
#     ols = min_trace(arima_x1B, method = "ols"),
#     mint = min_trace(arima_x1B, method = "mint_shrink")
#   )
# 
# fit_x9G <- 
#   hier_train1 %>%
#   filter(year(date) <= 2016) %>%
#   model(
#     arima_x1B = ARIMA(diff(sales)~0+pdq(0,1,1)+PDQ(0,0,0))
#   )%>%
#   reconcile(
#     bu = bottom_up(arima_x1B),
#     ols = min_trace(arima_x1B, method = "ols"),
#     mint = min_trace(arima_x1B, method = "mint_shrink")
#   )
# 
# fit_x9H <- 
#   hier_train1 %>%
#   filter(year(date) <= 2016) %>%
#   model(
#     arima_x1B = ARIMA(diff(sales)~0+pdq(1,1,1)+PDQ(0,0,0))
#   )%>%
#   reconcile(
#     bu = bottom_up(arima_x1B),
#     ols = min_trace(arima_x1B, method = "ols"),
#     mint = min_trace(arima_x1B, method = "mint_shrink")
#   )
# 
# fit_x9I <- 
#   hier_train1 %>%
#   filter(year(date) <= 2016) %>%
#   model(
#     arima_x1B = ARIMA(diff(sales)~0+pdq(2,0,0)+PDQ(0,0,0))
#   )%>%
#   reconcile(
#     bu = bottom_up(arima_x1B),
#     ols = min_trace(arima_x1B, method = "ols"),
#     mint = min_trace(arima_x1B, method = "mint_shrink")
#   )
# 
# fit_x9J <- 
#   hier_train1 %>%
#   filter(year(date) <= 2016) %>%
#   model(
#     arima_x1B = ARIMA(diff(sales)~0+pdq(2,0,1)+PDQ(0,0,0))
#   )%>%
#   reconcile(
#     bu = bottom_up(arima_x1B),
#     ols = min_trace(arima_x1B, method = "ols"),
#     mint = min_trace(arima_x1B, method = "mint_shrink")
#   )
# 
# fit_x9K <- 
#   hier_train1 %>%
#   filter(year(date) <= 2016) %>%
#   model(
#     arima_x1B = ARIMA(diff(sales)~0+pdq(2,1,0)+PDQ(0,0,0))
#   )%>%
#   reconcile(
#     bu = bottom_up(arima_x1B),
#     ols = min_trace(arima_x1B, method = "ols"),
#     mint = min_trace(arima_x1B, method = "mint_shrink")
#   )
# 
# 
# fit_x9L <- 
#   hier_train1 %>%
#   filter(year(date) <= 2016) %>%
#   model(
#     arima_x1B = ARIMA(diff(sales)~0+pdq(2,1,1)+PDQ(0,0,0))
#   )%>%
#   reconcile(
#     bu = bottom_up(arima_x1B),
#     ols = min_trace(arima_x1B, method = "ols"),
#     mint = min_trace(arima_x1B, method = "mint_shrink")
#   )

# fit_x10 <- 
#   hier_train1 %>%
#   filter(year(date) <= 2016) %>%
#   model(
#     auto_regressive_x2 = AR(diff(sales), ic="aic", lambda=0)
#   )%>%
#   reconcile(
#     bu = bottom_up(auto_regressive_x2),
#     ols = min_trace(auto_regressive_x2, method = "ols"),
#     mint = min_trace(auto_regressive_x2, method = "mint_shrink")
#   )

# fit_x11 <- 
#   hier_train1 %>%
#   filter(year(date) <= 2016) %>%
#   model(
#     croston_x2 = CROSTON(diff(sales))
#   )%>%
#   reconcile(
#     bu = bottom_up(croston_x2),
#     ols = min_trace(croston_x2, method = "ols"),
#     mint = min_trace(croston_x2, method = "mint_shrink")
#   )
# 
# fit_x12 <- 
#   hier_train1 %>%
#   filter(year(date) <= 2016) %>%
#   model(
#     avg_x2 = MEAN(diff(sales))
#   )%>%
#   reconcile(
#     bu = bottom_up(avg_x2),
#     ols = min_trace(avg_x2, method = "ols"),
#     mint = min_trace(avg_x2, method = "mint_shrink")
#   )
# 
# fit_x13 <- 
#   hier_train1 %>%
#   filter(year(date) <= 2016) %>%
#   model(
#     nn_x2 = NNETAR(diff(sales), lambda=0)
#   )%>%
#   reconcile(
#     bu = bottom_up(nn_x2),
#     ols = min_trace(nn_x2, method = "ols"),
#     mint = min_trace(nn_x2, method = "mint_shrink")
#   )

# fit_x14 <- 
#   hier_train1 %>%
#   filter(year(date) <= 2016) %>%
#   model(
#     random_walk_x2 = RW(diff(sales), lambda=0)
#   )%>%
#   reconcile(
#     bu = bottom_up(random_walk_x2),
#     ols = min_trace(random_walk_x2, method = "ols"),
#     mint = min_trace(random_walk_x2, method = "mint_shrink")
#   )

# fit_x15 <- 
#   hier_train1 %>%
#   filter(year(date) <= 2016) %>%
#   model(
#     tslm_x2 = TSLM(diff(sales))
#   )%>%
#   reconcile(
#     bu = bottom_up(tslm_x2),
#     ols = min_trace(tslm_x2, method = "ols"),
#     mint = min_trace(tslm_x2, method = "mint_shrink")
#   )

fc_x0 <- fit_x0 %>% forecast(h = "7 months")
fc_x2_Best <- fit_x2_Best %>% forecast(h = "7 months")
fc_x2A <- fit_x2A %>% forecast(h = "7 months")
?forecast
fc_x2B <- fit_x2B %>% forecast(h = "7 months")
fc_x2C <- fit_x2C %>% forecast(h = "7 months")
fc_x2D <- fit_x2D %>% forecast(h = "7 months")
fc_x2E <- fit_x2E %>% forecast(h = "7 months")
# fc_x2F <- fit_x2F %>% forecast(h = "7 months")
fc_x2G <- fit_x2G %>% forecast(h = "7 months")
fc_x2H <- fit_x2H %>% forecast(h = "7 months")
fc_x2I <- fit_x2I %>% forecast(h = "7 months")
# fc_x2J <- fit_x2J %>% forecast(h = "7 months")
fc_x2K <- fit_x2K %>% forecast(h = "7 months")
fc_x2L <- fit_x2L %>% forecast(h = "7 months")

autoplot(fc_x2A$sales)
plot(fit_x3)
fc_x3 <- fit_x3 %>% forecast(h = "7 months")
fc_x4 <- fit_x4 %>% forecast(h = "7 months")
fc_x5 <- fit_x5 %>% forecast(h = "7 months")
# need to add fc_x6
# fc_x6 <- fit_x6 %>% forecast(h = "7 months")
fc_x7 <- fit_x7 %>% forecast(h = "7 months")
fc_x8 <- fit_x8 %>% forecast(h = "7 months")

fc_x2A_ <- fc_x2A %>% hilo(level=c(80,95))
autoplot(hier_train1, level = 80, alpha = 0.5)
autoplot(fc_x2A, level = 95, alpha = 0.5)

# fc_x9A_Best <- fit_x2A %>% forecast(h = "7 months")
# fc_x9A <- fit_x2A %>% forecast(h = "7 months")
# fc_x9B <- fit_x9B %>% forecast(h = "7 months")
# fc_x9C <- fit_x9C %>% forecast(h = "7 months")
# fc_x9D <- fit_x9D %>% forecast(h = "7 months")
# fc_x9E <- fit_x9E %>% forecast(h = "7 months")
# fc_x9F <- fit_x9F %>% forecast(h = "7 months")
# fc_x9G <- fit_x9G %>% forecast(h = "7 months")
# fc_x9H <- fit_x9H %>% forecast(h = "7 months")
# fc_x9I <- fit_x9I %>% forecast(h = "7 months")
# fc_x9J <- fit_x9J %>% forecast(h = "7 months")
# fc_x9K <- fit_x9K %>% forecast(h = "7 months")
# fc_x9L <- fit_x9L %>% forecast(h = "7 months")

# fc_x10 <- fit_x10 %>% forecast(h = "7 months")
# fc_x11 <- fit_x11 %>% forecast(h = "7 months")
# fc_x12 <- fit_x12 %>% forecast(h = "7 months")
# fc_x13 <- fit_x13 %>% forecast(h = "7 months")
# fc_x14 <- fit_x14 %>% forecast(h = "7 months")
# fc_x15 <- fit_x15 %>% forecast(h = "7 months")

acc_full_x0 <-
  fc_x0 %>%
  filter(is_aggregated(family)) %>%
  accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)

acc_family_x0 <-
  fc_x0 %>%
  filter(!is_aggregated(family)) %>%
  accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)

acc_full_x2_Best <-
  fc_x2_Best %>%
  filter(is_aggregated(family)) %>%
  accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)

acc_family_x2_Best <-
  fc_x2_Best %>%
  filter(!is_aggregated(family)) %>%
  accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)

acc_full_x2A <-
  fc_x2A %>%
  filter(is_aggregated(family)) %>%
  accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)

acc_family_x2A <-
  fc_x2A %>%
  filter(!is_aggregated(family)) %>%
  accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)

acc_full_x2B <-
  fc_x2B %>%
  filter(is_aggregated(family)) %>%
  accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)

acc_family_x2B <-
  fc_x2B %>%
  filter(!is_aggregated(family)) %>%
  accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)

acc_full_x2C <-
  fc_x2C %>%
  filter(is_aggregated(family)) %>%
  accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)

acc_family_x2C <-
  fc_x2C %>%
  filter(!is_aggregated(family)) %>%
  accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)

acc_full_x2D <-
  fc_x2D %>%
  filter(is_aggregated(family)) %>%
  accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)

acc_family_x2D <-
  fc_x2D %>%
  filter(!is_aggregated(family)) %>%
  accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)

acc_full_x2E <-
  fc_x2E %>%
  filter(is_aggregated(family)) %>%
  accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)

acc_family_x2E <-
  fc_x2E %>%
  filter(!is_aggregated(family)) %>%
  accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)
# 
# acc_full_x2F <-
#   fc_x2B %>%
#   filter(is_aggregated(family)) %>%
#   accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
#   group_by(.model) %>%
#   summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)
# 
# acc_family_x2F <-
#   fc_x2B %>%
#   filter(!is_aggregated(family)) %>%
#   accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
#   group_by(.model) %>%
#   summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)

acc_full_x2G <-
  fc_x2G %>%
  filter(is_aggregated(family)) %>%
  accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)

acc_family_x2G <-
  fc_x2G %>%
  filter(!is_aggregated(family)) %>%
  accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)

acc_full_x2H <-
  fc_x2H %>%
  filter(is_aggregated(family)) %>%
  accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)

acc_family_x2H <-
  fc_x2H %>%
  filter(!is_aggregated(family)) %>%
  accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)

acc_full_x2I <-
  fc_x2I %>%
  filter(is_aggregated(family)) %>%
  accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)

acc_family_x2I <-
  fc_x2I %>%
  filter(!is_aggregated(family)) %>%
  accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)

# acc_full_x2J <-
#   fc_x2B %>%
#   filter(is_aggregated(family)) %>%
#   accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
#   group_by(.model) %>%
#   summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)
# 
# acc_family_x2J <-
#   fc_x2B %>%
#   filter(!is_aggregated(family)) %>%
#   accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
#   group_by(.model) %>%
#   summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)

acc_full_x2K <-
  fc_x2K %>%
  filter(is_aggregated(family)) %>%
  accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)

acc_family_x2K <-
  fc_x2K %>%
  filter(!is_aggregated(family)) %>%
  accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)

acc_full_x2L <-
  fc_x2L %>%
  filter(is_aggregated(family)) %>%
  accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)

acc_family_x2L <-
  fc_x2L %>%
  filter(!is_aggregated(family)) %>%
  accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)

acc_full_x3 <-
  fc_x3 %>%
  filter(is_aggregated(family)) %>%
  accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)

acc_family_x3 <-
  fc_x3 %>%
  filter(!is_aggregated(family)) %>%
  accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)

acc_full_x4 <-
  fc_x4 %>%
  filter(is_aggregated(family)) %>%
  accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)

acc_family_x4 <-
  fc_x4 %>%
  filter(!is_aggregated(family)) %>%
  accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)

acc_full_x5 <-
  fc_x5 %>%
  filter(is_aggregated(family)) %>%
  accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)

acc_family_x5 <-
  fc_x5 %>%
  filter(!is_aggregated(family)) %>%
  accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)

# acc_full_x6 <-
#   fc_x6 %>%
#   filter(is_aggregated(family)) %>%
#   accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
#   group_by(.model) %>%
#   summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)
# 
# acc_family_x6 <-
#   fc_x6 %>%
#   filter(!is_aggregated(family)) %>%
#   accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
#   group_by(.model) %>%
#   summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)

acc_full_x7 <-
  fc_x7 %>%
  filter(is_aggregated(family)) %>%
  accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)

acc_family_x7 <-
  fc_x7 %>%
  filter(!is_aggregated(family)) %>%
  accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)

acc_full_x8 <-
  fc_x8 %>%
  filter(is_aggregated(family)) %>%
  accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)

acc_family_x8 <-
  fc_x8 %>%
  filter(!is_aggregated(family)) %>%
  accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)

# acc_full_x9A <-
#   fc_x9A %>%
#   filter(is_aggregated(family)) %>%
#   accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
#   group_by(.model) %>%
#   summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)
# 
# acc_family_x9A <-
#   fc_x9A %>%
#   filter(!is_aggregated(family)) %>%
#   accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
#   group_by(.model) %>%
#   summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)

acc_full_x9B <-
  acc_family_x9B %>%
  filter(is_aggregated(family)) %>%
  accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)

acc_family_x9B <-
  acc_family_x9B %>%
  filter(!is_aggregated(family)) %>%
  accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)

acc_full_x9C <-
  acc_family_x9C %>%
  filter(is_aggregated(family)) %>%
  accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)

acc_family_x9C <-
  acc_family_x9C %>%
  filter(!is_aggregated(family)) %>%
  accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)

acc_full_x9D <-
  acc_family_x9D %>%
  filter(is_aggregated(family)) %>%
  accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)

acc_family_x9D <-
  acc_family_x9D %>%
  filter(!is_aggregated(family)) %>%
  accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)

acc_full_x9E <-
  acc_family_x9E %>%
  filter(is_aggregated(family)) %>%
  accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)

acc_family_x9E <-
  acc_family_x9E %>%
  filter(!is_aggregated(family)) %>%
  accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)

# acc_full_x9F <-
#   acc_family_x9B %>%
#   filter(is_aggregated(family)) %>%
#   accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
#   group_by(.model) %>%
#   summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)
# 
# acc_family_x9F <-
#   acc_family_x9B %>%
#   filter(!is_aggregated(family)) %>%
#   accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
#   group_by(.model) %>%
#   summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)

acc_full_x9G <-
  acc_family_x9G %>%
  filter(is_aggregated(family)) %>%
  accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)

acc_family_x9G <-
  acc_family_x9G %>%
  filter(!is_aggregated(family)) %>%
  accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)

acc_full_x9H <-
  acc_family_x9H %>%
  filter(is_aggregated(family)) %>%
  accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)

acc_family_x9H <-
  acc_family_x9H %>%
  filter(!is_aggregated(family)) %>%
  accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)

acc_full_x9I <-
  acc_family_x9I %>%
  filter(is_aggregated(family)) %>%
  accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)

acc_family_x9I <-
  acc_family_x9I %>%
  filter(!is_aggregated(family)) %>%
  accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)

# acc_full_x9J <-
#   acc_family_x9B %>%
#   filter(is_aggregated(family)) %>%
#   accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
#   group_by(.model) %>%
#   summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)
# 
# acc_family_x9J <-
#   acc_family_x9B %>%
#   filter(!is_aggregated(family)) %>%
#   accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
#   group_by(.model) %>%
#   summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)

acc_full_x9K <-
  acc_family_x9K %>%
  filter(is_aggregated(family)) %>%
  accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)

acc_family_x9K <-
  acc_family_x9K %>%
  filter(!is_aggregated(family)) %>%
  accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)

acc_full_x9L <-
  acc_family_x9L %>%
  filter(is_aggregated(family)) %>%
  accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)

acc_family_x9L <-
  acc_family_x9L %>%
  filter(!is_aggregated(family)) %>%
  accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)

# acc_full_x10 <-
#   fc_x10 %>%
#   filter(is_aggregated(family)) %>%
#   accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
#   group_by(.model) %>%
#   summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)

# acc_family_x10 <-
#   fc_x10 %>%
#   filter(!is_aggregated(family)) %>%
#   accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
#   group_by(.model) %>%
#   summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)
# 
# acc_full_x11 <-
#   fc_x11 %>%
#   filter(is_aggregated(family)) %>%
#   accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
#   group_by(.model) %>%
#   summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)
# 
# acc_family_x11 <-
#   fc_x11 %>%
#   filter(!is_aggregated(family)) %>%
#   accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
#   group_by(.model) %>%
#   summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)
# 
# acc_full_x12 <-
#   fc_x12 %>%
#   filter(is_aggregated(family)) %>%
#   accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
#   group_by(.model) %>%
#   summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)
# 
# acc_family_x12 <-
#   fc_x12 %>%
#   filter(!is_aggregated(family)) %>%
#   accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
#   group_by(.model) %>%
#   summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)
# 
# acc_full_x13 <-
#   fc_x13 %>%
#   filter(is_aggregated(family)) %>%
#   accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
#   group_by(.model) %>%
#   summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)
# 
# acc_family_x13 <-
#   fc_x13 %>%
#   filter(!is_aggregated(family)) %>%
#   accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
#   group_by(.model) %>%
#   summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)
# 
# acc_full_x14 <-
#   fc_x14 %>%
#   filter(is_aggregated(family)) %>%
#   accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
#   group_by(.model) %>%
#   summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)
# 
# acc_family_x14 <-
#   fc_x14 %>%
#   filter(!is_aggregated(family)) %>%
#   accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
#   group_by(.model) %>%
#   summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)
# 
# acc_full_x15 <-
#   fc_x15 %>%
#   filter(is_aggregated(family)) %>%
#   accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
#   group_by(.model) %>%
#   summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)
# 
# acc_family_x15 <-
#   fc_x15 %>%
#   filter(!is_aggregated(family)) %>%
#   accuracy(data = hier_train1, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
#   group_by(.model) %>%
#   summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)

acc_full_x0
acc_full_x2_Best
acc_full_x2A
acc_full_x2B
acc_full_x2C
acc_full_x2D
acc_full_x2E
# acc_full_x2F
acc_full_x2G
acc_full_x2H
acc_full_x2I
# acc_full_x2J
acc_full_x2K
acc_full_x2L
acc_full_x3
acc_full_x4
acc_full_x5
# acc_full_x6
acc_full_x7
acc_full_x8
# acc_full_x9B
# acc_full_x9C
# acc_full_x9D
# acc_full_x9E
# # acc_full_x9F
# acc_full_x9G
# acc_full_x9H
# acc_full_x9I
# # acc_full_x9J
# acc_full_x9K
# acc_full_x9L


acc_family_x0
acc_family_x2_Best
acc_family_x2A
acc_family_x2B
acc_family_x2C
acc_family_x2D
acc_family_x2E
# acc_family_x2F
acc_family_x2G
acc_family_x2H
acc_family_x2I
# acc_family_x2J
acc_family_x2K
acc_family_x2L
acc_family_x3
acc_family_x4
acc_family_x5
# acc_family_x6
acc_family_x7
acc_family_x8
# acc_family_x9B
# acc_family_x9C
# acc_family_x9D
# acc_family_x9E
# # acc_family_x9F
# acc_family_x9G
# acc_family_x9H
# acc_family_x9I
# # acc_family_x9J
# acc_family_x9K
# acc_family_x9L

residuals(acc_family_x8)

########################################## FINAL
# aggregated by day, store, family
memory.limit(size=500000)

train_storeOne_Agg <-
  aggregate(x=train_storeOne_wItemFamily$unit_sales,
          by=list(train_storeOne_wItemFamily$date,
                  train_storeOne_wItemFamily$family,
                  train_storeOne_wItemFamily$class,
                  train_storeOne_wItemFamily$store_nbr),
          FUN=sum)
is.data.table(train_storeOne_DT)

train_storeOneOverallSales <-
  aggregate(x=train_storeOne_wItemFamily$unit_sales,
                    by=list(train_storeOne_wItemFamily$family),
            FUN=sum)

train_storeOneOverallSales_wDate <-
  aggregate(x=train_storeOne_wItemFamily$unit_sales,
            by=list(train_storeOne_wItemFamily$date,
                    train_storeOne_wItemFamily$family),
            FUN=sum)

train_storeOneOverallSales_wQuarter <-
  aggregate(x=train_storeOne_wItemFamily$unit_sales,
            by=list(quarter(train_storeOne_wItemFamily$date),
                    train_storeOne_wItemFamily$family),
            FUN=sum)

train_storeOneOverallSales_wQuarter_DT <-
  as.data.table(train_storeOneOverallSales_wQuarter)

# train_storeOneOverallSales_wDate_DT2 <-
#   train_storeOneOverallSales_wQuarter_DT[,Group.1 := as.Date(Group.1)]

train_storeOneOverallSales$Group.1

train_storeOneOverallSales_wDate_DT2[,
                                     .(sumQ = sum(x)),
                                     by=list(train_storeOneOverallSales_wDate_DT2$Group2, 
                                             train_storeOneOverallSales_wDate_DT2$quarter(Group.1))]

ggplot(train_storeOneOverallSales_wQuarter_DT, 
       aes(x=reorder(Group.2, x), y=x)) +
geom_bar(stat="identity", fill="#FFA500") +
  geom_text(aes(label=scales::comma(x)), hjust=0.3) +
  coord_flip() +
  facet_wrap(Group.1~.)+
  ggtitle("Overall Product Sales by Category for each quarter, January 2013 to August 2017")

ggplot(train_storeOneOverallSales, 
       aes(x=reorder(Group.1, x), y=x)) +
  geom_bar(stat="identity", fill="#FFA500") +
  geom_text(aes(label=scales::comma(x)), hjust=0.3) +
  coord_flip() +
  ggtitle("Overall Product Sales by Category, January 2013 to August 2017")


a<-ggplot(data=telco_full_x3, aes(Churn_Label)) +
  geom_bar(fill="#87CEEB") +
  geom_text(stat="count", 
            aes(label=scales::comma(..count..)), 
            vjust=-0.5) +
  ggtitle("Customer Churn in Current Quarter")

train_storeOneOverallSales
train_storeOne_DT <-
  as.data.table(train_storeOne_Agg)

train_storeOne_DT2 <-
  train_storeOne_DT[, Group.1 := as.Date(Group.1)]

train_storeOne_DT2[,head(train_storeOne_DT2)]

str(train_storeOne_wItemFamily_classSum)
head(train_storeOne_wItemFamily_classSum)
train_storeOne_wItemFamily_classSum[,head(train_storeOne_wItemFamily_classSum)]


head(train_One_TS)
train_storeOne_wItemFamily[,head(train_storeOne_wItemFamily)]
train_storeOne_wItemFamily[,nrow(train_storeOne_wItemFamily)]

train_storeOne_wItemFamily[,nrow(table(family))] #32
train_storeOne_wItemFamily[,nrow(table(class))] #313
train_storeOne_wItemFamily[,nrow(table(item_nbr))] #3567

train_StoresOne_NEW[,nrow(train_StoresOne_NEW)]
train_storeOne_wItemFamily_classSum[,head(train_storeOne_wItemFamily_classSum)]
train_storeOne_wItemFamily_classSum

### fable package
train_One_TS <-
  as_tsibble(train_storeOne_DT2,
             index=Group.1,
             key=Group.3)

### Shows trend line for all families in store 1


train_One_TS %>%
  mutate(
    Day = lubridate::wday(Group.1, label = TRUE),
    Weekend = (Day %in% c("Sun", "Sat"))
  ) %>%
  ggplot(aes(x = Group.1, 
             y = x)) +
  geom_line(aes(col = Group.2)) +
  facet_wrap(Group.2 ~ .) +
  theme(legend.position = "bottom") +
  ggtitle("Daily sales for each product family at La Favorita")

### Shows trend line for all class items in GROCERY I in store 1
train_One_TS %>%
  mutate(
    Day = lubridate::wday(Group.1, label = TRUE),
    Weekend = (Day %in% c("Sun", "Sat"))
  ) %>%
  filter(Group.2=="GROCERY I") %>% 
  ggplot(aes(x = Group.1, 
             y = x)) +
  geom_line(aes(col = Group.3)) +
  facet_wrap(Group.3 ~ .) +
  theme(legend.position = "bottom") +
  ggtitle("Daily sales for each product class in the Grocery I family")

train_One_TS_fillGaps <-
  tsibble::fill_gaps(.data = train_One_TS,
                     .full = TRUE)

train_One_TS_fillGaps2 <-
  train_One_TS_fillGaps %>% 
  mutate(Group.4 = replace_na(Group.4,1),
         x = replace_na(x,0))

##### bottom-up forecasts
hier_train2 <- 
  train_One_TS_fillGaps2 %>%
  aggregate_key(Group.3, x = sum(x))

table(hier_train2$x==0)
sum(is.na(hier_train2$x))

fit_x0 <- 
  train_One_TS_fillGaps2 %>%
  filter(year(Group.1) <= 2016) %>%
  model(
    base = ETS(x)
  )%>%
  reconcile(
    bu = bottom_up(base),
    ols = min_trace(base, method = "ols"),
    mint = min_trace(base, method = "mint_shrink")
  )

fc_x0 <- fit_x0 %>% forecast(h = "7 months")

acc_full_x0 <-
  fc_x0 %>%
  filter(is_aggregated(Group.2)) %>%
  accuracy(data = hier_train2, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)

acc_family_x0 <-
  fc_x0 %>%
  filter(!is_aggregated(Group.2)) %>%
  accuracy(data = hier_train2, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)

acc_class_x0 <-
  fc_x0 %>%
  filter(!is_aggregated(Group.3)) %>%
  accuracy(data = hier_train2, measures = list(rmse = RMSE, mase = MASE, mae = MAE, crps = CRPS, ss = skill_score(CRPS))) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase), mae = mean(mae), sspc = mean(ss) * 100)
