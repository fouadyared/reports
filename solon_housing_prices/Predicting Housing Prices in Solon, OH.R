# Fouad Yared, Multivariate Final Project, Fall 2020

library(data.table)
library(tidyverse)
library(glmnet)
library(randomForest)
library(caret)
library(car)
library(rpart)
library(e1071)
library(pls)
library(ISLR)
library(leaps)
library(outliers)
library(cluster)
library(factoextra)
library(mclust)

rm(list=ls()) # remove all variables

setwd("C:/Users/fouad/Documents/misc/fouad/hunter/stat 717 multivariate analysis/project/cuy_home_prices")

solon <- fread("SolonOH_Home_Sales_Jan2016toNov2020_CSV.csv")

# count of NA values in specific columns
solon %>%
  summarise_all(funs(sum(is.na(.))))

solon_x1 <-
  solon %>% 
  # vars with lots of NAs
  select(-c(Living_Area_Upper, 
            Floor_Location,
            hasNoProperty, 
  # vars that appear as part of Living_Area_Total
            Living_Area_1, 
            Living_Area_2, 
            Living_Area_Basement, # has lots of NAs too
            Living_Area_Upper,
  # 90% of values for these vars are the same
            Dupe, 
            CityAscending,
            Apt, 
            Attic_Type,
            Occupancy, 
            Heat_Type, 
            Roof_Material, 
            Last_Transfer_Amnt, 
            Last_Transfer_DT,
            Party_Wall,
            # Garage_Type, 
            Basement_Type,
  # Full Address already appears
            House_No,
            Addr_HouseNo_StreetName,
            Street_type,
            Parcel_ID,
            Building_Record_Number,
            Zip,
            Parcels
  ))


# monthly sales by year below (bar plot)
solon_A1 <-solon %>% 
  group_by(YEAR) %>% 
  summarize(Price=mean(Price),
            n=n())

solon_A2 <-solon %>% 
  group_by(YEAR, Bedrooms) %>% 
  summarize(Price=mean(Price),
            n=n())

solon_A3 <-solon %>% 
  group_by(Street_Name) %>% 
  summarize(Price=mean(Price),
            n=n())


solon_A4 <- solon_living %>% 
  group_by(YEAR, Living_Area_Total_Group) %>% 
  summarize(Price=mean(Price),
            n=n())

f1 <- quantile(solon$Price,probs = c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1))
f2 <- quantile(solon$Living_Area_Total,
         probs = c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1))

home_prices <- 
  as.data.frame(rbind(f1, f2)) %>% 
  t()

home_prices_DF <- as.data.frame(home_prices)

home_prices_DF_x2 <-
  home_prices_DF %>% 
  select(Price=f1, Living_Area_Total=f2)

par(mfrow=c(1,2))
plot(home_prices_DF_x2$Price, main="Sale prices, split by 10% decile")
plot(home_prices_DF_x2$Living_Area_Total, main="Total Living Area, split by 10% decile")

solon_living <-
  solon %>% 
  mutate(Living_Area_Total_Group=case_when(
    Living_Area_Total <= 1500 ~ "1) Less than 1500 SQ FT",
    Living_Area_Total >= 1501 & Living_Area_Total <= 3000 ~ "2) Between 1501-3000 SQ FT",
    Living_Area_Total >= 3001 & Living_Area_Total <= 4500 ~ "3) Between 3001-4500 SQ FT",
    Living_Area_Total >= 4501 & Living_Area_Total <= 6000 ~ "4) Between 4501-6000 SQ FT",
    Living_Area_Total >= 6001 ~ "5) More than 6001 SQ FT"
  ))

# Average home price by year
a <- ggplot(data = solon_A1, 
          aes(x=YEAR, y=Price, 
              group = YEAR, fill=as.factor(YEAR))) +
  geom_bar(stat="identity") +
  geom_text(aes(label=scales::comma(Price)), vjust=-0.4) +
  geom_text(aes(label=scales::comma(n)), vjust=1.5) +
  ggtitle("Average home price by year") +
  theme(legend.position="bottom")

# Average home price by year, by number of bedrooms
b<-ggplot(data = solon_A2, 
       aes(x=Bedrooms, y=Price, 
           group = Bedrooms, fill=as.factor(Bedrooms))) +
  geom_bar(stat="identity") +
  ggtitle("Average home price by year, by number of bedrooms") +
  geom_text(aes(label=scales::comma(Price)), vjust=1) +
  geom_text(aes(label=scales::comma(n)), vjust=2.5) +
  theme(legend.position="bottom") +
  facet_wrap(~ YEAR)+ 
  scale_y_continuous(labels = comma)

# Comparing price of homes with same number of bedrooms, by year
c<-ggplot(data = solon_A2,
          aes(x=YEAR, y=Price,
              group = YEAR, fill=as.factor(YEAR))) +
  geom_bar(stat="identity") +
  ggtitle("Comparing price of homes with same number of bedrooms, by year") +
  geom_text(aes(label=scales::comma(Price)), vjust=0.4) +
  geom_text(aes(label=scales::comma(n)), vjust=2.5) +
  theme(legend.position="bottom") +
  facet_wrap(~ Bedrooms)+
  scale_y_continuous(labels = comma)

# Average home price by year, by Living Area
d<-ggplot(data = solon_A4, 
       aes(x=Living_Area_Total_Group, y=Price, 
           group = Living_Area_Total_Group, fill=as.factor(Living_Area_Total_Group))) +
  geom_bar(stat="identity") +
  ggtitle("Average home price by year, by Living Area") +
  geom_text(aes(label=scales::comma(Price)), vjust=0.2) +
  geom_text(aes(label=scales::comma(n)), vjust=2.5) +
  theme(legend.position="bottom") +
  facet_wrap(~ YEAR)+ 
  scale_y_continuous(labels = comma)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

grid.arrange(a,b,c,d,nrow=2,ncol=2)

# find variables where 95% of the values are in one category
solon_isChar <-
  solon %>%
  select_if(is.character)

freq_table <- function(i) {
  prop_table <- prop.table(table(solon[[i]]))
  prop_table_df <- as.data.frame(prop_table)
  
  prop_table_df_x2 <- 
    prop_table_df  %>%
    rename(level=Var1, freq=Freq) %>%
    mutate(var=as.character(i)) %>%
    filter(freq >= 0.85)
}

df <- colnames(solon_isChar) %>% 
  map_df(~freq_table(.)) %>%
  dplyr::select(var, level, freq) %>%
  arrange(desc(freq), var, level)

# count of NA values in specific columns
solon_x2 %>%
  summarise_all(funs(sum(is.na(.))))

# regroup variables
solon_x2 <-
  solon_x1 %>% 
  mutate(Garage_Type2 = 
           case_when(
             Garage_Type %in% 
               c("BUILT-IN", "ATTACHED") ~ "Attached_or_BuiltIn",
             !(Garage_Type %in% 
                 c("BUILT-IN", "ATTACHED")) ~ Garage_Type),
         Construction_Quality2 = 
           case_when(
             Construction_Quality %in% 
               c("A+ / EXCELLENT","AA / EXCELLENT+") ~ "A+ / EXCELLENT or better",
             !(Construction_Quality %in% 
                 c("A+ / EXCELLENT","AA / EXCELLENT+")) ~ Construction_Quality),
         Style2 = 
           case_when(
             Style %in% 
               c("BUNGALOW","RANCH") ~ "RANCH OR BUNGALOW",
             Style %in% 
               c("BI-LEVEL","SPLIT-LEVEL") ~ "SPLIT OR BILEVEL",
             !(Style %in% 
                 c("BUNGALOW","RANCH", 
                   "BI-LEVEL","SPLIT-LEVEL")) ~ Style),
         RoofType2 = 
           case_when(
             Roof_Type %in% 
               c("","GAMBREL", "GABLE") ~ "GABLE or Unknown",
             !(Roof_Type %in% 
                 c("","GABLE", "GABLE")) ~ Roof_Type), 
         Exterior_Walls2 = 
           case_when(
             Exterior_Walls %in% 
               c("BRICK/STUCCO","BRICK", "STUCCO",
                 "AV/BRICK", "STONE") ~ "Brick or other",
             Exterior_Walls %in% 
               c("ALUM/VINYL","COMP-SIDING") ~ "Vinyl or Comp-siding",
             !(Exterior_Walls %in% 
                 c("BRICK/STUCCO","BRICK", "STUCCO",
                   "AV/BRICK", "STONE",
                   "ALUM/VINYL","COMP-SIDING")) ~ Exterior_Walls),
         Air_Conditioning2 = 
           case_when(
             Air_Conditioning %in% 
               c("NONE","THRU-WALL") ~ "None or ThruWall",
             !(Air_Conditioning %in% 
                 c("NONE","THRU-WALL")) ~ Air_Conditioning),
         Story_Height2 = 
           case_when(
             Story_Height %in% 
               c("1","1.5","1.75") ~ "Between 1 to 2 floors",
             (Story_Height %in% 
                 c("2","2.5","2.75")) ~ "Two or more floors"),
         Condition2 = 
           case_when(
             Condition %in% 
               c("EXCELLENT","VERY-GOOD") ~ "Above Average",
             (Condition %in% 
                 c("GOOD","FAIR", "AVERAGE")) ~ "Average")
         ) %>% 
  select(-Garage_Type, -Construction_Quality, 
         -Style, -Roof_Type, -Exterior_Walls, -Street_Name,
         -Air_Conditioning,-Story_Height,-Condition)

solon_x3 <-
  solon_x2 %>% 
  mutate(Year_Garage_Built2 = replace_na(Year_Garage_Built, replace=10),
         Garage_Size2 = replace_na(Garage_Size, replace=0),
         Garage_Capacity2 = replace_na(Garage_Capacity, replace=0),
         Basement_Square_Feet2 = replace_na(Basement_Square_Feet, replace=0)) %>% 
  mutate(Year_Garage_Built_Decade = floor(Year_Garage_Built2/10)*10,
         Year_Built_Decade = floor(Year_Built/10)*10) %>%
  mutate(GarageYrBlt_decade = factor(Year_Garage_Built_Decade,
                                     levels=c("10",
                                              "1920","1930","1940",
                                              "1950","1960","1970",
                                              "1980","1990","2000",
                                              "2010"),
                                     ordered=TRUE)) %>% 
  mutate(GarageYrBlt_decade2 = recode_factor(GarageYrBlt_decade,
                                            `10` ="no garage",
                                            `1920` = "1920-1940",
                                            `1930` ="1920-1940",
                                            `1940` ="1920-1940",
                                            `1950` = "1950",
                                            `1960` = "1960",
                                            `1970` = "1970",
                                            `1980` = "1980",                                          
                                            `1990` ="1990",
                                            `2000` ="2000-2010",
                                            `2010` = "2000-2010")) %>% 
  mutate(Year_Built_Decade2 = recode_factor(Year_Built_Decade,
                                            `1850` ="1850-1940",
                                            `1870` ="1850-1940",
                                            `1880` = "1850-1940",
                                            `1900` = "1850-1940",
                                            `1910` = "1850-1940",
                                            `1920` = "1850-1940",
                                            `1930` ="1850-1940",
                                            `1940` ="1850-1940",
                                            `1950` = "1950",
                                            `1960` = "1960",
                                            `1970` = "1970",
                                            `1980` = "1980",                                          
                                            `1990` ="1990",
                                            `2000` ="2000-2010",
                                            `2010` = "2000-2010")) %>% 
  mutate(Total_Bathrooms = Bathrooms + (0.5*Half_Baths)) %>% 
  select(-c(Year_Garage_Built, Year_Garage_Built2, 
            Year_Garage_Built_Decade, Garage_Size, 
            Basement_Square_Feet, Garage_Capacity, Year_Built_Decade,
            Half_Baths, Bathrooms, Year_Built, GarageYrBlt_decade)) %>% 
  mutate(Date2 = as.Date(Date, format='%m/%d/%Y'),
         Month = month(Date2),
         QTR2 = as.character(QTY),
         YEAR2 = factor(YEAR, ordered=TRUE),
         Month2 = factor(Month, ordered=TRUE)) %>% 
  select(-Date, -Date2, -QTY,-QTR2, -Month, -YEAR) %>% 
  filter(!Price %in% c(850000,999500,1130000,1445000)) %>% # removed outliers
  filter(!Style2=="FOUR-PLEX") # removed rare observations

solon_x3B <-
  solon_x3 %>% 
  select_if(is.numeric)

#### Univariate outliers: looking at Zscores, Tstudentized values
Zscores_df <-
  solon_x3B %>% 
  mutate(
    Price_Zscore = outliers::scores(Price, type="z"),
    Rooms_Zscore = outliers::scores(Rooms, type="z"),
    Living_Area_Total_Zscore = outliers::scores(Living_Area_Total, type="z"),
    Bedrooms_Zscore = outliers::scores(Bedrooms, type="z"),
    Garage_Size2_Zscore = outliers::scores(Garage_Size2, type="z"),
    Garage_Capacity2_Zscore = outliers::scores(Garage_Capacity2, type="z"),
    Basement_Square_Feet2_Zscore = outliers::scores(Basement_Square_Feet2, type="z"),
    Total_Bathrooms_Zscore = outliers::scores(Total_Bathrooms, type="z")
    )
View(Zscores_df)

table(solon_x3$Price==999500)
#price 1445000 has zscore of 6.8 for price
#price 999500 has zscore of 6.4 for living area, 4.5 for basement SQ FT
#price 850000 has zscore of 5.8 for living area also garagesize2 is 5.9, garageCapacity 4.8

#### Multivariate outliers: Mahalanobis D 
mean <- colMeans(solon_x3B)
cov_mat <- cov(solon_x3B)
d2 <- mahalanobis(solon_x3B,mean,cov_mat,tol=1e-40)

solon_x3B_mahal <- 
  solon_x3B %>% 
  mutate(d2 = d2,
         d2_Tstud = scores(d2, type="t")) %>% 
  dplyr::select(Price, d2,d2_Tstud) %>% 
  filter(d2_Tstud <=5.99) %>% 
  arrange(desc(d2_Tstud))

#850k home has mahal of 5.68. others are below 5

#### High leverage points: looking at Cook's distance, hat values
lm_2 <- lm(data=solon_x3, formula = Price~.)
summary(lm_2)

summary(influence.measures(lm_2))
influential_measures<-
  as.data.frame(summary(influence.measures(lm_2)))

#####################
# Manually add index to csv file (not working in R)
write.csv(influential_measures,
          "influential_measuresX1.csv")

influential_X2 <-
  as.data.frame(as.matrix(influential_measures)) %>% 
  mutate(Id = row.names(influential_measures)) %>% 
  select(Id,cook.d,hat)

influential_X2

influ2 <- read.csv("influential_measuresX1.csv")

influ2X <-
  influ2 %>% 
  filter(cook.d > (4/nrow(solon_x3B)) |
           hat > 2*((206)/nrow(solon_x3B))) %>% 
  dplyr::select(Id)
influ2X

# Observations with cook's distance above threshold
cook_dist <-
  influ2 %>% 
  filter(cook.d > (4/nrow(solon_x3B))) %>% 
  dplyr::select(Id)
cook_dist
# 35

# Observations with hat values above threshold
hat_items <-
  influ2 %>% 
  filter(hat > 2*((206)/nrow(solon_x3B))) %>% 
  dplyr::select(Id)
hat_items
# 23

# Observations with both cook's distance and hat values above threshold
both_cook_hat <-
  influ2 %>% 
  filter(cook.d > (4/nrow(solon_x3B)) &
           (hat > 2*((206)/nrow(solon_x3B)))) %>% 
  dplyr::select(Id)
both_cook_hat
# 8

solon_x3 %>% 
  mutate(id = rowid_to_column())

# count of NA values in specific columns
solon_x3 %>%
  summarise_all(funs(sum(is.na(.))))

# Histogram of housing prices (before removing outliers)
g1 <- ggplot(data=solon, aes(Price)) +
  geom_histogram(fill="#013220") +
  ggtitle("Housing Prices in Solon, OH from 2016-2020 (before removing outliers)")+
  scale_x_continuous(labels = comma)

# Histogram of housing prices (after removing outliers)
g2 <- ggplot(data=solon_x3, aes(Price)) +
  geom_histogram(fill="#013220") +
  ggtitle("Housing Prices in Solon, OH from 2016-2020 (after removing outliers)")+
  scale_x_continuous(labels = comma)

grid.arrange(g1,g2,nrow=1,ncol=2)


##################################
############################################
### Model Matrices for Lasso, Ridge ########
############################################

# create 60/40 cross-validation set

set.seed(2020)
# 
# samples=sample(nrow(solon_x3), 
#                0.6*nrow(solon_x3))
# train = solon_x3[samples, ]
# test = solon_x3[-samples,]
# ytest = solon_x3$Price

ncol(solon_x3)
# split across stata
train_idx <-
  createDataPartition(solon_x3$Price, 
                      times = 1, 
                      p = 0.75, list=F)
train_x2 = solon_x3[train_idx, ]
test_x2 = solon_x3[-train_idx,]

# producing model matrices for ridge,lasso models
x_matrix <- model.matrix(Price~.,
                         solon_x3)
names(solon_x3)
lapply(solon_x3, table)

# x_matrix
y <- solon_x3$Price
ytrain1 = y[train_idx]
ytest1 = y[-train_idx]

lambda <- 10^seq(10,-2,length=100)  

names(solon_x3_PL_wC)
solon_x3_PL_wC <-
  solon_x3_PL %>% 
  mutate(mclust = drmod$classification,
         mclust = as.character(mclust))

solon_x3_PL <- solon_x3_PL_wC
############################################
############# Lasso Regression #############
############################################

# set alpha=0 for ridge regression, alpha=1 for lasso regression
lasso_mod <- glmnet(x_matrix[train_idx,],
                    y[train_idx],alpha=1,
                    lambda=lambda,
                    scale=TRUE)

# As the lambda value increases, the coefficients get closer to zero.
predict(lasso_mod, s=.1, type='coefficients')
predict(lasso_mod, s=1, type='coefficients')
predict(lasso_mod, s=10, type='coefficients')
predict(lasso_mod, s=100, type='coefficients')

set.seed(2020)
cv_lasso_mod <- 
  cv.glmnet(x_matrix[train_idx,],y[train_idx],alpha=1,
            scale=TRUE, nfolds=10)
cv_lasso_mod # shows lambda, other values
bestlam_lasso <- cv_lasso_mod$lambda.min
bestlam_lasso # best lambda is  1638.745

plot(cv_lasso_mod,main="Lasso Regression")
r_squared_lasso=1-cv_lasso_mod$cvm/var(y[train_idx])

r_squared_lasso_max <- 
  max(1-cv_lasso_mod$cvm/var(y[train_idx]))
r_squared_lasso_max # 76.02%

plot(cv_lasso_mod$lambda,r_squared_lasso, 
     main=expression(R^{2}~"values for different lambda parameters in Lasso Regression Model"))

plot(lasso_mod,xvar="lambda",
     main="
     Coefficients of predictors in Lasso Regression model
     
     ")

plot(lasso_mod, main="Lasso Regression,
     Relationship of L1 Norm and Coefficients")

lasso_modBest <- glmnet(x_matrix[train_idx,],
                        y[train_idx],
                        alpha=1,
                        lambda=bestlam_lasso,
                        scale=TRUE)

lasso_mod_coefs_Best <-
  predict(lasso_modBest,
          type="coefficients",
          s=bestlam_lasso,
          scale=TRUE)

lasso_pred_Best <- 
  predict(lasso_modBest,
          s=bestlam_lasso,
          newx=x_matrix[-train_idx,],
          scale=TRUE)

data.frame(R2 = caret::R2(lasso_pred_Best, test_x2$Price),
           RMSE = caret::RMSE(lasso_pred_Best, test_x2$Price),
           MAE = caret::MAE(lasso_pred_Best, test_x2$Price))

# test MSE=4,632,645,329
mean((lasso_pred_Best-ytest1)^2) 
# test MSE with only Y intercept=
mean((mean(y[train_idx])-ytest1)^2) 

# show coeficients of all predictors (zero and non-zero)
lasso_coef_matrix <- 
  as.data.frame(as.matrix(lasso_mod_coefs_Best))
row.names(lasso_coef_matrix)

lasso_df2 <-
  lasso_coef_matrix %>% 
  mutate(predictor = row.names(lasso_coef_matrix)) %>% 
  dplyr::select(predictor, "1") %>% 
  arrange(predictor)

lasso_df2_nonzero_coefs <-
  lasso_df2 %>% 
  rename(coefs ="1") %>% 
  filter(coefs!=0) %>% 
  arrange(predictor)

write.csv(lasso_df2,
           "first_lasso_coefs_from_solon_x3.csv")
write.csv(lasso_df2_nonzero_coefs,
           "first_lasso_nonzero_coefs_from_solon_x3.csv")

##############################
############## post Lasso
##############################
names(solon_x3_PL)
nonzero_lasso <- fread("nonZero_Lasso.csv")
ncol(solon_x3_PL)

solon_x3_PL <-
  solon_x3 %>% 
  dplyr::select(c(nonzero_lasso$NonZero_Lasso))

solon_x3_PL_x2 <-
  solon_x3_PL %>% 
  select_if(is.numeric)
names(solon_x3_PL)
##############################

### Clustering
#########################################################################


###################################################
solon_x3_PL %>%
  summarise_all(funs(sum(is.na(.))))
# creates scree plot to help determine optimal number of variables
set.seed(1)

summary(solon_x3_PL)

fviz_nbclust(solon_x3_PL_x2, kmeans, method = "wss")
fviz_nbclust(solon_x3_PL_x2, kmeans, method = "silhouette")

gap_stat <- clusGap(solon_x3_PL_x2, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
fviz_gap_stat(gap_stat)
# Optimal number of kmeans clusters is 1
?fviz_gap_stat
# Apply K-means and MCLUST method
distance <- get_dist(solon_x3_PL_x2)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

k2 <- kmeans(solon_x3_PL_x2, centers = 2, nstart = 30)
fviz_cluster(k2, data = solon_x3_PL_x2) + ggtitle("Cluster Plot, k = 2")

k3 <- kmeans(solon_x3_PL_x2, centers = 3, nstart = 30)
fviz_cluster(k3, data = solon_x3_PL_x2) + ggtitle("Cluster Plot, k = 3")
#######################################################

############ MVA
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5096736/

solon_x3_PL_XX <-
  solon_x3_PL %>% 
  select_if(is.numeric) %>% 
  select(-Price)

mod1 <- mclust::Mclust(solon_x3_PL_XX)
summary(mod1)
summary(mod1$BIC)

par(mfrow=c(2,1))

# Use the plots below to dtmn how many components
BIC <- mclustBIC(solon_x3_PL_XX)
BIC
plot(BIC)

ICL <- mclustICL(solon_x3_PL_XX)
ICL
plot(ICL)

# Mclust with 7 Clusters (VEV form, BIC=-482.6)
drmod <- MclustDR(mod1, lambda = 1)
summary(drmod)
plot(drmod, what = "contour")
drmod$classification

head(solon_x3_PL_wC$mclust)

names(solon_x3_PL_wC)
solon_x3_PL_wC <-
  solon_x3_PL %>% 
  mutate(mclust = drmod$classification,
         mclust = as.character(mclust))

solon_x3_PL_wC
### end of MVA
#####################################################
model_A <- lm(data=solon_x3_PL_wC, Price~.)
summary(model_A)
model_B <- lm(data=solon_x3_PL, Price~.)
summary(model_B)
anova(model_A, model_B)
anova(model_B)

############################################
### Model Matrices for Relaxed Lasso ########
############################################

# create 60/40 cross-validation set

set.seed(2020)
# 
# samples=sample(nrow(solon_x3_PL), 
#                0.7*nrow(solon_x3_PL))
# train = solon_x3_PL[samples, ]
# test = solon_x3_PL[-samples,]
# ytest = solon_x3_PL$Price

dim(solon_x3_PL)

# split across stata
train_idx <-
  createDataPartition(solon_x3_PL$Price, 
                      times = 1, 
                      p = 0.75, list=F)
train_x2 = solon_x3_PL[train_idx, ]
test_x2 = solon_x3_PL[-train_idx,]

# producing model matrices for ridge,lasso models
x_matrix <- model.matrix(Price~.,
                         solon_x3_PL)

# x_matrix
y <- solon_x3_PL$Price
ytrain1 = y[train_idx]
ytest1 = y[-train_idx]

lambda <- 10^seq(10,-2,length=100)  


############################################
############# Relaxed lasso Regression #############
############################################

# set alpha=0 for ridge regression, alpha=1 for lasso regression
lasso_mod <- glmnet(x_matrix[train_idx,],
                    y[train_idx],alpha=1,
                    lambda=lambda,
                    scale=TRUE)

# As the lambda value increases, the coefficients get closer to zero.
predict(lasso_mod, s=.1, type='coefficients')
predict(lasso_mod, s=1, type='coefficients')
predict(lasso_mod, s=10, type='coefficients')
predict(lasso_mod, s=100, type='coefficients')

set.seed(2020)
cv_lasso_mod <- 
  cv.glmnet(x_matrix[train_idx,],y[train_idx],alpha=1,
            scale=TRUE, nfolds=10)
cv_lasso_mod # shows lambda, other values
bestlam_lasso <- cv_lasso_mod$lambda.min
bestlam_lasso # best lambda is  4535.24

plot(cv_lasso_mod,main="Lasso Regression")
r_squared_lasso=1-cv_lasso_mod$cvm/var(y[train_idx])

r_squared_lasso_max <- 
  max(1-cv_lasso_mod$cvm/var(y[train_idx]))
r_squared_lasso_max # 64.8%

plot(cv_lasso_mod$lambda,r_squared_lasso, 
     main=expression(R^{2}~"values for different lambda parameters in Lasso Regression Model"))

plot(lasso_mod,xvar="lambda",
     main="
     Coefficients of predictors in Lasso Regression model
     
     ")

plot(lasso_mod, main="Lasso Regression,
     Relationship of L1 Norm and Coefficients")

lasso_modBest <- glmnet(x_matrix[train_idx,],
                        y[train_idx],
                        alpha=1,
                        lambda=bestlam_lasso,
                        scale=TRUE)

lasso_mod_coefs_Best <-
  predict(lasso_modBest,
          type="coefficients",
          s=bestlam_lasso,
          scale=TRUE)

lasso_pred_Best <- 
  predict(lasso_modBest,
          s=bestlam_lasso,
          newx=x_matrix[-train_idx,],
          scale=TRUE)

data.frame(R2 = caret::R2(lasso_pred_Best, test_x2$Price),
           RMSE = caret::RMSE(lasso_pred_Best, test_x2$Price),
           MAE = caret::MAE(lasso_pred_Best, test_x2$Price))

# test MSE=4,632,645,329
mean((lasso_pred_Best-ytest1)^2) 
# test MSE with only Y intercept=
mean((mean(y[samples])-ytest1)^2) 

# show coeficients of all predictors (zero and non-zero)
lasso_coef_matrix <- as.data.frame(as.matrix(lasso_mod_coefs_Best))
row.names(lasso_coef_matrix)

lasso_df2 <-
  lasso_coef_matrix %>% 
  mutate(predictor = row.names(lasso_coef_matrix)) %>% 
  dplyr::select(predictor, "1") %>% 
  arrange(predictor)

lasso_df2_nonzero_coefs <-
  lasso_df2 %>% 
  rename(coefs ="1") %>% 
  filter(coefs!=0) %>% 
  arrange(predictor)

write.csv(lasso_df2,
          "first_lasso_coefs_from_solon_x3_PL.csv")
write.csv(lasso_df2_nonzero_coefs,
          "first_lasso_nonzero_coefs_from_solon_x3_PL.csv")

############################################
############# Ridge Regression #############
############################################

# set alpha=0 for ridge regression, alpha=1 for lasso regression
ridge_mod <- 
  glmnet::glmnet(x_matrix[train_idx,],
                 y[train_idx],alpha=0,
                 lambda=lambda,
                 scale=TRUE)

set.seed(2020)
cv_ridge_mod <- 
  cv.glmnet(x_matrix[train_idx,],y[train_idx],alpha=0,
            scale=TRUE, nfolds=10)
cv_ridge_mod # shows lambda, other values
bestlam_ridge <- cv_ridge_mod$lambda.min
bestlam_ridge # best lambda is 16,257

plot(cv_ridge_mod,main="Ridge Regression")
r_squared_ridge=1-cv_ridge_mod$cvm/var(y[train_idx])

r_squared_ridge_max <- 
  max(1-cv_ridge_mod$cvm/var(y[train_idx]))
r_squared_ridge_max # 51.0%

plot(cv_ridge_mod$lambda,r_squared_ridge, 
     main=expression(R^{2}~"values for different lambda parameters in Ridge Regression Model"))

plot(ridge_mod,xvar="lambda",
     main="
     Coefficients of predictors in Ridge Regression model
     
     ")

plot(ridge_mod, main="Ridge Regression,
     Relationship of L1 Norm and Coefficients")

ridge_modBest <- 
  glmnet(x_matrix[train_idx,],
         y[train_idx],
         alpha=0,
         lambda=bestlam_ridge,
         scale=TRUE)

ridge_mod_coefs_Best <-
  predict(ridge_modBest,
          type="coefficients",
          s=bestlam_ridge,
          scale=TRUE)

ridge_pred_Best <- 
  predict(ridge_modBest,
          s=bestlam_ridge,
          newx=x_matrix[-train_idx,],
          scale=TRUE)

data.frame(R2 = caret::R2(ridge_pred_Best, test_x2$Price),
           RMSE = caret::RMSE(ridge_pred_Best, test_x2$Price),
           MAE = caret::MAE(ridge_pred_Best, test_x2$Price))

# test MSE=10,621,005,345
# test MSE=6,078,596,879
mean((ridge_pred_Best-ytest1)^2) 
# test MSE with only Y intercept=5,813,413,964
mean((mean(y[train_idx])-ytest1)^2) 

# show coeficients of all predictors (zero and non-zero)
ridge_coef_matrix <- as.data.frame(as.matrix(ridge_pred_Best))
row.names(lasso_coef_matrix)

##################################

################################################################
##### Random forest
################################################################
names(solon_x3_PL)

# 16 variables for random forest (only non-zero after lasso)
dim(solon_x3_PL)

memory.limit()
memory.limit(100000)

names(solon_x3_PL_wC) # find index of Price
rf_nonzero_coefs_postLasso <- 
  randomForest(y=train_x2$Price,
               x=train_x2[,-1],
               ntree=500,
               norm.votes=FALSE, 
               do.trace=10,
               importance=TRUE)

rf_nonzero_coefs_postLasso
### with all vars var explained is 76.17%
# MSE is 6,552,957,049

importance(rf_nonzero_coefs_postLasso,  main="Random forest model using 18 variables, MSE as trees increase")
varImpPlot(rf_nonzero_coefs_postLasso, main="Random forest model using 18 variables, Variable Importance Plot")

rf_nonzero_coefs_postLasso_predict <-
  predict(rf_nonzero_coefs_postLasso,
          newdata=test_x2)

rf_nonzero_coefs_postLasso_predict
plot(rf_nonzero_coefs_postLasso_predict)
(rf_nonzero_coefs_postLasso_predict - test_x2$Price) / rf_nonzero_coefs_postLasso_predict

bind_RF_predict_test <- 
  as.data.frame(cbind(rf_nonzero_coefs_postLasso_predict,
                      test_x2$Price))

data.frame(R2 = caret::R2(rf_nonzero_coefs_postLasso_predict, test_x2$Price),
           RMSE = caret::RMSE(rf_nonzero_coefs_postLasso_predict, test_x2$Price),
           MAE = caret::MAE(rf_nonzero_coefs_postLasso_predict, test_x2$Price))

################################################################
##### Final models: cross-validation in linear regression 
##### Using standardized coefficients
################################################################
# Define training control
set.seed(2020) 
train.control <- trainControl(method = "cv", number = 10)

# Train the model
model <- train(Price~., 
                   data = train_x2, 
                   method = "lm",
                   trControl = train.control)
# Summarize the results
summary(model)

predictions <- model %>% predict(test_x2)

data.frame(R2 = caret::R2(predictions, test_x2$Price),
           RMSE = caret::RMSE(predictions, test_x2$Price),
           MAE = caret::MAE(predictions, test_x2$Price))

# Adjusted R^2 is 0.8876
summary(model)$adj

# RMSE is 24817.13
# MSE is 615,889,941

# bias
mean(predictions - test_x2$Price)
median(predictions - test_x2$Price)

# max deviation
max(predictions - test_x2$Price)

# mean absolute deviation
mean(abs(predictions - test_x2$Price))
median(abs(predictions - test_x2$Price))

# mean squared error
mean((predictions - test_x2$Price)^2)

################################################################
##### Final models: cross-validation in random forest
##### Using normal coefficients (standardizing not necessary)
##### Using rpart() function for random forest
################################################################

# Cross validation random forest
numFolds <- trainControl(method = "cv", number = 10)

cpGrid <- expand.grid(.cp = seq(0.01, 0.5, 0.01))
# final value for cp is 0.01

rv_CV <- 
  rpart::rpart(Price~.,
               data = train_x2, 
               method = "anova",
               cp = 0.01)

eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))
  
  # Model performance metrics
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square
  )
  
}

#####################
predictions_train = predict(rv_CV, data = train_x2)
eval_results(train_x2$Price, predictions_train, train_x2)

predictions_test = predict(rv_CV, newdata = test_x2)
eval_results(test_x2$Price, predictions_test, test_x2)

predictionCV <- predict(rv_CV, 
                        newdata = test_x2, type = "vector")
summary(rv_CV)
# relative error is 1-R^2
# relative error is 0.2078753
# R^2 = 1-0.2078753 = 0.7921247

?rsq.rpart
rsq.rpart(rv_CV)

tmp <- printcp(rv_CV)
rsq.val <- 1-tmp[,c(3,4)] 
rsq.val


# bias
mean(predictionCV - test$Price)
# -82.00716
median(predictionCV - test$Price)
# 1495.73

# max deviation
max(abs(predictionCV - test$Price))
# 178233.5

# mean absolute deviation
mean(abs(predictionCV - test$Price))
# 25845.86
median(abs(predictionCV - test$Price))
# 18941.89

# mean squared error
mean((predictionCV - test_x2$Price)^2)
# 1,266,812,831

#################################

####################################################################=
##### Checking for multicollinearity
####################################################################=
ncol(solon_x3_PL)
# 9 variables
model_Z <- 
  lm(data=solon_x3_PL,
     Price~.)
plot(model_Z,
     main="Residual plots of nine variable MLR")
par(mfrow=c(2,2))

summary(lm_16vars)
vif(solon_x3_PL_x1)

solon_x3_PL_x2 <-
  solon_x3_PL %>% 
  select(-Year_Built_Decade2)

ncol(solon_x3_PL_x1)
model_K <- 
  lm(data=solon_x3_PL_x1,
     Price~.)
vif(model_K)
summary(model_K)
head(solon_x3_PL_x1)
# plot(lm_15vars,
#      main="Residual plots of nine variable MLR")
summary(lm_15vars)
vif(lm_15vars)

solon_x3_PL_x1 <-
  solon_x3_PL %>% 
  select(-c(
            Basement_Square_Feet2, 
            GarageYrBlt_decade2,
            Garage_Type2,
            Construction_Quality2, 
            # Rooms, # highly correlated with SQ FT, 0% of R^2
            Condition2, 
            # RoofType2,
            Garage_Capacity2,
            Garage_Size2,
            # Style2, # 1% of R^2
            # Year_Built_Decade2, #9%
            # Street_Name, # lot
            Exterior_Walls2
            ))

anova(lm_5vars)
anova(lm_16vars, lm_5vars)
# mean squared error

anova(lm_16vars)['Residuals', 'Mean Sq']
anova(lm_5vars)['Residuals', 'Mean Sq']

summary(lm_16vars)$adj
summary(lm_5vars)$adj

### Breush-Pagan test for constant error variance
lmtest::bptest(Price~.,
               data=solon_x3_PL_x1,
               studentize=TRUE)
bptest(lm_5vars)
plot(lm_5vars)
### We reject the null hypothesis of constant error variance
### Should address this

lm_9vars_logY <- 
  lm(data=clean_df4_9vars,log(SalePrice)~.)
summary(lm_9vars_logY)
bptest(lm_9vars_logY)
# Still have heteroscedasticity after transforming SalePrice


################################################################
##### Final models: cross-validation in random forest
##### Using normal coefficients (standardizing not necessary)
##### Using rpart() function for random forest
################################################################

# Cross validation random forest
numFolds <- trainControl(method = "cv", number = 10)

cpGrid <- expand.grid(.cp = seq(0.01, 0.5, 0.01))
train(Price~., 
      data = solon_x3_PL_x1, 
      method = "rpart", 
      trControl = numFolds, 
      tuneGrid = cpGrid)
# final value for cp is 0.01

rv_CV <- 
  rpart::rpart(Price~.,
               data = solon_x3_PL, 
               method = "anova",
               cp = 0.01)

predictionCV <- predict(rv_CV, 
                        newdata = test, type = "vector")
summary(rv_CV)
# relative error is 1-R^2
# relative error is 0.2078753
# R^2 = 1-0.2078753 = 0.7921247


# rsq.rpart(rv_CV)

tmp <- printcp(rv_CV)
rsq.val <- 1-tmp[,c(3,4)] 
rsq.val


# bias
mean(predictionCV - test$Price)
# -82.00716
median(predictionCV - test$Price)
# 1495.73

# max deviation
max(abs(predictionCV - test$Price))
# 178233.5

# mean absolute deviation
mean(abs(predictionCV - test$Price))
# 25845.86
median(abs(predictionCV - test$Price))
# 18941.89

# mean squared error
mean((predictionCV - test$Price)^2)
# 1,266,812,831

##### same MSE when using 4 or 16 predictors!!!

#####################
############ PCR
set.seed(2020)
pcr.fit <- 
  pcr(data=solon_x3_PL,Price~., validation="CV")
summary(pcr.fit)

validationplot(pcr.fit, val.type = "MSEP")

####
regfit_full <- 
  regsubsets(Price~.,data=solon_x3_PL,nvmax=16)
