library(rlang)
library(vctrs)
library(readxl)
library(forcats)
library(lubridate)
library(purrr)
library(writexl)
library(openxlsx)
library(BAS)
library(BAS)
library(lm.beta)
library(car)
library(outliers)
library(glmnet)
library(caret)
library(leaps)
library(MASS)
library(tidyverse) 
# tidyverse must go after MASS. use dplyr::select()
library(devtools)
library(dplyr)
library(arm)
library(randomForest)
library(e1071)
library(Metrics)
library(rpart)
library(gridExtra)
library(lmtest)
library(magrittr)
setwd("C:/Users/fouad/Documents/misc/fouad/hunter/stats 707 glm 2/project/data")

#####################################################################
######## Reading in data
#####################################################################

train <- read_excel("training.xlsx")
test <- read_excel("test.xlsx")

# train %>% 
#   dplyr::select(BldgType)

# read in the columns to remove file
col_to_remove_DF <-
  read_excel("ames columns to remove 04-25-20.xlsx",
             sheet = "to_remove")

col_to_remove_DF_allLookedAt <-
  read_excel("ames columns to remove 04-25-20.xlsx",
             sheet = "all_looked_at")

# read in the columns to regroup file
regroup_columns <-
  read_excel("columns_to_regroup.xlsx",
             sheet = "Sheet1")

#####################################################################
######## Plots of caetgorical variables that are 90%+ the same  
#####################################################################

plot1 <-
  ggplot(data=col_to_remove_DF_allLookedAt_x2,
         aes(x=reorder(Removed_columns, perc),y=perc,label=perc))+
  geom_bar(stat="identity", fill="gray") +
  coord_flip() + geom_text() +
  ggtitle("Ordered by 
          Percentage")

plot2 <- 
  ggplot(data=col_to_remove_DF_allLookedAt_x2,
         aes(x=Removed_columns,y=perc,label=perc))+
  geom_bar(stat="identity", fill="gray") +
  coord_flip() + geom_text() +
  ggtitle("Ordered by 
          Reverse alphabetical")

# Categorical Variables with 90%+ of the same values
grid.arrange(plot1, plot2, ncol=2)

#####################################################################
######## First run of cleaning data set
#####################################################################

### Converts columns to character or numeric
### Makes all cells in character columns lowercase
clean_df <-
  train %>% 
  mutate_at(regroup_columns$Reshaped_columns,
            as.character) %>%
  mutate_if(is.character,tolower) %>% 
  mutate(MasVnrArea=as.numeric(MasVnrArea),
         LotFrontage=as.numeric(LotFrontage),
         GarageYrBlt=as.numeric(GarageYrBlt),
         MSSubClass=as.character(MSSubClass)) 

#####################################################################
######## Second run of cleaning data set
#####################################################################

### Conflate categorical levels that appear less than 5% (53) of the time 
clean_df2 <-
  clean_df %>% 
  mutate(
    BldgType =case_when(
      clean_df$BldgType %in% c("2fmcon","duplex")~"two_family",
      clean_df$BldgType %in% c("twnhs","twnhse")~"townhouse",
      clean_df$BldgType=="1fam"~"one_family"),
    BsmtExposure = case_when(
      clean_df$BsmtExposure %in% c("gd") ~ "bsmt_good_exposure",
      clean_df$BsmtExposure %in% c("av") ~ "bsmt_average_exposure",
      clean_df$BsmtExposure %in% c("mn") ~ "bsmt_minimum_exposure",
      clean_df$BsmtExposure=="no"~"bsmt_no_exposure",
      clean_df$BsmtExposure=="na"~"no_basement"),
    BsmtFinType1 = case_when(
      clean_df$BsmtFinType1 %in% c("glq") ~ "BsmtFin_goodLivingQuarters",
      clean_df$BsmtFinType1 %in% c("alq") ~ "BsmtFin_averageLivingQuarters",
      clean_df$BsmtFinType1 %in% c("blq","lwq") ~ "BsmtFin_belowAverageLivingQuarters",
      clean_df$BsmtFinType1 %in% c("rec") ~ "BsmtFin_averageRecRoom",
      clean_df$BsmtFinType1=="unf"~"BsmtFin_Unfinished",
      clean_df$BsmtFinType1=="na"~"No_Basesment"),
    BsmtFinType2 = case_when(
      clean_df$BsmtFinType2 %in% c("alq","glq","rec") ~ "BsmtFin_AverageOrBetter",
      clean_df$BsmtFinType2 %in% c("blq","lwq") ~ "BsmtFin_BelowAverage", 
      clean_df$BsmtFinType2=="unf"~"BsmtFin_Unfinished",
      clean_df$BsmtFinType2=="na"~"No_Basesment"),
    BsmtQual = case_when(
      clean_df$BsmtQual %in% c("ex") ~"excellent",
      clean_df$BsmtQual %in% c("gd") ~"good",
      clean_df$BsmtQual %in% c("ta","fa") ~"average_or_below", 
      clean_df$BsmtQual=="na"~"no_basement"),
    Condition1 =case_when(
      clean_df$Condition1 %in% c("rrae","rran","rrne","rrnn") ~"Railroad",
      clean_df$Condition1 %in% c("artery","feedr","posa","posn") ~"Postive", 
      clean_df$Condition1=="norm"~"Normal"),
    Electrical = case_when(
      clean_df$Electrical %in% c("fusea","fusef","fusep","mix") ~ "fuse_box",
      clean_df$Electrical %in% c("sbrkr") ~ "breaker_box"),
    ExterCond = case_when(
      clean_df$ExterCond %in% c("ex","gd") ~ "above_average",
      clean_df$ExterCond %in% c("ta","fa","po") ~ "average_or_below"),
    Exterior1st = case_when(
      clean_df$Exterior1st %in% c("asbshng","cblock","cemntbd","asphshn","stone",
                                  "stucco","brkcomm","brkface") ~ "cement_or_other_siding",
      clean_df$Exterior1st %in% c("wd sdng","wdshing","plywood") ~ "wood_siding",
      clean_df$Exterior1st %in% c("metalsd") ~ "metal_siding",
      clean_df$Exterior1st %in% c("vinylsd") ~ "vinyl_siding",
      clean_df$Exterior1st %in% c("hdboard") ~ "hardboard_siding"),
    ExterQual = case_when(
      clean_df$ExterQual %in% c("ex","gd") ~ "above_average",
      clean_df$ExterQual %in% c("ta","fa") ~ "average"), 
    Fence = case_when(
      clean_df$Fence %in% c("gdprv","gdwo") ~ "good_privacy_or_good_wood",
      clean_df$Fence %in% c("mnprv","mnww") ~ "min_privacy_or_material",
      clean_df$Fence %in% c("na") ~ "no_fence"), 
    FireplaceQu =case_when(
      clean_df$FireplaceQu %in% c("ex","gd") ~ "above_average",
      clean_df$FireplaceQu %in% c("fa","ta","po") ~ "average_or_worse",
      clean_df$FireplaceQu %in% c("na") ~ "no_fireplace"), 
    Foundation = case_when(
      clean_df$Foundation %in% c("pconc","slab") ~ "nonporous_foundation",
      clean_df$Foundation %in% c("stone","cblock","wood") ~ "porous_foundation",
      clean_df$Foundation %in% c("brktil") ~ "brick_foundation"),
    Functional = case_when(
      clean_df$Functional %in% c("maj1","maj2","min1","min2","mod","sev") ~ "some_work_needed",
      clean_df$Functional %in% c("typ") ~ "typical"),
    GarageFinish = case_when(
      clean_df$GarageFinish %in% c("fin") ~ "finished",
      clean_df$GarageFinish %in% c("unf") ~ "unfinished",
      clean_df$GarageFinish %in% c("rfn") ~ "rough_finished",
      clean_df$GarageFinish %in% c("na") ~ "no_garage"),
    GarageType = case_when(
      clean_df$GarageType %in% c("attchd","basment","builtin","carport","2types") ~ "attached",
      clean_df$GarageType %in% c("detchd") ~ "detached",
      clean_df$GarageType %in% c("na") ~ "no_garage"),
    HeatingQC = case_when(
      clean_df$HeatingQC %in% c("ex") ~ "excellent",
      clean_df$HeatingQC %in% c("gd") ~ "good",
      clean_df$HeatingQC %in% c("ta","fa","po") ~ "average_or_worse"),
    HouseStyle = case_when(
      clean_df$HouseStyle %in% c("1.5fin") ~ "stories_1.5fin",
      clean_df$HouseStyle %in% c("1story","1.5unf") ~ "stories_1fin_or_1.5unfin",
      clean_df$HouseStyle %in% c("sfoyer","slvl") ~ "split_level",
      clean_df$HouseStyle %in% c("2story","2.5fin","2.5unf") ~ "stories_2_or_more"),
    KitchenQual = case_when(
      clean_df$KitchenQual %in% c("ex") ~ "excellent",
      clean_df$KitchenQual %in% c("gd") ~ "good",
      clean_df$KitchenQual %in% c("fa","ta") ~ "average"),
    LandContour = case_when(
      clean_df$LandContour %in% c("lvl") ~ "normal",
      clean_df$LandContour %in% c("bnk","hls","low") ~ "not_normal"),
    LandSlope = case_when(
      clean_df$LandSlope %in% c("gtl") ~ "gentle",
      clean_df$LandSlope %in% c("mod","sev") ~ "non-gentle"),
    LotConfig = case_when(
      clean_df$LotConfig %in% c("corner") ~ "Corner",
      clean_df$LotConfig %in% c("culdsac") ~ "CulDSac",
      clean_df$LotConfig %in% c("inside","fr2","fr3") ~ "inside_or_nextToEmptyLots"),
    LotShape = case_when(
      clean_df$LotShape %in% c("reg") ~ "Corner",
      clean_df$LotShape %in% c("ir1","ir2","ir3") ~ "irregular"),
    MasVnrType = case_when(
      clean_df$MasVnrType %in% c("brkcmn","brkface") ~ "brick",
      clean_df$MasVnrType %in% c("stone") ~ "stone",
      clean_df$MasVnrType %in% c("na","none") ~ "none"),
    MSZoning = case_when(
      clean_df$MSZoning %in% c("rl") ~ "Resi_Low_Density",
      clean_df$MSZoning %in% c("rm", "rh") ~ "Resi_Medium_orHigh_Density",
      clean_df$MSZoning %in% c("fv", "c (all)") ~ "Floating_village_or_All"),
    PavedDrive = case_when(
      clean_df$PavedDrive %in% c("y") ~ "paved",
      clean_df$PavedDrive %in% c("n","p") ~ "not_or_partially_paved"),
    RoofStyle = case_when(
      clean_df$RoofStyle %in% c("gable","gambrel","flat","shed") ~ "two_or_one_sided_roof",
      clean_df$RoofStyle %in% c("mansard","hip") ~ "four_sided_roof"),
    SaleCondition = case_when(
      clean_df$SaleCondition %in% c("normal","alloca") ~ "normal",
      clean_df$SaleCondition %in% c("abnorml", "family","adjland") ~ "abnormal",
      clean_df$SaleCondition %in% c("partial") ~ "partial"),
    SaleType = case_when(
      clean_df$SaleType %in% c("wd","cwd","oth","cod","con","conld","conli","conlw") ~ "normal",
      clean_df$SaleType %in% c("new") ~ "new"))

#####################################################################
######## Finding columns with NAs
#####################################################################

table(train$PoolQC)

# shows all "NA" values
as.data.frame(sapply(X = clean_df2,
                     FUN = function(x) sum(is.na(x))))

# There are 88 null values for LotFrontage, 
# 6 for MasVnrArea
# 58 for GarageYrBuilt

as.data.frame(sapply(X = clean_df2,
                     FUN = function(x) sum(x=="NA")))

summary(clean_df2)
quantile(clean_df4$SalePrice,c(0,.2,.4,.6,.8,.9,.95,1))
q1 <- scale(clean_df4$SalePrice)
q1 <- scores(clean_df4$SalePrice, type="z")
str(q1)
head(q1)

myfunction <- function(df, col){
  centered = df$col-mean(df$col)
  scaled = centered/sd(df$col)
  return(scaled)
}
SalePriceX2 <- 
  sapply(clean_df4$SalePrice, function(x) x-mean(x)/sd(x))

SalePriceX2 <- 
  sapply(clean_df4$SalePrice, function(x) (x)-mean(x))

SalePriceX2
str(clean_df4$SalePrice)
mean(clean_df4$SalePrice)

clean_df4 %>%
  dplyr::select(SalePrice) %>%
  mutate(SalePriceX2 = ((SalePrice-mean(SalePrice))/sd(SalePrice)))

SalePriceX2

myfunction <- function(df, col){
  centered = col-mean(col)
  scaled = centered/sd(df$col)
  return(scaled)
}
myfunction(clean_df4,clean_df4$SalePrice)

myfunction <- 
  function(arg1, arg2, ... ){
    statements
    return(object)
  }


# find variables where 95% of the values are in one category
train_isChar <-
  train %>%
  select_if(is.character)

freq_table <- function(i) {
  prop_table <- prop.table(table(train[[i]]))
  prop_table_df <- as.data.frame(prop_table)

  prop_table_df_x2 <- 
    prop_table_df  %>%
    rename(level=Var1, freq=Freq) %>%
    mutate(var=as.character(i)) %>%
    filter(freq >= 0.90)
}

df <- colnames(train_isChar) %>% 
  map_df(~freq_table(.)) %>%
  dplyr::select(var, level, freq) %>%
  arrange(desc(freq), var, level)

df


df_total <- rbind(df_total, prop_table_df_x2)

for (i in 1:7){
  # vector output
  model <- #some processing
    
    # add vector to a dataframe
    df <- data.frame(model)
  df_total <- rbind(df_total,df)
}

prop_table_df_x2

prop_table <- prop.table(table(train$Street))
prop_table_df <- as.data.frame(prop_table)

prop_table_df  %>%
  rename(level=Var1, freq=Freq) %>%
  mutate(var=as.character("Street"))  %>%
  filter(freq >= 0.95)




# prop_table_df$Freq_label <-
#   ifelse(prop_table_df$Freq >= 0.95, 
#        "appears very often",
#        "appears not as often")

if(prop_table_df$Freq)

prop_table_df  %>%
  filter(Freq >= 0.95) %>%
  mutate(colX)



str(prop_table)
prop_table$table
str(as.data.frame(prop_table))

myfunction <- function(df, col){
  col
  return(df$col)
}

a2 <- myfunction(clean_df4,clean_df4$MSSubClass)
table(myfunction(clean_df4,clean_df4$MSSubClass))
table(clean_df4$MSSubClass)


ifelse(prop.table(table(clean_df4$MSSubClass)),
       )
# 188 values have NA for LotFrontage
# impute median based on zoning, HouseStyle 
table(train$LotFrontage)

# 6 values have NA for MasVnrArea (MasVnrArea not specified)
# impute median based on zoning, HouseStyle 
table(train$MasVnrType)
table(train$MasVnrArea)

# 58 values have NA for GarageYrBlt (no garage)
# impute as 0
table(train$GarageType)
table(train$GarageYrBlt)

# Impute LotFrontage, MasVnrArea with median Zoning Type, Housing Style
values_to_impute <- 
  clean_df2 %>% 
  # group_by(Neighborhood,HouseStyle) %>% 
  group_by(MSZoning,HouseStyle) %>% 
  summarize(median_LotFrontage = median(LotFrontage,na.rm=TRUE),
            median_MasVnrArea = median(MasVnrArea,na.rm=TRUE))

#####################################################################
######## Third run of cleaning data set
#####################################################################

####################################################
######## Imputing data for columns with NAs ########
######## Making discrete variables ordinal #########
######## Splitting years into decades ##############
####################################################

clean_df3 <-
  clean_df2 %>% 
  left_join(values_to_impute,
            by=c("MSZoning"="MSZoning",
                 "HouseStyle"="HouseStyle")) %>% 
  # impute missing values
  mutate(LotFrontage =
           ifelse(is.na(LotFrontage), median_LotFrontage, LotFrontage),
         MasVnrArea = 
           ifelse(is.na(MasVnrArea),median_MasVnrArea,MasVnrArea),
         GarageYrBlt =
           ifelse(is.na(GarageYrBlt),10,GarageYrBlt)) %>%
  dplyr::select(-c(median_LotFrontage,median_MasVnrArea)) %>%
  # create new columns summing full bath and half bath
  mutate(TotalBath_AbvGround = FullBath + (0.5*HalfBath),
         TotalBath_Basement = BsmtFullBath + (0.5*BsmtHalfBath),
         TotalBath_AbvGrAndBasement = 
           TotalBath_AbvGround + TotalBath_Basement) %>% 
  dplyr::select(-FullBath,-HalfBath,-BsmtFullBath,-BsmtHalfBath,
                -TotalBath_AbvGround,-TotalBath_Basement) %>% 
  mutate(TotalBath_AbvGrAndBasement = 
           as.character(TotalBath_AbvGrAndBasement),
         TotalBath_AbvGrAndBasement = case_when(
           TotalBath_AbvGrAndBasement %in% c("1","1.5") ~ "bathrooms_1-1.5",
           TotalBath_AbvGrAndBasement %in% c("2","2.5") ~ "bathrooms_2-2.5",
           TotalBath_AbvGrAndBasement %in% c("3","3.5","4","4.5","5","6") ~ "bathrooms_3+"),
         TotRmsAbvGrd = as.character(TotRmsAbvGrd),
         TotRmsAbvGrd = case_when(
           TotRmsAbvGrd %in% c("2","3","4","5") ~ "total_rooms_2-5",
           TotRmsAbvGrd %in% c("6","7") ~ "total_rooms_6-7",
           TotRmsAbvGrd %in% c("8","9","10","11","12","14") ~ "total_rooms_8+"),
         BedroomAbvGr = as.character(BedroomAbvGr),
         BedroomAbvGr = case_when(
           BedroomAbvGr %in% c("0","1","2") ~ "bedrooms_0-2",
           BedroomAbvGr %in% c("3") ~ "bedrooms_3",
           BedroomAbvGr %in% c("4","5","6","7","8") ~ "bedrooms_4+"),
         KitchenAbvGr = as.character(KitchenAbvGr),
         KitchenAbvGr = case_when(
           KitchenAbvGr %in% c("0","1") ~ "kitchens_1",
           KitchenAbvGr %in% c("2") ~ "kitchens_2")) %>% 
  # make MoSold Categorical 
  mutate(MoSold = as.character(MoSold),
         MoSold = case_when(
           clean_df2$MoSold %in% c("1","2","3","4") ~ "January to April",
           clean_df2$MoSold %in% c("5","6","7","8") ~ "May to August",
           clean_df2$MoSold %in% c("9","10","11","12") ~ "September to December")) %>%
  # make discrete variables ordinal
  mutate(TotalBath_AbvGrAndBasement = 
           factor(TotalBath_AbvGrAndBasement,ordered=TRUE),
         BedroomAbvGr = factor(BedroomAbvGr,ordered=TRUE),
         KitchenAbvGr = factor(KitchenAbvGr,ordered=TRUE),
         TotRmsAbvGrd = factor(TotRmsAbvGrd,ordered=TRUE),
         Fireplaces = factor(Fireplaces,ordered=TRUE),
         GarageCars = factor(GarageCars,ordered=TRUE),
         OverallQual = factor(OverallQual,ordered=TRUE),
         OverallCond = factor(OverallCond,ordered=TRUE)) %>% 
  # make year variables ordinal
  mutate(YrSold = factor(YrSold,ordered=TRUE)) %>% 
  # MoSold = factor(MoSold,ordered=TRUE)
  # get decade for each year with many levels
  mutate(YearBuilt_decade = floor(YearBuilt/30)*30,
         YearRemodAdd_decade = floor(YearRemodAdd/30)*30,
         GarageYrBlt_decade = floor(GarageYrBlt/30)*30) %>%
  dplyr::select(-YearBuilt,-YearRemodAdd,-GarageYrBlt) %>%
  # make decades ordinal
  mutate(YearBuilt_decade = factor(YearBuilt_decade,
                                   levels=c("1860","1890","1920",
                                            "1950","1980"),
                                   ordered=TRUE),
         YearBuilt_decade = recode_factor(clean_df3$YearBuilt_decade,
                                          `1860` ="1920-1950 or before",
                                          `1890` ="1920-1950 or before",
                                          `1920` ="1920-1950 or before",
                                          `1950` = "1950-1980",
                                          `1980` = "1980-2010"),
         YearRemodAdd_decade = factor(YearRemodAdd_decade,
                                      levels=c("1950","1980","2010"),
                                      ordered=TRUE),
         YearRemodAdd_decade = recode_factor(clean_df3$YearRemodAdd_decade,
                                             `1950` = "1950-1980",
                                             `1980` = "1980-2010 or later",
                                             `2010` = "1980-2010 or later"),
         GarageYrBlt_decade = factor(GarageYrBlt_decade,
                                     levels=c("0",
                                              "1890","1920","1950",
                                              "1980","2010"),
                                     ordered=TRUE),
         GarageYrBlt_decade = recode_factor(clean_df3$GarageYrBlt_decade,
                                            `0` ="no_garage",
                                            `1890` ="1920-1950 or before",
                                            `1920` = "1920-1950 or before",
                                            `1950` = "1950-1980",
                                            `1980` = "1980-2010 or later",
                                            `2010` = "1980-2010 or later"))

# Checking the results 
sum(table(clean_df3$TotalBath_AbvGrAndBasement))
sum(table(clean_df3$TotRmsAbvGrd))
sum(table(clean_df3$BedroomAbvGr))
sum(table(clean_df3$KitchenAbvGr))
sum(table(clean_df3$YearBuilt_decade))
sum(table(clean_df3$YearRemodAdd_decade))
sum(table(clean_df3$GarageYrBlt_decade))
table(clean_df3$GarageYrBlt_decade) # No garage is separate
table(clean_df3$BsmtCond) # BsmtCond has a hard-coded "na" for no basement

clean_df3_onlyNumeric <-
  clean_df3 %>% 
  select_if(is.numeric)

colnames(clean_df3_onlyNumeric)

#####################################################################
######## Univariate and Multivariate outliers, High leverage points
#####################################################################

#### Univariate outliers: looking at Zscores, Tstudentized values
Zscores_df <-
  clean_df3 %>% 
  # confirming numeric variables are numeric
  mutate(LotFrontage_Zscore = outliers::scores(LotFrontage, type="z"),
         LotArea_Zscore = scores(LotArea, type="z"),
         MasVnrArea_Zscore = scores(MasVnrArea, type="z"),
         BsmtFinSF1_Zscore = scores(BsmtFinSF1, type="z"),
         BsmtFinSF2_Zscore = scores(BsmtFinSF2, type="z"),
         BsmtUnfSF_Zscore = scores(BsmtUnfSF, type="z"),
         TotalBsmtSF_Zscore = scores(TotalBsmtSF, type="z"),
         X1stFlrSF_Zscore = scores(X1stFlrSF, type="z"),
         X2ndFlrSF_Zscore = scores(X2ndFlrSF, type="z"),
         LowQualFinSF_Zscore = scores(LowQualFinSF, type="z"),
         GrLivArea_Zscore = scores(GrLivArea, type="z"),
         GarageArea_Zscore = scores(GarageArea, type="z"),
         WoodDeckSF_Zscore = scores(WoodDeckSF, type="z"),
         OpenPorchSF_Zscore = scores(OpenPorchSF, type="z"),
         EnclosedPorch_Zscore = scores(EnclosedPorch, type="z"),
         X3SsnPorch_Zscore = scores(X3SsnPorch, type="z"),
         ScreenPorch_Zscore = scores(ScreenPorch, type="z"),
         PoolArea_Zscore = scores(PoolArea, type="z"),
         MiscVal_Zscore = scores(MiscVal, type="z"),
         SalePrice_Zscore = scores(SalePrice, type="z"),
         # confirming number of rooms is normal too
         TotalBath_AbvGrAndBasement_Zscore = scores(as.numeric(TotalBath_AbvGrAndBasement), type="z"),
         BedroomAbvGr_Zscore = scores(as.numeric(BedroomAbvGr), type="z"),
         KitchenAbvGr_Zscore = scores(as.numeric(KitchenAbvGr), type="z"),
         TotRmsAbvGrd_Zscore = scores(as.numeric(TotRmsAbvGrd), type="z"),
         Fireplaces_Zscore = scores(as.numeric(Fireplaces), type="z"),
         GarageCars_Zscore = scores(as.numeric(GarageCars), type="z")) %>% 
  dplyr::select(Id,LotArea,LotArea_Zscore,GrLivArea,GrLivArea_Zscore,
                SalePrice,SalePrice_Zscore)
# View(Zscores_df)

Tscores_df <-
  clean_df3 %>% 
  # confirming numeric variables are numeric
  mutate(LotFrontage_Tstud = outliers::scores(LotFrontage, type="t"),
         LotArea_Tstud = scores(LotArea, type="t"),
         MasVnrArea_Tstud = scores(MasVnrArea, type="t"),
         BsmtFinSF1_Tstud = scores(BsmtFinSF1, type="t"),
         BsmtFinSF2_Tstud = scores(BsmtFinSF2, type="t"),
         BsmtUnfSF_Tstud = scores(BsmtUnfSF, type="t"),
         TotalBsmtSF_Tstud = scores(TotalBsmtSF, type="t"),
         X1stFlrSF_Tstud = scores(X1stFlrSF, type="t"),
         X2ndFlrSF_Tstud = scores(X2ndFlrSF, type="t"),
         LowQualFinSF_Tstud = scores(LowQualFinSF, type="t"),
         GrLivArea_Tstud = scores(GrLivArea, type="t"),
         GarageArea_Tstud = scores(GarageArea, type="t"),
         WoodDeckSF_Tstud = scores(WoodDeckSF, type="t"),
         OpenPorchSF_Tstud = scores(OpenPorchSF, type="t"),
         EnclosedPorch_Tstud = scores(EnclosedPorch, type="t"),
         X3SsnPorch_Tstud = scores(X3SsnPorch, type="t"),
         ScreenPorch_Tstud = scores(ScreenPorch, type="t"),
         PoolArea_Tstud = scores(PoolArea, type="t"),
         MiscVal_Tstud = scores(MiscVal, type="t"),
         SalePrice_Tstud = scores(SalePrice, type="t"),
         # confirming number of rooms is normal too
         TotalBath_AbvGrAndBasement = scores(as.numeric(TotalBath_AbvGrAndBasement), type="t"),
         BedroomAbvGr_Tstud = scores(as.numeric(BedroomAbvGr), type="t"),
         KitchenAbvGr_Tstud = scores(as.numeric(KitchenAbvGr), type="t"),
         TotRmsAbvGrd_Tstud = scores(as.numeric(TotRmsAbvGrd), type="t"),
         Fireplaces_Tstud = scores(as.numeric(Fireplaces), type="t"),
         GarageCars_Tstud = scores(as.numeric(GarageCars), type="t")) %>% 
  dplyr::select(Id,LotArea,LotArea_Tstud,GrLivArea,GrLivArea_Tstud,
                SalePrice,SalePrice_Tstud)

# View(Tscores_df)

#### Multivariate outliers: Mahalanobis D 

clean_df3_onlyNumeric <-
  clean_df3 %>% 
  dplyr::select_if(is.numeric) %>% 
  dplyr::select(-c(Id))

mean <- colMeans(clean_df3_onlyNumeric)
cov_mat <- cov(clean_df3_onlyNumeric)
d2 <- mahalanobis(clean_df3_onlyNumeric,mean,cov_mat,tol=1e-40)

clean_df3b <- 
  clean_df3 %>% 
  mutate(d2 = d2,
         d2_Tstud = scores(d2, type="t")) %>% 
  dplyr::select(Id,d2,d2_Tstud) %>% 
  filter(d2_Tstud <=5.99)
# View(clean_df3b)

#### High leverage points: looking at Cook's distance, hat values
lm_2 <- lm(data=clean_df4, formula = SalePrice~.)
summary(lm_2)

summary(influence.measures(lm_2))
influential_measures<-
  as.data.frame(summary(influence.measures(lm_2)))

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
  filter(cook.d > (4/nrow(clean_df4)) |
           hat > 2*((206)/nrow(clean_df4))) %>% 
  dplyr::select(Id)
influ2X

# Observations with cook's distance above threshold
cook_dist <-
  influ2 %>% 
  filter(cook.d > (4/nrow(clean_df4))) %>% 
  dplyr::select(Id)
cook_dist
# 35

# Observations with hat values above threshold
hat_items <-
  influ2 %>% 
  filter(hat > 2*((206)/nrow(clean_df4))) %>% 
  dplyr::select(Id)
hat_items
# 23

# Observations with both cook's distance and hat values above threshold
both_cook_hat <-
  influ2 %>% 
  filter(cook.d > (4/nrow(clean_df4)) &
           (hat > 2*((206)/nrow(clean_df4)))) %>% 
  dplyr::select(Id)
both_cook_hat
# 8

#### clean_df4: outliers are removed, leverage points are retained
clean_df4 <-
  clean_df3_afterColumnsRemoved %>% 
  filter(GrLivArea<=4000 &
           LotArea<= 70000 &
           SalePrice <= 570000)

#### clean_df4b: both outliers and leverage points are removed
clean_df4b <-
  clean_df4 %>% 
  filter(!Id %in% influ2X$Id)
dim(clean_df4b)

## Outliers removed
write.xlsx(clean_df4,
           "clean_df4_FINAL.xlsx")

## Outliers and some high leverage removed
write.xlsx(clean_df4b,
           "clean_df4b_FINAL.xlsx")

#### Comparing the output models with and without outliers, high leverage points
clean_df3_afterColumnsRemoved_woLeverage <-
  clean_df3_afterColumnsRemoved %>% 
  filter(!Id %in% influ2X$Id)

lm_zero_Observations_Removed <-
  lm(data=clean_df3_afterColumnsRemoved,
     SalePrice~.)
a1<-summary(lm_zero_Observations_Removed)$adj

lm_Outliers_Removed <-
  lm(data=clean_df4,
     SalePrice~.)
a2<-summary(lm_Outliers_Removed)$adj

lm_HighLeverage_Removed <-
  lm(data=clean_df3_afterColumnsRemoved_woLeverage,
     SalePrice~.)
a3<-summary(lm_HighLeverage_Removed)$adj

lm_Outliers_and_HighLeverage_Removed <-
  lm(data=clean_df4b,
     SalePrice~.)
a4<-summary(lm_Outliers_and_HighLeverage_Removed)$adj

adj_r2 <- as.data.frame(rbind(a1,a2,a3,a4))

combinedX1 <-
  as.data.frame(cbind(adj_r2,
                      c("lm_Zero_Observations_Removed",
                        "lm_Outliers_Removed",
                        "lm_HighLeverage_Removed",
                        "lm_Outliers_and_HighLeverage_Removed")))

combinedX2 <-
  combinedX1 %>% 
  dplyr::rename(perc=V1,models=2) %>% 
  mutate(perc=round(perc*100,2))

ggplot(data=combinedX2,
       aes(x=models,
           y=perc,
           label=perc))+
  geom_bar(stat="identity", fill="gray") +
  coord_flip() + geom_text() +
  ggtitle("Adjusted R^2 of 62 variables in various models
          With and without outliers and high leverage points")

#####################################################################
######## Exploratory analysis
#####################################################################

# Looking at SalePrice
boxplot(clean_df4$SalePrice, ylab= 'SalesPrice', main='Box Plot of SalesPrice')

hist(clean_df4$SalePrice,main = "Histogram of SalesPrice", xlab = 'SalesPrice')

summary(clean_df4$SalePrice)

clean_df4

# shows all "NA" values
as.data.frame(sapply(X = clean_df4,
                     FUN = function(x) sum(is.na(x))))

as.data.frame(sapply(X = clean_df4,
                     FUN = function(x) sum(x=="NA")))

#Forward Stepwise Regression
FitALL<-lm(clean_df4$SalePrice~.,data=clean_df4)
FitStart<-lm(clean_df4$SalePrice~1, data=clean_df4)
step(FitStart, direction = "forward", scope=formula(FitALL))

summary(lm(formula = clean_df4$SalePrice ~ 
             OverallQual + GrLivArea + Neighborhood + 
             BsmtFinSF1 + MSSubClass + SaleCondition + 
             OverallCond + BsmtExposure + YearBuilt_decade + 
             GarageCars + Functional + TotalBsmtSF + LotArea + 
             SaleType + KitchenQual + Exterior1st + BsmtQual + 
             KitchenAbvGr + WoodDeckSF + Condition1 + 
             HeatingQC + BsmtFinType1 + X3SsnPorch + 
             FireplaceQu + LandContour + BsmtUnfSF + 
             LotConfig + ExterCond + CentralAir + 
             MasVnrArea + OpenPorchSF + 
             GarageYrBlt_decade, 
           data = clean_df4))

anova(lm(formula = clean_df4$SalePrice ~ OverallQual + 
           GrLivArea + Neighborhood + BsmtFinSF1 + 
           MSSubClass + SaleCondition + OverallCond + 
           BsmtExposure + YearBuilt_decade + GarageCars + 
           Functional + TotalBsmtSF + LotArea + 
           SaleType + KitchenQual + Exterior1st + 
           BsmtQual + KitchenAbvGr + WoodDeckSF + 
           Condition1 + HeatingQC + BsmtFinType1 + 
           X3SsnPorch + FireplaceQu + LandContour + 
           BsmtUnfSF + LotConfig + ExterCond + 
           CentralAir + MasVnrArea + OpenPorchSF + 
           GarageYrBlt_decade, 
         data = clean_df4))

#First attempt eliminating predictor variables analyzing P-values
summary(lm(formula = clean_df4$SalePrice ~ OverallQual + 
             GrLivArea + Neighborhood + BsmtFinSF1 + 
             MSSubClass + SaleCondition + OverallCond + 
             BsmtExposure + YearBuilt_decade + GarageCars + 
             Functional + TotalBsmtSF + LotArea + 
             SaleType + KitchenQual + Exterior1st + 
             BsmtQual + KitchenAbvGr + WoodDeckSF + 
             Condition1 + HeatingQC + BsmtFinType1 + 
             X3SsnPorch + LotConfig + CentralAir + 
             GarageYrBlt_decade, 
           data = clean_df4))

anova(lm(formula = clean_df4$SalePrice ~ OverallQual + 
           GrLivArea + Neighborhood + BsmtFinSF1 + 
           MSSubClass + SaleCondition + OverallCond + 
           BsmtExposure + YearBuilt_decade + GarageCars + 
           Functional + TotalBsmtSF + LotArea + 
           SaleType + KitchenQual + Exterior1st + 
           BsmtQual + KitchenAbvGr + WoodDeckSF + 
           Condition1 + HeatingQC + BsmtFinType1 + 
           X3SsnPorch + LotConfig + CentralAir + 
           GarageYrBlt_decade, 
         data = clean_df4))

#Second attempt eliminating predictor variables analyzing P-values
summary(lm(formula = clean_df4$SalePrice ~ OverallQual + 
             GrLivArea + Neighborhood + BsmtFinSF1 + 
             MSSubClass + SaleCondition + OverallCond + 
             BsmtExposure + YearBuilt_decade + GarageCars + 
             Functional + TotalBsmtSF + LotArea + 
             SaleType + KitchenQual + Exterior1st + 
             BsmtQual + KitchenAbvGr + WoodDeckSF + 
             Condition1 + HeatingQC + CentralAir + 
             GarageYrBlt_decade, 
           data = clean_df4))

anova(lm(formula = clean_df4$SalePrice ~ OverallQual + 
           GrLivArea + Neighborhood + BsmtFinSF1 + 
           MSSubClass + SaleCondition + OverallCond + 
           BsmtExposure + YearBuilt_decade + GarageCars + 
           Functional + TotalBsmtSF + LotArea + 
           SaleType + KitchenQual + Exterior1st + 
           BsmtQual + KitchenAbvGr + WoodDeckSF + 
           Condition1 + HeatingQC + CentralAir + 
           GarageYrBlt_decade, 
         data = clean_df4))

#Last attempt eliminating predictor variables analyzing P-values
summary(lm(formula = clean_df4$SalePrice ~ OverallQual + 
             GrLivArea + Neighborhood + BsmtFinSF1 + 
             SaleCondition + OverallCond + BsmtExposure + 
             YearBuilt_decade + GarageCars + Functional + 
             TotalBsmtSF + LotArea + SaleType + 
             KitchenQual + Exterior1st + BsmtQual + 
             KitchenAbvGr + WoodDeckSF + Condition1 + 
             HeatingQC, 
           data = clean_df4))

anova(lm(formula = clean_df4$SalePrice ~ OverallQual + 
           GrLivArea + Neighborhood + BsmtFinSF1 + 
           SaleCondition + OverallCond + BsmtExposure + 
           YearBuilt_decade + GarageCars + Functional + 
           TotalBsmtSF + LotArea + SaleType + 
           KitchenQual + Exterior1st + BsmtQual + 
           KitchenAbvGr + WoodDeckSF + Condition1 + 
           HeatingQC, 
         data = clean_df4))

#Checking multicollinearity
cor_plot_items <-
  clean_df4 %>% 
  dplyr::select(LotArea, BsmtFinSF1, TotalBsmtSF,
                GrLivArea,WoodDeckSF,SalePrice)

lm_cor_plot_items <-lm(data=cor_plot_items, 
                       SalePrice~.)

vif_cor_plot_items <-vif(lm_cor_plot_items)

pairs(cor_plot_items)

cor(cor_plot_items)

#Further analysis of this model final model with 17 variables

summary(lm(formula = clean_df4$SalePrice ~ OverallQual + 
             GrLivArea + Neighborhood + OverallCond + 
             BsmtExposure + YearBuilt_decade + GarageCars + 
             Functional + TotalBsmtSF + SaleCondition + 
             KitchenQual + Exterior1st + BsmtQual + 
             KitchenAbvGr + WoodDeckSF + Condition1 + 
             HeatingQC, 
           data = clean_df4))

anova(lm(formula = clean_df4$SalePrice ~ OverallQual + 
           GrLivArea + Neighborhood + OverallCond + 
           BsmtExposure + YearBuilt_decade + GarageCars + 
           Functional + TotalBsmtSF + SaleCondition + 
           KitchenQual + Exterior1st + BsmtQual + 
           KitchenAbvGr + WoodDeckSF + Condition1 + 
           HeatingQC, 
         data = clean_df4))

# Final model using this method
summary(lm(formula = clean_df4$SalePrice ~ OverallQual + 
             GrLivArea + Neighborhood + OverallCond + 
             BsmtExposure + YearBuilt_decade + GarageCars + 
             Functional + TotalBsmtSF + SaleCondition + 
             KitchenQual + Exterior1st + BsmtQual + 
             KitchenAbvGr + WoodDeckSF + Condition1, 
           data = clean_df4))

anova(lm(formula = clean_df4$SalePrice ~ OverallQual + 
           GrLivArea + Neighborhood + OverallCond + 
           BsmtExposure + YearBuilt_decade + GarageCars + 
           Functional + TotalBsmtSF + SaleCondition + 
           KitchenQual + Exterior1st + BsmtQual + 
           KitchenAbvGr + WoodDeckSF + Condition1, 
         data = clean_df4))

#Model Validation plots
residuals<-
  residuals(lm(SalePrice~GrLivArea + 
                 Functional + Neighborhood + GarageCars + 
                 SaleType + OverallQual + OverallCond + 
                 LotArea + TotalBsmtSF, 
               data = clean_df4))

par(mfrow=c(2,2)) 
plot(lm(residuals~clean_df4$SalePrice))

############################################
### Model Matrices for Lasso, Ridge ########
############################################

# create 60/40 cross-validation set

set.seed(2020)

samples=sample(nrow(clean_df4), 
               0.6*nrow(clean_df4))
train = clean_df4[samples, ]
test = clean_df4[-samples,]
ytest = clean_df4$SalePrice

dim(clean_df4)

# producing model matrices for ridge,lasso models
x_matrix <- model.matrix(SalePrice~.,
                         clean_df4)
y <- clean_df4$SalePrice
ytrain1 = y[samples]
ytest1 = y[-samples]

lambda <- 10^seq(10,-2,length=100)  

############################################
############# Ridge Regression #############
############################################

# set alpha=0 for ridge regression, alpha=1 for lasso regression
ridge_mod <- 
  glmnet::glmnet(x_matrix[samples,],
                 y[samples],alpha=0,
                 lambda=lambda,
                 scale=TRUE)

set.seed(2020)
cv_ridge_mod <- 
  cv.glmnet(x_matrix[samples,],y[samples],alpha=0,
            scale=TRUE, nfolds=10)
cv_ridge_mod # shows lambda, other values
bestlam_ridge <- cv_ridge_mod$lambda.min
bestlam_ridge # best lambda is 16,257

plot(cv_ridge_mod,main="Ridge Regression")
r_squared_ridge=1-cv_ridge_mod$cvm/var(y[samples])

r_squared_ridge_max <- 
  max(1-cv_ridge_mod$cvm/var(y[samples]))
r_squared_ridge_max # 90.37%

plot(cv_ridge_mod$lambda,r_squared_ridge, 
     main=expression(R^{2}~"values for different lambda parameters in Ridge Regression Model"))

plot(ridge_mod,xvar="lambda",
     main="
     Coefficients of predictors in Ridge Regression model
     
     ")

plot(ridge_mod, main="Ridge Regression,
     Relationship of L1 Norm and Coefficients")

ridge_modBest <- 
  glmnet(x_matrix[samples,],
         y[samples],
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
          newx=x_matrix[-samples,],
          scale=TRUE)

# test MSE=599,633,800
mean((ridge_pred_Best-ytest1)^2) 
# test MSE with only Y intercept=5,813,413,964
mean((mean(y[samples])-ytest1)^2) 

# show coeficients of all predictors (zero and non-zero)
ridge_coef_matrix <- as.data.frame(as.matrix(ridge_pred_Best))
row.names(lasso_coef_matrix)

############################################
############# Lasso Regression #############
############################################

# set alpha=0 for ridge regression, alpha=1 for lasso regression
lasso_mod <- glmnet(x_matrix[samples,],
                    y[samples],alpha=1,
                    lambda=lambda,
                    scale=TRUE)

# As the lambda value increases, the coefficients get closer to zero.
predict(lasso_mod, s=.1, type='coefficients')
predict(lasso_mod, s=1, type='coefficients')
predict(lasso_mod, s=10, type='coefficients')
predict(lasso_mod, s=100, type='coefficients')

set.seed(2020)
cv_lasso_mod <- 
  cv.glmnet(x_matrix[samples,],y[samples],alpha=1,
            scale=TRUE, nfolds=10)
cv_lasso_mod # shows lambda, other values
bestlam_lasso <- cv_lasso_mod$lambda.min
bestlam_lasso # best lambda is 508.142

plot(cv_lasso_mod,main="Lasso Regression")
r_squared_lasso=1-cv_lasso_mod$cvm/var(y[samples])

r_squared_lasso_max <- 
  max(1-cv_lasso_mod$cvm/var(y[samples]))
r_squared_lasso_max # 90.76%

plot(cv_lasso_mod$lambda,r_squared_lasso, 
     main=expression(R^{2}~"values for different lambda parameters in Lasso Regression Model"))

plot(lasso_mod,xvar="lambda",
     main="
     Coefficients of predictors in Lasso Regression model
     
     ")

plot(lasso_mod, main="Lasso Regression,
     Relationship of L1 Norm and Coefficients")

lasso_modBest <- glmnet(x_matrix[samples,],
                        y[samples],
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
          newx=x_matrix[-samples,],
          scale=TRUE)

# test MSE=516,962,202
mean((lasso_pred_Best-ytest1)^2) 
# test MSE with only Y intercept=5,813,413,964
mean((mean(y[samples])-ytest1)^2) 

# show coeficients of all predictors (zero and non-zero)
lasso_coef_matrix <- as.data.frame(as.matrix(lasso_mod_coefs_Best))
row.names(lasso_coef_matrix)

lasso_df2 <-
  lasso_coef_matrix %>% 
  mutate(predictor = row.names(lasso_coef_matrix)) %>% 
  dplyr::select(predictor, "1")

lasso_df2_nonzero_coefs <-
  lasso_df2 %>% 
  rename(coefs ="1") %>% 
  filter(coefs!=0)

write.xlsx(lasso_df2,
           "first_lasso_coefs_from_clean_df4_A1.xlsx")
write.xlsx(lasso_df2_nonzero_coefs,
           "first_lasso_nonzero_coefs_from_clean_df4_A1.xlsx")

################################################################
##### Relaxed lasso: re-fit lasso with only non-zero variables
##### If at least one of the level is non-zero, include variable
################################################################

# read in the columns with non-zero coefficients (after lasso regression)
nonzero_coefs <-
  read_excel("nonzero_coef_lasso_clean_df4.xlsx",
             sheet = "first_lasso_coefs_all")
#### Need to add SalePrice

clean_df4_nonzero_coefs <-
  clean_df4 %>% 
  dplyr::select(c(nonzero_coefs$NonZero_Coefficients))

############################################
### Model Matrices for Relaxed Lasso ########
############################################

# create 60/40 cross-validation set

set.seed(2020)

samples=sample(nrow(clean_df4_nonzero_coefs), 
               0.6*nrow(clean_df4_nonzero_coefs))
train = clean_df4_nonzero_coefs[samples, ]
test = clean_df4_nonzero_coefs[-samples,]
ytest = clean_df4_nonzero_coefs$SalePrice

dim(clean_df4)
ytrain1 = y[samples]
ytest1 = y[-samples]


# producing model matrices for ridge,lasso models
x_matrix <- model.matrix(SalePrice~.,
                         clean_df4_nonzero_coefs)
y <- clean_df4_nonzero_coefs$SalePrice

lambda <- 10^seq(10,-2,length=100)  

############################################
############# Relaxed Lasso Regression 
############################################

# set alpha=0 for ridge regression, alpha=1 for lasso regression
lasso_mod <- 
  glmnet(x_matrix[samples,],
         y[samples],alpha=1,lambda=lambda,
         scale=TRUE)
# As the lambda value increases, the coefficients get closer to zero.
predict(lasso_mod, s=.1, type='coefficients')
predict(lasso_mod, s=1, type='coefficients')
predict(lasso_mod, s=10, type='coefficients')
predict(lasso_mod, s=100, type='coefficients')

cv_lasso_mod <- 
  cv.glmnet(x_matrix[samples,],y[samples],alpha=1,
            scale=TRUE, nfolds=10)
cv_lasso_mod # shows lambda, other values
bestlam_lasso <- cv_lasso_mod$lambda.min
bestlam_lasso # best lambda is 508.14

plot(cv_lasso_mod,main="Lasso Regression")
r_squared_lasso=1-cv_lasso_mod$cvm/var(y[samples])
r_squared_lasso_max <- 
  max(1-cv_lasso_mod$cvm/var(y[samples]))
r_squared_lasso_max # 90.96%
# adj$r2 is 81.5 with coefs greater than 3.8k
# adj$r2 is 87.5 with coefs greater than 1k
# adj$r2 is 90.8 with ALL coefs

plot(cv_lasso_mod$lambda,r_squared_lasso, 
     main=expression(R^{2}~"values for different lambda parameters in Lasso Regression Model"))

plot(lasso_mod,xvar="lambda",
     main="
     Coefficients of predictors in Lasso Regression model
     
     ")

plot(lasso_mod, main="Lasso Regression,
     Relationship of L1 Norm and Coefficients")

lasso_modBest <- glmnet(x_matrix[samples,],
                        y[samples],
                        alpha=1,
                        lambda=bestlam_lasso,
                        scale=TRUE)

lasso_mod_coefs_Best <-
  predict(lasso_modBest,
          type="coefficients",s=bestlam_lasso,
          scale=TRUE)

lasso_pred_Best <- 
  predict(lasso_modBest,
          s=bestlam_lasso,newx=x_matrix[-samples,],
          scale=TRUE)


# test MSE=517,879,541 with all coefs
mean((lasso_pred_Best-ytest1)^2) 
# test MSE with only Y intercept=5,813,413,964
mean((mean(y[samples])-ytest1)^2) 

### Reducing the number of non-zero coefficients leads to increasing MSE
# test MSE=1,226,027,911 with coefs greater than 5k/3.8k
# test MSE=866,225,628 with coefs greater than 1k

# show coeficients of all predictors (zero and non-zero)
lasso_coef_matrix <- as.data.frame(as.matrix(lasso_mod_coefs_Best))
row.names(lasso_coef_matrix)

lasso_df2 <-
  lasso_coef_matrix %>% 
  mutate(predictor = row.names(lasso_coef_matrix)) %>% 
  select(predictor, "1")


lasso_df2_nonzero_coefs <-
  lasso_df2 %>% 
  rename(coefs ="1") %>% 
  filter(coefs!=0.00000)

# View(lasso_df2)
write.xlsx(lasso_df2,
           "relaxed_lasso_allCoefs.xlsx")

#### Use this for interpretation and deciding which variables to keep!!!
write.xlsx(lasso_df2_nonzero_coefs,
           "relaxed_lasso_nonzero.xlsx")

#### We retain the same variables from relaxed lasso

######################### Linear model with nonzero coefficients
lm_nonzero <- 
  lm(data=clean_df4_nonzero_coefs,
     SalePrice~.)
summary(lm_nonzero)

vif(lm_nonzero)
# mean squared error
anova(lm_nonzero)['Residuals', 'Mean Sq']
### MSE is 407,943,035

################################################################
##### Reading in non-zero variables from relaxed lasso regression
################################################################
nonzero_coefs <-
  read_excel("nonzero_coef_lasso_clean_df4.xlsx",
             sheet = "Sheet1")

clean_df4_nonzero_coefs <-
  clean_df4b %>% 
  select(c(nonzero_coefs$NonZero_Coefficients))

summary(clean_df4_nonzero_coefs)

################################################################
##### Random forest
################################################################

# 50 Non-zero variables for random forest
dim(clean_df4_nonzero_coefs)

memory.limit()
memory.limit(100000)
names(clean_df4_nonzero_coefs) # find index of SalePrice

set.seed(2020)
rf_nonzero_coefs_postLasso <- 
  randomForest(y=clean_df4_nonzero_coefs$SalePrice,
               x=clean_df4_nonzero_coefs[,-50],
               ntree=500,
               norm.votes=FALSE, 
               do.trace=10,
               importance=TRUE)

rf_nonzero_coefs_postLasso
# MSE 608,244,593
# Variance explained 88.81%
importance(rf_nonzero_coefs_postLasso)
varImpPlot(rf_nonzero_coefs_postLasso)

rf_nonzero_coefs_postLasso_predict <-
  predict(rf_nonzero_coefs_postLasso,
          newdata=test)
rf_nonzero_coefs_postLasso_predict

plot(rf_nonzero_coefs_postLasso_predict)
(rf_nonzero_coefs_postLasso_predict - test$SalePrice) / 
  rf_nonzero_coefs_postLasso_predict

bind_RF_predict_test <- 
  as.data.frame(cbind(rf_nonzero_coefs_postLasso_predict,
                      test$SalePrice))

################################################################
##### Random forest
################################################################

# 62 variables for random forest (zero and non-zero)
dim(clean_df4)

memory.limit()
memory.limit(100000)

names(clean_df4) # find index of SalePrice
rf_nonzero_coefs_postLasso <- 
  randomForest(y=clean_df4$SalePrice,
               x=clean_df4[,-58],
               ntree=500,
               norm.votes=FALSE, 
               do.trace=10,
               importance=TRUE)

rf_nonzero_coefs_postLasso
### with all vars var explained is 88.45%
# MSE is 627,688,525

importance(rf_nonzero_coefs_postLasso,  main="Random forest model using 62 variables, MSE as trees increase")
varImpPlot(rf_nonzero_coefs_postLasso, main="Random forest model using 62 variables, Variable Importance Plot")

rf_nonzero_coefs_postLasso_predict <-
  predict(rf_nonzero_coefs_postLasso,
          newdata=test)
rf_nonzero_coefs_postLasso_predict
plot(rf_nonzero_coefs_postLasso_predict)
(rf_nonzero_coefs_postLasso_predict - test$SalePrice) / rf_nonzero_coefs_postLasso_predict

bind_RF_predict_test <- 
  as.data.frame(cbind(rf_nonzero_coefs_postLasso_predict,
                      test$SalePrice))

################################################################
##### Final models: cross-validation in linear regression 
##### Using regular coefficients
################################################################

clean_df4_9vars <-
  clean_df4 %>% 
  dplyr::select(SalePrice, GarageCars, Neighborhood, OverallQual, OverallCond, 
         Functional, LotArea, SaleType, TotalBsmtSF, GrLivArea)

# Define training control
set.seed(2020) 
train.control <- trainControl(method = "cv", number = 10)

# Train the model
model <- train(SalePrice~., 
               data = clean_df4_9vars, method = "lm",
               trControl = train.control)
summary(model)

predictions <- model %>% predict(test)
data.frame(R2 = R2(predictions, test$SalePrice),
           RMSE = RMSE(predictions, test$SalePrice),
           MAE = MAE(predictions, test$SalePrice))

# Adjusted R^2 is 0.8876
summary(model)$adj

# RMSE is 24817.13
24817.13^2
# MSE is 615,889,941
615889941

# bias
mean(predictions - test$SalePrice)
# -150.6951
median(predictions - test$SalePrice)
# 230.3316

# max deviation
max(abs(predictions - test$SalePrice))
# 115611.1

# max deviation
max(predictions - test$SalePrice)
# 115611.1


# mean absolute deviation
mean(abs(predictions - test$SalePrice))
# 18262.73
median(abs(predictions - test$SalePrice))
# 13820.66

# mean squared error
mean((predictions - test$SalePrice)^2)
# 615,890,127

################################################################
##### Final models: cross-validation in linear regression 
##### Using standardized coefficients
################################################################

################# standarize Cross validation Linear regression 

# without standardizing SalePrice, GarageCars (tried, doesn't help..)
clean_df4_9vars_scaled <-
  clean_df4 %>%
  dplyr::select(SalePrice, GarageCars, Neighborhood, OverallQual, OverallCond, 
         Functional, LotArea, SaleType, TotalBsmtSF, GrLivArea) %>%
  mutate(LotArea = (LotArea-mean(LotArea))/sd(LotArea),
         TotalBsmtSF = (TotalBsmtSF-mean(TotalBsmtSF))/sd(TotalBsmtSF),
         GrLivArea = (GrLivArea-mean(GrLivArea))/sd(GrLivArea),
         SalePrice = (SalePrice-mean(SalePrice))/sd(SalePrice)) 

test_scaled <-
  test %>%
  dplyr::select(SalePrice, GarageCars, Neighborhood, OverallQual, OverallCond, 
                Functional, LotArea, SaleType, TotalBsmtSF, GrLivArea) %>%
  mutate(LotArea = (LotArea-mean(LotArea))/sd(LotArea),
         TotalBsmtSF = (TotalBsmtSF-mean(TotalBsmtSF))/sd(TotalBsmtSF),
         GrLivArea = (GrLivArea-mean(GrLivArea))/sd(GrLivArea),
         SalePrice = (SalePrice-mean(SalePrice))/sd(SalePrice)) 

# Define training control
set.seed(2020) 
train.control <- trainControl(method = "cv", number = 10)

# Train the model
model_std <- train(SalePrice~., 
                   data = clean_df4_9vars_scaled, 
                   method = "lm",
                   trControl = train.control)
# Summarize the results
summary(model_std)

predictions_std <- model_std %>% predict(test_scaled)
data.frame(R2 = R2(predictions_std, test_scaled$SalePrice),
           RMSE = RMSE(predictions_std, test_scaled$SalePrice),
           MAE = MAE(predictions_std, test_scaled$SalePrice))

# Adjusted R^2 is 0.8876
summary(model_std)$adj

## Need to convert back to normal coefficients for evaluation criteria

# RMSE is 24817.13
# 24817.13^2
# MSE is 615,889,941
# 615,889,941

# bias
mean(predictions_std - test_scaled$SalePrice)
# -150.6951
median(predictions_std - test_scaled$SalePrice)
# 230.3316

# max deviation
max(abs(predictions_std - test_scaled$SalePrice))
# 115611.1

# mean absolute deviation
mean(abs(predictions_std - test_scaled$SalePrice))
# 18262.73
median(abs(predictions_std - test_scaled$SalePrice))
# 13820.66

# mean squared error
mean((predictions_std - test_scaled$SalePrice)^2)
# 615,890,127

################################################################
##### Final models: cross-validation in random forest
##### Using normal coefficients (standardizing not necessary)
##### Using rpart() function for random forest
################################################################

# Cross validation random forest
numFolds <- trainControl(method = "cv", number = 10)

cpGrid <- expand.grid(.cp = seq(0.01, 0.5, 0.01))
train(SalePrice~., 
      data = clean_df4_9vars, 
      method = "rpart", 
      trControl = numFolds, 
      tuneGrid = cpGrid)
# final value for cp is 0.01

rv_CV_9vars <- 
  rpart::rpart(SalePrice~.,
               data = clean_df4_9vars, 
               method = "anova",
               cp = 0.01)
predictionCV <- predict(rv_CV_9vars, 
                        newdata = test, type = "vector")
summary(rv_CV_9vars)
# relative error is 1-R^2
# relative error is 0.2078753
# R^2 = 1-0.2078753 = 0.7921247


rsq.rpart(rv_CV_9vars)

tmp <- printcp(rv_CV_9vars)
rsq.val <- 1-tmp[,c(3,4)] 
rsq.val


# bias
mean(predictionCV - test$SalePrice)
# -82.00716
median(predictionCV - test$SalePrice)
# 1495.73

# max deviation
max(abs(predictionCV - test$SalePrice))
# 178233.5

# mean absolute deviation
mean(abs(predictionCV - test$SalePrice))
# 25845.86
median(abs(predictionCV - test$SalePrice))
# 18941.89

# mean squared error
mean((predictionCV - test$SalePrice)^2)
# 1,266,812,831

################################################################
##### Final models: cross-validation in random forest 
##### Using normal coefficients (standardizing not necessary)
##### Using randomforest() function for random forest
################################################################

memory.limit()
memory.limit(100000)

set.seed(2020)
rf_9vars <- 
  randomForest(y=clean_df4_9vars$SalePrice,
               x=clean_df4_9vars[,-1],
               ntree=500,
               norm.votes=FALSE, 
               do.trace=10,
               importance=TRUE)

plot(rf_9vars,main="Random Forest model using nine variables
     MSE as trees increase")

mean(rf_9vars$rsq)
# R^2 0.859

mean(rf_9vars$mse)
# mse 765,709,896

importance(rf_9vars)
varImpPlot(rf_9vars,main="Random Forest model using nine variables, Variable Importance Plot")

rf_9vars_predict <-
  predict(rf_9vars,newdata=test)
test

rf_9vars_predict
plot(rf_9vars_predict,test$SalePrice)

# bias
mean(rf_9vars_predict - test$SalePrice)
# -11298.37
median(rf_9vars_predict - test$SalePrice)
# -8516.72

# max deviation
max(abs(rf_9vars_predict - test$SalePrice))
# 268938.3

# mean absolute deviation
mean(abs(rf_9vars_predict - test$SalePrice))
# 23578.92
median(abs(rf_9vars_predict - test$SalePrice))
# 15882.81


# mean squared error
# sum of squared residuals divided by n
# part1 <-sum((rf_9vars_predict - test$SalePrice)^2)
part1 <-(sum(rf_9vars_predict - test$SalePrice)^2)
sum(rf_9vars_predict - test$SalePrice)^2 / 1470

part1/(1470)
# 74083105

# below.. formula is correct, but it's implemented differently
# 1,373,419,282
mean(rf_9vars_predict - test$SalePrice)^2

nrow(clean_df4_9vars)
nrow(test)
# rows: 1050+420

################################################################
##### Final models: Lasso regression
################################################################

############################################
### Model Matrices for Lasso, Ridge ########
############################################

# producing model matrices for ridge,lasso models
set.seed(2020)
lambda <- 10^seq(10,-2,length=100)  

clean_df4_9vars_x2 <-
  clean_df4_9vars %>% 
  mutate(dataset="train")

test_9vars <-
  test %>% 
  dplyr::select(SalePrice,GarageCars, Neighborhood, 
         OverallQual, OverallCond, 
         Functional, LotArea, SaleType, 
         TotalBsmtSF, GrLivArea) %>% 
  mutate(dataset="test")

train_test <- rbind(clean_df4_9vars_x2,test_9vars)
table(train_test$dataset)

train = train_test[1:1050, ]
test = train_test[1051:1470,]
ytest = train_test$SalePrice

dim(clean_df4_9vars)

x_matrix <- model.matrix(SalePrice~.,
                         train_test)
y <- train_test$SalePrice
ytrain1 = y[1:1050]
ytest1 = y[1051:1470]


###########################################
############# Lasso Regression #############
############################################

# set alpha=0 for ridge regression, alpha=1 for lasso regression
lasso_mod <- 
  glmnet::glmnet(x_matrix[1:1050,],
                 y[1:1050],alpha=1,
                 lambda=lambda,
                 scale=TRUE)

# As the lambda value increases, the coefficients get closer to zero.
predict(lasso_mod, s=.1, type='coefficients')
predict(lasso_mod, s=1, type='coefficients')
predict(lasso_mod, s=10, type='coefficients')
predict(lasso_mod, s=100, type='coefficients')

set.seed(2020)
cv_lasso_mod <- 
  cv.glmnet(x_matrix[1:1050,],y[1:1050],alpha=1,
            scale=TRUE, nfolds=10)
cv_lasso_mod # shows lambda, other values
bestlam_lasso <- cv_lasso_mod$lambda.min
bestlam_lasso # best lambda is 0.00685

plot(cv_lasso_mod,main="Lasso Regression")
r_squared_lasso=1-cv_lasso_mod$cvm/var(y[1:1050])
r_squared_lasso_max <- 
  max(1-cv_lasso_mod$cvm/var(y[1:1050]))
r_squared_lasso_max # 88.21%

plot(cv_lasso_mod$lambda,r_squared_lasso, 
     main=expression(R^{2}~"values for different lambda parameters in Lasso Regression Model"))

plot(lasso_mod,xvar="lambda",
     main="
     Coefficients of predictors in Lasso Regression model
     
     ")

plot(lasso_mod, main="Lasso Regression,
     Relationship of L1 Norm and Coefficients")

lasso_modBest <- 
  glmnet(x_matrix[1:1050,],
         y[1:1050],
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
          newx=x_matrix[1051:1470,],
          scale=TRUE)

# test MSE = 621,324,415
mean((lasso_pred_Best-ytest1)^2) 
# train MSE with only Y intercept=5,751,023,863
mean((mean(y[1051:1470])-ytest1)^2) 

# bias
mean(lasso_pred_Best - test$SalePrice)
# 761.6966
median(lasso_pred_Best - test$SalePrice)
# 51.35832

# max deviation
max(abs(lasso_pred_Best - test$SalePrice))
# 150183.3

# mean absolute deviation
mean(abs(lasso_pred_Best - test$SalePrice))
# 17382.87
median(abs(lasso_pred_Best - test$SalePrice))
# 12683.46

mean((lasso_pred_Best - test$SalePrice)^2)
# 602,170,342

############################################
############# Ridge Regression #############
############################################

# set alpha=0 for ridge regression, alpha=1 for lasso regression
ridge_mod <- 
  glmnet(x_matrix[1:1050,],
         y[1:1050],alpha=0,
         lambda=lambda,
         scale=TRUE)

?cv.glmnet
set.seed(2020)
cv_ridge_mod <- 
  cv.glmnet(x_matrix[1:1050,],y[1:1050],alpha=0,
            scale=TRUE, standardize=TRUE, nfolds=10)
cv_ridge_mod # shows lambda, other values
bestlam_ridge <- cv_ridge_mod$lambda.min
bestlam_ridge # best lambda 

# plot(cv_ridge_mod,main="Ridge Regression")
r_squared_ridge=1-cv_ridge_mod$cvm/var(y[1:1050])
r_squared_ridge_max <- 
  max(1-cv_ridge_mod$cvm/var(y[1:1050]))
r_squared_ridge_max # 88.02%

# plot(cv_ridge_mod$lambda,r_squared_ridge, 
#      main=expression(R^{2}~"values for different lambda parameters in Lasso Regression Model"))

# plot(lasso_mod,xvar="lambda",
#      main="
#      Coefficients of predictors in Lasso Regression model
#      
#      ")
# 
# plot(lasso_mod, main="Lasso Regression,
#      Relationship of L1 Norm and Coefficients")

ridge_modBest <- 
  glmnet(x_matrix[1:1050,],
         y[1:1050],
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
          newx=x_matrix[1051:1470,],
          scale=TRUE)

# ridge test MSE = 629,478,270
mean((ridge_pred_Best-test$SalePrice)^2) 
# train MSE with only Y intercept=5,751,023,863
mean((mean(y[1051:1470])-ytest1)^2) 

# bias
mean(ridge_pred_Best - test$SalePrice)
# 697.1072
median(ridge_pred_Best - test$SalePrice)
# -48.93018

# max deviation
max(abs(ridge_pred_Best - test$SalePrice))
# 150997.5

# mean absolute deviation
mean(abs(ridge_pred_Best - test$SalePrice))
# 17349.72
median(abs(ridge_pred_Best - test$SalePrice))
# 12651.14

mean((ridge_pred_Best - test$SalePrice)^2)
# 607,638,193

####################################################################=
##### Checking for multicollinearity
####################################################################=

######################### Linear model with nonzero coefficients
# 62 variables
lm_full <- 
  lm(data=clean_df4,
     SalePrice~.)
plot(lm_full, 
     main="Residual plots of 62 variable MLR")

# 15 variables
lm_15vars <- 
  lm(data=clean_df4,
     SalePrice~GarageCars+Neighborhood+OverallQual+OverallCond+
       Functional+LotArea+SaleType+TotalBsmtSF+GrLivArea+
       BldgType+KitchenQual+KitchenAbvGr+SaleCondition+Condition1)
summary(lm_15vars)
vif(lm_15vars)

# 9 variables
lm_9vars <- 
  lm(data=clean_df4,
     SalePrice~GarageCars+Neighborhood+OverallQual+OverallCond+
       Functional+LotArea+SaleType+TotalBsmtSF+GrLivArea)
plot(lm_9vars,
     main="Residual plots of nine variable MLR")
summary(lm_9vars)
vif(lm_9vars)

anova(lm_9vars,lm_15vars)
# mean squared error

anova(lm_full)['Residuals', 'Mean Sq']
anova(lm_15vars)['Residuals', 'Mean Sq']
anova(lm_9vars)['Residuals', 'Mean Sq']

summary(lm_full)$adj
summary(lm_15vars)$adj
summary(lm_9vars)$adj

clean_df4_9vars <-
  clean_df4 %>% 
  dplyr::select(SalePrice, GarageCars, Neighborhood, 
                OverallQual, OverallCond,Functional, 
                LotArea, SaleType, TotalBsmtSF, GrLivArea)

lm_9vars <- 
  lm(data=clean_df4_9vars,SalePrice~.)
summary(lm_9vars)

vif_table <- as.data.frame(car::vif(lm_9vars))

vif_table_x2 <-
  vif_table %>% 
  rename(GVIF_modified = "GVIF^(1/(2*Df))") %>% 
  mutate(predictor = row.names(vif_table),
         vif_new = GVIF_modified^2) %>% 
  dplyr::select(predictor, GVIF, Df, GVIF_modified,vif_new) %>%
  arrange(desc(vif_new))
# vif_table_x2

plot(vif_table_x2$predictor,
     vif_table_x2$vif_new)

ggplot(data=vif_table_x2, 
          aes(x=predictor, y=vif_new)) +
  geom_bar(stat="identity") + 
  geom_text(vjust=-0.2,
            label=round(vif_table_x2$vif_new,1))

### Breush-Pagan test for constant error variance
lmtest::bptest(SalePrice~.,
               data=clean_df4_9vars,
               studentize=TRUE)
bptest(lm_9vars)
plot(lm_9vars)
### We reject the null hypothesis of constant error variance
### Should address this

lm_9vars_logY <- 
  lm(data=clean_df4_9vars,log(SalePrice)~.)
summary(lm_9vars_logY)
bptest(lm_9vars_logY)
# Still have heteroscedasticity after transforming SalePrice