library(survey)
library(data.table)
library(tidyverse)
library(jtools)

# https://stats.idre.ucla.edu/r/seminars/survey-data-analysis-with-r/

setwd("C:/Users/fouad/Documents/misc/fouad/hunter/stat 754 surveys/project/nhis")

adult <- 
  fread("nhis_sampleAdult_2019.csv")
names(adult)

adult_x1 <-
  adult %>% 
  mutate(NOTCOV_A2 = case_when(
    NOTCOV_A=="1" ~ "0", 
    NOTCOV_A=="2" ~ "1"
  ))

table(adult_x1$NOTCOV_A2)


adult_x2 <-
  adult %>% 
  filter(NOTCOV_A != "9")

nrow(adult)
nrow(adult_x2)

# create data frame with NHIS design information, using existing data frame of NHIS data
nhissvy <- svydesign(id=~PPSU, strata=~PSTRAT,
                     nest = TRUE,
                     weights=~WTFA_A,
                     data=adult)

# create data frame with NHIS design information, using existing data frame of NHIS data
nhissvy_x1 <- svydesign(id=~PPSU, strata=~PSTRAT,
                     nest = TRUE,
                     weights=~WTFA_A,
                     data=adult_x1)

# stats for continuous variables
svymean(~NOTCOV_A, design=nhissvy)
# mean 1.8833, SE 0.0032

svyvar(~NOTCOV_A, design=nhissvy, na=TRUE)
# var 0.10307, SE 0.0025

summary(nhissvy)
?svydesign

cv(svymean(~NOTCOV_A, design=nhissvy))
# coefficient of variation for mean

# stats for categorical variables
svytable(~NOTCOV_A, design=nhissvy)

# sex by not covered
svytable(~SEX_A+NOTCOV_A, design=nhissvy)

# race/ethn by not covered
svytable(~HISPALLP_A+NOTCOV_A, design=nhissvy)


# race/ethn by health status by not covered
svytable(~HISDETP_A+PHSTAT_A+NOTCOV_A, design=nhissvy)
svytable(~PHSTAT_A+NOTCOV_A, design=nhissvy)

# svyby gets retains records for SE, removes otherwise
# need to tinker with svyby()
svyby(formula=~NOTCOV_A, design=nhissvy, by= svymean)
svyby()

# here subset is used...
nhissvy_x2 <-
  subset(nhissvy, NOTCOV_A != "9")

nhissvy_x1x2 <-
  subset(nhissvy_x1, NOTCOV_A != "9")

summary(nhissvy_x2)

svytable(~NOTCOV_A, design=nhissvy_x2)

# ttest formula
# doesn't make sense with categorical data
svyttest(NOTCOV_A~0, nhissvy_x2, na=TRUE)
# looks at whether means of two groups is zero
svyttest(NOTCOV_A~SEX_A, nhissvy_x2, na=TRUE)

# glm on categorical survey data
summary(svyglm(NOTCOV_A~factor(SEX_A)*factor(HISPALLP_A),
               design=nhissvy_x2, na.action = na.omit))

summary(svyglm(NOTCOV_A~factor(SEX_A),
               design=nhissvy_x2, na.action = na.omit))

# not sure how to get it to work with categorical outcome
summary(svyglm(pad630~factor(female)*factor(dmdmartl),
               design=nhc, na.action = na.omit))

# logistic regression. need to have 0,1 values for Y
logit1 <- 
  (svyglm(NOTCOV_A~factor(SEX_A),
          family=quasibinomial, 
          design=nhissvy_x1x2, na.action = na.omit))
summary(logit1)



logit1 <- (svyglm(paq665~factor(hsd010)+ridageyr, family=quasibinomial, design=nhc, na.action = na.omit))
summary(logit1)

# calculating the deff (design effect)
svymean(~NOTCOV_A, 
        design=nhissvy_x2, 
        na=TRUE, deff=TRUE)
# DEFF calculated for continuous data, not categorical...
svyciprop(~I(NOTCOV_A==1), design=nhissvy_x2)

# chi-squared stat
svychisq(~NOTCOV_A + SEX_A, 
         design=nhissvy_x2, 
         statistic = "adjWald")

# calculate DEFF by hand.
# age of blood cancer
# out of pocket premium cost HICOSTR1_A

nhissvy_x2b <-
  subset(nhissvy, !NOTCOV_A %in% c("1", "9") & 
           !HICOSTR1_A %in% c("99997", "99999"))

nhissvy_x2b_emp <-
  subset(nhissvy, !NOTCOV_A %in% c("1", "9") & 
           !HICOSTR1_A %in% c("99997", "99999") &
           PLNWRKR1_A =="1")

nhissvy_x2b_purchDirectly <-
  subset(nhissvy, !NOTCOV_A %in% c("1", "9") & 
           !HICOSTR1_A %in% c("99997", "99999") &
           PLNWRKR1_A =="2")

nhissvy_x2b_ACA <-
  subset(nhissvy, !NOTCOV_A %in% c("1", "9") & 
           !HICOSTR1_A %in% c("99997", "99999") &
           PLNWRKR1_A =="3")

nhissvy_x2b_StateLocalPlan <-
  subset(nhissvy, !NOTCOV_A %in% c("1", "9") & 
           !HICOSTR1_A %in% c("99997", "99999") &
           PLNWRKR1_A =="4")

table(adult$PLNWRKR1_A)

barplt <- 
  svyby(~HICOSTR1_A,~PLNWRKR1_A, 
        design=nhissvy_x2b, 
        na=TRUE, svymean)
barplot(barplt, beside=TRUE, legend=TRUE)

# cost by type health insurance, 18 to 65
barplt2 <- 
  svyby(~HICOSTR1_A,~COVER_A, 
        design=nhissvy_x2b, 
        na=TRUE, svymean)
barplot(barplt2, beside=TRUE, legend=TRUE)

barplt3 <- 
  svyby(~HICOSTR1_A,~COVER65_A, 
        design=nhissvy_x2b, 
        na=TRUE, svymean)
barplot(barplt3, beside=TRUE, legend=TRUE)

# stats for continuous variables
svymean(~HICOSTR1_A, 
        design=nhissvy_x2b, 
        na=TRUE)

svyvar(~HICOSTR1_A, 
       design=nhissvy_x2b, 
       na=TRUE)

# rem. cv is coefficient of variation
cv(svymean(~HICOSTR1_A, 
                 design=nhissvy_x2b,
                 na=TRUE))

# calculating the deff (design effect)
svymean(~HICOSTR1_A, 
        design=nhissvy_x2b, 
        na=TRUE, deff=TRUE)

svyquantile(~HICOSTR1_A, 
            design = nhissvy_x2b, 
            na = TRUE, 
            c(.25,.5,.75),
            ci=TRUE)

svyhist(~HICOSTR1_A, 
        design = nhissvy_x2b, main=5)

svyboxplot(~HICOSTR1_A~1, 
           design = nhissvy_x2b,
           all.outliers=TRUE)

svyboxplot(~HICOSTR1_A~factor(SEX_A), 
           design = nhissvy_x2b,
           all.outliers=TRUE, na==T)

barplt <- 
  svyby(~HICOSTR1_A~factor(SEX_A))
# HICOSTR1_A for premiums. 
## Exclude those who refused, don't know
# NOTCOV_A for not covered.
## Exclude those who don't have insurance.

#### Avearge cost of HC Plan thru Employer
# stats for continuous variables
svymean(~HICOSTR1_A, 
        design=nhissvy_x2b_emp, 
        na=TRUE)

svyvar(~HICOSTR1_A, 
       design=nhissvy_x2b_emp, 
       na=TRUE)

# calculating the deff (design effect)
svymean(~HICOSTR1_A, 
        design=nhissvy_x2b_purchDirectly, 
        na=TRUE, deff=TRUE)

svyquantile(~HICOSTR1_A, 
            design = nhissvy_x2b_purchDirectly, 
            na = TRUE, 
            c(.25,.5,.75),
            ci=TRUE)

svyhist(~HICOSTR1_A, 
        design = nhissvy_x2b_purchDirectly)

#### Avearge cost of HC Plan, Purchased Directly
# stats for continuous variables
svymean(~HICOSTR1_A, 
        design=nhissvy_x2b_purchDirectly, 
        na=TRUE)

svyvar(~HICOSTR1_A, 
       design=nhissvy_x2b_purchDirectly, 
       na=TRUE)

# calculating the deff (design effect)
svymean(~HICOSTR1_A, 
        design=nhissvy_x2b_purchDirectly, 
        na=TRUE, deff=TRUE)

svyquantile(~HICOSTR1_A, 
            design = nhissvy_x2b_purchDirectly, 
            na = TRUE, 
            c(.25,.5,.75),
            ci=TRUE)

svyhist(~HICOSTR1_A, 
        design = nhissvy_x2b_purchDirectly)

#### Avearge cost of HC Plan, Aff Care Act
# stats for continuous variables
svymean(~HICOSTR1_A, 
        design=nhissvy_x2b_ACA, 
        na=TRUE)

svyvar(~HICOSTR1_A, 
       design=nhissvy_x2b_ACA, 
       na=TRUE)

# calculating the deff (design effect)
svymean(~HICOSTR1_A, 
        design=nhissvy_x2b_ACA, 
        na=TRUE, deff=TRUE)

svyquantile(~HICOSTR1_A, 
            design = nhissvy_x2b_ACA, 
            na = TRUE, 
            c(.25,.5,.75),
            ci=TRUE)

svyhist(~HICOSTR1_A, 
        design = nhissvy_x2b_ACA)

#### Avearge cost of State, Local plan
# stats for continuous variables
svymean(~HICOSTR1_A, 
        design=nhissvy_x2b_StateLocalPlan, 
        na=TRUE)

svyvar(~HICOSTR1_A, 
       design=nhissvy_x2b_StateLocalPlan, 
       na=TRUE)

# calculating the deff (design effect)
svymean(~HICOSTR1_A, 
        design=nhissvy_x2b_StateLocalPlan, 
        na=TRUE, deff=TRUE)

svyquantile(~HICOSTR1_A, 
            design = nhissvy_x2b_StateLocalPlan, 
            na = TRUE, 
            c(.25,.5,.75),
            ci=TRUE)

svyhist(~HICOSTR1_A, 
        design = nhissvy_x2b_StateLocalPlan)

svyboxplot(~HICOSTR1_A~1, 
        design = nhissvy_x2b_StateLocalPlan,
        all.outliers=TRUE)

########################
# create data frame with NHIS design information, using existing data frame of NHIS data
nhissvy <- svydesign(id=~PPSU, strata=~PSTRAT,
                     nest = TRUE,
                     weights=~WTFA_A,
                     data=adult)

# use subset() to retain SE values
nhissvy_x3 <-
  subset(nhissvy, 
         !AGEP_A %in% c("97","98","99"),
         !SEX_A %in% c("7","8","9"),
         !INCGRP_A %in% c("8"),
         !HISPALLP_A %in% c("7","8","9"),
         !EDUC_A %in% c("97","98","99"),
         !PHSTAT_A %in% c("7","8","9"),
         !HICOSTR1_A %in% c("99997","99998","99999"),
         !PRDEDUC1_A %in% c("7","8","9"),
         !PRHDHP1_A %in% c("7","8","9"),
         !RSNHICOST_A %in% c("7","8","9"),
         !PAYBLL12M_A  %in% c("7","8","9"),
         !PAYNOBLLNW_A %in% c("7","8","9"),
         !PAYWORRY_A %in% c("7","8","9"),
         !NOTCOV_A  %in% c("7","8","9")
         )

nhissvy_x3A <- 
  subset(nhissvy_x3, 
         !PRDEDUC1_A %in% c("1", "9") & 
           !PRHDHP1_A  %in% c("99997", "99999"))

nhissvy_x3B <- 
  subset(nhissvy_x3A, 
         !PRDEDUC1_A %in% c("7","8","9") & 
           !PRHDHP1_A  %in% c("7","8","9")
         )

str(adult$NOTCOV_A)

svytable(~NOTCOV_A, 
         design=nhissvy_x3)

a <- svytable(~NOTCOV_A, 
         design=nhissvy_x3)
a/sum(a)

svytable(~SEX_A+NOTCOV_A, 
         design=nhissvy_x3)
a2 <- svytable(~AGEP_A+NOTCOV_A, 
         design=nhissvy_x3)

svytable(~INCGRP_A+NOTCOV_A,
         design=nhissvy_x3)
svytable(~AGEP_A+INCGRP_A+NOTCOV_A,
         design=nhissvy_x3)

# 
svytable(~COVER_A,
         design=nhissvy_x3)
svytable(~COVER65_A ,
         design=nhissvy_x3)

# confidence intervals
svyciprop(~I(NOTCOV_A==1), design=nhissvy_x3)
svyciprop(~I(NOTCOV_A==2), design=nhissvy_x3)
svyciprop(~I(NOTCOV_A==9), design=nhissvy_x3)

# stats for continuous variables
# DEFF calculated for continuous data, not categorical...
# calculating the deff (design effect)
svymean(~HICOSTR1_A, 
        design=nhissvy_x3A, 
        na=TRUE, deff=TRUE)

svyvar(~HICOSTR1_A, 
       design=nhissvy_x3A, 
       na=TRUE)

svyquantile(~HICOSTR1_A, 
            design = nhissvy_x3A, 
            na = TRUE, 
            c(.25,.5,.75),
            ci=TRUE)

svyquantile(~HICOSTR1_A, 
            design = nhissvy_x3A, 
            na = TRUE, 
            c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1),
            ci=TRUE)

# cost by type health insurance, 18 to 65
barplt2 <- 
  svyby(~HICOSTR1_A, ~INCGRP_A, 
        design=nhissvy_x3A, 
        na=TRUE, svymean)

barplt2A <- 
  barplot(barplt2, beside=TRUE, 
        legend=TRUE, 
        main="Average Health Insurance Cost by Income Group")

barplt3 <- 
  svyby(~HICOSTR1_A,~COVER65_A, 
        design=nhissvy_x3A, 
        na=TRUE, svymean)

prem <-svysmooth(~HICOSTR1_A, 
          design=nhissvy_x3A)
plot(prem)

prem2 <-svysmooth(~INCGRP_A+HICOSTR1_A, 
                 design=nhissvy_x3A)
plot(prem2, 
     main = "Density Plot of Average Health Insurance Cost")

svytable(~PRDEDUC1_A,
         design=nhissvy_x3B)

svytable(~ PRHDHP1_A,
         design=nhissvy_x3B)
31997/1.8554
?forecast
