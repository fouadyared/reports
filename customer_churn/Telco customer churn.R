###############################
# survival project

# telco is fictional!
## below: data dictionary
# https://community.ibm.com/community/user/businessanalytics/blogs/steven-macko/2019/07/11/telco-customer-churn-1113
# Multiple Lines: Indicates if the customer subscribes to multiple telephone lines with the company: Yes, No
# helpful guide: https://www.drizopoulos.com/courses/emc/ep03_%20survival%20analysis%20in%20r%20companion
# library(devtools)
library(ggfortify)
library(rcompanion)
library(data.table)
library(tidyverse)
library(survival)
library(survminer)
library(rcompanion)
library(pec)
library(GGally)
library(ggcorrplot)
library(vcd)
library(scales)
library(ggrepel)
library(gridExtra)
library(survMisc)
library(caret)
library(MASS)

setwd("C:/Users/fouad/Documents/misc/fouad/hunter/survival analysis self-study/project/fuller_telco_data/CSVs")

telco <- fread("Telco_customer_churn.csv")
telco_x1 <- fread("Telco_customer_churn_demographics.csv")
telco_x2 <- fread("Telco_customer_churn_location.csv")
telco_x3 <- fread("Telco_customer_churn_population.csv")
telco_x4 <- fread("Telco_customer_churn_services.csv")
telco_x5 <- fread("Telco_customer_churn_status.csv")

nrow(telco)
# remove spaces from column names
colnames(telco) <- gsub(" ", "_", colnames(telco))
colnames(telco_x1) <- gsub(" ", "_", colnames(telco_x1))
colnames(telco_x2) <- gsub(" ", "_", colnames(telco_x2))
colnames(telco_x3) <- gsub(" ", "_", colnames(telco_x3))
colnames(telco_x4) <- gsub(" ", "_", colnames(telco_x4))
colnames(telco_x5) <- gsub(" ", "_", colnames(telco_x5))

names(telco)
names(telco_x1)
names(telco_x2)
names(telco_x3)
names(telco_x4)
names(telco_x5)

# from telco_x1
# add under30, married, number of dependdents
# Married column exact same as Partner column. not included

# from telco_x2
# add nothing (all labels alredy included)

# from telco_x3
# add population, then standarize counts per zip

# from telco_x4
# add quarter, referred friend, number referrals, offer,
# long dist charges, internet type, gb download,
# streaming music, unlimited data, total refunds
# total data charge, total long dist charge
# total revenue

# from telco_x5
# add satis score, customer status (subset of churn_label)

table(telco_x5$Customer_Status)
table(telco_x5$Churn_Label)

table(telco_x4$Internet_Service)
table(telco_x4$Internet_Type)

# some relationship...
table(telco_full_x3$Paperless_Billing, 
      telco_full_x3$Payment_Method)

telco_full <-
  telco %>% 
  left_join(telco_x1[,c(1,5,9)], by=c("CustomerID"="Customer_ID")) %>% 
  left_join(telco_x3[,c(2,3)], by=c("Zip_Code"="Zip_Code")) %>% 
  left_join(telco_x4[,c(1,3,4,5,7,
                        9,12,13,
                        20,21,27,
                        28,29,30)], 
            by=c("CustomerID"="Customer_ID")) %>% 
  left_join(telco_x5[,c(1,4,5)], by=c("CustomerID"="Customer_ID"))

write.csv(telco_full, "telco_full.csv")
summary(telco_full)
table(telco_full$Churn_Value)
table(telco_full$Churn_Label)
# 0 no churn, 1 is churn

telco_full_x2 <-
  telco_full %>% 
  mutate(Churn_recode = 
           ifelse(Churn_Label=="Yes", 0, 1))
table(telco_full_x2$Churn_Value2,
      telco_full_x2$Churn_Value)

telco_full_x2$Contract

table(telco_full_x3$Satisfaction_Score_Group)
telco_full_x3 <-
  telco_full_x2 %>% 
  mutate(
    Zip_Code = as.character(Zip_Code),
    Lat_Long = as.character(Lat_Long),
    Latitude = as.character(Latitude),
    Longitude = as.character(Longitude),
    Population = as.numeric(gsub(",","",Population)),
    Satisfaction_Score = factor(Satisfaction_Score, 
                                order(levels = c(1,2,3,4,5))),
    Monthly_Charges_Group = 
      case_when(
        Monthly_Charges>=0 & Monthly_Charges<=35 ~"$0-35",
        Monthly_Charges>35 & Monthly_Charges<=70 ~">$35-70",
        Monthly_Charges>70 & Monthly_Charges<=90 ~">$70-90",
        Monthly_Charges>90 & Monthly_Charges<=120 ~">$90-120"
        # Monthly_Charges>=0 & Monthly_Charges<=30 ~"_$0-30",
        # Monthly_Charges>30 & Monthly_Charges<=60 ~">$30-60",
        # Monthly_Charges>60 & Monthly_Charges<=90 ~">$60-90",
        # Monthly_Charges>90 & Monthly_Charges<=120 ~">$90-120"  
      ), 
    
    Number_of_Dependents_Group = 
      case_when(
        Number_of_Dependents==0 ~ Number_of_Dependents,
        Number_of_Dependents==1  ~ Number_of_Dependents,
        Number_of_Dependents==2 ~ Number_of_Dependents,
        Number_of_Dependents==3  ~ Number_of_Dependents,
        Number_of_Dependents>=4 & Number_of_Dependents<=9 ~4L
      ) , 
    Number_of_Referrals_Group = 
      case_when(
        Number_of_Referrals>=0 & Number_of_Referrals<=10 ~Number_of_Referrals,
        Number_of_Referrals>=10 & Number_of_Referrals<=11 ~10L
      ), 
    Total_Charges_Present = 
      ifelse(is.na(Total_Charges), 
             Monthly_Charges,
             Total_Charges + Monthly_Charges),
    
    Satisfaction_Score_Group =
      fct_collapse(Satisfaction_Score,
                   "Rated 3, 4, or 5" = c("3", "4", "5"),
                   "Rated 1 or 2" = c("1", "2")
      )
      )

telco_full_x3X <-
  telco_full_x3 %>% 
  mutate(Tenure_Months_Group = 
           case_when(
             Tenure_Months>=0 & Tenure_Months<=12 ~"1) 0-12 months",
             Tenure_Months>=13 & Tenure_Months<=24 ~"2) 13-24 months",
             Tenure_Months>=25 & Tenure_Months<=36 ~"3) 25-36 months",
             Tenure_Months>=37 & Tenure_Months<=48 ~"4) 37-48 months",
             Tenure_Months>=49 & Tenure_Months<=60 ~"5) 49-60 months",
             Tenure_Months>=61 & Tenure_Months<=72 ~"6) 61-72 months"
  )) %>% 
  group_by(Churn_Label, Tenure_Months_Group) %>% 
  summarize(mean_monthly_charges = mean(Monthly_Charges), 
            median_monthly_charges = median(Monthly_Charges),
            count=n())
  
ggplot(data = telco_full_x3X, 
          aes(x=Tenure_Months_Group, y=round(median_monthly_charges, 0), 
              group = Churn_Label, fill=as.factor(Churn_Label))) +
  geom_bar(stat="identity", position="dodge") +
  geom_text(aes(label=scales::comma(round(median_monthly_charges,0))), vjust=-0.4, 
              position = position_dodge(width = 1)) +
  geom_text(aes(label=scales::comma(round(median_monthly_charges,0))), vjust=-0.4, 
            position = position_dodge(width = 1)) +
  ggtitle("Median monthly charges by Tenure Length and Churn Status
           Top figures: Median monthly charges (in $), Bottom figures: Counts per subgroup") +
  theme(legend.position="bottom")


# exploring the data
table(telco_full_x3$Churn_Label)
prop.table(table(telco_full_x3$Churn_Value))

a<-ggplot(data=telco_full_x3, aes(Churn_Label)) +
  geom_bar(fill="#87CEEB") +
  geom_text(stat="count", 
            aes(label=scales::comma(..count..)), 
            vjust=-0.5) +
  ggtitle("Customer Churn in Current Quarter")

b<-ggplot(data=telco_full_x3, aes(Tenure_Months)) +
  geom_bar(fill="#87CEEB") +
  # geom_text(stat="count", 
  #           aes(label=scales::comma(..count..)), 
  #           vjust=-3,
  #           position=position_jitter(width=3,height=5)) +
  geom_text_repel(stat="count", 
                  aes(x=Tenure_Months,
                      # y=..count..,
                      label=scales::comma(..count..))) +
  ggtitle("Length of time (in months) as customer")

grid.arrange(a,b, top="Churn and Tenure with company")

###

# below: histogram code (works, doesn't look good here)
# ggplot(data=telco_full_x3, aes(Tenure_Months)) +
#   geom_histogram(fill="#87CEEB") +
#   # geom_text(stat="count", 
#   #           aes(label=scales::comma(..count..)), vjust=-3) +
#   geom_text_repel(stat="count", 
#                   aes(x=Tenure_Months,
#                       # y=..count..,
#                       label=scales::comma(..count..))) +
#   ggtitle("Length of time (in months) as customer")

### EDA of Demographics
d<-ggplot(data=telco_full_x3, aes(Senior_Citizen)) +
  geom_bar(fill="#FFA500") +
  geom_text(stat="count", 
            aes(label=scales::comma(..count..)), vjust=-0.05) +
  ggtitle("Customer is 65 years old or older")

e<-ggplot(data=telco_full_x3, aes(Partner)) +
  geom_bar(fill="#FFA500") +
  geom_text(stat="count", 
            aes(label=scales::comma(..count..)), vjust=-0.05) +
  ggtitle("Customer is married or has a partner")

f<-ggplot(data=telco_full_x3, aes(Dependents)) +
  geom_bar(fill="#FFA500") +
  geom_text(stat="count", 
            aes(label=scales::comma(..count..)), vjust=-0.05) +
  ggtitle("Customer lives with dependents (children, parents, grandparents, etc)")

grid.arrange(d,e,f, top="Customer Demographics")

# ggplot(data=telco_full_x3, aes(Under_30)) +
#   geom_bar(fill="#013220") +
#   geom_text(stat="count", 
#             aes(label=scales::comma(..count..)), vjust=-1) +
#   ggtitle("Customer Satisfaction with company (1 is Very Unsatisfied, 5 is Very Satisfied)")

## Details on Customer
c<-ggplot(data=telco_full_x3, aes(Satisfaction_Score)) +
  geom_bar(fill="#B19CD9") +
  geom_text(stat="count", 
            aes(label=scales::comma(..count..)), vjust=-0.5) +
  ggtitle("Customer Satisfaction with company 
          (1 is Very Unsatisfied, 5 is Very Satisfied)")

g<-ggplot(data=telco_full_x3, aes(Contract)) +
  geom_bar(fill="#B19CD9") +
  geom_text(stat="count", 
            aes(label=scales::comma(..count..)), vjust=-0.5) +
  ggtitle("Type of contract")

h<-ggplot(data=telco_full_x3, aes(Payment_Method)) +
  geom_bar(fill="#B19CD9") +
  geom_text(stat="count",
            aes(label=scales::comma(..count..)), vjust=-0.5) +
  ggtitle("Type of payment method")

i<-ggplot(data=telco_full_x3, aes(Offer)) +
  geom_bar(fill="#B19CD9") +
  geom_text(stat="count", 
            aes(label=scales::comma(..count..)), vjust=-0.5) +
  ggtitle("Last marketing offer customer accepted")

grid.arrange(c,g,h,i, top="Customer Details")

# ggplot(data=telco_full_x3, aes(Paperless_Billing)) +
#   geom_bar(fill="#013220") +
#   geom_text(stat="count", 
#             aes(label=scales::comma(..count..)), vjust=-1) +
#   ggtitle("Whether cusomter uses paperless billing")

# ggplot(data=telco_full_x3, aes(Referred_a_Friend)) +
#   geom_bar(fill="#013220") +
#   geom_text(stat="count", 
#             aes(label=scales::comma(..count..)), vjust=-1) +
#   ggtitle("Whether customer ever referred a friend or family member to company")

### Services
j<-ggplot(data=telco_full_x3, aes(Phone_Service)) +
  geom_bar(fill="#90EE90") +
  geom_text(stat="count", 
            aes(label=scales::comma(..count..)), vjust=-0.05) +
  ggtitle("Type of Phone service subscribed to with company")

k<-ggplot(data=telco_full_x3, aes(Internet_Type)) +
  geom_bar(fill="#90EE90") +
  geom_text(stat="count", 
            aes(label=scales::comma(..count..)), vjust=-0.5) +
  ggtitle("Type of Internet service subscribed to with company")

grid.arrange(j,k, top="Services")

### value
l<-ggplot(data=telco_full_x3, aes(Total_Charges_Present)) +
  geom_histogram(fill="#FFCCBB") +
  ggtitle("Customer's Total Charges (actual; includes current quarter)")

m<-ggplot(data=telco_full_x3, aes(CLTV)) +
  geom_histogram(fill="#FFCCBB") +
  ggtitle("Customer Lifetime Value (predicted; higher the value, more valuable the customer)")

grid.arrange(l,m, top="Customer Value")


names(telco_full_x3)

table(telco_full_x3$Satisfaction_Score_Group)
cor(((telco_full_x3$Tenure_Months +1) * telco_full_x3$Monthly_Charges),
    telco_full_x3$Total_Charges_Present)
# 99.95% correlation btwn tenure*months and total charges

summary(telco_full_x3$Total_Refunds)
summary(telco_full_x3$Total_Extra_Data_Charges)
summary(telco_full_x3$Total_Long_Distance_Charges)
summary(telco_full_x3$Total_Revenue)
cor(telco_full_x3$Total_Revenue, telco_full_x3$CLTV)
# 0.3541981
cor(telco_full_x3$Total_Charges_Present, telco_full_x3$CLTV)
# 0.3404417

summary(telco_full_x3$Total_Charges_Present)
telco_full_x4 <-
  telco_full_x3 %>% 
  mutate(estimated_total = Monthly_Charges*Tenure_Months,
         diff_total = estimated_total - Total_Charges, 
         diff_perc = 100*(diff_total/Total_Charges)) %>% 
  select(Monthly_Charges, Total_Charges, estimated_total, diff_total, diff_perc) %>% 
  arrange(desc(diff_total))

telco_full_x4B <-
  telco_full_x3 %>% 
  filter(is.na(Total_Charges)) %>% 
  select(Monthly_Charges, Tenure_Months)

telco_full_x4C <-
  telco_full_x3 %>% 
  filter(Tenure_Months==1) %>% 
  select(Monthly_Charges, Tenure_Months, Total_Charges)

# Total_Charges are missing for those who are new to Telco
table(telco_full_x3$Tenure_Months)

# fix will be to add monthly_charges to total_charges
# so it would be up to that month

View(telco_full_x4)
summary(telco_full_x4$diff_total)
quantile(telco_full_x4$diff_total, na.rm=TRUE)
quantile(telco_full_x4$diff_perc, na.rm=TRUE)

plot(quantile(telco_full_x3$Monthly_Charges))

telco_full_x3A <-
  telco_full_x3 %>% 
  select_if(is.character) %>% 
  select(-c(CustomerID, Lat_Long, Latitude, Longitude, Zip_Code))

telco_full_x3B <-
  telco_full_x3 %>% 
  select_if(is.numeric) %>% 
  select(-Count, -Churn_recode, -Total_Charges) %>% 
  select(Churn_Value, everything())

corr1 <- cor(telco_full_x3B, method="pearson")

# below: works
# # Churn_Value should be excluded from corr plot
# GGally::ggcorr(telco_full_x3B, 
#        method = c("everything", "pearson"),
#        digits = 3, 
#        label = TRUE,
#        label_color = "black",
#        label_alpha = TRUE,
#        hjust = 1.1, vjust = .1,
#        # angle = 90,
#        layout.exp = 7)

ggcorrplot(corr1, 
           hc.order=TRUE,
           type="lower",
           lab=TRUE
)

sapply(telco_full_x3A, table)
table(telco_full_x3$Partner)
table(telco_full_x3$Dependents)
table(telco_full_x3$Phone_Service)
table(telco_full_x3$MultipleLines)

names(telco_full_x3A)
names(telco_full_x3)

# labs <- round(prop.table(Titanic), 2)
# mosaic(Titanic, pop = FALSE)
# labeling_cells(text = labs, margin = 0)(Titanic)

tbl_x1 <- xtabs(~Satisfaction_Score + Churn_Label, 
                telco_full_x3)
ftable(tbl_x1)
vcd::mosaic(tbl_x1, main="Mosaic Plot")

# below: works, shows labels for percents of total 
# prop.table(tbl_x1)/tbl_x1
# labs <- round(prop.table(tbl_x1), 2)
# mosaic(tbl_x1, pop = FALSE)
# labeling_cells(text = labs, margin = 0)(tbl_x1)

# below: works, shows labels for percents of row 
labs <- round(prop.table(tbl_x1, margin=1)*100, 2)
mosaic(tbl_x1, pop = FALSE)
labeling_cells(text = labs, margin = 0)(tbl_x1)

tbl_x2 <- xtabs(~Phone_Service + Churn_Label, 
                telco_full_x3)
ftable(tbl_x2)
vcd::mosaic(tbl_x2, main="Mosaic Plot")

tbl_x3 <- xtabs(~Contract + Churn_Label, 
                telco_full_x3)
ftable(tbl_x3)
vcd::mosaic(tbl_x3, main="Mosaic Plot")
labs <- round(prop.table(tbl_x3, margin=1)*100, 2)
mosaic(tbl_x3, pop = FALSE)
labeling_cells(text = labs, margin = 0)(tbl_x3)

tbl_x4 <- xtabs(~Payment_Method + Churn_Label, 
                telco_full_x3)
ftable(tbl_x4)
vcd::mosaic(tbl_x4, main="Mosaic Plot")

tbl_x5 <- xtabs(~Offer + Churn_Label,
                telco_full_x3)
ftable(tbl_x5)
vcd::mosaic(tbl_x5, main="Mosaic Plot")
labs <- round(prop.table(tbl_x5, margin=1)*100, 2)
mosaic(tbl_x5, pop = FALSE)
labeling_cells(text = labs, margin = 0)(tbl_x5)


tbl_x6 <- xtabs(~Senior_Citizen + Churn_Label, 
                telco_full_x3)
ftable(tbl_x6)
vcd::mosaic(tbl_x6, main="Mosaic Plot")

tbl_x7 <- xtabs(~Partner + Churn_Label, 
                telco_full_x3)
ftable(tbl_x7)
vcd::mosaic(tbl_x7, main="Mosaic Plot")

names(telco_full_x3)
tbl_x8 <- xtabs(~Online_Security + Internet_Service, 
                telco_full_x3)
ftable(tbl_x8)
vcd::mosaic(tbl_x8, main="Mosaic Plot")

labs <- round(prop.table(tbl_x8, margin=1)*100, 2)
mosaic(tbl_x8, pop = FALSE)
labeling_cells(text = labs, margin = 0)(tbl_x8)
##############
# nominal correlation
sapply(telco_full_x3A, table)
names(telco_full_x3A)
cramerV(telco_full_x3$Churn_Label, 
        telco_full_x3$Gender, bias.correct = TRUE)
cramerV(telco_full_x3$Senior_Citizen, 
        telco_full_x3$Churn_Label, bias.correct = TRUE)
cramerV(telco_full_x3$Partner, 
        telco_full_x3$Churn_Label, bias.correct = TRUE)
cramerV(telco_full_x3$Dependents, 
        telco_full_x3$Churn_Label, bias.correct = TRUE)
cramerV(telco_full_x3$Phone_Service, 
        telco_full_x3$Churn_Label, bias.correct = TRUE)
cramerV(telco_full_x3$Paperless_Billing, 
        telco_full_x3$Churn_Label, bias.correct = TRUE)
cramerV(telco_full_x3$Under_30, 
        telco_full_x3$Churn_Label, bias.correct = TRUE)
cramerV(telco_full_x3$Unlimited_Data, 
        telco_full_x3$Churn_Label, bias.correct = TRUE)
cramerV(telco_full_x3$Referred_a_Friend, 
        telco_full_x3$Churn_Label, bias.correct = TRUE)
cramerV(telco_full_x3$Streaming_Music, 
        telco_full_x3$Churn_Label, bias.correct = TRUE)
cramerV(telco_full_x3$Churn_Label, 
        telco_full_x3$Churn_Reason!="", bias.correct = TRUE)
table(telco_full_x3$Online_Security)

cv_df <- fread("Churn_Label_CramerV.csv")


str(cv_df)

cramerV(telco_full_x3$Phone_Service, 
        telco_full_x3$Multiple_Lines != "Yes", bias.correct = TRUE)
cramerV(telco_full_x3$Internet_Service, 
        telco_full_x3$Multiple_Lines != "Yes", bias.correct = TRUE)

cramerV(telco_full_x3$Internet_Service == "No", 
        telco_full_x3$Online_Security == "No", bias.correct = TRUE)
cramerV(telco_full_x3$Streaming_Movies == "No internet service", 
        telco_full_x3$Online_Security == "No internet service", bias.correct = TRUE)

cramerV(telco_full_x3$Internet_Service == "No", 
        telco_full_x3$Online_Security == "No internet service", bias.correct = TRUE)
cramerV(telco_full_x3$Internet_Service == "DSL", 
        telco_full_x3$Online_Security == "No internet service", bias.correct = TRUE)

cramerV(telco_full_x3$Senior_Citizen, 
        telco_full_x3$Partner, bias.correct = TRUE)
cramerV(telco_full_x3$Partner, 
        telco_full_x3$Churn_Label, bias.correct = TRUE)
cramerV(telco_full_x3$Dependents, 
        telco_full_x3$Churn_Label, bias.correct = TRUE)
cramerV(telco_full_x3$Phone_Service, 
        telco_full_x3$Churn_Label, bias.correct = TRUE)
cramerV(telco_full_x3$Paperless_Billing, 
        telco_full_x3$Churn_Label, bias.correct = TRUE)
cramerV(telco_full_x3$Under_30, 
        telco_full_x3$Churn_Label, bias.correct = TRUE)
cramerV(telco_full_x3$Unlimited_Data, 
        telco_full_x3$Churn_Label, bias.correct = TRUE)
cramerV(telco_full_x3$Referred_a_Friend, 
        telco_full_x3$Churn_Label, bias.correct = TRUE)
cramerV(telco_full_x3$Streaming_Music, 
        telco_full_x3$Churn_Label, bias.correct = TRUE)

cramerV(telco_full_x3$Senior_Citizen, 
        telco_full_x3$Partner)
cramerV(telco_full_x3$Partner, 
        telco_full_x3$Dependents)
fisher.test(telco_full_x3$Partner, 
            telco_full_x3$Dependents)

cramerV(telco_full_x3$Senior_Citizen, 
        telco_full_x3$Satisfaction_Score,
        bias.correct = TRUE)

# fisher's test looks at null hypot of rows and colums 
?fisher.test 

fisher.test(telco_full_x3$Partner, 
            telco_full_x3$Dependents)
# since p-value is less than .05, we reject null hypoth
# that there's a rlshp btwn Partner and Dependents

table(telco_full_x3$Phone_Service, 
      telco_full_x3$MultipleLines)
fisher.test(x=telco_full_x3$Phone_Service, 
            y=telco_full_x3$MultipleLines)

fisher.test(telco_full_x3$PhoneService, 
            telco_full_x3$MultipleLines,
            telco_full_x3$InternetService)
names(telco_full_x3A)

# cramerV only works for 2x2 matrices
cramerV(telco_full_x3$Under_30, 
        telco_full_x3$Senior_Citizen)

cramerV(telco_full_x3$Under_30, 
        telco_full_x3$Paperless_Billing)

# cramerV is 1
table(telco_full_x3$PhoneService)
table(telco_full_x3$MultipleLines)

### Survival plots

###
fit_All <- survfit(Surv(time = Tenure_Months, 
                        Churn_Value) ~ 
                     1, 
                   data = telco_full_x3)

ggsurvplot(fit_All, 
           pval = TRUE, 
           conf.int = TRUE,
           risk.table = TRUE, 
           risktable.col = "strata", 
           linetype = "strata", 
           surv.median.line = "hv",
           ggtheme = theme_bw(),
           palette = c("#E7B800", "#2E9FDF", "#330066")) + 
  ggtitle("Survival Plot of Telco Customers by Contract Length")


###

fit_x1 <- survfit(Surv(time = Tenure_Months, 
                       Churn_Value) ~ 
                    Contract, 
                  data = telco_full_x3)

print(fit_x1)
summary(fit_x1)

ggsurvplot(fit_x1, 
           pval = TRUE, 
           conf.int = TRUE,
           risk.table = TRUE, 
           risktable.col = "strata", 
           linetype = "strata", 
           surv.median.line = "hv",
           ggtheme = theme_bw(),
           palette = c("#E7B800", "#2E9FDF", "#330066")) + 
  ggtitle("Survival Plot of Telco Customers by Contract Length")

# log-rank tests
survdiff(Surv(time = Tenure_Months, 
              Churn_Value) ~ 
           Contract, 
         data = telco_full_x3)

###
fit_x2 <- survfit(Surv(time = Tenure_Months, 
                       Churn_Value) ~ 
                    Satisfaction_Score, 
                  # as.character(Satisfaction_Score),
                  data = telco_full_x3)

ggsurvplot(fit_x2, 
           pval = TRUE, 
           conf.int = TRUE,
           risk.table = TRUE, 
           risktable.col = "strata", 
           linetype = "strata", 
           surv.median.line = "hv",
           ggtheme = theme_bw(),
           palette = c("#E7B800", "#2E9FDF", "#330066", 
                       "#E7B8FF", "#2E9F33")) + 
  ggtitle("Survival Plot of Telco Customers by Satisfaction Score")

survdiff(Surv(time = Tenure_Months, 
              Churn_recode) ~ 
           Satisfaction_Score, 
         data = telco_full_x3)

# cumulative hazard plot
# we want the plots to grow away from each other over time
plot(fit_x2, fun="cumhaz", main="cum haz")

plot(KM_fit, xlab = "Years", ylab = "Survival", col = 1:2)
legend("topright", levels(pbc2.id$sex), lty = 1, col = 1:2, bty = "n")

# plotting -log(log) function ph assumptions
# plot shows assumption may not hold...
plot(fit_x2, fun = function (s) -log(-log(s)), xlab = "Years", 
     ylab = "-log(- log(Survival))", col = 1:2)

# plot(KM_fit, xlab = "Years", ylab = "Survival", col = 1:2)
# legend("topright", levels(pbc2.id$sex), lty = 1, col = 1:2, bty = "n")
# 
# plot(KM_fit, fun = function (s) -log(-log(s)), xlab = "Years", 
#      ylab = "-log(- log(Survival))", col = 1:2)

resids <- 
  (log(fit_weib$y[, 1]) - fitted_values) / fit_weib$scale

fitted_values <- fit_weib$linear.predictors
resids <- (log(fit_weib$y[, 1]) - fitted_values) / fit_weib$scale
###
fit_x3 <- survfit(Surv(time = Tenure_Months, 
                       Churn_Value) ~ 
                    Offer, 
                  data = telco_full_x3)
ggsurvplot(fit_x3, 
           pval = TRUE, 
           conf.int = TRUE,
           risk.table = TRUE, 
           risktable.col = "strata", 
           linetype = "strata", 
           surv.median.line = "hv",
           ggtheme = theme_bw(),
           palette = c("#E7B800", "#2E9F99", "#330066", 
                       "#E7B8FF", "#2E9F33", "#330099")) + 
  ggtitle("Survival Plot of Telco Customers by Offer Accepted")

survdiff(Surv(time = Tenure_Months, 
              Churn_recode) ~ 
           Offer, 
         data = telco_full_x3)
table(telco_full_x3$Offer)
###
# MonthlyCharges
summary(telco_full_x3$Monthly_Charges)
quintile(telco_full_x3$Monthly_Charges)

fit_x4 <- survfit(Surv(time = Tenure_Months, 
                       Churn_Value) ~ 
                    Monthly_Charges_Group, 
                  data = telco_full_x3)
ggsurvplot(fit_x4, 
           pval = TRUE, 
           conf.int = TRUE,
           risk.table = TRUE, 
           risktable.col = "strata", 
           linetype = "strata", 
           surv.median.line = "hv",
           ggtheme = theme_bw(),
           palette = c("#E7B800", "#2E9FDF", "#330066", 
                       "#E7B8FF", "#2E9F33")) + 
  ggtitle("Survival Plot of Telco Customers by Contract Length")

survdiff(Surv(time = Tenure_Months, 
              Churn_recode) ~ 
           Payment_Method, 
         data = telco_full_x3)

table(telco_full_x3$Satisfaction_Score_Group)
# Releveling the variables so hazard ratios are mostly greater than 1
telco_full_x3 <- 
  telco_full_x3 %>% 
  mutate(Contract =  relevel(as.factor(Contract), ref = "Two year"),
         Payment_Method =  relevel(as.factor(Payment_Method), ref = "Credit card (automatic)"),
         Offer =  relevel(as.factor(Offer), ref = "Offer A"),
         Satisfaction_Score_Group =  relevel(as.factor(Satisfaction_Score_Group), ref = "Rated 3, 4, or 5"))
nrow(telco_full_x3)
# 100% of those who scored telco a 1,2 left. 
# 100% of those who scored telco a 4,5 stayed.
# most who said 3 stayed.
table(telco_full_x3$Satisfaction_Score, 
      telco_full_x3$Churn_Value)

names(telco_full_x3)
lapply(telco_full_x3, table)
table(telco_full_x3$Referred_a_Friend, telco_full_x3$Number_of_Referrals_Group)
table(telco_full_x3$Dependents, telco_full_x3$Number_of_Dependents_Group)
table(telco_full_x3$Payment_Method)
### Cox Proportional Hazard plots
cox1 <- coxph(Surv(time = Tenure_Months, 
                   Churn_Value) ~ 
                Senior_Citizen + 
                Partner + 
                Under_30 +
                Gender + 
                Contract + 
                Paperless_Billing +
                Payment_Method +
                Number_of_Dependents_Group +
                Number_of_Referrals_Group + 
                Offer + 
                Monthly_Charges + 
                Satisfaction_Score_Group,
              data = telco_full_x3)

cox1
summary(cox1)
survfit(cox1)
ggsurvplot(survfit(cox1),
           palette = "#2E9FDF",
           ggtheme = theme_minimal(),
           data = telco_full_x3)

gof(cox1)
table(telco_full_x3$Payment_Method)
# cox2 only includes variables chosen after survival var selection with selectCox()
cox2 <- coxph(Surv(time = Tenure_Months, 
                   Churn_Value) ~ 
                # Senior_Citizen + 
                # Partner + 
                # Under_30 +
                # Gender + 
                Contract + 
                # Paperless_Billing +
                Payment_Method +
                Number_of_Dependents_Group +
                Number_of_Referrals_Group +
                Offer +
                Monthly_Charges +
                Satisfaction_Score_Group,
              data = telco_full_x3)

cox2X <- coxph(Surv(time = Tenure_Months, 
                   Churn_Value) ~ 
                # Senior_Citizen + 
                # Partner + 
                # Under_30 +
                # Gender + 
                Contract + 
                # Paperless_Billing +
                Payment_Method +
                Number_of_Dependents_Group +
                Number_of_Referrals_Group +
                Monthly_Charges + 
                 strata(Offer) +
                Satisfaction_Score_Group,
              data = telco_full_x3)

summary(cox2)
# splits the output of cox model
cox2_outputA <- summary(cox2)$coefficients
confint(cox2)
cox2_outputB <- exp(confint(cox2))
palette = 
summary(cox2X)
cox.zph(cox2X)

ggsurvplot(survfit(cox2), 
           palette = c("#E7B800", "#2E9FDF", "#330066", 
                       "#C3CDE6","#E7B8FF", "#2E9F33"),
           ggtheme = theme_minimal(),
           data = telco_full_x3)
ggsurvplot(survfit(cox2X), 
           palette = c("#E7B800", "#2E9FDF", "#330066", 
                       "#C3CDE6","#E7B8FF", "#2E9F33"),
           ggtheme = theme_minimal(),
           data = telco_full_x3)
anova(cox2)

# without Monthly_Charges_Group
write.csv(cox2_outputA, "C:/Users/fouad/Documents/misc/fouad/hunter/survival analysis self-study/project/fuller_telco_data/cox2_outputA.csv")
write.csv(cox2_outputB, "C:/Users/fouad/Documents/misc/fouad/hunter/survival analysis self-study/project/fuller_telco_data/cox2_outputB.csv")

# with Monthly_Charges_Group
write.csv(cox2_outputA, "C:/Users/fouad/Documents/misc/fouad/hunter/survival analysis self-study/project/fuller_telco_data/cox2_monthly_outputA.csv")
write.csv(cox2_outputB, "C:/Users/fouad/Documents/misc/fouad/hunter/survival analysis self-study/project/fuller_telco_data/cox2_monthly_outputB.csv")


cox2$coefficients
exp(cox2$coefficients)
?coxph


ggsurvplot(survfit(cox2), palette = "#2E9FDF",
           ggtheme = theme_minimal(),
           data = telco_full_x3,main="dd")
anova(cox2)

# cox3 only includes variables chosen from stepwise logistic regression model
cox3 <- coxph(Surv(time = Tenure_Months,
                   Churn_Value) ~
                Contract + 
                Payment_Method +
                Offer + 
                Satisfaction_Score_Group,
              data = telco_full_x3)
summary(cox3)
cox3$coefficients
anova(cox3)
anova(cox2, cox1)
anova(cox3, cox2)
# listing models smallest to largest

# Comparing cox1 and cox2, 
# Since the p-value for the log-lik test is >= 0.05
# we prefer to use the reduced model (cox2)

# Comparing cox2 and cox3, 
# Since the p-value for the log-lik test is < 0.05
# we prefer to use the fuller model (cox2)

# use fitted values to predict...
# plot residuals
plot(cox3$residuals)
mean(cox3$residuals)
# page 47
### need to check proportional hazard assumptions
# cox.zph() works on non-singular models
?cox.zph()
# null hypoth: variable satistifies PH
# most vars in model are less than 0.05...
zp2 <- cox.zph(cox2, transform = "km")
zp2$table
write.csv(zp2$table, "C:/Users/fouad/Documents/misc/fouad/hunter/survival analysis self-study/project/fuller_telco_data/zp2.csv")
# below: plots scaled Schoenfeld residuals against transformed time
ggcoxzph(zp2)
ggcoxdiagnostics(cox2, type = "schoenfeld", linear.predictions = FALSE)
ggcoxdiagnostics(cox2, type = "dfbeta", linear.predictions = FALSE)
ggcoxdiagnostics(cox2, type = "deviance", linear.predictions = FALSE)

telco_full_x3F <-
  telco_full_x3 %>% 
  select(Churn_Value)
# deviance plot shows lots of deviation for first 1869 records (where churn_value is 1) 

zp3 <- cox.zph(cox3, transform = "km")
zp3
# below: plots scaled Schoenfeld residuals against transformed time
ggcoxzph(zp3)
ggcoxdiagnostics(cox3, type = "schoenfeld", linear.predictions = FALSE)
ggcoxdiagnostics(cox3, type = "dfbeta", linear.predictions = FALSE)
ggcoxdiagnostics(cox3, type = "deviance", linear.predictions = FALSE)
?ggcoxdiagnostics
# plotting -log(log) function ph assumptions
# plot shows assumption may not hold...
plot(survfit(cox3), fun = function (s) -log(-log(s)), xlab = "Years", 
     ylab = "-log(- log(Survival))", col = 9:10)
cox3
# checking linearity
# it's not linear


#### start of logistic regression section

### logistic regression
# split across stata
names(telco_full_x3)
telco_full_x3_LM <- 
  telco_full_x3 %>% 
  select(Contract,
         Payment_Method,
         # Dependents, # not helpful; removed
         # Referred_a_Friend, # not helpful; removed
         # Number_of_Dependents_Group, # not helpful; removed
         # Number_of_Referrals_Group, # not helpful; removed
         Offer,
         # Monthly_Charges, # not helpful; removed
         Satisfaction_Score_Group, 
         Churn_Value)

train_idx <-
  createDataPartition(telco_full_x3_LM$Churn_Value, 
                      times = 1, 
                      p = 0.75, list=F)
train_x2 = telco_full_x3_LM[train_idx, ]
test_x2 = telco_full_x3_LM[-train_idx,]

# producing model matrices for ridge,lasso models
x_matrix <- model.matrix(Churn_Value~.,
                         telco_full_x3_LM)

# x_matrix
y <- telco_full_x3_LM$Churn_Value
ytrain1 = y[train_idx]
ytest1 = y[-train_idx]

logistic_model_x1 <- 
  glm(Churn_Value ~., data = train_x2, family = binomial)
summary(logistic_model_x1)

# stepwise variable selection for logistic regression
step_logistic_model_x1 <-
  MASS::logistic_model_x1 %>% 
  stepAIC(trace=FALSE)

summary(step_logistic_model_x1)
# stepAIC removes #dependents, #referred, monthly charges
# only retains contract, payment method, offer, satisfaction score

# model <- glm( diabetes ~., data = train.data, family = binomial)
# Make predictions
probabilities <- 
  logistic_model_x1 %>% 
  predict(test_x2, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "1", "0")
# Model accuracy
mean(predicted.classes == test_x2$Churn_Value)
# 93.41% accuracy of logistic model (with #dependents, #referred)
# 94.37% accuracy of logistic model (without #dependents, #referred)
# 93.92% accuracy of logistic model (without #dependents, #referred, monthly charges)
table(predicted.classes, test_x2$Churn_Value)

#### end of logistic regression section


# test for non-linearity
# below: doesn't work
# ggcoxfunctional(cox2, telco_full_x3)
# ?ggcoxfunctional

plot(zp3[1], resid=TRUE)
plot(zp2[2], resid=TRUE)
abline(coef(cox3)[2],0,lty=3)
?cox.zph

ggsurvplot(survfit(cox2), palette = "#2E9FDF",
           ggtheme = theme_minimal(),
           data = telco_full_x3)

anova(cox2)
anova(cox1, cox2)
# anova shows how diff models are, if significant

# concordance is used to evaluate survival models
# it's used if censoring is present in a data set

# data(GBSG2)
table(telco_full_x3$Internet_Service)
table(telco_full_x3$Multiple_Lines)

# this does variable selection

f1 <- pec::selectCox(Surv(time = Tenure_Months, 
                          Churn_Value) ~
                       Senior_Citizen + 
                       Partner + 
                       Under_30 +
                       Gender + 
                       Contract + 
                       Paperless_Billing +
                       Payment_Method +
                       Number_of_Dependents_Group +
                       Number_of_Referrals_Group + 
                       Offer + 
                       Monthly_Charges + 
                       Satisfaction_Score_Group,
                     data = telco_full_x3,
                     rule = "aic")
f1

# 100% of Churn scores 5-64 stay
# 100% of Churn scores 81-100 stay
# mix for churn scores 65-80
## will exclude because likelihood to churn has some leakage
table(telco_full_x3$Churn_Score, 
      telco_full_x3$Churn_Value)
# ok to keep CLTV since it doesn't seem associated with Churn_Value
cor(telco_full_x3$CLTV, 
    telco_full_x3$Churn_Value)
# -0.1274631
cor(telco_full_x3$CLTV, 
    telco_full_x3$Total_Revenue)
# 0.3541981

# ggsurvplot(survfit(f1), palette = "#2E9FDF",
#            ggtheme = theme_minimal(),
#            data = telco_full_x3)

f1A <- selectCox(Surv(time = Tenure_Months, 
                      Churn_Value) ~
                   # Senior_Citizen + 
                   # Partner + 
                   # Under_30 +
                   # Gender + 
                   Contract + 
                   # Paperless_Billing +
                   Payment_Method +
                   Number_of_Dependents_Group +
                   Number_of_Referrals_Group + 
                   Offer +
                   Monthly_Charges +
                   Satisfaction_Score_Group,
                 data=telco_full_x3,
                 rule = "aic")
f1A

cox5 <- coxph(Surv(time = Tenure_Months, 
                   Churn_Value) ~ 
                Partner +
                Total_Refunds +
                strata(Senior_Citizen),
              data = telco_full_x3)
summary(cox5)
ggsurvplot(survfit(cox5), palette = c("#2E9FDF","#E7B800"),
           ggtheme = theme_minimal(),
           data = telco_full_x3)

# prop-hazard for first var: partner
plot(cox.zph(cox2), var=1)
abline(h = coef(cox2)[1], lty = 2, col = "red")
# prop-hazard for first var: total_refunds
plot(cox.zph(cox2), var=2)

anova(cox2)

# shows test for every var in model
anova(cox1)

# plotting -log(log) function ph assumptions
# plot shows assumption may not hold...
plot(survfit(cox3), fun = function (s) -log(-log(s)), xlab = "Years", 
     ylab = "-log(- log(Survival))", col = 9:10)
cox3
# checking linearity
# it's not linear
cox4 <- coxph(Surv(time = Tenure_Months, 
                   Churn_Value) ~ 
                pspline(Total_Charges) +
                Senior_Citizen + 
                Partner + 
                Phone_Service,
              data = telco_full_x3)

print(cox4, digits=2)
termplot(cox4, term=1, se=TRUE)

####################
#### function assumptions

# p123 in book (ch3)
# ph assumption requires HR is constant over time

# can stratify curves so diff models for
# vars that don't have an effect on our outcome
# or it doesn't mean PH assumption

# use log-likelihood test to compare models