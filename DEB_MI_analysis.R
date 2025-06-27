## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
knitr::knit_hooks$set(purl = knitr::hook_purl)

#packages
#install.packages("foreign")
#install.packages("effects")
#install.packages("mediation")
#install.packages("bruceR")
#install.packages("naniar")
#install.packages("VIM")
library(VIM)
library(naniar)
library(bruceR)
library(foreign)
library(dplyr)
library(psych)
library(ggplot2)
library(car)
library(effects)
library(lavaan)
library(kableExtra)
library(broom)
library(tidyverse)
library(gridExtra)
library(mice)
library(miceadds)
library(finalfit)
library(mitml)


## ----include=FALSE------------------------------------------------------------
#load data
avon.data <- foreign::read.spss("Data_2.sav", to.data.frame = TRUE, reencode = TRUE)


## ----Create new table---------------------------------------------------------

# ccm991a = age
# kz021 = sex
# c645a = education
# c804 = ethnicity

data <- avon.data[, c("cidB3980","ccm991a", "kz021","c804", "a_happ_5g", "b_bore_5g", "c_reli_5g", "d_hope_5g","e_exci_5g", "f_fail_5g", "g_lone_5g", "h_sad_5g", "i_luck_5g", "j_rela_5g", "ccs5520", "ccs5530", "ccs5540", "ccs5541","ccs5510","ccs5511","ccs5512","ccs5513","ccs5550","ccs5560", "ccs3000","ccs3001", "ccs3002","ccs3003","ccs3004","ccs3005","ccs3006","ccs3007","ccs3008","ccs3009","ccs3010","ccs3011","cct4105","cct4106","cct4107","cct4108","cct4110","cct4112","cct4115","cct4116","cct4120","cct4125","ccs4500", "ccs4502", "ccs4503", "ccs4504", "ccs4505", "ccs4506", "ccs4508", "ccs4509","ccs4511", "ccs4512", "ccs4513", "ccs4514", "ccs4515","c645a","ccs5583")]

data <- rename(data, ID = cidB3980)
data <- rename(data, Sex = kz021)
data <- rename(data, Ethnicity = c804)
data <- rename(data, Age = ccm991a)




## ----Filter by AMS and score--------------------------------------------------

# Code as NA
data <- data %>% 
  mutate(
         a_happ_5g = na_if(a_happ_5g, "Consent withdrawn by YP"),
         b_bore_5g = na_if(b_bore_5g, "Consent withdrawn by YP"),
         c_reli_5g = na_if(c_reli_5g, "Consent withdrawn by YP"),
         d_hope_5g = na_if(d_hope_5g, "Consent withdrawn by YP"),
         e_exci_5g = na_if(e_exci_5g, "Consent withdrawn by YP"),
         f_fail_5g = na_if(f_fail_5g, "Consent withdrawn by YP"),
         g_lone_5g = na_if(g_lone_5g, "Consent withdrawn by YP"),
         h_sad_5g = na_if(h_sad_5g, "Consent withdrawn by YP"),
         i_luck_5g = na_if(i_luck_5g, "Consent withdrawn by YP"),
         j_rela_5g = na_if(j_rela_5g, "Consent withdrawn by YP")
         )

# Remove participants with missing AMS values
AMS_vars <- c("a_happ_5g", "b_bore_5g", "c_reli_5g", "d_hope_5g",
                  "e_exci_5g", "f_fail_5g", "g_lone_5g", "h_sad_5g",
                  "i_luck_5g", "j_rela_5g")

# Filter out rows with missing values in any of the AMS_vars
data <- data[complete.cases(data[, AMS_vars]), ]

data1 <- data %>%
  mutate(PosAMS = rowSums(select(.,
    c(
      "a_happ_5g",
      "c_reli_5g",
      "e_exci_5g",
      "i_luck_5g",
      "j_rela_5g"
    )
  ) == "4. specific", na.rm = FALSE)) %>%
  mutate(NegAMS = rowSums(select(.,
    c("b_bore_5g", "d_hope_5g", "f_fail_5g", "g_lone_5g", "h_sad_5g")
  ) == "4. specific", na.rm = FALSE)) %>%
  mutate(AMStotal = rowSums(select(.,
    c(
      "a_happ_5g",
      "b_bore_5g",
      "c_reli_5g",
      "d_hope_5g",
      "e_exci_5g",
      "f_fail_5g",
      "g_lone_5g",
      "h_sad_5g",
      "i_luck_5g",
      "j_rela_5g"
    )
  ) == "4. specific", na.rm = FALSE))



describeData1 <- describe(data1)
describe
summaryData1 <- summary(data1)
view(summaryData1) # Sample characteristics

## ----Sample characteristics at T1---------------------------------------------
descriptivesD1 <- data1 %>%
  group_by(Sex) %>%
  summarise(n = n(),
            mean = mean(AMStotal),
            sd = sd(AMStotal))
descriptivesD1

data1%>%
  select(c645a,AMStotal)%>%
  filter(complete.cases(AMStotal)) %>%
  group_by(c645a) %>%
  summarise(n = n()) %>%
  ungroup()

hist(data1$AMStotal)

data1%>%
  select(Ethnicity,AMStotal)%>%
  filter(complete.cases(AMStotal)) %>%
  group_by(Ethnicity) %>%
  summarise(n = n()) %>%
  ungroup()

## ----Coding LOC---------------------------------------------------------------
# Locus of control data
# ccs3000 - ccs3011 = loc data

LoC <- data [, c("ID","ccs3000","ccs3001", "ccs3002","ccs3003","ccs3004","ccs3005","ccs3006","ccs3007","ccs3008","ccs3009","ccs3010","ccs3011")]

describe(LoC)

## -----------------------------------------------------------------------------
# Coding Locus of control data
data2 <- data1 %>%
  mutate(across(ccs3000:ccs3010, ~ifelse(. == "Yes", 1, 0)),
         ccs3011 = ifelse(ccs3011 == "No", 1, 0)) %>%
  select(-c(a_happ_5g, b_bore_5g, c_reli_5g, d_hope_5g,e_exci_5g, f_fail_5g, g_lone_5g, h_sad_5g, i_luck_5g, j_rela_5g)) 

summary(data2)

## -----------------------------------------------------------------------------
# Adding total LoC scores
Total_LoC <- rowSums(data2[,c("ccs3000","ccs3001","ccs3002","ccs3003","ccs3004","ccs3005","ccs3006","ccs3007","ccs3008","ccs3009","ccs3010","ccs3011")])
Total_LoC[!complete.cases(data2[,c("ccs3000","ccs3001","ccs3002","ccs3003","ccs3004","ccs3005","ccs3006","ccs3007","ccs3008","ccs3009","ccs3010","ccs3011")])] <- NA
data2$Total_LoC <- Total_LoC



## -----------------------------------------------------------------------------
# Recoding depressive symptoms at age 16 
# ccs4500 - ccs4515 = depression data

data2_new <- data2 %>%
  mutate(across(ccs4500:ccs4515, ~ as.numeric(.))) %>% # recode as numerical
  mutate(across(ccs4500:ccs4515, ~ 3 - .)) %>% # reverse coding
  mutate(TotalDep = rowSums(select(., ccs4500, ccs4502, ccs4503, ccs4504, ccs4505, ccs4506, ccs4508, ccs4509, ccs4511, ccs4512, ccs4513, ccs4514, ccs4515), na.rm = FALSE)) %>%
  mutate(MaternalHighEd = ifelse(c645a == "A level" | c645a == "Degree", 1, 0))


describe(data2_new)
summary(data2_new)



## ----DEB 16 presence during past 12 months------------------------------------
# Coding DEB age 16 present during the past 12 months
data3 <- data2_new %>%
  mutate(Fast16 = 
           ifelse(ccs5520 == "Never",0,1)) %>% 
  mutate(Vomit = 
           ifelse(ccs5530 == "Never",0,1)) %>% 
  mutate(Laxatives = 
           ifelse(ccs5540 == "Never", 0,1)) %>% 
  mutate(Purge16 = 
           ifelse(Vomit == 1|Laxatives == 1, 1,0)) %>%
  mutate(Exercise16 = 
           ifelse((ccs5510 != "Never" & ccs5512 == "Yes, frequently" ) & ((ccs5513 =="Yes, frequently" | ccs5513 == "Do not miss any exercise sessions") | ccs5511 == "Yes, frequently"), 1, 0)) %>%
  mutate(Binge16 = 
           ifelse(ccs5550 != "Never" & ccs5560 == "Yes, usually", 1, 0)) %>%
  mutate(AnyDEB16 = 
           ifelse(Fast16 == 1|Purge16 == 1|Exercise16 == 1|Binge16 == 1, 1,0)) %>%
  select(ID,Age,Sex,Ethnicity,MaternalHighEd,TotalDep,AMStotal,Fast16,Purge16,Exercise16,Binge16,AnyDEB16,PosAMS,NegAMS) 




## ----Sample characteristics at T2---------------------------------------------
data3_demo <- data3 %>% filter(complete.cases(AnyDEB16)) 
  
descriptives16 <- data3_demo %>%
  group_by(Sex) %>%
  summarise(
    n_total = n(),
    n_AnyDEB16 = sum(AnyDEB16, na.rm = TRUE),
    pct_AnyDEB16 = round(100 * n_AnyDEB16 / n_total, 1),
    n_Fast16 = sum(Fast16, na.rm = TRUE),
    pct_Fast16 = round(100 * n_Fast16 / n_total, 1),
    n_Purge16 = sum(Purge16, na.rm = TRUE),
    pct_Purge16 = round(100 * n_Purge16 / n_total, 1),
    n_Binge16 = sum(Binge16, na.rm = TRUE),
    pct_Binge16 = round(100 * n_Binge16 / n_total, 1),
    n_Exercise16 = sum(Exercise16, na.rm = TRUE),
    pct_Exercise16 = round(100 * n_Exercise16 / n_total, 1)
  )
descriptives16

Dep16 <- data3_demo %>%
  select(Sex,TotalDep) %>%
  group_by(Sex) %>%
  summarise(n = n(),
            mean = mean(TotalDep),
            sd = sd(TotalDep)) %>%
  ungroup()
Dep16
  
data3_demo %>%
  select(TotalDep,MaternalHighEd) %>%
  group_by(MaternalHighEd) %>%
  summarise(n = n()) %>%
  ungroup()

data3_demo %>%
  select(TotalDep,Ethnicity) %>%
  group_by(Ethnicity) %>%
  summarise(n = n()) %>%
  ungroup()

data3_prep <- data3 %>% 
  select(-c(ID,Age)) %>%
  mutate_at(
    c(
      "Sex",
      "Ethnicity",
      "MaternalHighEd",
      "Fast16",
      "Purge16",
      "Exercise16",
      "Binge16",
      "AnyDEB16"
    ),
    as.factor
  )

summary(data3_prep)

## ----DEB 16 more than once a month--------------------------------------------
# Coding DEB age 16 more than once a month
data3_c <- data2_new %>%
  mutate(Fast16 = 
           ifelse((ccs5520 == "1-3 times a month" | ccs5520 == "Once a week" | ccs5520 == "2 or more a week" ),1,0)) %>% 
  mutate(Vomit = 
           ifelse((ccs5530 == "1-3 times a month" | ccs5530 == "Once a week" | ccs5530 == "2-6 times a week" | ccs5530 == "Every day"),1,0)) %>% 
  mutate(YesLaxatives = 
           ifelse((ccs5540 == "Yes, laxative" | ccs5540 == "Yes, other"), 1,0)) %>%
  mutate(FreqLaxatives = 
           ifelse (YesLaxatives == 1 & (ccs5541 == "1-3 times a month" | ccs5541 == "Once a week"| ccs5541 == "2-6 times a week"|ccs5541 == "Every day"), 1,0)) %>% 
  mutate(Purge16 = 
           ifelse(Vomit == 1|FreqLaxatives == 1, 1,0)) %>%
  mutate(Exercise16 = 
           ifelse(((ccs5510 == "1-3 times a month" | 
                      ccs5510 == "1-4 times a week" | 
                      ccs5510 == "5 or more times a week") & 
                     ccs5512 == "Yes, frequently" ) & 
                    ((ccs5513 =="Yes, frequently" | ccs5513 == "Do not miss any exercise sessions") | 
                       ccs5511 == "Yes, frequently"), 1, 0)) %>%
  mutate(Binge16 = 
           ifelse(((ccs5550 != "Never") & (ccs5550 != "Less than once a month")) & ccs5560 == "Yes, usually", 1, 0)) %>%
  mutate(AnyDEB16 = 
           ifelse(Fast16 == 1|Purge16 == 1|Exercise16 == 1|Binge16 == 1, 1,0)) %>%
  select(ID,Age,Sex,Ethnicity,MaternalHighEd,TotalDep,AMStotal,Fast16,Purge16,Exercise16,Binge16,AnyDEB16)


describe(data3_c)

data3c_prep <- data3_c %>% 
  select(-c(ID,Age)) %>%
  mutate_at(
    c(
      "Sex",
      "Ethnicity",
      "MaternalHighEd",
      "Fast16",
      "Purge16",
      "Exercise16",
      "Binge16",
      "AnyDEB16"
    ),
    as.factor
  )

summary(data3c_prep)


## -----------------------------------------------------------------------------
descriptives16 <- data3_c %>%
  group_by(Sex) %>%
  summarise(
    n_AnyDEB16 = sum(AnyDEB16, na.rm = TRUE),
    n_Fast16 = sum(Fast16, na.rm = TRUE),
    n_Purge16 = sum(Purge16, na.rm = TRUE),
    n_Binge16 = sum(Binge16, na.rm = TRUE),
    n_Exercise16 = sum(Exercise16, na.rm = TRUE),
    meanAMS = mean(AMStotal, na.rm = TRUE),
    sdAMS = sd(AMStotal, na.rm = TRUE),
    n_LOC = sum(!is.na(Total_LoC)),
    meanLOC = mean(Total_LoC, na.rm = TRUE),
    sdLOC = sd(Total_LoC, na.rm = TRUE),
    n_Depression = sum(!is.na(TotalDep)),
    meanDepression = mean(TotalDep, na.rm = TRUE),
    sdDepression = sd(TotalDep, na.rm = TRUE),
    n = n()
  )
descriptives16

## ----DEB age 16 more than once a week-----------------------------------------
# Coding DEB age 16 more than once a week
data3_w <- data2_new %>%
  mutate(Fast16 = 
           ifelse((ccs5520 == "Once a week" | ccs5520 == "2 or more a week" ),1,0)) %>% 
  mutate(Vomit = 
           ifelse((ccs5530 == "Once a week"|ccs5530 == "2-6 times a week"|ccs5530 == "Every day"),1,0)) %>% 
  mutate(YesLaxatives = 
           ifelse((ccs5540 == "Yes, laxative" | ccs5540 == "Yes, other"), 1,0)) %>%
  mutate(FreqLaxatives = 
           ifelse (YesLaxatives == 1 & (ccs5541 == "Once a week"| ccs5541 == "2-6 times a week"|ccs5541 == "Every day"), 1,0)) %>% 
  mutate(Purge16 = 
           ifelse(Vomit == 1|FreqLaxatives == 1, 1,0)) %>%
   mutate(Exercise16 = 
           ifelse(((ccs5510 == "1-4 times a week" | ccs5510 == "5 or more times a week") & ccs5512 == "Yes, frequently" ) & ((ccs5513 =="Yes, frequently" | ccs5513 == "Do not miss any exercise sessions") | ccs5511 == "Yes, frequently"), 1, 0)) %>%
  mutate(Binge16 = 
           ifelse((ccs5550 == "More than once a week" | ccs5550 == "Once a week") & ccs5560 == "Yes, usually", 1, 0)) %>%
  mutate(AnyDEB16 = 
           ifelse(Fast16 == 1|Purge16 == 1|Exercise16 == 1|Binge16 == 1, 1,0)) %>%
  select(ID,Age,Sex,Ethnicity,MaternalHighEd,TotalDep,AMStotal,Fast16,Purge16,Exercise16,Binge16,AnyDEB16) 


describe(data3_w)

data3w_prep <- data3_w %>% 
  select(-c(ID,Age)) %>%
  mutate_at(
    c(
      "Sex",
      "Ethnicity",
      "MaternalHighEd",
      "Fast16",
      "Purge16",
      "Exercise16",
      "Binge16",
      "AnyDEB16"
    ),
    as.factor
  )

summary(data3w_prep)


## ----Check whether data is MCAR/MNAR------------------------------------------

# data3_prep: presence of DEB
aggr_plot <-
  aggr(
    data3_prep,
    col = c('skyblue', 'lightpink'),
    numbers = TRUE,
    sortVars = TRUE,
    labels = names(data3_prep),
    cex.axis = .7,
    gap = 3,
    ylab = c("Histogram of missing data", "Pattern")
  )

naniar::prop_miss(data3_prep) # 18.39% missing data in total
naniar::miss_var_summary(data3_prep)
naniar::prop_miss(data3_prep$AnyDEB16) # 36.23%
naniar::prop_miss(data3_prep$TotalDep) # 35.52%
md.pattern(data3_prep) # N=3475 samples complete

# MAR?
explanatory = c("Sex", "Ethnicity", "MaternalHighEd", "TotalDep")
dependent = "AnyDEB16"
data3_prep %>% 
  missing_pairs(dependent, explanatory)

# Recode variables 
data_MAR <- data3_prep %>% 
  mutate(naDEB = if_else(is.na(AnyDEB16), 0, 1))


amsMAR <- glm(naDEB ~ AMStotal, data = data_MAR, family = binomial)
summary_model <- summary(amsMAR)
amsMAR_pvals <- summary_model$coefficients["AMStotal", "Pr(>|z|)"]
amsMAR_pvals

exp(coef(amsMAR))
exp(cbind(Odds_Ratio = coef(amsMAR), confint(amsMAR)))




## -----------------------------------------------------------------------------

# data3c_prep: presence of monthly DEB
aggr_plot <-
  aggr(
    data3c_prep,
    col = c('skyblue', 'lightpink'),
    numbers = TRUE,
    sortVars = TRUE,
    labels = names(data3c_prep),
    cex.axis = .7,
    gap = 3,
    ylab = c("Histogram of missing data", "Pattern")
  )

naniar::prop_miss(data3c_prep) 
md.pattern(data3c_prep) 
# MAR?
explanatory = c("Sex", "Ethnicity", "MaternalHighEd", "AMStotal", "TotalDep")
dependent = "AnyDEB16"
data3c_prep %>% 
  missing_pairs(dependent, explanatory)

# Recode variables 
data_MARc <- data3c_prep %>% 
  mutate(naDEB = if_else(is.na(AnyDEB16), 0, 1))


amsMARc <- summary(glm(naDEB ~ AMStotal, data = data_MARc, family = binomial))
amsMARc_pvals <- amsMARc$coefficients[2, 4]
amsMARc_pvals
# AMS total predicts DEB NA values (specifically lower AMS predictive of DEB NA)

depMARc <- summary(glm(naDEB ~ TotalDep, data = data_MARc, family = binomial))
depMARc_pvals <- depMARc$coefficients[2, 4]
depMARc_pvals
# Depression does not predict DEB NA 



## -----------------------------------------------------------------------------

# data3c_prep: presence of weekly DEB
aggr_plot <-
  aggr(
    data3w_prep,
    col = c('skyblue', 'lightpink'),
    numbers = TRUE,
    sortVars = TRUE,
    labels = names(data3w_prep),
    cex.axis = .7,
    gap = 3,
    ylab = c("Histogram of missing data", "Pattern")
  )

naniar::prop_miss(data3w_prep) 
md.pattern(data3w_prep)

# MAR?
explanatory = c("Sex", "Ethnicity", "MaternalHighEd", "AMStotal", "TotalDep")
dependent = "AnyDEB16"
data3w_prep %>% 
  missing_pairs(dependent, explanatory)

# Recode variables 
data_MARw <- data3w_prep %>% 
  mutate(naDEB = if_else(is.na(AnyDEB16), 0, 1))


amsMARw <- summary(glm(naDEB ~ AMStotal, data = data_MARw, family = binomial))
amsMARw_pvals <- amsMARw$coefficients[2, 4]
amsMARw_pvals



## -----------------------------------------------------------------------------

# # Reduce multicollinearity in dataset before imputing
# # Create predictor matrix
# pred_matrix <- make.predictorMatrix(data3_prep)
# # Set variables
# ams_vars <- c("AMStotal", "PosAMS", "NegAMS")
# deb_vars <- c("Fast16", "Purge16", "Exercise16", "Binge16", "AnyDEB16")
# 
# # Exclude AMS variables from predicting each other
# for (var in ams_vars) {
#   pred_matrix[var, setdiff(ams_vars, var)] <- 0
# }
# 
# # Exclude DEB variables from predicting each other
# for (var in deb_vars) {
#   pred_matrix[var, setdiff(deb_vars, var)] <- 0
# }
# 
# pred_matrix
# 
# 
# imp_method1 = c(
#   "logreg", #Sex
#   "polyreg", #Ethnicity
#   "logreg", #MaternalHighEd
#   "pmm", #TotalDep
#   "pmm", #AMStotal
#   "logreg", #Fast16
#   "logreg", #Purge16
#   "logreg", #Exercise16
#   "logreg", #Binge16
#   "logreg", #AnyDEB16
#   "pmm", #PosAMS
#   "pmm" #NegAMS
# )
# 
# data3_imp <- mice(data3_prep,
#               predictorMatrix = pred_matrix,
#               m = 20,
#               method = imp_method1,
#               seed = 5000)
# write.mice.imputation(data3_imp, name = "data3_imp_save", mids2spss = F)
# 
# 
# 
# # Check imputed dataset
# 
# 
# bwplot(data3_imp)
# densityplot(data3_imp)
# xyplot(data3_imp, AnyDEB16 ~ AMStotal | as.factor(.imp))


## -----------------------------------------------------------------------------

# # Reduce multicollinearity in dataset before imputing
# # Create predictor matrix
# pred_matrix <- make.predictorMatrix(data3c_prep)
# # Set variables
# deb_vars <- c("Fast16", "Purge16", "Exercise16", "Binge16", "AnyDEB16")
# 
# # Exclude DEB variables from predicting each other
# for (var in deb_vars) {
#   pred_matrix[var, setdiff(deb_vars, var)] <- 0
# }
# 
# pred_matrix
# 
# 
# imp_method2 = c(
#   "logreg", #Sex
#   "polyreg", #Ethnicity
#   "logreg", #MaternalHighEd
#   "pmm", #TotalDep
#   "pmm", #AMStotal
#   "logreg", #Fast16
#   "logreg", #Purge16
#   "logreg", #Exercise16
#   "logreg", #Binge16
#   "logreg" #AnyDEB16
# )
# 
# data3c_imp <- mice(data3c_prep,
#               predictorMatrix = pred_matrix,
#               m = 20,
#               method = imp_method2,
#               seed = 5000)
# write.mice.imputation(data3c_imp, name = "data3c_imp_save", mids2spss = F)
# 
# data3w_imp <- mice(data3w_prep,
#               predictorMatrix = pred_matrix,
#               m = 20,
#               method = imp_method2,
#               seed = 5000)
# write.mice.imputation(data3w_imp, name = "data3w_imp_save", mids2spss = F)
# 
# 
# 
# # Check imputed dataset
# 
# 
# bwplot(data3c_imp)
# densityplot(data3c_imp)
# xyplot(data3c_imp, AnyDEB16 ~ AMStotal | as.factor(.imp))
# 
# 
# bwplot(data3w_imp)
# densityplot(data3w_imp)
# xyplot(data3w_imp, AnyDEB16 ~ AMStotal | as.factor(.imp))


## -----------------------------------------------------------------------------
load("data3_imp_save/data3_imp_save.Rdata")
data3_imp = mi.res

 
load("data3c_imp_save/data3c_imp_save.Rdata")
data3c_imp = mi.res

 
load("data3w_imp_save/data3w_imp_save.Rdata")
data3w_imp = mi.res


## ----Assumption Check with complete cases-------------------------------------
# Create model
model1 <- glm(AnyDEB16 ~ AMStotal + TotalDep + MaternalHighEd + Sex, data = data3_prep, family = binomial)

# No high multicolinearity
vif(model1) #variables are not really correlated

# influential values
plot(model1, which = 4, id.n = 3)

model1.data <- augment(model1) %>% 
  mutate(index = 1:n()) 

model1.data %>% top_n(3, .cooksd)

ggplot(model1.data, aes(index, .std.resid)) + 
  geom_point(aes(), alpha = .5) +
  theme_bw()
model1.data %>% 
  filter(abs(.std.resid) > 3) # No influential outliers

# Absence of outliers
outlierTest(model1)

# Power


## ----Logistic Regression DEB16 - complete cases-------------------------------
# Analysis age 16 #presence of any DEB
model1 <- glm(AnyDEB16 ~ AMStotal + TotalDep + MaternalHighEd + Sex, data = data3_prep, family = binomial)
summary(model1) 

logistic_coefficients_model1 <- 
  model1$coefficients %>%
  unclass() %>% 
  as.data.frame() %>% 
  rename('B' = '.') %>%
  mutate(expB = exp(B)) 

logistic_coefficients_model1 %>% 
  kable(digits = 3)

exp(coef(model1))
exp(cbind(Odds_Ratio = coef(model1), confint(model1)))

## ----Logistic Regression DEB16 - MI-------------------------------------------
# Analysis age 16 #presence of any DEB
modelMI1 <- with(data3_imp, glm(AnyDEB16 ~ AMStotal + TotalDep + MaternalHighEd + Sex, family = binomial))

# Pool results
pooled_model <- pool(modelMI1)
summary_model <- summary(pooled_model, conf.int = TRUE, exponentiate = TRUE)
print(summary_model)

# Create the results tribble manually or programmatically
results <- summary_model %>%
  mutate(
    Outcome = "DEB 16",                         
    Predictor = term,                                 
    OR = estimate,
    CI_lower = `2.5 %`,
    CI_upper = `97.5 %`,
    p.value = p.value
  ) %>%
  select(Outcome, Predictor, OR, CI_lower, CI_upper, p.value) %>%
  as_tibble()

print(results)



## ----Logistic regression - presence of any DEB > once a month-----------------
# Complete case analysis age 16 #presence of any DEB > once a month
model1_c <- glm(AnyDEB16 ~ AMStotal + TotalDep + MaternalHighEd + Sex, data = data3c_prep, family = binomial)
summary(model1_c) 

logistic_coefficients_model1_c <- 
  model1_c$coefficients %>%
  unclass() %>% 
  as.data.frame() %>% 
  rename('B' = '.') %>%
  mutate(expB = exp(B))         

logistic_coefficients_model1_c %>% 
  kable(digits = 3)

exp(coef(model1_c))
exp(cbind(Odds_Ratio = coef(model1_c), confint(model1_c)))


## ----Logistic Regression DEB16 - presence of any DEB > once a month - MI------
# Analysis age 16 #presence of any DEB
modelMI1_c <- with(data3c_imp, glm(AnyDEB16 ~ AMStotal + TotalDep + MaternalHighEd + Sex, family = binomial))
summary(pool(modelMI1_c)) 

# Pool results
pooled_model <- pool(modelMI1_c)
summary_model <- summary(pooled_model, conf.int = TRUE, exponentiate = TRUE)
print(summary_model)

# Create the results tribble manually or programmatically
results <- summary_model %>%
  mutate(
    Outcome = "DEB 16",                         
    Predictor = term,                                 
    OR = estimate,
    CI_lower = `2.5 %`,
    CI_upper = `97.5 %`,
    p.value = p.value
  ) %>%
  select(Outcome, Predictor, OR, CI_lower, CI_upper, p.value) %>%
  as_tibble()

print(results)


## ----AMS & Separate DEB 16----------------------------------------------------
# Fasting
model1_Fast <- glm(Fast16 ~ AMStotal + TotalDep + MaternalHighEd + Sex, data = data3_prep, family = binomial)
summary(model1_Fast)
exp(coef(model1_Fast))
exp(cbind(Odds_Ratio = coef(model1_Fast), confint(model1_Fast)))

# Binge eating
model1_Binge <- glm(Binge16 ~ AMStotal+ TotalDep + MaternalHighEd + Sex, data = data3_prep, family = binomial)
summary(model1_Binge)
exp(coef(model1_Binge))
exp(cbind(Odds_Ratio = coef(model1_Binge), confint(model1_Binge)))

# Purging
model1_Purge <- glm(Purge16 ~ AMStotal+ TotalDep + MaternalHighEd + Sex, data = data3_prep, family = binomial)
summary(model1_Purge)
exp(coef(model1_Purge))
exp(cbind(Odds_Ratio = coef(model1_Purge), confint(model1_Purge)))

# Excessive exercise
model1_Exercise <- glm(Exercise16 ~ AMStotal+ TotalDep + MaternalHighEd + Sex, data = data3_prep, family = binomial)
summary(model1_Exercise)
exp(coef(model1_Exercise))
exp(cbind(Odds_Ratio = coef(model1_Exercise), confint(model1_Exercise)))


## ----AMS & Separate DEB 16 - MI-----------------------------------------------
# Fasting
modelMI1_Fast <- with(data3_imp, glm(Fast16 ~ AMStotal + TotalDep + MaternalHighEd + Sex, family = binomial))
summary(pool(modelMI1_Fast))

# Pool results
pooled_model <- pool(modelMI1_Fast)
summary_model <- summary(pooled_model, conf.int = TRUE, exponentiate = TRUE)
print(summary_model)

# Create the results tribble manually or programmatically
results <- summary_model %>%
  mutate(
    Outcome = "Fasting",                         
    Predictor = term,                                 
    OR = estimate,
    CI_lower = `2.5 %`,
    CI_upper = `97.5 %`,
    p.value = p.value
  ) %>%
  select(Outcome, Predictor, OR, CI_lower, CI_upper, p.value) %>%
  as_tibble()

print(results)


# Binge eating
modelMI1_Binge <- with(data3_imp, glm(Binge16 ~ AMStotal+ TotalDep + MaternalHighEd + Sex, family = binomial))
summary(pool(modelMI1_Binge))

# Pool results
pooled_model <- pool(modelMI1_Binge)
summary_model <- summary(pooled_model, conf.int = TRUE, exponentiate = TRUE)
print(summary_model)

# Create the results tribble manually or programmatically
results <- summary_model %>%
  mutate(
    Outcome = "Binge",                         
    Predictor = term,                                 
    OR = estimate,
    CI_lower = `2.5 %`,
    CI_upper = `97.5 %`,
    p.value = p.value
  ) %>%
  select(Outcome, Predictor, OR, CI_lower, CI_upper, p.value) %>%
  as_tibble()

print(results)


# Purging
modelMI1_Purge <- with(data3_imp, glm(Purge16 ~ AMStotal+ TotalDep + MaternalHighEd + Sex, family = binomial))
summary(pool(modelMI1_Purge))

# Pool results
pooled_model <- pool(modelMI1_Purge)
summary_model <- summary(pooled_model, conf.int = TRUE, exponentiate = TRUE)
print(summary_model)

# Create the results tribble manually or programmatically
results <- summary_model %>%
  mutate(
    Outcome = "Purge",                         
    Predictor = term,                                 
    OR = estimate,
    CI_lower = `2.5 %`,
    CI_upper = `97.5 %`,
    p.value = p.value
  ) %>%
  select(Outcome, Predictor, OR, CI_lower, CI_upper, p.value) %>%
  as_tibble()

print(results)


# Excessive exercise
modelMI1_Exercise <- with(data3_imp, glm(Exercise16 ~ AMStotal+ TotalDep + MaternalHighEd + Sex, family = binomial))
summary(pool(modelMI1_Exercise))

# Pool results
pooled_model <- pool(modelMI1_Exercise)
summary_model <- summary(pooled_model, conf.int = TRUE, exponentiate = TRUE)
print(summary_model)

# Create the results tribble manually or programmatically
results <- summary_model %>%
  mutate(
    Outcome = "Exercise",                         
    Predictor = term,                                 
    OR = estimate,
    CI_lower = `2.5 %`,
    CI_upper = `97.5 %`,
    p.value = p.value
  ) %>%
  select(Outcome, Predictor, OR, CI_lower, CI_upper, p.value) %>%
  as_tibble()

print(results)



## ----Exploratory Analysis - DEB16 by memory valence---------------------------
# Positive AMS
model1Pos <- glm(AnyDEB16 ~ PosAMS + TotalDep + MaternalHighEd + Sex, data = data3_prep, family = binomial)
summary(model1Pos) 

logistic_coefficients_model1Pos <- 
  model1Pos$coefficients %>%
  unclass() %>% 
  as.data.frame() %>% 
  rename('B' = '.') %>%
  mutate(expB = exp(B))         

exp(coef(model1Pos))
exp(cbind(Odds_Ratio = coef(model1Pos),
          confint(model1Pos)))

# Negative AMS
model1Neg <- glm(AnyDEB16 ~ NegAMS + TotalDep + MaternalHighEd + Sex, data = data3_prep, family = binomial)
summary(model1Neg) 

logistic_coefficients_model1Neg <- 
  model1Neg$coefficients %>%
  unclass() %>% 
  as.data.frame() %>% 
  rename('B' = '.') %>%
  mutate(expB = exp(B))         

exp(coef(model1Neg))
exp(cbind(Odds_Ratio = coef(model1Neg),
          confint(model1Neg)))

## ----Exploratory Analysis - DEB16 by memory valence - MI----------------------
# Positive AMS
modelMI1Pos <- with(data3_imp, glm(AnyDEB16 ~ PosAMS + TotalDep + MaternalHighEd + Sex, family = binomial))
summary(pool(modelMI1Pos)) 

# Pool results
pooled_model <- pool(modelMI1Pos)
summary_model <- summary(pooled_model, conf.int = TRUE, exponentiate = TRUE)
print(summary_model)

# Create the results tribble manually or programmatically
results <- summary_model %>%
  mutate(
    Outcome = "DEB 16",                         
    Predictor = term,                                 
    OR = estimate,
    CI_lower = `2.5 %`,
    CI_upper = `97.5 %`,
    p.value = p.value
  ) %>%
  select(Outcome, Predictor, OR, CI_lower, CI_upper, p.value) %>%
  as_tibble()

print(results)


# Negative AMS
modelMI1Neg <- with(data3_imp, glm(AnyDEB16 ~ NegAMS + TotalDep + MaternalHighEd + Sex, family = binomial))
summary(pool(modelMI1Neg)) 

# Pool results
pooled_model <- pool(modelMI1Neg)
summary_model <- summary(pooled_model, conf.int = TRUE, exponentiate = TRUE)
print(summary_model)

# Create the results tribble manually or programmatically
results <- summary_model %>%
  mutate(
    Outcome = "DEB 16",                         
    Predictor = term,                                 
    OR = estimate,
    CI_lower = `2.5 %`,
    CI_upper = `97.5 %`,
    p.value = p.value
  ) %>%
  select(Outcome, Predictor, OR, CI_lower, CI_upper, p.value) %>%
  as_tibble()

print(results)



## ----DEB at age 18 (presence of DEB during past 12 months)--------------------
# Coding DEB for age 18 presence during past 12 months
data4 <- data2_new %>%
  mutate(Fast18 = 
           ifelse(cct4110 == "Never",0,1)) %>% 
  mutate(Vomit18 = 
           ifelse(cct4112 == "Never",0,1)) %>% 
  mutate(Laxatives18 = 
           ifelse(cct4115 == "Never", 0,1)) %>% 
  mutate(Purge18 = 
           ifelse(Vomit18 == 1|Laxatives18 == 1, 1,0)) %>%
  mutate(Exercise18 = 
           ifelse((cct4105 != "Never" & 
                    cct4107 == "Yes frequently") & ((cct4108 == "Yes frequently" |
                                                       cct4108 == "Do not miss any exercise sessions")|
                                                       cct4106 == "Yes frequently"), 1, 0)) %>%
  mutate(Binge18 = ifelse(cct4120 != "Never" & cct4125 == "Yes, usually",1,0)) %>%
  mutate(AnyDEB18 = ifelse(Fast18 == 1|Purge18 == 1|Exercise18 == 1|Binge18 == 1, 1,0)) %>%
  select(ID,Age,Sex,MaternalHighEd,AMStotal,Fast18,Purge18,Exercise18,Binge18,AnyDEB18,Total_LoC,TotalDep,Ethnicity,NegAMS)



## ----Sample characteristics at age 18-----------------------------------------
data4_demo <- data4 %>% filter(complete.cases(AnyDEB18)) 

descriptives18 <- data4_demo %>%
  group_by(Sex) %>%
  summarise(
    n_total = n(),
    n_AnyDEB18 = sum(AnyDEB18, na.rm = TRUE),
    pct_AnyDEB18 = round(100 * n_AnyDEB18 / n_total, 1),
    n_Fast18 = sum(Fast18, na.rm = TRUE),
    pct_Fast18 = round(100 * n_Fast18 / n_total, 1),
    n_Purge18 = sum(Purge18, na.rm = TRUE),
    pct_Purge18 = round(100 * n_Purge18 / n_total, 1),
    n_Binge18 = sum(Binge18, na.rm = TRUE),
    pct_Binge18 = round(100 * n_Binge18 / n_total, 1),
    n_Exercise18 = sum(Exercise18, na.rm = TRUE),
    pct_Exercise18 = round(100 * n_Exercise18 / n_total, 1)
  )
descriptives18


data4_demo %>%
  select(Sex) %>%
  group_by(Sex) %>%
  summarise(n = n()) %>%
  ungroup()

data4_demo %>%
  select(Ethnicity) %>%
  group_by(Ethnicity) %>%
  summarise(n = n()) %>%
  ungroup()

data4_demo %>%
  select(MaternalHighEd) %>%
  group_by(MaternalHighEd) %>%
  summarise(n = n()) %>%
  ungroup()

## ----DEB at age 18 (presence of DEB > once a month)---------------------------
# Coding DEB for age 18 > once a month during past 12 months
data4_c <- data2_new %>%
  mutate(Fast18 = 
           ifelse(cct4110 == "Once a week"|
                    cct4110 == "2 or more times a week" | 
                    cct4110 ==  "1-3 times a month",1,0)) %>% 
  mutate(Vomit18 = 
           ifelse(cct4112 == "Once a week"|
                  cct4112 == "2-6 times a week"|
                  cct4112 == "1-3 times a month"|
                  cct4112 == "Every day",1,0)) %>% 
  mutate(YesLaxatives = 
           ifelse(cct4115 == "Yes, laxative"|
                    cct4115 == "Yes, other", 1,0)) %>%
  mutate(LaxativesFreq = 
           ifelse(cct4116 == "1-3 times a month"|
                  cct4116 == "Once a week"|
                  cct4116 == "Every day"|
                  cct4116 == "2-6 times a week",
                  1,0)) %>%
  mutate(Laxatives18 = 
           ifelse(YesLaxatives == 1 & 
                    LaxativesFreq == 1, 1,0)) %>%
  mutate(Purge18 = 
           ifelse(Vomit18 == 1|Laxatives18 == 1, 1,0)) %>%
  mutate(Exercise18 = 
           ifelse(((cct4105 == "5 or more times a week"|
                    cct4105 == "1-4 times a week"|
                    cct4105 == "1-3 times a month") & 
                    cct4107 == "Yes frequently") & ((cct4108 == "Yes frequently" |
                                                       cct4108 == "Do not miss any exercise sessions")|
                                                       cct4106 == "Yes frequently"), 1, 0)) %>%
  mutate(Binge18 = ifelse((cct4120 == "1-3 times a month"|
                            cct4120 == "More than once a week"|
                            cct4120 == "Once a week") & 
                            cct4125 == "Yes, usually",1,0)) %>%
  mutate(AnyDEB18 = ifelse(Fast18 == 1|Purge18 == 1|Exercise18 == 1|Binge18 == 1, 1,0)) %>%
  select(ID,Age,Sex,MaternalHighEd,AMStotal,Fast18,Purge18,Exercise18,Binge18,AnyDEB18,Total_LoC,TotalDep) # %>% filter(complete.cases(Total_LoC,AnyDEB18,TotalDep,MaternalHighEd,AMStotal)) 


## -----------------------------------------------------------------------------
describe(data4_c)
summary(data4_c)
sum(data4_c$Fast18 == TRUE, na.rm = TRUE)
sum(data4_c$Purge18 == TRUE, na.rm = TRUE)
sum(data4_c$Binge18 == TRUE, na.rm = TRUE)
sum(data4_c$Exercise18 == TRUE, na.rm = TRUE)
sum(data4_c$AnyDEB18 == TRUE, na.rm = TRUE)

descriptives18_monthly <- data4_c %>%
  group_by(Sex) %>%
  summarise(n_AnyDEB16 = sum(AnyDEB18, na.rm = TRUE),
            n = n(),
            n_Fast16 = sum(Fast18, na.rm = TRUE),
            n_Purge16 = sum(Purge18, na.rm = TRUE),
            n_Binge16 = sum(Binge18, na.rm = TRUE),
            n_Exercise16 = sum(Exercise18, na.rm = TRUE),
            meanAMS = mean(AMStotal,na.rm = TRUE),
            meanLOC = mean(Total_LoC,na.rm = TRUE),
            meanDepression = mean(TotalDep,na.rm = TRUE)
            )
descriptives18_monthly

## ----DEB at age 18 (presence of DEB > once a week)----------------------------
# Coding DEB for age 18 > once a week during past 12 months
data4_w <- data2_new %>%
  mutate(Fast18 = 
           ifelse(cct4110 == "Once a week"|
                    cct4110 == "2 or more times a week",1,0)) %>% 
  mutate(Vomit18 = 
           ifelse(cct4112 == "Once a week"|
                  cct4112 == "2-6 times a week"|
                  cct4112 == "Every day",1,0)) %>% 
  mutate(YesLaxatives = 
           ifelse(cct4115 == "Yes, laxative"|
                    cct4115 == "Yes, other", 1,0)) %>%
  mutate(LaxativesFreq = 
           ifelse(cct4116 == "Once a week"|
                  cct4116 == "Every day"|
                  cct4116 == "2-6 times a week",
                  1,0)) %>%
  mutate(Laxatives18 = 
           ifelse(YesLaxatives == 1 & 
                    LaxativesFreq == 1, 1,0)) %>%
  mutate(Purge18 = 
           ifelse(Vomit18 == 1|Laxatives18 == 1, 1,0)) %>%
  mutate(Exercise18 = 
           ifelse(((cct4105 == "5 or more times a week"|
                    cct4105 == "1-4 times a week") & 
                    cct4107 == "Yes frequently") & ((cct4108 == "Yes frequently" |
                                                       cct4108 == "Do not miss any exercise sessions")|
                                                       cct4106 == "Yes frequently"), 1, 0)) %>%
  mutate(Binge18 = ifelse((cct4120 == "More than once a week"|
                            cct4120 == "Once a week") & 
                            cct4125 == "Yes, usually",1,0)) %>%
  mutate(AnyDEB18 = ifelse(Fast18 == 1|Purge18 == 1|Exercise18 == 1|Binge18 == 1, 1,0)) %>%
  select(ID,Age,Sex,MaternalHighEd,AMStotal,Fast18,Purge18,Exercise18,Binge18,AnyDEB18,Total_LoC,TotalDep) # %>% filter(complete.cases(Total_LoC,AnyDEB18,TotalDep,MaternalHighEd,AMStotal)) 


## -----------------------------------------------------------------------------
describe(data4_w)
summary(data4_w)
sum(data4_w$Fast18 == TRUE, na.rm = TRUE)
sum(data4_w$Purge18 == TRUE, na.rm = TRUE)
sum(data4_w$Binge18 == TRUE, na.rm = TRUE)
sum(data4_w$Exercise18 == TRUE, na.rm = TRUE)
sum(data4_w$AnyDEB18 == TRUE, na.rm = TRUE)

## -----------------------------------------------------------------------------
# data4_prep <- data4 %>% select(-c("ID", "Age"))
# # Reduce multicollinearity in dataset before imputing
# # Create predictor matrix
# pred_matrix <- make.predictorMatrix(data4_prep)
# # Set variables
# ams_vars <- c("AMStotal", "NegAMS")
# deb_vars <- c("Fast18", "Purge18", "Exercise18", "Binge18", "AnyDEB18")
# 
# # Exclude AMS variables from predicting each other
# for (var in ams_vars) {
#   pred_matrix[var, setdiff(ams_vars, var)] <- 0
# }
# 
# # Exclude DEB variables from predicting each other
# for (var in deb_vars) {
#   pred_matrix[var, setdiff(deb_vars, var)] <- 0
# }
# 
# pred_matrix
# 
# imp_method1 = c(
#   "logreg", #Sex
#   "logreg", #MaternalHighEd
#   "pmm", #AMStotal
#   "logreg", #Fast18
#   "logreg", #Purge18
#   "logreg", #Exercise18
#   "logreg", #Binge18
#   "logreg", #AnyDEB18
#   "pmm", #TotalLoC
#   "pmm", #TotalDep
#   "polyreg", #Ethnicity
#   "pmm" #NegAMS
# )
# 
# data4_imp <- mice(data4_prep,
#               predictorMatrix = pred_matrix,
#               m = 20,
#               method = imp_method1,
#               seed = 5000)
# write.mice.imputation(data4_imp, name = "data4_imp_save", mids2spss = F)
# 
# data4c_prep <- data4_c %>% select(-c("ID", "Age"))
# data4w_prep <- data4_w %>% select(-c("ID", "Age"))
# # Reduce multicollinearity in dataset before imputing
# # Create predictor matrix
# pred_matrix <- make.predictorMatrix(data4c_prep)
# # Set variables
# deb_vars <- c("Fast18", "Purge18", "Exercise18", "Binge18", "AnyDEB18")
# 
# # Exclude DEB variables from predicting each other
# for (var in deb_vars) {
#   pred_matrix[var, setdiff(deb_vars, var)] <- 0
# }
# 
# pred_matrix
# 
# imp_method2 = c(
#   "logreg", #Sex
#   "logreg", #MaternalHighEd
#   "pmm", #AMStotal
#   "logreg", #Fast18
#   "logreg", #Purge18
#   "logreg", #Exercise18
#   "logreg", #Binge18
#   "logreg", #AnyDEB18
#   "pmm", #TotalLoC
#     "pmm" #TotalDep
# )
# 
# data4c_imp <- mice(data4c_prep,
#               predictorMatrix = pred_matrix,
#               m = 20,
#               method = imp_method2,
#               seed = 5000)
# write.mice.imputation(data4c_imp, name = "data4c_imp_save", mids2spss = F)
# 
# data4w_imp <- mice(data4w_prep,
#               predictorMatrix = pred_matrix,
#               m = 20,
#               method = imp_method2,
#               seed = 5000)
# write.mice.imputation(data4w_imp, name = "data4w_imp_save", mids2spss = F)


## -----------------------------------------------------------------------------
load("data4_imp_save/data4_imp_save.Rdata")
data4_imp = mi.res
imp.df4 <- datlist_create(data4_imp)
 
load("data4c_imp_save/data4c_imp_save.Rdata")
data4c_imp = mi.res
imp.df4c <- datlist_create(data4c_imp)

load("data4w_imp_save/data4w_imp_save.Rdata")
data4w_imp = mi.res
imp.df4w <- datlist_create(data4w_imp)
 

## ----Mediation (presence of DEB18)--------------------------------------------
# LoC mediation model
# Create the Model Syntax

modelDEB18 <- ' 

# Direct effects
AnyDEB18 ~ c*AMStotal + TotalDep + MaternalHighEd + Sex

# Mediator
Total_LoC ~ a*AMStotal + TotalDep + MaternalHighEd + Sex
AnyDEB18 ~ b*Total_LoC

# Indirect effect
ab := a*b

# Total effect
total := c + (a*b)
' 
fitDEB18<-sem(model = modelDEB18, data = data4, estimator = "WLSMV", ordered = "AnyDEB18")
summary(fitDEB18)
parameterEstimates(fitDEB18, standardized = TRUE)

## ----Mediation (presence of DEB18) - MI---------------------------------------

# apply simultaneously to all imp datasets
imp.med.model1 <- lapply(imp.df4, FUN = function(data) {
  res <- sem(modelDEB18, data = data, estimator = "WLSMV", ordered = "AnyDEB18")
  return(res)
})

# extract relevant parameters for imp datasets
results.imp.med.model1 <- testEstimates(imp.med.model1)
print(results.imp.med.model1, digits = 4)
confint.mitml.testEstimates(results.imp.med.model1)


## ----Mediation (presence of DEB18 and NegAMS)---------------------------------
# LoC mediation model
# Create the Model Syntax

modelDEB18 <- ' 

# Direct effects
AnyDEB18 ~ c*NegAMS + TotalDep + MaternalHighEd + Sex

# Mediator
Total_LoC ~ a*NegAMS + TotalDep + MaternalHighEd + Sex
AnyDEB18 ~ b*Total_LoC

# Indirect effect
ab := a*b

# Total effect
total := c + (a*b)
' 
fitDEB18<-sem(model = modelDEB18, data = data4, estimator = "WLSMV", ordered = "AnyDEB18")
summary(fitDEB18)
parameterEstimates(fitDEB18, standardized = TRUE)

## ----Mediation - MI-----------------------------------------------------------

# apply simultaneously to all imp datasets
imp.med.model1 <- lapply(imp.df4, FUN = function(data) {
  res <- sem(modelDEB18, data = data, estimator = "WLSMV", ordered = "AnyDEB18")
  return(res)
})

# extract relevant parameters for imp datasets
results.imp.med.model1 <- testEstimates(imp.med.model1)
print(results.imp.med.model1, digits = 4)
confint.mitml.testEstimates(results.imp.med.model1)




