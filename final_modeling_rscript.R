# Cleaned Up Modeling for Project

# Import Packages ----
library(MASS)
library(tidyverse)
library(car)
library(caret)
library(leaps)

# Data Cleaning ---- 
## see data cleaning file

# Load in Cleaned Data Files with Train and Test 80/20 split ----
load('data/train.RData')
load('data/test.RData')

# EDA ----
## Fit base linear model with multicollinearity fixed
base.model <- lm(early_career_pay ~ . -name-state-mid_career_pay-state_code
             -`Total Minority`-out_of_state_total-`Non-Resident Foreign`
             -in_state_total-region-Black-in_state_tuition-rank-poverty_rate
             -pred_degree_type-highest_degree, 
             data = train)
summary(base.model)
vifslm1 <- vif(base.model)
vifslm1[order(vifslm1[,1],decreasing = T),]

## determine response transformation
bc <- boxCox(base.model)
## log transform the response!

## determine predictor transformations
boxTidwell(log(early_career_pay) ~ room_and_board + total_enrollment + ST_FIPS + 
             admission_rate + sat_avg + first_gen + med_fam_inc + pct_domestic + 
             median_hh_income, 
           other.x = ~ degree_length + make_world_better_percent + 
             stem_percent + degree_type + type_school + locale + hbcu + pbi + 
             annhi + AANAPII + his + nanti + men_only + women_only + 
             distance_only,  
           data=train)

## Fit base linear model with transformations
transformed.lm = lm(log(early_career_pay) ~ degree_length + room_and_board +
                      out_of_state_tuition +
                      make_world_better_percent + stem_percent + degree_type +
                      type_school + ST_FIPS + locale + hbcu + annhi + AANAPII +
                      his + nanti + men_only + women_only + admission_rate
                    + I(admission_rate^-1) + log(total_enrollment) +
                      sat_avg + distance_only + netprice_lowincome +
                      tuit_fte + inexp_fte + avg_fac_salary + pct_ft_fac +
                      pct_pell + graduation_rate + age_entry + first_gen +
                      pct_grad_degree + pct_domestic + I(pct_domestic^4) +
                      unemployment_rate +
                      Women + AmericanIndian + Asian + Hispanic +
                      PacificIslander + White + TwoOrMoreRaces + Unknown
                    , 
                    data=train)
summary(transformed.lm)

# Linear Regression with Stepwise Regression ----
## set seed and number of groups for CV
set.seed(704)
K = 10
## fit stepwise model using train function 
stepwise.selection <- train(log(early_career_pay) ~ degree_length + room_and_board +
                              out_of_state_tuition +
                              make_world_better_percent + stem_percent + degree_type +
                              type_school + ST_FIPS + locale + hbcu + annhi + AANAPII +
                              his + nanti + men_only + women_only + admission_rate
                            + I(admission_rate^-1) + log(total_enrollment) +
                              sat_avg + distance_only + netprice_lowincome +
                              tuit_fte + inexp_fte + avg_fac_salary + pct_ft_fac +
                              pct_pell + graduation_rate + age_entry + first_gen +
                              pct_grad_degree + pct_domestic + I(pct_domestic^4) +
                              unemployment_rate +
                              Women + AmericanIndian + Asian + Hispanic +
                              PacificIslander + White + TwoOrMoreRaces + Unknown, 
                            data=train,
                            method='leapSeq',
                            tuneGrid=data.frame(nvmax = 1:50),
                            trControl=trainControl(method='cv',
                                                   number=K,
                                                   returnResamp='all',
                                                   selectionFunction='oneSE')
)
stepwise.selection ## Model selected has six variables
## Get the final model from stepwise
coef(stepwise.selection$finalModel, id=6)
stepwise.model = lm(log(early_career_pay) ~ make_world_better_percent + 
                      stem_percent +
                      tuit_fte + avg_fac_salary + pct_pell + pct_domestic + 
                      I(pct_domestic^4),
                    data=train)
summary(stepwise.model)
## Verify that our model meets linear regression assumptions
plot(stepwise.model)
## Calculate RMSEP based on test set
stepwise.preds = predict(stepwise.model, newdata = test)
stepwise.rmsep = RMSE(stepwise.preds, log(test$early_career_pay))

# Fit LASSO Model ----
