# Cleaned Up Modeling for Project

# Import Packages ----
library(MASS)
library(tidyverse)
library(car)
library(caret)
library(leaps)
library(glmnet)
library(pls)

# Data Cleaning ---- 
## see data cleaning file

# Load in Cleaned Data Files with Train and Test 80/20 split ----
load('data/train.RData')
load('data/test.RData')

# EDA ----
## Fit base linear model with multicollinearity fixed
base.model <- lm(early_career_pay ~ . -name-state-mid_career_pay-state_code
             -`Total Minority`-out_of_state_total-`Non-Resident Foreign`, 
             data = train)
vifslm1 <- vif(base.model)
vifs.base = vifslm1[order(vifslm1[,1],decreasing = T),]
vifs.names = rownames(vifs.base)
rownames(vifs.base) = NULL
vifs.to.print = data.frame(Predictor = vifs.names,
                           VIF = round(vifs.base[,1], 3))
library(xtable)
print(xtable(vifs.to.print[vifs.to.print$VIF > 10,]),
      booktabs=TRUE, include.rownames=FALSE)

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

model.formula = as.formula(transformed.lm)

plot(rstudent(transformed.lm) ~ hatvalues(transformed.lm))
car::influencePlot(transformed.lm)
car::outlierTest(transformed.lm, cutoff=.05, n.max=Inf)

model.without.outliers = lm(model.formula,
                            data=train[-c(444), ])
summary(model.without.outliers)
plot(rstudent(model.without.outliers) ~ hatvalues(model.without.outliers))
car::influencePlot(model.without.outliers)
car::outlierTest(model.without.outliers, cutoff=.05, n.max=Inf)
train.without.outliers = train[-c(444),]


# Linear Regression with Stepwise Regression ----
## set seed and number of groups for CV
set.seed(704)
K = 10
## fit stepwise model using train function 
stepwise.selection <- train(model.formula, 
                            data=train.without.outliers,
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
stepwise.model = lm(log(early_career_pay) ~ sat_avg + 
                      stem_percent +
                      tuit_fte + avg_fac_salary + pct_pell + pct_domestic + 
                      I(pct_domestic^4),
                    data=train.without.outliers)
summary(stepwise.model)
## Verify that our model meets linear regression assumptions
plot(stepwise.model)
## Calculate RMSEP based on test set
stepwise.preds = predict(stepwise.model, newdata = test)
stepwise.rmsep = RMSE(stepwise.preds, log(test$early_career_pay))

# Fit LASSO Model ----
## Fit the lasso model
X.lasso = model.matrix(model.without.outliers)[,-1]
fit.lasso <- glmnet(X.lasso, log(train.without.outliers$early_career_pay), 
                    alpha=1)
#plot(fit.lasso, label=TRUE, xvar='lambda')
lambda.lasso = fit.lasso$lambda
betahat.lasso = fit.lasso$beta
# plot standardized LASSO coefficients
nn <- nrow(X.lasso)
Xsd <- apply(X.lasso,2,sd) * sqrt((nn-1)/nn)
ysd = sd(train$early_career_pay) * sqrt((nn-1)/nn)
par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
matplot(lambda.lasso, t(betahat.lasso * Xsd), type="l",
        log="x", lwd=2,lty=1:5,col=1:6,
        xlab=expression(lambda), ylab='Standardized Coefficients')
axis(3, at=lambda.lasso, labels=fit.lasso$df)
# perform CV to find optimal lambda
set.seed(704)
cv.lasso <- cv.glmnet(X.lasso, log(train.without.outliers$early_career_pay), 
                      alpha=1)
plot(cv.lasso)
lambda.lasso.chosen <- cv.lasso$lambda.1se
coef(fit.lasso, s=lambda.lasso.chosen)
coef(cv.lasso$glmnet.fit, s=lambda.lasso.chosen)
X.test.lasso = model.matrix(model.formula, data=test)[,-1]
lasso.preds = predict(fit.lasso, s=lambda.lasso.chosen, newx=X.test.lasso)
lasso.rmsep = RMSE(lasso.preds, log(test$early_career_pay))

# Fit PLS Model ----
set.seed(704)
fit.pls = plsr(model.formula,
     data=train.without.outliers,
     scale=TRUE,
     validation = "CV",
     segments=10,
     segment.type='random',
     method='simpls'
)
ncomp.pls1se = selectNcomp(fit.pls,method="onesigma",plot=TRUE)
pls.preds = predict(fit.pls, newdata=test, ncomp=ncomp.pls1se)
pls.rmsep = RMSE(pls.preds, log(test$early_career_pay))

# 
