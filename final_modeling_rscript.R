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
             -`Total Minority`-out_of_state_total-`Non-Resident Foreign`-ST_FIPS, 
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
                      type_school + locale + hbcu + annhi + AANAPII +
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

# OLS get RMSE as baseline
round(abs(RMSE(predict(model.without.outliers, newdata=train.without.outliers),
     log(train.without.outliers$early_career_pay)) - 
RMSE(predict(model.without.outliers, newdata=test),
     log(test$early_career_pay))),4)

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
lm.beta.model <- lm.beta(stepwise.model)
xtable::xtable(lm.beta.model)

## Verify that our model meets linear regression assumptions
plot(stepwise.model)
## Calculate RMSEP based on test set
stepwise.preds = predict(stepwise.model, newdata = test)
stepwise.rmsep = RMSE(stepwise.preds, log(test$early_career_pay))
stepwise.preds.train = predict(stepwise.model, newdata=train.without.outliers)
stepwise.rmse = RMSE(stepwise.preds.train, log(train.without.outliers$early_career_pay))
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
lasso.coefs = coef(fit.lasso, s=lambda.lasso.chosen)
lasso.coefs.table = data.frame(Variable = rownames(lasso.coefs)[(lasso.coefs@i + 1)], 
                               Coefficient = lasso.coefs@x)
coef(cv.lasso$glmnet.fit, s=lambda.lasso.chosen)
X.test.lasso = model.matrix(model.formula, data=test)[,-1]
lasso.train.preds = predict(fit.lasso, s=lambda.lasso.chosen,
                            newx=X.lasso)
lasso.rmse = RMSE(lasso.train.preds, log(train.without.outliers$early_career_pay))
lasso.preds = predict(fit.lasso, s=lambda.lasso.chosen, newx=X.test.lasso)
lasso.rmsep = RMSE(lasso.preds, log(test$early_career_pay))
round(lasso.rmsep - lasso.rmse, 4)
knitr::kable(lasso.coefs.table, row.names = FALSE,
             booktabs=TRUE, format='latex', digits=4)
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
pls.rmse = RMSE(predict(fit.pls, newdata=train.without.outliers, ncomp=ncomp.pls1se),
                log(train.without.outliers$early_career_pay))
getScreePlot<-function(fit,label="PCR",suggested=1,...){
  plot(cumsum(fit$Xvar / fit$Xtotvar),type="b",
       xlab="NumberofComponents",ylab="%VarianceExplained",
       main=paste0("ScreePlotfor ",label))
  abline(h=0.5,col="red")
  abline(h=0.7,col="blue")
  abline(h=0.9,col="pink")
  abline(v=suggested)
  legend(x=12,y=0.45,legend= c("50%","70%","90%"),fill= c("red","blue","pink"))
}

getScreePlot(fit.pls, "PLSR", suggested = ncomp.pls1se)
# Fit GLM Model

gamma.mod1 = glm(early_career_pay ~ sat_avg + stem_percent +
      tuit_fte + avg_fac_salary + pct_pell + pct_domestic + 
      I(pct_domestic^4), data=train.without.outliers,
    family=Gamma(link = "log"))
summary(gamma.mod1)

glm.preds = predict(gamma.mod1, type='response', test)
glm.train.preds = predict(gamma.mod1, type='response', train.without.outliers)
RMSE(log(glm.preds), log(test$early_career_pay))
RMSE(log(glm.train.preds), log(train.without.outliers$early_career_pay))
plot(2*log(glm.train.preds), residuals.glm(gamma.mod1, type='deviance'),
     xlab=expression(2*log(hat(mu))), ylab="Deviance Residuals",
     main="Fitted vs. Deviance Residuals")
zero.fn = function(x) {
  return(x*0)
}
curve(zero.fn, from=20.9, to=23, add=TRUE, col='red')

plot(log(train.without.outliers$early_career_pay), 
     predict(gamma.mod1, type='link'),
     xlab="Log of Early Career Pay",
     ylab="Log-Link Predicted Values",
     main="Check of Proper Link Function")
abline(0,1, col='red')
glm.summary = summary(gamma.mod1)
glm.output <- lm.beta(gamma.mod1)
xtable::xtable(glm.output)
