options(java.parameters = c("-XX:+UseConcMarkSweepGC", "-Xmx5g"))
library(bartMachine)
library(dplyr)

load("data/train.Rdata")
load("data/test.Rdata")


train <- train %>% mutate(
  admission_rate_neg1 = I(admission_rate^-1),
  pct_dom_fth = I(pct_domestic^4),
  log_total_enrollment = log(total_enrollment)
)
## TEMP
train <- train[-c(444),]
test <- test %>% mutate(
  admission_rate_neg1 = I(admission_rate^-1),
  pct_dom_fth = I(pct_domestic^4),
  log_total_enrollment = log(total_enrollment)
)

cv.bartMod <- bartMachineCV(X = as.data.frame(train %>% dplyr::select(
  degree_length, make_world_better_percent, stem_percent,
  locale, men_only, log_total_enrollment, distance_only, 
  tuit_fte, avg_fac_salary, pct_pell, graduation_rate, 
  pct_grad_degree, pct_dom_fth, pct_domestic, Women, Asian,
  -early_career_pay)), y = log(train$early_career_pay),
  num_burn_in = 1000, num_tree_cvs = c(5,10,20,40),
  num_iterations_after_burn_in = 4000, serialize = TRUE
)


save(cv.bartMod, file = "./data/bartModFinal420.Rdata")

# Load BART Simpson
# MAKE SURE THE CONSOLE SAYS ENOUGH GB OF RAM :)

options(java.parameters = c("-XX:+UseConcMarkSweepGC", "-Xmx10g"))
library(bartMachine)
library(ggplot2)
library(dplyr)
load("./data/bartModFinal420.Rdata")
load("data/train.Rdata")
load("data/test.Rdata")


train <- train %>% mutate(
  admission_rate_neg1 = I(admission_rate^-1),
  pct_dom_fth = I(pct_domestic^4),
  log_total_enrollment = log(total_enrollment)
)
## TEMP
train <- train[-c(444),]

test <- test %>% mutate(
  admission_rate_neg1 = I(admission_rate^-1),
  pct_dom_fth = I(pct_domestic^4),
  log_total_enrollment = log(total_enrollment)
)


cv.bartMod$cv_stats
sort(bartMachine::get_var_counts_over_chain(cv.bartMod))

# varImp.BART <- investigate_var_importance(cv.bartMod,num_replicates_for_avg = 20)
# varImp.BART.df <- data.frame(
#   vars = names(varImp.BART$avg_var_props),
#   means = varImp.BART$avg_var_props,
#   sds = varImp.BART$sd_var_props
# )
# varImp.BART.df <- varImp.BART.df[order(varImp.BART.df$means,decreasing = T),]
# 
# varImp.BART.df$vars <- factor(varImp.BART.df$vars, 
#                               levels = varImp.BART.df$vars[order(varImp.BART.df$means,
#                                                                  decreasing = T)])
# 
# varImp.BART.df.top20 <- varImp.BART.df[1:20,]
# 
# 
# 
# # Create a barplot with error bars
# ggplot(varImp.BART.df.top20, aes(x = vars, y = means)) +
#   geom_bar(stat = "identity", position = "dodge", alpha = 0.5) +
#   geom_errorbar(aes(ymin = means - sds, ymax = means + sds), 
#                 width = 0.2, position = position_dodge(0.9), col = "blue") +
#   labs(x = "Variable", y = "Variable Inclusion Proportion", 
#        title = "Top 20 Variable Importance for BART from Training") + theme_minimal() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 


bartMachine::check_bart_error_assumptions(cv.bartMod)
bartMachine::plot_convergence_diagnostics(cv.bartMod)
bartMachine::plot_y_vs_yhat(cv.bartMod, 
                            Xtest = as.data.frame(train %>% dplyr::select(
                              degree_length, make_world_better_percent, stem_percent,
                              locale, men_only, log_total_enrollment, distance_only, 
                              tuit_fte, avg_fac_salary, pct_pell, graduation_rate, 
                              pct_grad_degree, pct_dom_fth, pct_domestic, Women, Asian,
                              -early_career_pay)), 
                            ytest = log(train$early_career_pay),prediction_intervals = T)
bartMachine::plot_y_vs_yhat(cv.bartMod, 
                            Xtest = as.data.frame(test %>% dplyr::select(
                              degree_length, make_world_better_percent, stem_percent,
                              locale, men_only, log_total_enrollment, distance_only, 
                              tuit_fte, avg_fac_salary, pct_pell, graduation_rate, 
                              pct_grad_degree, pct_dom_fth, pct_domestic, Women, Asian,
                              -early_career_pay
                            )), 
                            ytest = log(test$early_career_pay),prediction_intervals = T)
bartMachine::get_sigsqs(cv.bartMod,plot_hist = T)

bart.preds.test <-bartMachine::bart_predict_for_test_data(bart_machine = cv.bartMod,
                                                          Xtest = as.data.frame(test %>% dplyr::select(
                                                            degree_length, make_world_better_percent, stem_percent,
                                                            locale, men_only, log_total_enrollment, distance_only, 
                                                            tuit_fte, avg_fac_salary, pct_pell, graduation_rate, 
                                                            pct_grad_degree, pct_dom_fth, pct_domestic, Women, Asian,
                                                            -early_career_pay
                                                          )), 
                                                          ytest = log(test$early_career_pay))

bart.preds.train <-bartMachine::bart_predict_for_test_data(bart_machine = cv.bartMod,
                                                           Xtest = as.data.frame(train %>% dplyr::select(
                                                             degree_length, make_world_better_percent, stem_percent,
                                                             locale, men_only, log_total_enrollment, distance_only, 
                                                             tuit_fte, avg_fac_salary, pct_pell, graduation_rate, 
                                                             pct_grad_degree, pct_dom_fth, pct_domestic, Women, Asian,
                                                             -early_career_pay
                                                           )), 
                                                           ytest = log(train$early_career_pay))

library(bartMan)
tree_data.BART <- extractTreeData(cv.bartMod, as.data.frame(train %>% dplyr::select(
  degree_length, room_and_board, out_of_state_tuition, 
  make_world_better_percent, stem_percent, degree_type, 
  type_school, ST_FIPS, locale, hbcu, annhi, AANAPII, his,
  nanti, men_only, women_only, admission_rate, admission_rate_neg1,
  log_total_enrollment, sat_avg, distance_only, netprice_lowincome, 
  tuit_fte, inexp_fte, avg_fac_salary, pct_ft_fac, pct_pell, 
  graduation_rate, age_entry, first_gen, pct_grad_degree, pct_domestic, 
  pct_dom_fth, unemployment_rate, Women, AmericanIndian, Asian, 
  Hispanic, PacificIslander, White, TwoOrMoreRaces, Unknown,
  -early_career_pay)))
saveRDS(tree_data.BART, "./data/tree_data_BART.Rds")

?bartMan::plotTrees

tree_data.BART <- readRDS("data/tree_data_BART.Rds")

bartMan::viviBartPlot(viviBartMatrix(tree_data.BART))
