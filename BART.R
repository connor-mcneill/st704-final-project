options(java.parameters = c("-XX:+UseConcMarkSweepGC", "-Xmx8192m"))
library(bartMachine)
library(dplyr)

load("data/train.Rdata")
load("data/test.Rdata")


train <- train %>% mutate(
  admission_rate_neg1 = I(admission_rate^-1),
  pct_dom_fth = I(pct_domestic^4),
  log_total_enrollment = log(total_enrollment)
)

bartModFinal <- bartMachineCV(X = as.data.frame(train %>% dplyr::select(
  degree_length, room_and_board, out_of_state_tuition, 
  make_world_better_percent, stem_percent, degree_type, 
  type_school, ST_FIPS, locale, hbcu, annhi, AANAPII, his,
  nanti, men_only, women_only, admission_rate, admission_rate_neg1,
  log_total_enrollment, sat_avg, distance_only, netprice_lowincome, 
  tuit_fte, inexp_fte, avg_fac_salary, pct_ft_fac, pct_pell, 
  graduation_rate, age_entry, first_gen, pct_grad_degree, pct_domestic, 
  pct_dom_fth, unemployment_rate, Women, AmericanIndian, Asian, 
  Hispanic, PacificIslander, White, TwoOrMoreRaces, Unknown,
  -early_career_pay)), y = log(train$early_career_pay),
  serialize = TRUE, num_burn_in = 1000, 
  num_iterations_after_burn_in = 4000
  )

save(bartModFinal, file = "./data/bartModFinal.RData")

bartModFinal$cv_stats
sort(bartMachine::get_var_counts_over_chain(bartModFinal))
bartMachine::investigate_var_importance(bartModFinal)

