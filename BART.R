library(bartMachine)
library(dplyr)

load("data/train.Rdata")
load("data/test.Rdata")


train <- train %>% mutate(
  admission_rate_neg1 = I(admission_rate^-1),
  pct_dom_fth = I(pct_domestic^4),
  log_total_enrollment = log(total_enrollment)
)

options(java.parameters = "-Xmx8000m")
set_bart_machine_num_cores(4)
bartMod <- bartMachineCV(X = as.data.frame(train %>% dplyr::select(
  degree_length, room_and_board, out_of_state_tuition, 
  make_world_better_percent, stem_percent, degree_type, 
  type_school, ST_FIPS, locale, hbcu, annhi, AANAPII, his,
  nanti, men_only, women_only, admission_rate, admission_rate_neg1,
  log_total_enrollment, sat_avg, distance_only, netprice_lowincome, 
  tuit_fte, inexp_fte, avg_fac_salary, pct_ft_fac, pct_pell, 
  graduation_rate, age_entry, first_gen, pct_grad_degree, pct_domestic, 
  pct_dom_fth, unemployment_rate, Women, AmericanIndian, Asian, 
  Hispanic, PacificIslander, White, TwoOrMoreRaces, Unknown,
  -early_career_pay)), y = log(train$early_career_pay)
  )