library(TTESimulation)
library(tidyverse)

library(progressr)
handlers("cli")
progress_interval <- 50

library(future.apply)
#> Loading required package: future
oplan <- plan(cluster, 
              workers = parallelly::availableCores(max = 12, omit = 2))

# prepare random seed for simulation
Prepare_Seed <- function(seed, num){
  future.apply::future_lapply(
    seq(num), 
    FUN = function(x) .Random.seed,
    future.chunk.size = Inf, 
    future.seed = as.integer(seed))
}

# ------------ Compare Cox v.s. parametric MLE ------------
sample_size <- 100L
dco_obj <- TTES_DCO_By_Event(cut_event_num = as.integer(sample_size * 0.7))
dco_obj2 <- TTES_DCO_By_Date(cut_date = 100000)

enroll_obj <- TTES_Enroll_Same()

subj_strat_obj <- TTES_Subj_Strat_Data(
  var_vec = c("fct_a"), 
  prob = c(fct_a = 0.5), 
  use_strat_comb_prob = FALSE
)

treatment_vec <- c("placebo", "experimental")
rand_obj <- TTES_Strat_Randomize_Simple(
  treatment = treatment_vec, 
  strat_var = "fullfct_fct", 
  trt_prob = 1, 
  ctrl_prob = 1
)

tte_strat_obj <- TTES_TTE_Strat_Data(
  treatment = treatment_vec, 
  var_vec = c("fct_a"), 
  beta = list(
    beta0 = 1, 
    beta_trt = 0.23, 
    beta_prog = c(0.0), 
    beta_pred = c(0)
  ), 
  evt_shape = 1, 
  cnsr_lambda = -log(0.95) / 12
)

ana_strat_obj <- TTES_Analyze_Strat(
  strat_var = "tte1_ana_ms_fct_dco1", 
  event_var = "tte1_is_evt_dco1", 
  small_cut = 5L
)

ana_tte_obj <- TTES_Analyze_TTE(
  strat_var = "rand_fct", 
  tte = "tte1", 
  dco = "dco1", 
  strat_rhs_str = "treatment", 
  unstrat_rhs_str = "treatment"
)

strat_merge_obj <- TTES_Strat_Merge_Dummy(res_var = "tte1_ana_ms_fct_dco1")


my_design2 <- Design(
  sample_size = sample_size, 
  enroll_obj = enroll_obj, 
  subj_strat_obj = subj_strat_obj, 
  rand_obj = rand_obj, 
  tte_strat_obj = tte_strat_obj, 
  dco_obj = dco_obj, 
  ana_strat_obj = ana_strat_obj, 
  ana_tte_obj = ana_tte_obj, 
  strat_merge_obj = strat_merge_obj
)

# prepare current seed
sim_num <- 10000L
sim_seeds <- Prepare_Seed(seed = 43, num = sim_num)

# perform simulation
with_progress({
  sim_res_full <- simulate(
    my_design2, 
    nsim = as.integer(sim_num), 
    seed = sim_seeds, 
    progress_interval = progress_interval
  )
})

sum_strat_res <- sim_res_full$sum_strat_res
sum_tte_res <- sim_res_full$sum_tte_res

sum_tte_res %>%
  summarise(
    strat_hr = mean(tte1_overall_dco1_strat_hr), 
    unstrat_hr = mean(tte1_overall_dco1_unstrat_hr), 
    strat_loghr = mean(log(tte1_overall_dco1_strat_hr)), 
    unstrat_loghr = mean(log(tte1_overall_dco1_unstrat_hr)), 
    strat_power = mean(tte1_overall_dco1_strat_pval <= 0.05), 
    unstrat_power = mean(tte1_overall_dco1_unstrat_pval <= 0.05), 
    strat_hrsd = sd(tte1_overall_dco1_strat_hr), 
    unstrat_hrsd = sd(tte1_overall_dco1_unstrat_hr), 
    strat_loghrsd = sd(log(tte1_overall_dco1_strat_hr)), 
    unstrat_loghrsd = sd(log(tte1_overall_dco1_unstrat_hr)),  
  )

sim_data_list <- sim_res_full$subj_dat_list

res_mle <- do.call(rbind, 
                   lapply(sim_data_list, function(indata){
                     subj_dat <- indata@subj_dat
                     subj_dat %>%
                       summarise(
                         n_evt_trt = sum(tte1_is_evt_dco1 & treatment == "experimental"), 
                         n_evt_ctrl = sum(tte1_is_evt_dco1 & treatment == "placebo"), 
                         tot_trt = sum(tte1_obs_time_dco1 * (treatment == "experimental")), 
                         tot_ctrl = sum(tte1_obs_time_dco1 * (treatment == "placebo"))
                       ) %>%
                       mutate(
                         beta_hat = log(n_evt_trt * tot_ctrl / n_evt_ctrl / tot_trt), 
                         hr_hat = exp(beta_hat))
                   }))

res_mle %>%
  summarise(
    hr = mean(hr_hat), 
    loghr = mean(beta_hat), 
    hrsd = sd(hr_hat), 
    loghrsd = sd(beta_hat)
  )
