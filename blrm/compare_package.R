library(OncoBayes2)
# library(rmsb)
library(blrm)
library(crmPack)

## Example from Neuenschwander, B., et al. (2009). Stats in Medicine

num_comp <- 1 # one investigational drug
num_inter <- 0 # no drug-drug interactions need to be modeled
num_groups <- nlevels(hist_SA$group_id) # no stratification needed
num_strata <- 1 # no stratification needed


dref <- 50

blrmfit <- blrm_exnex(
    cbind(num_toxicities, num_patients - num_toxicities) ~
        1 + log(drug_A / dref) |
        0 |
        group_id,
    data = hist_SA,
    prior_EX_mu_mean_comp = matrix(
        c(logit(1/2), # mean of intercept on logit scale
          log(1)),    # mean of log-slope on logit scale
        nrow = num_comp,
        ncol = 2
    ),
    prior_EX_mu_sd_comp = matrix(
        c(2,  # sd of intercept
          1), # sd of log-slope
        nrow = num_comp,
        ncol = 2
    ),
    ## Here we take tau as known and as zero.
    ## This disables the hierarchical prior which is
    ## not required in this example as we analyze a
    ## single trial.
    prior_EX_tau_mean_comp = matrix(
        c(0, 0),
        nrow = num_comp,
        ncol = 2
    ),
    prior_EX_tau_sd_comp = matrix(
        c(1, 1),
        nrow = num_comp,
        ncol = 2
    ),
    prior_EX_prob_comp = matrix(1, nrow = num_comp, ncol = 1),
    prior_tau_dist = 0,
    prior_PD = FALSE
)

plot_toxicity_curve(blrmfit, x = "drug_A")
plot_toxicity_intervals(blrmfit, x = "drug_A")
plot_toxicity_intervals_stacked(blrmfit, x = "drug_A")

summary(blrmfit)
summary(blrmfit, interval_prob = c(0, 0.16, 0.33, 1))

posterior_interval(blrmfit)
# posterior_linpred(blrmfit)
# posterior_predict(blrmfit)

print(blrmfit)


hist_SA

hist_SA_rep <- data.frame(
    group_id = rep("A", 18), 
    drug_A = c(rep(1, 3), rep(2.5, 4), rep(5, 5), rep(10, 4), rep(25, 2)), 
    DLT = factor(c(rep(0, 16), rep(2, 2)))
)
hist_SA_rep

# rmsb::blrm(DLT ~ log(drug_A / 50), data = hist_SA_rep)
# 
# mean <- c()
# ?blrm::blrm_mono_ms()


mean <- c(1, 0)
se <- c(2, 1)
corr <- 0
prior <- list(mean = mean, se = se, corr = corr)


# parameters
seeds <- 1:2
nsamples <- 10000
burn_in <- 0.2
drug_name <- "DRUG-X"
dose_unit <- "mg"
prov_dose <- c(1, 2.5, 5, 10, 25) # prov_dose <- c(360, 480, 720, 1080, 1440)
ref_dose <- 50 # ref_dose <- 720
category_bound <- c(0.16, 0.33)
category_name <- c("under-dosing", "targeted-toxicity", "over-dosing")
ewoc <- 0.25

# observed cohorts
dose <- c(1, 2.5, 5, 10, 25)
n_pat <- c(3, 4, 5, 4, 2)
dlt <- c(0, 0, 0, 0, 2)

# combine prior, parameters, and observed cohorts to a list
data <- list(seeds=seeds, nsamples=nsamples, burn_in=burn_in, drug_name=drug_name,
             dose_unit=dose_unit, prov_dose=prov_dose, ref_dose=ref_dose,
             dose=dose, n_pat=n_pat, dlt=dlt, category_bound=category_bound,
             category_name=category_name, ewoc=ewoc)

save_pdf_output <- TRUE
trial <- blrm_mono_ss(prior=prior, data=data, output_excel=FALSE, output_pdf=save_pdf_output)
if(save_pdf_output){
    print(paste0("PDF figures are saved to folder ", getwd()))
}


(prob_posterior <- trial$prob_posterior)    # probability of these doses in different category (under-dosing, targeted-toxicity, over-dosing)
(pi_posterior <- trial$pi_posterior)    # DLT rate, each column for one dose. The stored are: mean, sd, 0.025, 0.5, 0.975 quantile of DLT rate
View(trial$current_summary)


#------ crmPack version ------
# Define the dose grid.
empty_data <- Data(doseGrid = c(1, 2.5, 5, 10, 25))

# Initialize the CRM model.
model <- LogisticLogNormal(
    mean = c(1, 0),
    cov = matrix(c(4, 0, 0, 1), nrow = 2),
    ref_dose = 50
)

# a visual representation of the prior
prior_samples <- mcmc(
    data = empty_data,
    model = model, 
    options = McmcOptions(burnin = 100, step = 2, samples = 1000)
)
plot(prior_samples, model, empty_data)

# Choose the rule for dose increments.
my_increments <- IncrementsRelative(
    intervals = c(0, 30),
    increments = c(1, 0.5)
)

# Choose the rule for selecting the next dose. -- EWOC
my_next_best <- NextBestNCRM(
    target = c(0.2, 0.35),
    overdose = c(0.35, 1),
    max_overdose_prob = 0.25
)

# Choose the rule for the cohort size.
my_size_1 <- CohortSizeRange(
    intervals = c(0, 30),
    cohort_size = c(1, 3)
)
my_size_2 <- CohortSizeDLT(
    intervals = c(0, 1),
    cohort_size = c(1, 3)
)
my_size <- maxSize(my_size_1, my_size_2)

# Choose the rule for stopping.
my_stopping_1 <- StoppingMinCohorts(nCohorts = 3)
my_stopping_2 <- StoppingTargetProb(
    target = c(0.2, 0.35),
    prob = 0.5
)
my_stopping_3 <- StoppingMinPatients(nPatients = 20)
my_stopping <- (my_stopping_1 & my_stopping_2) | my_stopping_3

# Initialize the design.
design <- Design(
    model = model,
    nextBest = my_next_best,
    stopping = my_stopping,
    increments = my_increments,
    cohort_size = my_size,
    data = empty_data,
    startingDose = 1
)

examine(design)

# input the hist_sa data
hist_sa_data <- Data(
    x = c(rep(1, 3), rep(2.5, 4), rep(5, 5), rep(10, 4), rep(25, 2)), 
    y = c(rep(0, 16), rep(1, 2)), 
    ID = 1 : 18, 
    cohort = c(rep(1, 3), rep(2, 4), rep(3, 5), rep(4, 2), rep(5, 2), rep(6, 2)),
    doseGrid = c(1, 2.5, 5, 10, 25)
)
plot(hist_sa_data)

postSample <- mcmc(
    data = hist_sa_data, 
    model = model, 
    options = McmcOptions(burnin = 2000, step = 2, samples = 10000)
)
plot(postSample, model, hist_sa_data)


tabulatePosterior <- function(mcmcSamples, observedData) {
    as_tibble(
        nextBest(
            my_next_best,
            doselimit = 100,
            samples = mcmcSamples,
            model = model,
            data = observedData
        )$probs
    ) %>%
        left_join(
            tibble(
                dose = observedData@x,
                WithDLT = observedData@y
            ) %>%
                group_by(dose) %>%
                summarise(
                    Treated = n(),
                    WithDLT = sum(WithDLT),
                    .groups = "drop"
                ),
            by = "dose"
        ) %>%
        replace_na(list(Treated = 0, WithDLT = 0)) %>%
        select(dose, Treated, WithDLT, target, overdose) 
}

tabulatePosterior(postSample, hist_sa_data)

maxDose(my_increments, hist_sa_data)
nextBest(
    my_next_best, 
    doselimit = maxDose(my_increments, hist_sa_data), 
    samples = postSample, 
    model = model, 
    data = hist_sa_data
)
stopTrial(
    my_stopping, 
    dose = 10, 
    postSample, 
    model, 
    hist_sa_data
)
