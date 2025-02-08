## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "man/figures/README-",
  out.width = "100%"
)

## ----eval=FALSE---------------------------------------------------------------
# install.packages("RLescalation")

## ----eval=FALSE---------------------------------------------------------------
# # install.packages("remotes")
# remotes::install_github("MatsuuraKentaro/RLescalation")

## ----eval=FALSE---------------------------------------------------------------
# library(RLescalation)

## ----eval=FALSE, echo=FALSE---------------------------------------------------
# RLescalation::setup_python()

## ----eval=FALSE---------------------------------------------------------------
# escalation_rule <- learn_escalation_rule(
#   J = 6, target = 0.25, epsilon = 0.04, delta = 0.1,
#   N_total = 36, N_cohort = 3, seed = 123,
#   rl_config = rl_config_set(iter = 1000)
# )
# 
# escalation_rule
# #> <EscalationRule>
# #> dir: escalation_rules/20250101_162633
# #> created at: 2025-01-01 17:43:23
# #> call:
# #> learn_escalation_rule(J = 6, target = 0.25, epsilon = 0.04, delta = 0.1,
# #>     N_total = 36, N_cohort = 3, seed = 123, rl_config = rl_config_set(iter = 1000))
# #> iterations: 1000
# #> checkpoints: 500, 600, 700, 800, 900, 1000

## ----eval=FALSE---------------------------------------------------------------
# current_dose <- 3
# some_Ns   <- c(3, 6, 3, 0, 0, 0)
# some_DLTs <- c(0, 1, 1, 0, 0, 0)
# 
# escalation_rule$opt_action(current_dose, some_Ns, some_DLTs)
# #> [1] "up"

## ----eval=FALSE---------------------------------------------------------------
# eval_scenarios <- list(
#   c(0.04, 0.05, 0.09, 0.14, 0.15, 0.24),
#   c(0.07, 0.16, 0.23, 0.27, 0.34, 0.55),
#   c(0.34, 0.42, 0.46, 0.49, 0.58, 0.62),
#   c(0.05, 0.08, 0.11, 0.15, 0.60, 0.72)
# )
# 
# n_sim <- 1000  # the number of simulated clinical trials
# sim_list <- list()
# 
# for (scenarioID in seq_len(length(eval_scenarios))) {
#   prob_true <- eval_scenarios[[scenarioID]]
#   for (simID in seq_len(n_sim)) {
#     sim_one <- simulate_one_trial(escalation_rule, prob_true, seed = simID)
#     sim_list[[length(sim_list) + 1]] <- data.frame(
#       scenarioID = scenarioID, simID = simID, sim_one, check.names = FALSE)
#   }
# }
# 
# d_sim <- do.call(rbind, sim_list)
# head(d_sim, 13)
# #>    scenarioID simID cohortID dose N DLT recommended
# #> 1           1     1        1    1 3   0          up
# #> 2           1     1        2    2 3   0          up
# #> 3           1     1        3    3 3   0          up
# #> 4           1     1        4    4 3   1          up
# #> 5           1     1        5    5 3   0          up
# #> 6           1     1        6    6 3   2        stay
# #> 7           1     1        7    6 3   2        stay
# #> 8           1     1        8    6 3   1        stay
# #> 9           1     1        9    6 3   1        stay
# #> 10          1     1       10    6 3   0        stay
# #> 11          1     1       11    6 3   0        stay
# #> 12          1     1       12    6 3   0       MTD_6
# #> 13          1     2        1    1 3   0          up

## ----eval=FALSE---------------------------------------------------------------
# library(dplyr)
# 
# MTD_true <- list("MTD_6", c("MTD_3", "MTD_4"), "no_MTD", "MTD_4")
# 
# d_res <- d_sim |>
#   filter(cohortID == max(cohortID), .by = c(scenarioID, simID)) |>
#   rowwise() |>
#   mutate(correct = if_else(recommended %in% MTD_true[[scenarioID]], 1, 0)) |>
#   ungroup() |>
#   summarise(PCS = mean(correct), .by = scenarioID)
# 
# d_res
# #> # A tibble: 4 × 2
# #>   scenarioID   PCS
# #>        <int> <dbl>
# #> 1          1 0.833
# #> 2          2 0.731
# #> 3          3 0.411
# #> 4          4 0.531

## ----eval=FALSE---------------------------------------------------------------
# my_scenarios <- list(
#   prob = list(c(0.05, 0.11, 0.25, 0.31, 0.32, 0.40),
#               c(0.23, 0.27, 0.45, 0.47, 0.50, 0.57),
#               c(0.38, 0.40, 0.43, 0.47, 0.51, 0.55)),
#   MTD = list(3, c(1, 2), -1),  # -1 means "no MTD"
#   weight = c(1, 2, 1)
# )
# 
# escalation_rule <- learn_escalation_rule(
#   J = 6, target = 0.25, epsilon = 0.04, delta = 0.1,
#   N_total = 36, N_cohort = 3, seed = 123,
#   rl_config = rl_config_set(iter = 1000),
#   rl_scenarios = my_scenarios
# )

## ----eval=FALSE---------------------------------------------------------------
# saveRDS(escalation_rule, file = "escalation_rule.RDS")

## ----eval=FALSE---------------------------------------------------------------
# escalation_rule <- readRDS(file = "escalation_rule.RDS")

## ----eval=FALSE---------------------------------------------------------------
# escalation_rule$input

## ----eval=FALSE---------------------------------------------------------------
# escalation_rule$log

## ----eval=FALSE---------------------------------------------------------------
# escalation_rule$resume_learning(iter = 100)

## ----eval=FALSE---------------------------------------------------------------
# another_escalation_rule <- EscalationRule$new(dir = "checkpoints/20250101_162633_00900")

