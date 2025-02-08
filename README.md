
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RLescalation <img src="man/figures/logo.png" align="right" width="120" />

<!-- badges: start -->

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/RLescalation)](https://cran.r-project.org/package=RLescalation)
[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/RLescalation)](https://cran.r-project.org/package=RLescalation)
[![R-CMD-check](https://github.com/MatsuuraKentaro/RLescalation/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/MatsuuraKentaro/RLescalation/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Overview

The purpose of this `RLescalation` package is to easily construct an
dose escalation rule that directly optimizes the percentages of correct
selection (PCS) of the maximum tolerated dose (MTD). Several high-level
functions are also provided to make it easy to perform simulation
studies.

## Installation

You can install the stable version from CRAN as follows.

``` r
install.packages("RLescalation")
```

You can install the development version from GitHub as follows.

``` r
# install.packages("remotes")
remotes::install_github("MatsuuraKentaro/RLescalation")
```

# Example

We demonstrate computing an optimal dose escalation by reinforcement
learning for the example in Sect. 3 of [the original
paper](https://doi.org/10.1080/10543406.2023.2170402).

When you load `RLescalation` as follows, Python itself and the Python
packages to conduct reinforcement learning will be installed.

``` r
library(RLescalation)
```

## Learn a dose escalation rule

We obtain an optimal dose escalation rule by executing
`learn_escalation_rule()` with the number of doses `J` and the target
DLT probability `target` (please see `help("learn_escalation_rule")` for
other arguments).

``` r
escalation_rule <- learn_escalation_rule(
  J = 6, target = 0.25, epsilon = 0.04, delta = 0.1,
  N_total = 36, N_cohort = 3, seed = 123,
  rl_config = rl_config_set(iter = 1000)
)

escalation_rule
#> <EscalationRule>
#> dir: escalation_rules/20250101_162633
#> created at: 2025-01-01 17:43:23
#> call:
#> learn_escalation_rule(J = 6, target = 0.25, epsilon = 0.04, delta = 0.1, 
#>     N_total = 36, N_cohort = 3, seed = 123, rl_config = rl_config_set(iter = 1000))
#> iterations: 1000
#> checkpoints: 500, 600, 700, 800, 900, 1000
```

With the default settings, it takes roughly 5-20 seconds per iter, so it
would take about 1.5-6 hours when `iter = 1000`.

## How to use the escalation rule

To compute optimal action using the obtained escalation rule, pass the
current dose index (i.e., one of `1`, `2`, …, `J`) and data of the
number of assigned patients and DLTs for each dose to `opt_action()`.

``` r
current_dose <- 3
some_Ns   <- c(3, 6, 3, 0, 0, 0)
some_DLTs <- c(0, 1, 1, 0, 0, 0)

escalation_rule$opt_action(current_dose, some_Ns, some_DLTs)
#> [1] "up"
```

If the returned action is `MTD_1`, …, `MTD_J`, or `no_MTD` (stop the
trial because of toxicity), it means the end of the trial.

## How to evaluate the escalation rule

A convenient high-level function (`simulate_one_trial`) is provided to
evaluate the obtained escalation rule. The following is an example of
code to perform a simulation study similar to Sect. 3 of the original
paper.

``` r
eval_scenarios <- list(
  c(0.04, 0.05, 0.09, 0.14, 0.15, 0.24),
  c(0.07, 0.16, 0.23, 0.27, 0.34, 0.55),
  c(0.34, 0.42, 0.46, 0.49, 0.58, 0.62),
  c(0.05, 0.08, 0.11, 0.15, 0.60, 0.72)
)

n_sim <- 1000  # the number of simulated clinical trials
sim_list <- list()

for (scenarioID in seq_len(length(eval_scenarios))) {
  prob_true <- eval_scenarios[[scenarioID]]
  for (simID in seq_len(n_sim)) {
    sim_one <- simulate_one_trial(escalation_rule, prob_true, seed = simID)
    sim_list[[length(sim_list) + 1]] <- data.frame(
      scenarioID = scenarioID, simID = simID, sim_one, check.names = FALSE)
  }
}

d_sim <- do.call(rbind, sim_list)
head(d_sim, 13)
#>    scenarioID simID cohortID dose N DLT recommended
#> 1           1     1        1    1 3   0          up
#> 2           1     1        2    2 3   0          up
#> 3           1     1        3    3 3   0          up
#> 4           1     1        4    4 3   1          up
#> 5           1     1        5    5 3   0          up
#> 6           1     1        6    6 3   2        stay
#> 7           1     1        7    6 3   2        stay
#> 8           1     1        8    6 3   1        stay
#> 9           1     1        9    6 3   1        stay
#> 10          1     1       10    6 3   0        stay
#> 11          1     1       11    6 3   0        stay
#> 12          1     1       12    6 3   0       MTD_6
#> 13          1     2        1    1 3   0          up
```

The following code is an example of calculating the PCS.

``` r
library(dplyr)

MTD_true <- list("MTD_6", c("MTD_3", "MTD_4"), "no_MTD", "MTD_4")

d_res <- d_sim |> 
  filter(cohortID == max(cohortID), .by = c(scenarioID, simID)) |> 
  rowwise() |>
  mutate(correct = if_else(recommended %in% MTD_true[[scenarioID]], 1, 0)) |>
  ungroup() |> 
  summarise(PCS = mean(correct), .by = scenarioID)

d_res
#> # A tibble: 4 × 2
#>   scenarioID   PCS
#>        <int> <dbl>
#> 1          1 0.833
#> 2          2 0.731
#> 3          3 0.411
#> 4          4 0.531
```

# Tips

## Custom scenarios for reinforcement learning

If you want to use custom scenarios for reinforcement learning, you can
pass the custom scenarios by specifying the argument `rl_scenarios` in
`learn_escalation_rule` function.

``` r
my_scenarios <- list(
  prob = list(c(0.05, 0.11, 0.25, 0.31, 0.32, 0.40), 
              c(0.23, 0.27, 0.45, 0.47, 0.50, 0.57),
              c(0.38, 0.40, 0.43, 0.47, 0.51, 0.55)),
  MTD = list(3, c(1, 2), -1),  # -1 means "no MTD"
  weight = c(1, 2, 1)
)

escalation_rule <- learn_escalation_rule(
  J = 6, target = 0.25, epsilon = 0.04, delta = 0.1,
  N_total = 36, N_cohort = 3, seed = 123,
  rl_config = rl_config_set(iter = 1000),
  rl_scenarios = my_scenarios
)
```

See the return of `compute_rl_scenarios()` for details.

## What to do if the learning is unstable

If an error occurs during reinforcement learning, please try the
following.

- Use `checkpoint` before the error (see below)
- Change `seed` in `learn_escalation_rule()`
- Try Linux or WSL instead of Windows because Ray on Windows is
  currently in beta
- Reduce `sgd_minibatch_size` in `rl_config_set()` to `100L`

The obtained dose escalation rules may overfit some scenarios, so that
the MTD cannot be estimated correctly at all in some other scenarios. In
such cases, please try the following.

- Change `checkpoint` (see below)
- Change `seed` in `learn_escalation_rule()`
- Try custom scenarios

## How to use Escalation Rule Class

The `escalation_rule` above is an object of the Escalation Rule Class
(R6). Here is a brief explanation of how to use it.

### Save the escalation rule

The obtained escalation rule can be saved using `saveRDS`, a standard R
function.

``` r
saveRDS(escalation_rule, file = "escalation_rule.RDS")
```

To load it, use `readRDS`.

``` r
escalation_rule <- readRDS(file = "escalation_rule.RDS")
```

### Inputs of `learn_escalation_rule` function

The inputs passed to the `learn_escalation_rule` function can be
retrieved as follows.

``` r
escalation_rule$input
```

### Obtain returns during reinforcement learning

The statistics of returns during reinforcement learning can be retrieved
as follows.

``` r
escalation_rule$log
```

### Resume learning

Reinforcement learning can be resumed with the following function.

``` r
escalation_rule$resume_learning(iter = 100)
```

### Use checkpoint

Multiple checkpoints are created by `learn_escalation_rule` function. By
default, the last checkpoint is used to build an escalation rule. If you
want to build another escalation rule using another checkpoint, specify
the directory name created by `learn_escalation_rule` function as
follows.

``` r
another_escalation_rule <- EscalationRule$new(dir = "checkpoints/20250101_162633_00900")
```
