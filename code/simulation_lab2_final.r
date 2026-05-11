# ------------------------------------------------------------
# Lab Assignment 2: Simulation Study
# Population: Polish gminas, Census 2021
# y = total resident population
# x = total dwelling stock
# ------------------------------------------------------------

# libraries
library(dplyr)
library(janitor)
#library(sampling)

setwd("C:\\Users\\HP\\Desktop\\studies\\sample surveys\\lab 2") # replace with the needed folder path

# load and prep data
naro  <- read.csv2("NARO_4181_CTAB_20260508153506.csv")
dwell <- read.csv2("GOSP_2166_CTAB_20260511201617.csv")

naro  <- clean_names(naro)  %>%
  rename(population = total_total_2021_person) %>%
  select(-x)

dwell <- clean_names(dwell) %>%
  rename(dwellings = total_dwellings_2021) %>%
  select(-x)

# filter to gmina level (TERYT codes ending in 1, 2, 3)
filter_gminas <- function(df) {
  df %>% filter(code %% 10 %in% c(1, 2, 3))
}

naro  <- filter_gminas(naro)
dwell <- filter_gminas(dwell)

# merge and derive stratification variables
# note: two approaches for stratification are explored
pop_data <- naro %>%
  inner_join(dwell, by = intersect(colnames(naro), colnames(dwell))) %>%
  filter(!is.na(population), !is.na(dwellings),
         population > 0, dwellings > 0) %>%
  mutate(
    code_padded = formatC(code, width = 7, flag = "0"),
    voivodship  = substr(code_padded, 1, 2), # 16 voivodships
    gmina_type  = factor(code %% 10, # 3 gmina types
                         levels = c(1, 2, 3),
                         labels = c("Urban", "Rural", "Urban-rural"))
  )

# population params
N <- nrow(pop_data)
y <- pop_data$population
x <- pop_data$dwellings
mu_y <- mean(y)
mu_x <- mean(x)

cat("N:", N, "\n")
cat("true mean population (mu_y):", round(mu_y, 2), "\n")
cat("true mean dwellings (mu_x):", round(mu_x, 2), "\n")
cat("Pearson corr r(y,x):", round(cor(y, x), 3), "\n\n")

# compare stratifications
cat("stratification A: voivodship (16 strata)\n")
print(table(pop_data$voivodship))
cat("within-stratum SD (voivodship):\n")
print(round(tapply(y, pop_data$voivodship, sd)))

cat("\nstratification B: gmina type (3 strata)\n")
print(table(pop_data$gmina_type))
cat("within-stratum mean (gmina type):\n")
print(round(tapply(y, pop_data$gmina_type, mean)))
cat("within-stratum SD (gmina type):\n")
print(round(tapply(y, pop_data$gmina_type, sd)))

# notes:
# why voivodship as strata did not work (post-analysis):
# within-stratum SDs are extremely big (17K–106K) relative to stratum means
# Mazowieckie (14) has SD=105,678, Warsaw drags this stratum's variance sky high
# no real homogeneity within voivodships for population size

# simulation
compute_stats <- function(estimates, true_mean) {
  c(mean = mean(estimates),
    bias = mean(estimates) - true_mean,
    variance = var(estimates),
    mse = mean((estimates - true_mean)^2))
}

run_simulation <- function(strat_var, n = 300, B = 1000, seed = 123) {

  strata <- sort(unique(strat_var))
  H <- length(strata)
  N_h <- as.integer(table(strat_var))
  names(N_h) <- strata

  # proportional allocation
  n_h <- round(n * N_h / N)
  n_h <- pmax(n_h, 2)
  n_h[which.max(N_h)] <- n_h[which.max(N_h)] + (n - sum(n_h))

  mu_x_h <- tapply(x, strat_var, mean)

  set.seed(seed)
  est_A1 <- numeric(B)
  est_A2 <- numeric(B)
  est_B1 <- numeric(B)
  est_B2 <- numeric(B)
  est_B2_sep <- numeric(B)

  for (b in 1:B) {

    # design A: SRS
    idx <- sample(1:N, n, replace = FALSE)
    y_s <- y[idx]; x_s <- x[idx]

    est_A1[b] <- mean(y_s)
    est_A2[b] <- (mean(y_s) / mean(x_s)) * mu_x

    # design B: stratified SRS
    y_h <- numeric(H); x_h <- numeric(H)

    for (h in 1:H) {
      idx_h <- which(strat_var == strata[h])
      idx_samp <- sample(idx_h, n_h[h], replace = FALSE)
      y_h[h] <- mean(y[idx_samp])
      x_h[h] <- mean(x[idx_samp])
    }

    # B1: stratified mean
    est_B1[b] <- sum((N_h / N) * y_h)

    # B2: combined ratio estimator (within strata)
    est_B2[b] <- sum((N_h / N) * (y_h / x_h) * mu_x_h)

    # B2*: separate ratio estimator (across full stratified sample)
    # note: was additionally added after poor stratification performance with voivodships as strata
    y_st <- sum((N_h / N) * y_h)
    x_st <- sum((N_h / N) * x_h)
    est_B2_sep[b] <- (y_st / x_st) * mu_x
  }

  # compute stats
  s_A1 <- compute_stats(est_A1, mu_y)
  s_A2 <- compute_stats(est_A2, mu_y)
  s_B1 <- compute_stats(est_B1, mu_y)
  s_B2 <- compute_stats(est_B2, mu_y)
  s_B2_sep <- compute_stats(est_B2_sep, mu_y)

  results <- data.frame(
    Estimator = c("A1: SRS + Mean",
                  "A2: SRS + Ratio",
                  "B1: Stratified + Mean",
                  "B2: Stratified + Combined Ratio",
                  "B2*: Stratified + Separate Ratio"),
    Mean = round(c(s_A1["mean"], s_A2["mean"], s_B1["mean"],
                        s_B2["mean"], s_B2_sep["mean"]), 2),
    Bias = round(c(s_A1["bias"], s_A2["bias"], s_B1["bias"],
                        s_B2["bias"], s_B2_sep["bias"]), 2),
    Variance = round(c(s_A1["variance"], s_A2["variance"], s_B1["variance"],
                        s_B2["variance"], s_B2_sep["variance"]), 2),
    MSE = round(c(s_A1["mse"], s_A2["mse"], s_B1["mse"],
                        s_B2["mse"], s_B2_sep["mse"]), 2),
    RE_vs_A1 = round(s_A1["mse"] / c(s_A1["mse"], s_A2["mse"], s_B1["mse"],
                                       s_B2["mse"], s_B2_sep["mse"]), 3)
  )

  list(results = results,
       estimates = list(A1 = est_A1, A2 = est_A2,
                        B1 = est_B1, B2 = est_B2, B2_sep = est_B2_sep),
       n_h = n_h, N_h = N_h, strata = strata)
}

# run both simulations
cat("\nresults: stratification by voivodship\n")
sim_voi <- run_simulation(pop_data$voivodship)
print(sim_voi$results, row.names = FALSE)

cat("\nresults: stratification by gmina type\n")
sim_gmt <- run_simulation(pop_data$gmina_type)
print(sim_gmt$results, row.names = FALSE)

# plots
plot_histograms <- function(sim, title) {
  est   <- sim$estimates
  all   <- c(est$A1, est$A2, est$B1, est$B2, est$B2_sep)
  xlim  <- quantile(all, c(0.001, 0.999))
  cols  <- c("#4C72B0", "#DD8452", "#55A868", "#C44E52", "#9467BD")
  names <- c("A1: SRS+Mean", "A2: SRS+Ratio",
             "B1: Strat+Mean", "B2: Strat+CombRatio",
             "B2*: Strat+SepRatio")

  par(mfrow = c(2, 3), mar = c(4, 4, 3, 1))
  for (i in 1:5) {
    hist(est[[i]], breaks = 40, xlim = xlim, col = cols[i],
         border = "white", main = names[i],
         xlab = "Estimate", freq = FALSE)
    abline(v = mu_y, col = "red", lwd = 2, lty = 2)
  }
  par(mfrow = c(1, 1))
}

plot_boxplots <- function(sim, title) {
  est  <- sim$estimates
  cols <- c("#4C72B0", "#DD8452", "#55A868", "#C44E52", "#9467BD")
  boxplot(est$A1, est$A2, est$B1, est$B2, est$B2_sep,
          names  = c("A1", "A2", "B1", "B2", "B2*"),
          col    = cols, border = "grey30",
          main   = title,
          ylab   = "Estimate of Population Mean",
          outline = FALSE)
  abline(h = mu_y, col = "red", lwd = 2, lty = 2)
  legend("topright", legend = "True mean", col = "red",
         lty = 2, lwd = 2, bty = "n")
}

plot_histograms(sim_voi, "Stratification by Voivodship")
plot_histograms(sim_gmt, "Stratification by Gmina Type")
plot_boxplots(sim_voi, "Stratification by Voivodship")
plot_boxplots(sim_gmt, "Stratification by Gmina Type")

# additional scope: try running simulations for different sample sizes
sample_sizes <- c(50, 100, 200, 300, 500)

results_by_n <- lapply(sample_sizes, function(n_val) {
  sim <- run_simulation(pop_data$gmina_type, n = n_val)
  sim$results$n <- n_val
  sim$results
})

results_all_n <- do.call(rbind, results_by_n)

# summary
for (n_val in sample_sizes) {
  cat("\n=== n =", n_val, "===\n")
  print(results_all_n[results_all_n$n == n_val, 
                       c("Estimator", "MSE", "RE_vs_A1")], 
        row.names = FALSE)
}

# RE vs n plot for gmina type stratification
png("re_by_n.png", width = 1100, height = 600, res = 120)

par(mar = c(5, 5, 4, 18))  # large right margin for legend

matplot(sample_sizes, t(re_plot),
        type = "b", pch = pchs, lty = 1, lwd = lwds,
        col = cols,
        xlab = "Sample size n",
        ylab = "Relative efficiency vs A1",
        main = "Relative efficiency by sample size\n(Stratification by gmina type)",
        cex.axis = 1.1, cex.lab = 1.2, cex.main = 1.2,
        log = "y",
        ylim = c(0.9, 100))

abline(h = 1, lty = 2, col = "grey50", lwd = 1.5)

legend(x = par("usr")[2],
       y = par("usr")[4],
       legend = labels,
       col = cols,
       lty = 1,
       lwd = 2,
       pch = pchs,
       bty = "n",
       cex = 0.9,
       pt.cex = 1.2,
       xpd = TRUE)

dev.off()
# looking at the plot: B2  consistently best across all n, decreasing from ~82 to ~39