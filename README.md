# Estimating municipal population in Poland: a simulation study

## Overview

This repository contains the data and R code for a Monte Carlo 
simulation study evaluating the impact of auxiliary information 
and sampling design on the accuracy of estimating the mean 
resident population of Polish gminas (municipalities).

The study compares four estimator-design combinations across 
two stratification variables and four sample sizes, using a 
fully observed finite population of N = 2,477 Polish gminas 
from the 2021 National Census.

## Repository Structure

The structure of this repository is as follows:
```
polish_gmina_population_sampling/
├── README.md
├── data/
│   ├── NARO_4181_CTAB_20260508153506.csv      # population
│   ├── GOSP_2166_CTAB_20260511201617.csv      # dwelling stock
│   ├── PODZ_1410_CTAB_20260508120159.csv      # area
│   └── data_sources.md                        # download instructions
├── code/
│   └── simulation_lab2_final.R 
└── figures/
    ├── boxplots_gmina.png
    └── re_by_n.png
```

## Data sources

All datasets used in this study were downloaded from the **GUS Local Data Bank** 
([bdl.stat.gov.pl](https://bdl.stat.gov.pl)). The process of data acquisition is detailed in data/data_sources.md.

## Reproducing the analysis

1. Clone the repository
2. Open `code/simulation_lab2_final.R` in R or RStudio (alternative: VS Code with R extension)
3. Update the `setwd()` path on lines 13 (dataset location) and 200 (plots location) to your local directory 
4. Run the full script

**Dependencies:**
```r
install.packages(c("dplyr", "janitor"))
```

**R version:** 4.6.0  
**Seed:** `set.seed(123)`

## Results

| Estimator | Stratification | RE vs A1 (n=300) |
|-----------|---------------|-----------------|
| A1: SRS + mean | — | 1.000 |
| A2: SRS + ratio | — | 9.126 |
| B1: Stratified + mean | Gmina type | 1.049 |
| B2: Stratified + combined ratio | Gmina type | 42.543 |

The combination of gmina type stratification and the combined 
ratio estimator (B2) achieves the highest relative efficiency 
across all sample sizes, demonstrating that well-chosen 
stratification and auxiliary information produce multiplicative 
rather than additive efficiency gains.
