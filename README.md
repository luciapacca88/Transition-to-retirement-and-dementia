# The transition to retirement and dementia risk: An application of sequence analysis

This repository contains the code used for the analyses presented in the manuscript:

Pacca, L., Hebert, J., Carr, D. C., Brenowitz, W. D., Glymour, M. M., Harrati, A., & Vable, A. M. *The transition to retirement and dementia risk: An application of sequence analysis.*

## Study overview

Using data from the US Health and Retirement Study (HRS), this study examines associations between employment and retirement trajectories from ages 51 to 75 and dementia probability at age 76 and older.

We use sequence and cluster analysis to identify ten distinct transition-to-retirement trajectories, followed by regression analyses examining their associations with later-life dementia probability.

## Code organization

The repository contains three main code files:

**01 – Data cleaning and analysis (Stata)**

Contains the Stata code used for data preparation, variable construction, and statistical analyses examining associations between retirement trajectories and dementia probability.

**02 – Sequence analysis (R)**

Contains the R code used to construct and analyze employment trajectories, calculate sequence dissimilarities, identify transition-to-retirement clusters, and generate sequence analysis visualizations.

**03 – Table 1 (Stata)**

Contains the Stata code used to generate descriptive statistics by transition-to-retirement cluster, as presented in Table 1 of the manuscript.

## Data availability

This study uses data from a subsample of the Health and Retirement Study (HRS), a longitudinal study of older adults in the United States, sponsored by the National Institute on Aging and conducted by the University of Michigan.

Researchers interested in reproducing the analyses must register with the HRS and comply with the applicable data-use requirements. Information on obtaining access to HRS data is available at https://hrs.isr.umich.edu/data-products.

Researchers may contact the corresponding author (pacca@wustl.edu) to inquire about access to the analytic dataset. Any sharing of the dataset will be subject to HRS data-use policies and applicable permissions.

## Software requirements and reproducibility

The analyses were conducted using Stata and R. The Stata scripts perform data preparation, variable construction, regression analyses, and descriptive statistics. The R script performs sequence and cluster analyses and generates the corresponding visualizations.

Researchers wishing to reproduce the analyses should install the relevant software and required packages, obtain access to the HRS data, and update the file paths in the scripts to reflect their local environment.

The analytic dataset is not included in this repository. Access to the underlying HRS data is subject to applicable data-use requirements.

The repository provides the analytical code used in the manuscript but does not include a standalone software application or demonstration dataset.

## Contact

Lucia Pacca, PhD  
Washington University in St. Louis  
pacca@wustl.edu
