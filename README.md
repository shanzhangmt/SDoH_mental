## Social Determinants of Health and Depressive Symptoms Project 

## System requirement 
### Hardware and software requirement
We used StataMP version 17 and RStudio (released at 2023.06.2) with R version 4.3.1 for data analysis in this study. The two softwares both have versions for Windows, macOS, and Linux operating systems (OS). We downloaded, installed and used them on 64-bit Windows 11 OS version 22H2. No non-standard hardware was required.
### R package requirement
We performed generalized estimation equations as our main analysis using R package function geeglm in package geepack version 1.3.9. We used function metagen in package meta version 6.5-0 for meta-analysis, function glmer in package lme4 version 1.1-34 for linear mixed-effects model, and function bife in package bife version 0.7.2 for fixed effects logistic model. R package mice version 3.16.0 was used for multiple imputation.

## Installation guide
### Instructions
The softwares as StataMP version 17 and RStudio with R version 4.3.1 can be downloaded from their official websites (StataMP: https://www.stata.com/statamp/; RStudio and R: https://posit.co/download/rstudio-desktop/). The R packages can be installed using the following code:
install.packages('geepack')
install.packages('meta')
install.packages('lme4')
install.packages('bife')
install.packages('mice')
### Typical install time
The install time of StataMP version 17 in our Windows OS was about 4 minutes, and the install time of RStudio with R version 4.3.1 was about 2 minutes. The installation of the R packages should take several seconds to few minutes.

## Demo
We have provided a small real dataset consist of 400 observations from HRS wave 10 to 13 as an example of the six cohort datasets, filed as “data_HRS_Demo.dta”. We also uploaded the R code for data analysis, including generalized estimation equations (GEE), subgroup analysis, meta-analysis, five sets of sensitivity analyses, and interaction effect analysis. The provided R code was designed for HRS dataset, thus other researchers can run with the data and code to obtain results. The data structure and code were similar among the six cohort studies. The expected outputs were presented in the manuscript and supplementary material. The run time for demo vary across analyses, which should less than a few minutes.

## Instruction for use
The basic operations of the Stata and RStudio software can be accessed through relevant resources. Other researchers would highly likely to run the models successfully with these knowledges using the uploaded dataset and code. The researcher can contact the author if there was any problem (Email: shan_zhangmt@pku.edu.cn).


