# Research-report
 
This repository contains all of the necessary code to fully reproduce the results presented in the paper: **Research Report: Prior Sensitivity of Null Hypothesis Bayesian Testing in the Context of Two-level Models**

## Content:
This repository containts: 
 - the data stored as  `exam.sav`; 
 - `wrapper_function.R`, containing the programmed wrapper for testing hypotheses from two-level models using the package `bain`;
 - `Sensitivity_analysis.R` containining the code from the *senitivity analysis*.
 - `Research-report.tex` containing the research paper along with the code for the tables.

## Instructions:
Download the repository as a `zip` folder, extract it, store it on your local computer and then run the `Sensitivity_analysis.R` scripit (don't forget to set the working directory where the other two remaining files are stored, otherwise the code won't run).


Please note, if you wish to obtan the data directly from the `R2MLwiN` package, you are required to use the variable names specified in that version of the data (this is also noted in `Sensitivity_analysis.R`). It is till the same data set, the only difference is that the `exam.sav` data available in this repository, only containts the (renamed) variables used for the purpose of the analysis.
