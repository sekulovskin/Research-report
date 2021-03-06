# Research Report
 
This repository contains all of the necessary code to fully reproduce the results presented in the paper: **Research Report**: ***Prior Sensitivity of Null Hypothesis Bayesian Testing in the Context of Two-level Models***

## Content:

 - the data stored as  `tutorial.sav`; 
 - `wrapper_function.R`, containing the programmed wrapper for testing hypotheses for the parameters of two-level models, using the `R` package `bain`;
 - `Sensitivity_analysis.R` containining the code for the *senitivity analysis* (including the plot presented in the paper);
 - A folder `Paper` which containts:
     - the markup manuscript of the paper (`Research-report.Rmd`);
     - a ready-to-read pdf (`Research-report.pdf`);
     - `BibTex` file (`refs.bib`) containing all the citations used in the paper.

## Instructions:
Download the repository as a `zip` folder, extract it, store it on your local computer and then run the `Sensitivity_analysis.R` scripit. Do not forget to set the working directory in the same folder where the data (`tutorial.sav`) and the wrapper function (`wrapper_function.R`) are stored, otherwise the code will not run!

## Notes:
If you wish to obtain the data directly from the `R2MLwiN` package, you are required to use the variable names specified in that version of the data (this is also noted in `Sensitivity_analysis.R`), thus you are advised to use the provided version of the data (`tutorial.sav`). It is still the same data set, the only difference is that the `tutorial.sav` data available in this repository containts only the (renamed) variables used for the purpose of the analysis.
