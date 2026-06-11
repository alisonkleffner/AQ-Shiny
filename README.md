# AQ-Shiny Usage Instructions

AQ-Shiny is a web-based application intended for the exploration, comparison, and prediction of PM2.5 using spatial and spatio-temporal models. It was built under R (4.5.2) and R studio (2026.01.1+403) using R Shiny version 1.13.0.


### Repository Layout:

- app.R: Shiny app code for creation of AQ-Shiny
- manifest.json: R Package information used in AQ-Shiny
- example-data folder: some toy data sets so users can test out AQ-Shiny


### Instructions: Download AQ-Shiny on local computer

To use AQ-Shiny on your own computer:
- Download "app.R" which contains all of the code used to create AQ-Shiny. 
- Version of R Packages used in creation of AQ-Shiny can be found in "manifest.json", so for best use of AQ-Shiny make sure necessary packages are updated to the proper version.
- Open "app.R" in R Studio and click "Run App" to get AQ-Shiny to appear in a separate window.



### Instructions: Use Deployed AQ-Shiny in Posit Connect Cloud

A fully deployed, web-based version of AQ-Shiny using the free tier of Posit Connect Cloud can be found [here](https://019ead44-ba6b-8287-8f7f-e2f4d87d9aeb.share.connect.posit.cloud). 
- Access to R on your computer is not necessary to access this version of AQ-Shiny.


### Version Notes:

- This is the first version of AQ-Shiny (Version 1.0.0). New features will be added to AQ-Shiny in future versions (like the ability to include covariates). As AQ-Shiny is updated, new version information can be found here.
- If you would like to request a feature or report a bug: submit a GitHub issue to help us keep track of them.



Please cite this repository if AQ-Shiny was used in any published work.