# What we are supposed to do


1) import data for clickstreams and orders <br>

2) clean imported data
* set appropriate variable types
* mark NA values
* delete empty columns (only NAs)

3) create plots<br>
* simple univariate descriptive plots
* time series
* grouped plots for comparisons
* more specific plots, e.g., distribution of revenue across products or
product categories (long tail), Lorentz-curve

4) further analysis; some ideas
* try to merge clickstreams and orders
* customer segmentation (recency, frequency, monetary value)
* product clustering
* streams of customers between product clusters (sankes diagram)
* sequential patterns in clickstreams and orders

# Regarding Programming in R and Python

All exercises should be done in R _and_ Python. For the project, the following
rules apply:


Step  | R or Python
--|--
Import, manipulate  |  both, R: `tidyverse`, mostly `dplyr` , Python: `NumPy`, `Pandas`
Plots  |  up to the teams, but the packages should implement the Grammer of Graphics (R: `ggplot2`, Python: `plotnine`)
Documentation, Table  |  up to the teams
Inference  |  up to the teams
Prediction |  up to the teams (probably better in Python)
