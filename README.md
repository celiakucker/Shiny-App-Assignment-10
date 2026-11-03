# Shiny-App-Assignment-10
## Author
Celia Kucker

## Overview
This project analyzes fatal traffic crashes in the United States from 2023 to 2024 using data from the National Highway Traffic Safety Administration (NHTSA) Fatality Analysis Reporting System (FARS). This Shiny app allows users to explore patterns and trends in fatal crashes across states, weather conditions, and vehicle types.

## Data Sources
The data used in this project comes from https://www.nhtsa.gov/data and uses fatal crash data from 2023 and 2024, including accident and vehicle information.

## Features
**Vehicle Types:** Compares the number of vehicles involved in fatal crashes by vehicle type.
**Weather & Road Conditions:** Explores the relationship between weather conditions and road surface conditions in fatal crashes.
**State Comparison:** Displays the distribution of fatal crashes across the U.S. using an interactive map.
Users can filter by year, state, and weather condition.

## How to Run the Shiny App
1. Install R and RStudio
2. download this repository
3. Install required R packages:
  install.packages(c("shiny", "bslib", "dplyr", "ggplot2", "arrow", "plotly", "usmap"))
4. Open project in RStudio and run App.
