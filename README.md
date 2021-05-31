# Eurofound: Living, Working and Covid-19 survey data visualisation

1. [Project Motivation](#motivation)
2. [Data](#data)
3. [File Descriptions](#files)
4. [Structure](#structure)
4. [Results](#results)

## Project Motivation<a name="motivation"></a>
In March and April 2020, [Eurofound](https://www.eurofound.europa.eu/) launched a 10-minute e-survey to capture the most immediate changes and their impact of Covid-19 on people's quality of life and work. Tow further rounds of the survey, with more questions, have been implemented in June/July 2020 and February/March 2021. This repository contains the code for the data visualisation of the data on the Eurofound website.  

## Data<a name="data"></a>
The data used are the results of the Eurofound survey. Currently the data are not publicly available. The data was collected with [soSci](https://www.soscisurvey.de/).

## File Descriptions <a name="files"></a>
This repository contains the following files:
Data preparation

* merge_waves123.R: script for merging the datasets of the three waves of the survey into one datafile (calls cleaning_wave1.R , cleaning_wave 2.R, cleaning wave_3.R)
* create_reference_list.R: creates a list object that contains all variable names, descriptions and other attributes. This is used later on in the webapp to choose labels and texts. Updates the existing file.
* label_and_recode.R: script for recoding and labelling the raw data from the API
* weighting_by_country.R: script for weighting the data.

App
* app.R: main R Shiny app including ui and server. Run this script to start the app. It loads several helper functions listed below
* make_data.R: script for calculating the data for the maps and plots. Its output is also the data that can be downloaded with the 'download data' button.
* make_description.R: script for creating the text under the plot. It generates text describing what is shown in the plot and which categories have been excluded if any.
* make_map.R: script for creating the map using Leaflet and the shapefile in the 'data' folder.
* make_plot.R: script for making plotly plot. 

Dashboard
/dashboard: contains files for a separate app tracking the survey response while open (wave 1).

The data folder is not included in this Github repository because the raw data is not public. 

## Results<a name="results"></a>
The web visualisation can be found on the [Eurofound website](https://www.eurofound.europa.eu/data/covid-19). It is deployed on shinyapps.io. The benchmark that is built in to the app is only available to respondents to the survey. 

## Authors <a name="files"></a>

The author of the dataviz and the dashboard for the first round of the survey is Mathijn Wilkens. The data preparation was done by Eszter Sandor for all three waves and Mathijn Wilkens contributed to it for the first wave. The updates for the app and complementing scripts for the second and third round have been made by Eleonora Peruffo (wave 2 and 3) and  Christopher White (wave 3).
