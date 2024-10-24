## GUIDE TO SCRIPTS AND DATA
See google doc section here: https://docs.google.com/document/d/1UO8U8SbEmjn5-ZJiG06v6y_bGqGIeJXaWeM63hopaG0/edit?tab=t.z6zsxus96gkl#bookmark=id.46tnzbgiawnc

## GITIGNORE
Data is not tracked as files are too large for git

## Notes on Data and Variable Names
*Pasted from official BIOTIME paper and its Supporting Information*
There are five main levels of organization: record, sample, plot, site and study. A record is our fundamental unit of observation of the abundance of a species in a sample. A sample includes all the records that belong to the same sampling event; for example, a quadrat on the seashore, a single plankton tow or a bird transect. A sample is defined by a single location and a single date. If the exact location has been repeatedly sampled through time, then all the samples that correspond to that location belong to the same plot. Multiple samples and plots can be located in the same area, which we term a site. Finally, the highest observational unit is a study, which is defined by having a regular and consistent sampling methodology. Sources of data in which the sampling methodology changed during the course of the study were classified as separate studies. 
Some Variable Definitions from https://onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1111%2Fgeb.12729&file=geb12729-sup-0002-suppinfo_02.pdf
- SAMPLE_DESC: The sample ID provided from the original data (if any), or a concatenation of relevant fields to determine the spatial and temporal sampling event;
e.g. latitude_longitude_year_month_day_depth
- PLOT: Plot identifier (if any), this can be name of quadrat/plot/site or a concatenation of plot with separate areas, e.g. PLOT1/Q7, PLOT1/Q8
- DEPTH: Depth or elevation in metres (if available)
- GRAIN_SIZE_TEXT: Size of spatial grain in text if available
- GRAIN_SQ_KM: Size of spatial grain in square km if available
- AREA_SQ_KM: Area of study in square km
- ABUNDANCE_TYPE: Description of abundance measurement, e.g if it's count, density, presence/absence, or mean count

## WALKING THROUGH KEY CODE IN PROP_TREND.R
Please see the text under section "25/9/2024 Clarifying methodology and issues" in this link: https://docs.google.com/document/d/1UO8U8SbEmjn5-ZJiG06v6y_bGqGIeJXaWeM63hopaG0/edit?usp=sharing
