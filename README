
# Lythrum Paper README

## Data

VASCAN-Lythrum.csv, bison-lythrum_salicaria.csv, and GBIF-Lythrum.csv are occurrence downloads from VASCAN, BISON, and GBIF respectively. Check these sources for information on column names and meanings.

### PopData_2018_02_08 

This is the csv containing manual measurements of herbarium specimens, extracted as in methods of paper.

Column Names and meanings:

- Pop_Code: barcode on herbarium specimen
- standard: pixel length of scale standard on herbarium image
- actual: actual length of scale
- fruit.inf.Length: pixel length of fruiting influorescence
- aborted.inf.length: pixel length of aborted influorescence
- flower.inf.length: pixel length of flowering influorescence
- bud.inf.Length: pixel length of budding influorescence
- Location: textual description of collection location
- Date: date of collection
- Latitude: GPS latitude coordinates of collection site
- Longitude: GPS longitude coordinates of collection site
- yday: Julian day of collection date 
- Year: year of collection

## Functions

- geodist.R: Calculate Geodetic distance between two lat/long points
- idw.R: Inverse distance weighing function to interpolate value at specific point (supply data frame of known observation values, and distance to specific point from known observation points)

## Scripts

### DownloadStationData.R
Downloads NOAA data and  finds closest stations to locations in data for all years in range. Saves to WeatherRawData

### CalculateSeasonMetricsbyDay.R
Reads climate data from WeatherRawData/ and calculates season metrics. This method interpolates GDD at the site using daily temperature values and interpolating season metrics to the collection site.

### Data_Prep.R

Takes csv of phenological measurements, collection date and location. Splits into different regions based on geocoordinates. Computes actual length of influorescences. Uses all four data files to get earliest recorded occurrence for cells of 0.5 lat x long, create datafile of earliest occurrences to use for krieging.

### fig_a1_krieg_2.R

Performs krieging using earliest occurrences and creates map showing interpolated spread. (Scripts takes some time to run due to krieging.)

### Data_Prep_krieg.R 
Uses krieging data from fig_a1_krieg_2.R to get interpolated establishment date for each herbarium specimen.

### Validation_data_prep.R

Prepares data downloaded from Colautti and Barret, Montague and Eckert for validation against herbarium data. Pools specimens into 0.5 latitude populations. 

### new_model.R
Fit full NLS model, and does bootstrapping to get estimates for herbarium data. Results of bootstrap are saved in an Rdata file due to amount of time needed to generate bootstraps.

### Figure scripts

combine_figs.R generates the main figure based on the phenological NLS model in the paper and the theory figure of PEM vs GEM.
fig_1_map.R generates the map figure in the paper.
fig_2_fti_gd.R generates the facet plot of FTI vs season length.
All other scripts starting with fig are for plots in appendix

### methods.R
This is the code to summarize coefficients and generate model fits as reported in paper and appendix.


## Shapefile: small & clip_map

clip_map is a shapefile of North America clipped from the world map containing the major lakes taken from Natural Earth
at small data scale. 

small contains a simplified version used in the data analysis after putting the clip_map sp through ms_simplify.




