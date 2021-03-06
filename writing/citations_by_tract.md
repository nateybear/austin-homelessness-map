Just a note to remind myself of how the panel dataset was compiled.

## Courts

First, data was found on the data.austintexas.gov portal. There is "caseload" data for the two courts serving these citations in Austin, the Downtown Austin Community Court (DACC) and Austin Municipal Court (AMC). Found years 2017-2019 for DACC and 2015-2019 for AMC. Judicial records request necessary for the rest of the DACC data.

## Identifying particular citations

I downloaded them to the data folder and gave them standardized file names. Then I had to filter out only the citations I'm interested in. I used the data from Texas Observer to get the list of offense descriptions that are used by AMC and DACC. (The Observer data is just a concatenation of the spreadsheets on the [Dropbox](https://www.dropbox.com/sh/nrmu408a98ogs6e/AABj1VCQCgTKivZHXrK7CkHSa?dl=0&preview=Austin.zip) from their article)

## Geocoding (see geocoding.Rmd)

Then I had to geocode the street address from the CSVs to obtain a latitude and longitude. The package ggmap provides an R interface to the Google Maps API that is very easy to use, but requires an API key from Google. Out of 5500+ addresses, there was ONE that Google was not able to parse. Google does do some "guessing," so a data quality check would be to go back and find the count of unique latitude/longitude combinations in the google_cache.csv file and use suspiciously common combinations to reverse engineer address queries that led to those outputs. For example, insufficiently informative addresses like "I35 Austin, Texas" may be coded to some arbitrary point in downtown Austin. We should check those in the original data to see if we can either make them a more informative address, or drop them.

After geocoding, need to use the FCC's block FIPS API to go from a latitude and longitude to block FIPS. The block FIPS is more specific than a census tract, but contains a census tract within it. We also cache results here to avoid too many HTTP requests.

## Duplicates

There were 1672 duplicates because there was overlap between years. Needed to cull before saving