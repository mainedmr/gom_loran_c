# Libraries
require(rvest)
require(tidyverse)
library(rvest)
library(tidyverse)

#' @title Convert LORAN-C to latitude and longitude
#'
#' @description Passes a single pair of LORAN-C time differences to the NOAA
#' LORAN-C converter website.
#'
#' @author Bill DeVoe, Maine Department of Marine Resources, william.devoe@@maine.gov
#'
#' @rdname td_to_ll
#' @param loran1 First TD
#' @param loran2 Second TD
#' @param chain The LORAN-C chain to use, defaults to LORAN9960
#' @return Vector of length 2, containing the longitude and latitude; &nbsp if
#' result invalid.
#' @export
#' @import rvest
#' @import tidyverse
td_to_ll <- function(loran1, loran2, chain = 'Loran9960') {
  options(stringsAsFactors = F)
  # Check valid chain arg
  chains <- c('Loran9960', 'Loran7980', 'Loran5930')
  # if (!(chain %in% chains)) {
  #   stop('Invalid LORAN chain input.')
  # }
  # URL to the NOAA LORAN converter
  #url <- 'https://fish.nefsc.noaa.gov/loranconv/latlonarea.pl'
  url <- 'https://apps-nefsc.fisheries.noaa.gov/loranconv/latlonarea.shtml'
  # Start a web session and get the form
  session <- html_session(url)
  form <- html_form(session)[[1]]
  # Fill in the form
  form_filled <- form %>%
    set_values(# Set the LORAN chain
               ConvTypeName0 = chain[1],
               ConvTypeName1 = chain[2],
               ConvTypeName2 = chain[3],
               ConvTypeName3 = chain[4],
               ConvTypeName4 = chain[5],
               ConvTypeName5 = chain[6],
               ConvTypeName6 = chain[7],
               ConvTypeName7 = chain[8],
               ConvTypeName8 = chain[9],
               ConvTypeName9 = chain[10],
               # Set the output to decimal degrees
               ConvTypeOutName0 = 'D.d',
               ConvTypeOutName1 = 'D.d',
               ConvTypeOutName2 = 'D.d',
               ConvTypeOutName3 = 'D.d',
               ConvTypeOutName4 = 'D.d',
               ConvTypeOutName5 = 'D.d',
               ConvTypeOutName6 = 'D.d',
               ConvTypeOutName7 = 'D.d',
               ConvTypeOutName8 = 'D.d',
               ConvTypeOutName9 = 'D.d',
               # Input the loran vectors
               begLatLoran1Name0 = loran1[1],
               begLatLoran1Name1 = loran1[2],
               begLatLoran1Name2 = loran1[3],
               begLatLoran1Name3 = loran1[4],
               begLatLoran1Name4 = loran1[5],
               begLatLoran1Name5 = loran1[6],
               begLatLoran1Name6 = loran1[7],
               begLatLoran1Name7 = loran1[8],
               begLatLoran1Name8 = loran1[9],
               begLatLoran1Name9 = loran1[10],
               begLatLoran2Name0 = loran2[1],
               begLatLoran2Name1 = loran2[2],
               begLatLoran2Name2 = loran2[3],
               begLatLoran2Name3 = loran2[4],
               begLatLoran2Name4 = loran2[5],
               begLatLoran2Name5 = loran2[6],
               begLatLoran2Name6 = loran2[7],
               begLatLoran2Name7 = loran2[8],
               begLatLoran2Name8 = loran2[9],
               begLatLoran2Name9 = loran2[10])
  # Submit the form and get a response
  response <- submit_form(session, form_filled)
  # The response is in raw binary format, parse it into html and parse the table
  parsed <- readBin(response$response$content, what = 'character') %>%
    minimal_html() %>%
    html_node("table") %>%
    html_table(fill = T)
  # Grab the lat and lon from the table
  lat <- parsed$X18[4:13]
  lon <- parsed$X21[4:13]
  # Return as vector
  return(data.frame(lon = lon, lat = lat))
}

# Function to iterative over a dataframe and convert each TD
batch_loran <- function(data, loran1, loran2, chain,
                        latitude, longitude) {
  # Checks
  if (!('data.frame' %in% class(data))) {
    stop('Data argument must be interpretable as data.frame')
  }
  if (!(loran1 %in% colnames(data))) {
    stop(sprintf('Column %s from loran1 arg missing from input data.', loran1))
  }
  if (!(loran2 %in% colnames(data))) {
    stop(sprintf('Column %s from loran1 arg missing from input data.', loran1))
  }
  if (missingArg(latitude)) {
    data$latitude <- 0
    latitude <- 'latitude'
  }
  if (missingArg(longitude)) {
    data$longitude <- 0
    longitude <- 'longitude'
  }
  # Iterate over dataframe, converting each TD
  for (i in 1:ceiling(nrow(data) / 10)) {
    s <- ifelse((i - 1) * 10 == 0, 1, (i - 1) * 10)
    f <- ifelse(s + 9 > nrow(data), nrow(data), s + 9)
    f <- ifelse(f == 10, 9, f)
    message(paste0(s, ':', f))
    coords <- td_to_ll(data[[loran1]][s:f], data[[loran2]][s:f], data[[chain]][s:f])
    data[[longitude]][s:f] <- ifelse(coords$lon[1:(f-s+1)] == '&nbsp', NA, coords$lon[1:(f-s+1)])
    data[[latitude]][s:f] <- ifelse(coords$lat[1:(f-s+1)] == '&nbsp', NA, coords$lat[1:(f-s+1)])
  }
  data[[latitude]] <- as.numeric(data[[latitude]])
  data[[longitude]] <- as.numeric(data[[longitude]])
  return(data)
}


#### Example of usage
# Dataframe with columns of TDs
# last TD is nonsense and the converter returns null
tds <- data.frame(loran1 = c(13600.75, 15600, 13680, 14000, 13600.75),
                  loran2 = c(25938.66, 27000, 25500, 26000, 500000))
# Set the chain, in this case they are all 9960
tds$chain <- 'Loran9960'

# To convert the TDs in a dataframe, pass the dataframe to batch_loran as the
# first argument - give the column names of the two loran columns and the chain
# column as the 2nd, 3rd, and 4th arg. The return value is the same dataframe,
# but with added columns for latitude and longitude.
tds_conv <- batch_loran(tds, 'loran1', 'loran2', 'chain')


