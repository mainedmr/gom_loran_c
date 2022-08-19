# Gulf of Maine Loran-C Map Layer
### Created November 16, 2020 by Bill DeVoe, Maine Department of Marine Resources, william@devoe.maine.gov

## About

This repo contains scripts and data used to make a representation of historic Loran-C coverage for the Gulf of Maine, using [NOAA Fisheries Loran Conversion Tool](https://www.fisheries.noaa.gov/resource/tool-app/loran-conversion-tool) to convert intersections of TDs. These intersections were then connected to form a reference layer of Loran coverage for the Gulf of Maine.

Not to be used for navigational purposes. Intended use is to provide contextual reference to fisheries agencies/groups during discussions where historic Loran lines are mentioned.

This layer is also available for download and as an ESRI REST service on DMR's Open Data site [here](https://dmr-maine.opendata.arcgis.com/datasets/maine::mainedmr-gulf-of-maine-loran-c-lines-9960-chain/about)

This process may be updated at a future date to utilize other Loran converters.

## Contents

- [noaa_converter_batch.R](noaa_converter_batch.R): Contains several functions for programmatically querying the NOAA Fisheries Loran Conversion tool. Most notably, the `batch_loran` accepts a dataframe of TDs and queries the NOAA tool to convert them to lat/lon. Documentation for these functions is provided in file using ROxygen.
- [make_loran_grids.R](make_loran_grids.R): Creates intersections points for the W/X/Y Loran 9960 stations in the Gulf of Maine and converts them to lat/lon, using a 10 TD interval for the W and X stations and 5 TD interval for the Y. These points are then connected and smoothed to form line respresentations. This script outputs several files.
- [loran_lines.shp](loran_lines.shp) and [loran_lines.geojson](loran_lines.geojson): ShapeFile and GeoJSON output of the above script.
- [loran_lines.html](loran_lines.html): Leaflet map displaying the output layer.
- [ne_coast.Rda](ne_coast.Rda): R Data file containing high resolution SF polygon representation of the US/Canadian coast from NY to Newfoundland.
- All the CSVs: Outputs of the various chunks of the conversion process. Most notably, [wx_grid_points.csv](wx_grid_points.csv) and [wy_grid_points.csv](wy_grid_points.csv) contain the coordinates in lat/lon and TDs for all of the line intersections.


## Layer Attribute Fields

All lines are based on the 9960 chain.

- `td`: TD of the line as integer.
- `station`: Hyphen delimited string representing the stations from which the TD was calculated, where the first character represents the station of the TD, and the second character represents the secondary station used to generate the intersections to form the hyperbole representing the TD.
- `interval`: Largest interval the TD is evenly divisible by; intended use is to draw layer with scale dependency. Interval will be one of 5, 10, 25, 50, or 100.
