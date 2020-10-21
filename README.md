# thesis-data-analysis

## Contents

* [Thesis data analysis overview](#thesis-data-analysis-overview)
  * [Important outcomes](#important-outcomes)
  * [Associated repositories](#associated-repositories)
* [Repository structure](#repository-structure)
  * [Data analysis workflow](#data-analysis-workflow)
  * [Data](#data)
* [Programming environment details](#programming-environment-details)
  * [Python](#python)
  * [R](#r)
* [Sonic Landscapes](#sonic-landscapes)

## Thesis data analysis overview

Data analysis scripts for the master's thesis of Sampo Vesanen.

This repository contains the scripts written for my University of Helsinki master's thesis, *Parking private cars and spatial accessibility in Helsinki Capital Region – Parking time as a part of the total travel time*.

The thesis is available as PDF at University of Helsinki E-thesis: https://ethesis.helsinki.fi/en/ **(NB: the thesis not yet published)**

To promote openness and transparency in science, the thesis, its development history, and all of the data analysis is available for viewing here on GitHub. This repository is the landing page, if you will, for all of the separate repositories created for the thesis.

In 2019, a survey was conducted for my thesis. The collected data was then preprocessed in Python. The preprocessed survey data was finally used as input in several analysis applications written in R, using Shiny, the web application platform package.

### Important outcomes

The raw data collected for this thesis are available here:

* **[Survey responses (csv)](../master/thesis_data_python/records.csv)**
* **[Survey visitors (csv)](../master/thesis_data_python/visitors.csv)**

The web applications created for this thesis are available for viewing:

* **Survey web application: https://parking-survey.socialsawblade.fi**
* **Survey data analysis and statistics: https://sampoves.shinyapps.io/analysis/**
* **Survey visitors data analysis: https://sampoves.shinyapps.io/visitors/**
* **Travel time comparison application: https://sampoves.shinyapps.io/comparison/**

### Associated repositories

The other repositories for this thesis are as follows:

| Repository | Description | Language(s) |
| --- | --- | --- |
| https://github.com/sampoves/Masters-2020 | The Master's thesis | LaTeX |
| https://github.com/sampoves/parking-in-helsinki-region | The web based survey application | HTML, Javascript, PHP |
| https://github.com/sampoves/thesis-analysis-shinyapps | shinyapps.io deployment of the survey data analysis and statistics application | R |
| https://github.com/sampoves/thesis-visitors-shinyapps | shinyapps.io deployment of the visitors analysis application | R |
| https://github.com/sampoves/thesis-comparison-shinyapps | shinyapps.io deployment of the travel time comparison application | R |

During the process of creating the thesis, the following side products came into being:

| Repository | Description | Language(s) |
| --- | --- | --- |
| https://github.com/sampoves/leaflet-map-survey-point | A variant of the survey application. Users place points on the map instead of postal code areas | HTML, Javascript, PHP |
| https://github.com/sampoves/msc-thesis-template | A barebones LaTeX thesis template in the style required by department of geosciences and geography in the University of Helsinki in late 2020 | LaTeX |

## Repository structure

The contents of this repository work in a linear fashion. From the starting point of the raw research data, one script uses the outcomes of other scripts until a end result, the Shiny applications are produced. Please see the following table (or thesis figure 3, pp. 13) for more information.

### Data analysis workflow

| Order | Main script file | Associated script files | Input | Output |
| --- | --- | --- | --- | --- |
| 1 | [thesis_data_anonymisation.py](../master/thesis_data_anonymisation.py) | – | Raw survey data with IP addresses | Survey data with IP addresses anonymised: [records.csv](../master/thesis_data_python/records.csv), [visitors.csv](../master/thesis_data_python/visitors.csv) |
| 2 | [thesis_data_analysis.py](../master/thesis_data_analysis.py) | [thesis_data_analysis_funcs.py](../master/thesis_data_analysis_funcs.py), [thesis_data_zipcodes.py](../master/thesis_data_zipcodes.py) | Survey data with IP addresses anonymised, various spatial data | Preprocessed survey data for later visualisation in R: [records_for_r.csv](../master/thesis_data_r/records_for_r.csv), [visitors_for_r.csv](../master/thesis_data_r/visitors_for_r.csv) |
| 3 | [thesis_stats_vis.R](../master/thesis_stats_vis.R) | [thesis_stats_vis_funcs.R](../master/thesis_stats_vis_funcs.R), [thesis_stats_vis_script.js](../master/thesis_stats_vis_script.js), [thesis_stats_vis_style.css](../master/thesis_stats_vis_style.css) | records_for_r.csv, visitors_for_r.csv and various spatial data | the Shiny web applications: [records app](https://sampoves.shinyapps.io/analysis/), [visitors app](https://sampoves.shinyapps.io/visitors/) |
| 4a | [thesis_data_traveltime_conv.R](../master/thesis_data_traveltime_conv.R) | – | [Helsinki Region Travel Time Matrix 2018](https://blogs.helsinki.fi/accessibility/helsinki-region-travel-time-matrix-2018/) | Efficiently compressed travel time data: [TTM18_postal](../master/TTM18_postal) |
| 4b | [thesis_data_traveltime.R](../master/thesis_data_traveltime.R) | [thesis_data_traveltime_funcs.R](../master/thesis_data_traveltime_funcs.R), [thesis_data_traveltime_info.html](../master/thesis_data_traveltime_info.html), [thesis_data_traveltime_script.js](../master/thesis_data_traveltime_script.js), [thesis_data_traveltime_style.css](../master/thesis_data_traveltime_style.css), [thesis_data_traveltime_tooltip.html](../master/thesis_data_traveltime_tooltip.html) | TTM18_postal, records_for_r.csv, visitors_for_r.csv and various spatial data | the Shiny web application: [comparison app](https://sampoves.shinyapps.io/comparison/) |

### Data

| File | Purpose in this thesis | Original data | Web address | 
| --- | --- | --- | --- |
| [thesis_data_python/MetropAccess_YKR_grid_EurefFIN.shp](../master/thesis_data_python/MetropAccess_YKR_grid_EurefFIN.shp) | Match thesis survey data with Helsinki Region Travel Time Matrix 2018 | MetropAccess-YKR-grid shapefile | https://blogs.helsinki.fi/accessibility/helsinki-region-travel-time-matrix-2018/ |
| [thesis_data_python/YKRVyohykkeet2017.shp](../master/thesis_data_python/YKRVyohykkeet2017.shp) | Largest zone of urban structure in each postal code area | Yhdyskuntarakenteen vyöhykkeet 2017 (Zones of urban structure) | https://ckan.ymparisto.fi/dataset/%7B18AF2F7C-1D7E-4EBE-BB14-265FEAF91410%7D |
| [thesis_data_python/clc2018_level1_dissolve.shp](../master/thesis_data_python/clc2018_level1_dissolve.shp) | Percentage of artificial surfaces in each postal code area | CORINE Land Cover 2018 | https://ckan.ymparisto.fi/dataset/%7B0B4B2FAC-ADF1-43A1-A829-70F02BF0C0E5%7D |
| [thesis_data_python/pno_dissolve.shp](../master/thesis_data_python/pno_dissolve.shp) | Study area outer boundary | Paavo – Open data by postal code area (2018) | http://www.stat.fi/org/avoindata/paikkatietoaineistot/paavo_en.html |
| [thesis_data_python/pno_tilasto_2019.shp](../master/thesis_data_python/pno_tilasto_2019.shp) | Study area municipalities | Paavo – Open data by postal code area (2018) | http://www.stat.fi/org/avoindata/paikkatietoaineistot/paavo_en.html |
| [thesis_data_r/PKS_suuralue.kml](../master/thesis_data_r/PKS_suuralue.kml) | Visualise and analyse by the subdivisions of the study area | Metropolitan area in districts | https://hri.fi/data/en_GB/dataset/paakaupunkiseudun-aluejakokartat |
| [thesis_data_r/hcr_muns.shp](../master/thesis_data_r/hcr_muns.shp) | Study area municipalities, for visualisation | Regional population density 2012, Paavo – Open data by postal code area (2018) | https://etsin.fairdata.fi/dataset/b5a5ba2f-d79f-4746-9d92-f3bba995a46b, http://www.stat.fi/org/avoindata/paikkatietoaineistot/paavo_en.html |
| [thesis_data_r/hcr_muns_sea.shp](../master/thesis_data_r/hcr_muns_sea.shp) | Study area municipalities with open water territories, for visualisation | Regional population density 2012, Paavo – Open data by postal code area (2018) | https://etsin.fairdata.fi/dataset/b5a5ba2f-d79f-4746-9d92-f3bba995a46b, http://www.stat.fi/org/avoindata/paikkatietoaineistot/paavo_en.html |
| [thesis_data_r/hcr_muns_unreachable.shp](../master/thesis_data_r/hcr_muns_unreachable.shp) | Study area islands unreachable by car, for visualisation | Regional population density 2012, Paavo – Open data by postal code area (2018) | https://etsin.fairdata.fi/dataset/b5a5ba2f-d79f-4746-9d92-f3bba995a46b, http://www.stat.fi/org/avoindata/paikkatietoaineistot/paavo_en.html |
| [thesis_data_r/mainroads.shp](../master/thesis_data_r/mainroads.shp) | Visualise main roads in the study area | Digiroad road and street database | https://vayla.fi/en/open-data/road-network |
| [thesis_data_r/other_muns.shp](../master/thesis_data_r/other_muns.shp) | Neighboring municipalities of the study area, for visualisation | Regional population density 2012, Paavo – Open data by postal code area (2018) | https://etsin.fairdata.fi/dataset/b5a5ba2f-d79f-4746-9d92-f3bba995a46b, http://www.stat.fi/org/avoindata/paikkatietoaineistot/paavo_en.html |
| [thesis_data_r/ua2012_water.shp](../master/thesis_data_r/ua2012_water.shp) | Visualise largest lakes in the study area | Urban Atlas 2012 | https://land.copernicus.eu/local/urban-atlas/urban-atlas-2012 |
| [TTM18_postal/](../master/TTM18_postal) | Travel time data for the comparison application | Helsinki Region Travel Time Matrix 2018 | https://blogs.helsinki.fi/accessibility/helsinki-region-travel-time-matrix-2018/ |

## Programming environment details

### Python
The Python code was written and tested in Anaconda Personal Distribution 2020.02 (Python 3.7.6). The most important software packages not included in Anaconda were:

| Package | Version |
| --- | --- |
| GeoPandas | 0.5.0 |
| Shapely | 1.6.4.post1 |

### R
The R code was written in R for Windows 3.6.3 (RStudio 1.2.5033). However, all R code has been updated to in tested in R for Windows 4.0.3 (RStudio 1.3.1093). The most important software packages were:

| Package | Version |
| --- | --- |
| Shiny | 1.4.0.2 |
| ggplot2 | 3.3.0 |
| ggiraph | 0.7.7 |
| dygraphs | 1.1.1.6 |
| fst | 0.9.2 |

## Sonic landscapes

These scripts were written while immersed in the deep sonic landscapes produced by some unbelievably talented ambient, downtempo, and psychill artists and projects: [Atmoswaves](https://mindspringmusic.bandcamp.com/album/distant-horizons), [Solar Fields](https://beatspace.bandcamp.com/album/rgb-red-green-blue), [Carbon Based Lifeforms](https://carbonbasedlifeforms.bandcamp.com/album/derelicts), [Lauge](https://iboga-beatspace.bandcamp.com/album/dawn), [Stellardrone](https://stellardrone.bandcamp.com/album/light-years), [Atmoflow](https://atmoflow.bandcamp.com/album/transparence), [Connect.Ohm](https://ultimae.bandcamp.com/album/9980), [Dreamstate Logic](https://dreamstatelogic.bandcamp.com/album/era4), [Neuroq](https://mysticsound.bandcamp.com/album/neuroq-spacephoria-3), [Dreaming Cooper](https://dreamingcooper.bandcamp.com/album/exploring-the-universe), and [Distant System](https://distantsystem.bandcamp.com/album/infinite-continuum).
