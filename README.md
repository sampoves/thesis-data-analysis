# thesis-data-analysis
Data analysis scripts for the master's thesis of Sampo Vesanen.

This repository contains the scripts written for my University of Helsinki master's thesis, *Parking private cars and spatial accessibility in Helsinki Capital Region – Parking time as a part of the total travel time*.

The thesis is available as PDF at University of Helsinki E-thesis: https://ethesis.helsinki.fi/en/ **(NB: the thesis not yet published)**

To promote openness and transparency in science, the thesis, its development history, and all of the data analysis is available for viewing here on GitHub. This repository is the landing page, if you will, for all of the separate repositories created for the thesis.

In 2019, a survey was conducted for my thesis. The collected data was then preprocessed in Python. The preprocessed survey data was finally used as input in several analysis applications written in R, using Shiny, the web application platform package.

The web applications created for this thesis are available for viewing:

* Survey web application: https://parking-survey.socialsawblade.fi
* Survey data analysis and statistics: https://sampoves.shinyapps.io/analysis/
* Survey visitors data analysis: https://sampoves.shinyapps.io/visitors/
* Travel time comparison application: https://sampoves.shinyapps.io/comparison/

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

The contents of this repository work in a linear fashion. From the starting point of the raw research data, one script uses the outcomes of other scripts until a end result, the Shiny applications are produced. Please see the following table for more information.

| Script file | Data used | Outcome |
| [thesis_data_anonymisation.py](../blob/master/thesis_data_anonymisation.py) | --- | --- |
| --- | --- | --- |
| --- | --- | --- |
| --- | --- | --- |
| --- | --- | --- |
| --- | --- | --- |

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
