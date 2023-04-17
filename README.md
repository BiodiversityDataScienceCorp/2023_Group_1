## Group 1 Applied Data Science Practicum 2023
## Repository for Making Species Occurrence and Distribution maps for the *Rana Boylii* - Foothill Yellow Legged Frog 

![](https://github.com/BiodiversityDataScienceCorp/2023_Group_1/blob/main/images/rboyliiam.jpg)

### Elizabeth EbadiRad, Abby Wood, Molly Hennelly, Ahmed Abdalla Ahmed Esmail

## 🐸 Overview 🐸 

#### 	The foothill yellow legged frog, *Rana Boylii* was first described by Baird in 1854. It is part of the kingdom Animalia, the phylum Chordata, the class Amphibia, the order Anura, and the family Ranidae. It is in a subgenus called pacific brown frogs, which consists of the northern red legged frog, the cascades frog, the California red legged frog, the Columbia spotted frog, the (northern) mountain yellow legged frog, the Oregon spotted frog, and the (southern) Sierra Nevada yellow legged frog. It is similar to the mountain and Sierra Nevada yellow legged frogs, but lives at different altitudes and has morphological differences. 
#### It is a medium sized frog, with webbing on its hind legs. Unlike other frogs, its skin has a distinctive rough texture. Its coloration is highly variable and can be gray, brownish, olive, and sometimes even red. The red coloring distinguishes it from the other yellow legged frogs, which don’t have a red phenotype. It has a light colored band on the top of its head. There is dark spotting on the throat and chest. The underside of the rear legs and abdomen are yellow, giving it its name.
![](https://github.com/BiodiversityDataScienceCorp/2023_Group_1/blob/main/images/rboyliivent.jpg)
#### The foothill yellow legged frog is found in riparian habitats. It is often found in streams and rivers with rocky substrate and sunny banks. These habitats are located in forests, chaparral, and woodlands. 
#### The original range of the frog was from Oregon, west of the cascades, down to southern California. It occurs with the red legged frog along the northern and central coasts of California.  Used to occur in southern california but doesn’t anymore. There are also some reports of an isolated population in south central california but that is thought to be extinct. Populations in California on the north coast and the northern Sierra Nevada are said to be the healthiest. Its range overlaps with the other yellow legged frogs, and with red legged frogs, all of whom are part of the same subgenus of pacific brown frogs. 

### This repository serves to examine occurrences and the current and future distributions of the *Rana Boylii* populations in North America. Data from the Global Biodiversity Information Facility (GBIF), an open-access source of biodiversity data from several sources (including, but not limited to, iNaturalist, USGS, and museum collections), was utilized in this project.

## 🐸 Dependencies 🐸
The following additional R packages are required 

+ dismo
+ maps
+ maptools
+ raster
+ rJava
+ geodata
+ spocc
+ tidyverse

## 🐸 Structure 🐸

### 📁 data: contains data used for occurence and distribution maps and natural history information

##### [`Natural History Description.docx`](https://github.com/BiodiversityDataScienceCorp/2023_Group_1/blob/main/data/Natural%20History%20Description.docx) Document describing taxonomy, key identification characterisitcs, habitats, and historic range of *Rana Boylii* 

##### [`ranaData.csv`](https://github.com/BiodiversityDataScienceCorp/2023_Group_1/blob/main/data/ranaData.csv) Data harvested from [GBIF](https://www.gbif.org/) for Foothill Yellow Legged Frog (*R. Boylii*)

##### [`cmip5`]() Forecast climate data from [WorldClim](http://www.worldclim.org). These data were originally downloaded from the WorldClim website, but stored in the `.RData` format for ease of use. The data are for the year 2070. (_note_: this folder is not under version control, but will be created by running `currentSDM.R`)

##### `wc2-5`Climate data from [WorldClim](http://www.worldclim.org) (_note_: this folder is not under version control, but will be created by running `currentSDM.R`) 

### 📁 images: contains imgaes used in repository and documents

##### [`rboyliiam.jpg`](https://github.com/BiodiversityDataScienceCorp/2023_Group_1/blob/main/images/rboyliiam.jpg) photo at the beginning of repository from [California Herps](https://californiaherps.com/frogs/pages/r.boylii.html)

##### [`rboyliivent.jpg`](https://github.com/BiodiversityDataScienceCorp/2023_Group_1/blob/main/images/rboyliivent.jpg) second photo in repository from [California Herps](https://californiaherps.com/frogs/pages/r.boylii.html)

### 📁 output: contains generated maps

##### [`currentSDM.jpg`](https://github.com/BiodiversityDataScienceCorp/2023_Group_1/blob/main/output/currentSDM.jpg)Species distribution map of current conditions of *R. Boylii* in the western United States (California, Nevada, and Oregon), produced by `currentSDM.R`

##### [`futureSDM.jpg`](https://github.com/BiodiversityDataScienceCorp/2023_Group_1/blob/main/output/futureSDM.jpg) Species distribution map of future conditions (70 years from now) of *R. Boylii* in the western United States (California, Nevada, and Oregon), produced by `futureSDM.R`

##### `occurrenceMapRBoylii.jpg` Occurrence map of *R. Boylii* in the western United States (California, Nevada, and Oregon), produced by `gbif.R`

### 📁 scripts: contains R scripts for gathering occurrence data, running forecast models, and creating map outputs

##### `currentSDM.R` Code for producing species distribution map of current conditions of *R. Boylii* in the western United States (California, Nevada, and Oregon)

##### `futureSDM.R` Code for producing species distribution map of future conditions of *R. Boylii* in the western United States (California, Nevada, and Oregon)

##### `gbif.R` Code for producing occurrence map of *R. Boylii* in the western United States (California, Nevada, and Oregon)

### `LICENSE` BSD 2-Clause License - BiodiversityDataScienceCorp licensing information

### `README.md` this file

## 🐸 Running the Code 🐸

### Run the code in the following order:

##### `gbif.R`

##### [`currentSDM.jpg`](https://github.com/BiodiversityDataScienceCorp/2023_Group_1/blob/main/output/currentSDM.jpg)

##### [`futureSDM.jpg`](https://github.com/BiodiversityDataScienceCorp/2023_Group_1/blob/main/output/futureSDM.jpg)

## 🐸 Additional Resources 🐸

Reference repository from Applied Data Science Practicum 2022 [link](https://github.com/BiodiversityDataScienceCorp/milkfli-mapping)

US Fish and Wildlife *Rana Boylii* [website](https://www.fws.gov/species/foothill-yellow-legged-frog-rana-boylii) 

California Herps *Rana Boylii* [website](https://californiaherps.com/frogs/pages/r.boylii.html#description)

Global Biodiversity Information Facility's (GBIF) *Rana Boylii* [website](https://www.gbif.org/species/2426814)

Lewis and Clark College [website](https://www.lclark.edu/)

University of Arizona [website](https://www.arizona.edu/)