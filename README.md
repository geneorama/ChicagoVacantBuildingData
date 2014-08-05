ChicagoVacantBuildingData
=========================

This project is an example of how to use open source tools to explore data on Chicago's open data portal, specifically data related to vacant and / or abandoned buildings.


We will be using:
 - R http://www.r-project.org/
 - RStudio http://www.rstudio.com/products/rstudio/download/
 - git http://git-scm.com/ (If you are using windows, also be sure to install git bash)

Some relevant data sources:
 - Main open data portal for Chicago: https://data.cityofchicago.org/
 - Building code violations: https://data.cityofchicago.org/Buildings/Building-Violations/22u3-xenr
 - Vacant and abandoned building violations issued on properties owned by financial institutions: https://data.cityofchicago.org/Buildings/Vacant-and-Abandoned-Buildings-Violations/kc9i-wq85
 - Vacant and Abandoned Buildings Service Requests: http://www.cityofchicago.org/city/en/depts/bldgs/dataset/vacant_and_abandonedbuildingsservicerequests.html 
 - Vacant Building Register: https://ipiweb.cityofchicago.org/VBR/

Please update packages and install the geneorama pacakge with the following commands in R:

NOTE: On Windows you will need R Tools for devtools to work:
http://cran.r-project.org/bin/windows/Rtools/installer.html

```{r}
## Might have run Rstudio as admin (in terminal):
##    sudo rstudio
## Linux users might have to install these first (in terminal):
##    sudo apt-get -y build-dep libcurl4-gnutls-dev
##    sudo apt-get -y install libcurl4-gnutls-dev
update.packages()
## Only run if you don't already have devtools installed:
install.packages('devtools')
## Load devtools library and use it to install geneorama
library('devtools')
devtools::install_github('geneorama/geneorama')
```


