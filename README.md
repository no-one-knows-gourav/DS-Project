# CLIMATE VULNERABILITY OF INDIA: ANALYSING CHARACTERISTIC CLIMATIC PHENOMENAS  

This Shiny application provides a comprehensive visualization and statistical analysis of climate hazard vulnerability across major Indian states. It moves beyond simple hazard counting by utilizing the Normalized Vulnerability Index (NVI) for three critical and contrasting hazard types: Heatwaves, Coldwaves, and Lightning.

The dashboard is structured around four research questions, providing temporal, geographical, and anomaly-based insights for policy and risk management. 

--- 

## Reproducibility Instructions: 

### Prerequisites: 

System must have the following packages installed: 
- shiny 
- ggplot2 
- dplyr 
- markdown 
- rvest  
- httr 
- jsonlite 
- tidyverse 
- reshape2
- sf

You may want to run the following command in the console of the parent directory 

```R 
install.packages(c("shiny", "ggplot2", "dplyr", "markdown", "rvest", "httr", "jsonlite", "tidyverse", "reshape2", "sf"))
```

**Note:** installing sf may be complicated if R is configured to install from source, in case the above installation throws a non-zero exit status for sf, follow the below instructions: 

#### For MacOS 

Run the following commands in the terminal: 
```bash
brew install gdal geom proj udunits
```
Then in the R console, execute the commands: 
```R
install.packages(c("units", "sf"))
```
#### For Windows 
Make sure to have Rtools installed 
```R
install.packages("Rtools")
```
then run the command 
```R
install.packages("sf")
```
You may be prompted to download sf from source or binary, click on No 

### Data Acquisition and Preparation (Run Once) 

Due to certain package conflicts, map overlays have been included in the project directory to render the respective graphs. Other than that, the data pertaining to spatio-temporal Normalised Vulnerability Index is obtained by running `data_scraper.R` 

1. Open `data_scraper.R` file in RStudio
2. Execute the command 
```R
source("data_scraper.R")
``` 
in the console 

### Running the Shiny App (locally) 

1. Open `app.R` file in RStudio 
2. Click the **Run App** button or execute the command 
```R 
shiny::runApp("app") 
``` 

### Dashboard URL 

The dashboard can be accessed via the following link. The Dashboard has been deployed on ShinyApps.io 

https://no-one-knows-gourav.shinyapps.io/nvi_analysis/ 

