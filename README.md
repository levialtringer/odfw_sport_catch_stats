# Explore ODFW Sport Catch Statistics

The Oregon Department of Fish & Wildlife (ODFW) sport catch statistics data are stored in cumbersome PDFs and CSVs, which limits the utility of these data. To address this, I’ve developed a simple Shiny application that allows users to easily explore trends and seasonal patterns in reported salmon and steelhead catch statistics by waterbody. While this harvest data is unverified by ODFW, it offers a heuristics for new anglers seeking information on when and where to target these species. The application can be accessed [here](https://levialtringer.shinyapps.io/odfw_sport_catch_stats/ "ODFW Sport Catch Statistics App").

All data collection and cleaning of the ODFW sport catch statistics data were perfomed in R (version 4.4.1), as was creation of the application. The files required to replicate the data collection and Shiny application are contained in this repository. These are:

  1. `clean_data.R`
  2. `app.R`
  3. data
     1. `waterbody_information.csv`
     2. `odfw.RData`

While I was able to acheive general automation in extracting and cleaning the data, slight nuances in the naming and structure of PDFs across years required some *ad hoc* solutions. In so far as the location and naming of the data on the [ODFW website](https://www.dfw.state.or.us/resources/fishing/sportcatch.asp) don't change, the code provided in `clean_data.R` should generate an `odfw.RData` file that is then used in `app.R`. Otherwise, I've provided a copy of `odfw.RData` in the repository. The `waterbody_information.csv` file is called in `clean_data.R` and simply provides a more legible waterbody name for each waterbody code.
