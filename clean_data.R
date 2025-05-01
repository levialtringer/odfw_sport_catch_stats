
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%          PARSING AND CLEANING THE OREGON DEPARTMENT OF FISH AND WILDLIFE (ODFW) SPORT CATCH STATISTICS           %%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


# UPDATED: 4/29/2025


# This script pulls, parses, and cleans the ODFW sport catch statistics from 
# https://www.dfw.state.or.us/resources/fishing/sportcatch.asp and turns it into usable data. 

# As the ODFW states:
#   "The reports on this page represent harvest statistics gathered from sports harvest angler tags (punch cards) 
#   "returned by anglers to ODFW. This sports harvest data has not been verified by ODFW and may be inaccurate for 
#   "several reasons. Errors may arise from anglers incorrectly reporting locations, dates, and/or species of catches; 
#   "or from errors in data entry caused by difficult-to-read harvest cards."

# The result of running this script is a dataframe that contains monthly reported sport catch data for nearly 230 water 
# bodies in Oregon over the 1996-2024 period.

# The structure of the script is:
# 1) PREAMBLE (setting up the R environment)
# 2) FUNCTIONS (User-written functions to pull, parse, and clean the ODFW sport catch data)
# 3) DATA COMPILING (Compiling the yearly dataframes and saving into one master dataframe)
# 4) FINAL DATA CLEANING (Final data cleaning adjustments like consistent waterbody naming across years)



# PREAMBLE ----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


# Clear environment:
rm(list = ls())

# Load necessary libraries for parsing the ODFW catch data:
pkgs <- c("pdftools", "stringr", "tidyverse", "dplyr", "doBy")
installed_pkgs <- pkgs %in% rownames(installed.packages())
if (any(!installed_pkgs)) {
  install.packages(pkgs[!installed_pkgs], repos = "https://cloud.r-project.org")
}
invisible(lapply(pkgs, library, character.only = TRUE))
rm(pkgs, installed_pkgs)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#



# FUNCTIONS ----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# User written function to parse the 1996-2012 ODFW PDFs:
parse_old_pdf <- function(my_pdf, year) {
  
  # This function reads the ODFW sport catch PDFs for 1996-2012 and returns a dataframe of the data
  # contained in the PDFs. The two input arguments are:
  #   1) The link (url) or file path of the PDF to be parsed.
  #   2) The year that the data represent.
  
  # Read the PDF into R environment:
  PDF <- pdf_text(my_pdf) %>% readr::read_lines()
  # Remove unnecessary white spaces (tabs) from text:
  PDF <- PDF[c(4:length(PDF))] %>% str_squish()
  # Turn each line of PDF into a dataframe row:
  df <- plyr::ldply(PDF)
  # Remove the rows that don't contain the relevant catch data:
  df <- subset(df, !grepl("Jan Feb", df$V1, fixed = T)) 
  df <- subset(df, !grepl("Revised", df$V1, fixed = T)) 
  df <- subset(df, !grepl("Date", df$V1, fixed = T))
  df <- subset(df, !grepl("Page", df$V1, fixed = T))
  df <- subset(df, !grepl("Codes", df$V1, fixed = T))
  df <- subset(df, !grepl("Coastal", df$V1, fixed = T))
  df <- subset(df, V1!="")
  # Remove the space from the species identifiers (for now):
  df$V1 <- str_replace(df$V1, "Other Salmon", "OtherSalmon")
  df$V1 <- str_replace(df$V1, "Other salmon", "OtherSalmon")
  df$V1 <- str_replace(df$V1, "Spring Chinook", "SpringChinook")
  df$V1 <- str_replace(df$V1, "Fall Chinook", "FallChinook")
  df$V1 <- str_replace(df$V1, "Winter Steelhead", "WinterSteelhead")
  df$V1 <- str_replace(df$V1, "Summer Steelhead", "SummerSteelhead")
  # Generate dataframe variables after every white space in each line of text: 
  df <- df %>% separate(V1, c(letters[1:14]), sep = " ")
  # Generate water body code and name variables:
  df$wb_code <- NA
  df$wb_name <- NA
  df <- df[,c("wb_code", "wb_name",letters[1:14] )]
  df$wb_code <- ifelse(as.numeric(df$a) %in% c(1:300), df$a, df$wb_code)
  df$wb_name <- str_remove_all(paste(df$b, df$c, df$d, df$e, df$f, df$g, df$h, df$i, df$j, df$k, df$l, df$m), " NA")
  df$wb_name <- ifelse(str_count(df$wb_name,"\\d")>5, NA, df$wb_name)
  df <- df %>% fill(wb_code, .direction = "down")
  df <- df %>% fill(wb_name, .direction = "down")
  # Remove unnecessary (empty) rows of data:
  df <- subset(df, !is.na(n))
  # Name columns of dataframe:
  colnames(df) <- c("WB_CODE", "WB_NAME", "SPECIES", "JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", 
                    "SEP", "OCT", "NOV", "DEC", "TOTAL")
  # Turn the columns containing numbers into numeric types:
  for (i in c(1,4:16)) {
    df[,i] <- str_replace(df[,i], ",", "")
    df[,i] <- as.numeric(df[,i])
  }
  # Turn data frame long:
  df <- gather(df, MONTH, CATCH, JAN:TOTAL, factor_key=F)
  # Rename the last column "CATCH_(year)":
  colnames(df) <- c("WB_CODE", "WB_NAME", "SPECIES", "MONTH", "CATCH")
  df$YEAR <- year
  # Return the cleaned data frame:
  return(df)
}

# User written function to parse the 2013-2018 ODFW PDFs:
parse_new_pdf <- function(my_pdf, species, year) {
  
  # This function reads the ODFW sport catch PDFs for 2013-2018 and returns a dataframe of the data
  # contained in the PDFs. The two input arguments are:
  #   1) The link (url) or file path of the PDF to be parsed.
  #   2) The species of fish the data represent.
  #   3) The year that the data represent.
  
  # Read the PDF into R environment:
  PDF <- pdf_text(my_pdf) %>% readr::read_lines()
  # Remove unnecessary white spaces (tabs) from text:
  PDF <- PDF[c(6:length(PDF))] %>% str_squish()
  # Turn each line of PDF into a dataframe row:
  df <- plyr::ldply(PDF)
  # Remove the rows that don't contain the relevant catch data:
  df <- subset(df, !grepl("Jan Feb", df$V1, fixed = T)) 
  df <- subset(df, !grepl("Generated", df$V1, fixed = T))
  df <- subset(df, !grepl("Coastal", df$V1, fixed = T))
  df <- subset(df, !grepl("Columbia", df$V1, fixed = T))
  df <- subset(df, !grepl("TONGUE", df$V1, fixed = T))
  df <- subset(df, !grepl("Created", df$V1, fixed = T))
  df <- subset(df, V1!="")
  # Generate water body code and name variables:
  df$wb_code <- as.numeric(substr(df$V1, 1, 3))
  df$wb_name <- str_remove_all(paste(df$V1), "[,0123456789-]")
  df$wb_name <- trimws(df$wb_name, which = "left")
  # Remove unneeded (empty) data rows: 
  df <- subset(df, !is.na(wb_code))
  # Generate species variable:
  df$species <- species
  # Generate monthly catch variables:
  df$catch <- substr(df$V1, 4, 300)
  if (year>=2016) {
    df$catch <- str_remove_all(paste(df$catch), "[ABCDEFGHIJKLMNOPQRSTUVWXYZ()&./,]")
  } else {
    df$catch <- str_remove_all(paste(df$catch), "[ABCDEFGHIJKLMNOPQRSTUVWXYZ()&./,-]")
  }
  df$catch <- trimws(df$catch, which = "left")
  df <- df %>% separate(catch, c(letters[1:13]), sep = " ")
  # Remove unneeded variable:
  df$V1 <- NULL
  # Name columns of dataframe:
  colnames(df) <- c("WB_CODE", "WB_NAME", "SPECIES", "JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", 
                    "SEP", "OCT", "NOV", "DEC", "TOTAL")
  # Turn the columns containing numbers into numeric types:
  for (i in c(4:16)) {
    df[,i] <- str_replace(df[,i], ",", "")
    df[,i] <- as.numeric(df[,i])
    df[,i] <- ifelse(is.na(df[,i]), 0, df[,i])
  }
  # Turn data frame long:
  df <- gather(df, MONTH, CATCH, JAN:TOTAL, factor_key=F)
  # Rename the last column "CATCH_(year)":
  colnames(df) <- c("WB_CODE", "WB_NAME", "SPECIES", "MONTH", "CATCH")
  df$YEAR <- year
  # Return the cleaned data frame:
  return(df)
}

# User written function to parse the 2013-2018 ODFW PDFs:
parse_ports_pdf <- function(my_pdf, year) {
  
  # This function reads the ODFW sport catch PDFs for ocean ports in years 2013-2018 and returns a dataframe of the 
  # data contained in the PDFs. The two input arguments are:
  #   1) The link (url) or file path of the PDF to be parsed.
  #   2) The year that the data represent.
  
  # Read the PDF into R environment:
  PDF <- pdf_text(my_pdf) %>% readr::read_lines()
  # Remove unnecessary white spaces (tabs) from text:
  PDF <- PDF[c(4:length(PDF))] %>% str_squish()
  # Turn each line of PDF into a dataframe row:
  df <- plyr::ldply(PDF)
  # Remove the rows that don't contain the relevant catch data:
  df <- subset(df, !grepl("Jan Feb", df$V1, fixed = T)) 
  df <- subset(df, !grepl("Revised", df$V1, fixed = T)) 
  df <- subset(df, !grepl("Date", df$V1, fixed = T))
  df <- subset(df, !grepl("Page", df$V1, fixed = T))
  df <- subset(df, !grepl("Codes", df$V1, fixed = T))
  df <- subset(df, !grepl("Coastal", df$V1, fixed = T))
  df <- subset(df, V1!="")
  # Correct 2014 error in PDF
  if (year==2014) {
    df$V1[rownames(df)==95] <- paste0("15 ", df$V1[rownames(df)==95])
    df$V1[rownames(df)==96] <- str_replace(df$V1[rownames(df)==96], "15 Chinook", "Chinook")
    df$V1[rownames(df)==101] <- paste0("16 ", df$V1[rownames(df)==101])
    df$V1[rownames(df)==102] <- str_replace(df$V1[rownames(df)==102], "16 Chinook", "Chinook")
    df$V1[rownames(df)==107] <- paste0("17 ", df$V1[rownames(df)==107])
    df$V1[rownames(df)==108] <- str_replace(df$V1[rownames(df)==108], "17 Chinook", "Chinook")
    df$V1[rownames(df)==114] <- paste0("18 ", df$V1[rownames(df)==114])
    df$V1[rownames(df)==115] <- str_replace(df$V1[rownames(df)==115], "18 Chinook", "Chinook")
    df$V1[rownames(df)==120] <- paste0("19 ", df$V1[rownames(df)==120])
    df$V1[rownames(df)==121] <- str_replace(df$V1[rownames(df)==121], "19 Chinook", "Chinook")
    df$V1[rownames(df)==126] <- paste0("20 ", df$V1[rownames(df)==126])
    df$V1[rownames(df)==127] <- str_replace(df$V1[rownames(df)==127], "20 Chinook", "Chinook")
  }
  if (year>=2016) {
    df$V1 <- gsub("-", "0", df$V1)
  }
  # Remove the space from the species identifiers (for now):
  df$V1 <- str_replace(df$V1, "Other Salmon", "OtherSalmon")
  df$V1 <- str_replace(df$V1, "Other salmon", "OtherSalmon")
  # Generate dataframe variables after every white space in each line of text: 
  df <- df %>% separate(V1, c(letters[1:14]), sep = " ")
  # Generate water body code and name variables:
  df$wb_code <- NA
  df$wb_name <- NA
  df <- df[,c("wb_code", "wb_name",letters[1:14] )]
  df$wb_code <- ifelse(as.numeric(df$a) %in% c(1:300), df$a, df$wb_code)
  df$wb_name <- str_remove_all(paste(df$b, df$c, df$d, df$e, df$f, df$g, df$h, df$i, df$j, df$k, df$l, df$m), " NA")
  df$wb_name <- ifelse(str_count(df$wb_name,"\\d")>5, NA, df$wb_name)
  df <- df %>% fill(wb_code, .direction = "down")
  df <- df %>% fill(wb_name, .direction = "down")
  # Remove unnecessary (empty) rows of data:
  df <- subset(df, !is.na(n))
  # Name columns of dataframe:
  colnames(df) <- c("WB_CODE", "WB_NAME", "SPECIES", "JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", 
                    "SEP", "OCT", "NOV", "DEC", "TOTAL")
  # Turn the columns containing numbers into numeric types:
  for (i in c(1,4:16)) {
    df[,i] <- str_replace(df[,i], ",", "")
    df[,i] <- as.numeric(df[,i])
  }
  # Turn data frame long:
  df <- gather(df, MONTH, CATCH, JAN:TOTAL, factor_key=F)
  # Rename the last column "CATCH_(year)":
  colnames(df) <- c("WB_CODE", "WB_NAME", "SPECIES", "MONTH", "CATCH")
  df$YEAR <- year
  # Return the cleaned data frame:
  return(df)
}

# User written function to parse the 2019-2024 ODFW CSVs:
parse_csv_data <- function(species_list, year) {
  
  df <- data.frame(WB_CODE = integer(),
                   WB_NAME = character(),
                   SPECIES = character(),
                   MONTH = character(),
                   CATCH = integer(),
                   YEAR = integer())
  
  for(s in species_list) {
    
    if(s=="ChinookSalmon" & year==2023) {
      temp_df <- read.csv(paste0("https://www.dfw.state.or.us/resources/fishing/docs/sportcatch/",
                               s,"ByMonthAndWaterbodeCode",year,".csv"))
    } else {
      temp_df <- read.csv(paste0("https://www.dfw.state.or.us/resources/fishing/docs/sportcatch/",
                                 s,"ByMonthAndWaterbodyCode",year,".csv"))
    }
    
    temp_df <- temp_df %>% select_if(names(.) %in% c("Waterbody.Code", "Waterbody.code", "Location", "Location.Code", "Species", month.name, "Total"))
    
    if(s=="Steelhead" & year==2024) {
      colnames(temp_df) <- c("SPECIES", "WB_CODE", "WB_NAME", toupper(month.abb), "TOTAL")
    } else {
      colnames(temp_df) <- c("SPECIES", "WB_NAME", "WB_CODE", toupper(month.abb), "TOTAL")
    }
    
    temp_df <- gather(temp_df, MONTH, CATCH, JAN:TOTAL, factor_key=F)
    temp_df$YEAR <- year
    
    df <- rbind(df, temp_df)
    
  }
  
  df <- df[,c("WB_CODE", "WB_NAME", "SPECIES", "MONTH", "CATCH", "YEAR")]
  
  return(df)
  
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#



# DATA COMPILING ----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# Use the user-written functions to pull, parse, and clean each year of the ODFW data:

# All species 1996-2012:
df_1996 <- parse_old_pdf(my_pdf = "https://www.dfw.state.or.us/resources/fishing/docs/sportcatch/by_month_1996_Expanded_Catch.pdf", year = 1996)
df_1997 <- parse_old_pdf(my_pdf = "https://www.dfw.state.or.us/resources/fishing/docs/sportcatch/by_month_1997_Expanded_Catch.pdf", year = 1997)
df_1998 <- parse_old_pdf(my_pdf = "https://www.dfw.state.or.us/resources/fishing/docs/sportcatch/by_month_1998_Expanded_Catch.pdf", year = 1998)
df_1999 <- parse_old_pdf(my_pdf = "https://www.dfw.state.or.us/resources/fishing/docs/sportcatch/by_month_1999_Expanded_Catch.pdf", year = 1999)
df_2000 <- parse_old_pdf(my_pdf = "https://www.dfw.state.or.us/resources/fishing/docs/sportcatch/by_month_2000_Expanded_Catch.pdf", year = 2000)
df_2001 <- parse_old_pdf(my_pdf = "https://www.dfw.state.or.us/resources/fishing/docs/sportcatch/by_month_2001_Expanded_Catch.pdf", year = 2001)
df_2002 <- parse_old_pdf(my_pdf = "https://www.dfw.state.or.us/resources/fishing/docs/sportcatch/by_month_2002_Expanded_Catch.pdf", year = 2002)
df_2003 <- parse_old_pdf(my_pdf = "https://www.dfw.state.or.us/resources/fishing/docs/sportcatch/by_month_2003_Expanded_Catch.pdf", year = 2003)
df_2004 <- parse_old_pdf(my_pdf = "https://www.dfw.state.or.us/resources/fishing/docs/sportcatch/by_month_2004_Expanded_Catch.pdf", year = 2004)
df_2005 <- parse_old_pdf(my_pdf = "https://www.dfw.state.or.us/resources/fishing/docs/sportcatch/by_month_2005_Expanded_Catch.pdf", year = 2005)
df_2006 <- parse_old_pdf(my_pdf = "https://www.dfw.state.or.us/resources/fishing/docs/sportcatch/by_month_2006_Expanded_Catch.pdf", year = 2006)
df_2007 <- parse_old_pdf(my_pdf = "https://www.dfw.state.or.us/resources/fishing/docs/sportcatch/by_month_2007_Expanded_Catch.pdf", year = 2007)
df_2008 <- parse_old_pdf(my_pdf = "https://www.dfw.state.or.us/resources/fishing/docs/sportcatch/by_month_2008_Expanded_Catch.pdf", year = 2008)
df_2009 <- parse_old_pdf(my_pdf = "https://www.dfw.state.or.us/resources/fishing/docs/sportcatch/by_month_2009_Expanded_Catch.pdf", year = 2009)
df_2010 <- parse_old_pdf(my_pdf = "https://www.dfw.state.or.us/resources/fishing/docs/sportcatch/by_month_2010_Expanded_Catch.pdf", year = 2010)
df_2011 <- parse_old_pdf(my_pdf = "https://www.dfw.state.or.us/resources/fishing/docs/sportcatch/by_month_2011_Expanded_Catch.pdf", year = 2011)
df_2012 <- parse_old_pdf(my_pdf = "https://www.dfw.state.or.us/resources/fishing/docs/sportcatch/by_month_2012_Expanded_Catch.pdf", year = 2012)


# Spring Chinook 2013-2018:
sc_df_2013 <- parse_new_pdf(my_pdf = "https://www.dfw.state.or.us/resources/fishing/docs/sportcatch/2013_CHS_Expanded_Catch_by_Waterbody_3.pdf", species = "SpringChinook", year = 2013)
sc_df_2014 <- parse_new_pdf(my_pdf = "https://www.dfw.state.or.us/resources/fishing/docs/sportcatch/2014_CHS_Expanded_Catch_by_Waterbody_1.pdf", species = "SpringChinook", year = 2014)
sc_df_2015 <- parse_new_pdf(my_pdf = "https://www.dfw.state.or.us/resources/fishing/docs/sportcatch/2015%20CHS%20Expanded%20Catch%20by%20Waterbody%202-F.pdf", species = "SpringChinook", year = 2015)
sc_df_2016 <- parse_new_pdf(my_pdf = "https://www.dfw.state.or.us/resources/fishing/docs/sportcatch/2016%20CHS%20Expanded%20Catch%20by%20Waterbody%201,%20Prelim.pdf", species = "SpringChinook", year = 2016)
sc_df_2017 <- parse_new_pdf(my_pdf = "https://www.dfw.state.or.us/resources/fishing/docs/sportcatch/2017%20CHS%20Expanded%20Catch%20by%20Waterbody%201.pdf", species = "SpringChinook", year = 2017)
sc_df_2018 <- parse_new_pdf(my_pdf = "https://www.dfw.state.or.us/resources/fishing/docs/sportcatch/2018%20CHS%20Expanded%20Catch%20by%20Waterbody,%20No.1-P.pdf", species = "SpringChinook", year = 2018)

# Fall Chinook 2013-2018:
fc_df_2013 <- parse_new_pdf(my_pdf = "https://www.dfw.state.or.us/resources/fishing/docs/sportcatch/2013_CHF_Expanded_Catch_by_Waterbody_5.pdf", species = "FallChinook", year = 2013)
fc_df_2014 <- parse_new_pdf(my_pdf = "https://www.dfw.state.or.us/resources/fishing/docs/sportcatch/2014_CHF_Expanded_Catch_by_Waterbody_1.pdf", species = "FallChinook", year = 2014)
fc_df_2015 <- parse_new_pdf(my_pdf = "https://www.dfw.state.or.us/resources/fishing/docs/sportcatch/2015%20CHF%20Expanded%20Catch%20by%20Waterbody%202-F.pdf", species = "FallChinook", year = 2015)
fc_df_2016 <- parse_new_pdf(my_pdf = "https://www.dfw.state.or.us/resources/fishing/docs/sportcatch/2016%20CHF%20Expanded%20Catch%20by%20Waterbody%201,%20Prelim.pdf", species = "FallChinook", year = 2016)
fc_df_2017 <- parse_new_pdf(my_pdf = "https://www.dfw.state.or.us/resources/fishing/docs/sportcatch/2017%20CHF%20Expanded%20Catch%20by%20Waterbody%201.pdf", species = "FallChinook", year = 2017)
fc_df_2018 <- parse_new_pdf(my_pdf = "https://www.dfw.state.or.us/resources/fishing/docs/sportcatch/2018%20CHF%20Expanded%20Catch%20by%20Waterbody,%20No.1-P.pdf", species = "FallChinook", year = 2018)

# Summer Steelhead 2013-2018:
ss_df_2013 <- parse_new_pdf(my_pdf = "https://www.dfw.state.or.us/resources/fishing/docs/sportcatch/2013_STS_Expanded_Catch_by_Waterbody.pdf", species = "SummerSteelhead", year = 2013)
ss_df_2014 <- parse_new_pdf(my_pdf = "https://www.dfw.state.or.us/resources/fishing/docs/sportcatch/2014_STS_Expanded_Catch_by_Waterbody_1.pdf", species = "SummerSteelhead", year = 2014)
ss_df_2015 <- parse_new_pdf(my_pdf = "https://www.dfw.state.or.us/resources/fishing/docs/sportcatch/2015%20STS%20Expanded%20Catch%20by%20Waterbody%202-F.pdf", species = "SummerSteelhead", year = 2015)
ss_df_2016 <- parse_new_pdf(my_pdf = "https://www.dfw.state.or.us/resources/fishing/docs/sportcatch/2016%20STS%20Expanded%20Catch%20by%20Waterbody%201,%20Prelim.pdf", species = "SummerSteelhead", year = 2016)
ss_df_2017 <- parse_new_pdf(my_pdf = "https://www.dfw.state.or.us/resources/fishing/docs/sportcatch/2017%20STS%20Expanded%20Catch%20by%20Waterbody%201.pdf", species = "SummerSteelhead", year = 2017)
ss_df_2018 <- parse_new_pdf(my_pdf = "https://www.dfw.state.or.us/resources/fishing/docs/sportcatch/2018%20STS%20Expanded%20Catch%20by%20Waterbody,%20No.1-P.pdf", species = "SummerSteelhead", year = 2018)

# Winter Steelhead 2013-2018:
ws_df_2013 <- parse_new_pdf(my_pdf = "https://www.dfw.state.or.us/resources/fishing/docs/sportcatch/2013_STW_Expanded_Catch_by_Waterbody_4.pdf", species = "WinterSteelhead", year = 2013)
ws_df_2014 <- parse_new_pdf(my_pdf = "https://www.dfw.state.or.us/resources/fishing/docs/sportcatch/2014_STW_Expanded_Catch_by_Waterbody_1.pdf", species = "WinterSteelhead", year = 2014)
ws_df_2015 <- parse_new_pdf(my_pdf = "https://www.dfw.state.or.us/resources/fishing/docs/sportcatch/2015%20STW%20Expanded%20Catch%20by%20Waterbody%202-F.pdf", species = "WinterSteelhead", year = 2015)
ws_df_2016 <- parse_new_pdf(my_pdf = "https://www.dfw.state.or.us/resources/fishing/docs/sportcatch/2016%20STW%20Expanded%20Catch%20by%20Waterbody%201,%20Prelim.pdf", species = "WinterSteelhead", year = 2016)
ws_df_2017 <- parse_new_pdf(my_pdf = "https://www.dfw.state.or.us/resources/fishing/docs/sportcatch/2017%20STW%20Expanded%20Catch%20by%20Waterbody%201.pdf", species = "WinterSteelhead", year = 2017)
ws_df_2018 <- parse_new_pdf(my_pdf = "https://www.dfw.state.or.us/resources/fishing/docs/sportcatch/2018%20STW%20Expanded%20Catch%20by%20Waterbody,%20No.1-P.pdf", species = "WinterSteelhead", year = 2018)

# Coho 2013-2018:
c_df_2013 <- parse_new_pdf(my_pdf = "https://www.dfw.state.or.us/resources/fishing/docs/sportcatch/2013_Coho_Expanded_Catch_by_Waterbody_2.pdf", species = "Coho", year = 2013)
c_df_2014 <- parse_new_pdf(my_pdf = "https://www.dfw.state.or.us/resources/fishing/docs/sportcatch/2014_Coho_Expanded_Catch_by_Waterbody_1.pdf", species = "Coho", year = 2014)
c_df_2015 <- parse_new_pdf(my_pdf = "https://www.dfw.state.or.us/resources/fishing/docs/sportcatch/2015%20Coho%20Expanded%20Catch%20by%20Waterbody%202-F.pdf", species = "Coho", year = 2015)
c_df_2016 <- parse_new_pdf(my_pdf = "https://www.dfw.state.or.us/resources/fishing/docs/sportcatch/2016%20Coho%20Expanded%20Catch%20by%20Waterbody%201,%20Prelim.pdf", species = "Coho", year = 2016)
c_df_2017 <- parse_new_pdf(my_pdf = "https://www.dfw.state.or.us/resources/fishing/docs/sportcatch/2017%20Coho%20Expanded%20Catch%20by%20Waterbody%201.pdf", species = "Coho", year = 2017)
c_df_2018 <- parse_new_pdf(my_pdf = "https://www.dfw.state.or.us/resources/fishing/docs/sportcatch/2018%20CO%20Expanded%20Catch%20by%20Waterbody,%20No.1-P.pdf", species = "Coho", year = 2018)

# All species ports 2013-2018:
p_df_2013 <- parse_ports_pdf(my_pdf = "https://www.dfw.state.or.us/resources/fishing/docs/sportcatch/2013_Ocean_Ports_Only_Expanded_Catch_by_Waterbody.pdf", year = 2013)
p_df_2014 <- parse_ports_pdf(my_pdf = "https://www.dfw.state.or.us/resources/fishing/docs/sportcatch/2014_Ocean_Ports_Only_Expanded_Sport_Catch_by_Waterbody_1.pdf", year = 2014)
p_df_2015 <- parse_ports_pdf(my_pdf = "https://www.dfw.state.or.us/resources/fishing/docs/sportcatch/2015%20Ocean%20Ports%20Only--Expanded%20Catch%20by%20Waterbody%202-F.pdf", year = 2015)
p_df_2016 <- parse_ports_pdf(my_pdf = "https://www.dfw.state.or.us/resources/fishing/docs/sportcatch/2016%20Ocean%20Ports%20Only--Expanded%20Catch%20by%20W-body%201,%20Prelim.pdf", year = 2016)
p_df_2017 <- parse_ports_pdf(my_pdf = "https://www.dfw.state.or.us/resources/fishing/docs/sportcatch/2017%20Ocean%20Ports%20Only--Expanded%20Catch%20by%20Waterbody,%20No.1.pdf", year = 2017)
p_df_2018 <- parse_ports_pdf(my_pdf = "https://www.dfw.state.or.us/resources/fishing/docs/sportcatch/2018%20Ocean%20Ports%20Only--Expanded%20Catch%20by%20Waterbody%20No.1-P.pdf", year = 2018)

# Compile the species-specific dataframes into one dataframe for each year in 2013-2018:
df_2013 <- rbind(sc_df_2013, fc_df_2013, ss_df_2013, ws_df_2013, c_df_2013, p_df_2013)
df_2014 <- rbind(sc_df_2014, fc_df_2014, ss_df_2014, ws_df_2014, c_df_2014, p_df_2014)
df_2015 <- rbind(sc_df_2015, fc_df_2015, ss_df_2015, ws_df_2015, c_df_2015, p_df_2015)
df_2016 <- rbind(sc_df_2016, fc_df_2016, ss_df_2016, ws_df_2016, c_df_2016, p_df_2016)
df_2017 <- rbind(sc_df_2017, fc_df_2017, ss_df_2017, ws_df_2017, c_df_2017, p_df_2017)
df_2018 <- rbind(sc_df_2018, fc_df_2018, ss_df_2018, ws_df_2018, c_df_2018, p_df_2018)

# Remove species-specific dataframes:
rm(sc_df_2013, fc_df_2013, ss_df_2013, ws_df_2013, c_df_2013, p_df_2013,
   sc_df_2014, fc_df_2014, ss_df_2014, ws_df_2014, c_df_2014, p_df_2014,
   sc_df_2015, fc_df_2015, ss_df_2015, ws_df_2015, c_df_2015, p_df_2015,
   sc_df_2016, fc_df_2016, ss_df_2016, ws_df_2016, c_df_2016, p_df_2016,
   sc_df_2017, fc_df_2017, ss_df_2017, ws_df_2017, c_df_2017, p_df_2017,
   sc_df_2018, fc_df_2018, ss_df_2018, ws_df_2018, c_df_2018, p_df_2018)

# All species 2019-2024:
df_2019 <- parse_csv_data(species_list = c("ChinookSalmon", "Steelhead", "CohoSalmon"), year = 2019)
df_2020 <- parse_csv_data(species_list = c("ChinookSalmon", "Steelhead", "CohoSalmon"), year = 2020)
df_2021 <- parse_csv_data(species_list = c("ChinookSalmon", "Steelhead", "CohoSalmon"), year = 2021)
df_2022 <- parse_csv_data(species_list = c("ChinookSalmon", "Steelhead", "CohoSalmon"), year = 2022)
df_2023 <- parse_csv_data(species_list = c("ChinookSalmon", "Steelhead", "CohoSalmon"), year = 2023)
df_2024 <- parse_csv_data(species_list = c("ChinookSalmon", "Steelhead", "CohoSalmon"), year = 2024)

# Bind yearly data sets together:
list_of_dataframes <- mget(grep("df_", ls(), value = TRUE))
df <- bind_rows(list_of_dataframes); rm(list_of_dataframes); rm(list = grep("df_", ls(), value = TRUE))



# FINAL DATA CLEANING ----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# Remove unwanted species data:
df <- subset(df, !(SPECIES %in% c("OtherSalmon", "166")))

# Remove inconsistent waterbody names an bring the consistent, presentable names
df$WB_NAME <- NULL

# Recode SPECIES and MONTH variables (this removes the Spring and Fall distinction from the catch data to match the 
# 2019-present data:
df$SPECIES <- recode(df$SPECIES, SpringChinook = "Chinook",
                       FallChinook = "Chinook", 
                       SummerSteelhead = "Steelhead", 
                       WinterSteelhead = "Steelhead")
df$MONTH <- recode(df$MONTH, JAN = 1, FEB = 2, MAR = 3, APR = 4, 
                     MAY = 5, JUN = 6, JUL = 7, AUG = 8, 
                     SEP = 9, OCT = 10, NOV = 11, DEC = 12,
                     TOTAL = 0)

# Collapse data now that Spring and Fall distinction has been removed from Chinook and Steelhead data:
df <- summaryBy(CATCH ~ WB_CODE + SPECIES + MONTH + YEAR, data = df, keep.names = T, FUN = sum)

# Generate a balanced panel:
crosswalk <- 
  df[,c("WB_CODE", "SPECIES", "YEAR", "MONTH")] %>% 
  dplyr::group_by(WB_CODE) %>%
  complete(SPECIES = c("Chinook", "Coho", "Steelhead"),
           YEAR = seq(1996,2024,1), 
           MONTH = seq(0,12,1))
df <- merge(crosswalk, df, by=c("WB_CODE", "SPECIES", "MONTH", "YEAR"), all.x = T)
rm(crosswalk)

# Bring in consistent and legible waterbody names:
names_df <- read.csv("data/waterbody_information.csv")
df <- merge(df, names_df, by="WB_CODE", all.x = T); rm(names_df)
df <- df[,c("WB_CODE", "WB_NAME", "SPECIES", "MONTH", "YEAR", "CATCH")]

# Write data:
save(df, file = "data/odfw.RData")

########################################################################################################################


########################################################################################################################
########################################################################################################################
###                                                         END                                                      ###
########################################################################################################################
########################################################################################################################
