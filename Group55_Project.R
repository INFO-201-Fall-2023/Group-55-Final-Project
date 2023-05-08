library(dplyr)
library(stringr)
library(tidyr)
library(tidyverse)

# Load Netflix and GDP dataset
netflix <- read.csv("netflix_titles.csv")
GDP <- read.csv("GDP_growth.csv")

# Filter releases that are missing country
netflix_clean_df <- filter(netflix, country != "")

# Separate releases with multiple countries into their own row for each country
netflix_df <- separate_longer_delim(netflix_clean_df, country, ", ")
netflix_df$country <- gsub(",", "", netflix_df$country)

# Only create dataframe with releases that were released in the 2000s
yr_2000s <- netflix_df[netflix_df$release_year >= 2000, ]

# Group then summarize the total rleases by country and release year
netflix_country_yr <- group_by(yr_2000s, release_year, country )
by_year_country <- summarize(netflix_country_yr, total_releases = n())
#print(by_year_country)

# Create a summary of the total releases by year
netflix_yr <- group_by(yr_2000s, release_year)
total_by_yr <- summarize(netflix_yr, total_releases = n())
#print(total_by_yr)

# Create a new dataframe only for making columns by years
year_col <- as.data.frame(t(total_by_yr))
colnames(year_col) <- c(2000:2021)
year_col <- year_col[-1,]
colnames(year_col) <- paste("Releases_from_", colnames(year_col), sep = "")


# Same process as before but this time create rows by country's name
netflix_country <- group_by(netflix_df, country)
total_by_country <- summarize(netflix_country, total_releases = n())
#print(total_by_country)
country_only <- total_by_country[, -2]

# Merge the country and years columns for the ideal dataframe look
combo <- merge(country_only, year_col)
combo <- filter(combo, country != "") # filter any missing country
combo[1:nrow(combo), 2:ncol(combo)] <- NA # clean the new dataframe to insert new values

# Make a function that find the total shows of each country at any given year
shows <- function(what_country, what_yr){
    info <- subset(by_year_country, country == what_country & release_year == what_yr)
    tot_releases <- sum(info$total_releases)
    return(tot_releases)
}

# Create a for loop function that replace all missing values
# with the number of shows calculated by the shows() function
for (n in 1:ncol(combo[,-1])){
  combo[, (2+(n-1))] <- mapply(shows, combo$country, (2000+(n-1)))
}

# Merge the GDP dataframe with the clean Netflix dataframe
colnames(GDP) <- gsub("X", "GDP_growth_", as.character(colnames(GDP)))
GDP$Country <- gsub("Korea", "South Korea", GDP$Country) # For some reason, Korea is listed as one country
                                                          # To make it easy, assume Korea as South Korea
df <- merge(combo, GDP, by.x = "country", by.y = "Country", all.x = TRUE)
colnames(df) <- gsub("country", "Country_of_Origin", as.character(colnames(df)))
project_df <- filter(df, !is.na(df$Releases_from_2000)) # remove countries that aren't included in Netflix dataset

