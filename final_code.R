library(dplyr)
library(stringr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(reshape2)

source("Group55_Project_V2.R")
GDP_df <- GDP_flip
Netflix_df <- combo_flip

top_netflix <- function(data, year){
  yr <- as.character(year)
  filt <- select(data, Country_of_Origin, starts_with("Releases_from_") & ends_with(yr))

  order <- arrange(filt, -filt[,2])
  top_10 <- head(order, 10)
  Netflix_rank <- ggplot(data = top_10, aes(x = top_10[,2], 
                                            y = reorder(Country_of_Origin, top_10[,2])
                                            )) +
    geom_bar(stat = "identity", fill = "red") +
    ggtitle(paste("Netflix Releases Ranking in ", yr)) +
    labs(x = "Releases", y ="Country") + xlim(0, max(filt[,2]))
  
  return(Netflix_rank)
}


top_gdp <- function(data, year){
  yr <- as.character(year)
  
  filt <- select(data, Country_of_Origin, starts_with("GDP_growth_") & ends_with(yr),
                 starts_with("Releases_from_") & ends_with(yr))

  order_GDP <- arrange(filt, -filt[,2])
  top_10_GDP <- head(order_GDP, 10)
  GDP_rank <- ggplot(data = top_10_GDP, aes(x = top_10_GDP[,3],
                                            y = reorder(Country_of_Origin, top_10_GDP[,2])
                                            )) +
    geom_bar(stat = "identity", fill = "blue") +
    ggtitle(paste("Netflix Releases of the top GDP countries in ", yr)) +
    labs(x = "Releases", y = "Country") + xlim(0, max(filt[,3]))
  
  return(GDP_rank)
}

data <- project_df
yr <- "2020"

netflix_overtime <- function(data, year){
  yr <- as.character(year)
  
  filt_N <- select(data, Country_of_Origin, starts_with("Releases_from_") & ends_with(yr))
  order_N <- arrange(filt_N, -filt_N[,2])
  top_10 <- head(order_N, 10)
  name_top <- paste(c(top_10$Country_of_Origin), collapse = "|")
  net_top <- data[str_detect(data$Country_of_Origin, name_top), ]
  net_top <- net_top[!str_detect(net_top$Country_of_Origin, c("East|West")), ]
  
  net_df <- select(Netflix_df, release_year, c(net_top$Country_of_Origin))
  merge_net <- melt(net_df, "release_year")
  
  net_line <- ggplot(data = merge_net, aes(release_year, value, col = variable)) +
    geom_line() +
    ggtitle(paste("Netflix Releases over time of the top Netflix countries in ", yr)) +
    labs(x = "Year", y = "Total Releases") + ylim(0, max(merge_net$value))
  
  return(net_line)
}

gdp_overtime <- function(data, year){
  yr <- as.character(year)
  
  filt_gdp <- select(data, Country_of_Origin, starts_with("GDP_growth_") & ends_with(yr),
                     starts_with("Releases_from_") & ends_with(yr))
  order_GDP <- arrange(filt_gdp, -filt_gdp[,2])
  top_10_GDP <- head(order_GDP, 10)
  name_gdp <- paste(c(top_10_GDP$Country_of_Origin), collapse = "|")
  gdp_top <- data[str_detect(data$Country_of_Origin, name_gdp), ]
  gdp_top <- gdp_top[!str_detect(gdp_top$Country_of_Origin, c("East|West")), ]
  
  gdp <- select(Netflix_df, release_year, c(gdp_top$Country_of_Origin))
  merge_gdp <- melt(gdp, "release_year")
  
  gdp_line <- ggplot(data = merge_gdp, aes(release_year, value, col = variable)) +
    geom_line() +
    ggtitle(paste("Netflix Releases over time of the top GDP countries in ", yr)) +
    labs(x = "Year", y = "Total Releases") + ylim(0, max(merge_gdp$value))
  
  return(gdp_line)
}

