library(dplyr)
library(stringr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(plotly)


# Aiman's code
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

#---------------------------------------------------------------------------
# Isha's code
# Load data
og_df <- read.csv("project_region.csv", stringsAsFactors = FALSE)

og_df <- og_df %>%
  mutate(
    Country_of_Origin = ifelse(Country_of_Origin == 'United States', 'USA', Country_of_Origin),
    Country_of_Origin = ifelse(Country_of_Origin == 'United Kingdom', 'UK', Country_of_Origin)
  )

og_df <- og_df[og_df$Releases_from_2020 != 0, ]

og_df$Country_and_ReleasesFrom2020 <- paste(og_df$Country_of_Origin, ", ", og_df$Releases_from_2020, "releases")

# Join og_df data to the world shapefile
world_shape <- map_data("world") %>%
  rename(Country_of_Origin = region) %>%
  left_join(og_df, by = "Country_of_Origin")

#Pie chart code:

pie_chart_fn <- function(top_x_countries, is_us_enabled) {
  custom_title <-
    paste("Top",
          top_x_countries,
          "Countries for Netflix Content Releases in 2020")
  
  if (is_us_enabled) {
    pie_chart_df <- og_df
  } else {
    pie_chart_df <- og_df[og_df$Country_of_Origin != 'USA', ]
  }
  top_countries <- pie_chart_df %>%
    group_by(Country_and_ReleasesFrom2020) %>%
    summarise(Releases_from_2020 = sum(Releases_from_2020, na.rm = TRUE)) %>%
    arrange(desc(Releases_from_2020)) %>%
    slice_head(n = top_x_countries)  # get the top x countries with highest population
  
  # Plot the pie chart
  pie_chart_plot <-
    ggplot(top_countries,
           aes(
             x = "",
             y = Releases_from_2020,
             fill = reorder(Country_and_ReleasesFrom2020, Releases_from_2020)
           )) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    labs(x = "", y = "", fill = "Country") +
    theme_classic() +
    theme(
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      plot.title = element_text(hjust = 1)
    ) +
    labs(title = custom_title)
  
  return(pie_chart_plot)
}

#---------------------------------------------------------------------------
#Alisa's code
gdp_df <-read.csv("GDP_countries_yr.csv")
releases_df <- read.csv("netflix_countries_yr.csv")

new_gdp_df<-gdp_df[,c("GDP_Growth_per_yr","United.States","Canada","South.Korea","Germany","United.Kingdom","India")]
for(i in 1:ncol(new_gdp_df)){
  new_gdp_df[,i]<-as.numeric(new_gdp_df[,i])
}
d <- melt(new_gdp_df,id.vars="GDP_Growth_per_yr")
line <-ggplot(data=d, aes(x=GDP_Growth_per_yr,y=value,col=variable)) +
  geom_line()
print(line)

new_releases_df<-releases_df[,c("release_year","United.States","Canada","South.Korea","Germany","United.Kingdom","India")]
country_names <-colnames(new_releases_df)
new_country_names <- country_names[2:7]
for(i in 1:ncol(new_releases_df)){
  new_releases_df[,i]<-as.numeric(new_releases_df[,i])
}
d_2 <- melt(new_releases_df,id.vars="release_year")
line_2 <-ggplot(data=d_2, aes(x=release_year,y=value,col=variable)) +
  geom_line()
print(line_2)



