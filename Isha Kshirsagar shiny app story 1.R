library(shiny)
library(ggplot2)
library(dplyr)
library(stringr)
library(rworldmap)

# Visualization 1: World Map

# Load data
og_df <- read.csv("project_region.csv", stringsAsFactors = FALSE)

og_df <- og_df %>%  mutate(Country_of_Origin = ifelse(Country_of_Origin == 'United States', 'USA', Country_of_Origin))
og_df <- og_df %>%  mutate(Country_of_Origin = ifelse(Country_of_Origin == 'United Kingdom', 'UK', Country_of_Origin))

# Join og_df data to the world shapefile
world_shape <- map_data("world") %>% # load world shapefile
  rename(Country_of_Origin = region) %>% # rename for joining
  left_join(og_df, by="Country_of_Origin")

# Draw the map setting the `fill` of each country using its number of Netflix releases
ggplot(world_shape) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = Releases_from_2020),
    color = "black", # show country outlines
    size = .1        # thinly stroked
  ) +
  coord_map() + # use a map-based coordinate system
  scale_fill_continuous(low = "#DAF5FB", high = "#03045E") +
  labs(title = "Countries of Origin of Netflix Content Releases in 2020", 
       x = "Longitude", y = "Latitude", fill = "Number of Netflix Releases in 2020") +
  blank_theme # variable including map styles

# Define a minimalist theme for maps
blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),        # remove axis lines
    axis.text = element_blank(),        # remove axis labels
    axis.ticks = element_blank(),       # remove axis ticks
    axis.title = element_blank(),       # remove axis titles
    plot.background = element_blank(),  # remove gray background
    panel.grid.major = element_blank(), # remove major grid lines
    panel.grid.minor = element_blank(), # remove minor grid lines
    panel.border = element_blank()      # remove border around plot
  )

  
#----------------------------------------------------------------------------------------------

# Visualization 2: Pie Chart

pie_chart_fn <- function(top_x_countries){
  custom_title <- paste("Top", top_x_countries, "Countries for Netflix Content Releases in 2020")
  
top_countries <- og_df %>%
  group_by(Country_of_Origin) %>%
  summarise(Releases_from_2020 = sum(Releases_from_2020, na.rm = TRUE)) %>%
  arrange(desc(Releases_from_2020)) %>%
  slice_head(n = top_x_countries)  # get the top 10 countries with highest population

# Plot the pie chart
pie_chart <- ggplot(top_countries, aes(x = "", y = Releases_from_2020, fill = reorder(Country_of_Origin, Releases_from_2020))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(x = "", y = "", fill = "Country") +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = custom_title)

return(pie_chart)
}
