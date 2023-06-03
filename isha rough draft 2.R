# Load libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)

# Load data
og_df <- read.csv("project_region.csv", stringsAsFactors = FALSE)

og_df <- og_df %>%
  mutate(
    Country_of_Origin = ifelse(Country_of_Origin == 'United States', 'USA', Country_of_Origin),
    Country_of_Origin = ifelse(Country_of_Origin == 'United Kingdom', 'UK', Country_of_Origin)
  )

# Join og_df data to the world shapefile
world_shape <- map_data("world") %>%
  rename(Country_of_Origin = region) %>%
  left_join(og_df, by = "Country_of_Origin")

# Define UI for application that renders the plot
my_ui <- fluidPage(
  # Application title
  titlePanel("Number of Netflix Releases By Country"),
  
  # Select number of countries for pie chart
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        inputId = "num_countries",
        label = "Select a number of countries to view",
        min = 1,
        max = 200,
        value = 19
      )
    )
    ,
    
    # Display the plot in the main panel
    mainPanel(
      plotlyOutput(outputId = "pie_chart"),
      plotlyOutput(outputId = "world_map")
    )
  )
)

# Define server that renders the plot
my_server <- function(input, output) {
  
  # Define a plot to render in the UI
  output$world_map <- renderPlotly({
    # Define a minimalist theme for maps
    blank_theme <- theme_bw() +
      theme(
        axis.line = element_blank(),
        # remove axis lines
        axis.text = element_blank(),
        # remove axis labels
        axis.ticks = element_blank(),
        # remove axis ticks
        axis.title = element_blank(),
        # remove axis titles
        plot.background = element_blank(),
        # remove gray background
        panel.grid.major = element_blank(),
        # remove major grid lines
        panel.grid.minor = element_blank(),
        # remove minor grid lines
        panel.border = element_blank()      # remove border around plot
      )
    
    # Create the ggplot object
    p <- ggplot(world_shape) +
      geom_polygon(
        mapping = aes(
          x = long,
          y = lat,
          group = group,
          fill = Releases_from_2020
        ),
        color = "black",
        size = .1
      ) +
      scale_fill_continuous(low = "#DAF5FB", high = "#03045E") +
      labs(
        title = "Countries of Origin of Netflix Content Releases in 2020",
        x = "Longitude",
        y = "Latitude",
        fill = "Number of Netflix Releases in 2020"
      ) +
      blank_theme # variable including map styles
    
    # Convert ggplot object to plotly
    ggplotly(p)
  })
  
  output$pie_chart <- renderPlotly({
    pie_chart_fn <- function(top_x_countries){
      custom_title <- paste("Top", top_x_countries, "Countries for Netflix Content Releases in 2020")
      
      top_countries <- og_df %>%
        group_by(Country_of_Origin) %>%
        summarise(Releases_from_2020 = sum(Releases_from_2020, na.rm = TRUE)) %>%
        arrange(desc(Releases_from_2020)) %>%
        slice_head(n = top_x_countries)  # get the top 10 countries with highest population
      
      # Plot the pie chart
      pie_chart_plot <- ggplot(top_countries, aes(x = "", y = Releases_from_2020, fill = reorder(Country_of_Origin, Releases_from_2020))) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y", start = 0) +
        labs(x = "", y = "", fill = "Country") +
        theme_classic() +
        theme(axis.line = element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              plot.title = element_text(hjust = 0.5)) +
        labs(title = custom_title)
      
      return(pie_chart_plot)
      
      # Convert the ggplot object to plotly
      ggplotly(pie_chart)
    }
  })
  
}


# Start running the application
shinyApp(ui = my_ui, server = my_server)