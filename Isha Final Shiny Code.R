# Load libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(reshape2)

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

# Shiny App:

# Define UI for application that renders the plot
my_ui <- fluidPage(
  # Application title
  titlePanel("Number of Netflix Releases By Country"),
  
  # Select number of countries for pie chart
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        inputId = "num_countries",
        label = "Select a number of countries to view in the pie chart",
        min = 1,
        max = 60,
        value = 10
      ),
      checkboxInput(
        inputId = "USA",
        label = "Include the United States?",
        value = TRUE
      ),
    sidebarPanel(
      br(),
      h1("Takeaway Message:"),
      p(strong("As observed through both visualizations, it is obvious that the United States is a large and significant
         outlier when it comes to its contributions to the Netflix content library.")),
      p("The United States produced more content in 2020 than the next ten countries combined. 
        This makes sense, as the US is home to both the world's largest filmmaking industry - Hollywood - 
        and Netflix's origins."),
      p("However, we should be very mindful of the US's outlier status while performing further analysis to avoid our findings
        being too inaccurately skewed."),
    )
    ),
    
    
    # Display the plot in the main panel
    mainPanel(
      plotOutput(outputId = "pie_chart"),
      strong("Hover over the map to view exact numbers of Netflix releases for each country."), 
      p(strong("You can also zoom in and zoom out using buttons in the top right corner.")),
      plotlyOutput(outputId = "world_map")
    )
    
  ),
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
    interactive_world_map <- ggplot(world_shape) +
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
    ggplotly(interactive_world_map)
  })
  
  #---------------------------------------------------------------------
  
  #Pie chart:
  
  output$pie_chart <- renderPlot({
    top_x_countries <- input$num_countries
    pie_chart_fn(top_x_countries, input$USA)
    
  })
  
}


# Start running the application
shinyApp(ui = my_ui, server = my_server)