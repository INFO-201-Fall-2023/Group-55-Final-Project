library(shiny)

source("Group55_Project_V2.R")

ui <- fluidPage(
  titlePanel("Netflix Releases"),
  sidebarLayout(
    sidebarPanel(
      h3("Controls"),
      sliderInput(
        inputId = "yr",
        label = "Filter by year",
        min = 2000,
        max = 2020,
        value = 21

      ),
      checkboxInput(
        inputId = "US",
        label = "Include United States?",
        value = TRUE
      )
    ),
    mainPanel(
      plotOutput("netflix_plot"),
      plotOutput("gdp_plot"),
      plotOutput("region_N")
    )
  )
)

server <- function(input, output){
  
  output$netflix_plot <- renderPlot({
    year <- as.character(input$yr)
    
    filt_N <- select(project_df, Country_of_Origin, starts_with("Releases_from_") & ends_with(year))
    if (input$US == FALSE){
      filt_N <- filter(filt_N, Country_of_Origin != "United States")
    }

    filt_N[,2] <- as.numeric(filt_N[,2])
    order <- arrange(filt_N, -filt_N[,2])
    top_10 <- head(order, 10)
    Netflix_rank <- ggplot(data = top_10, aes(x = top_10[,2], y = reorder(Country_of_Origin, top_10[,2]))) +
      geom_bar(stat = "identity") +
      #coord_flip() +
      ggtitle("Netflix Releases Ranking by Year") +
      labs(x = "Releases", y ="Country")
    
    return(Netflix_rank)
  })
  
  output$gdp_plot <- renderPlot({
    year <- as.character(input$yr)
    
    filt_GDP <- select(project_df, Country_of_Origin, starts_with("GDP_growth_") & ends_with(year))
    if (input$US == FALSE){
      filt_GDP <- filter(filt_GDP, Country_of_Origin != "United States")
    }
    
    filt_GDP[,2] <- as.numeric(filt_GDP[,2])
    order_GDP <- arrange(filt_GDP, -filt_GDP[,2])
    top_10_GDP <- head(order_GDP, 10)
    GDP_rank <- ggplot(data = top_10_GDP, aes(x = top_10_GDP[,2], y = reorder(Country_of_Origin, top_10_GDP[,2]))) +
      geom_point(stat = "identity") +
      #coord_flip() +
      ggtitle("GDP Growth Ranking by Year ") +
      labs(x = "GDP growth", y = "Country")
    
    return(GDP_rank)
  })
  
  
}

shinyApp(ui, server)