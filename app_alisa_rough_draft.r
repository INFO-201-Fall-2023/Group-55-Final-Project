library(shiny)
library(ggplot2)
source("graphs.r")
country_names <-colnames(new_releases_df)
new_country_names <- country_names[2:7]

ui <- fluidPage(
  titlePanel("Top 6 countries GDP Growth & Netflix releases"),
  sidebarLayout(
    sidebarPanel(
      h1("Control Panel"),
      selectInput(
        inputId = "country_name",
        label = "Select a country",
        choices = new_country_names
      )
    ),
     mainPanel(
    tabsetPanel(
      tabPanel("Country GDP & Netflix releases plots",plotOutput(outputId = "line_graph"),plotOutput(outputId = "line_graph_2")),
      #tabPanel("Netflix Releases Plot",plotOutput(outputId = "line_graph_2"))
    )
    
  )
  )
 
)

server <- function(input,output){
output$line_graph_2 <- renderPlot({
  line_3 <-ggplot(data=d_2, aes(x=release_year,y=value,col=variable)) +
    geom_line()
   return(line_3)
})
output$line_graph <- renderPlot({
  line_4 <-ggplot(data=d, aes(x=GDP_Growth_per_yr,y=value,col=variable)) +
    geom_line()
  return(line_4)
})
}
shinyApp(ui,server)