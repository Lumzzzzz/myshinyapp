library(shiny)

ui <- fluidPage(
  titlePanel("My First Shiny App"),
  sidebarLayout(
    sidebarPanel(
      h1("Star Wars Characters"),
      h2("My app from scratch"),
      sliderInput("taille",
        label = "Height of characters",
        min = 0,
        max = 250,
        value = 30
      )
    ),
    mainPanel(
      plotOutput("StarWarsPlot")
    )
  )
)

server <- function(input, output) {
  output$StarWarsPlot <- renderPlot({
    starwars |>
      filter(height > input$taille) |>
      ggplot(aes(x = height)) + 
      geom_histogram(
        bindwidth = 10, 
        fill = "darkgray",
        color = "white"
      )
  })
}

shinyApp(ui = ui, server = server)