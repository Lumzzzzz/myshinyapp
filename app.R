library(shiny)
library(ggplot2)
library(glue)
library(dplyr)
library(DT)
library(shinylive)
library(bslib)
library(thematic)

thematic_shiny(font = "auto")

ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bootswatch = "journal"
  ),
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
      ),
      selectInput(
        inputId = "gender",
        label = "Choisir le genre des personnages",
        choices = c("masculine", "feminine")
      ),
    ),
    mainPanel(
      textOutput(
        outputId = "starWarsTitle"
      ),
      plotOutput("StarWarsPlot"),
      DTOutput(
        outputId = "StarWarsTable"
      )
    )
    
  )
)

server <- function(input, output) {
  output$StarWarsPlot <- renderPlot({
    starwars |>
      filter(height > input$taille) |>
      filter(gender == input$gender) |>
      ggplot(aes(x = height)) + 
      geom_histogram(
        bindwidth = 10, 
        fill = "white",
        color = "black"
      ) +
        labs(title = glue("Vous avez sélectionné le genre : {input$gender}"))
  })
  output$starWarsTitle <- renderText({
    nb_lignes <- starwars |>
      filter(height > input$taille) |>
      filter(gender == input$gender) |>
      nrow()
    glue("Nombre de lignes sélectionnées : {nb_lignes}")
  })
  output$StarWarsTable <- renderDT({
    starwars |>
    filter(height > input$taille) |>
    filter(gender == input$gender) 
  })
}

shinyApp(ui = ui, server = server)