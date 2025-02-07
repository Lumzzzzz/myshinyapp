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
    bootswatch = "superhero"
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
      actionButton(
        inputId = "bouton",
        label = "Cliquez ici")
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
  
  rv <- reactiveValues ()
  observeEvent(c(input$taille, input$gender), {
    rv$starwars_filter <- starwars |>
      filter(height > input$taille) |>
      filter(gender == input$gender) 
  })
  
  output$StarWarsPlot <- renderPlot({
    rv$starwars_filter |>
      ggplot(aes(x = height)) + 
      geom_histogram(
        binwidth = 10, 
        fill = "#DF652A",
        color = "white"
      ) +
        labs(title = glue("Vous avez sélectionné le genre : {input$gender}"))
  })
  
  output$starWarsTitle <- renderText({
    nb_lignes <- rv$starwars_filter |>
      nrow()
    glue("Nombre de lignes sélectionnées : {nb_lignes}")
  })
  output$StarWarsTable <- renderDT({
    rv$starwars_filter
  })
  observeEvent(input$bouton, {
    message("vous avez cliqué sur le bouton")
  })
  observeEvent(input$taille, {
      showNotification(
      glue("La valeur du slider a changé pour {input$taille} !"),
      type = "message")
      })
}

shinyApp(ui = ui, server = server)