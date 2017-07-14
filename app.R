library(shiny)
library(dplyr)
library(ggplot2)
library(tidytext)
library(purrr)
library(readr)

# Carga de datos Global: Esto corre una vez al iniciar la aplicacion
CargarDatos <- function(){
  data <- dir(".", pattern = "posts.csv", recursive = TRUE) %>%
    map(read_csv) %>%
    reduce(rbind)
  data
}

datosCandidatos <- CargarDatos()
stopWords <- scan("http://www.webmining.cl/wp-content/uploads/2011/03/stopwords.es.txt", character())
stopWords <- c(stopWords, c("san", "<NA>"))


# UI
ui <- fluidPage(titlePanel("Likes Historicos de los Candidatos"),
                dateRangeInput('dateRange',
                               label = 'Rango de Fechas a Visualizar',
                               start = min(datosCandidatos$created_time), 
                               end = max(datosCandidatos$created_time)
                ),
                plotOutput("likes"),
                titlePanel("Ranking de palabras más usadas por los Candiatos"),
                tableOutput("rankingPalabras"))

# SERVER
server <- function(input, output){
  # Gráfico de Likes  
  dataLikes <- reactive({
    datosCandidatos %>% 
      filter(created_time > as.POSIXct(input$dateRange[1]), 
             created_time < as.POSIXct(input$dateRange[2]))
  })
  output$likes <- renderPlot({
    ggplot(dataLikes(), aes(x=created_time, y=likes_count, color=from_name)) + 
      geom_line() +
      labs(y =  "Cantidad de Likes", 
           x =  "Fecha",
           title = "Historial de Likes de los Candidatos",
           subtitle = "Datos extraidos de los perfiles públicos de los candidatos.") +
      theme_minimal()
  })
  
  # Tabla de Palabras
  output$rankingPalabras <- renderTable({
    datosCandidatos %>% 
      select(from_name, message) %>%
      unnest_tokens(palabra, message) %>%
      filter(!(palabra %in% stopWords)) %>%
      filter(!(is.na(palabra))) %>%
      group_by(from_name) %>%
      count(palabra) %>%
      arrange(from_name, desc(n)) %>%
      slice(1:15) %>%
      mutate(ranking = row_number()) %>%
      select(from_name, palabra, ranking) %>%
      tidyr::spread(from_name, palabra)
  })
}

shinyApp(ui = ui, server = server)