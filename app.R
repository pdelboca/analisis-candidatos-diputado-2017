## app.R ##
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(tidytext)
library(purrr)
library(readr)
library(lubridate)
library(DT)
library(RColorBrewer)

# Carga de datos Global: Esto corre una vez al iniciar la aplicacion
CargarDatos <- function(){
  data <- dir(".", pattern = "posts.csv", recursive = TRUE) %>%
    map(read_csv) %>%
    reduce(rbind)
  data
}

datosCandidatos <- CargarDatos()
stopWords <- scan("http://www.webmining.cl/wp-content/uploads/2011/03/stopwords.es.txt", character())
stopWords <- c(stopWords, c("san", "<NA>","http"))

ui <- dashboardPage(
  dashboardHeader(title = "Candidatos Córdoba 2017", titleWidth = 300),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Candidatos", tabName = "candidatos", icon = icon("users")),
      menuItem("Discursos", tabName = "discurso", icon = icon("table"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "candidatos",
        fluidRow(
          box(dateRangeInput("inputFechas", label="Fechas Posts:" ,start=min(datosCandidatos$created_time), end=max(datosCandidatos$created_time)), width = 3),
          valueBoxOutput("likesValueBox", width = 3),
          valueBoxOutput("commentsValueBox", width = 3),
          valueBoxOutput("sharesValueBox", width = 3)
        ),
        fluidRow(
          box(radioButtons(inputId = "candidato", "Canidato:", choices = unique(datosCandidatos$from_name)), width = 2),
          box(plotOutput("likes"), width = 7, title = "Historial de Likes del Candidato"),
          box(dataTableOutput("rankingPalabras"), width = 3, title = "Palabra mas Usada")
        ),
        fluidRow(
          box(dataTableOutput("evolucionPalabrasCandidato"), width = 12, title = "Evolucion Palabras del Candidato (Por Cuatrimestre)")
        ),
        fluidRow(
          box(dataTableOutput("postsCandidato"), width = 12, title = "Post del Candidato")
        )
      ),
      tabItem(tabName = "discurso",
        fluidRow(
          box(tableOutput("tablaComparativaPalabras"), width = 12, title = "Palabras más usadas por los Candidatos")
        )
      )
    )
  )
)

server <- function(input, output) {
  # Info Boxes
  output$likesValueBox <- renderValueBox({
    likes <- datosCandidatos %>% 
      filter(from_name == input$candidato) %>%
      summarise(total = sum(likes_count)) %>%
      select(total)
    valueBox(likes$total, "Likes", icon = icon("thumbs-up"), color = "blue")
  })
  
  output$commentsValueBox <- renderValueBox({
    comments <- datosCandidatos %>% 
      filter(from_name == input$candidato) %>%
      summarise(total = sum(comments_count)) %>%
      select(total)
    valueBox(comments$total, "Comments", icon = icon("comments"), color = "green")
  })
  
  output$sharesValueBox <- renderValueBox({
    shares <- datosCandidatos %>% 
      filter(from_name == input$candidato) %>%
      summarise(total = sum(shares_count)) %>%
      select(total)
    valueBox(shares$total, "Shares", icon = icon("share"), color = "maroon")
  })
  
  # Gráfico de Likes  
  dataLikes <- reactive({
    datosCandidatos %>% 
      filter(from_name == input$candidato)
  })

  output$likes <- renderPlot({
    ggplot(dataLikes(), aes(x=created_time, y=likes_count)) + 
      geom_line() +
      geom_smooth() +
      labs(y =  "Cantidad de Likes", 
           x =  "Fecha") +
      theme_minimal()
  })
  
  # Tabla de Palabras
  output$rankingPalabras <- renderDataTable({
    dat <- datosCandidatos %>%
      filter(from_name == input$candidato) %>% 
      select(message) %>%
      unnest_tokens(palabra, message) %>%
      filter(!(palabra %in% stopWords)) %>%
      filter(!(is.na(palabra))) %>%
      count(palabra, sort=TRUE) %>%
      slice(1:10) %>%
      mutate(ranking = row_number()) %>% 
      select(ranking, palabra)
  
    tabla <- datatable(dat, options = list(dom = 't', pageLength = 10)) %>% 
      formatStyle(colnames(dat[-1]),
                  backgroundColor = styleEqual(dat$palabra, brewer.pal(n = 10, name = "Set3"))
      )
    return(tabla)
  })
  
  # Tabla Posts Candidato
  output$postsCandidato <- renderDataTable({
    datosCandidatos %>%
      filter(from_name == input$candidato) %>%
      mutate(linkHTML = paste0("<a href=",link,">Link</a>")) %>%
      arrange(desc(likes_count)) %>%
      select(from_name, created_time, message, likes_count, comments_count, shares_count, linkHTML)
  },
  options = list(lengthMenu = c(5, 10), pageLength = 5), escape = FALSE)
  
  # Tabla Evolucion de Palabras
  output$evolucionPalabrasCandidato <- renderDataTable({
    principalesPalabras <- datosCandidatos %>%
      filter(from_name == input$candidato) %>% 
      select(message) %>%
      unnest_tokens(palabra, message) %>%
      filter(!(palabra %in% stopWords)) %>%
      filter(!(is.na(palabra))) %>%
      count(palabra, sort=TRUE) %>%
      slice(1:10) %>%
      mutate(ranking = row_number()) %>% 
      select(ranking, palabra)
    
    dat <- datosCandidatos %>%
      filter(from_name == input$candidato) %>%
      select(created_time, message) %>%
      mutate(mes = quarter(created_time, with_year=TRUE)) %>%
      unnest_tokens(palabra, message) %>%
      filter(!(palabra %in% stopWords)) %>%
      filter(!(is.na(palabra))) %>%
      group_by(mes) %>%
      count(palabra) %>%
      arrange(mes, desc(n)) %>% 
      slice(1:15) %>%
      mutate(ranking = row_number()) %>%
      select(mes, palabra, ranking) %>% 
      tidyr::spread(mes, palabra)
    
    tabla <- datatable(dat, options = list(dom = 't', pageLength = 15)) %>% 
      formatStyle(colnames(dat[-1]),
                  backgroundColor = styleEqual(principalesPalabras$palabra,
                  brewer.pal(n = 10, name = "Set3"))
                  )
    
    return(tabla)
  })
  
  # Tabla comparativa de Palabras
  output$tablaComparativaPalabras <- renderTable({
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

shinyApp(ui, server)
