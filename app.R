library(shiny)
library(shinydashboard)
library(shinyfullscreen)
library(plotly)
library(dplyr)
library(tidyr)
library(haven)
library(fst)
library(DT)
library(stringr)
library(shinyWidgets)
library(shinyBS)
library(shinyjs)
library(leaflet)
library(sp)
library(sf)
library(htmltools)

logo <- "marca_agencia.png"

ui <- dashboardPage(
  dashboardHeader(
    title = div(
      span(
        tags$a(href = "https://www.agenciaeducacion.cl/", target = "_blank",
               img(src = logo, height = 100)
        ), 
        style = "float: left; margin-left: 50px; margin-bottom: 5px; margin-top: 5px;"
      ),
      span(
        "Explorador Simce", 
        style = "color: #00495f; font-size: 30px; font-weight: bold; line-height: 80px; display: inline-block;"
      )
    )
    ,
    titleWidth = "100%",
    tags$li(class = "dropdown",
            tags$style(".main-header {height: 90px !important; max-height: 90px !important; background-color: white !important; border-bottom: 1px solid #dcdcdc !important;}"),
            tags$style(".main-header .logo {height: 110px !important; background-color: white !important;}"),
            tags$style(".sidebar-toggle {height: 90px !important; padding-top: 25px !important; background-color: white !important;}"),
            tags$style(".navbar {min-height: 90px !important; background-color: white !important;}")
    )
  ),
  dashboardSidebar(
    div(style = "padding: 10px;",
        h3("Filtros", style = "color: #00495f; text-align: center;"),
        sliderInput("agno", "Seleccione Rango de Años:", 
                    min = 2012, max = 2023, 
                    value = c(2015, 2023), sep = "", post = " "),
        selectInput("asig", "Seleccione Asignatura:", 
                    choices = c("Matemática" = "mate", "Lectura" = "lect"), 
                    selected = "lect"),
        selectInput("grado", "Seleccione Grado:", 
                    choices = c("4b", "2m"), 
                    selected = "4b")
    ),
    div(
      hr(),
      div("Versión 1.0.0", 
          style = "color: white; font-size: 12px; font-style: italic; text-align: center; padding: 5px;"),
      style = "padding: 10px;"
    ),
    
    width = 250
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    useShinyjs(),
    
    fluidRow(
      column(
        width = 12,
        div(style = "margin-bottom: 15px; margin-left: 250px; display: flex; gap: 100px; justify-content: center;",
            bsButton("btn_nacional", "Nacional", icon = icon("flag"), style = "default", class = "btn-custom"),
            bsButton("btn_genero", "Género", icon = icon("venus-mars"), style = "default", class = "btn-custom"),
            bsButton("btn_dependencia", "Ed. Pública", icon = icon("university"), style = "default", class = "btn-custom"),
            bsButton("btn_mapa", "Territorial", icon = icon("location-arrow"), style = "default", class = "btn-custom")
        )
      )
    ),
    
    uiOutput("dynamic_body"),
    
    uiOutput("modal_ui"),
    
    tags$footer(
      tags$hr(),
      tags$p("© 2025 Agencia de Calidad de la Educación. Todos los derechos reservados.",
             style = "text-align: center; font-size: 12px; color: gray; margin-left: 375px; margin-right: 230px;")
    )
  )
)

server <- function(input, output, session) {
  
  observe({
    showModal(modalDialog(
      title = div(style = "display: flex; align-items: center;",
                  tags$img(src = "marca_agencia.png", height = "50px", style = "margin-right: 10px;"),
                  div("¡Bienvenido al centro interactivo de datos Simce!", 
                      style = "flex-grow: 1; text-align: center; font-size: 20px; font-weight: bold;")
      ),
      "Lorem ipsum dolor sit amet, consectetur adipiscing elit.
      Cras eu lectus orci. Fusce eget eleifend tellus. Duis luctus eget risus id porttitor.
      Sed ipsum dolor, aliquet eget finibus in, congue at mi.
      Donec tincidunt nisi consectetur enim placerat, nec finibus lacus fringilla.
      Proin tincidunt diam nulla. Sed laoreet velit id ultrices volutpat.
      Nulla facilisi. Quisque convallis sagittis commodo. Aenean lacinia facilisis orci.
      Nunc porta tellus ac nisl tincidunt tempus."
      ,
      easyClose = TRUE,
      footer = div(style = "text-align: center;", modalButton("Cerrar"))
    ))
  })
  
  data_rv <- reactiveValues()
  observe({
    isolate({
      data_rv$panel_data   <- readRDS("panel_data.rds")
      data_rv$panel_brecha <- readRDS("panel_brecha.rds")
      data_rv$panel_comuna <- readRDS("panel_comuna.rds")
    })
  })
  
  selected_panel  <- reactiveVal("nacional")
  selected_region <- reactiveVal(NULL)
  selected_comuna <- reactiveVal(NULL)
  
  observeEvent(input$btn_nacional, { selected_panel("nacional") })
  observeEvent(input$btn_genero, { selected_panel("genero") })
  observeEvent(input$btn_dependencia, { selected_panel("dependencia") })
  observeEvent(input$btn_mapa, { selected_panel("mapa") })
  
  observe({
    btn_id <- paste0("btn_", selected_panel())
    
    runjs(sprintf('
      $(".btn-custom").removeClass("btn-selected");
      $("#%s").addClass("btn-selected");
    ', btn_id))
    
  })
  
  
  observeEvent(input$plot41_shape_click, {
    updateSelectInput(session, "comuna",
                      selected = input$plot41_shape_click$id)
  })
  
  
  observe({
    req(data_rv$panel_comuna)
    
    if (is.null(selected_region())) {
      selected_region("METROPOLITANA DE SANTIAGO")
    }
    
    if (is.null(input$region) || !(input$region %in% data_rv$panel_comuna$Region)) {
      updateSelectInput(session, "region",
                        choices = unique(data_rv$panel_comuna$Region),
                        selected = selected_region())
    }
    
    comunas_region <- unique(data_rv$panel_comuna$NOM_COM_RBD[data_rv$panel_comuna$Region == selected_region()])
    
    if (length(comunas_region) == 0) {
      selected_comuna(NULL)
      return()
    }
    
    if (is.null(selected_comuna()) || !(selected_comuna() %in% comunas_region)) {
      nueva_comuna <- if ("PROVIDENCIA" %in% comunas_region) "PROVIDENCIA" else comunas_region[1]
      selected_comuna(nueva_comuna)
    }
    
    if (is.null(input$comuna) || !(input$comuna %in% comunas_region)) {
      updateSelectInput(session, "comuna",
                        choices = comunas_region,
                        selected = selected_comuna())
    }
  })
  
  observeEvent(input$region, {
    selected_region(input$region) 
    
    nuevas_comunas <- unique(data_rv$panel_comuna$NOM_COM_RBD[data_rv$panel_comuna$Region == input$region])
    
    nueva_comuna <- if (selected_comuna() %in% nuevas_comunas) selected_comuna() else nuevas_comunas[1]
    selected_comuna(nueva_comuna)
    
    updateSelectInput(session, "comuna",
                      choices = nuevas_comunas,
                      selected = nueva_comuna)
  })
  
  observeEvent(input$comuna, {
    selected_comuna(input$comuna)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%g %s<br/>Total: %d estudiantes<br/>Año: %s",
      filtered_comuna_mapa()$NOM_COM_RBD, 
      round(filtered_comuna_mapa()$num, 1),
      ifelse(input$var_mapa == "Puntaje", "Puntos", "%"),
      filtered_comuna_mapa()$n,
      input$anio_sel
    ) %>% lapply(htmltools::HTML)
    
    
    leafletProxy("plot41") %>%
      clearShapes() %>%
      addPolygons(
        data = filtered_comuna_mapa(),
        fillColor = ~colorNumeric("Blues", domain = filtered_comuna_mapa()$num)(num),
        color = ~ifelse(NOM_COM_RBD == selected_comuna(), "red", "black"),
        weight = ~ifelse(NOM_COM_RBD == selected_comuna(), 4, 1),
        fillOpacity = 0.8,
        layerId = ~NOM_COM_RBD,
        label = labels,
        highlightOptions = highlightOptions(
          weight = 3, color = "#666", fillOpacity = 1,
          bringToFront = TRUE
        )
      )
  })
  
  # Info Plots
  observeEvent(input$info_plot11, {
    showModal(modalDialog(
      title = "Promedio Nacional",
      "Lorem ipsum dolor sit amet, consectetur adipiscing elit.
      Cras eu lectus orci. Fusce eget eleifend tellus. Duis luctus eget risus id porttitor.
      Sed ipsum dolor, aliquet eget finibus in, congue at mi.
      Donec tincidunt nisi consectetur enim placerat, nec finibus lacus fringilla.
      Proin tincidunt diam nulla. Sed laoreet velit id ultrices volutpat.
      Nulla facilisi. Quisque convallis sagittis commodo. Aenean lacinia facilisis orci.
      Nunc porta tellus ac nisl tincidunt tempus.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$info_plot12, {
    showModal(modalDialog(
      title = "Promedio por GSE",
      "Lorem ipsum dolor sit amet, consectetur adipiscing elit.
    Cras eu lectus orci. Fusce eget eleifend tellus. Duis luctus eget risus id porttitor.
    Sed ipsum dolor, aliquet eget finibus in, congue at mi.
    Donec tincidunt nisi consectetur enim placerat, nec finibus lacus fringilla.
    Proin tincidunt diam nulla. Sed laoreet velit id ultrices volutpat.
    Nulla facilisi. Quisque convallis sagittis commodo. Aenean lacinia facilisis orci.
    Nunc porta tellus ac nisl tincidunt tempus.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$info_plot13, {
    showModal(modalDialog(
      title = "Distribución Estándares de Aprendizaje Nacional",
      "Lorem ipsum dolor sit amet, consectetur adipiscing elit.
    Cras eu lectus orci. Fusce eget eleifend tellus. Duis luctus eget risus id porttitor.
    Sed ipsum dolor, aliquet eget finibus in, congue at mi.
    Donec tincidunt nisi consectetur enim placerat, nec finibus lacus fringilla.
    Proin tincidunt diam nulla. Sed laoreet velit id ultrices volutpat.
    Nulla facilisi. Quisque convallis sagittis commodo. Aenean lacinia facilisis orci.
    Nunc porta tellus ac nisl tincidunt tempus.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$info_plot14, {
    showModal(modalDialog(
      title = "Brecha GSE Alto - Bajo",
      "Lorem ipsum dolor sit amet, consectetur adipiscing elit.
    Cras eu lectus orci. Fusce eget eleifend tellus. Duis luctus eget risus id porttitor.
    Sed ipsum dolor, aliquet eget finibus in, congue at mi.
    Donec tincidunt nisi consectetur enim placerat, nec finibus lacus fringilla.
    Proin tincidunt diam nulla. Sed laoreet velit id ultrices volutpat.
    Nulla facilisi. Quisque convallis sagittis commodo. Aenean lacinia facilisis orci.
    Nunc porta tellus ac nisl tincidunt tempus.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$info_plot21, {
    showModal(modalDialog(
      title = "Promedio por Género",
      "Lorem ipsum dolor sit amet, consectetur adipiscing elit.
    Cras eu lectus orci. Fusce eget eleifend tellus. Duis luctus eget risus id porttitor.
    Sed ipsum dolor, aliquet eget finibus in, congue at mi.
    Donec tincidunt nisi consectetur enim placerat, nec finibus lacus fringilla.
    Proin tincidunt diam nulla. Sed laoreet velit id ultrices volutpat.
    Nulla facilisi. Quisque convallis sagittis commodo. Aenean lacinia facilisis orci.
    Nunc porta tellus ac nisl tincidunt tempus.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$info_plot22, {
    showModal(modalDialog(
      title = "Brecha género por GSE",
      "Lorem ipsum dolor sit amet, consectetur adipiscing elit.
    Cras eu lectus orci. Fusce eget eleifend tellus. Duis luctus eget risus id porttitor.
    Sed ipsum dolor, aliquet eget finibus in, congue at mi.
    Donec tincidunt nisi consectetur enim placerat, nec finibus lacus fringilla.
    Proin tincidunt diam nulla. Sed laoreet velit id ultrices volutpat.
    Nulla facilisi. Quisque convallis sagittis commodo. Aenean lacinia facilisis orci.
    Nunc porta tellus ac nisl tincidunt tempus.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$info_plot23, {
    showModal(modalDialog(
      title = "Distribución Estándares de Aprendizaje por Género",
      "Lorem ipsum dolor sit amet, consectetur adipiscing elit.
    Cras eu lectus orci. Fusce eget eleifend tellus. Duis luctus eget risus id porttitor.
    Sed ipsum dolor, aliquet eget finibus in, congue at mi.
    Donec tincidunt nisi consectetur enim placerat, nec finibus lacus fringilla.
    Proin tincidunt diam nulla. Sed laoreet velit id ultrices volutpat.
    Nulla facilisi. Quisque convallis sagittis commodo. Aenean lacinia facilisis orci.
    Nunc porta tellus ac nisl tincidunt tempus.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$info_plot24, {
    showModal(modalDialog(
      title = "Distribución de brechas a nivel Escuela",
      "Lorem ipsum dolor sit amet, consectetur adipiscing elit.
    Cras eu lectus orci. Fusce eget eleifend tellus. Duis luctus eget risus id porttitor.
    Sed ipsum dolor, aliquet eget finibus in, congue at mi.
    Donec tincidunt nisi consectetur enim placerat, nec finibus lacus fringilla.
    Proin tincidunt diam nulla. Sed laoreet velit id ultrices volutpat.
    Nulla facilisi. Quisque convallis sagittis commodo. Aenean lacinia facilisis orci.
    Nunc porta tellus ac nisl tincidunt tempus.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$info_plot31, {
    showModal(modalDialog(
      title = "Distribución por Dependencia",
      "Lorem ipsum dolor sit amet, consectetur adipiscing elit.
    Cras eu lectus orci. Fusce eget eleifend tellus. Duis luctus eget risus id porttitor.
    Sed ipsum dolor, aliquet eget finibus in, congue at mi.
    Donec tincidunt nisi consectetur enim placerat, nec finibus lacus fringilla.
    Proin tincidunt diam nulla. Sed laoreet velit id ultrices volutpat.
    Nulla facilisi. Quisque convallis sagittis commodo. Aenean lacinia facilisis orci.
    Nunc porta tellus ac nisl tincidunt tempus.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$info_plot32, {
    showModal(modalDialog(
      title = "Promedio por SLEP",
      "Lorem ipsum dolor sit amet, consectetur adipiscing elit.
    Cras eu lectus orci. Fusce eget eleifend tellus. Duis luctus eget risus id porttitor.
    Sed ipsum dolor, aliquet eget finibus in, congue at mi.
    Donec tincidunt nisi consectetur enim placerat, nec finibus lacus fringilla.
    Proin tincidunt diam nulla. Sed laoreet velit id ultrices volutpat.
    Nulla facilisi. Quisque convallis sagittis commodo. Aenean lacinia facilisis orci.
    Nunc porta tellus ac nisl tincidunt tempus.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$info_plot33, {
    showModal(modalDialog(
      title = "Brecha Dependencia - Nacional por GSE agrupado",
      "Lorem ipsum dolor sit amet, consectetur adipiscing elit.
    Cras eu lectus orci. Fusce eget eleifend tellus. Duis luctus eget risus id porttitor.
    Sed ipsum dolor, aliquet eget finibus in, congue at mi.
    Donec tincidunt nisi consectetur enim placerat, nec finibus lacus fringilla.
    Proin tincidunt diam nulla. Sed laoreet velit id ultrices volutpat.
    Nulla facilisi. Quisque convallis sagittis commodo. Aenean lacinia facilisis orci.
    Nunc porta tellus ac nisl tincidunt tempus.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$info_plot34, {
    showModal(modalDialog(
      title = "Brecha SLEP - Municipal",
      "Lorem ipsum dolor sit amet, consectetur adipiscing elit.
    Cras eu lectus orci. Fusce eget eleifend tellus. Duis luctus eget risus id porttitor.
    Sed ipsum dolor, aliquet eget finibus in, congue at mi.
    Donec tincidunt nisi consectetur enim placerat, nec finibus lacus fringilla.
    Proin tincidunt diam nulla. Sed laoreet velit id ultrices volutpat.
    Nulla facilisi. Quisque convallis sagittis commodo. Aenean lacinia facilisis orci.
    Nunc porta tellus ac nisl tincidunt tempus.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$info_plot41, {
    showModal(modalDialog(
      title = "Resultados por Comuna",
      "Lorem ipsum dolor sit amet, consectetur adipiscing elit.
    Cras eu lectus orci. Fusce eget eleifend tellus. Duis luctus eget risus id porttitor.
    Sed ipsum dolor, aliquet eget finibus in, congue at mi.
    Donec tincidunt nisi consectetur enim placerat, nec finibus lacus fringilla.
    Proin tincidunt diam nulla. Sed laoreet velit id ultrices volutpat.
    Nulla facilisi. Quisque convallis sagittis commodo. Aenean lacinia facilisis orci.
    Nunc porta tellus ac nisl tincidunt tempus.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  
  

  # Renderizar UI dinámico basado en el botón seleccionado
  output$dynamic_body <- renderUI({
    switch(selected_panel(),
           "nacional" = fluidRow(
             column(
               width = 5,
               box(
                 title = tagList("Promedio Nacional", 
                                 actionLink("info_plot11", icon("circle-question"), 
                                            style = "color: #ffffff; font-size: 18px; text-decoration: none; 
                                          position: absolute; right: 10px; top: 10px; z-index: 1050;")),
                 status = "primary",
                 solidHeader = TRUE,
                 height = "330px",
                 plotlyOutput("plot11", height = "275px"),
                 width = NULL
                 )
               ),
             column(
               width = 5,
               box(
                 title = tagList("Promedio por GSE", 
                                 actionLink("info_plot12", icon("circle-question"), 
                                            style = "color: #ffffff; font-size: 18px; text-decoration: none; 
                                          position: absolute; right: 10px; top: 10px; z-index: 1050;")),
                 status = "primary",
                 solidHeader = TRUE,
                 height = "330px",
                 uiOutput("gse_filter"),
                 plotlyOutput("plot12", height = "200px"),
                 width = NULL
                 )
               ),
             column(
               width = 5,
               box(
                 title = tagList("Distribución Estándares de Aprendizaje Nacional", 
                                 actionLink("info_plot13", icon("circle-question"), 
                                            style = "color: #ffffff; font-size: 18px; text-decoration: none; 
                                          position: absolute; right: 10px; top: 10px; z-index: 1050;")),
                 status = "primary",
                 solidHeader = TRUE,
                 height = "330px",
                 plotlyOutput("plot13", height = "275px"),
                 width = NULL
                 )
               ),
             column(
               width = 5,
               box(
                 title = tagList("Brecha GSE Alto - Bajo", 
                                 actionLink("info_plot14", icon("circle-question"), 
                                            style = "color: #ffffff; font-size: 18px; text-decoration: none; 
                                          position: absolute; right: 10px; top: 10px; z-index: 1050;")),
                 status = "primary",
                 solidHeader = TRUE,
                 height = "330px",
                 plotlyOutput("plot14", height = "275px"),
                 width = NULL
                 )
               )
             ),
           "genero" = fluidRow(
             column(
               width = 5,
               box(
                 title = tagList("Promedio por Género", 
                                 actionLink("info_plot21", icon("circle-question"), 
                                            style = "color: #ffffff; font-size: 18px; text-decoration: none; 
                                          position: absolute; right: 10px; top: 10px; z-index: 1050;")),
                 status = "primary",
                 solidHeader = TRUE,
                 height = "330px",
                 plotlyOutput("plot21", height = "275px"),
                 width = NULL
               )
             ),
             column(
               width = 5,
               box(
                 title = tagList("Brecha género por GSE", 
                                 actionLink("info_plot22", icon("circle-question"), 
                                            style = "color: #ffffff; font-size: 18px; text-decoration: none; 
                                          position: absolute; right: 10px; top: 10px; z-index: 1050;")),
                 status = "primary",
                 solidHeader = TRUE,
                 height = "330px",
                 uiOutput("gse_filter"),
                 plotlyOutput("plot22", height = "200px"),
                 width = NULL
               )
             ),
             column(
               width = 5,
               box(
                 title = tagList("Distribución Estándares de Aprendizaje Nacional", 
                                 actionLink("info_plot23", icon("circle-question"), 
                                            style = "color: #ffffff; font-size: 18px; text-decoration: none; 
                                          position: absolute; right: 10px; top: 10px; z-index: 1050;")),
                 status = "primary",
                 solidHeader = TRUE,
                 height = "330px",
                 plotlyOutput("plot23", height = "275px"),
                 width = NULL
               )
             ),
             column(
               width = 5,
               box(
                 title = tagList("Distribución de brechas a nivel Escuela", 
                                 actionLink("info_plot24", icon("circle-question"), 
                                            style = "color: #ffffff; font-size: 18px; text-decoration: none; 
                                          position: absolute; right: 10px; top: 10px; z-index: 1050;")),
                 status = "primary",
                 solidHeader = TRUE,
                 height = "330px",
                 plotlyOutput("plot24", height = "275px"),
                 width = NULL
               )
             )
           ),
            "dependencia" = fluidRow(
              column(
                width = 5,
                box(
                  title = tagList("Promedio por Dependencia", 
                                  actionLink("info_plot31", icon("circle-question"), 
                                             style = "color: #ffffff; font-size: 18px; text-decoration: none; 
                                          position: absolute; right: 10px; top: 10px; z-index: 1050;")),
                  status = "primary",
                  solidHeader = TRUE,
                  height = "330px",
                  plotlyOutput("plot31", height = "275px"),
                  width = NULL
                )
              ),
              column(
                width = 5,
                box(
                  title = tagList("Promedio por SLEP", 
                                  actionLink("info_plot32", icon("circle-question"), 
                                             style = "color: #ffffff; font-size: 18px; text-decoration: none; 
                                          position: absolute; right: 10px; top: 10px; z-index: 1050;")),
                  status = "primary",
                  solidHeader = TRUE,
                  height = "330px",
                  plotlyOutput("plot32", height = "275px"),
                  width = NULL
                )
              ),
              column(
                width = 5,
                box(
                  title = tagList("Brecha Dependencia - Nacional por GSE agrupado", 
                                  actionLink("info_plot33", icon("circle-question"), 
                                             style = "color: #ffffff; font-size: 18px; text-decoration: none; 
                                          position: absolute; right: 10px; top: 10px; z-index: 1050;")),
                  status = "primary",
                  solidHeader = TRUE,
                  height = "330px",
                  uiOutput("gse2_filter"),
                  plotlyOutput("plot33", height = "200px"),
                  width = NULL
                )
              ),
              column(
                width = 5,
                box(
                  title = tagList("Brecha SLEP - Municipal", 
                                  actionLink("info_plot34", icon("circle-question"), 
                                             style = "color: #ffffff; font-size: 18px; text-decoration: none; 
                                          position: absolute; right: 10px; top: 10px; z-index: 1050;")),
                  status = "primary",
                  solidHeader = TRUE,
                  height = "330px",
                  plotlyOutput("plot34", height = "275px"),
                  width = NULL
                )
              )
           ),
           "mapa" = fluidRow(
             column(
               width = 10,
               box(
                 title = tagList("Resultados por Comuna", 
                                 actionLink("info_plot41", icon("circle-question"), 
                                            style = "color: #ffffff; font-size: 18px; text-decoration: none; 
                                          position: absolute; right: 10px; top: 10px; z-index: 1050;")),
                 status = "primary",
                 solidHeader = TRUE,
                 height = "675px",
                 width = "100%",
                 style = "overflow: hidden; position: relative;",
                 
                 leafletOutput("plot41", height = "605px", width = "98.5%"),
                 
                 absolutePanel(
                   id = "year_panel",
                   class = "custom-absolute-panel",
                   fixed = TRUE,
                   draggable = TRUE,
                   top = 90, left = 10, bottom = "auto", right = "auto",
                   width = 150, height = "auto",
                   
                   style = "background: rgba(255, 255, 255, 0.85); padding: 12px; border-radius: 8px; 
                 box-shadow: 2px 2px 5px rgba(0,0,0,0.2); z-index: 1050; font-size: 12px;",
                   
                   selectInput("anio_sel", "Año a mapear:", 
                               choices = sort(unique(data_rv$panel_comuna$agno), decreasing = TRUE),
                               selected = max(data_rv$panel_comuna$agno))
                 ),
                 
                 absolutePanel(
                   id = "control_panel",
                   class = "custom-absolute-panel",
                   fixed = TRUE,
                   draggable = TRUE,
                   top = 10, right = 10, bottom = "auto", left = "auto",
                   width = 260, height = "auto",
                   
                   style = "background: rgba(255, 255, 255, 0.85); padding: 12px; border-radius: 8px; 
                 box-shadow: 2px 2px 5px rgba(0,0,0,0.2); z-index: 1050; font-size: 12px;",
                   
                   div(class = "filter-container",
                       selectInput("region", "Seleccione Región:", 
                                   choices = unique(data_rv$panel_comuna$Region),
                                   selected = "METROPOLITANA DE SANTIAGO"),
                       selectInput("comuna", "Seleccione Comuna:", 
                                   choices = unique(data_rv$panel_comuna$NOM_COM_RBD[data_rv$panel_comuna$Region == "METROPOLITANA DE SANTIAGO"]),
                                   selected = "PROVIDENCIA"),
                       selectInput("var_mapa", "Seleccione Variable a Mapear:", 
                                   choices = c("Puntaje" = "Puntaje", 
                                               "Porcentaje Estudiantes Insuficiente" = "Insuficiente", 
                                               "Porcentaje Estudiantes Elemental" = "Elemental", 
                                               "Porcentaje Estudiantes Adecuado" = "Adecuado"),
                                   selected = "Puntaje")
                   ),
                   
                   div(class = "plot-container",
                       plotOutput("plot42", height = "160px", width = "100%"),
                       plotOutput("plot43", height = "160px", width = "100%")
                   )
                 )
               )
             )
           )
           
           
           
           
    )
  })
  
 
  
  # Filtros para gráficos
  
  output$gse_filter <- renderUI({
    div(
      style = "position: relative; z-index: 1050;",
      pickerInput(
        inputId = "gse",
        label = "Seleccione GSE(s):",
        choices = c("Nacional","Bajo","Medio bajo","Medio","Medio alto","Alto"),
        multiple = TRUE,
        selected = c("Nacional","Alto","Bajo"),
        options = pickerOptions(
          actionsBox = TRUE,
          liveSearch = TRUE,
          noneSelectedText = "Seleccione GSE(s)"
        )
      )
    )
  })
  
  output$gse2_filter <- renderUI({
    div(
      style = "position: relative; z-index: 1050;",
      pickerInput(
        inputId = "gse2",
        label = "Seleccione GSE(s):",
        choices = c("Bajo - Medio bajo","Medio","Medio alto - Alto"),
        multiple = FALSE,
        selected = "Medio",
        options = pickerOptions(
          actionsBox = FALSE,
          liveSearch = TRUE,
          noneSelectedText = "Seleccione un GSE"
        )
      )
    )
  })
  
  # Tratamiento Nacional
  
  filtered_prom_nac1 <- reactive({
    data_rv$panel_data %>%
      filter(
        panel == "Nacional",
        txt %in% c("Nacional"),
        asig == input$asig,
        grado == input$grado,
        agno >= input$agno[1] & agno <= input$agno[2]
      ) %>% 
      mutate(pos_x = as.numeric(factor(agno)))
  })
  
  filtered_prom_nac2 <- reactive({
    data_rv$panel_data %>%
      filter(
        panel == "Nacional",
        asig == input$asig,
        grado == input$grado,
        agno >= input$agno[1] & agno <= input$agno[2],
        txt %in% input$gse
      )
  })
  
  filtered_eda_nac <- reactive({
    data_rv$panel_data %>%
      filter(
        panel == "Nacional",
        txt %in% c("Insuficiente","Elemental","Adecuado"),
        asig == input$asig,
        grado == input$grado,
        agno >= input$agno[1] & agno <= input$agno[2]
      )
  })
  
  filtered_brecha_nac <- reactive({
    data_rv$panel_brecha %>%
      filter(
        panel == "Nacional",
        asig == input$asig,
        grado == input$grado,
        agno >= input$agno[1] & agno <= input$agno[2],
        txt %in% input$gse
      )
  })
  
  # Tratamiento - Genero
  
  filtered_prom_nac_gen <- reactive({
    data_rv$panel_data %>%
      filter(
        panel == "Genero",
        txt %in% c("Hombres", "Mujeres"),
        num > 100,
        asig == input$asig,
        grado == input$grado,
        agno >= input$agno[1] & agno <= input$agno[2]
      )
  })
  
  filtered_brecha_gen <- reactive({
    data_rv$panel_brecha %>%
      filter(
        panel == "Genero",
        asig == input$asig,
        grado == input$grado,
        agno >= input$agno[1] & agno <= input$agno[2],
        txt %in% input$gse
      )
  })
  
  filtered_eda_gen <- reactive({
    data_rv$panel_data %>%
      filter(
        panel == "Genero",
        !is.na(txt2),
        asig == input$asig,
        grado == input$grado,
        agno >= input$agno[1] & agno <= input$agno[2]
      )
  })
  
  filtered_porc_brecha_nac <- reactive({
    data_rv$panel_brecha %>%
      filter(
        panel == "Genero",
        !is.na(num),
        asig == input$asig,
        grado == input$grado,
        agno >= input$agno[1] & agno <= input$agno[2]
      )
  })
  
  # Tratamiento - Dependencia
  
  filtered_depe <- reactive({
    data_rv$panel_data %>%
      filter(
        panel == "Dependencia",
        txt %in% c("Particular Pagado","Particular Subvencionado","Público"),
        asig == input$asig,
        grado == input$grado,
        agno >= input$agno[1] & agno <= input$agno[2]
      ) %>% 
      mutate(txt2 = txt,
             txt = str_replace_all(txt, " ", "<br>"))
  })
  
  filtered_slep <- reactive({
    data_rv$panel_data %>%
      filter(
        panel == "Dependencia",
        txt %in% c("Municipal","Slep inicio 2018","Slep inicio 2019","Slep inicio 2020"),
        asig == input$asig,
        grado == input$grado,
        agno >= input$agno[1] & agno <= input$agno[2]
      )
  })
  
  filtered_depe_brecha <- reactive({
    data_rv$panel_brecha %>%
      filter(
        panel == "Dependencia",
        txt %in% c("Particular Pagado","Particular Subvencionado","Público"),
        asig == input$asig,
        grado == input$grado,
        agno >= input$agno[1] & agno <= input$agno[2],
        txt2 %in% input$gse2
      )%>% 
      mutate(txt2 = txt,
             txt = str_replace_all(txt, " ", "<br>"))
  })
  
  filtered_slep_brecha <- reactive({
    data_rv$panel_brecha %>%
      filter(
        panel == "Dependencia",
        txt %in% c("Slep inicio 2018","Slep inicio 2019","Slep inicio 2020"),
        asig == input$asig,
        grado == input$grado,
        agno >= input$agno[1] & agno <= input$agno[2]
      )
  })
  
  # Tratamiento - Mapas
  
  filtered_comuna_mapa <- reactive({
    simce_geo <- data_rv$panel_comuna %>%
      filter(
        asig == input$asig,
        grado == input$grado,
        agno == input$anio_sel,
        Region == input$region,
        txt == input$var_mapa
      ) 
    
    simce_geo <- st_sf(simce_geo)
    simce_geo <- st_transform(simce_geo, crs = 4326)
  })
  
  filtered_comuna <- reactive({
    data_rv$panel_comuna %>%
      filter(
        asig == input$asig,
        grado == input$grado,
        agno >= input$agno[1] & agno <= input$agno[2],
        Region == input$region,
        NOM_COM_RBD == input$comuna,
        txt %in% c("Puntaje")
      ) 
  })
  
  filtered_comuna2 <- reactive({
    data_rv$panel_comuna %>%
      filter(
        asig == input$asig,
        grado == input$grado,
        agno >= input$agno[1] & agno <= input$agno[2],
        Region == input$region,
        NOM_COM_RBD == input$comuna,
        txt != c("Puntaje")
      ) 
  })
  
  # Definir colores
  colores_eda <- c("Adecuado" = "#69ccd6", "Elemental" = "#ff5235", "Insuficiente" = "#024a61")
  colores_gse <- c("Alto" = "#613b75", "Medio alto" = "#69ccd6", "Medio"= "#bfbfbf", "Medio bajo"= "#ff5235", "Bajo" = "#024a61", "Nacional" = "black")
  colores_gen <- c("Hombres" = "#e74a2d", "Mujeres" = "#004a62")
  colores_dep <- c("Público" = "#AA9000", "Particular<br>Subvencionado" = "#00AC71", "Particular<br>Pagado" = "gray62")
  colores_sle <- c("Municipal" = "#00455d", "Slep inicio 2018" = "#ff5235", "Slep inicio 2019" = "#69ccd6", "Slep inicio 2020" = "#bfbfbf")

  # Gráficos Nacional

  output$plot11 <- renderPlotly({
    req(filtered_prom_nac1())
    plot_ly(
      data = filtered_prom_nac1(),
    x = ~factor(agno),
    y = ~num,
    type = 'scatter',
    mode = 'lines+markers+text',
    text = ~round(num, 0),
    textposition = "top center",
    marker = list(color = "#004a62"),
    line = list(color = "#004a62"),
    textfont = list(color = "#004a62")
  ) %>%
    layout(
      title = "",
      xaxis = list(title = ""),
      yaxis = list(title = "", range = c(200, 320)),
      legend = list(title = list(text = ""))
    )
  
  })

  output$plot12 <- renderPlotly({
    req(filtered_prom_nac2())
    plot_ly(
      data = filtered_prom_nac2(),
      x = ~factor(agno),
      y = ~num,
      color = ~factor(txt),
      colors = colores_gse,
      type = 'scatter',
      mode = 'lines+markers'
    ) %>%
      layout(title = "", xaxis = list(title = ""), yaxis = list(title = "", range = c(200, 320)))
  })
  
  output$plot13 <- renderPlotly({
    req(filtered_eda_nac())
    filtered_eda_nac() %>%
      ggplot(., aes(x = factor(agno), y = num, fill = txt)) +
      geom_bar(stat = "identity", position = "stack") +
      geom_text(aes(label = paste0(round(num, 1), "%")), 
                position = position_stack(vjust = 0.5), 
                size = 3, color = "white", fontface = "bold") +
      scale_fill_manual(values = colores_eda) +
      labs(
        title = "",
        x = "",
        y = "",
        fill = ""  
      ) +
      theme_minimal() +
      theme(
        panel.background = element_rect(fill = "white", color = NA),  
        plot.background = element_rect(fill = "white", color = NA),  
        axis.text.x = element_text(size = 9, color = "black"), 
        axis.text.y = element_blank(),  
        axis.ticks.y = element_blank(),  
        legend.position = "right",  
        legend.text = element_text(size = 10)
      )
  })
  
  output$plot14 <- renderPlotly({
    req(filtered_brecha_nac())
    plot_ly(
      data = filtered_brecha_nac(),
      x = ~factor(agno),
      y = ~brecha,
      type = "bar",
      text = ~round(brecha, 1),
      textposition = "outside",
      marker = list(color = "rgba(15, 105, 180, 0.8)"),
      hoverinfo = "text",
      hovertext = ~paste(
        " Año:", agno, "<br>",
        "Brecha: ", round(brecha, 1), " puntos", "<br>",
        "Favorece a GSE: ", categoria, "<br>",
        "Diferencia: ", ifelse(sig == 1, "Significativa", "No Significativa")
      )
    ) %>%
      layout(
        title = "",
        xaxis = list(title = ""),
        yaxis = list(title = "", range = c(0, max(filtered_brecha_nac()$brecha)+10)),
        bargap = 0.3
      )
  })
  
  # Gráficos Género
  output$plot21 <- renderPlotly({
    req(filtered_prom_nac_gen())
    plot_ly(
      data = filtered_prom_nac_gen(),
      x = ~factor(agno),
      y = ~num,
      color = ~txt,
      type = 'scatter',
      mode = 'lines+markers+text',
      text = ~round(num, 0),
      textposition = ~ifelse(txt == "Hombres" & asig == "mate", "top center", 
                             ifelse(txt == "Mujeres" & asig == "lect", "top center", "bottom center")),
      colors = colores_gen
    ) %>%
      layout(
        title = "",
        xaxis = list(title = ""),
        yaxis = list(title = "", range = c(200, 320)),
        legend = list(title = list(text = ""))
      )
  })
  
  output$plot22 <- renderPlotly({
    req(filtered_brecha_gen())
    plot_ly(
      data = filtered_brecha_gen(),
      x = ~agno,
      y = ~brecha,
      color = ~txt,
      colors = colores_gse,
      type = 'scatter',
      mode = 'lines+markers',
      textposition = ~ifelse(brecha >= 0, "top center", "bottom center"),
      hovertext = ~paste0(
        "Año: ", agno, "<br>",
        "Brecha: ", round(brecha, 1), " puntos", "<br>",
        "Grupo Socioeconómico: ", txt, "<br>",
        "Favorece a: ", categoria
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        title = "",
        xaxis = list(
          title = "",
          type = "category",
          categoryorder = "array",
          categoryarray = levels(factor(filtered_brecha_gen()$agno)) 
        ),
        yaxis = list(title = "", range = c(floor(min(filtered_brecha_gen()$brecha))-2, ceiling(max(filtered_brecha_gen()$brecha))+2)),
        legend = list(title = list(text = ""))
      )
  })
  
  output$plot23 <- renderPlotly({
    req(filtered_eda_gen())
    filtered_eda_gen() %>%
      ggplot(., aes(x = factor(agno), y = num, fill = txt2)) +
      geom_bar(stat = "identity", position = "stack") +
      geom_text(aes(label = paste0(round(num, 1), "%")), 
                position = position_stack(vjust = 0.5), 
                size = 3, color = "white", fontface = "bold") +
      scale_fill_manual(values = colores_eda) +
      labs(
        title = "",
        x = "",
        y = "",
        fill = "" 
      ) +
      facet_grid(rows = vars(txt), scales = "free_y", space = "fixed", switch = "y") + 
      theme_minimal() +
      theme(
        panel.background = element_rect(fill = "white", color = NA), 
        plot.background = element_rect(fill = "white", color = NA),  
        axis.text.x = element_text(size = 9, color = "black"), 
        axis.text.y = element_blank(),  
        axis.ticks.y = element_blank(),  
        legend.position = "right",  
        legend.text = element_text(size = 10), 
        strip.text.y.left = element_text(size = 14, face = "bold", color = "blue"), 
        strip.placement = "outside" 
      )
  })
  
  output$plot24 <- renderPlotly({
    req(filtered_porc_brecha_nac())
    
    filtered_porc_brecha_nac() %>% 
      filter(categoria %in% c("Favorece Hombres", "Favorece Mujeres")) %>% 
      mutate(num = ifelse(categoria == "Favorece Mujeres", -num, num)) %>%
      ggplot(aes(x = factor(agno), y = num, fill = categoria)) +
      geom_bar(stat = "identity") + 
      geom_text(aes(label = paste0(round(abs(num), 1), "%")),
                position = position_stack(vjust = 0.5), 
                size = 3, color = "white", fontface = "bold") +
      scale_fill_manual(values = c("Favorece Hombres" = "#007EA7", 
                                   "Favorece Mujeres" = "#FF6F61")) +
      scale_y_continuous(labels = abs) + 
      labs(
        title = "",
        x = "",
        y = "",
        fill = ""
      ) +
      theme_minimal() +
      theme(
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        axis.text.x = element_text(size = 9, color = "black"),
        axis.text.y = element_text(size = 9, color = "black"),
        axis.ticks.y = element_blank(),
        legend.position = "right", 
        legend.text = element_text(size = 8)
      )
  })
  
  # Gráficos Dependencia
  output$plot31 <- renderPlotly({
    req(filtered_depe())
    plot_ly(
      data = filtered_depe(),
      x = ~factor(agno),
      y = ~num,
      color = ~txt,
      type = 'scatter',
      mode = 'lines+markers+text',
      text = ~round(num, 0),
      textposition = "top center",
      colors = colores_dep
    ) %>%
      layout(
        title = "",
        xaxis = list(title = ""),
        yaxis = list(title = "", range = c(200, 340)),
        #yaxis = list(title = "", range = c(min(filtered_depe()$num)-10, max(filtered_depe()$num)+10)),
        legend = list(title = list(text = ""))
      )
  })
  
  output$plot32 <- renderPlotly({
    req(filtered_slep())
    plot_ly(
      data = filtered_slep(),
      x = ~factor(agno),
      y = ~num,
      color = ~txt,
      type = 'scatter',
      mode = 'lines+markers',
      colors = colores_sle
    ) %>%
      layout(
        title = "",
        xaxis = list(title = ""),
        yaxis = list(title = "", range = c(200, 320)),
        legend = list(title = list(text = ""))
      )
  })
  
  output$plot33 <- renderPlotly({
    req(filtered_depe_brecha())
    plot_ly(
      data = filtered_depe_brecha(),
      x = ~agno,
      y = ~brecha,
      color = ~txt,
      colors = colores_dep,
      type = 'scatter',
      mode = 'lines+markers',
      textposition = ~ifelse(brecha >= 0, "top center", "bottom center"),
      hovertext = ~paste0(
        "Año: ", agno, "<br>",
        "Brecha: ", round(brecha), " puntos", "<br>",
        "Diferencia: ", ifelse(brecha_abs >= 6, "Significativa", "No Significativa")
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        title = "",
        xaxis = list(
          title = "",
          type = "category",
          categoryorder = "array",
          categoryarray = levels(factor(filtered_depe_brecha()$agno)) 
        ),
        yaxis = list(title = "", range = c(floor(min(filtered_depe_brecha()$brecha))-2, ceiling(max(filtered_depe_brecha()$brecha))+2)),
        legend = list(title = list(text = ""))
      )
  })
  
  output$plot34 <- renderPlotly({
    req(filtered_slep_brecha())
    plot_ly(
      data = filtered_slep_brecha(),
      x = ~agno,
      y = ~brecha,
      color = ~txt,
      colors = colores_sle,
      type = 'scatter',
      mode = 'lines+markers',
      textposition = ~ifelse(brecha >= 0, "top center", "bottom center"),
      hovertext = ~paste0(
        "Año: ", agno, "<br>",
        "Brecha: ", round(brecha), " puntos", "<br>",
        "Diferencia: ", ifelse(brecha_abs >= 6, "Significativa", "No Significativa")
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        title = "",
        xaxis = list(
          title = "",
          type = "category",
          categoryorder = "array",
          categoryarray = levels(factor(filtered_slep_brecha()$agno)) 
        ),
        yaxis = list(title = "", range = c(floor(min(filtered_slep_brecha()$brecha))-2, ceiling(max(filtered_slep_brecha()$brecha))+2)),
        legend = list(title = list(text = ""))
      )
  })
  
  # Gráficos Mapas
  output$plot41 <- renderLeaflet({
    req(filtered_comuna_mapa())
    
    center <- st_centroid(st_union(filtered_comuna_mapa()))
    center_coords <- st_coordinates(center)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%g %s<br/>Total: %d estudiantes<br/>Año: %s",
      filtered_comuna_mapa()$NOM_COM_RBD, 
      round(filtered_comuna_mapa()$num, 1),
      ifelse(input$var_mapa == "Puntaje", "Puntos", "%"),
      filtered_comuna_mapa()$n,
      input$anio_sel
    ) %>% lapply(htmltools::HTML)
    
    paleta <- colorNumeric("Blues", domain = filtered_comuna_mapa()$num)
    
    mapa <- leaflet(filtered_comuna_mapa()) %>%
      addProviderTiles(provider = providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~paleta(num),
        color = ~ifelse(NOM_COM_RBD == selected_comuna(), "red", "black"),
        weight = ~ifelse(NOM_COM_RBD == selected_comuna(), 4, 1), 
        fillOpacity = 0.8,
        label = labels,
        layerId = ~NOM_COM_RBD,
        highlightOptions = highlightOptions(
          weight = 3, color = "#666", fillOpacity = 1,
          bringToFront = TRUE
        )
      ) %>%
      addLegend(
        pal = paleta, 
        values = ~num, 
        position = "bottomleft",
        title = ifelse(input$var_mapa == "Puntaje", "Puntaje", "Porcentaje de Estudiantes"),
        labFormat = labelFormat(suffix = ifelse(input$var_mapa == "Puntaje", " pts", "%"))
      )
    
    if (all(filtered_comuna_mapa()$Region == "VALPARAÍSO")) {
      mapa <- mapa %>% setView(lng = center_coords[1], lat = center_coords[2], zoom = 7.5)
    }
    
    mapa
  })
  
  
  output$plot42 <- renderPlot({
    req(filtered_comuna())
    
    df_long <- filtered_comuna() %>%
      select(agno, num, ptje_nac) %>%
      pivot_longer(cols = c(num, ptje_nac), 
                   names_to = "tipo", 
                   values_to = "puntaje")
    
    ggplot(data = df_long %>% arrange(agno), aes(x = factor(agno), y = puntaje, group = tipo, color = tipo)) +  
      geom_line(data = df_long %>% filter(tipo == "num"), linewidth = 1, color = "blue") +  
      geom_line(data = df_long %>% filter(tipo == "ptje_nac"), linewidth = 1, alpha = 0.8, 
                color = "orange", linetype = "dashed") +  
      geom_point(data = df_long %>% filter(tipo == "num"), aes(shape = tipo), size = 3, color = "blue") +  
      geom_point(data = df_long %>% filter(tipo == "ptje_nac"), aes(shape = tipo), size = 3, alpha = 0.5, color = "orange") +  
      geom_text(data = df_long %>% filter(tipo == "num"), 
                aes(x = factor(agno), y = puntaje, label = round(puntaje, 0)), 
                vjust = -1, size = 3, color = "black", fontface = "bold", inherit.aes = FALSE) +  
      
      scale_x_discrete() +
      scale_y_continuous(limits = c(min(df_long$puntaje) - 10, max(df_long$puntaje) + 10)) +
      labs(x = "", y = "") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(size = 10, angle = -90, vjust = 0.5, hjust = 1, face = "bold"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "top"
      ) +
      scale_color_manual(values = c("num" = "blue", "ptje_nac" = "orange"), labels = c("Puntaje Comuna", "Puntaje Nacional")) + 
      scale_shape_manual(values = c("num" = 16, "ptje_nac" = 16), labels = c("Puntaje Comuna", "Puntaje Nacional")) + 
      guides(shape = guide_legend(title = ""), color = guide_legend(title = ""))
  })
  
  output$plot43 <- renderPlot({
    req(filtered_comuna2())
    ggplot(data = filtered_comuna2() %>% arrange(agno), aes(x = factor(agno), y = num, fill = txt)) +  
      geom_bar(stat = "identity", position = "stack") +
      geom_text(aes(label = paste0(round(num), "%")), 
                position = position_stack(vjust = 0.5), 
                size = 3.5, color = "white", fontface = "bold") +
      scale_fill_manual(values = colores_eda) +
      labs(
        title = "",
        x = "",
        y = "",
        fill = ""  
      ) +
      theme_minimal() +
      theme(
        panel.background = element_rect(fill = "white", color = NA),  
        plot.background = element_rect(fill = "white", color = NA),  
        axis.text.x = element_text(size = 10, angle = -90, vjust = 0.5, hjust = 1, face = "bold"), 
        axis.text.y = element_blank(),  
        axis.ticks.y = element_blank(),  
        legend.position = "none",  
        legend.text = element_text(size = 8)
      )
  })
  
}

shinyApp(ui, server)