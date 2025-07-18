library(shiny)
library(shinydashboard)
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

ui <- dashboardPage(
  title = "Visor Simce",
  dashboardHeader(
    title = tags$div(
      class = "header-wrapper",
      style = "display: flex; align-items: center; padding-left: 40px;",
      
      tags$div(class = "logo-container",
               tags$img(src = "marca_agencia3.png", height = "70px")
      ),
      
      tags$div(
        id = "custom-toggle",
        class = "custom-toggle",
        tags$i(class = "fa fa-bars")
      ),
      
      tags$div(
        class = "header-title-right",
        "Visor Simce"
      )
    ),
    titleWidth = 1600
  )
  ,
  dashboardSidebar(
    useShinyjs(),
    
    conditionalPanel(
      condition = "input.panel_seleccionado != 'niveles'",
      fluidRow(
        column(12,
               div(style = "padding: 10px;",
                   sliderInput("agno", "Seleccione Rango de Años:",
                               min = 2001, max = 2024,
                               value = c(2015, 2024), step = 1, sep = "", post = " ", animate = FALSE),
                   selectInput("asig", "Seleccione Asignatura:",
                               choices = c("Matemática" = "mate", "Lectura" = "lect"),
                               selected = "lect"),
                   selectInput("grado", "Seleccione Grado:",
                               choices = c("4° básico" = "4b", "II medio" = "2m"),
                               selected = "4b")
               )
        )
      )
    ),
    conditionalPanel(
      condition = "input.panel_seleccionado == 'niveles'",
      fluidRow(
        column(12,
               div(style = "padding: 10px;",
                   selectInput("filtro_idps_agno", "Año:",
                               choices = c(2024)),
                   selectInput("filtro_idps_ind", "Indicador de Desarrollo Personal y Social:",
                               choices = c("Autoestima y Motivación" = "am", "Clima de Convivencia Escolar" = "cc", "Hábitos de Vida Saludable" = "hv", "Participación y Formación Ciudadana" = "pf")),
                   selectInput("filtro_idps_grupo", "Grupo:",
                               choices = c("Género", "GSE")),
                   selectInput("filtro_idps_nivel", "Nivel:",
                               choices = c("Alto", "Medio", "Bajo"))
               )
        )
      )
    ),
    
    div(
      hr(),
      div("Versión 1.0.7",
          id = "version-text",
          style = "color: white; font-size: 12px; font-style: italic; text-align: center; padding: 5px;"),
      style = "padding: 10px;"
    ),
    
    tags$style(HTML("
    .irs-grid-text {
      transform: rotate(90deg);
      transform-origin: center;
      font-size: 10px;
      white-space: nowrap;
    }
  ")),
    
    width = 250
  )
  ,
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),

      # Google Analytics 4
      tags$script(
        async = NA,
        src = "https://www.googletagmanager.com/gtag/js?id=G-W26XY3TCKS"
      ),
      tags$script(HTML("
      window.dataLayer = window.dataLayer || [];
      function gtag(){dataLayer.push(arguments);}
      gtag('js', new Date());
      gtag('config', 'G-W26XY3TCKS');
    "))
    ),
    # tags$head(
    #   tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    # ),
    
    useShinyjs(),
    tags$script(HTML("
  $(document).ready(function() {
    function toggleSidebar() {
      const body = $('body');

      if ($(window).width() <= 768) {
        body.toggleClass('sidebar-open');
      } else {
        body.toggleClass('sidebar-collapse');
      }
    }

    $('#custom-toggle').on('click', function () {
      toggleSidebar();
    });
  });
  let clickCount = 0;
  let timeout;

  $(document).on('click', '#version-text', function() {
    clickCount++;
    clearTimeout(timeout);
    timeout = setTimeout(function() { clickCount = 0; }, 1000);

    if (clickCount >= 10) { 
      window.open('https://www.agenciaeducacion.cl/', '_blank');
    }
  });
"))
    
    ,
    fluidRow(
      column(width = 12,
             div(class = "box-botones",
                 tags$style(HTML("
      .box-botones .box {
        border: none !important;
        box-shadow: none !important;
        background-color: transparent !important;
      }
    ")),
        box(
            title = NULL,
            width = 12,
            solidHeader = FALSE,
            status = NULL,
            div(class = "btn-bar",
             div(class = "btn-wrapper", bsButton("btn_inicio", "Inicio", icon = icon("house-user"), class = "btn-custom")),
             div(class = "btn-wrapper", bsButton("btn_nacional", "Nacional", icon = icon("flag"), class = "btn-custom")),
             div(class = "btn-wrapper", bsButton("btn_genero", "Género", icon = icon("venus-mars"), class = "btn-custom")),
             div(class = "btn-wrapper", bsButton("btn_mapa", "Territorial", icon = icon("location-arrow"), class = "btn-custom")),
             div(class = "btn-wrapper", bsButton("btn_niveles", "IDPS", icon = icon("users"), class = "btn-custom"))
            )
          )
        )
      )
    ),
    
    uiOutput("dynamic_body"),
    uiOutput("modal_ui"),
    
    fluidRow(
      column(width = 12,
             tags$footer(
               tags$hr(),
               tags$p("© 2025 Agencia de Calidad de la Educación. Todos los derechos reservados.",
                      style = "text-align: center; font-size: 12px; color: gray;")
             )
      )
    )
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$boton_link, {
    shinyjs::runjs("window.open('https://agce-img-publicas.s3.us-east-1.amazonaws.com/visor-simce/Terminos_condiciones_de_uso.pdf', '_blank')")
  })
  
  observeEvent(input$anio_sel, {
    if (input$anio_sel < 2012) {
      updateSelectInput(session, "var_mapa",
                        choices = c("Puntaje" = "Puntaje"),
                        selected = "Puntaje")
    } else {
      updateSelectInput(session, "var_mapa",
                        choices = c("Puntaje" = "Puntaje", 
                                    "Porcentaje Estudiantes Insuficiente" = "Insuficiente", 
                                    "Porcentaje Estudiantes Elemental" = "Elemental", 
                                    "Porcentaje Estudiantes Adecuado" = "Adecuado"),
                        selected = isolate(input$var_mapa))
    }
  })
  

  
  data_rv <- reactiveValues()
  observe({
    isolate({
      data_rv$panel_data   <- readRDS("panel_data.rds")
      data_rv$panel_brecha <- readRDS("panel_brecha.rds")
      data_rv$panel_comuna <- readRDS("panel_comuna.rds")
      data_rv$panel_niveles <- readRDS("panel_niveles.rds")
      data_rv$comunas_geo <- readRDS("comunas_geo.rds")
    })
  })
  
  selected_panel  <- reactiveVal("inicio")
  selected_region <- reactiveVal(NULL)
  selected_comuna <- reactiveVal(NULL)
  
  # observeEvent(input$btn_inicio, { selected_panel("inicio") })
  # observeEvent(input$btn_nacional, { selected_panel("nacional") })
  # observeEvent(input$btn_genero, { selected_panel("genero") })
  # observeEvent(input$btn_niveles, { selected_panel("niveles") })
  # observeEvent(input$btn_dependencia, { selected_panel("dependencia") })
  # observeEvent(input$btn_mapa, { selected_panel("mapa") })
  
  observeEvent(input$btn_inicio, {
    selected_panel("inicio")
    runjs("Shiny.setInputValue('panel_seleccionado', 'inicio');")
  })
  
  observeEvent(input$btn_nacional, {
    selected_panel("nacional")
    runjs("Shiny.setInputValue('panel_seleccionado', 'nacional');")
  })
  
  observeEvent(input$btn_genero, {
    selected_panel("genero")
    runjs("Shiny.setInputValue('panel_seleccionado', 'genero');")
  })
  
  observeEvent(input$btn_niveles, {
    selected_panel("niveles")
    runjs("Shiny.setInputValue('panel_seleccionado', 'niveles');")
  })
  
  observeEvent(input$btn_dependencia, {
    selected_panel("dependencia")
    runjs("Shiny.setInputValue('panel_seleccionado', 'dependencia');")
  })
  
  observeEvent(input$btn_mapa, {
    selected_panel("mapa")
    runjs("Shiny.setInputValue('panel_seleccionado', 'mapa');")
  })
  
  observeEvent(selected_panel(), {
    btn_id <- paste0("btn_", selected_panel())
    
    runjs(sprintf('
    $(".btn-custom").removeClass("btn-selected");
    $("#%s").addClass("btn-selected");
  ', btn_id))
    
    if (selected_panel() == "inicio") {
      runjs("if (!$('body').hasClass('sidebar-collapse')) { $('#custom-toggle').click(); }")
    } else {
      runjs("if ($('body').hasClass('sidebar-collapse')) { $('#custom-toggle').click(); }")
    }
    
    runjs(sprintf("Shiny.setInputValue('panel_seleccionado', '%s');", selected_panel()))
  })
  
  observeEvent(input$plot41_shape_click, {
    updateSelectInput(session, "comuna",
                      selected = input$plot41_shape_click$id)
  })
  
  
  observe({
    req(data_rv$panel_comuna)
    
    if (is.null(selected_region())) {
      selected_region("Metropolitana de Santiago")
    }
    
    if (is.null(input$region) || !(input$region %in% data_rv$panel_comuna$nombre_region)) {
      updateSelectInput(session, "region",
                        choices = unique(data_rv$panel_comuna$nombre_region),
                        selected = selected_region())
    }
    
    comunas_region <- unique(data_rv$panel_comuna$nombre_comuna[data_rv$panel_comuna$nombre_region == selected_region()])
    
    if (length(comunas_region) == 0) {
      selected_comuna(NULL)
      return()
    }
    
    if (is.null(selected_comuna()) || !(selected_comuna() %in% comunas_region)) {
      nueva_comuna <- if ("Providencia" %in% comunas_region) "Providencia" else comunas_region[1]
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
    
    nuevas_comunas <- unique(data_rv$panel_comuna$nombre_comuna[data_rv$panel_comuna$nombre_region == input$region])
    
    nueva_comuna <- if (selected_comuna() %in% nuevas_comunas) selected_comuna() else nuevas_comunas[1]
    selected_comuna(nueva_comuna)
    
    updateSelectInput(session, "comuna",
                      choices = nuevas_comunas,
                      selected = nueva_comuna)
  })
  
  observeEvent(input$comuna, {
    selected_comuna(input$comuna)
    
    df <- filtered_comuna_mapa()
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%g %s<br/>Total: %d estudiantes<br/>Año: %s",
      df$nombre_comuna, 
      round(df$num, 1),
      ifelse(input$var_mapa == "Puntaje", "Puntos", "%"),
      df$n,
      input$anio_sel
    ) %>% lapply(htmltools::HTML)
    
    paleta <- colorNumeric("Blues", domain = df$num, na.color = "white")
    
    leafletProxy("plot41") %>%
      clearShapes() %>%
      addPolygons(
        data = df,
        fillColor = ~paleta(num),
        color = ~ifelse(nombre_comuna == selected_comuna(), "red", "black"),
        weight = ~ifelse(nombre_comuna == selected_comuna(), 4, 1),
        fillOpacity = 0.8,
        layerId = ~nombre_comuna,
        label = ~labels,
        highlightOptions = highlightOptions(
          weight = 3, color = "#666", fillOpacity = 1,
          bringToFront = TRUE
        )
      )
  })
  
  
  # Info Plots
  observeEvent(input$info_plotXX, {
    showModal(modalDialog(
      title = "Lorem Impsum",
      "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Curabitur laoreet vestibulum mauris ac egestas. Praesent bibendum commodo nibh vel auctor. Integer efficitur aliquam tempus. Nulla malesuada vehicula vestibulum. Etiam nibh elit, feugiat vitae neque a, gravida lobortis tortor. Ut pellentesque mattis justo. Etiam semper, neque varius auctor rhoncus, lorem leo lobortis felis, sed dapibus tellus sem ut urna. Quisque elit arcu, mollis ut consectetur eu, blandit accumsan nibh. Nunc egestas eros rutrum urna congue vehicula. Vivamus lacinia rutrum placerat. Donec enim mauris, maximus quis eleifend in, interdum eget odio. Sed vel quam sit amet leo efficitur lobortis id in orci.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$info_plot11, {
    showModal(modalDialog(
      title = "Promedio nacional",
      "Este gráfico muestra la evolución del puntaje promedio Simce a nivel nacional, para una combinación específica de asignatura (Lenguaje o Matemática) y nivel educativo (4° básico o II medio), en el período comprendido entre los años 2001 y 2024. El eje horizontal representa el año de aplicación de la prueba, mientras que el eje vertical indica el puntaje promedio obtenido por los estudiantes a nivel nacional. En términos generales, el gráfico permite identificar tendencias de mejora, estabilidad o retroceso en el desempeño promedio de los estudiantes. En este contexto, se considera que una variación es significativa cuando alcanza o supera los 5 puntos en la escala Simce.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$info_plot12, {
    showModal(modalDialog(
      title = "Promedio por GSE",
      "Este gráfico muestra la evolución del puntaje promedio Simce desagregado por grupo socioeconómico (GSE), para una combinación específica de asignatura (Lenguaje o Matemática) y nivel educativo (4° básico o II medio), en el período comprendido entre los años 2001 y 2024. El eje horizontal representa el año de aplicación de la prueba, mientras que el eje vertical indica el puntaje promedio obtenido por los estudiantes dentro de cada grupo. El gráfico permite comparar el rendimiento académico de estudiantes pertenecientes a distintos niveles socioeconómicos, observando tanto las tendencias internas de cada grupo como las brechas entre ellos. Se considera que una variación es significativa cuando alcanza o supera los 5 puntos en la escala Simce.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$info_plot13, {
    showModal(modalDialog(
      title = "Distribución Estándares de Aprendizaje Nacional",
      "Este gráfico muestra la distribución porcentual de los estudiantes en los distintos estándares de aprendizaje definidos por el Simce, para una combinación específica de asignatura (Lenguaje o Matemática) y nivel educativo (4° básico o II medio), en el período comprendido entre los años 2012 y 2024. El eje horizontal indica el año de aplicación de la prueba, mientras que el eje vertical representa el porcentaje de estudiantes ubicados en cada nivel de desempeño. Los estándares se agrupan en tres niveles:
      Adecuado: estudiantes cuyo desempeño se ubica en este Nivel de Aprendizaje han logrado lo exigido en el currículum de manera satisfactoria.
      Elemental: estudiantes cuyo desempeño se ubica en este Nivel de Aprendizaje han logrado lo exigido en el currículum de manera parcial.
      Insuficiente: estudiantes cuyo desempeño se encuentra en este nivel no logran demostrar consistentemente que han adquirido las habilidades y los conocimientos más elementales estipulados en el currículum para el periodo evaluado.
      Este gráfico permite identificar tendencias en la distribución de desempeño. En este contexto, se considera que una variación en la proporción de un estándar es significativa cuando alcanza o supera los 5 puntos porcentuales.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$info_plot14, {
    showModal(modalDialog(
      title = "Brecha GSE Alto - Bajo",
      "Este gráfico muestra la evolución de la brecha de puntaje Simce entre estudiantes del grupo socioeconómico (GSE) Alto y del GSE Bajo, para una combinación específica de asignatura (Lenguaje o Matemática) y nivel educativo (4° básico o II medio), en el período comprendido entre los años 2001 y 2024. El eje horizontal representa el año de aplicación de la prueba, y el eje vertical indica la magnitud de la diferencia de puntaje promedio entre ambos grupos.
      La brecha se calcula como la diferencia entre el puntaje promedio del GSE Alto y el del GSE Bajo. Un valor más alto en este gráfico refleja una mayor desigualdad de resultados entre los extremos socioeconómicos.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$info_plot21, {
    showModal(modalDialog(
      title = "Promedio por género",
      "Este gráfico muestra la evolución del puntaje promedio Simce desagregado por género, para una combinación específica de asignatura (Lenguaje o Matemática) y nivel educativo (4° básico o II medio), en el período comprendido entre los años 2002 y 2024. El eje horizontal representa el año de aplicación de la prueba, mientras que el eje vertical indica el puntaje promedio obtenido por los estudiantes, separados en dos categorías: hombres y mujeres.
      Esta visualización permite observar diferencias en el rendimiento entre ambos grupos, así como identificar posibles tendencias de convergencia o divergencia de los puntajes a lo largo del tiempo.
      Se considera que una variación entre años consecutivos es significativa cuando alcanza o supera los 5 puntos en la escala Simce.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$info_plot22, {
    showModal(modalDialog(
      title = "Brecha género por GSE",
      "Este gráfico muestra la evolución de la brecha de puntaje Simce entre mujeres y hombres, desagregada por grupo socioeconómico (GSE), para una combinación específica de asignatura (Lenguaje o Matemática) y nivel educativo (4° básico o II medio), en el período comprendido entre los años 2002 y 2024. El eje horizontal representa el año de aplicación de la prueba, mientras que el eje vertical indica la magnitud de la diferencia de puntaje promedio entre géneros.
      La brecha se calcula como la diferencia absoluta entre el puntaje promedio de mujeres y hombres, lo que permite observar desigualdades de rendimiento entre géneros al interior de cada grupo socioeconómico: Alto, Medio-Alto, Medio, Medio-Bajo, Bajo y el promedio Nacional.
      Este gráfico permite analizar cómo varía la desigualdad de género en distintos contextos socioeconómicos y si existen patrones persistentes o convergencias a lo largo del tiempo.
      Se considera que una variación en la brecha es significativa cuando alcanza o supera los 5 puntos en la escala Simce.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$info_plot23, {
    showModal(modalDialog(
      title = "Distribución Estándares de Aprendizaje por Género",
      "Este gráfico muestra la distribución porcentual de estudiantes en los tres estándares de aprendizaje definidos por el Simce (Insuficiente, Elemental y Adecuado) desagregados por género, para una combinación específica de asignatura (Lenguaje o Matemática) y nivel educativo (4° básico o II medio), en el período comprendido entre los años 2012 y 2024. El eje horizontal representa los años de aplicación, mientras que las columnas verticales indican la proporción de estudiantes por nivel de logro.
      Los estándares se agrupan en tres niveles:
      Adecuado: estudiantes cuyo desempeño se ubica en este Nivel de Aprendizaje han logrado lo exigido en el currículum de manera satisfactoria.
      Elemental: estudiantes cuyo desempeño se ubica en este Nivel de Aprendizaje han logrado lo exigido en el currículum de manera parcial.
      Insuficiente: estudiantes cuyo desempeño se encuentra en este nivel no logran demostrar consistentemente que han adquirido las habilidades y los conocimientos más elementales estipulados en el currículum para el periodo evaluado.
      Este gráfico permite identificar tendencias en la distribución de desempeño. En este contexto, se considera que una variación en la proporción de un estándar es significativa cuando alcanza o supera los 5 puntos porcentuales.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$info_plot24, {
    showModal(modalDialog(
      title = "Distribución de brechas a nivel establecimiento educacional",
      "Este gráfico muestra la proporción de escuelas en las que existe una brecha de puntaje Simce significativa entre hombres y mujeres, para una combinación específica de asignatura (Lenguaje o Matemática) y nivel educativo (4° básico o II medio), en el período comprendido entre los años 2002 y 2024. El eje horizontal representa el año de evaluación, mientras que el eje vertical indica el porcentaje de escuelas.
      Las barras están divididas en dos segmentos:
      Favorece Mujeres: porcentaje de escuelas donde las estudiantes mujeres obtienen un puntaje significativamente mayor que los hombres.
      Favorece Hombres: porcentaje de escuelas donde los estudiantes hombres obtienen un puntaje significativamente mayor que las mujeres.
      Esta visualización permite observar la distribución de la ventaja de rendimiento por género a nivel escolar, identificando si hay un sesgo sistemático en el rendimiento de uno u otro grupo, y cómo ha evolucionado con el tiempo. En este contexto, se considera que una diferencia es significativa cuando la brecha entre géneros supera los 5 puntos Simce en una misma escuela.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  # observeEvent(input$info_plot31, {
  #   showModal(modalDialog(
  #     title = "Distribución por Dependencia",
  #     "Este gráfico muestra la evolución del puntaje promedio Simce desagregado según el tipo de dependencia administrativa del establecimiento escolar (Público y SLEP, Particular Subvencionado y Particular Pagado) para una combinación específica de asignatura (Lenguaje o Matemática) y nivel educativo (4° básico o II medio), en el período comprendido entre los años 2002 y 2024. El eje horizontal representa el año de aplicación de la prueba, mientras que el eje vertical indica el puntaje promedio obtenido por los estudiantes en cada categoría de dependencia.
  #     Esta visualización permite comparar el desempeño académico según el tipo de establecimiento, lo cual es relevante para analizar diferencias estructurales dentro del sistema escolar chileno. En este contexto, una variación de 5 puntos o más entre años consecutivos se considera significativa.",
  #     easyClose = TRUE,
  #     footer = NULL
  #   ))
  # })
  # 
  # observeEvent(input$info_plot32, {
  #   showModal(modalDialog(
  #     title = "Promedio por SLEP",
  #     "Este gráfico muestra la evolución del puntaje promedio Simce en establecimientos escolares de administración pública, desagregados según el tipo de gestión: Municipal o Servicios Locales de Educación Pública (SLEP), para una combinación específica de asignatura (Lenguaje o Matemática) y nivel educativo (4° básico o II medio), entre los años 2002 y 2024. El eje horizontal representa el año de evaluación, y el eje vertical, el puntaje promedio.
  #     Los establecimientos bajo administración SLEP se agrupan según su año de traspaso al nuevo sistema (2018, 2019, 2020, etc.), lo que permite realizar comparaciones longitudinales y entre cohortes de implementación.
  #     Este gráfico permite evaluar el impacto y evolución del proceso de desmunicipalización impulsado por la reforma educativa chilena. Se considera que una variación de 5 puntos o más en el puntaje entre un año y otro corresponde a un cambio significativo.",
  #     easyClose = TRUE,
  #     footer = NULL
  #   ))
  # })
  # 
  # observeEvent(input$info_plot33, {
  #   showModal(modalDialog(
  #     title = "Brecha Dependencia - Nacional por GSE agrupado",
  #     "Este gráfico muestra la evolución de la brecha de puntaje Simce entre tipos de dependencia escolar dentro de un mismo grupo socioeconómico agrupago en 3 categorías (Alto y Medio Alto; Medio; Medio Bajo y Bajo), para una combinación específica de asignatura (Lenguaje o Matemática) y nivel educativo (4° básico o II medio), en el período comprendido entre los años 2002 y 2024. El eje horizontal representa el año de evaluación, mientras que el eje vertical indica la magnitud de la diferencia de puntaje respecto del promedio nacional.
  #     Cada línea representa el puntaje promedio de un tipo de dependencia escolar para el grupo socioeconómico seleccionado, comparado con el promedio nacional. Los valores positivos indican un rendimiento superior al promedio nacional, mientras que los negativos reflejan un rendimiento inferior.
  #     Este gráfico permite observar si las diferencias entre tipos de establecimientos persisten al interior de un mismo nivel socioeconómico, lo que aporta una mirada más ajustada y equitativa sobre el desempeño educativo.
  #     En este contexto, se considera que una variación en la brecha es significativa cuando alcanza o supera los 5 puntos en la escala Simce.",
  #     easyClose = TRUE,
  #     footer = NULL
  #   ))
  # })
  # 
  # observeEvent(input$info_plot34, {
  #   showModal(modalDialog(
  #     title = "Brecha SLEP - Municipal",
  #     "Este gráfico muestra la evolución de la brecha de puntaje Simce entre establecimientos que fueron traspasados a los Servicios Locales de Educación Pública (SLEP) y los que continuaron bajo administración municipal, para una combinación específica de asignatura (Lenguaje o Matemática) y nivel educativo (4° básico o II medio), durante el período comprendido entre los años 2002 y 2024. El eje horizontal indica el año de evaluación, y el eje vertical representa la diferencia de puntaje promedio entre ambos tipos de gestión.
  #     Cada línea representa a un grupo de SLEP según su año de inicio: 2018, 2019 o 2020. El valor de la brecha se calcula como la diferencia entre el puntaje promedio de los establecimientos SLEP y los municipales. Valores positivos indican un mayor rendimiento de los SLEP respecto de los municipios, mientras que valores cercanos a cero o negativos indican un rendimiento similar o inferior.
  #     Este gráfico permite evaluar si la transición hacia el nuevo modelo de administración pública ha estado asociada a cambios en los resultados de aprendizaje en comparación con los establecimientos que permanecieron bajo gestión municipal.
  #     Se considera que una variación de 5 puntos o más representa un cambio significativo en la brecha.",
  #     easyClose = TRUE,
  #     footer = NULL
  #   ))
  # })
  
  observeEvent(input$info_plot41, {
    showModal(modalDialog(
      title = "Resultados por comuna",
      "Este gráfico presenta la distribución geográfica de los resultados Simce a nivel comunal, para una combinación específica de asignatura (Lenguaje o Matemática), nivel educativo (4° básico o II medio) y año de evaluación. El mapa permite comparar el desempeño promedio de las comunas dentro de una misma región, facilitando la identificación de diferencias territoriales en los resultados de aprendizaje.
      El color de cada comuna refleja el puntaje promedio obtenido, utilizando una escala de color que va desde los tonos más claros (puntajes más bajos) hasta los más oscuros (puntajes más altos). El panel lateral permite seleccionar:
      Región: para acotar el análisis territorial.
      Comuna: para visualizar en detalle su evolución histórica.
      Variable a mapear: como puntaje promedio o porcentaje en estándares de aprendizaje.
      Además del mapa, se presentan dos gráficos complementarios:
      Serie histórica: compara la evolución del puntaje promedio comunal con el promedio nacional en el tiempo.
      Distribución de estándares: muestra la proporción de estudiantes en los niveles Insuficiente, Elemental y Adecuado para la comuna seleccionada.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$info_plot52, {
    showModal(modalDialog(
      title = "Distribución nacional por niveles",
      "Este gráfico muestra la distribución porcentual de los estudiantes en los distintos niveles de desarrollo de los subdimensiones de los indicadores de desarrollo personal y social, para una combinación específica de subdimensión y nivel educativo (4° básico o II medio), para el año 2024. 
      El eje vertical indica las subdimensiones asociados al indicador seleccionado, mientras que el eje horizontal representa el porcentaje de estudiantes ubicados en cada nivel de desarrollo. Algunas subdimensiones cuentan con tres niveles: 
      Alto, Medio y Bajo, y otras con dos: Alto y Bajo, dependiendo de las características del aspecto evaluado. Este gráfico permite identificar tendencias en la distribución.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$info_plot54, {
    showModal(modalDialog(
      title = "Distribución de niveles por grupo y nivel seleccionado: ",
      "Este gráfico muestra el porcentaje de estudiantes que se encuentra en el nivel de desarrollo seleccionado (Alto, Medio o Bajo), comparando distintas categorías dentro del grupo seleccionado (género o grupo socioeconómico), para una combinación específica de subdimensión y nivel educativo (4° básico o II medio) en el año 2024. 
      En particular, permite visualizar diferencias entre grupos y detectar posibles brechas en una subdimensión determinada.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$info_plot53, {
    showModal(modalDialog(
      title = "Distribución de niveles por grupo",
      "Este gráfico muestra la distribución porcentual de los estudiantes en los distintos niveles de desarrollo de los subdimensiones de los indicadores de desarrollo personal y social, para una combinación específica de subdimensión y nivel educativo (4° básico o II medio), para el año 2024, comparando las distintas categorías de los grupos seleccionados (género o grupo socioeconómico). 
      El eje vertical indica las subdimensiones asociadas al indicador seleccionado, mientras que el eje horizontal representa el porcentaje de estudiantes ubicados en cada nivel de desarrollo, en las distintas categorías del grupo seleccionado. 
      Algunas subdimensiones cuentan con tres niveles: Alto, Medio y Bajo, y otras con dos: Alto y Bajo, dependiendo de las características del aspecto evaluado. 
      Este gráfico permite identificar tendencias en la distribución.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  output$dynamic_body <- renderUI({
    switch(selected_panel(),
           "inicio" = fluidRow(
             column(
               width = 12,
               div(  
                 class = "box-inicio",
                 tagList(
                   tags$style(HTML("
                     .box-inicio .box {
                       border: none !important;
                       box-shadow: none !important;
                       background-color: transparent !important;
                     }
                   ")),
                   
                   box(
                     title = NULL,
                     width = "100%",
                     solidHeader = FALSE,
                     status = NULL,
                     tagList(
                       HTML('
      <h2>Bienvenidos al Visor de datos Simce</h2>
      <p>Este dashboard ha sido desarrollado por la Agencia de Calidad de la Educación para explorar de manera interactiva los resultados del Simce,
      y fomentar el análisis y la toma de decisiones basada en evidencia.
      La herramienta permite visualizar datos en distintos niveles de agregación, incluyendo análisis nacionales, 
      de género, por dependencia educativa y a nivel territorial.</p>

      <h2>Características principales</h2>
      <ul>
        <li><strong>Exploración de datos nacionales:</strong> sección donde se presentan los promedios de puntajes Simce a nivel nacional, junto con la distribución de los niveles de aprendizaje y brechas entre distintos grupos socioeconómicos.</li>
        <li><strong>Análisis por género:</strong> permite visualizar diferencias en desempeño según género y explorar brechas dentro de cada grupo socioeconómico.</li>
        <li><strong>Visualización territorial:</strong> presenta un mapa interactivo que permite explorar los resultados a nivel regional y comunal, facilitando la identificación de patrones geográficos.</li>
        <li><strong>Niveles de desarrollo de las subdimensiones de los Indicadores de Desarrollo Personal y Social:</strong> corresponden a una clasificación en progresión de los aspectos percibidos por los y las estudiantes en cada subdimensión.</li>
      </ul>

      <h2>Cómo usar el dashboard</h2>
      <ol>
        <li><strong>Selecciona los filtros</strong> en la barra lateral para elegir el rango de años, la asignatura de interés (Matemáticas o Lectura) y el grado (4° básico o II medio).</li>
        <li><strong>Explora las distintas secciones</strong> utilizando los botones de navegación en la parte superior:
          <ul>
            <li><strong>Inicio:</strong> Información general sobre la herramienta.</li>
            <li><strong>Nacional:</strong> Datos agregados a nivel país.</li>
            <li><strong>Género:</strong> Comparaciones entre hombres y mujeres.</li>
            <li><strong>Territorial:</strong> Exploración de datos a nivel de comuna.</li>
            <li><strong>IDPS:</strong> Análisis de la distribución de los distintos niveles de desarrollo de las dimensiones de los Indicadores de Desarrollo Personal y Social (IDPS).</li>
          </ul>
        </li>
        <li><strong>Consulta información adicional</strong> presionando los íconos de interrogación en cada sección para acceder a descripciones detalladas de los indicadores.</li>
      </ol>

      <h2>Contacto y soporte</h2>
      <p>Si tienes dudas sobre el uso de este dashboard o necesitas información adicional, puedes acceder a la 
      <a href="https://www.agenciaeducacion.cl/oirs/">OIRS institucional</a>.</p>
    '),
     div(
       style = "text-align: center; margin-top: 20px;",
       bsButton("boton_link", "Términos y condiciones de uso", icon = icon("external-link"), class = "btn-custom")
     )
                     )
                   )
                   
                 )
               )
             )
           ),
           "nacional" = fluidRow(
             column(
               width = 6,
               div(class = "box-tipo",
                   box(
                     title = tagList("Promedio nacional", 
                                     actionLink("info_plot11", icon("circle-question"), 
                                                style = "color: #ffffff; font-size: 18px; text-decoration: none; 
                                          position: absolute; right: 10px; top: 10px; z-index: 1050;")),
                     solidHeader = TRUE,
                     status = NULL,
                     height = "330px",
                     plotlyOutput("plot11", height = "275px"),
                     width = NULL
                   )
               )
             ),
             column(
               width = 6,
               div(class = "box-tipo",
                   box(
                     title = tagList("Promedio por GSE", 
                                     actionLink("info_plot12", icon("circle-question"), 
                                                style = "color: #ffffff; font-size: 18px; text-decoration: none; 
                                            position: absolute; right: 10px; top: 10px; z-index: 1050;")),
                     status = NULL,
                     solidHeader = TRUE,
                     height = "330px",
                     uiOutput("gse_filter"),
                     plotlyOutput("plot12", height = "200px"),
                     width = NULL
                   )
               )
             ),
             column(
               width = 6,
               div(class = "box-tipo",
                   box(
                     title = tagList("Distribución Estándares de Aprendizaje Nacional", 
                                     actionLink("info_plot13", icon("circle-question"), 
                                                style = "color: #ffffff; font-size: 18px; text-decoration: none; 
                                          position: absolute; right: 10px; top: 10px; z-index: 1050;")),
                     status = NULL,
                     solidHeader = TRUE,
                     height = "330px",
                     plotlyOutput("plot13", height = "275px"),
                     width = NULL
                   )
               )
             ),
             column(
               width = 6,
               div(class = "box-tipo",
                   box(
                     title = tagList("Brecha GSE Alto - Bajo", 
                                     actionLink("info_plot14", icon("circle-question"), 
                                                style = "color: #ffffff; font-size: 18px; text-decoration: none; 
                                          position: absolute; right: 10px; top: 10px; z-index: 1050;")),
                     status = NULL,
                     solidHeader = TRUE,
                     height = "330px",
                     plotlyOutput("plot14", height = "275px"),
                     width = NULL
                   )
               )
             )
           ),
           "genero" = fluidRow(
             column(
               width = 6,
               div(class = "box-tipo",
                   box(
                     title = tagList("Promedio por género", 
                                     actionLink("info_plot21", icon("circle-question"), 
                                                style = "color: #ffffff; font-size: 18px; text-decoration: none; 
                                          position: absolute; right: 10px; top: 10px; z-index: 1050;")),
                     status = NULL,
                     solidHeader = TRUE,
                     height = "330px",
                     plotlyOutput("plot21", height = "275px"),
                     width = NULL
                   )
               )
             ),
             column(
               width = 6,
               div(class = "box-tipo",
                   box(
                     title = tagList("Brecha género por GSE", 
                                     actionLink("info_plot22", icon("circle-question"), 
                                                style = "color: #ffffff; font-size: 18px; text-decoration: none; 
                                          position: absolute; right: 10px; top: 10px; z-index: 1050;")),
                     status = NULL,
                     solidHeader = TRUE,
                     height = "330px",
                     uiOutput("gse_filter"),
                     plotlyOutput("plot22", height = "200px"),
                     width = NULL
                   )
               )
             ),
             column(
               width = 6,
               div(class = "box-tipo",
                   box(
                     title = tagList("Distribución Estándares de Aprendizaje Nacional", 
                                     actionLink("info_plot23", icon("circle-question"), 
                                                style = "color: #ffffff; font-size: 18px; text-decoration: none; 
                                          position: absolute; right: 10px; top: 10px; z-index: 1050;")),
                     status = NULL,
                     solidHeader = TRUE,
                     height = "330px",
                     plotlyOutput("plot23", height = "275px"),
                     width = NULL
                   )
               )
             ),
             column(
               width = 6,
               div(class = "box-tipo",
                   box(
                     title = tagList("Distribución de brechas a nivel establecimiento educacional", 
                                     actionLink("info_plot24", icon("circle-question"), 
                                                style = "color: #ffffff; font-size: 18px; text-decoration: none; 
                                          position: absolute; right: 10px; top: 10px; z-index: 1050;")),
                     status = NULL,
                     solidHeader = TRUE,
                     height = "330px",
                     plotlyOutput("plot24", height = "275px"),
                     width = NULL
                   )
               )
             )
           ),
           #  "dependencia" = fluidRow(
           #    column(
           #      width = 6,
           #      box(
           #        title = tagList("Promedio por Dependencia", 
           #                        actionLink("info_plot31", icon("circle-question"), 
           #                                   style = "color: #ffffff; font-size: 18px; text-decoration: none; 
           #                                position: absolute; right: 10px; top: 10px; z-index: 1050;")),
           #        status = "primary",
           #        solidHeader = TRUE,
           #        height = "330px",
           #        plotlyOutput("plot31", height = "275px"),
           #        width = NULL
           #      )
           #    ),
           #    column(
           #      width = 6,
           #      box(
           #        title = tagList("Promedio por SLEP", 
           #                        actionLink("info_plot32", icon("circle-question"), 
           #                                   style = "color: #ffffff; font-size: 18px; text-decoration: none; 
           #                                position: absolute; right: 10px; top: 10px; z-index: 1050;")),
           #        status = "primary",
           #        solidHeader = TRUE,
           #        height = "330px",
           #        plotlyOutput("plot32", height = "275px"),
           #        width = NULL
           #      )
           #    ),
           #    column(
           #      width = 6,
           #      box(
           #        title = tagList("Brecha Dependencia - Nacional por GSE agrupado", 
           #                        actionLink("info_plot33", icon("circle-question"), 
           #                                   style = "color: #ffffff; font-size: 18px; text-decoration: none; 
           #                                position: absolute; right: 10px; top: 10px; z-index: 1050;")),
           #        status = "primary",
           #        solidHeader = TRUE,
           #        height = "330px",
           #        uiOutput("gse2_filter"),
           #        plotlyOutput("plot33", height = "200px"),
           #        width = NULL
           #      )
           #    ),
           #    column(
           #      width = 6,
           #      box(
           #        title = tagList("Brecha SLEP - Municipal", 
           #                        actionLink("info_plot34", icon("circle-question"), 
           #                                   style = "color: #ffffff; font-size: 18px; text-decoration: none; 
           #                                position: absolute; right: 10px; top: 10px; z-index: 1050;")),
           #        status = "primary",
           #        solidHeader = TRUE,
           #        height = "330px",
           #        plotlyOutput("plot34", height = "275px"),
           #        width = NULL
           #      )
           #    )
           # ),
           "mapa" = fluidRow(
             column(
               width = 12,
               div(class = "box-tipo",
                   box(
                     title = tagList("Resultados por comuna", 
                                     actionLink("info_plot41", icon("circle-question"), 
                                                style = "color: #ffffff; font-size: 18px; text-decoration: none; 
                                          position: absolute; right: 10px; top: 10px; z-index: 1050;")),
                     solidHeader = TRUE,
                     status = NULL,
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
                       width = 400, height = "auto",
                       style = "background: rgba(255, 255, 255, 0.85); padding: 12px; border-radius: 8px; 
                 box-shadow: 2px 2px 5px rgba(0,0,0,0.2); z-index: 1050; font-size: 12px;",
                       
                       div(class = "filter-container",
                           selectInput("region", "Seleccione Región:", 
                                       choices = unique(data_rv$panel_comuna$nombre_region),
                                       selected = "Metropolitana de Santiago"),
                           selectInput("comuna", "Seleccione Comuna:", 
                                       choices = unique(data_rv$panel_comuna$nombre_comuna[data_rv$panel_comuna$nombre_region == "Metropolitana de Santiago"]),
                                       selected = "Providencia"),
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
           ),
           "niveles" = fluidRow(
             # column(
             #   width = 3,
             #   div(class = "box-tipo",
             #       box(
             #         title = tagList("Filtros", 
             #                         actionLink("info_plotXX", icon("circle-question"), 
             #                                    style = "color: #ffffff; font-size: 16px; text-decoration: none; 
             #                              position: absolute; right: 10px; top: 10px; z-index: 1050;")),
             #         status = NULL,
             #         solidHeader = TRUE,
             #         height = "330px",
             #         width = NULL,
             #         fluidRow(
             #           column(width = 12,
             #                  tags$div(
             #                    style = "font-size: 12px;",
             #                    selectInput("filtro_idps_agno", "Año:",
             #                                choices = c(2024))
             #                  ),
             #           ),
             #           column(width = 12,
             #                  tags$div(
             #                    style = "font-size: 12px;",
             #                    selectInput("filtro_idps_ind", "Indicador de Desarrollo Personal y Social:",
             #                                choices = c("Autoestima y Motivación" = "am", "Clima de Convivencia Escolar" = "cc", "Hábitos de Vida Saludable" = "hv", "Participación y Formación Ciudadana" = "pf"))
             #                  ),
             #           ),
             #           column(width = 12,
             #                  tags$div(
             #                    style = "font-size: 12px;",
             #                    selectInput("filtro_idps_grupo", "Grupo:",
             #                                choices = c("Género", "GSE"))
             #                  )
             #           ),
             #           column(width = 12,
             #                  tags$div(
             #                    style = "font-size: 12px;",
             #                    selectInput("filtro_idps_nivel", "Nivel:",
             #                                choices = c("Alto", "Medio", "Bajo"))
             #                  )
             #           )
             #         )
             #       )
             #   )
             # ),
             column(
               width = 6,
               div(class = "box-tipo",
                   box(
                     title = tagList("Distribución nacional por niveles", 
                                     actionLink("info_plot52", icon("circle-question"), 
                                                style = "color: #ffffff; font-size: 18px; text-decoration: none; 
                                          position: absolute; right: 10px; top: 10px; z-index: 1050;")),
                     status = NULL,
                     solidHeader = TRUE,
                     height = "330px",
                     plotlyOutput("plot52", height = "275px"),
                     width = NULL
                   )
               )
             ),
             column(
               width = 6,
               uiOutput("box_plot54")
             ),
             column(
               width = 12,
               div(class = "box-tipo",
                   box(
                     title = tagList("Distribución de niveles por grupo", 
                                     actionLink("info_plot53", icon("circle-question"), 
                                                style = "color: #ffffff; font-size: 18px; text-decoration: none; 
                                          position: absolute; right: 10px; top: 10px; z-index: 1050;")),
                     status = NULL,
                     solidHeader = TRUE,
                     height = "330px",
                     plotlyOutput("plot53", height = "275px"),
                     width = NULL
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
        agno >= max(2012,input$agno[1]) & agno <= input$agno[2]
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
        !is.na(txt),
        !is.na(txt2),
        asig == input$asig,
        grado == input$grado,
        agno >= max(2012,input$agno[1]) & agno <= input$agno[2]
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
  
  # filtered_depe <- reactive({
  #   data_rv$panel_data %>%
  #     filter(
  #       panel == "Dependencia",
  #       txt %in% c("Particular Pagado","Particular Subvencionado","Público"),
  #       asig == input$asig,
  #       grado == input$grado,
  #       agno >= input$agno[1] & agno <= input$agno[2]
  #     ) %>% 
  #     mutate(txt2 = txt,
  #            txt = str_replace_all(txt, " ", "<br>"))
  # })
  # 
  # filtered_slep <- reactive({
  #   data_rv$panel_data %>%
  #     filter(
  #       panel == "Dependencia",
  #       txt %in% c("Municipal","Slep inicio 2018","Slep inicio 2019","Slep inicio 2020","Slep inicio 2022"),
  #       asig == input$asig,
  #       grado == input$grado,
  #       agno >= input$agno[1] & agno <= input$agno[2]
  #     )
  # })
  # 
  # filtered_depe_brecha <- reactive({
  #   data_rv$panel_brecha %>%
  #     filter(
  #       panel == "Dependencia",
  #       txt %in% c("Particular Pagado","Particular Subvencionado","Público"),
  #       asig == input$asig,
  #       grado == input$grado,
  #       agno >= input$agno[1] & agno <= input$agno[2],
  #       txt2 %in% input$gse2
  #     )%>% 
  #     mutate(txt2 = txt,
  #            txt = str_replace_all(txt, " ", "<br>"))
  # })
  # 
  # filtered_slep_brecha <- reactive({
  #   data_rv$panel_brecha %>%
  #     filter(
  #       panel == "Dependencia",
  #       txt %in% c("Slep inicio 2018","Slep inicio 2019","Slep inicio 2020","Slep inicio 2022"),
  #       asig == input$asig,
  #       grado == input$grado,
  #       agno >= input$agno[1] & agno <= input$agno[2]
  #     )
  # })
  
  # Tratamiento - IDPS
  
  filtered_niv_nac <- reactive({
    aux <- data_rv$panel_niveles %>%
      filter(
        panel == "Niveles",
        is.na(txt2),
        idps == input$filtro_idps_ind,
        grado == input$grado,
        agno == input$filtro_idps_agno
      ) %>% 
      mutate(txt = factor(txt, levels = c("Alto", "Medio", "Bajo")))
    
    
    aux$label_sdim <- str_wrap(aux$label_sdim, width = 27)
    aux
  })
  
  filtered_niv_grupo <- reactive({
    req(input$filtro_idps_grupo, input$filtro_idps_ind, input$grado, input$filtro_idps_agno)
    
    base <- data_rv$panel_niveles %>%
      filter(
        panel == "Niveles",
        idps == input$filtro_idps_ind,
        grado == input$grado,
        agno == input$filtro_idps_agno
      )
    
    if (input$filtro_idps_grupo == "Género") {
      base <- base %>%
        filter(txt %in% c("Hombres", "Mujeres")) %>%
        mutate(
          txt  = factor(txt, levels = c("Hombres", "Mujeres")),
          txt2 = factor(txt2, levels = c("Alto", "Medio", "Bajo"))
        )
      base$label_sdim <- str_wrap(base$label_sdim, width = 27)
      base
    } else if (input$filtro_idps_grupo == "GSE") {
      base <- base %>%
        filter(!txt %in% c("Hombres", "Mujeres") & !is.na(txt2)) %>%
        mutate(
          txt  = factor(txt, levels = c("Bajo", "Medio bajo", "Medio", "Medio alto", "Alto")),
          txt2 = factor(txt2, levels = c("Alto", "Medio", "Bajo"))
        )
      base$label_sdim <- str_wrap(base$label_sdim, width = 27)
      base
    } else {
      base[0, ]
    }
  })
  
  
  # Tratamiento - Mapas
  
  filtered_comuna_mapa <- reactive({
    simce_geo <- data_rv$panel_comuna %>%
      filter(
        asig == input$asig,
        grado == input$grado,
        agno == input$anio_sel,
        nombre_region == input$region,
        txt == input$var_mapa
      ) 
    
    simce_geo <- simce_geo %>% 
      left_join(data_rv$comunas_geo, 
                by = c("codigo_comuna","nombre_comuna","nombre_region","codigo_region"))
    simce_geo <- st_sf(simce_geo)
    simce_geo <- st_transform(simce_geo, crs = 4326)
    simce_geo <- st_simplify(simce_geo, dTolerance = 100)
  })
  
  filtered_comuna <- reactive({
    data_rv$panel_comuna %>%
      filter(
        asig == input$asig,
        grado == input$grado,
        agno >= input$agno[1] & agno <= input$agno[2],
        nombre_region == input$region,
        nombre_comuna == input$comuna,
        txt %in% c("Puntaje"),
        !is.na(num)
      ) 
  })
  
  filtered_comuna2 <- reactive({
    data_rv$panel_comuna %>%
      filter(
        asig == input$asig,
        grado == input$grado,
        agno >= max(2012,input$agno[1]) & agno <= input$agno[2],
        nombre_region == input$region,
        nombre_comuna == input$comuna,
        txt != c("Puntaje"),
        !is.na(num)
      ) 
  })
  
  # Definir colores
  colores_eda  <- c("Adecuado" = "#69CCD6", "Elemental" = "#FF5235", "Insuficiente" = "#024A61")
  colores_gse  <- c("Alto" = "#9883E5", "Medio alto" = "#69CCD6", "Medio"= "#024A61", "Medio bajo"= "#FF5235", "Bajo" = "#024A61", "Nacional" = "#012F3A")
  colores_gen  <- c("Hombres" = "#FF5235", "Mujeres" = "#024A61")
  colores_niv  <- c("Alto" = "#69CCD6", "Medio" = "#FF5235", "Bajo" = "#024A61")
  colores_niv2 <- c("Nivel Alto" = "#69CCD6", "Nivel Medio" = "#FF5235", "Nivel Bajo" = "#024A61")
  
  #colores_dep <- c("Público" = "#AA9000", "Particular<br>Subvencionado" = "#00AC71", "Particular<br>Pagado" = "gray62")
  #colores_sle <- c("Municipal" = "#024a61", "Slep inicio 2018" = "#ff5235", "Slep inicio 2019" = "#69ccd6", "Slep inicio 2020" = "#bfbfbf", "Slep inicio 2022" = "#9883E5")

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
    marker = list(color = "#024A61"),
    line = list(color = "#024A61"),
    textfont = list(color = "#024A61"),
    hoverinfo = "text",
    hovertext = ~paste(
      " Año:", agno, "<br>",
      "Puntaje: ", round(num, 1), " puntos"
    )
  ) %>%
    layout(
      title = "",
      xaxis = list(title = "",tickangle = 90),
      yaxis = list(title = "", range = c(200, 320)),
      legend = list(title = list(text = ""))
    ) %>% 
      config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = list("sendDataToCloud")
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
      mode = 'lines+markers',
      hoverinfo = "text",
      hovertext = ~paste(
        " Año:", agno, "<br>",
        "Puntaje: ", round(num, 1), " puntos", "<br>",
        "GSE: ", txt
      )
    ) %>%
      layout(title = "", xaxis = list(title = "",tickangle = 90), yaxis = list(title = "", range = c(200, 320))) %>% 
      config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = list("sendDataToCloud")
      )
  })
  
  output$plot13 <- renderPlotly({
    req(filtered_eda_nac())
    
    p <- filtered_eda_nac() %>%
      ggplot(aes(
        x = factor(agno),
        y = num,
        fill = txt,
        text = paste0("Año: ", agno, "<br>Nivel: ", txt, "<br>Porcentaje: ", round(num, 0), "%")
      )) +
      geom_bar(stat = "identity", position = "stack") +
      geom_text(aes(
        x = factor(agno),
        y = num,
        label = paste0(round(num, 0), "%")
      ),
      position = position_stack(vjust = 0.5),
      size = 2, color = "white", fontface = "bold",
      inherit.aes = FALSE) +
      scale_fill_manual(values = colores_eda) +
      labs(x = NULL, y = NULL, fill = NULL) +
      theme_minimal() +
      theme(
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        axis.text.x = element_text(size = 9, color = "black", angle = -90),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "right",
        legend.text = element_text(size = 10)
      )
    
    plot <- ggplotly(p, tooltip = "text") %>% 
      config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = list("sendDataToCloud")
      )
    
    text_trace_index <- which(sapply(plot$x$data, function(tr) tr$type == "scatter" && tr$mode == "text"))
    
    if (length(text_trace_index) > 0) {
      plot <- style(plot, hoverinfo = "none", traces = text_trace_index)
    }
    
    plot
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
        xaxis = list(title = "",tickangle = 90),
        yaxis = list(title = "", range = c(0, max(filtered_brecha_nac()$brecha)+10)),
        bargap = 0.3
      ) %>% 
      config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = list("sendDataToCloud")
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
      textfont = list(size = 9),
      textposition = ~ifelse(txt == "Hombres" & asig == "mate", "top center", 
                             ifelse(txt == "Mujeres" & asig == "lect", "top center", "bottom center")),
      colors = colores_gen,
      hoverinfo = "text",
      hovertext = ~paste(
        " Año:", agno, "<br>",
        "Género:", txt, "<br>",
        "Puntaje: ", round(num, 1), " puntos"
      )
    ) %>%
      layout(
        title = "",
        xaxis = list(title = "",tickangle = 90),
        yaxis = list(title = "", range = c(200, 320)),
        legend = list(title = list(text = ""))
      ) %>% 
      config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = list("sendDataToCloud")
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
          tickangle = 90,
          type = "category",
          categoryorder = "array",
          categoryarray = levels(factor(filtered_brecha_gen()$agno)) 
        ),
        yaxis = list(title = "", range = c(floor(min(filtered_brecha_gen()$brecha))-2, ceiling(max(filtered_brecha_gen()$brecha))+2)),
        legend = list(title = list(text = ""))
      ) %>% 
      config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = list("sendDataToCloud")
      )
  })
  
  output$plot23 <- renderPlotly({
    req(filtered_eda_gen())
    p <- filtered_eda_gen() %>%
      ggplot(., aes(x = factor(agno), y = num, fill = txt2,
                    text = paste0("Año: ", agno, "<br>Nivel: ", txt2, "<br>Porcentaje: ", round(num, 0), "%"))) +
      geom_bar(stat = "identity", position = "stack") +
      geom_text(aes(
        x = factor(agno),
        y = num,
        label = paste0(round(num, 0), "%")
      ),
      position = position_stack(vjust = 0.5),
      size = 2, color = "white", fontface = "bold",
      inherit.aes = FALSE) +
      scale_fill_manual(values = colores_eda) +
      labs(
        title = NULL,
        x = NULL,
        y = NULL,
        fill = NULL 
      ) +
      facet_grid(rows = vars(txt), scales = "free_y", space = "fixed", switch = "y") + 
      theme_minimal() +
      theme(
        panel.background = element_rect(fill = "white", color = NA), 
        plot.background = element_rect(fill = "white", color = NA),  
        axis.text.x = element_text(size = 9, color = "black", angle = -90), 
        axis.text.y = element_blank(),  
        axis.ticks.y = element_blank(),  
        legend.position = "right",  
        legend.text = element_text(size = 10), 
        strip.text.y.left = element_text(size = 14, face = "bold", color = "blue"), 
        strip.placement = "outside" 
      )
    plot <- ggplotly(p, tooltip = "text") %>% 
      config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = list("sendDataToCloud")
      )
    
    text_trace_index <- which(sapply(plot$x$data, function(tr) tr$type == "scatter" && tr$mode == "text"))
    
    if (length(text_trace_index) > 0) {
      plot <- style(plot, hoverinfo = "none", traces = text_trace_index)
    }
    
    plot
  })
  
  output$plot24 <- renderPlotly({
    req(filtered_porc_brecha_nac())
    
    p <- filtered_porc_brecha_nac() %>% 
      filter(categoria %in% c("Favorece Hombres", "Favorece Mujeres")) %>% 
      mutate(num = ifelse(categoria == "Favorece Mujeres", -num, num)) %>%
      ggplot(aes(x = factor(agno), y = num, fill = categoria,
                 text = paste0("Año: ", agno, "<br>", categoria, "<br>Porcentaje: ", round(abs(num), 0), "%"))) +
      geom_bar(stat = "identity") + 
      geom_text(aes(
        x = factor(agno),
        y = num,
        label = paste0(round(abs(num), 0), "%")
      ),
      position = position_stack(vjust = 0.5),
      size = 2, color = "white", fontface = "bold",
      inherit.aes = FALSE) +
      scale_fill_manual(values = c("Favorece Hombres" = "#FF5235", 
                                   "Favorece Mujeres" = "#024A61")) +
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
        axis.text.x = element_text(size = 9, color = "black", angle = -90),
        axis.text.y = element_text(size = 9, color = "black"),
        axis.ticks.y = element_blank(),
        legend.position = "right", 
        legend.text = element_text(size = 8)
      )
    plot <- ggplotly(p, tooltip = "text") %>% 
      config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = list("sendDataToCloud")
      )
    
    text_trace_index <- which(sapply(plot$x$data, function(tr) tr$type == "scatter" && tr$mode == "text"))
    
    if (length(text_trace_index) > 0) {
      plot <- style(plot, hoverinfo = "none", traces = text_trace_index)
    }
    
    plot
  })
  
  # Gráficos Dependencia
  # output$plot31 <- renderPlotly({
  #   req(filtered_depe())
  #   plot_ly(
  #     data = filtered_depe(),
  #     x = ~factor(agno),
  #     y = ~num,
  #     color = ~txt,
  #     type = 'scatter',
  #     mode = 'lines+markers+text',
  #     text = ~round(num, 0),
  #     textfont = list(size = 9),
  #     textposition = "top center",
  #     colors = colores_dep
  #   ) %>%
  #     layout(
  #       title = "",
  #       xaxis = list(title = "",tickangle = 90),
  #       yaxis = list(title = "", range = c(200, 340)),
  #       #yaxis = list(title = "", range = c(min(filtered_depe()$num)-10, max(filtered_depe()$num)+10)),
  #       legend = list(title = list(text = ""))
  #     )
  # })
  # 
  # output$plot32 <- renderPlotly({
  #   req(filtered_slep())
  #   plot_ly(
  #     data = filtered_slep(),
  #     x = ~factor(agno),
  #     y = ~num,
  #     color = ~txt,
  #     type = 'scatter',
  #     mode = 'lines+markers',
  #     colors = colores_sle
  #   ) %>%
  #     layout(
  #       title = "",
  #       xaxis = list(title = "",tickangle = 90),
  #       yaxis = list(title = "", range = c(200, 320)),
  #       legend = list(title = list(text = ""))
  #     )
  # })
  # 
  # output$plot33 <- renderPlotly({
  #   req(filtered_depe_brecha())
  #   plot_ly(
  #     data = filtered_depe_brecha(),
  #     x = ~agno,
  #     y = ~brecha,
  #     color = ~txt,
  #     colors = colores_dep,
  #     type = 'scatter',
  #     mode = 'lines+markers',
  #     textposition = ~ifelse(brecha >= 0, "top center", "bottom center"),
  #     hovertext = ~paste0(
  #       "Año: ", agno, "<br>",
  #       "Brecha: ", round(brecha), " puntos", "<br>",
  #       "Diferencia: ", ifelse(brecha_abs >= 6, "Significativa", "No Significativa")
  #     ),
  #     hoverinfo = "text"
  #   ) %>%
  #     layout(
  #       title = "",
  #       xaxis = list(
  #         title = "",
  #         tickangle = 90,
  #         type = "category",
  #         categoryorder = "array",
  #         categoryarray = levels(factor(filtered_depe_brecha()$agno)) 
  #       ),
  #       yaxis = list(title = "", range = c(floor(min(filtered_depe_brecha()$brecha))-2, ceiling(max(filtered_depe_brecha()$brecha))+2)),
  #       legend = list(title = list(text = ""))
  #     )
  # })
  # 
  # output$plot34 <- renderPlotly({
  #   req(filtered_slep_brecha())
  #   plot_ly(
  #     data = filtered_slep_brecha(),
  #     x = ~agno,
  #     y = ~brecha,
  #     color = ~txt,
  #     colors = colores_sle,
  #     type = 'scatter',
  #     mode = 'lines+markers',
  #     textposition = ~ifelse(brecha >= 0, "top center", "bottom center"),
  #     hovertext = ~paste0(
  #       "Año: ", agno, "<br>",
  #       "Brecha: ", round(brecha), " puntos", "<br>",
  #       "Diferencia: ", ifelse(brecha_abs >= 6, "Significativa", "No Significativa")
  #     ),
  #     hoverinfo = "text"
  #   ) %>%
  #     layout(
  #       title = "",
  #       xaxis = list(
  #         title = "",
  #         tickangle = 90,
  #         type = "category",
  #         categoryorder = "array",
  #         categoryarray = levels(factor(filtered_slep_brecha()$agno)) 
  #       ),
  #       yaxis = list(title = "", range = c(floor(min(filtered_slep_brecha()$brecha))-2, ceiling(max(filtered_slep_brecha()$brecha))+2)),
  #       legend = list(title = list(text = ""))
  #     )
  # })
  
  # Gráficos Mapas
  output$plot41 <- renderLeaflet({
    req(filtered_comuna_mapa())
    
    df <- filtered_comuna_mapa()
    
    center <- st_centroid(st_union(df))
    center_coords <- st_coordinates(center)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%g %s<br/>Total: %d estudiantes<br/>Año: %s",
      df$nombre_comuna, 
      round(df$num, 1),
      ifelse(input$var_mapa == "Puntaje", "Puntos", "%"),
      df$n,
      input$anio_sel
    ) %>% lapply(htmltools::HTML)
    
    paleta <- colorNumeric("Blues", domain = df$num, na.color = "white")
    
    mapa <- leaflet(df) %>%
      addProviderTiles(provider = providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~paleta(num),
        color = ~ifelse(nombre_comuna == selected_comuna(), "red", "black"),
        weight = ~ifelse(nombre_comuna == selected_comuna(), 4, 1), 
        fillOpacity = 0.8,
        label = ~labels,
        layerId = ~nombre_comuna,
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
        labFormat = labelFormat(suffix = ifelse(input$var_mapa == "Puntaje", " pts", "%")),
        na.label = ""
      )
    
    if (all(df$nombre_region == "Valparaiso")) {
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
      geom_line(data = df_long %>% filter(tipo == "num"), linewidth = 1, color = "#69CCD6") +  
      geom_line(data = df_long %>% filter(tipo == "ptje_nac"), linewidth = 1, alpha = 0.8, 
                color = "#FF5235", linetype = "dashed") +  
      geom_point(data = df_long %>% filter(tipo == "num"), aes(shape = tipo), size = 3, color = "#69CCD6") +  
      geom_point(data = df_long %>% filter(tipo == "ptje_nac"), aes(shape = tipo), size = 3, alpha = 0.5, color = "#FF5235") +  
      geom_text(data = df_long %>% filter(tipo == "num"), 
                aes(x = factor(agno), y = puntaje, label = round(puntaje, 0)), 
                vjust = -1, size = 3.5, color = "black", fontface = "bold", inherit.aes = FALSE) +  
      
      scale_x_discrete() +
      scale_y_continuous(limits = c(min(df_long$puntaje) - 10, max(df_long$puntaje) + 15)) +
      labs(x = "", y = "") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(size = 10, angle = -90, vjust = 0.5, hjust = 1, face = "bold"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "top"
      ) +
      scale_color_manual(values = c("num" = "#69CCD6", "ptje_nac" = "#FF5235"), labels = c("Puntaje Comuna", "Puntaje Nacional")) + 
      scale_shape_manual(values = c("num" = 16, "ptje_nac" = 16), labels = c("Puntaje Comuna", "Puntaje Nacional")) + 
      guides(shape = guide_legend(title = ""), color = guide_legend(title = ""))
  })
  
  output$plot43 <- renderPlot({
    req(filtered_comuna2())
    ggplot(data = filtered_comuna2() %>% arrange(agno), aes(x = factor(agno), y = num, fill = txt)) +  
      geom_bar(stat = "identity", position = "stack") +
      geom_text(aes(label = paste0(round(num), "%")), 
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
        axis.text.x = element_text(size = 10, angle = -90, vjust = 0.5, hjust = 1, face = "bold"), 
        axis.text.y = element_blank(),  
        axis.ticks.y = element_blank(),  
        legend.position = "none",  
        legend.text = element_text(size = 8)
      )
  })
  
  # Graficos Niveles
  
  output$plot52 <- renderPlotly({
    req(filtered_niv_nac())
    
    p <- filtered_niv_nac() %>%
      mutate(txt = case_when(
        txt == "Bajo"  ~ "Nivel Bajo",
        txt == "Medio" ~ "Nivel Medio",
        txt == "Alto"  ~ "Nivel Alto",
        TRUE           ~ txt
      ),
      txt = factor(txt, levels = c("Nivel Alto", "Nivel Medio", "Nivel Bajo"))
      ) %>% 
      ggplot(aes(
        x = factor(label_sdim, levels = rev(unique(label_sdim))),
        y = num,
        fill = txt,
        text = paste0(
          "Año: ", agno,
          "<br>Nivel: ", substring(txt, 7),
          "<br>Porcentaje: ", round(num, 0), "%"
        )
      )) +
      geom_bar(stat = "identity") +
      geom_text(
        aes(
          x = factor(label_sdim, levels = rev(unique(label_sdim))),
          y = num,
          label = paste0(round(num, 0), "%")
        ),
        position = position_stack(vjust = 0.5),
        size = 2,
        color = "white",
        fontface = "bold",
        inherit.aes = FALSE
      ) +
      coord_flip() +
      scale_fill_manual(values = colores_niv2) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        legend.key.height = unit(0.8, "cm"),
        legend.key.width  = unit(0.8, "cm"),
        legend.text = element_text(size = 8),
        strip.text = element_text(face = "bold"),
        axis.text.y = element_text(hjust = 1),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      ) +
      labs(x = NULL, y = NULL, fill = NULL)
    
    plot <- ggplotly(p, tooltip = "text") %>% 
      config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = list("sendDataToCloud")
      )
    
    text_trace_index <- which(sapply(plot$x$data, function(tr) {
      tr$type == "scatter" && tr$mode == "text"
    }))
    
    if (length(text_trace_index) > 0) {
      plot <- style(plot, hoverinfo = "none", traces = text_trace_index)
    }
    
    plot
  })
  
  output$plot53 <- renderPlotly({
    req(filtered_niv_grupo())
    
    text_size <- if (input$filtro_idps_grupo == "Género") 2 else 1.5
    
    p <- filtered_niv_grupo() %>%
      mutate(txt2 = case_when(
        txt2 == "Bajo"  ~ "Nivel Bajo",
        txt2 == "Medio" ~ "Nivel Medio",
        txt2 == "Alto"  ~ "Nivel Alto",
        TRUE           ~ txt2
      ),
        txt2 = factor(txt2, levels = c("Nivel Alto", "Nivel Medio", "Nivel Bajo"))
      ) %>% 
      ggplot(aes(
        x = factor(label_sdim, levels = rev(unique(label_sdim))),
        y = num,
        fill = txt2,
        text = paste0(
          "Año: ", agno,
          "<br>Grupo: ", txt,
          "<br>Nivel: ", substring(txt2, 7),
          "<br>Porcentaje: ", round(num, 0), "%"
        )
      )) +
      geom_bar(stat = "identity") +
      geom_text(
        aes(
          x = factor(label_sdim, levels = rev(unique(label_sdim))),
          y = num,
          label = paste0(round(num, 0), "%")
        ),
        position = position_stack(vjust = 0.5),
        size = 2,
        color = "white",
        fontface = "bold",
        inherit.aes = FALSE
      ) +
      coord_flip() +
      facet_wrap(~txt, ncol = 5) +
      scale_fill_manual(
        values = colores_niv2
      ) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        legend.key.height = unit(0.8, "cm"),
        legend.key.width  = unit(0.8, "cm"),
        legend.text = element_text(size = 8),
        strip.text = element_text(face = "bold"),
        axis.text.y = element_text(hjust = 1),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      ) +
      labs(x = NULL, y = NULL, fill = NULL)
    
    plot <- ggplotly(p, tooltip = "text") %>% 
      config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = list("sendDataToCloud")
      )
    
    text_trace_index <- which(sapply(plot$x$data, function(tr) {
      tr$type == "scatter" && tr$mode == "text"
    }))
    
    if (length(text_trace_index) > 0) {
      plot <- style(plot, hoverinfo = "none", traces = text_trace_index)
    }
    
    plot
  })
  
  output$box_plot54 <- renderUI({
    req(input$filtro_idps_nivel)
    
    div(class = "box-tipo",
        box(
          title = tagList(
            paste0("Distribución de Niveles por grupo y nivel: ", input$filtro_idps_nivel),
            actionLink("info_plot54", icon("circle-question"),
                       style = "color: #ffffff; font-size: 18px; text-decoration: none; 
                              position: absolute; right: 10px; top: 10px; z-index: 1050;")
          ),
          status = NULL,
          solidHeader = TRUE,
          height = "330px",
          plotlyOutput("plot54", height = "275px"),
          width = NULL
        )
    )
  })
  
  
  output$plot54 <- renderPlotly({
    req(filtered_niv_grupo())
    
    df <- filtered_niv_grupo() %>%
      filter(txt2 == input$filtro_idps_nivel) %>%
      mutate(
        txt = if (input$filtro_idps_grupo == "GSE") {
          factor(txt, levels = c("Alto", "Medio alto", "Medio", "Medio bajo", "Bajo"))
        } else {
          factor(txt, levels = c("Hombres", "Mujeres"))
        }
      ) %>%
      select(txt, label_sdim, num) %>%
      pivot_wider(names_from = label_sdim, values_from = num)

    categorias <- sort(setdiff(names(df), "txt"))
    max_char_linea <- 9
    
    categorias_etiquetas <- sapply(categorias, function(cat) {
      if (nchar(cat) > max_char_linea) {
        corte <- max(gregexpr(" ", substr(cat, 1, max_char_linea))[[1]])
        if (is.finite(corte) && corte > 0) {
          paste0(
            substr(cat, 1, corte - 1), "<br>",
            substr(cat, corte + 1, nchar(cat))
          )
        } else {
          cat
        }
      } else {
        cat
      }
    })
    
    
    
    categorias_cerradas <- c(categorias_etiquetas, categorias_etiquetas[1])
    
    
    color <- if (input$filtro_idps_grupo == "Género") colores_gen else colores_gse
    
    p <- plot_ly()
    
    df <- df[order(df$txt), ]
    
    
    for (i in 1:nrow(df)) {
      grupo <- df$txt[i]
      valores <- as.numeric(df[i, categorias])
      valores_cerrados <- c(valores, valores[1])
      texto_hover <- paste0(
        "Grupo: ", grupo, "<br>",
        "Porcentaje: ", round(valores_cerrados,0), "%"
      )
      
      p <- p %>%
        add_trace(
          type = "scatterpolar",
          r = valores_cerrados,
          theta = categorias_cerradas,
          mode = "lines+markers",
          name = grupo,
          line = list(color = color[grupo], width = 3),
          marker = list(size = 8, color = color[grupo]),
          fill = "none",
          text = texto_hover,
          hoverinfo = "text"
        )
    }
    
    p %>%
      layout(
        polar = list(
          gridshape = "linear",
          angularaxis = list(
            rotation = 90,
            direction = "clockwise",
            tickfont = list(size = 10)
          ),
          radialaxis = list(
            visible = TRUE,
            range = c(0, 100),
            angle = 90,
            tickangle = 90,
            tickfont = list(color = "gray")
          )
        ),
        legend = list(
          orientation = "v",
          x = 1.05,
          xanchor = "left",
          y = 0.5
        ),
        margin = list(l = 50, r = 120, b = 50, t = 50)
      )  %>% 
      config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = list("sendDataToCloud")
      )
  })
}

shinyApp(ui, server)
