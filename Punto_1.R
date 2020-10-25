#Punto 1 - Felipe Velásquez


library(lubridate)
library(dplyr)
library(plotly)
library(shiny)
library(shinydashboard)

#Lectura y procesamiento de datos

ruta1 <- "F:/Desktop/Prueba Tecnica Inversiones/DATA1.csv"
ruta2 <- "F:/Desktop/Prueba Tecnica Inversiones/DATA2.csv"
data <- read.csv(ruta1, sep = ";", stringsAsFactors = FALSE)
data_acciones <- read.csv(ruta2, sep = ";", stringsAsFactors = FALSE)

data$Fecha <- dmy(data$Fecha)

data_diaria <- data %>% mutate(Fecha_mes = format(ymd(as.character(paste(year(Fecha), month(Fecha), "01", sep = "-"))), "%Y-%m"))

data_mensual <- data %>% mutate(Periodo = year(Fecha),
                                Mes = month(Fecha)) %>% group_by(Periodo, Mes) %>% summarise(TRM = mean(TRM),
                                                                                             BRENT = mean(BRENT))
data_mensual$Fecha <- format(ymd(as.character(paste(data_mensual$Periodo, data_mensual$Mes, "01", sep = "-"))), "%Y-%m")
data_mensual <- data_mensual %>% select(Fecha, TRM, BRENT)

data_anual <- data %>% group_by(Periodo = year(Fecha)) %>% summarise(TRM = mean(TRM), BRENT = mean(BRENT))

corr_d <- cor(data_diaria$TRM, data_diaria$BRENT)
corr_m <- cor(data_mensual$TRM, data_mensual$BRENT)
corr_a <- cor(data_anual$TRM, data_anual$BRENT)

stats <- data %>% group_by(Periodo = year(Fecha)) %>% summarise(Min_TRM = min(TRM), Promedio_TRM = mean(TRM), Max_TRM = max(TRM),
                                                                Min_BRENT = min(BRENT), Promedio_BRENT = mean(BRENT), Max_BRENT = max(BRENT))
data_acciones$Fecha <- dmy(data_acciones$Fecha)

ui <- dashboardPage(
        dashboardHeader(title = "Prueba Técnica Analista de Inversiones - Punto 1 Felipe Velásquez", titleWidth = 800),
        dashboardSidebar(
          sidebarMenu(id = 'sidebarmenu',
                      
                      # first menu item
                      menuItem('Parte 1: TRM y Petroleo',
                               icon = icon('line-chart'),
                               menuSubItem('Gráficas',
                                           tabName = 'graficas',
                                           icon = icon('line-chart')),
                               menuSubItem('Boxplots',
                                           tabName = 'boxplots',
                                           icon = icon('line-chart')),
                               menuSubItem('Correlación',
                                           tabName = 'correlacion',
                                           icon = icon('line-chart')),             
                               menuSubItem('Tabla',
                                           tabName = 'tabla',
                                           icon = icon('line-chart'))             
                               ),
                      
                      # second menu item
                      menuItem("Visualizador Acciones S&P 500", tabName = "P2", icon = icon("dashboard"))

          )),
        
        
        dashboardBody(
          tabItems(
            tabItem("graficas", 
                    h4("Comportamiento diario, mensual y anual: TRM vs Petroleo"),
                    fluidRow(
                      box(width = 12, plotlyOutput(outputId = "g1"))
                      ),
                    fluidRow(
                      box(width = 12, plotlyOutput(outputId = "g2"))
                    ),
                    fluidRow(
                      box(width = 12, plotlyOutput(outputId = "g3"))
                    )
                  ),
            tabItem("boxplots", 
                    h4("Boxplots mensuales"),
                    fluidRow(
                      box(width = 12,  plotlyOutput(outputId = "g4"))
                    ),
                    fluidRow(
                      box(width = 12, plotlyOutput(outputId = "g5"))
                    )
                  ),
            tabItem("correlacion", 
                    h4("Correlación hitórica TRM vs Petroleo"),
                    fluidRow(column(12,
                                    selectInput("Periodicidad",
                                                h3("Seleccione la Periodicidad Deseada"),
                                                choices = c("Diaria", "Mensual", "Anual"),
                                                selected = "Diaria")),
                      box(width = 8, plotlyOutput(outputId = "g6")),
                      box(width = 4, valueBoxOutput(outputId = "vb1"))
                    )
                  ),
            tabItem("tabla", 
                    h4("Tabla estádisticas descriptivas mensuales"),
                    fluidRow(
                      box(width = 12, tableOutput('t1'))
                    )
                  ),
            tabItem("P2", 
                    h4("Visualizador Acciones S&P 500"),
                    fluidRow(
                      column(3,
                             selectInput("Tickers",
                                         h3("Tickers"),
                                         choices = unique(data_acciones$Ticker),
                                         selected = "AAPL")),
                      column(3,
                             dateInput("Fecha_Inicio",
                                       h3("Fecha Inicial"),
                                       value = min(data_acciones$Fecha))),
                      column(3,
                             dateInput("Fecha_Fin",
                                       h3("Fecha Final"),
                                       value = max(data_acciones$Fecha)))
                    ),
                    
                    mainPanel(width = 12,
                      plotlyOutput(outputId = "g7")
                    )
            
            )
          )
          

          
        )
)


server <- function(input, output) {
  
  output$g1 <- renderPlotly({
    
    plot1 <- plot_ly(data = data_diaria) %>%
              add_lines(x = ~Fecha, y = ~TRM, name = "TRM", mode = "lines") %>%
              add_lines(x = ~Fecha, y = ~BRENT, name = "BRENT", yaxis = "y2", mode = "lines") %>%
              layout(
                title = "TRM vs BRENT: Precios Diarios",
                yaxis2 = list(overlaying = "y", side = "right")
                )
  })
    
  output$g2 <- renderPlotly({
    
    plot2 <- plot_ly(data = data_mensual) %>%
      add_lines(x = ~Fecha, y = ~TRM, name = "TRM Promedio Mes", mode = "lines") %>%
      add_lines(x = ~Fecha, y = ~BRENT, name = "BRENT Promedio Mes", yaxis = "y2", mode = "lines") %>%
      layout(
        title = "TRM vs BRENT: Precios Mensuales",
        yaxis2 = list(overlaying = "y", side = "right")
      )
  })
  
  output$g3 <- renderPlotly({
    
    plot3 <- plot_ly(data = data_anual) %>%
      add_lines(x = ~Periodo, y = ~TRM, name = "TRM Promedio Año", mode = "lines") %>%
      add_lines(x = ~Periodo, y = ~BRENT, name = "BRENT Promedio Año", yaxis = "y2", mode = "lines") %>%
      layout(
        title = "TRM vs BRENT: Precios Anuales",
        yaxis2 = list(overlaying = "y", side = "right")
      )
  })
  
  output$g4 <- renderPlotly({
    
    plot4 <- plot_ly(data = data_diaria) %>%
      add_boxplot(x = ~Fecha_mes, y = ~TRM, name = "BoxPlot Mensual TRM") %>%
      layout(
        title = "Box Plots Mensuales TRM"
      )
  })
  
  output$g5 <- renderPlotly({
    
    plot5 <- plot_ly(data = data_diaria) %>%
      add_boxplot(x = ~Fecha_mes, y = ~BRENT, name = "BoxPlot Mensual BRENT") %>%
      layout(
        title = "Box Plots Mensuales BRENT"
      )
  })
  
  var_corr <- reactive({
    
    if (input$Periodicidad == "Diaria") {
      
      return(corr_d)
      
    } else if (input$Periodicidad == "Mensual") {
      
      return(corr_m)
      
    } else {
      
      return(corr_a)
      
    }
  })
  
  var_data <- reactive({
    
    if (input$Periodicidad == "Diaria") {
      
      return(data_diaria)
      
    } else if (input$Periodicidad == "Mensual") {
      
      return(data_mensual)
      
    } else {
      
     return(data_anual)
      
    }
  })
  
  output$g6 <- renderPlotly({
    
    plot6 <- plot_ly(data = var_data(), x = ~TRM, y = ~BRENT) %>%
      layout(
        title = "Correlación Petroleo vs TRM"
      )
  })
  
  output$vb1 <- renderValueBox({
    valueBox(
      paste0(round(var_corr() * 100, 2), "%"), "Correlación", icon = icon("list"),
      color = "blue"
    )
  })
  
  output$t1 <- renderTable(stats)
  
  output$g7 <- renderPlotly({
    
    data_acciones_f <- filter(data_acciones, Ticker == input$Tickers, Fecha >= input$Fecha_Inicio, Fecha <= input$Fecha_Fin)
    
    plot7 <- plot_ly(data = data_acciones_f, x = ~Fecha, type = "candlestick",
                     open = ~Apertura, close = ~ Cierre,
                     high = ~Max, low = ~Min) %>%
      layout(xaxis = list(rangeslider = list(visible = F)))
  })
  
}

shinyApp(ui = ui, server = server)
