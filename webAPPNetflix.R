############################################################################
# 1. CARREGAMENTO DOS PACOTES
############################################################################

library(shiny)
library(shinyBS)
library(plotly)

############################################################################
# 2. CARREGAMENTO DOS DADOS
############################################################################

# Carregamento dos Datasets
dataset1 <- read.csv(
  'https://raw.githubusercontent.com/ThiagoFerreiraWD/Netflix-DataAnalysis/main/dataset1.csv')
genre <- read.csv(
  'https://raw.githubusercontent.com/ThiagoFerreiraWD/Netflix-DataAnalysis/main/dataset2.csv')
tree <- read.csv(
  'https://raw.githubusercontent.com/ThiagoFerreiraWD/Netflix-DataAnalysis/main/dataset3.csv')
countries <- read.csv(
  'https://raw.githubusercontent.com/ThiagoFerreiraWD/Netflix-DataAnalysis/main/dataset6.csv')

# Ajuste do tipo de dado de algumas colunas
dataset1$X..of.Subscribers.Q4.2021..Estimate. <- as.numeric(
  gsub(",", "", dataset1$X..of.Subscribers.Q4.2021..Estimate.))
dataset1$Q4.2021.Revenue....Estimate. <- as.numeric(
  gsub(",", "", dataset1$Q4.2021.Revenue....Estimate.))

# Filtro de Outliers
dataset1_scat_out <- filter(dataset1, Country != "United States")
dataset1_bar <- filter(dataset1, Country != "Switzerland")
dataset1_bar_out <- filter(dataset1_bar, Country != "South Africa")

# Filtro lista de países removendo valores NA
listaPaises <- filter(countries, is.na(parents))

############################################################################
# 3. UI
############################################################################


light_theme <- bslib::bs_theme(primary = "#E50914",
                         "input-border-color" = "#E50914",
                         base_font = c("Grandstander", "sans-serif"),
                         heading_font = "'Helvetica Neue', Helvetica, sans-serif")

dark_theme <- bslib::bs_theme(bg = "#121212",
                        fg = "#E4E4E4",
                        primary = "#E50914",
                        secondary = "#FFFFFF",
                        base_font = c("Grandstander", "sans-serif"),
                        heading_font = "'Helvetica Neue', Helvetica, sans-serif",
                        "input-border-color" = "#E50914")


ui <- 
  navbarPage(
    
    # TÍTULO DO PROJETO
    title = span("NETFLIX", style = "color: #E50914; font-size: 30px"),
    
    # TEMA DO SHINY
    theme = dark_theme,
    
    
    # CHECKBOX DARK MODE
    checkboxInput('dark_mode', 'Desabilitar Tema Escuro'),
    
    
# -- ABA 1 - VISÃO GERAL ---------------------------------------------------
                    
    tabPanel(
      
      span("Visão Geral", style = "font-size: 15px"),
      
      sidebarLayout(
        sidebarPanel(
          width = 2,
          selectInput("select", 
                      label = h6("Selecione a Variável do Eixo Y:"), 
                      choices = list("Faturamento Netflix Q4-2021" = "Q4.2021.Revenue....Estimate.", 
                                     "Assinaturas Netflix Q4-2021" = "X..of.Subscribers.Q4.2021..Estimate.",
                                     "Tamanho do Catálogo" = "Total.Library.Size", 
                                     "Preço Assinatura Basic" = "Cost.Per.Month...Basic....", 
                                     "Preço Assinatura Standard"= "Cost.Per.Month...Standard....",
                                     "Preço Assinatura Premium" = "Cost.Per.Month...Premium...."), 
                      selected = 1),
          checkboxInput("outlierscatter", "Mostrar Outlier", FALSE)),
        mainPanel(
          plotlyOutput("scatPlot")))),
        

# -- ABA 2 - DESIGUALDADE SALARIAL ------------------------------------------   
    
    tabPanel(
      
      span("Desigualdade Salarial", style = "font-size: 15px"),
      
      sidebarLayout(
        sidebarPanel(
          width = 2,
          checkboxInput("outlierbar", "Exibir Outliers", F),
        ),
        mainPanel(
          h4("Disparidade de Renda e Diferenças nos Preços da Assinatura Basic, 
             Standard e Premium da Netflix (Mensal)"),
          plotlyOutput("barPlot")))),
    
# -- ABA 3 - GÊNEROS POPULARES ----------------------------------------------

    tabPanel(
      
      span('Gêneros Populares', style = 'font-size: 15px'),
      
      sidebarLayout(
        sidebarPanel(
          width = 2,
          selectInput('Country', 
                      label = h5('Selecione o País:'), 
                      choices = listaPaises$labels, 
                      selected = 1)
        ),
        mainPanel(
          h4('Popularidade de Gênero dos Filmes Por País'),
          span(h6('Baseado no número de vezes em que um filme/programa de TV 
              de um determinado gênero esteve no Top 10 semanal em um país 
              (Dados de Junho 2021-Março 2022).', style = 'font-size: 15px')),
          plotlyOutput('countryPlot')))),
    
# -- ABA 4 - ASSINANTES_-------------------------------------------------------

    tabPanel(
      
      span('Assinantes', style = 'font-size: 15px'),
      
      sidebarLayout(
        sidebarPanel(
          width = 2,
          checkboxInput('outliermap', 'Remover Estados Unidos', T),
        ),
        mainPanel(
          h4('Número Total de Assinantes. Ref. Q4 - 2021'),
          plotlyOutput('mapPlot')
        ),
      )),

# -- ABA 5 - VER MAIS ------------------------------------------------------

    navbarMenu(
      
      span('Ver Mais', style = 'font-size: 15px'),
      
      tabPanel(a('Projeto Completo', 
                 href='https://github.com/ThiagoFerreiraWD/Netflix-DataAnalysis', 
                 target='_blank')),
      tabPanel(a('Site do Autor', 
                 href='https://thiagoferreirads.com', 
                 target='_blank')))


)

############################################################################
# 4. SERVER
############################################################################

server <- function(input, output, session) {
  
  observe(session$setCurrentTheme(
    if (isTRUE(input$dark_mode)) light_theme else dark_theme))
  
# -- ABA 1 - SCATTER --------------------------------------------------------  

  output$scatPlot <- renderPlotly({
    
    if (input$outlierscatter) dfs <- dataset1 else dfs <- dataset1_scat_out
    
    fig <- plot_ly(data = dfs, 
                   x = ~X2020.GDP..World.Bank., 
                   y = ~get(input$select), 
                   type = "scatter", 
                   mode = "markers", 
                   text = ~Country,
                   marker = list(size = 8.5,
                                 color = 'rgba(229, 9, 20, .7)',
                                 line = list(color = 'rgba(0, 0, 0)',
                                             width = 1)))
    
    fig <- fig %>% layout(yaxis = list(title = input$select), 
                          xaxis = list(title = 'PIB (Em USD)'))
    fig
  })
  
# -- ABA 2 - BAR PLOT --------------------------------------------------------
  output$barPlot <- renderPlotly({
    
    if (input$outlierbar) dfb <- dataset1_bar else dfb <- dataset1_bar_out
    
    fig <- plot_ly(dfb, 
                   x = ~gini_disp, 
                   y = ~Cost.Per.Month...Basic...., 
                   type = 'bar', 
                   name = 'Basic', 
                   text = ~Country)
    
    fig <- fig %>% add_trace(y = ~basic_standard_diff, 
                             name = 'Standard')
    fig <- fig %>% add_trace(y = ~standard_premium_diff, 
                             name = 'Premium')
    fig <- fig %>% layout(yaxis = 
                            list(title = 'Custo Mensal dos Planos Basic, Standard e Premium (USD)', 
                                       titlefont = list(size=10)), 
                          xaxis = 
                            list(title = 'Desigualdade Salarial (GINI)'), 
                          barmode = 'stack')
    fig
  })

# -- ABA 3 - TREE MAP --------------------------------------------------------
  
  output$countryPlot <- renderPlotly({
    country <- filter(countries, parents == input$Country)
    country <- rbind(filter(countries, labels == input$Country), country)
    fig <- plot_ly(country, 
                   ids = ~id, 
                   labels = ~labels, 
                   parents = ~parents, 
                   values = ~n, type = 'treemap', 
                   branchvalues = 'total', 
                   pathbar = list(visible = TRUE),
                   opacity=0.85,
                   textinfo="label+value+percent")
    fig
  })
  
  output$treePlot <- renderPlotly({
    fig <- plot_ly(tree, 
                   ids = ~id, 
                   labels = ~label, 
                   parents = ~parent, 
                   values = ~n, 
                   type = 'treemap', 
                   branchvalues = 'total', 
                   pathbar = list(visible = TRUE),
                   opacity = 0.85)
    fig
  })
  
# -- ABA 4 - NÚMERO DE ASSINANTES----------------------------------------------

  
  output$mapPlot <- renderPlotly({
    if (input$outliermap){
      dfm <- dataset1
      dfm$logAssinantes <- dfm$X..of.Subscribers.Q4.2021..Estimate.
    } else {
      dfm <- dataset1_scat_out
      dfm$logAssinantes <- log(dfm$X..of.Subscribers.Q4.2021..Estimate.)
    }
    
    l <- list(color = toRGB('black'), width = 0.5)
    
    fig <- plot_geo(dfm)
    fig <- fig %>% add_trace(z = ~logAssinantes, 
                             color = ~logAssinantes, 
                             colors = 'Blues',
                             text = ~Country, 
                             locations = ~Alpha.3.code,
                             marker = list(line = l))
    
    fig <- fig %>% colorbar(title = 'Escala')
    fig
  })
  
}



############################################################################
# 5. Execução da APP
############################################################################
shinyApp(ui, server)