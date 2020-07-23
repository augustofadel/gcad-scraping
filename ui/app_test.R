library(shiny)
library(shinydashboard)
library(shinyFiles)
library(lubridate)

dir_gcad <- '//WARQPRD14V/cempre/GCAD/REGISTRO_ADMINISTRATIVO'

ui <- dashboardPage(
  
  dashboardHeader(title = 'GCAD scraping'),
  dashboardSidebar(
    sidebarMenu(
      menuItem('ANATEL', tabName = 'anatel', icon = icon('database')),
      menuItem('BACEN', tabName = 'bacen', icon = icon('database')),
      menuItem('Bovespa', tabName = 'bovespa', icon = icon('database')),
      menuItem('CNES', tabName = 'cnes', icon = icon('database')),
      menuItem('MTur/Cadastur', tabName = 'cadastur', icon = icon('database')),
      menuItem('Portal da Trasparência', tabName = 'portal_transparencia', icon = icon('database')),
      menuItem('SRF', tabName = 'srf', icon = icon('check'))
    )
  ),
  
  # dashboardBody(
  #   # Boxes need to be put in a row (or column)
  #   fluidRow(
  #     box(plotOutput('plot1', height = 250)),
  #     
  #     box(
  #       title = 'Controls',
  #       sliderInput('slider', 'Number of observations:', 1, 100, 50)
  #     )
  #   )
  # )
  dashboardBody(
    tabItems(
      
      tabItem(
        tabName = 'bacen',
        h2('Relação de instituições em funcionamento no país'),
        h5(
          'Fonte: Banco Central do Brasil', 
          tags$a(
            href = 'https://www.bcb.gov.br/estabilidadefinanceira/relacsaveao_instituicoes_funcionamento', 
            icon('link')
          )
        ),
        fluidRow(
          box(
            checkboxGroupInput(
              inputId = 'tipo_bacen1', 
              label = 'Tipos de instituições', 
              choices = 
                list(
                  'conglomerados', 
                  'bancos', 
                  'cooperativas de crédito' = 'cooperativas', 
                  'sociedades', 
                  'administradoras de consórcios' = 'consorcios'
                ), 
              selected = 
                c(
                  'conglomerados', 
                  'bancos', 
                  'cooperativas', 
                  'sociedades', 
                  'consorcios'
                ),
              width = NULL, 
              choiceNames = NULL,
              choiceValues = NULL
            )
          ),
          box(
            dateRangeInput(
              inputId = 'periodo', 
              label = 'Período',
              start = lubridate::today() - months(1), 
              end = lubridate::today() - months(1), 
              min = '2007-01-01',
              max = lubridate::today(), 
              format = 'mm-yyyy', 
              startview = 'year',
              weekstart = 0, 
              language = 'pt-BR', 
              separator = ' a '
            )
          )
        ),
        tags$hr(),
        
        h2('Relação de agências, postos e filiais de administradoras de consórcio '),
        h5(
          'Fonte: Banco Central do Brasil', 
          tags$a(
            href = 'https://www.bcb.gov.br/estabilidadefinanceira/agenciasconsorcio', 
            icon('link')
          )
        ),
        fluidRow(
          box(
            checkboxGroupInput(
              inputId = 'tipo_bacen1', 
              label = 'Tipos de instituições', 
              choices = 
                list(
                  'agências' = 'agencias', 
                  'postos de atendimento' = 'postos', 
                  'postos de atendimento eletrônico' = 'pae', 
                  'filiais de administradoras de consórcio' = 'filiais_adm_consorcios'
                ), 
              selected = 
                c(
                  'agencias', 
                  'postos', 
                  'pae', 
                  'filiais_adm_consorcios'
                ),
              width = NULL, 
              choiceNames = NULL,
              choiceValues = NULL
            )
          ),
          box(
            dateRangeInput(
              inputId = 'periodo', 
              label = 'Período',
              start = lubridate::today() - months(1), 
              end = lubridate::today() - months(1), 
              min = '2007-01-01',
              max = lubridate::today(), 
              format = 'mm-yyyy', 
              startview = 'year',
              weekstart = 0, 
              language = 'pt-BR', 
              separator = ' a '
            )
          )
        ),
        tags$hr(),
        
        fluidRow(
          column(
            6,
            shinyDirButton(
              id = 'dir_bacen', 
              label = 'Diretório*', 
              title = 'Salvar em...',
              icon = icon('folder-open')
            ),
            verbatimTextOutput('dir_bacen', placeholder = TRUE),
            helpText('*cada tipo de instituição é armazenado em um subdiretório')
          ),
          column(
            2,
            actionButton(
              inputId = 'exec_bacen',
              label = 'Executar',
              icon = icon('download')
            ),
            # submitButton('Submit'))
            offset = 2
          )
        )
      ),
      
      tabItem(
        tabName = 'cadastur',
        h2('Cadastro dos prestadores de serviços turísticos'),
        h5(
          'Fonte: Portal Brasileiro de Dados Abertos', 
          tags$a(href = 'http://dados.turismo.gov.br/cadastur', icon('link'))
        )
      ),
      
      tabItem(
        tabName = 'cnes'
      ),
      
      tabItem(
        tabName = 'anatel'
      ),
      
      tabItem(
        tabName = 'bovespa'
      ),
      
      tabItem(
        tabName = 'portal_transparencia'
      ),
      
      tabItem(
        tabName = 'srf'
      )
      
    )
  )
)

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  
  shinyDirChoose(
    input = input,
    id = 'dir_bacen',
    roots = c(home = file.path(dir_gcad, 'bacen'))
  )
  global <- reactiveValues(datapath = dir_gcad)
  dir_bacen <- reactive(input$dir_bacen)
  output$dir_bacen <- renderText({global$datapath})
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {input$dir_bacen},
    handlerExpr = {
      if (!"path" %in% names(dir_bacen())) 
        return()
      global$datapath <- file.path(dir_gcad, dir_bacen()$path[-1])
    }
  )
  
  
  
  
}

shinyApp(ui, server)