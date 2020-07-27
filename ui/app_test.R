# https://fontawesome.com/icons?d=gallery&q=folder
# https://shiny.rstudio.com/tutorial/written-tutorial/lesson3/
# https://mastering-shiny.org/


# config ------------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(shinyFiles)
# library(shinyalert)
library(lubridate)
library(purrr)
library(stringr)

source('../scripts/bacen.R')

gcad_dir <- '//WARQPRD14V/cempre/GCAD/REGISTRO_ADMINISTRATIVO'


# ui ----------------------------------------------------------------------
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
  
  dashboardBody(
    tabItems(
      
# ui :: bacen -------------------------------------------------------------
      tabItem(
        tabName = 'bacen',
        h1('BACEN'),
        box(
          h3('Relação de instituições em funcionamento no país'),
          h5(
            'Fonte: Banco Central do Brasil', 
            tags$a(
              href = 'https://www.bcb.gov.br/estabilidadefinanceira/relacsaveao_instituicoes_funcionamento', 
              icon('link')
            )
          ),
          tags$br(),
          checkboxGroupInput(
            inputId = 'bacen_tipo1', 
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
          ),
          
          tags$hr(),
          
          h3('Relação de agências, postos e filiais de administradoras de consórcio'),
          h5(
            'Fonte: Banco Central do Brasil', 
            tags$a(
              href = 'https://www.bcb.gov.br/estabilidadefinanceira/agenciasconsorcio', 
              icon('link')
            )
          ),
          tags$br(),
          checkboxGroupInput(
            inputId = 'bacen_tipo2', 
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
            inputId = 'bacen_periodo', 
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
          ),
          checkboxGroupInput(
            inputId = 'bacen_ext', 
            label = 'Arquivos de saída', 
            choices = list('csv', 'txt' = 'tsv', 'xlsx'), 
            selected = 'tsv',
            inline = TRUE
          ),
          # TODO: substituir textInput por shinyDirButton para selecionar diretório
          # shinyDirButton(
          #   id = 'bacen_dir_sel', 
          #   label = 'Salvar em', 
          #   title = 'Salvar em...',
          #   FALSE,
          #   icon = icon('folder-open')
          # ),
          # verbatimTextOutput('bacen_dir', placeholder = TRUE),
          textInput(
            inputId = 'bacen_dir_sel',
            label = 'Diretório',
            value = file.path(gcad_dir, 'BACEN/ORIGINAL'),
            placeholder = TRUE
          ),
          helpText('*cada tipo de instituição é armazenado em um subdiretório'),
          actionButton(
            inputId = 'bacen_exec',
            label = 'Executar',
            icon = icon('download')
          )
        ),
        # TODO: melhorar visualização tabela final para incluir url (linhas separada?)
        box(dataTableOutput('bacen_tab'))
      ),

# ui :: cadastur ----------------------------------------------------------
      tabItem(
        tabName = 'cadastur',
        h2('Cadastro dos prestadores de serviços turísticos'),
        h5(
          'Fonte: Portal Brasileiro de Dados Abertos', 
          tags$a(href = 'http://dados.turismo.gov.br/cadastur', icon('link'))
        )
      ),

# ui :: cnes --------------------------------------------------------------
      tabItem(
        tabName = 'cnes'
      ),

# ui :: anatel ------------------------------------------------------------
      tabItem(
        tabName = 'anatel'
      ),

# ui :: bovespa -----------------------------------------------------------
      tabItem(
        tabName = 'bovespa'
      ),

# ui :: portal da trasnparencia -------------------------------------------
      tabItem(
        tabName = 'portal_transparencia'
      ),

# ui :: srf ---------------------------------------------------------------
      tabItem(
        tabName = 'srf'
      )
      
    )
  )

)


# server ------------------------------------------------------------------
server <- function(input, output) {
  
# server :: bacen ---------------------------------------------------------
  bacen_periodo <-
    reactive({
      seq(input$bacen_periodo[1], input$bacen_periodo[2], by = 'months') %>%
        str_sub(1, 7)
    })
  bacen_tipo <- reactive(c(input$bacen_tipo1, input$bacen_tipo2))
  
  # shinyDirChoose(
  #   input = input,
  #   id = 'bacen_dir_sel',
  #   roots = c(home = file.path(gcad_dir, 'bacen'))
  # )
  # output$bacen_dir <- renderText({gcad_dir})
  # 
  # shinyDirChoose(
  #   input = input,
  #   id = 'bacen_dir_sel',
  #   roots = c(home = file.path(gcad_dir, 'bacen'))
  # )
  # dirs$bacen <- reactive(input$bacen_dir)
  # # bacen_dir <- reactive(input$bacen_dir)
  # output$bacen_dir <- renderText({dirs$bacen})
  # # observeEvent(
  # #   ignoreNULL = TRUE,
  # #   eventExpr = {input$bacen_dir},
  # #   handlerExpr = {
  # #     if (!'path' %in% names(bacen_dir()))
  # #       return()
  # #     global$datapath <- file.path(gcad_dir, bacen_dir()$path[-1])
  # #   }
  # # )
  output$bacen_dir <- renderText({input$bacen_dir_sel})
  
  bacen_result <- eventReactive(input$bacen_exec, {
    expand.grid(
      tipo = bacen_tipo(),
      periodo = bacen_periodo(),
      stringsAsFactors = F
    ) %>%
      tibble::as_tibble() %>%
      purrr::pmap_dfr(
        function(tipo, periodo) {
          bacen(periodo, tipo, input$bacen_dir_sel, input$bacen_ext)
        }
      )
  })
  
  output$bacen_tab <- 
    renderDataTable(
      # TODO: incluir url consultada  na tabela final
      dplyr::select(bacen_result(), -url), 
      options = 
        list(
          lengthMenu = list(c(5, 10), c('5', '10')),
          pageLength = 5,
          searching = FALSE
        )
    )
  
# server :: srf -----------------------------------------------------------



  
  
  
  
}

shinyApp(ui, server)