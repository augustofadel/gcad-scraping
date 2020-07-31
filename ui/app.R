# https://fontawesome.com/icons?d=gallery&q=folder
# https://shiny.rstudio.com/tutorial/written-tutorial/lesson3/
# https://mastering-shiny.org/

# instala pacotes
packages.list <- 
  c(
    'shiny',
    'shinydashboard',
    'shinydashboard',
    'lubridate',
    'purrr',
    'stringr',
    'rvest',
    'tools',
    'readxl',
    'assertr',
    'magrittr',
    'openxlsx'
  )
new.packages <- packages.list[!(packages.list %in% installed.packages()[,'Package'])]
if(length(new.packages)) {
  install.packages(new.packages)
}

# config ------------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(shinyFiles)
# library(shinyalert)
library(lubridate)
library(purrr)
library(stringr)
library(rvest)

source('../scripts/bacen.R')

gcad_dir <- '//WARQPRD14V/cempre/GCAD/REGISTRO_ADMINISTRATIVO'
srf_url <- 'http://receita.economia.gov.br/orientacao/tributaria/cadastros/cadastro-nacional-de-pessoas-juridicas-cnpj/dados-publicos-cnpj'
cadastur_url <- 'http://dados.turismo.gov.br/cadastur'


# ui ----------------------------------------------------------------------
ui <- dashboardPage(
  
  dashboardHeader(title = 'GCAD scraping'),
  dashboardSidebar(
    sidebarMenu(
      # menuItem('ANATEL', tabName = 'anatel', icon = icon('database')),
      menuItem('BACEN', tabName = 'bacen', icon = icon('database')),
      # menuItem('Bovespa', tabName = 'bovespa', icon = icon('database')),
      menuItem('Cadastur', tabName = 'cadastur', icon = icon('database')),
      # menuItem('CNES', tabName = 'cnes', icon = icon('database')),
      # menuItem('Portal da Trasparência', tabName = 'portal_transparencia', icon = icon('database')),
      menuItem('Receita Federal', tabName = 'srf', icon = icon('check'))
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
          # TODO: substituir textInput por shinyDirButton para selecionar diretório bacen
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
            label = 'Executar download',
            icon = icon('download')
          )
        ),
        # TODO: melhorar visualização tabela final para incluir url (linhas separada?)
        box(
          h5(strong('Arquivos baixados')),
          dataTableOutput('bacen_tab')
        )
      ),

# ui :: cadastur ----------------------------------------------------------
      tabItem(
        tabName = 'cadastur',
        h1('Cadastur'),
        box(
          h2('Cadastro dos prestadores de serviços turísticos'),
          h5(
            'Fonte: Portal Brasileiro de Dados Abertos', 
            tags$a(href = 'http://dados.turismo.gov.br/cadastur', icon('link'))
          ),
          tags$br(),
          # TODO: substituir textInput por shinyDirButton para selecionar diretório bacen
          textInput(
            inputId = 'cadastur_dir_sel',
            label = 'Diretório',
            value = file.path(gcad_dir, 'CADASTUR'),
            placeholder = TRUE
          ),
          actionButton(
            inputId = 'cadastur_exec',
            label = 'Executar download',
            icon = icon('download')
          )
        ),
        box(
          h5(strong('Histórico de arquivos')),
          tableOutput('cadastur_summary')
          # TODO: avaliar melhor saída resultados cadastur ui
          # dataTableOutput('cadastur_tab')
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
        tabName = 'srf',
        h1('Receita Federal'),
        box(
          h3('Dados públicos CNPJ'),
          h5(
            'Fonte: Receita Federal', 
            tags$a(
              href = srf_url, 
              icon('link')
            )
          ),
          tags$br(),
          # TODO: substituir textInput por shinyDirButton para selecionar diretório srf
          textInput(
            inputId = 'srf_dir_sel',
            label = 'Diretório',
            value = file.path(gcad_dir, 'RECEITA_FEDERAL/ORIGINAL/CNPJ'),
            placeholder = TRUE
          ),
          h5(strong('Data do arquivo local mais recente')),
          textOutput('srf_ultima_data_local'),
          h5(strong('Data da última atualização online')),
          textOutput('srf_ultima_data'),
          tags$br(),
          actionButton(
            inputId = 'srf_atu',
            label = 'Atualizar datas',
            icon = icon('sync-alt')
          )
          # TODO: implementar download
          # actionButton(
          #   inputId = 'srf_atu',
          #   label = 'Executar download',
          #   icon = icon('download')
          # )
        )
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
  # output$bacen_dir <- renderText({input$bacen_dir_sel})
  
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

# server :: cadastur ------------------------------------------------------
  href <- eventReactive(input$cadastur_exec, {
    xml2::read_html(cadastur_url) %>% 
      rvest::html_nodes(xpath = '//a') %>% 
      rvest::html_attr('href')
  })
  reactive({
    purrr::walk(
      href()[str_detect(href(), '\\.csv$')] %>% 
        str_extract('20[0-9]{2}') %>% 
        unique(),
      ~dir.create(file.path(path, .x))
    )
  })
  # TODO: criar funções independentes para i) verificar arquivos disponíveis e ii) baixar
  cadastur_result <- 
    reactive({
      purrr::map_dfr(
        href()[stringr::str_detect(href(), '\\.csv')],
        function(arq) {
          arq_local <-
            file.path(
              input$cadastur_dir_sel,
              basename(arq) %>% stringr::str_extract('20[0-9]{2}'),
              basename(arq)
            )
          message('Verificando ', basename(arq))
          if (!file.exists(arq_local)) {
            cat('\nBaixando', basename(arq), '\n\n')
            res <- 'baixado'
            tmp <- try(download.file(arq, arq_local))
            if (class(tmp) == 'try-error')
              res <- 'erro'
          } else {
            res <- 'local'
            message('Arquivo encontrado em ', arq_local)
          }
          tibble::tibble(
            ano = basename(arq) %>% stringr::str_extract('20[0-9]{2}'),
            url = arq,
            `arquivo origem` = basename(arq),
            `arquivo local` = ifelse(res == 'erro', NA_character_, basename(arq_local)),
            status = res
          )
        }
      ) %>% dplyr::mutate(
        status = 
          factor(
            status, 
            levels = c('local', 'baixado', 'erro'),
            ordered = TRUE
          )
      )
    })
  # TODO: avaliar usar renderDataTable
  output$cadastur_summary <- 
    renderTable(
      cadastur_result() %>% 
        dplyr::count(ano, status, .drop = FALSE) %>% 
        dplyr::mutate(n = round(n) %>% as.character()) %>% 
        tidyr::spread(status, n, fill = '0') %>% 
        dplyr::arrange(dplyr::desc(ano))
    )
  # TODO: avaliar melhor saída resultados cadastur server
  # output$cadastur_tab <- 
  #   renderDataTable(
  #     cadastur_result() %>% 
  #       dplyr::select(-url, -`arquivo origem`) %>% 
  #       dplyr::arrange(dplyr::desc(ano)), 
  #     options = 
  #       list(
  #         lengthMenu = list(c(5, 10), c('5', '10')),
  #         pageLength = 5,
  #         searching = FALSE
  #       )
  #   )
  
# server :: srf -----------------------------------------------------------
  srf_arqs_local <- 
    eventReactive(input$srf_atu, {
      list.files(
        path = input$srf_dir_sel,
        pattern = '\\.zip',
        recursive = TRUE,
        full.names = TRUE
      )
    })
  output$srf_ultima_data_local <- 
    renderText({
      srf_arqs_local()[str_detect(srf_arqs_local(), '[0-9]{4}/ORIGINAL/.*\\.zip$')] %>%
      file.mtime() %>%
      max() %>%
      format('%d/%m/%Y')
    })
  pg <- eventReactive(input$srf_atu, {xml2::read_html(srf_url)})
  content <- 
    reactive({
      pg() %>% 
        rvest::html_node('#portal-column-content') %>% 
        rvest::html_text() %>% 
        stringr::str_split('\n') %>% 
        unlist() %>% 
        stringr::str_trim()
    })
  output$srf_ultima_data <- 
    renderText({
      content()[stringr::str_detect(content(), 'Data de geração.*[0-9]{2}/[0-9]{2}/[0-9]{4}$')] %>% 
        stringr::str_extract('[0-9]{2}/[0-9]{2}/[0-9]{4}$')
    })
  
}

shinyApp(ui, server)