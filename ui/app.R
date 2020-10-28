# https://fontawesome.com/icons?d=gallery&q=folder
# https://shiny.rstudio.com/tutorial/written-tutorial/lesson3/
# https://mastering-shiny.org/

# TODO: perguntar antes de instalar pacotes

# instala pacotes
packages.list <- 
  c(
    'assertr',
    'dplyr',
    'lubridate',
    'magrittr',
    'openxlsx',
    'purrr',
    'readxl',
    'readr',
    'httr',
    'rvest',
    'shinydashboard',
    'shinyFiles',
    'stringr',
    'tidyr'
  )
new.packages <- 
  packages.list[!(packages.list %in% installed.packages()[,'Package'])]
if (length(new.packages) > 0) {
  install.packages(
    new.packages,
    repos = ' https://cran.rstudio.com'
  )
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
source('../scripts/anatel.R')

'augusto.fadel@ibge.gov.br | Fundacao Instituto Brasileiro de Geografia e Estatistica - IBGE' %>% 
  httr::user_agent() %>% 
  httr::set_config()

# TODO: incluir condicao, se gcad_dir não encontrado, usar ~
gcad_dir <- '//WARQPRD14V/cempre/GCAD/REGISTRO_ADMINISTRATIVO'
srf_url <- 'http://receita.economia.gov.br/orientacao/tributaria/cadastros/cadastro-nacional-de-pessoas-juridicas-cnpj/dados-publicos-cnpj'
cadastur_url <- 'http://dados.turismo.gov.br/cadastur'
cnes_url <- 'ftp://ftp.datasus.gov.br/cnes'
anatel_param <- 
  tibble::tibble(
    url = c(
      'http://www.dados.gov.br/dataset/empresas-autorizadas-scm',
      'http://www.dados.gov.br/dataset/relacao-de-empresas-autorizada-de-servicos-de-comunicacao-movel-pessoal-smp',
      'http://www.dados.gov.br/dataset/empresas-autorizadas-de-servico-telefonico-fixo-comutado',
      'http://www.dados.gov.br/dataset/empresas-autorizadas-seac'
    ),
    file_url = c(
      'http://www.anatel.gov.br/dadosabertos/PDA/Outorga/Empresas_Autorizadas_SCM.csv',
      'http://www.anatel.gov.br/dadosabertos/PDA/Outorga/Empresas_Autorizadas_SMP.csv',
      'http://www.anatel.gov.br/dadosabertos/PDA/Outorga/Empresas_Outorgadas_STFC.csv',
      'http://www.anatel.gov.br/dadosabertos/PDA/Outorga/Empresas_Autorizadas_SEAC.csv'
    ),
    file_name_prefix = c(
      'SCM',
      'SMP',
      'STFC',
      'SEAC'
    ),
    date_css_elem = c(
      'tr:nth-child(4) .automatic-local-datetime',
      'tr:nth-child(4) .automatic-local-datetime',
      'tr:nth-child(2) .automatic-local-datetime',
      'tr:nth-child(4) .automatic-local-datetime'
    ),
    date_attr = 'data-datetime'
  )
ans_param <- 
  tibble::tibble(
    url = c(
      'https://dados.gov.br/dataset/operadoras-de-planos-privados-de-saude',
      'https://dados.gov.br/dataset/operadoras-de-planos-de-saude-com-registro-cancelado',
      'https://dados.gov.br/dataset/operadoras-e-prestadores-nao-hospitalares',
      'https://dados.gov.br/dataset/produtos-e-prestadores-hospitalares'
    ),
    file_url = c(
      'http://ftp.dadosabertos.ans.gov.br/FTP/PDA/operadoras_de_plano_de_saude_ativas/Relatorio_cadop.csv',
      'http://ftp.dadosabertos.ans.gov.br/FTP/PDA/operadoras_de_plano_de_saude_canceladas/Relatorio_cadop_canceladas.csv',
      'http://ftp.dadosabertos.ans.gov.br/FTP/PDA/operadoras_e_prestadores_nao_hospitalares/operadoras_e_prestadores_nao_hospitalares.zip',
      'http://ftp.dadosabertos.ans.gov.br/FTP/PDA/produtos_e_prestadores_hospitalares/produtos_e_prestadores_hospitalares.zip'
    ),
    file_name_prefix = c(
      'ativas',
      'canceladas',
      'hospitalar',
      'nao_hospitalar'
    ),
    date_css_elem = 'tr:nth-child(4) .automatic-local-datetime',
    date_attr = 'data-datetime'
  )
mj_param <- 
  tibble::tibble(
    url = c(
      'https://dados.gov.br/dataset/lista-de-cartorios-do-brasil1',
      'https://dados.gov.br/dataset/cadastro-nacional-de-entidades-sociais',
      'https://dados.gov.br/dataset/comunidades-terapeuticas1'
    ),
    file_url = c(
      'https://portal.mj.gov.br/CartorioInterConsulta/consulta.do?action=baixarCSVTodosCartorios',
      'http://repositorio.dados.gov.br/pessoa-familia-sociedade/sociedade-civil-organizacao-participacao/cadastro-nacional-entidades-sociais.zip',
      'http://repositorio.dados.gov.br/justica-legislacao/justica/BASE_DE_DADOS_COMUNIDADES_TERAPEUTICAS.csv'
    ),
    file_name_prefix = c(
      'cartorios',
      'sociais',
      'terapeuticas'
    ),
    date_css_elem = c(
      'tr:nth-child(4) .automatic-local-datetime',
      'tr:nth-child(2) .automatic-local-datetime',
      'tr:nth-child(4) .automatic-local-datetime'
    ),
    date_attr = 'data-datetime'
  )


# ui ----------------------------------------------------------------------
ui <- dashboardPage(
  
  dashboardHeader(title = 'GCAD scraping'),
  dashboardSidebar(
    sidebarMenu(
      menuItem('ANATEL', tabName = 'anatel', icon = icon('database')),
      menuItem('ANS', tabName = 'ans', icon = icon('database')),
      menuItem('BACEN', tabName = 'bacen', icon = icon('database')),
      # menuItem('B3', tabName = 'b3', icon = icon('database')),
      menuItem('Cadastur', tabName = 'cadastur', icon = icon('database')),
      menuItem('CNES', tabName = 'cnes', icon = icon('database')),
      menuItem('Ministério da Justiça', tabName = 'mj', icon = icon('database')),
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
          h2('Relação de instituições em funcionamento no país'),
          h5(
            'Fonte: Banco Central do Brasil',
            tags$a(
              href = 'https://www.bcb.gov.br/estabilidadefinanceira/relacao_instituicoes_funcionamento',
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

          h2('Relação de agências, postos e filiais de administradoras de consórcio'),
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
          shinyDirButton(
            id = 'bacen_dir_sel',
            label = 'Salvar em',
            title = 'Salvar em...',
            icon = icon('folder-open')
          ),
          verbatimTextOutput('bacen_dir', placeholder = TRUE),
          helpText('*cada tipo de instituição será armazenado no subdiretório correspondente'),
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
          shinyDirButton(
            id = 'cadastur_dir_sel',
            label = 'Salvar em',
            title = 'Salvar em...',
            icon = icon('folder-open')
          ),
          verbatimTextOutput('cadastur_dir', placeholder = TRUE),
          tags$br(),
          actionButton(
            inputId = 'cadastur_exec',
            label = 'Verificar atualização',
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
        tabName = 'cnes',
        h1('CNES'),
        box(
          h2('Cadastro Nacional de Estabelecimentos de Saúde'),
          h5(
            'Fonte: Ministério da Saúde',
            tags$a(
              href = 'http://cnes.datasus.gov.br/',
              icon('link')
            )
          ),
          tags$br(),
          dateRangeInput(
            inputId = 'cnes_periodo',
            label = 'Período',
            start = lubridate::today() - months(1),
            end = lubridate::today() - months(1),
            min = '2017-06-01',
            max = lubridate::today(),
            format = 'mm-yyyy',
            startview = 'year',
            weekstart = 0,
            language = 'pt-BR',
            separator = ' a '
          ),
          tags$br(),
          shinyDirButton(
            id = 'cnes_dir_sel',
            label = 'Salvar em',
            title = 'Salvar em...',
            icon = icon('folder-open')
          ),
          verbatimTextOutput('cnes_dir', placeholder = TRUE),
          tags$br(),
          actionButton(
            inputId = 'cnes_exec',
            label = 'Verificar atualização',
            icon = icon('download')
          )
        ),
        box(
          h5(strong('Histórico de arquivos')),
          dataTableOutput('cnes_tab')
        )
      ),
      
      # ui :: ans ---------------------------------------------------------------
      tabItem(
        tabName = 'ans',
        h1('ANS'),
        box(
          h2('Operadoras de planos de saúde'),
          h5(
            'Fonte: Portal Brasileiro de Dados Abertos', 
            tags$a(href = 'http://ans.gov.br/perfil-do-setor/dados-abertos', icon('link'))
          ),
          tags$br(),
          checkboxGroupInput(
            inputId = 'ans_tipo', 
            label = 'Arquivos', 
            choices = 
              list(
                'Operadoras ativas' = 'ativas', 
                'Operadoras canceladas' = 'canceladas', 
                'Rede de atendimento hospitalar' = 'hospitalar', 
                'Rede de atendimento não hospitalar' = 'nao_hospitalar'
              ), 
            selected = c('ativas', 'canceladas', 'hospitalar', 'nao_hospitalar'),
            width = NULL, 
            choiceNames = NULL,
            choiceValues = NULL
          ),
          
          tags$br(),
          shinyDirButton(
            id = 'ans_dir_sel',
            label = 'Salvar em',
            title = 'Salvar em...',
            icon = icon('folder-open')
          ),
          verbatimTextOutput('ans_dir', placeholder = TRUE),
          tags$br(),
          actionButton(
            inputId = 'ans_exec',
            label = 'Verificar atualização',
            icon = icon('download')
          )
        ),
        box(
          h5(strong('Histórico de arquivos')),
          dataTableOutput('ans_tab')
        )
      ),
      
      # ui :: anatel ------------------------------------------------------------
      tabItem(
        tabName = 'anatel',
        h1('ANATEL'),
        box(
          h2('Empresas outorgadas/autorizadas a prestar serviços'),
          h5(
            'Fonte: Portal Brasileiro de Dados Abertos', 
            tags$a(href = 'https://www.anatel.gov.br/dados/component/content/article/125-chamadas/280-dados-abertos', icon('link'))
          ),
          tags$br(),
          checkboxGroupInput(
            inputId = 'anatel_tipo', 
            label = 'Tipos de serviço', 
            choices = 
              list(
                'Serviço de Comunicação Multimídia' = 'SCM', 
                'Serviço Móvel Pessoal' = 'SMP', 
                'Serviço Telefônico Fixo Comutado' = 'STFC', 
                'Serviço de Acesso Condicionado' = 'SEAC'
              ), 
            selected = c('SCM', 'SMP', 'STFC', 'SEAC'),
            width = NULL, 
            choiceNames = NULL,
            choiceValues = NULL
          ),
          tags$br(),
          shinyDirButton(
            id = 'anatel_dir_sel',
            label = 'Salvar em',
            title = 'Salvar em...',
            icon = icon('folder-open')
          ),
          verbatimTextOutput('anatel_dir', placeholder = TRUE),
          tags$br(),
          actionButton(
            inputId = 'anatel_exec',
            label = 'Verificar atualização',
            icon = icon('download')
          )
        ),
        box(
          h5(strong('Histórico de arquivos')),
          dataTableOutput('anatel_tab')
        )
      ),
      
      # ui :: mj ----------------------------------------------------------------
      tabItem(
        tabName = 'mj',
        h1('Ministério da Justiça'),
        box(
          h2('Bases de dados'),
          h5(
            'Fonte: Portal Brasileiro de Dados Abertos', 
            tags$a(href = 'https://www.justica.gov.br/dados-abertos/dados', icon('link'))
          ),
          tags$br(),
          checkboxGroupInput(
            inputId = 'mj_tipo', 
            label = 'Tipo', 
            choices = 
              list(
                'Cadastro Nacional de Entidades Sociais' = 'sociais',
                'Lista de Cartórios do Brasil' = 'cartorios',
                'Lista de Comunidades Terapêuticas' = 'terapeuticas'
              ), 
            selected = c('sociais', 'cartorios', 'terapeuticas'),
            width = NULL, 
            choiceNames = NULL,
            choiceValues = NULL
          ),
          
          tags$br(),
          shinyDirButton(
            id = 'mj_dir_sel',
            label = 'Salvar em',
            title = 'Salvar em...',
            icon = icon('folder-open')
          ),
          verbatimTextOutput('mj_dir', placeholder = TRUE),
          tags$br(),
          actionButton(
            inputId = 'mj_exec',
            label = 'Verificar atualização',
            icon = icon('download')
          )
        ),
        box(
          h5(strong('Histórico de arquivos')),
          dataTableOutput('mj_tab')
        )
      ),

      # ui :: b3 ----------------------------------------------------------------
      tabItem(
        tabName = 'b3'
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
          # textInput(
          #   inputId = 'srf_dir_sel',
          #   label = 'Diretório',
          #   value = file.path(gcad_dir, 'RECEITA_FEDERAL/ORIGINAL/CNPJ'),
          #   placeholder = TRUE
          # ),
          shinyDirButton(
            id = 'srf_dir_sel',
            label = 'Salvar em',
            title = 'Salvar em...',
            icon = icon('folder-open')
          ),
          verbatimTextOutput('srf_dir', placeholder = TRUE),
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
server <- function(input, output, session) {
  
  session$onSessionEnded(
    function() {
      stopApp()
    }
  )
  
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
  
  shinyDirChoose(
    input = input,
    id = 'bacen_dir_sel',
    session = session,
    roots = c(home = gcad_dir)
  )
  bacen_global <- reactiveValues(datapath = file.path(gcad_dir, 'BACEN/ORIGINAL'))
  bacen_dir <- reactive(input$bacen_dir_sel)
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {input$bacen_dir_sel},
    handlerExpr = {
      if (!'path' %in% names(bacen_dir())) return()
      home <- gcad_dir
      bacen_global$datapath <-
        file.path(
          home,
          paste(unlist(bacen_dir()$path[-1]), collapse = .Platform$file.sep)
        )
    })
  output$bacen_dir <- renderText({normalizePath(bacen_global$datapath)})
  
  
  bacen_tbl <- 
    eventReactive(input$bacen_exec, {
      tidyr::expand_grid(
        tipo = bacen_tipo(),
        periodo = bacen_periodo()
      )
    })

  closeAllConnections()
  output$bacen_tab <-
    renderDataTable({
      # TODO: incluir url consultada na tabela final (?)
      tot <- nrow(bacen_tbl())
      withProgress(
        message = 'BACEN',
        style = 'notification',
        detail = '',
        value = 0, {
          bacen_result <-
            purrr::pmap_dfr(
              bacen_tbl(),
              function(tipo, periodo) {
                incProgress(0, detail = paste('baixando', periodo, tipo))
                tmp <- bacen(periodo, tipo, bacen_global$datapath, input$bacen_ext)
                Sys.sleep(30)
                incProgress(1 / tot, detail = 'concluído')
                return(tmp)
              }
            )
        })
      dplyr::select(bacen_result, -url)
    },
    options =
      list(
        lengthMenu = list(c(4, 8, 12), c('4', '8', '12')),
        pageLength = 4,
        searching = FALSE
      )
    )
  
  # server :: cadastur ------------------------------------------------------
  shinyDirChoose(
    input = input,
    id = 'cadastur_dir_sel',
    session = session,
    roots = c(home = gcad_dir)
  )
  cadastur_global <- reactiveValues(datapath = file.path(gcad_dir, 'CADASTUR'))
  cadastur_dir <- reactive(input$cadastur_dir_sel)
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {input$cadastur_dir_sel},
    handlerExpr = {
      if (!'path' %in% names(cadastur_dir())) return()
      home <- gcad_dir
      cadastur_global$datapath <-
        file.path(
          home,
          paste(unlist(cadastur_dir()$path[-1]), collapse = .Platform$file.sep)
        )
    })
  output$cadastur_dir <- renderText({normalizePath(cadastur_global$datapath)})
  
  href <- eventReactive(input$cadastur_exec, {
    closeAllConnections()
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
      tot <- length(href()[stringr::str_detect(href(), '\\.csv')])
      withProgress(
        message = 'Baixando',
        style = 'notification',
        detail = '',
        value = 0, {
          result <- 
            purrr::map_dfr(
              href()[stringr::str_detect(href(), '\\.csv')],
              function(arq) {
                incProgress(0, detail = basename(arq))
                arq_local <-
                  file.path(
                    cadastur_global$datapath,
                    basename(arq) %>% stringr::str_extract('20[0-9]{2}'),
                    basename(arq)
                  )
                message('Verificando ', basename(arq))
                if (!file.exists(arq_local)) {
                  cat('\nBaixando', basename(arq), '\n\n')
                  res <- 'baixado'
                  tmp <- try(download.file(arq, arq_local, quiet = T))
                  if (class(tmp) == 'try-error')
                    res <- 'erro'
                } else {
                  res <- 'local'
                  message('Arquivo encontrado em ', arq_local)
                }
                incProgress(1 / tot, detail = basename(arq))
                tibble::tibble(
                  ano = basename(arq) %>% stringr::str_extract('20[0-9]{2}'),
                  url = arq,
                  `arquivo origem` = basename(arq),
                  `arquivo local` = 
                    ifelse(res == 'erro', NA_character_, basename(arq_local)),
                  status = res
                )
              }
            )
        })
      closeAllConnections()
      dplyr::mutate(
        result,
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
  

  # server :: cnes ----------------------------------------------------------
  cnes_periodo <-
    eventReactive(input$cnes_exec, {
      seq(input$cnes_periodo[1], input$cnes_periodo[2], by = 'months') %>%
        str_sub(1, 7)
    })

  shinyDirChoose(
    input = input,
    id = 'cnes_dir_sel',
    session = session,
    roots = c(home = gcad_dir)
  )
  cnes_global <- reactiveValues(datapath = file.path(gcad_dir, 'CNES'))
  cnes_dir <- reactive(input$cnes_dir_sel)
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {input$cnes_dir_sel},
    handlerExpr = {
      if (!'path' %in% names(cnes_dir())) return()
      home <- gcad_dir
      cnes_global$datapath <-
        file.path(
          home,
          paste(unlist(cnes_dir()$path[-1]), collapse = .Platform$file.sep)
        )
    })
  output$cnes_dir <- renderText({normalizePath(cnes_global$datapath)})

  closeAllConnections()
  output$cnes_tab <-
    renderDataTable({
      tot <- nrow(cnes_periodo())
      withProgress(
        message = 'CNES',
        style = 'notification',
        detail = '',
        value = 0, {
          cnes_result <-
            purrr::map_dfr(
              cnes_periodo(),
              function(periodo) {
                message(periodo)
                status <- 'erro'
                incProgress(0, detail = paste('verificando', periodo))
                file_url <-
                  paste0(
                    cnes_url, '/BASE_DE_DADOS_CNES_',
                    str_remove(periodo, '[^0-9]'), '.ZIP'
                  )
                file_path <-
                  file.path(
                    cnes_global$datapath,
                    basename(file_url)
                  )
                if (!file.exists(file_path)) {
                  message('baixando ', file_url, ' para ', file_path)
                  incProgress(0, detail = paste('baixando', periodo))
                  dw <-
                    try(
                      download.file(file_url, destfile = file_path, quiet = T)
                    )
                  status <-
                    ifelse(
                      class(dw) == 'try-error',
                      'erro download',
                      'baixado'
                    )
                  if (length(cnes_periodo()) > 1) {
                    message('aguardando...')
                    Sys.sleep(30)
                  }
                } else {
                  message('arquivo encontrado')
                  status <- 'arquivo ja existe'
                }
                incProgress(1 / tot, detail = 'concluído')
                tibble::tibble(
                  periodo = periodo,
                  arquivo = basename(file_url),
                  status = status
                )
              }
            )
        })
      closeAllConnections()
      cnes_result
    },
    options =
      list(
        lengthMenu = list(c(4, 8, 12), c('4', '8', '12')),
        pageLength = 4,
        searching = FALSE
      )
    )
  
  # server :: ans -----------------------------------------------------------
  shinyDirChoose(
    input = input,
    id = 'ans_dir_sel',
    session = session,
    roots = c(home = gcad_dir)
  )
  ans_global <- reactiveValues(datapath = file.path(gcad_dir, 'ANS'))
  ans_dir <- reactive(input$ans_dir_sel)
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {input$ans_dir_sel},
    handlerExpr = {
      if (!'path' %in% names(ans_dir())) return()
      home <- gcad_dir
      ans_global$datapath <-
        file.path(
          home,
          paste(unlist(ans_dir()$path[-1]), collapse = .Platform$file.sep)
        )
    })
  output$ans_dir <- renderText({normalizePath(ans_global$datapath)})
  
  ans_tbl <- 
    eventReactive(input$ans_exec, {
      dplyr::filter(ans_param, file_name_prefix %in% input$ans_tipo)
      
    })
  
  closeAllConnections()
  output$ans_tab <-
    renderDataTable({
      tot <- nrow(ans_tbl())
      withProgress(
        message = 'ANS',
        style = 'notification',
        detail = '',
        value = 0, {
          ans_result <-
            purrr::pmap_dfr(
              ans_tbl(),
              function(url, file_url, file_name_prefix, date_css_elem, date_attr) {
                incProgress(0, detail = paste('verificando', file_name_prefix))
                tmp <- 
                  anatel(
                    url, 
                    file_url, 
                    file_name_prefix, 
                    date_css_elem, 
                    date_attr, 
                    ans_global$datapath
                  )
                Sys.sleep(10)
                incProgress(1 / tot, detail = 'concluído')
                return(tmp)
              }
            )
        })
      ans_result %>% 
        dplyr::select(-arquivo) %>% 
        dplyr::rename(
          `data local` = data_local,
          `data online` = data_online
        )
    },
    options =
      list(
        lengthMenu = list(c(4, 8, 12), c('4', '8', '12')),
        pageLength = 4,
        searching = FALSE
      )
    )
  
  # server :: anatel --------------------------------------------------------
  shinyDirChoose(
    input = input,
    id = 'anatel_dir_sel',
    session = session,
    roots = c(home = gcad_dir)
  )
  anatel_global <- reactiveValues(datapath = file.path(gcad_dir, 'ANATEL'))
  anatel_dir <- reactive(input$anatel_dir_sel)
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {input$anatel_dir_sel},
    handlerExpr = {
      if (!'path' %in% names(anatel_dir())) return()
      home <- gcad_dir
      anatel_global$datapath <-
        file.path(
          home,
          paste(unlist(anatel_dir()$path[-1]), collapse = .Platform$file.sep)
        )
    })
  output$anatel_dir <- renderText({normalizePath(anatel_global$datapath)})
  
  anatel_tbl <- 
    eventReactive(input$anatel_exec, {
      dplyr::filter(anatel_param, file_name_prefix %in% input$anatel_tipo)
      
    })
  
  closeAllConnections()
  output$anatel_tab <-
    renderDataTable({
      tot <- nrow(anatel_tbl())
      withProgress(
        message = 'ANATEL',
        style = 'notification',
        detail = '',
        value = 0, {
          anatel_result <-
            purrr::pmap_dfr(
              anatel_tbl(),
              function(url, file_url, file_name_prefix, date_css_elem, date_attr) {
                incProgress(0, detail = paste('verificando', file_name_prefix))
                tmp <- 
                  anatel(
                    url, 
                    file_url, 
                    file_name_prefix, 
                    date_css_elem, 
                    date_attr, 
                    anatel_global$datapath
                  )
                Sys.sleep(10)
                incProgress(1 / tot, detail = 'concluído')
                return(tmp)
              }
            )
        })
      anatel_result %>% 
        dplyr::select(-arquivo) %>% 
        dplyr::rename(
          `data local` = data_local,
          `data online` = data_online
        )
    },
    options =
      list(
        lengthMenu = list(c(4, 8, 12), c('4', '8', '12')),
        pageLength = 4,
        searching = FALSE
      )
    )
    
  # server :: mj ------------------------------------------------------------
  shinyDirChoose(
    input = input,
    id = 'mj_dir_sel',
    session = session,
    roots = c(home = gcad_dir)
  )
  mj_global <- reactiveValues(datapath = file.path(gcad_dir, 'MJ'))
  mj_dir <- reactive(input$mj_dir_sel)
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {input$mj_dir_sel},
    handlerExpr = {
      if (!'path' %in% names(mj_dir())) return()
      home <- gcad_dir
      mj_global$datapath <-
        file.path(
          home,
          paste(unlist(mj_dir()$path[-1]), collapse = .Platform$file.sep)
        )
    })
  output$mj_dir <- renderText({normalizePath(mj_global$datapath)})
  
  mj_tbl <- 
    eventReactive(input$mj_exec, {
      dplyr::filter(mj_param, file_name_prefix %in% input$mj_tipo)
      
    })
  
  closeAllConnections()
  output$mj_tab <-
    renderDataTable({
      tot <- nrow(mj_tbl())
      withProgress(
        message = 'MJ',
        style = 'notification',
        detail = '',
        value = 0, {
          mj_result <-
            purrr::pmap_dfr(
              mj_tbl(),
              function(url, file_url, file_name_prefix, date_css_elem, date_attr) {
                incProgress(0, detail = paste('verificando', file_name_prefix))
                tmp <- 
                  anatel(
                    url, 
                    file_url, 
                    file_name_prefix, 
                    date_css_elem, 
                    date_attr, 
                    mj_global$datapath
                  )
                Sys.sleep(10)
                incProgress(1 / tot, detail = 'concluído')
                return(tmp)
              }
            )
        })
      mj_result %>% 
        dplyr::select(-arquivo) %>% 
        dplyr::rename(
          `data local` = data_local,
          `data online` = data_online
        )
    },
    options =
      list(
        lengthMenu = list(c(4, 8, 12), c('4', '8', '12')),
        pageLength = 4,
        searching = FALSE
      )
    )
  
  # server :: srf -----------------------------------------------------------
  shinyDirChoose(
    input = input,
    id = 'srf_dir_sel',
    session = session,
    roots = c(home = gcad_dir)
  )
  srf_global <- 
    reactiveValues(
      datapath = file.path(gcad_dir, 'RECEITA_FEDERAL/ORIGINAL/CNPJ')
    )
  srf_dir <- reactive(input$srf_dir_sel)
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {input$srf_dir_sel},
    handlerExpr = {
      if (!'path' %in% names(srf_dir())) return()
      home <- gcad_dir
      srf_global$datapath <-
        file.path(
          home,
          paste(unlist(srf_dir()$path[-1]), collapse = .Platform$file.sep)
        )
    })
  output$srf_dir <- renderText({normalizePath(srf_global$datapath)})
  
  srf_arqs_local <- 
    eventReactive(input$srf_atu, {
      list.files(
        path = srf_global$datapath,
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