# https://www.anatel.gov.br/dados/component/content/article/125-chamadas/280-dados-abertos

library(tidyverse)
library(rvest)
library(lubridate)
library(tools)

out_dir <- '//WARQPRD14V/cempre/GCAD/REGISTRO_ADMINISTRATIVO/ANATEL'

input <- 
  tibble(
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
    date_attr = c(
      'data-datetime',
      'data-datetime',
      'data-datetime',
      'data-datetime'
    )
  )

result <- 
  pmap(
    input,
    function(url, file_url, file_name_prefix, date_css_elem, date_attr) {
      message('\nArquivo: ', file_url)
      date <- 
        read_html(url) %>% 
        html_node(date_css_elem) %>% 
        html_attr(date_attr) %>% 
        as.POSIXct()
      message('Última atualização: ', date)
      files_found <- 
        list.files(path = out_dir, pattern = file_name_prefix, full.names = TRUE)
      last_mtime <- 
        suppressWarnings(file.mtime(files_found) %>% max)
      message(
        'Encontrados ', length(files_found), ' arquivos locais com prefixo ', 
        file_name_prefix, '.'
      )
      message(
        'Data mais recente para arquivo local com prefixo ', file_name_prefix, 
        ': ', last_mtime
      )
      if (last_mtime < date) {
        message('Iniciando download...')
        out_file <- 
          file.path(
            out_dir, 
            paste0(
              file_name_prefix, '_', str_remove_all(ymd(date), '-'), '.', 
              file_ext(file_url)
            )
          )
        dw <- download.file(url = file_url, destfile = out_file, quiet = TRUE)
        if (dw == 0) {
          message('Arquivo salvo em ', out_file)
          return('atualizado com sucesso')
        }
        else {
          message('Erro durante download.')
          return('erro no download')
        }
      }
      else{
        message('Nenhuma atualização disponível.')
        return('nenhuma atualização disponível')
      }
    }
  )

walk2(
  input$file_name_prefix,
  result,
  ~message(.x, ': ', .y)
)