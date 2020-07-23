library(tidyverse)
library(rvest)

url <- 'http://dados.turismo.gov.br/cadastur'
path <- '//WARQPRD14V/cempre/GCAD/REGISTRO_ADMINISTRATIVO/CADASTUR'

href <- 
  read_html(url) %>% 
  html_nodes(xpath = '//a') %>% 
  html_attr('href')

walk(
  href[str_detect(href, '\\.csv')] %>% 
    str_extract('20[0-9]{2}') %>% 
    unique(),
  ~ if (!dir.exists(file.path(path, .x)))
    dir.create(file.path(path, .x))
)

status_final <- 
  map_dfr(
    href[str_detect(href, '\\.csv')],
    function(arq) {
      arq_local <- 
        file.path(
          path,
          basename(arq) %>% str_extract('20[0-9]{2}'),
          basename(arq)
        )
      message('Verificando ', basename(arq))
      if (!file.exists(arq_local)) {
        cat('\nBaixando', basename(arq), '\n\n')
        res <- 'arquivo baixado'
        tmp <- try(download.file(arq, arq_local))
        if (class(tmp) == 'try-error')
          res <- 'erro'
      } else {
        res <- 'arquivo local encontrado'
        message('Arquivo encontrado em ', arq_local)
      }
      tibble(
        arquivo = basename(arq),
        status = res,
        link = arq,
        local = ifelse(res == 'erro', NA_character_, arq_local)
      )
    }
  )

count(status_final, status)
filter(status_final, status == 'erro')


# arquivos <- 
#   list.files(
#     '//WARQPRD14V/cempre/CRAWLER/CADASTUR',
#     pattern = '\\.csv', 
#     full.names = T, 
#     recursive = T
#   )
# tamanho <- file.size(arquivos)
# falha <- 
#   map_chr(
#     c(
#       3575808,
#       13754368,
#       1118208,
#       3592192,
#       933888,
#       2060288
#     ),
#     ~ arquivos[which.min(abs(tamanho - .x))] %>% basename()
#   )
# 
# walk(
#   falha[2],
#   function(arq) {
#     x <- href[str_detect(href, arq)]
#     try({
#       download.file(
#         x,
#         file.path(
#           '//WARQPRD14V/cempre/CRAWLER/CADASTUR',
#           basename(x) %>% str_extract('20[0-9]{2}'),
#           basename(x)
#         )
#       )
#     })
#   }
# )