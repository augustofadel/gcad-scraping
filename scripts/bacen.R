# Relação de Instituições em Funcionamento no País - BACEN
# http://www.bcb.gov.br/fis/info/instituicoes.asp
# https://www.bcb.gov.br/estabilidadefinanceira/relacao_instituicoes_funcionamento 


# instala pacotes
# packages.list <- c('stringr')
# new.packages <- packages.list[!(packages.list %in% installed.packages()[,'Package'])]
# if(length(new.packages)) {
#   install.packages(new.packages)
# }


# ler arquivo sem cabeçalho e linhas insconsistentes ----------------------
ler_tabela <- 
  function(fn_in, id_var_str = 'CNPJ', sht = 1, verbose = FALSE) {
    require(tools)
    require(readxl)
    require(assertr)
    require(stringr)
    require(magrittr)
    
    if (!tools::file_ext(fn_in) %in% c('xls', 'xlsx'))
      stop('Suporta apenas arquivos excel.')
    
    # identifica primeira linhas do arquivo que contém a string 'id_var_str'
    start_row <- 
      suppressMessages(
        readxl::read_excel(fn_in, sheet = sht, col_names = F, col_types = 'text')
      ) %>% 
      assertr::col_concat() %>% 
      stringr::str_detect(id_var_str) %>% 
      which %>% 
      min %>% 
      max(1)
    # carrega arquivo xls/xlsx desconsiderando as linhas de cabeçalho identificadas em 'start_row'
    tmp <- 
      suppressMessages(
        readxl::read_excel(
          fn_in,
          sheet = sht,
          skip = start_row - 1,
          col_types = 'text'
        )
      )  
    # identifica linhas inconsistente: com caracteres alfa na coluna id_var_str
    sel_id_var <- 
      tmp[, stringr::str_detect(names(tmp), id_var_str)] %>% 
      apply(2, function(x) str_detect(x, '[a-zA-Z]') | is.na(x)) %>% 
      apply(1, any)
    # identifica linhas inconsistentes: campos deslocados
    sel_shift <- 
      tmp[, str_detect(names(tmp), '^\\...')] %>% 
      apply(1, function(x) any(!is.na(x)))
    # informa linhas excluídas
    if (verbose)
      message(
        'Excluidas ', start_row - 1, ' linha(s) de cabeçalho não vazias e ', 
        sum(sel_id_var, sel_shift), ' linha(s) inconsistentes.'
      )
    # warning linhas com campos deslocados
    if (sum(sel_shift > 0))
      warning(
        'Verificar posição dos campos na(s) linha(s) ', 
        paste(which(sel_shift), collapse = (', ')),
        ' do arquivo ', basename(fn_in), ' original.'
      )
    # padroniza nomes de variáveis
    # names(tmp) <- 
    #   names(tmp) %>% 
    #   iconv(from = 'utf-8', to = 'ASCII//TRANSLIT') %>% 
    #   str_replace_all('[[:space:]]|[[:punct:]]', '_')
    # retorna df limpo
    tmp[!sel_id_var & !sel_shift, !str_detect(names(tmp), '^\\...')]
  }


# download
bacen <- 
  function(
    ano = NULL, #str_sub(Sys.Date(), 1, 4), 
    mes = NULL, #str_sub(Sys.Date(), 6, 7),
    tipo = 
      c(
        'conglomerados', 
        'bancos', 
        'cooperativas', 
        'sociedades', 
        'consorcios',
        'agencias', 
        'postos', 
        'pae', 
        'filiais_adm_consorcios'
      ), 
    destino = '//WARQPRD14V/cempre/GCAD/REGISTRO_ADMINISTRATIVO/BACEN/ORIGINAL',
    saida = 'csv' #c('xlsx', 'csv', 'tsv') 
  ) {
    
    require(stringr)
    require(openxlsx)
    require(purrr)
    
    ulr <- 'http://www.bcb.gov.br/fis/info/cad'
    
    for (i in tipo) {
      if (is.null(mes) & is.null(ano)) {
        data <-
          file.path(destino, toupper(tipo)) %>%
          list.files(pattern = '.xls') %>%
          str_sub(1, 6) %>%
          sort(decreasing = T)
        
        ano <- as.character(str_sub(data[1], 1, 4):str_sub(Sys.Date(), 1, 4))
        mes <- 
          str_sub(data[1], 5, 6):str_sub(Sys.Date(), 6, 7) %>%
          str_pad(width = 2, side = 'left', pad = '0')
      }
      
      for(j in ano) {
        for(l in mes) {
          if (tolower(i) == 'consorcios')
            arquivo <- paste0(j, l, 'ADMCONSORCIO', '.zip')
          else if (tolower(i) == 'filiais_adm_consorcios')
            arquivo <- paste0(j, l, 'FILIAISCONS', '.zip')
          else
            arquivo <- paste0(j, l, toupper(i), '.zip')
          
          if (is.null(destino))
            destino <- getwd()
          
          message('\n', l, '/', j, ' :: ', i)
          arq <- try(
            download.file(
              url = paste(ulr, tolower(i), arquivo, sep = '/'),
              destfile = file.path(destino, arquivo),
              quiet = T
            ),
            silent = T
          )
          if (class(arq) == 'try-error') {
            message('Arquivo não foi encontrado.')
          } else {
            arquivos_lst <- unzip(file.path(destino, arquivo), list = TRUE)$Name
            arquivo_xl <- arquivos_lst[str_detect(arquivos_lst, '\\.xls$|\\.xlsx$')]
            unzip(
              file.path(destino, arquivo), 
              files = arquivo_xl, 
              exdir = file.path(destino, i)
            )
            arquivo_xl <- file.path(destino, i, arquivo_xl)
            if (length(arquivo_xl) > 0) {
              purrr::walk(
                arquivo_xl,
                function(arq_i) {
                  sht <- 
                    ifelse(
                      'Plan1' %in% readxl::excel_sheets(arq_i),
                      'Plan1',
                      1
                    )
                  tab <- ler_tabela(arq_i, sht = sht, verbose = TRUE)
                  if ('xlsx' %in% saida)
                    openxlsx::write.xlsx(
                      tab, 
                      str_replace(arq_i, '\\.xls$', '\\.xlsx')
                    )
                  if ('tsv' %in% saida)
                    readr::write_tsv(
                      tab, 
                      str_replace(arq_i, '\\.xls$|\\.xlsx$', '\\.txt'), 
                      na = '',
                      col_names = TRUE
                    )
                  if ('csv' %in% saida)
                    readr::write_csv(
                      tab, 
                      str_replace(arq_i, '\\.xls$|\\.xlsx$', '\\.csv'), 
                      na = '',
                      col_names = TRUE
                    )
                }
              )
              message('Arquivo salvo com sucesso.')
            } else {
              message('Nenhum arquivo salvo.')
            }
            file.remove(file.path(destino, arquivo))
          }
        }
      }
    }
  }




# atualizacao
# desde o ultimo arquivo armazenado ate mes e ano correntes, todos os tipo
bacen()

# especifica ano e mes, todos os tipos
bacen('2017', '04')

# especifica ano, mes e tipo
bacen('2017', '03', 'conglomerados')

# especifica tipo, ano e mes corrente
bacen(tipo = 'consorcios')


library(lubridate)
ano <- 2007:2020
purrr::walk2(
  rep(str_pad(1:12, 2, 'left', '0'), length(ano)), 
  rep(ano, each = 12),
  function(mes, ano) {
    if (as_date(paste(ano, mes, 1, sep = '-')) < (today() - months(1))) {
      bacen(
        ano = ano, 
        mes = mes, 
        # tipo = c('postos', 'pae'), 
        saida = 'tsv'
      )
      Sys.sleep(runif(1, 5, 10))
    }
  }
)


bacen('2009', '08', 'cooperativas', saida = 'tsv')


# eliminar cabeçalho ------------------------------------------------------

library(tidyverse)
library(openxlsx)
destino <- '//WARQPRD14V/cempre/GCAD/REGISTRO_ADMINISTRATIVO/BACEN/ORIGINAL/sociedades'
arquivos <- list.files(destino, '\\.xlsx$|\\.xls$', recursive = T, full.names = T)
tot <- length(arquivos)
overw <- T
# gen_csv <- 
#   function(fn, new_fn, string = 'CNPJ', d = 1) {
#     tmp <- suppressMessages(read_excel(fn))
#     fst <- col_concat(tmp) %>% str_detect(string) %>% which %>% min %>% max(1)
#     if (min(which(!is.na(tmp[, 2]))) != fst)
#       warning(
#         basename(fn),
#         ':\nVERIFICAR STRING USADA PARA IDENTIFICAR PRIMEIRA LINHA'
#       )
#     read_excel(fn, skip = fst - d) %>% 
#       # write_csv(new_fn)
#       write.xlsx(new_fn)
#     message(basename(new_fn))
#   }

walk(
  1:length(arquivos),
  function(i) {
    message(
      '[', i, '/', tot, ' (', round(i / tot * 100, 1), '%)] ',
      basename(arquivos[i])
    )
    # new_fn <- str_replace(arquivos[i], '\\.xlsx$|\\.xls$', '.csv')
    new_fn <- arquivos[i]
    if (!file.exists(new_fn) | (file.exists(new_fn) & overw)) 
      ler_tabela(arquivos[i]) %>% 
      openxlsx::write.xlsx(new_fn)
  }
)
