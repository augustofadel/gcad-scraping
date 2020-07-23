library(tidyverse)
library(RSelenium)
library(lubridate)

url_base <- 'http://portais.niteroi.rj.gov.br/portal-da-transparencia/folha_pagamentos/pesquisar?'

params <- 
  expand.grid(
    instituicao = 
      c(
        '24', #COMPANHIA MUNIC LIMP URBANA DE NITEROI
        '15', #FUNDACAO DE ARTE DE NITEROI - FAN
        '25', #FUNDAÇÃO MUNICIPAL DE EDUCAÇÃO - FME
        '11', #FUNDACAO MUNICIPAL DE SAUDE - FMS
        '8', #FUNDO MUNICIPAL PARA ASSISTENCIA SOCIAL - FMAS
        '9', #FUNDO NITEROI PREV - FINANCEIRO
        '18', #FUNDO NITEROI PREV - PREVIDENCIARIO
        '26', #MUNICIPIO DE NITEROI
        '19', #NITEROI EMPRESA DE LAZER E TURISMO NELTUR
        '6', #NITEROI PREV - NITPREV
        '14' #NITEROI TRANSPORTE E TRANSITO S/A - NITTRANS
      ),
    mes = 1:12,
    ano = 2020:2010,
    stringsAsFactors = F
  ) %>% 
  as_tibble %>% 
  mutate(
    query = 
      str_c(
        'instituicao=', instituicao, 
        '&ano=', ano, 
        '&mes=', mes, 
        '&demitidos=0&cargo=&lotacao=&vinculo=&matricula=&nome='
      ),
    ref = 
      as_date(str_c(ano, mes, 1, sep = '-'))
  ) %>% 
  filter(ref < (today() - months(1)))

arq_orig <- '../Downloads/Download.csv'
fpath <- 'portal_transparencia_niteroi'
server <- 
  remoteDriver(
    remoteServerAddr = 'localhost',
    port = 4444L,
    browserName = 'chrome'
  )
server$open(silent = TRUE)
server$getStatus()

result <- 
  map_dfr(
    1:nrow(params),
    function(i) {
      arq <- 
        paste0(
          params$ano[i], 
          str_pad(params$mes[i], 2, 'left', '0'), 
          params$instituicao[i], 
          '.csv'
        )
      message('[', i, '/', nrow(params), '] ', arq)
      if (file.exists(file.path(fpath, arq))) {
        message('já existe')
        return(tibble(id = i, url = url, arq = arq))
      }
      url <- paste0(url_base, params$query[i])
      server$navigate(url)
      Sys.sleep(2)
      csv <- server$findElement(using = 'css selector', '.csv')
      csv$clickElement()
      t <- 0
      while(t < 5 & !file.exists(arq_orig))
        Sys.sleep(1)
      if (file.exists(arq_orig)) {
        file.copy(arq_orig, file.path(fpath, arq))
        file.remove(arq_orig)
        message('baixado')
        Sys.sleep(2)
        tibble(id = i, url = url, arq = arq)
      } else {
        message('ERRO')
        tibble(id = i, url = url, arq = NA_character_)
      }
    }
  )

server$close()

arqs <- list.files(fpath, '\\.csv', full.names = T)
dat <- 
  map_dfr(
    arqs,
    function(arq) {
      message('\n', basename(arq))
      tmp <- 
        read_csv2(
          arq, 
          locale = locale(encoding = 'ISO-8859-1'), 
          col_types = cols(.default = 'c')
        )
      tmp[, !str_detect(names(tmp), '^X[0-9]{1,}')] %>%
        mutate(
          Ano = basename(arq) %>% str_sub(1, 4),
          Mes = basename(arq) %>% str_sub(5, 6),
          Instituicao = basename(arq) %>% str_sub(7) %>% str_remove('\\..*')
        )
    }
  ) %>% 
  mutate(
    `Nome instituicao` = 
      case_when(
        Instituicao == '24' ~ 'COMPANHIA MUNIC LIMP URBANA DE NITEROI',
        Instituicao == '15' ~ 'FUNDACAO DE ARTE DE NITEROI - FAN',
        Instituicao == '25' ~ 'FUNDAÇÃO MUNICIPAL DE EDUCAÇÃO - FME',
        Instituicao == '11' ~ 'FUNDACAO MUNICIPAL DE SAUDE - FMS',
        Instituicao == '8' ~ 'FUNDO MUNICIPAL PARA ASSISTENCIA SOCIAL - FMAS',
        Instituicao == '9' ~ 'FUNDO NITEROI PREV - FINANCEIRO',
        Instituicao == '18' ~ 'FUNDO NITEROI PREV - PREVIDENCIARIO',
        Instituicao == '26' ~ 'MUNICIPIO DE NITEROI',
        Instituicao == '19' ~ 'NITEROI EMPRESA DE LAZER E TURISMO NELTUR',
        Instituicao == '6' ~ 'NITEROI PREV - NITPREV',
        Instituicao == '14' ~ 'NITEROI TRANSPORTE E TRANSITO S/A - NITTRANS'
      )
  ) %>% 
  arrange(`Nome instituicao`, Ano, Mes, Nome)

dat %>% distinct(Ano, Mes, Instituicao) %>% count(Ano, Mes) %>% print(n = 1000)

saveRDS(dat, file.path(fpath, 'final.rds'))
dat %>% 
  filter(
    as.Date(str_c(Ano, Mes, '01', sep = '-')) < as.Date('2020-07-01'),
    as.Date(str_c(Ano, Mes, '01', sep = '-')) > as.Date('2015-01-01')
  ) %>% 
  write_csv(file.path(fpath, 'final.csv'))
