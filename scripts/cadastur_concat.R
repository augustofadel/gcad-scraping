library(tidyverse)

data_dir <- '//WARQPRD14V/cempre/GCAD/REGISTRO_ADMINISTRATIVO/CADASTUR'

# TODO: incluir ano e semestre

# por ano -----------------------------------------------------------------
walk(
  2019:2020,
  function(ano) {
    message('\n\n#########\nano: ', ano)
    files <- 
      list.files(
        file.path(data_dir, ano),
        pattern = '\\.csv'
      )
    concat_ano <- 
      map_dfr(
        files,
        function(arq) {
          message('\narquivo: ', arq)
          tmp <- 
            file.path(data_dir, ano, arq) %>% 
            read_csv2(
              ., 
              col_types = cols(.default = 'c'), 
              locale = locale(encoding = guess_encoding(.)[['encoding']][1])
            ) %>% 
            set_names(
              names(.) %>% 
                str_to_lower %>% 
                iconv(from = 'utf-8', to = 'ASCII//TRANSLIT') %>% 
                str_remove_all('-') %>% 
                str_replace_all(' ', '_')
            )
          
          if (all(!str_detect(names(tmp), 'cnpj')))
            return(NULL)
          
          if (!'cnpj' %in% names(tmp))
            names(tmp)[str_detect(names(tmp), 'cnpj')] <- 'cnpj'
          if (!'telefone' %in% names(tmp))
            names(tmp)[names(tmp) == 'telefone_comercial'] <- 'telefone'
          if (!'site' %in% names(tmp))
            names(tmp)[names(tmp) == 'website'] <- 'site'
          
          tmp <- 
            tmp %>% 
            na_if('-') %>% 
            filter(
              # situacao == 'Em Operação',
              !is.na(cnpj)
            ) %>%
            mutate(
              cod_raiz_cnpj = 
                str_remove_all(cnpj, '[^0-9]') %>% 
                str_sub(1, 8),
              # cnae = 
              #   str_remove_all(cnae, '[^0-9]') %>% 
              #   str_sub(1, 4),
              telefone = str_remove_all(telefone, '[^0-9]'),
              arquivo = arq
            ) %>% 
            select(
              cod_raiz_cnpj,
              # cnae,
              telefone,
              email_institucional,
              email_comercial,
              site
            )
          message('num. linhas: ', nrow(tmp))
          return(tmp)
        }
      )
    concat_ano %>% 
      write_csv(file.path(data_dir, paste0(ano, '_cad_cnpj.csv')))
  }
)
