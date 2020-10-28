# Relação de Instituições em Funcionamento no País - BACEN
# http://www.bcb.gov.br/fis/info/instituicoes.asp
# https://www.bcb.gov.br/estabilidadefinanceira/relacao_instituicoes_funcionamento 


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
        'Excluidas ', start_row - 1, ' linha(s) de cabecalho nao vazias e ', 
        sum(sel_id_var, sel_shift), ' linha(s) inconsistentes.'
      )
    # warning linhas com campos deslocados
    if (sum(sel_shift > 0))
      warning(
        'Verificar posicao dos campos na(s) linha(s) ', 
        paste(which(sel_shift), collapse = (', ')),
        ' do arquivo ', basename(fn_in), ' original.'
      )
    tmp[!sel_id_var & !sel_shift, !str_detect(names(tmp), '^\\...')]
  }


# download
# TODO: user agent
bacen <- 
  function(ano_mes, tipo, destino, saida) {
    
    require(stringr)
    require(openxlsx)
    require(purrr)
    
    on.exit(closeAllConnections())
    
    if (tipo == 'consorcios')
      arquivo <- paste0(str_remove(ano_mes, '-'), 'ADMCONSORCIO', '.zip')
    else if (tipo == 'filiais_adm_consorcios')
      arquivo <- paste0(str_remove(ano_mes, '-'), 'FILIAISCONS', '.zip')
    else
      arquivo <- paste0(str_remove(ano_mes, '-'), toupper(tipo), '.zip')
    
    if (is.null(destino))
      return('Diretório de destino não especificado.')
    
    msg <- 
      tibble::tibble(
        periodo = ano_mes,
        tipo = tipo,
        url = NA_character_,
        arquivos = 0,
        status = 'falha'
      )
    
    message(file.path(destino, tipo))
    dir.create(file.path(destino, tipo), showWarnings = FALSE)
    url_arquivo <- 
      paste(
        'https://www.bcb.gov.br/fis/info/cad', 
        tolower(tipo), 
        arquivo, 
        sep = '/'
      )
    tmp_file <- tempfile()
    arq <- 
      try(
        download.file(
          url = url_arquivo,
          destfile = tmp_file, #file.path(destino, arquivo)
          method = 'wininet',
          quiet = T
        ),
        silent = T
      )
    if (class(arq) == 'try-error') {
      msg <- 
        tibble::tibble(
          periodo = ano_mes,
          tipo = tipo,
          url = url_arquivo,
          arquivos = 0,
          status = 'erro no download'
        )
      return(msg)
    } else {
      arquivos_lst <- unzip(tmp_file, list = TRUE)$Name
      arquivo_xl <- arquivos_lst[str_detect(arquivos_lst, '\\.xls$|\\.xlsx$')]
      unzip(
        tmp_file, 
        files = arquivo_xl, 
        exdir = file.path(destino, tipo)
      )
      arquivo_xl <- file.path(destino, tipo, arquivo_xl)
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
            file.remove(arq_i)
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
            message('Salvo em ', str_replace(arq_i, 'xls$|xlsx$', saida))
          }
        )
        # message('Salvo em ', file.path(destino, tipo))
        msg <- 
          tibble::tibble(
            periodo = ano_mes,
            tipo = tipo,
            url = url_arquivo,
            arquivos = sum(length(arquivo_xl)) * length(saida),
            status = 'ok'
          )
      }
      # file.remove(file.path(destino, arquivo))
      unlink(tmp_file)
    }
    return(msg)
  }
