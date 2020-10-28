# https://www.anatel.gov.br/dados/component/content/article/125-chamadas/280-dados-abertos

anatel <- 
  function(url, file_url, file_name_prefix, date_css_elem, date_attr, out_dir) {
    
    require(rvest)
    require(httr)
    require(tibble)
    require(purrr)
    require(lubridate)
    require(tools)
    
    on.exit(closeAllConnections())
    
    out <- 
      tibble(
        tipo = file_name_prefix,
        arquivo = basename(file_url),
        data_local = NA_Date_,
        data_online = NA_Date_,
        status = 'erro'
      )
    message('\nArquivo: ', file_url)
    closeAllConnections()
    date <- 
      try({
        GET(url, timeout(30)) %>% 
          read_html() %>% 
          html_node(date_css_elem) %>% 
          html_attr(date_attr) %>% 
          as.POSIXct()
      })
    if (any(attr(date, 'class') == 'try-error')) {
      if (str_detect(attr(date, 'condition'), 'Timeout|timed out'))
        out$status <- 'timeout'
      return(out)
    }
    out$data_online <- date
    message('Última atualização: ', date)
    files_found <- 
      list.files(path = out_dir, pattern = file_name_prefix, full.names = TRUE)
    last_mtime <- 
      suppressWarnings(file.mtime(files_found) %>% max)
    out$data_local <- last_mtime
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
            ifelse(file_ext(file_url) != '', file_ext(file_url), 'csv')
          )
        )
      closeAllConnections()
      dw <- download.file(url = file_url, destfile = out_file, quiet = TRUE)
      if (dw == 0) {
        message('Arquivo salvo em ', out_file)
        out$status <- 'atualizado'
      }
      else {
        message('Erro durante download.')
        out$status <- 'erro no download'
      }
    }
    else{
      message('Nenhuma atualização disponível.')
      out$status <- 'nenhuma atualizacao disponivel'
    }
    out
  }
