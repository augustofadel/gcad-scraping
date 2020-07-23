# SCRAP BOVESPA
# pacotes
packages.list <- c('rvest', 'readr', 'stringr', 'plyr', 'dplyr', 'tidyr', 'tibble', 'haven', 'formattable')
new.packages <- packages.list[!(packages.list %in% installed.packages()[,'Package'])]
if(length(new.packages)) {
     install.packages(new.packages)
}
lapply(packages.list, require, character.only = TRUE)


#### CVM ####
# funcao para recuperar tabelas de empresas CVM
scrap.tab <- function(x, node, head) {
     aux <- try(x %>% 
                     html_node(node) %>% 
                     html_table(header = head),
                silent = T)
     return(aux)
}


# funcao para recuperar codigos de empresas CVM
cvm.cod <- function(url = 'http://cvmweb.cvm.gov.br/SWB/Sistemas/SCW/CPublica/CiaAb/FormBuscaCiaAbOrdAlf.aspx?LetraInicial=', 
                    indice = c(LETTERS, 0:9), 
                    node) {
     cat("\nColetando relacao de empresas da CVM... ")
     empresas <- paste0(url, indice) %>%
          lapply(read_html) %>%
          lapply(scrap.tab, node = node, head = T)
     empresas <- ldply(empresas[lapply(empresas, class) == "data.frame"], as_tibble)
     cat("concluido.\n")
     names(empresas) <- iconv(names(empresas), 'UTF-8', 'ASCII//TRANSLIT')
     # cnpj <- empresas$CNPJ %>%
     #      str_replace_all("[[:punct:]]", "") %>%
     #      str_sub(1, 12) %>%
     #      unique() %>%
     #      sort()
     # cat("Identificados", length(cnpj), "CNPJ distintos.\n")
     cod <- empresas[!str_detect(empresas$`SITUACAO REGISTRO`, "Cancelado"), "CODIGO CVM"]
     cat("Identificados", length(cod), "registros com codigo CVM ativo.\n\n")
     
     return(cod)
}


#### BOVESPA ####
# coleta dados bovespa
bovespa <- function(ano, cod) {
     cat("\nColetando dados da Bovespa... ")
     info <- paste0("http://bvmf.bmfbovespa.com.br/pt-br/mercados/acoes/empresas/ExecutaAcaoConsultaInfoEmp.asp?CodCVM=", cod, "&amp;ViewDoc=1&amp;AnoDoc=", ano, "&amp;VersaoDoc=3&amp;NumSeqDoc=64444#a") %>%
          lapply(read_html)
     cat("concluido.\n")
     return(info)
}


# organiza dados
consolida.dados <- function(info, data.balanco, node.cad, node.end, node.bpatrimonial, node.dresultado, node.dfluxocaixa) {
     cat("\nCompilando informacoes cadastrais... ")
     dat <- info %>% 
          lapply(scrap.tab, node = node.cad, head = F)
     valid <- lapply(dat, class) == "data.frame"
     dat <- dat[valid] %>% 
          lapply(function(x) {
               aux <- x$X2
               names(aux) <- gsub(":", "", iconv(x$X1, "UTF-8", "ASCII//TRANSLIT"))
               aux <- enframe(aux) %>%
                    spread(name, value)
               return(aux)
          }) %>%
          ldply() %>%
          as_tibble()
     #names(dat) <- gsub(":", "", iconv(names(dat), "UTF-8", "ASCII//TRANSLIT"))
     dat$CNPJ <- dat$CNPJ %>% str_replace_all("[[:punct:]]", "")
     dat$`Codigos de Negociacao` <- dat$`Codigos de Negociacao` %>% 
          iconv("UTF-8", "ASCII//TRANSLIT") %>%
          str_split("\r\n") %>% 
          lapply(function(x) {gsub("Mais Codigos", "", x[1])}) %>% unlist()
     contato <- info[valid] %>% 
          lapply(scrap.tab, node = node.end, head = F) %>% 
          lapply(function(x) {
               aux <- str_replace_all(x$X2,"[\r\n\t]", "")
               names(aux) <- gsub(":", "", iconv(x$X1, "UTF-8", "ASCII//TRANSLIT"))
               aux <- enframe(aux) %>%
                    spread(name, value)
               return(aux)
          }) %>%
          ldply() %>%
          as_tibble()
     dat <- bind_cols(dat, contato)
     # dat$`Codigo CVM` <- cod[valid]
     dat$Atualizacao <- info[valid] %>% 
          lapply(function(x) {
               x %>% 
                    html_node("body > div") %>% 
                    html_text() %>% 
                    str_extract("[0-9]{2}/[0-9]{2}/[0-9]{4}") # %>% dmy()
          }) %>%
          unlist()
     cat("concluido.\n")
     cat("Encontrados", nrow(dat), "registros.\n")
     
     cat("\nCompilando dados economico-financeiros:\n")
     # Balanco Patrimonial
     cat("Balanco patrimonial... ")
     dat2.bpatrimonial <- info[valid] %>%
          lapply(scrap.tab, node = node.bpatrimonial, head = T)
     valid2 <- unlist(dat2.bpatrimonial %>% lapply(function(x) {sum(str_detect(names(x), data.balanco)) == 1}))
     dat2.bpatrimonial <- dat2.bpatrimonial[valid2] %>%
          lapply(function(x) {
               aux <- x[,str_detect(names(x), data.balanco)] %>% 
                    str_replace_all("\\.", "") %>% 
                    str_replace("\\(", "-") %>% 
                    str_replace("\\)", "") %>% 
                    as.numeric()
               # aux <- x[,str_detect(names(x), data.balanco)] %>% str_replace_all("\\.","") %>% accounting()
               names(aux) <- iconv(x[,1], "UTF-8", "ASCII//TRANSLIT") %>% str_replace_all("[[:punct:]]", "")
               aux <- enframe(aux) %>%
                    spread(name, value)
               return(aux)
          }) %>%
          ldply() %>%
          as_tibble()
     dat2.bpatrimonial$Data <- data.balanco
     dat2.bpatrimonial$CNPJ <- dat$CNPJ[valid2]
     cat("concluido.\n")
     
     # Demontracao do Resultado
     cat("Demonstracao do resultado... ")
     dat2.dresultado <- info[valid] %>%
          lapply(scrap.tab, node = node.dresultado, head = T)
     valid2 <- lapply(dat2.dresultado, class) == "data.frame"
     dat2.dresultado <- dat2.dresultado[valid2] %>%
          lapply(function(x) {
               aux1 <- x[,2] %>% 
                    str_replace_all("\\.", "") %>% 
                    str_replace("\\(", "-") %>% 
                    str_replace("\\)", "") %>% 
                    c(names(x)[2])
               names(aux1) <- x[,1] %>% iconv("UTF-8", "ASCII//TRANSLIT") %>% c("Periodo") %>% str_replace_all("[[:punct:]]", "") %>% str_replace("\\(", "") %>% str_replace("\\)", "")
               aux1 <- enframe(aux1) %>%
                    spread(name, value)
               aux2 <- x[,3] %>% 
                    str_replace_all("\\.", "") %>% 
                    str_replace("\\(", "-") %>% 
                    str_replace("\\)", "") %>% 
                    c(names(x)[3])
               names(aux2) <- x[,1] %>% iconv("UTF-8", "ASCII//TRANSLIT") %>% c("Periodo") %>% str_replace_all("[[:punct:]]", "") %>% str_replace("\\(", "") %>% str_replace("\\)", "")
               aux2 <- enframe(aux2) %>%
                    spread(name, value)
               aux <- bind_rows(aux1, aux2)
               return(aux)
          }) %>%
          ldply() %>%
          as_tibble() %>% 
          mutate_at(vars(-Periodo), funs(as.numeric)) %>%
          mutate_at(vars(-Periodo), funs(as.numeric)) %>%
          separate(Periodo, c("Inicio Periodo", "Fim Periodo"), " a ")
     dat2.dresultado$CNPJ <- rep(dat$CNPJ[valid2], each = 2)
     cat("concluido.\n")

     # Demonstracao do Fluxo de Caixa
     cat("Demonstracao do fluxo de caixa... ")
dat2.dfluxocaixa <- info[valid] %>%
          lapply(scrap.tab, node = node.dfluxocaixa, head = T)
     valid2 <- lapply(dat2.dfluxocaixa, class) == "data.frame"
     dat2.dfluxocaixa <- dat2.dfluxocaixa[valid2] %>%
          lapply(function(x) {
               aux1 <- x[,2] %>% 
                    str_replace_all("\\.", "") %>% 
                    str_replace("\\(", "-") %>% 
                    str_replace("\\)", "") %>% 
                    c(names(x)[2])
               names(aux1) <- x[,1] %>% iconv("UTF-8", "ASCII//TRANSLIT") %>% c("Periodo") %>% str_replace_all("[[:punct:]]", "") %>% str_replace("\\(", "") %>% str_replace("\\)", "")
               aux1 <- enframe(aux1) %>%
                    spread(name, value)
               aux2 <- x[,3] %>% 
                    str_replace_all("\\.", "") %>% 
                    str_replace("\\(", "-") %>% 
                    str_replace("\\)", "") %>%
                    c(names(x)[3])
               names(aux2) <- x[,1] %>% iconv("UTF-8", "ASCII//TRANSLIT") %>% c("Periodo") %>% str_replace_all("[[:punct:]]", "") %>% str_replace("\\(", "") %>% str_replace("\\)", "")
               aux2 <- enframe(aux2) %>%
                    spread(name, value)
               aux <- bind_rows(aux1, aux2)
               return(aux)
          }) %>%
          ldply() %>%
          as_tibble() %>% 
          mutate_at(vars(-Periodo), funs(as.numeric)) %>%
          separate(Periodo, c("Inicio Periodo", "Fim Periodo"), " a ")
     dat2.dfluxocaixa$CNPJ <- rep(dat$CNPJ[valid2], each = 2)
     cat("concluido.\n")
     
     cat("\nEstruturando datasets finais... ")
     dat1 <- dat %>% 
          select(c(CNPJ, `Nome de Pregao`, Endereco, Telefone, Site, Atualizacao)) %>%
          mutate(`Nome de Pregao` = str_trim(`Nome de Pregao`, "both")) %>%
          separate("Telefone", c("Telefone", "Fax"), sep = "- Fax:") %>% 
          mutate(Telefone = str_replace_all(Telefone, "[^[:digit:]]", ""),
                 Fax = str_replace_all(Fax, "[^[:digit:]]", "")) %>%
          separate("Endereco", c("Endereco", "CEP"), sep = "CEP:") %>%
          separate("CEP", c("CEP", "Cidade"), sep = "Cidade:") %>%
          separate("Cidade", c("Cidade", "UF"), sep = "UF:") %>% 
          mutate(Endereco = str_trim(Endereco, "both"),
                 CEP = str_pad(str_trim(CEP, "both"), 8, "left", "0"),
                 Cidade = str_trim(Cidade, "both"))
     suppressWarnings(
          dat3 <- dat %>% 
               mutate(`Atividade Principal` = str_trim(`Atividade Principal`, "both")) %>%
               select(c(CNPJ, `Atividade Principal`)) %>% 
               separate("Atividade Principal", paste0("ativ", 1:10), sep = ";") %>%
               gather(key = num_ativ, value = Atividade, -CNPJ, na.rm = T) %>%
               mutate(num_ativ = parse_number(num_ativ), 
                      Atividade = Atividade %>% str_replace_all("[.,\r\n\t]", "") %>% str_trim("both")) %>%
               filter(Atividade != "")
     )
     suppressWarnings(
          dat4 <- dat %>% 
               select(c(CNPJ, `Classificacao Setorial`)) %>% 
               separate("Classificacao Setorial", paste0("cls_set", 1:10), sep = "/") %>%
               gather(key = num_clas, value = Classificacao, -CNPJ, na.rm = T) %>%
               mutate(num_clas = parse_number(num_clas))
     )
     suppressWarnings(
          dat5 <- dat %>%
               mutate(`Codigos de Negociacao` = str_trim(`Codigos de Negociacao`, "both")) %>%
               select(c(CNPJ, `Codigos de Negociacao`)) %>%
               separate("Codigos de Negociacao", paste0("cod", 1:10), sep = ";") %>%
               gather(key = num_cod, value = Codigo, -CNPJ, na.rm = T) %>%
               mutate(num_cod = parse_number(num_cod)) %>%
               filter(Codigo != "")
     )
     cat("concluido.\n\n")
     
     return(list(info_cadastral = dat1,
                 balanco_patrimonial = dat2.bpatrimonial,
                 demonstracao_resultado = dat2.dresultado,
                 demonstracao_fluxocaixa = dat2.dfluxocaixa,
                 ativ_principal = dat3, 
                 clas_setorial = dat4, 
                 cod_negociacao = dat5))
}