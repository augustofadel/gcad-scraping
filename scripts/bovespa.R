# SCRAP BOVESPA
source("scripts/b3_init.R")

# coleta codigos CVM de empresas com registro nao cancelado
cod <- cvm.cod(node = "#dlCiasCdCVM")

# coleta informacoes na Bovespa
info <- bovespa("2017", cod)
# consolida dados
dados <- 
  consolida.dados(
    info, 
    data.balanco = "31/12/2019",
    node.cad = "#accordionDados > table", 
    node.end = "#divContatos > div:nth-child(1) > table",
    node.bpatrimonial = "#divDadosEconNovo > div:nth-child(1) > table",
    node.dresultado = "#divDadosEconNovo > div:nth-child(2) > table",
    node.dfluxocaixa = "#divDadosEconNovo > div:nth-child(3) > table"
  )

# exporta arquivos csv
for (i in names(dados)) {
  write.csv(
    dados[[i]],
    na = "",
    # file = file.path('//wnasprd03f/CEMPRE/CRAWLER/BOVESPA', paste0(i, '_', Sys.Date(),'.csv')),
    file = file.path('//wnasprd03f/CEMPRE/CRAWLER/BOVESPA', paste0(i, '.csv')),
    row.names = F
  )
}