
# Pacotes -----------------------------------------------------------------

install.packages('dplyr')
install.packages('ggplot2')
install.packages('tibble')
install.packages('gridExtra')
install.packages('readr')
install.packages('skimr')
install.packages('plotly')


# Carregando pacotes ------------------------------------------------------

library(dplyr)
library(ggplot2)
library(tibble)
library(gridExtra)
library(readr)
library(skimr)
library(plotly)

# Carregando os dados -----------------------------------------------------

#vendas de derivados de petroleo
vendas_petroleo <- utils::read.csv('vendas_derivados_petroleo.csv',
                                   sep = ';',
                                   dec = ',')

#liquidos de entregas por distribuidor atual
entregas_distribuidor <- utils::read.csv('Liquidos_Entregas_Distribuidor_Atual.csv',
                                   sep = ';',
                                   dec = '.')

#liquidos de entregas por fornecedor atual
entregas_fornecedor <- utils::read.csv('Liquidos_Entregas_Fornecedor_Atual.csv',
                                         sep = ';',
                                         dec = '.')

#bases de distribuição
bases_distribuicao <- utils::read.csv('BASES_DE_DISTRIBUICAO_CSV.csv',
                                         sep = ';',
                                         dec = ',')


# VENDAS_PETROLEO ---------------------------------------------------------

### ETANOL 

   petroleo_etanolh <- vendas_petroleo %>% 
    dplyr::filter(PRODUTO == 'ETANOL HIDRATADO' & ANO > 2019) %>%
  dplyr::group_by(GRANDE_REGIAO, ANO) %>% 
    dplyr::summarise(total = sum(VENDAS_m3))
  
# Gráfico



### DIESEL 
petroleo_diesel <- vendas_petroleo %>% 
  dplyr::filter(PRODUTO == 'ÓLEO DIESEL' & ANO > 2019) %>% 
  dplyr::group_by(GRANDE_REGIAO, ANO) %>% 
  dplyr::summarise(total = sum(VENDAS_m3))




# Gáfico

### GASOLINA C
petroleo_gasolina <- vendas_petroleo %>% 
  dplyr::filter(PRODUTO == 'GASOLINA C' & ANO > 2019) %>% 
  dplyr::group_by(GRANDE_REGIAO, ANO) %>% 
  dplyr::summarise(total = sum(VENDAS_m3))

# Gráficos

grafico_1 <- ggplot2::ggplot(data = petroleo_diesel)+
  ggplot2::aes(
    x = ANO, 
    y = total, 
    fill = GRANDE_REGIAO)+
  ggplot2::geom_point()+
  ggplot2::labs(
    x = 'Ano',
    y = 'Total de vendas por m3',
    fill = 'Região',
    title = 'Óleo Diesel - 2020 a 2022')

#GRAFico

grafico_2 <- ggplot2::ggplot(data = petroleo_gasolina)+
  ggplot2::aes(
    x = ANO,
    y = total,
  )+
  ggplot2::geom_point(
    aes(colour = GRANDE_REGIAO),
    size = 6
  )+
  ggplot2::labs(
    x = 'Ano - 2020 a 2022',
    y = 'Total de vendas por m3',
    title = 'Vendas de óleo gasolina em 2020 a 2022',
    caption = 'Fonte: ANP'
  )+
  ggplot2::geom_text(
    aes(
      x = ANO,
      y = total,
      label = GRANDE_REGIAO
      ),
    size = 2.5
)+
  ggplot2::theme(
    legend.position = 'none'
  )

plotly::ggplotly(grafico_2)


