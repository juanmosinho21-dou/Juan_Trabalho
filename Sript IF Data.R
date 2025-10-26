#-------OBJETIVOS DO TRABALHO------------

#1 - Medir a receita com TVM ✅
#2 - Medir indicadores (ROA/ROE)✅
#3 - Medir SPREAD bancário 

#-------LIBRARY PACOTES-------
library(dplyr)
library(GetBCBData)
library(xts)
library(lubridate)
library(urca)
library(tseries)
library(vars)
library(ggplot2)
library(readxl)
library(purrr)
#-------INFORMATIVENESS---------
#Todas as series são estacionárias a um nível 5% de sig
#Precisa coletar os dados na mão da Caixa para fechar todo o período
#Identificar também quais os meses vão passar com erro de NA e verificar se não existe mesmo

#-------CAMINHO do ZIP-----------
zip_path <- "C:/Users/juanm/Downloads/arquivos renomeados.zip"

Passivo <- IF_DATA_BACEN[grep("Passivo", names(IF_DATA_BACEN))]
Ativo <- IF_DATA_BACEN[grep("Ativo", names(IF_DATA_BACEN))]
DRE <- IF_DATA_BACEN[grep("Dem_Resultado", names(IF_DATA_BACEN))]
      
#--------FILTRANDO O PATRIMONIO LIQUIDO------
Passivo_df <- map_dfr(Passivo, as.data.frame)

Patrimônio_Líquido_test <- Passivo_df %>%
  filter(TCB == "b1", Código %in% AT_6$Código) %>%
  select(Instituição, Código, Data, `Patrimônio.Líquido..j.`) %>%
  rename(Patrimônio_Líquido = `Patrimônio.Líquido..j.`)

#--------FILTRANDO O LUCRO LIQUIDO----------

Lucro_Líquido <- bind_rows(DRE) %>%
  filter(TCB == "b1" & Código %in% AT_6$Código) %>%  
  select(Instituição, Código, Data, `Lucro.Líquido..j.....g.....h.....i.`)
Lucro_Líquido <- Lucro_Líquido %>%         
  rename(
    Lucro_Líquido = `Lucro.Líquido..j.....g.....h.....i.`)

#----------MEDINDO A QUANTIDADE----------
Número_dataPL <- Patrimônio_Líquido %>%
  group_by(Instituição) %>%
  summarise(n_datas = n_distinct(Data))

Número_dataLL <- Lucro_Líquido %>%
  group_by(Instituição) %>%
  summarise(n_datas = n_distinct(Data))

#--------FILTRANDO O ATIVO TOTAL---------

Ativo_Total <- bind_rows(Ativo) %>%
  filter(TCB == "b1" & (TC == 1 | TC == 2)) %>%
  select(Instituição, Código, Data, `Ativo.Total..k.....i.....j.`, TCB)
Ativo_Total <- Ativo_Total %>%         
  rename(
    Ativo_Total = `Ativo.Total..k.....i.....j.`)

#--------FILTRANDO OS TÍTULOS E VALORES MOBILIARIARIOS----------

TVM <- bind_rows(Ativo) %>%
  filter(TCB == "b1" & Código %in% AT_6$Código) %>%  
  select(Data, Instituição, Código, TVM.e.Instrumentos.Financeiros.Derivativos..c.)
TVM <- TVM %>%         
  rename(
    TVM = TVM.e.Instrumentos.Financeiros.Derivativos..c.)

#------------PUXANDO APENAS OS 5 MAIORES-----------------

#FICOU COM 6 PORQUÊ A CAIXA ALTERA O CÓDIGO AO LONGO DO TEMPO

AT_6 <- Ativo_Total %>%
  mutate(Ativo_Total = as.numeric(gsub("\\.", "", Ativo_Total))) %>%  
  filter(!is.na(Instituição)) %>%                                   
  group_by(TCB, Código, Instituição) %>%                             
  summarise(media_ativo = mean(Ativo_Total, na.rm = TRUE)) %>%      
  arrange(desc(media_ativo)) %>%
  head(6)

#-----------CALCULATION O ROE---------------

Lucro_Líquido <- Lucro_Líquido %>%
  mutate(Lucro_Líquido = as.numeric(gsub(",", ".", gsub("\\.", "", Lucro_Líquido))))

Patrimônio_Líquido <- Patrimônio_Líquido %>%
  mutate(Patrimônio_Líquido = as.numeric(gsub(",", ".", gsub("\\.", "", Patrimônio_Líquido))))

ROE <- Lucro_Líquido %>%
  left_join(Patrimônio_Líquido, by = c("Instituição", "Data")) %>%
  mutate(ROE = (Lucro_Líquido / Patrimônio_Líquido) * 100)
  
ROE <- ROE %>%
    select(-Lucro_Líquido,Patrimônio_Líquido,Código.y)

ROE <- ROE %>%
  select(Data, Instituição, ROE)


Número_ROE <- ROE %>%
  group_by(Instituição) %>%
  summarise(n_datas = n_distinct(Data))

#-----------TAXA SELIC------------
Taxa_SELIC <- gbcbd_get_series(
  id = 432, # Taxa Selic anualizada base 252
  first.date = Sys.Date() - 25 * 365,
  last.date = Sys.Date(),
  format.data = "long",
  be.quiet = FALSE
)

Taxa_SELIC_Trimestral <- Taxa_SELIC %>%
  mutate(
    Trimestre = paste0(year(ref.date), "-Q", quarter(ref.date))  
  ) %>%
  group_by(Trimestre) %>%
  summarise(
    SELIC_Média = mean(value, na.rm = TRUE),   # média do trimestre
    SELIC_Fim = last(value)                   
  )
#---------TAXA SELIC TRIMESTRAL----------------
Taxa_SELIC_Trimestral <- Taxa_SELIC_Trimestral %>%
  mutate(
    Mes = case_when(
      str_detect(Trimestre, "Q1") ~ "03",
      str_detect(Trimestre, "Q2") ~ "06",
      str_detect(Trimestre, "Q3") ~ "09",
      str_detect(Trimestre, "Q4") ~ "12"
    ),
    Ano = str_sub(Trimestre, 1, 4),
    Data = paste0(Mes, "/", Ano)
  ) %>%
  select(Data, everything(), -Mes, -Ano, -Trimestre)
#---------ROE INDIVIDUALMENTE----------
#PARA O SANTANDER
ROE_BB <- ROE %>%
  filter(Instituição %in% c("BB")) %>%
  select(Data, Instituição, ROE)

#PARA O SANTANDER
ROE_SANTANDER <- ROE %>%
  filter(Instituição %in% c("SANTANDER BRASIL","SANTANDER BANESPA", "SANTANDER"))

#PARA O BRADESCO
ROE_BRADESCO <- ROE %>%
  filter(Instituição == "BRADESCO") 

#PARA O CAIXA ECONOMICA FEDERAL
ROE_CAIXA <-ROE %>%
  filter(Instituição == "CAIXA ECONOMICA FEDERAL") 


#PARA O ITAU
ROE_ITAU <- ROE %>%
  filter(Instituição == "ITAU") %>%
  select(Data, Instituição, ROE)

#---------ADF INDIVIDUALMENTE------
#Teste ADF para Taxa SELIC
summary(ur.df(Taxa_SELIC_Trimestral$SELIC_Média, type = "drift", selectlags = "AIC"))
#PARA 5% e 1 é estacionária, para 10% não

# Teste ADF para ROE DO BANCO DO BRASIL
ROE_BB <- ROE_BB %>%
  filter(!is.na(ROE))
summary(ur.df(ROE_BB$ROE, type = "drift", selectlags = "AIC"))
#A 5% E 10% de significance é estácionario, ja 1% não

# Teste ADF para ROE DO SANTANDER
ROE_BRADESCO <- ROE_BRADESCO %>%
  filter(!is.na(ROE))
summary(ur.df(ROE_BRADESCO$ROE, type = "drift", selectlags = "AIC"))
#A 5% E 10% de significance é estácionario, ja 1% não

plot(ROE_CAIXA$ROE)
plot(ROE_CAIXA$Data, ROE_CAIXA$ROE)

# Teste ADF para ROE DO CAIXA
ROE_CAIXA <- ROE_CAIXA %>%
  filter(!is.na(ROE))
summary(ur.df(ROE_CAIXA$ROE, type = "drift", selectlags = "AIC"))
#Estacinária para todos os nível de significância 1%,5% e 10%

# Teste ADF para ROE DO ITAU
ROE_ITAU <- ROE_ITAU %>%
  filter(!is.na(ROE))
summary(ur.df(ROE_ITAU$ROE, type = "drift", selectlags = "AIC"))
#Estacinária para todos os nível de significância 1%,5% e 10%

# Teste ADF para ROE DO SANTANDER
ROE_SANTANDER <- ROE_SANTANDER %>%
  filter(!is.na(ROE))
summary(ur.df(ROE_SANTANDER$ROE, type = "drift", selectlags = "AIC"))
#Estacinária para todos os nível de significância 1%,5% e 10%

#Irei fazer o var para com nível de significância de 5%

#-----VETOR AUTOREGRESSIVO------
#Não foi possível fazer porquê os estão com tamanhos diferentes, irei fazer na mão os que estão faltando

VAR_todos <- data.frame(
  ROE_CAIXA = ROE_CAIXA$ROE,
  ROE_BB = ROE_BB$ROE,
  ROE_BRADESCO = ROE_BRADESCO$ROE,
  ROE_ITAU = ROE_ITAU$ROE,
  ROE_SANTANDER = ROE_SANTANDER$ROE,
  SELIC = Taxa_SELIC_Trimestral$SELIC_Média
)




