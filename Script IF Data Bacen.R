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
library(stringr)
library(writexl)
library(tidyverse)

#-------INFORMATIVENESS---------

#Puxar os dados para fazer a mesma analise para o spread e tvm
#system("git add Testando25.pdf") <- para adicionar arquivo no git pelo console
#Perguntar ao professor como se faz o criteiro de defasagem estando difente Selic X ROE de banco X
#Qual colocar?

#-------CAMINHO do ZIP-----------

# Caminho do zip
zip_path <- "C:/Users/juanm/Downloads/arquivos renomeados.zip"

# Pasta onde será extraído
dest_dir <- "C:/Users/juanm/Downloads/arquivos_renomeados_extraidos"
dir.create(dest_dir, showWarnings = FALSE)

# Extrair o conteúdo do ZIP
unzip(zip_path, exdir = dest_dir)

# Listar todos os arquivos .csv (inclusive dentro da subpasta)
arquivos_excel <- list.files(dest_dir, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
read.z
# Mostrar apenas o nome do arquivo (sem caminho)
nomes_arquivos <- basename(arquivos_excel)
IF_DATA_BACEN <- lapply(arquivos_excel, read.csv, sep = ";", dec = ",", encoding = "UTF-8")

Passivo <- IF_DATA_BACEN[grep("Passivo", names(IF_DATA_BACEN))]
Ativo <- IF_DATA_BACEN[grep("Ativo", names(IF_DATA_BACEN))]
DRE <- IF_DATA_BACEN[grep("Dem_Resultado", names(IF_DATA_BACEN))]

Passivo_df <- as.data.frame(Passivo)
Ativo_df   <- as.data.frame(Ativo)
DRE_df     <- as.data.frame(DRE)
      
#--------FILTRANDO O PATRIMONIO LIQUIDO------

Passivo <- bind_rows(Passivo)

Patrimônio_Líquido <- Passivo %>%
  filter(TCB == "b1", Código %in% AT_6$Código) %>%
  dplyr::select(Instituição, Código, Data, `Patrimônio.Líquido..j.`) %>%
  rename(Patrimônio_Líquido = `Patrimônio.Líquido..j.`)
  Patrimônio_Líquido <- na.omit(Patrimônio_Líquido)

#-------TRATANDO FALTANTES PARA PL-------

Bancos_Faltantes_PL <- Passivo %>%
  filter(TCB == "b1",
         Código %in% AT_6$Código,
         Data %in% FALTANTES_BB) %>%
  dplyr::select(Instituição, Código, Data, Patrimônio.Líquido..i.,Patrimônio.Líquido..j.) %>%
  rename(Patrimônio_Líquido = C(Patrimônio.Líquido..i.,Patrimônio.Líquido..j.))

#Organizando o Data Frame
Bancos_Faltantes_PL <- Bancos_Faltantes_PL %>%
    filter(Código %in% AT_6$Código) %>%
    mutate(Patrimônio_Líquido = coalesce(`Patrimônio.Líquido..i.`, `Patrimônio.Líquido..j.`)) %>%
    dplyr::select(Instituição,
                  Código,
                  Data,
                  Patrimônio_Líquido)

#Juntando os data frmames
Patrimônio_Líquido <- distinct(rbind(Bancos_Faltantes_PL, Patrimônio_Líquido))
  
#--------FILTRANDO O LUCRO LIQUIDO----------

Lucro_Líquido <- DRE %>%
  filter(TCB == "b1" & Código %in% AT_6$Código) %>%  
  select(Instituição, Código, Data, `Lucro.Líquido..j.....g.....h.....i.`)
Lucro_Líquido <- Lucro_Líquido %>%         
  rename(
  Lucro_Líquido = `Lucro.Líquido..j.....g.....h.....i.`)
Lucro_Líquido <- na.omit(Lucro_Líquido)

#-------TRATANDO FALTANTES PARA LL-------

Bancos_Faltantes_LL <- DRE %>%
  filter(TCB == "b1",
         Código %in% AT_6$Código,
         Data %in% FALTANTES_BB) %>%
  dplyr::select(Instituição, Código, Data, 'Lucro.Líquido..j.....g.....h.....i.') %>%
  rename(Lucro_Líquido = ('Lucro.Líquido..j.....g.....h.....i.'))

Bancos_Faltantes_LL <- Bancos_Faltantes_LL %>%
  mutate(Lucro_Líquido = gsub("\\.", "", Lucro_Líquido))

Lucro_Líquido <- distinct(bind_rows(Lucro_Líquido, Bancos_Faltantes_LL))

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

#--------FILTRANDO OS TÍTULOS E VALORES MOBILIARIOS--------

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

ROE_testando_todos <- Lucro_Líquido %>%
  left_join(Patrimônio_Líquido, by = c("Instituição", "Data", "Código")) %>%
  mutate(ROE = (Lucro_Líquido / Patrimônio_Líquido) * 100)

ROE_testando_todos <- ROE_testando_todos %>%
  dplyr::select(Data, Instituição, Código, ROE)

Número_ROE <- ROE_testando_todos %>%
  group_by(Instituição) %>%
  summarise(n_datas = n_distinct(Data))

#-----------TAXA SELIC------------

Taxa_SELIC <- gbcbd_get_series(
  id = 432, 
  first.date = as.Date("2000-01-01"),
  last.date  = as.Date("2024-12-31"),
  format.data = "long",
  be.quiet = FALSE)

Taxa_SELIC_Trimestral <- Taxa_SELIC %>%
  mutate(
    Trimestre = paste0(year(ref.date), "-Q", quarter(ref.date))  
  ) %>%
  group_by(Trimestre) %>%
  summarise(
    SELIC_Média = mean(value, na.rm = TRUE),
    SELIC_Fim = last(value) )

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
  )

Taxa_SELIC_Trimestral <- Taxa_SELIC_Trimestral %>%
  dplyr::select(Data, SELIC_Média, SELIC_Fim)

#---------ROE INDIVIDUALMENTE----------

ROE_BB <- ROE_testando_todos %>%
  filter(Código %in% ("49906")) %>%
  dplyr::select(Data, Código, Instituição, ROE)
ROE_BB <- na.omit(ROE_BB)

#PARA O SANTANDER, ATUALIZANDO -> ESTOU FAZENDO O VAR
ROE_SANTANDER <- ROE_testando_todos %>%
  filter(Código %in% c("30379"))
dplyr::select(Data, Código, Instituição, ROE)
ROE_SANTANDER <- na.omit(ROE_SANTANDER)

#PARA O BRADESCO
ROE_BRADESCO <- ROE_testando_todos %>%
  filter(Código == "10045") 
  dplyr::select(Data, Código, Instituição, ROE)
  ROE_BRADESCO <- na.omit(ROE_BRADESCO)
  
#PARA O CAIXA ECONOMICA FEDERAL
ROE_CAIXA <- ROE_testando_todos %>%
  filter(Código %in% c("51626","360305"))
  dplyr::select(Data, Código, Instituição, ROE)
ROE_CAIXA <- na.omit(ROE_CAIXA)

#PARA O ITAU
ROE_ITAU <- ROE_testando_todos %>%
  filter(Código == "10069") %>%
  dplyr::select(Data, Código, Instituição, ROE)
ROE_ITAU <- na.omit(ROE_ITAU)


#---------ADF INDIVIDUALMENTE------

#Teste ADF para Taxa SELIC
#Coloquei em gráfico para verificar se havia constância e tendência

plot(Taxa_SELIC_Trimestral$SELIC_Fim, type = "l", main = "Série temporal")

Df_Taxa_Selic <- adf.test(Taxa_SELIC_Trimestral$SELIC_Fim)
summary(Df_Taxa_Selic)

#data:  Taxa_SELIC_Trimestral$SELIC_Fim
#Dickey-Fuller = -2.2807, Lag order = 4, p-value = 0.4604
#alternative hypothesis: stationary

# Teste ADF para ROE DO BANCO DO BRASIL

plot(ROE_BB$ROE, type = "l", main = "Série temporal")

Df_BB <- adf.test(ROE_BB$ROE)
summary(Df_BB)

#Augmented Dickey-Fuller Test
#data:  ROE_BB$ROE
#Dickey-Fuller = -3.0473, Lag order = 4, p-value = 0.1428
#alternative hypothesis: stationary


# Teste ADF para ROE DO CAIXA
plot(ROE_CAIXA$ROE, type = "l", main = "Série temporal")

Df_CAIXA <- adf.test(ROE_CAIXA$ROE)
summary(Df_CAIXA)

#Augmented Dickey-Fuller Test
#data:  ROE_CAIXA$ROE
#Dickey-Fuller = -4.2465, Lag order = 4, p-value = 0.01
#alternative hypothesis: stationary


# Teste ADF para ROE DO SANTANDER
plot(ROE_SANTANDER$ROE, type = "l", main = "Série temporal")

Df_SANTANDER <- adf.test(ROE_SANTANDER$ROE)
summary(Df_SANTANDER)

#Augmented Dickey-Fuller Test
#data:  ROE_SANTANDER$ROE
#Dickey-Fuller = -4.7036, Lag order = 4, p-value = 0.01
#alternative hypothesis: stationary


# Teste ADF para ROE DO ITAU

plot(ROE_ITAU$ROE, type = "l", main = "Série temporal")

Df_Itaú <- adf.test(ROE_ITAU$ROE)
summary(Df_Itaú)

#Augmented Dickey-Fuller Test
#data:  ROE_ITAU$ROE
#Dickey-Fuller = -2.0933, Lag order = 4, p-value = 0.538
#alternative hypothesis: stationary

"Não é estacionária"


# Teste ADF para ROE DO BRADESCO

plot(ROE_BRADESCO$ROE, type = "l", main = "Série temporal")

Df_BRADESCO <- adf.test(ROE_BRADESCO$ROE)
summary(Df_SANTANDER)

#Augmented Dickey-Fuller Test
#data: ROE_BRADESCO$ROE
#Dickey-Fuller = -1.9171, Lag order = 4, p-value = 0.611
#alternative hypothesis: stationary

"Não é estacionária"

#------DETERMINANDO A ORDEM DE DEFASAGEM---------

#DEFININDO A TAXA SELIC -> SC(n) 2
VARselect(Taxa_SELIC_Trimestral$SELIC_Fim) 
#AIC(n)  HQ(n)  SC(n) FPE(n) 
#4      4      2      4 

#DEFININDO DEFASAGEM PARA O SANTANDER -> SC(n) 1
VARselect(ROE_SANTANDER$ROE)
#AIC(n)  HQ(n)  SC(n) FPE(n) 
#1      1      1      1 

#DEFININDO DEFASAGEM PARA O BANCO DO BRASIL -> SC(n) 2
VARselect(ROE_BB$ROE)
#AIC(n)  HQ(n)  SC(n) FPE(n) 
#2      2      2      2 

#DEFININDO DEFASAGEM PARA O CAIXA -> SC(n) 1
VARselect(ROE_CAIXA$ROE)
#AIC(n)  HQ(n)  SC(n) FPE(n) 
#1      1      1      1 

#DEFININDO DEFASAGEM PARA O ITAU -> SC(n) 2
VARselect(ROE_ITAU$ROE)
#AIC(n)  HQ(n)  SC(n) FPE(n) 
#6      3      2      6 

#DEFININDO DEFASAGEM PARA O BRADESCO -> SC(n) 1
VARselect(ROE_BRADESCO$ROE)
#AIC(n)  HQ(n)  SC(n) FPE(n) 
#8      3      1      8 

#---------VAR-----------

#VAR PARA O BANCO DO BRASIL
VAR_BB <- VAR(cbind(ROE = ROE_BB$ROE, 
    SELIC = Taxa_SELIC_Trimestral$SELIC_Fim), p = 2,  type = "const")

#Coefficients:
#  y1.l1    y2.l1    const
#0.5967  -0.2729119   3.2708951
# O ROE depende positivamente de seu própio valor defasado (59,67%)
#Aumento de 1 p.p reduz o roe em 0,2729119, sugere efeito negativo da selic no roe

#VAR PARA O SANTANDER
VAR_SANTANDER <- VAR(cbind(ROE = ROE_SANTANDER$ROE, 
    SELIC = Taxa_SELIC_Trimestral$SELIC_Fim), p = 1,   type = "const")

#Coefficients:
#  y1.l1     y2.l1     const  
#-0.06755  -0.38550   8.93252 
# O ROE tem efeito negativo sobre seu propio valor no cp (-0,06755%)
#Aumento de 1 p.p reduz o roe em 39%, sugere efeito negativo da selic no roe

#VAR PARA O ITAU
VAR_ITAU <- VAR(cbind(ROE = ROE_ITAU$ROE, 
  SELIC = Taxa_SELIC_Trimestral$SELIC_Fim), p = 2, type = "const", ic = ("SC"))

#y1.l1      y2.l1         const 
#0.2184639 -0.3188805    6.4584815 
# O ROE depende positivamente de seu própio valor defasado (0,2184639%)
#Aumento de 1 p.p reduz o roe em 31,85%, sugere efeito negativo da selic no roe

#VAR PARA O CAIXA
VAR_CAIXA <- VAR(cbind(ROE = ROE_CAIXA$ROE, 
              SELIC = Taxa_SELIC_Trimestral$SELIC_Fim), p = 1, type = "const")

#Coefficients:
#y1.l1       y2.l1       const 
#0.05751775 -0.41187828 11.75613751 
# O ROE depende positivamente de seu própio valor defasado (0,05752%)
#Aumento de 1 p.p reduz o roe em 41,18%, sugere efeito negativo da selic no roe


#VAR PARA O BRADESCO
VAR_BRADESCO <- VAR(cbind(ROE = ROE_BRADESCO$ROE,
    SELIC = Taxa_SELIC_Trimestral$SELIC_Fim), p = 1,type = "const")

#Coefficients:
#y1.l1    y2.l1    const  
#0.6567  -0.1032   3.5886  
# O ROE depende positivamente de seu própio valor defasado (0,6567%)
#Aumento de 1 p.p reduz o roe em 0,1032%, sugere efeito negativo da selic no roe
