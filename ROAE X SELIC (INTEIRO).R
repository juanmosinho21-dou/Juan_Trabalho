#--------OBJETIVOS DO TRABALHO------------

#1 - Medir a receita com TVM ✅
#O que está faltando: o ROE já está como ROAE. Entratanto os bancos usam de outra forma.

#--------LIBRARY PACOTES-------

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

#--------INFORMATIVENESS---------

#Está faltando encontrar as defasagens.
#system("git add Testando25.pdf") <- para adicionar arquivo no git pelo console

#--------FILTRANDO O PATRIMONIO LIQUIDO------

Passivo <- bind_rows(Passivo)
Patrimônio_Líquido <- Passivo %>%
  filter(TCB == "b1", Código %in% AT_6$Código) %>%
  dplyr::select(Instituição, Código, Data, `Patrimônio.Líquido..j.`) %>%
  rename(Patrimônio_Líquido = `Patrimônio.Líquido..j.`)
Patrimônio_Líquido <- na.omit(Patrimônio_Líquido)

#--------TRATANDO FALTANTES PARA PL-------

Bancos_Faltantes_PL <- Passivo %>%
  filter(TCB == "b1",
         Código %in% AT_6$Código,
         Data %in% FALTANTES_BB) %>%
  dplyr::select(Instituição, Código, Data, Patrimônio.Líquido..i.,Patrimônio.Líquido..j.) %>%
  rename(Patrimônio_Líquido = C(Patrimônio.Líquido..i.,Patrimônio.Líquido..j.))

#Organizando o Data Frame
Bancos_Faltantes_PL <- Bancos_Faltantes_PL %>%
  filter(Código %in% AT_6$Código) %>%
mutate(Patrimônio_Líquido = coalesce(`Patrimônio.Líquido..i.`, `Patrimônio.Líquido..i.`))  %>%
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

#--------FILTRANDO LUCRO LÍQUIDO DO PERÍODO------

#ROAE = LLP/((PLt1+ Plt2/2) 
#ROAE = LUCRO LÍQUIDO DO PERÍODO/PATRIMÔNIO LÍQUIDO MÉDIO
#LLP  = LUCRO LÍQUDIO TRIMESTRAL
#PLM  = PL DO PERÍODO ((PLT1 + PLT2)/2)

#--------JUNTANDO OS DATA FRAMES PARA O ROAE-------

ROAE <- Patrimônio_Líquido %>%
  left_join(Lucro_Líquido,
            by = c("Código","Data")) %>%
  arrange(Código, Data) %>%
  distinct()

#--------CAUCULANDO O ROAE--------

ROAE_BANCO <- ROAE %>%
  group_by(Código) %>%
  arrange(Data) %>% 
  mutate(PL_Médio = (Patrimônio_Líquido + lag(Patrimônio_Líquido)) / 2,
  ROAE = Lucro_Líquido / PL_Médio) %>% 
  ungroup() %>%
  distinct()

ROAE_BANCOINT <- ROAE_BANCO %>%
  mutate(ROAE = ROAE * 4 * 100)

#--------MEDINDO A QUANTIDADE----------

Número_dataPL <- Patrimônio_Líquido %>%
  group_by(Instituição) %>%
  summarise(n_datas = n_distinct(Data))

Número_dataLL <- Lucro_Líquido %>%
  group_by(Instituição) %>%
  summarise(n_datas = n_distinct(Data))

#--------FILTRANDO O ATIVO TOTAL---------

Ativo_Total <- bind_rows(Ativo) %>%
  filter(TCB == "b1" & (TC == 1 | TC == 2)) %>%
  select(Instituição, Código, Data, `Ativo.Total..k.....i.....j.`,TCB)
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

#--------PUXANDO APENAS OS 5 MAIORES-----------------

#FICOU COM 6 PORQUÊ A CAIXA ALTERA O CÓDIGO AO LONGO DO TEMPO

AT_6 <- Ativo_Total %>%
  mutate(Ativo_Total = as.numeric(gsub("\\.", "", Ativo_Total))) %>%  
  filter(!is.na(Instituição)) %>%                                   
  group_by(TCB, Código, Instituição) %>%                             
  summarise(media_ativo = mean(Ativo_Total, na.rm = TRUE)) %>%      
  arrange(desc(media_ativo)) %>%
  head(6)

#--------TAXA SELIC------------

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

#--------TAXA SELIC TRIMESTRAL----------------

Taxa_SELIC_Trimestral <- Taxa_SELIC_Trimestral %>%
  mutate(
    Mes = case_when(
      str_detect(Trimestre, "Q1") ~ "03",
      str_detect(Trimestre, "Q2") ~ "06",
      str_detect(Trimestre, "Q3") ~ "09",
      str_detect(Trimestre, "Q4") ~ "12"),
    Ano = str_sub(Trimestre, 1, 4),
    Data = paste0(Mes, "/", Ano))

Taxa_SELIC_Trimestral <- Taxa_SELIC_Trimestral %>%
  dplyr::select(Data, SELIC_Média, SELIC_Fim)

#--------ROAE INDIVIDUALMENTE-----

#PARA O BANCO DO BRASIL
ROAE_BB <- ROAE_BANCOINT %>%
  filter(Código %in% ("49906")) %>%
  dplyr::select(Data, Código, Instituição.x, ROAE)
ROAE_BB <- na.omit(ROAE_BB$ROAE)
plot(ROAE_BB)

#PARA O SANTANDER
ROAE_SANTANDER <- ROAE_BANCOINT %>%
  filter(Código %in% c("30379"))
dplyr::select(Data, Código, Instituição, ROAE)
ROAE_SANTANDER <- na.omit(ROAE_SANTANDER$ROAE)

#PARA O BRADESCO
ROAE_BRADESCO <- ROAE_BANCOINT %>%
  filter(Código == "10045") 
dplyr::select(Data, Código, Instituição, ROAE)
ROAE_BRADESCO <- na.omit(ROAE_BRADESCO$ROAE)

#PARA O CAIXA ECONOMICA FEDERAL
ROAE_CAIXA <- ROAE_BANCOINT %>%
  filter(Código %in% c("51626","360305"))
dplyr::select(Data, Código, Instituição, ROAE)
ROAE_CAIXA <- na.omit(ROAE_CAIXA$ROAE)

#PARA O ITAU
ROAE_ITAU <- ROAE_BANCOINT %>%
  filter(Código == "10069") %>%
  dplyr::select(Data, Código, Instituição.x, ROAE)
ROAE_ITAU <- na.omit(ROAE_ITAU$ROAE)

write_xlsx(ROAE_CAIXA, "ROAE_CAIXA.xlsx")

#--------ADF SELIC------

#Teste ADF para Taxa SELIC
#Coloquei em gráfico para verificar se havia constância e tendência
#O objetivo é rejeitar H0

plot(Taxa_SELIC_Trimestral$SELIC_Fim, type = "l", main = "Série temporal")
Df_Taxa_SELIC1 <- adf.test(Taxa_SELIC_Trimestral$SELIC_Fim)


summary(Df_Taxa_Selic)

#data:  Taxa_SELIC_Trimestral$SELIC_Fim
#Dickey-Fuller = -2.2807, Lag order = 4, p-value = 0.4604
#alternative hypothesis: stationary

"TEREMOS QUE TIRAR A PRIMEIRA DIFERENÇA"

Taxa_Selic_Trimestral_diff <- diff(Taxa_SELIC_Trimestral$SELIC_Fim)
adf.test(Taxa_Selic_Trimestral_diff)
summary(Taxa_Selic_Trimestral_diff)

#Para Bradesco e CAIXA e BRADESCO
Taxa_Selic_Trimestral_diff2 <- diff(diff(Taxa_SELIC_Trimestral$SELIC_Fim))


#Augmented Dickey-Fuller Test
#data:  Taxa_Selic_Trimestral_diff
#Dickey-Fuller = -4.6381, Lag order = 4, p-value = 0.01
#alternative hypothesis: stationary
#Rejeitamos H0

"Ajustando o data frame para ficar no mesmo df e com a mesma data"

Juntando_Taxa_Selic_Trimestral_diff <- data.frame(
  Data = Taxa_SELIC_Trimestral$Data[-1],
  Diff_SELIC = Taxa_Selic_Trimestral_diff)


#--------ADF BANCO DO BRASIL------

# Teste ADF para ROE DO BANCO DO BRASIL
Df_BB <- adf.test(ROAE_BB)
summary(Df_BB)

#Augmented Dickey-Fuller Test
#data:  ROAE_BB
#Dickey-Fuller = -2.6922, Lag order = 4, p-value = 0.29
#alternative hypothesis: stationary

#TEREMOS QUE TIRAR A PRIMEIRA DIFERENÇA

ROAE_BB_diff <- diff(ROAE_BB)
adf.test(ROAE_BB_diff)
Df_BB <- adf.test(ROAE_BB_diff)

#A série se tornou estacionária

#--------ADF CAIXA------


Df_CAIXA <- adf.test(ROAE_CAIXA)
summary(Df_CAIXA)

#Augmented Dickey-Fuller Test
#data:  ROAE_CAIXA
#Dickey-Fuller = -4.4536, Lag order = 4, p-value = 0.01
#alternative hypothesis: stationary


#--------ADF SANTANDER------

# Teste ADF para ROE DO SANTANDER
plot(ROAE_SANTANDER, type = "l", main = "Série temporal")

Df_SANTANDER <- adf.test(ROAE_SANTANDER)
summary(Df_SANTANDER)

#Augmented Dickey-Fuller Test
#data:  ROAE_SANTANDER
#Dickey-Fuller = -4.6468, Lag order = 4, p-value = 0.01
#alternative hypothesis: stationary

#--------ADF ITAÚ------

plot(ROAE_ITAU, type = "l", main = "Série temporal")

Df_Itaú <- adf.test(ROAE_ITAU)
summary(Df_Itaú)

#Augmented Dickey-Fuller Test
#data:  ROAE_ITAU
#Dickey-Fuller = -2.3395, Lag order = 4, p-value = 0.436
#alternative hypothesis: stationary
#Não rejeitamos H0

"TEREMOS QUE TIRAR A PRIMEIRA DIFERENÇA"
ROAE_ITAU_diff <- diff(ROAE_ITAU)
adf.test(ROAE_ITAU_diff)
Df_Itaú <- adf.test(ROE_ITAÚ_diff)

#adf.test(ROAE_ITAU_diff) : p-value smaller than printed p-value

Juntando_ROE_ITAÚ_diff <- data.frame(
  Data = ROAE_ITAU,
  Diff_ITAÚ = ROAE_ITAU_diff)

#--------ADF BRADESCO------

plot(ROE_BRADESCO$ROE, type = "l", main = "Série temporal")

Df_BRADESCO <- adf.test(ROAE_BRADESCO)
summary(Df_BRADESCO)

"TEREMOS QUE TIRAR A PRIMEIRA DIFERENÇA"
ROAE_BRADESCO_diff <- diff(ROAE_BRADESCO)
adf.test(ROAE_BRADESCO_diff)
Df_BRADESCO <- adf.test(ROE_BRADESCO_diff)

#adf.test(ROAE_BRADESCO_diff) : p-value smaller than printed p-value


#--------DETERMINANDO A ORDEM DE DEFASAGEM

lag_selection <- VARselect(cbind(Taxa_SELIC_Trimestral, ROE_BB), lag.max = 10, type = "const")

#DEFININDO A TAXA SELIC -> SC(n) 2
VARselect(Taxa_SELIC_Trimestral$SELIC_Fim) 
#AIC(n)  HQ(n)  SC(n) FPE(n) 
#4      4      2      4 

#DEFININDO DEFASAGEM PARA O SANTANDER -> SC(n) 1
VARselect(ROAE_SANTANDER)
#AIC(n)  HQ(n)  SC(n) FPE(n) 
#1      1      1      1 

#DEFININDO DEFASAGEM PARA O BANCO DO BRASIL -> SC(n) 2
VARselect(ROAE_BB_diff)
#AIC(n)  HQ(n)  SC(n) FPE(n) 
#1      1      1      1 

#DEFININDO DEFASAGEM PARA O CAIXA -> SC(n) 1
VARselect(ROAE_CAIXA)
#AIC(n)  HQ(n)  SC(n) FPE(n) 
#1      1      1      1 

#DEFININDO DEFASAGEM PARA O ITAU -> SC(n) 3
VARselect(ROAE_ITAU)
#AIC(n)  HQ(n)  SC(n) FPE(n) 
#6      3      3      6 

#DEFININDO DEFASAGEM PARA O BRADESCO -> SC(n) 1
VARselect(ROAE_BRADESCO_diff)
#AIC(n)  HQ(n)  SC(n) FPE(n) 
#2      2      1      2 

#--------VAR DO BANCO DO BRASIL-----------

"VAR PARA O BANCO DO BRASIL"
VAR_BB <- VAR(cbind(ROAE = ROAE_BB_diff, 
                    SELIC = Taxa_Selic_Trimestral_diff2), p = 1,  type = "const")
summary(VAR_BB)

#Estimated coefficients for equation ROAE: 
#ROAE = ROAE.l1 + SELIC.l1 + const 

#ROAE.l1     SELIC.l1        const 
#-0.386452640 -0.002076452  0.001047630 

causality(VAR_BB, cause = "SELIC")



#--------VAR DO SANTANDER---------

"VAR PARA O SANTANDER"
VAR_SANTANDER <- VAR(cbind(ROE = ROAE_SANTANDER, 
                           SELIC = Taxa_Selic_Trimestral_diff), p = 1,   type = "const")
summary(VAR_SANTANDER)

#Estimated coefficients for equation ROE: 

#Call:
#ROE = ROE.l1 + SELIC.l1 + const 
#ROE.l1    SELIC.l1       const 
#-0.04090665  0.01066176  0.03530340 

causality(VAR_SANTANDER, cause = "SELIC")


#--------VAR DO ITAÚ--------

"VAR PARA O ITAU"
VAR_ITAU <- VAR(cbind(ROAE = ROAE_ITAU_diff, 
                      SELIC = Taxa_Selic_Trimestral_diff), p = 3,   type = "const")
summary(VAR_ITAU)

#Estimated coefficients for equation ROAE: 
#ROAE = ROAE.l1 + SELIC.l1 + ROAE.l2 + SELIC.l2 + ROAE.l3 + SELIC.l3 + const 

#ROAE.l1      SELIC.l1       ROAE.l2      SELIC.l2       ROAE.l3      SELIC.l3 
#-0.8208522088 -0.0035377997 -0.4882218831  0.0005408694 -0.1003746627 -0.0006920628 
#const 
#0.0000126933 

causality(VAR_ITAU, cause = "SELIC")

#--------VAR DO CAIXA--------

"VAR PARA O CAIXA"
VAR_CAIXA <- VAR(cbind(ROAE = ROAE_CAIXA, 
                       SELIC = Taxa_Selic_Trimestral_diff2), p = 1, type = "const")
summary(VAR_CAIXA)

#VAR Estimation Results:

#Estimated coefficients for equation ROE: 
#ROAE = ROAE.l1 + SELIC.l1 + const 

#ROAE.l1   SELIC.l1      const 
#0.05376312 0.02155808 0.07498142 

#Há significância estatísticas nesse modelo. Entretanto, o aumento de 1 p.p na selic eleva o ROAE da caixa em 2.15%.

causality(VAR_CAIXA, cause = "SELIC")

#--------VAR DO BRADESCO--------

"VAR PARA O BRADESCO"
VAR_BRADESCO <- VAR(cbind(ROE = ROAE_BRADESCO_diff,
                          SELIC = Taxa_Selic_Trimestral_diff2), p = 1,type = "const")
summary(VAR_BRADESCO)

#VAR Estimation Results:

#Estimated coefficients for equation ROE: 
#Call:
#ROE = ROE.l1 + SELIC.l1 + const 

#ROE.l1   SELIC.l1      const 
#0.05376312 0.02155808 0.07498142 

causality(VAR_BRADESCO, cause = "SELIC")
