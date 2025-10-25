#-------OBJETIVOS DO TRABALHO------------
#1 - Medir a receita com TVM ✅
#2 - Medir indicadores (ROA/ROE)✅
#3 - Medir SPREAD bancário 
#-------INFORMAÇÃOES---------
#Estou depois do teste de dick-fuller, as séries são estácionarias

#------PUXANDO O ARQUIVO EM ZIP----------
# Caminho do ZIP
zip_path <- "C:/Users/juanm/Downloads/arquivos renomeados.zip"

# Lista arquivos dentro do ZIP
arquivos <- unzip(zip_path, list = TRUE)$Name
csv_files <- arquivos[grepl("\\.csv$", arquivos)]

IF_DATA_BACEN <- lapply(csv_files, function(file){read.csv2(unz(zip_path, file), encoding = "UTF-8")})

# Nomeia a lista
names(IF_DATA_BACEN) <- basename(csv_files)
#---------TESTANDO OUTRA FORMA-------------
Passivo <- IF_DATA_BACEN[grep("Passivo", names(IF_DATA_BACEN))]
Ativo <- IF_DATA_BACEN[grep("Ativo", names(IF_DATA_BACEN))]
DRE <- IF_DATA_BACEN[grep("Dem_Resultado", names(IF_DATA_BACEN))]
#--------FILTRANDO O PATRIMONIO LIQUIDO----------
Patrimônio_Líquido <- bind_rows(Passivo) %>%
  filter(TCB == "b1" & Código %in% AT_6$Código) %>%  
  select(Instituição, Código, Data, `Patrimônio.Líquido..j.`)
Patrimônio_Líquido <- Patrimônio_Líquido %>%         
  rename(
    Patrimônio_Líquido = `Patrimônio.Líquido..j.`)

#--------FILTRANDO O LUCRO LIQUIDO----------
Lucro_Líquido <- bind_rows(DRE) %>%
  filter(TCB == "b1" & Código %in% AT_6$Código) %>%  
  select(Instituição, Código, Data, `Lucro.Líquido..j.....g.....h.....i.`)
Lucro_Líquido <- Lucro_Líquido %>%         
  rename(
    Lucro_Líquido = `Lucro.Líquido..j.....g.....h.....i.`)

#----------MEDINDO OS NÚMEROS----------

Número_dataPL <- Patrimônio_Líquido %>%
  group_by(Instituição) %>%
  summarise(n_datas = n_distinct(Data))

Número_dataLL <- Lucro_Líquido %>%
  group_by(Instituição) %>%
  summarise(n_datas = n_distinct(Data))

#--------FILTRANDO O ATIVO TOTAL---------
Ativo_Total <- bind_rows(Ativo) %>%
  filter(TCB == "b1" & (TC == 1 | TC == 2)) %>%  # filtra TCB e TC
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

AT_6 <- Ativo_Total %>%
  mutate(Ativo_Total = as.numeric(gsub("\\.", "", Ativo_Total))) %>%  
  filter(!is.na(Instituição)) %>%                                   
  group_by(TCB, Código, Instituição) %>%                             
  summarise(media_ativo = mean(Ativo_Total, na.rm = TRUE)) %>%      
  arrange(desc(media_ativo)) %>%
  head(6)

#-----------CÁLCULANDO O ROE---------------
Lucro_Líquido <- Lucro_Líquido %>%
  mutate(Lucro_Líquido = as.numeric(gsub(",", ".", gsub("\\.", "", Lucro_Líquido))))

Patrimônio_Líquido <- Patrimônio_Líquido %>%
  mutate(Patrimônio_Líquido = as.numeric(gsub(",", ".", gsub("\\.", "", Patrimônio_Líquido))))

ROE <- Lucro_Líquido %>%
  left_join(Patrimônio_Líquido, by = c("Instituição", "Data")) %>%
  mutate((ROE = Lucro_Líquido/Patrimônio_Líquido) * 100)
  
ROE <- ROE %>%
    select(-Lucro_Líquido,Patrimônio_Líquido,Código.y)
ROE <- ROE %>%
  select(Data, Instituição, ROE)

Número_ROE <- ROE %>%
  group_by(Instituição) %>%
  summarise(n_datas = n_distinct(Data))

#-----------TAXA SELIC------------
install.packages("GetBCBData")
library(GetBCBData)
library(xts)

Taxa_SELIC <- gbcbd_get_series(
  id = 432, # Taxa Selic anualizada base 252
  first.date = Sys.Date() - 25 * 365,
  last.date = Sys.Date(),
  format.data = "long",
  be.quiet = FALSE
)

#Colocando em trimestre
library(lubridate)
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

#---------GRÁFICO ROE x SELIC---------

Dados_Combinados <- ROE %>%
  inner_join(Taxa_SELIC_Trimestral, by = "Data")

ggplot(Dados_Combinados, aes(x = as.Date(paste0("01/", Data), format = "%d/%m/%Y"))) +
  geom_line(aes(y = SELIC_Fim, color = "SELIC"), size = 1.2) +
  geom_line(aes(y = ROE * 100, color = "ROE"), size = 1.2) +
  scale_color_manual(values = c("ROE" = "blue", "SELIC" = "red")) +
  scale_y_continuous(
    name = "Percentual (%)",
    labels = label_number(suffix = "%")
  ) +
  labs(
    title = "Evolução Trimestral do ROE e da Taxa SELIC",
    x = "Ano",
    color = "Variável"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "top"
  )

#-----------TESTE DE DICKEY-FULLER----------
install.packages("urca")
library(urca)

VAR_Dados <- Dados_Combinados %>%
  select(ROE, SELIC_Fim) %>%
  na.omit()

# Teste ADF para ROE
summary(ur.df(VAR_Dados$ROE, type = "drift", selectlags = "AIC"))
# Teste ADF para SELIC
summary(ur.df(VAR_Dados$SELIC_Fim, type = "drift", selectlags = "AIC"))

#------------Augmented Dickey-Fuller Test Unit Root Test-------------- 
#Call:
#lm(formula = z.diff ~ z.lag.1 + 1 + z.diff.lag)

#Residuals:
#Min      1Q  Median      3Q     Max 
#-9.3564 -0.2301 -0.0023  0.2611  8.5421 

#Coefficients:
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)    0.71605    0.20424   3.506  0.000498 ***
#z.lag.1     -0.06074    0.01571  -3.866  0.000126 ***
#z.diff.lag   0.03017    0.04585   0.658  0.510791    

#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 1.707 on 475 degrees of freedom
#Multiple R-squared:  0.03051,	Adjusted R-squared:  0.02643 
#F-statistic: 7.473 on 2 and 475 DF,  p-value: 0.0006373

#RESULTADO
#Value of test-statistic is: -3.8661 7.4886 
#Critical values for test statistics: 
  
#  1pct  5pct 10pct
#tau2 -3.44 -2.87 -2.57
#phi1  6.47  4.61  3.79

#---------TESTEANDO VAR---------
library(tseries)
library(vars)

install.packages("vars")
install.packages("tseries")
adf.test(VAR_Dados$ROE)
adf.test(VAR_Dados$SELIC_Fim)

# Verificar número ótimo de defasagens
VARselect(VAR_Dados, lag.max = 10, type = "const")

# Suponha que o melhor seja p = 2
VAR_Ajustado <- VAR(VAR_Dados, p = 2, type = "const")

# Resumo do modelo
summary(VAR_Ajustado)

VAR_dados_diff <- diff(as.matrix(VAR_Dados))

# Resumo do modelo
summary(VAR_Ajustado)
VAR_dados_diff <- as.data.frame(VAR_dados_diff)
names(VAR_dados_diff) <- c("ROE","SELIC_Fim")

# Analise os resultados da regressão
summary(VAR_Ajustado)

