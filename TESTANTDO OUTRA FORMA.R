library(GetDFPData2)
library(dplyr)
install.packages("remotes")
library(remotes)

#------------------Puxando dados do Bancos-----------------

#Busquei informações dentro do pacote
Empresas_CVM <- get_info_companies()

#Dexei só as empresas com registro ativo
Empresas_CVM <- Empresas_CVM %>%
  filter(SIT_REG == "ATIVO") 

#Filtrei só por bancos
BANCOS <- Empresas_CVM %>%
  filter(SETOR_ATIV %in% ("Bancos")) %>%
  select(CNPJ, DENOM_COMERC, CD_CVM, SETOR_ATIV)

#Agora vou filtrar só por código 
CD_BANCOS <- Empresas_CVM %>%
  filter(SETOR_ATIV %in% ("Bancos")) %>%
  select(DENOM_COMERC, CD_CVM)

#------------------Puxando por códigos---------------------

library(GetDFPData2)
Resultados_Contábeis <- GetDFPData2:::get_itr_data(
  companies_cvm_codes = BANCOS$CD_CVM,
  first_year = 2000,
  last_year = lubridate::year(Sys.Date()),
  type_docs = c("BPA", "BPP", "DRE"),
  type_format = ("con"),
  clean_data = TRUE,
  use_memoise = FALSE,
  cache_folder = "gdfpd2_cache"
)
names(Resultados_Contábeis)

#------------------Indentificando LL e PL---------------------

#PARA PL
#Filtrei o LL e o PL
Lucro_Liquido1 <- Resultados_Contábeis[["DF Consolidado - Demonstração do Resultado"]] %>%
  filter(CD_CONTA == "3.11")

Lucro_Liquido2 <- Resultados_Contábeis[["DF Consolidado - Demonstração do Resultado"]] %>%
  filter(CD_CONTA == "3.09")

#Retirando "Lucro ou Prejuízo antes das Participações e Contribuições Estatutárias"
Removendo_rúbrica <- Lucro_Liquido2 %>%
   filter((!DS_CONTA %in% "Lucro ou Prejuízo antes das Participações e Contribuições Estatutárias"))

#Juntando os 3 dfs
Lucro_Liquido <- bind_rows(Lucro_Liquido1,Lucro_Liquido2,Removendo_rúbrica) %>%
distinct()

Lucro_Liquido <- Lucro_Liquido %>% 
  select(-VERSAO, -GRUPO_DFP, -MOEDA, -ESCALA_MOEDA, -COLUNA_DF, -source_file, -quarter)


#PARA PL
Patrimonio_Liquido <- Resultados_Contábeis[["DF Consolidado - Balanço Patrimonial Passivo"]] %>%
  filter(grepl("Patrimônio Líquido Consolidado", DS_CONTA, ignore.case = TRUE))

Patrimonio_Liquido <- Patrimonio_Liquido %>% 
  select(-VERSAO, -GRUPO_DFP, -MOEDA, -ESCALA_MOEDA, -COLUNA_DF, -source_file, -quarter)


#------------------ROE----------------------------------------

LL_PL <- Lucro_Liquido %>%
  inner_join(
    Patrimonio_Liquido,
    by = c("DT_REFER", "DENOM_CIA"),
    suffix = c(".LL", ".PL")         
  )
  
ROE <- LL_PL %>%
  mutate(ROE = (VL_CONTA.LL / VL_CONTA.PL) * 100) %>% 
  select(DT_REFER, DENOM_CIA, ROE) %>%
  distinct()

colnames(ROE) <- c("Data", "Nome", "ROE")

library(writexl)
write_xlsx(Lucro_Liquido, "Lucro_Liquido.xlsx")
#---------------ROA-------------------------------------------

Lucro_Liquido <- Resultados_Contábeis[["DF Individual - Demonstração do Resultado"]] %>%
  filter(grepl("3.11", CD_CONTA, ignore.case = TRUE))

LL_ROA <- Lucro_Liquido %>%
  inner_join(
    Ativo_total,
    by = c("DT_REFER", "DENOM_CIA"),
    suffix = c(".LL", ".AT")         
  )
ROA <- LL_ROA %>%
  mutate(ROA = (VL_CONTA.LL / VL_CONTA.AT) * 100) %>%  
  select(DT_REFER, DENOM_CIA, ROA) %>%
  distinct()

#------------------Identification AT---------------------
Ativo_total <- Resultados_Contábeis[["DF Consolidado - Balanço Patrimonial Ativo"]] %>%
  filter(grepl("Ativo Total", DS_CONTA, ignore.case = TRUE))

Ativo_total <- Ativo_total %>%
  select(DT_REFER,DENOM_CIA,VL_CONTA)

#---------------------Painel----------------

# Usando data.frame
Dados_Painel <- data.frame(
  Data = ROE$DT_REFER,
  Cia = ROA$DENOM_CIA,
  ROA = ROA$ROA,
  ROE = ROE$ROE
)

#----------------RESULTADO FINANCEIRO----------------------------------------

library(dplyr)
#Tem que tratar, há uma rúbrica passando e é necessário ver o motivo
Resultado_Intermediação <- Resultados_Contábeis[["DF Individual - Demonstração do Resultado"]] %>%
  filter(grepl("3.03", CD_CONTA, ignore.case = TRUE))

Resultado_Intermediação <- Resultado_Intermediação %>%
  select(DT_REFER, DENOM_CIA,VL_CONTA)


#---------------GRÁFICO-------------------


