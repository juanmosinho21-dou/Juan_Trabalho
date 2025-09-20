library(GetDFPData2)
library(dplyr)
#------------------Puxando dados do Bancos-----------------
#23 bancos trabalhados

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
  select(DENOM_COMERC, CD_CVM )

#------------------Puxando por códigos---------------------

library(GetDFPData2)
Resultados_Contábeis <- get_dfp_data(
  companies_cvm_codes = CD_BANCOS$CD_CVM,
  first_year = 2021,
  last_year = lubridate::year(Sys.Date()),
  type_docs = c("BPA", "BPP", "DRE"),
  type_format = c("con", "ind"),
  clean_data = TRUE,
  use_memoise = FALSE,
  cache_folder = "gdfpd2_cache",
  do_shiny_progress = FALSE
)

names(Resultados_Contábeis)
#------------------Indentificando LL e PL---------------------
#Filtrei o LL e o PL
Lucro_Liquido <- Resultados_Contábeis[["DF Individual - Demonstração do Resultado"]] %>%
  filter(grepl("3.11", CD_CONTA, ignore.case = TRUE))


Patrimonio_Liquido <- Resultados_Contábeis[["DF Individual - Balanço Patrimonial Passivo"]] %>%
  filter(grepl("Patrimônio Líquido", DS_CONTA, ignore.case = TRUE))

LL_PL <- inner_join(Lucro_Liquido, Patrimonio_Liquido, by = "DT_REFER")

#------------------ROE----------------------------------------
#Tentativa 2
LL_PL <- Lucro_Liquido %>%
  inner_join(
    Patrimonio_Liquido,
    by = c("DT_REFER", "DENOM_CIA"),
    suffix = c(".LL", ".PL")         
  )

ROE <- LL_PL %>%
  mutate(ROE = (VL_CONTA.LL / VL_CONTA.PL) * 100) %>%  
  select(DT_REFER, DENOM_CIA, ROE)

#RESUMO: O PACOTE TEM APENAS OS DADOS DE FINAL DE ANO, NÃO TEM TRIMESTRAL.

#----------------MARGEM FINANCEIRA----------------------------------------

#atividade para sexta 
#Encontrar os seguinte índices: o Retorno sobre ativos (ROA- Return on Assets), o Retorno sobre
#o Patrimônio Líquido (ROE- Return on Equity), a Receita Líquida de Juros (NII- Net
#Interest Income) e o Resultado Líquido de intermediação Financeira (NIM- Net interest
#Margin)." "FAZER DADOS EM PAINEL"

#Se possível
#tamanho do banco (representada pelo total de
#ativos), capitalização (medida pela razão patrimônio líquido sobre total de ativos- PLA), custos administrativos, 
#liquidez e qualidade dos créditos."

