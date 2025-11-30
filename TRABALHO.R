
install.packages("GetDFPData2")
#--------------------ITAÚ--------------------------
library(GetDFPData2)
 Itaú <- get_dfp_data(
  companies_cvm_codes = 19348,
  first_year = 2010,
  last_year = c("BPA, BPP, DRE"),
  type_format = c("con", "ind"),
  clean_data = TRUE,
  use_memoise = FALSE,
  cache_folder = "gdfpd2_cache",
  do_shiny_progress = FALSE
)
 
names(Itaú)
#Criando o  - PL, aqui está puxando também o LPA 
Lucro_Liquido <- Itaú[["DF Individual - Demonstração do Resultado"]] %>%
  filter(grepl("lucro", DS_CONTA, ignore.case = TRUE) |
           grepl("prejuízo no período", DS_CONTA, ignore.case = TRUE))

#Vou apagar as linhhas que estão com o LPA
Lucro_Liquido <- Lucro_Liquido[Lucro_Liquido[, "DS_CONTA"] != "Lucro por Ação - (R$ / Ação)", ]

#Criando o df PL
Patrimonio_Liquido <- Itaú[["DF Individual - Balanço Patrimonial Passivo"]] %>%
  filter(grepl("patrimônio líquido", DS_CONTA, ignore.case = TRUE))

LL_PL <- inner_join(Lucro_Liquido, Patrimonio_Liquido, by = "DT_REFER")

# Realizar a divisão e criar uma nova coluna
ROE <- LL_PL %>%
  mutate(ROE = (Lucro_Liquido$VL_CONTA / Patrimonio_Liquido$VL_CONTA) * 100) %>%
  select(DT_REFER, ROE)

#-----------------BRADESCO------------------------
library(GetDFPData2)
Bradesco <- get_dfp_data(
  companies_cvm_codes = 906,
  first_year = 2010,
  last_year = c("BPA, BPP, DRE"),
  type_format = c("con", "ind"),
  clean_data = TRUE,
  use_memoise = FALSE,
  cache_folder = "gdfpd2_cache",
  do_shiny_progress = FALSE
)
names(Bradesco)

#-----------------BTG------------------------
library(GetDFPData2)
BTG <- get_dfp_data(
  companies_cvm_codes = 22616,
  first_year = 2010,
  last_year = c("BPA, BPP, DRE"),
  type_format = c("con", "ind"),
  clean_data = TRUE,
  use_memoise = FALSE,
  cache_folder = "gdfpd2_cache",
  do_shiny_progress = FALSE
)
names(BTG)


