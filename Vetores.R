library(dplyr)
library(GetDFPData2)
library(lubridate)

#----CRIANDO VETOR PARA INDENTIFICAR QUAIS BANCOS ESTÃO NO DF--------

#Quem está em PL e também em LL, ou seja, quem estará no DF ROE
Banco_PresentesPL <- intersect(Patrimonio_Liquido$DENOM_CIA,Lucro_Liquido$DENOM_CIA)

#[1] "BCO ABC BRASIL S.A."                    "BCO PINE S.A"                          
#[3] "BCO SANTANDER (BRASIL) S.A."            "BANCO RCI BRASIL S.A."                 
#[5] "PARANA BCO S.A."                        "BCO BRASIL S.A."                       
#[7] "BRB BANCO DE BRASILIA S.A."             "BCO MERCANTIL DO BRASIL S.A."          
#[9] "BANESTES S.A. - BCO EST ESPIRITO SANTO" "BCO PAN S.A."                          
#[11] "BCO BRADESCO S.A."                      "BCO DAYCOVAL S.A."                     
#[13] "BCO ESTADO DO RIO GRANDE DO SUL S.A." 

#Quem está em PL e não está em LL
Bancos_ForaPL <- setdiff(Patrimonio_Liquido$DENOM_CIA,Lucro_Liquido$DENOM_CIA)

#[1] "ITAU UNIBANCO HOLDING S.A." "BANCO BMG S/" "BCO BTG PACTUAL S.A." 

