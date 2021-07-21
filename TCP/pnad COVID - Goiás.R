
library(srvyr); library(tidyverse); library(vroom); library(readxl); library(patchwork);
library(ggrepel)
install.packages("rio")
library(rio)

options(scipen = 999)
setwd("~/LAPEI/PNAD covid")

# Bases  ------------------------------------------------------------------


trabalhadores <- read_excel("trabalhadores.xlsx")
escolaridade <- read_excel("trabalhadores.xlsx",sheet = "Planilha1")
mot_afast <- read_excel("trabalhadores.xlsx", 
                        sheet = "afastamento", col_types = c("numeric", 
                                                             "text"))

pnad_covid_maio <- vroom("PNAD_COVID_052020.csv")
pnad_covid_agosto <- vroom("PNAD_COVID_082020.csv")
pnad_covid_nov <- vroom("PNAD_COVID_112020.csv")

trabalho = c("04","06","07")


# Tratamentos -------------------------------------------------------------

## Tratamento maio

pnad_com_peso_maio <- pnad_covid_maio %>%
  select(Ano, UF, V1032, Estrato, UPA, A002,
         A003, A004, A005, B0031, B0033, B0034, B0041, 
         B0042, B0043, B0044, B0045, B0046, B005, B006, B007, 
         C001, C002, C003, C007, C007C, C008, C009, C01012, C013, D0013, D0023,
         D0033, D0051, D0053, D0063, D0073,CAPITAL,RM_RIDE) %>%
  rename(idade = A002, sexo = A003, cor = A004, escolaridade = A005, 
         fica_casa = B0031, automedicamento = B0033, remedio_orientacao = B0034, 
         apoio_posto = B0041, apoio_upa = B0042, apoio_hospital_sus = B0043,
         amb_privado = B0044, ps_privado = B0045, hospital_privado = B0046, 
         internacao = B005, entubacao = B006, plano_saude = B007, 
         trabalhou = C001, afastado = C002, 
         motivo_afastamento = C003, tipo_trabalho = C007, funcao_trabalho = C007C,
         horas_trabalhava = C008, horas_trabalhou = C009, 
         rendimentos = C01012, teletrabalho = C013, rendimento_aposentadoria = D0013,
         rendimento_pensao = D0023, rendimento_pbf = D0033, recebeu_auxilio = D0051,
         rendimento_auxilio = D0053, rendimento_seguro = D0063, outros_rendimentos = D0073) %>% 
  mutate(idade = as.numeric(idade), horas_trabalhava = as.numeric(horas_trabalhava),
         horas_trabalhou = as.numeric(horas_trabalhou), rendimentos = as.numeric(rendimentos),
         rendimento_aposentadoria = as.numeric(rendimento_aposentadoria),
         rendimento_pensao = as.numeric(rendimento_pensao),
         rendimento_pbf = as.numeric(rendimento_pbf),
         rendimento_auxilio = as.numeric(rendimento_auxilio),
         rendimento_seguro = as.numeric(rendimento_seguro),
         outros_rendimentos = as.numeric(outros_rendimentos)) %>% 
  as_survey_design(ids = UPA, strata = Estrato, weights = V1032, nest = TRUE) %>% 
  filter(tipo_trabalho %in% trabalho)

#teste <- (pnad_com_peso_maio[["variables"]])

# analises maio -------------------------------------------------------

# caracteriza??o dos usu?rios ---------------------------------------------
# quantidades por tipo de trabalho

tipotrabalho_maio <- pnad_com_peso_maio %>% 
  filter(UF == "52") %>%
  group_by(tipo_trabalho,sexo) %>% 
  summarise(proportion = survey_mean(),
            total = survey_total()) %>% 
  mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                   tipo_trabalho == "06" ~ "Empregador",
                                   tipo_trabalho == "07" ~ "Trabalhador por conta propria"),
         sexo = case_when(sexo == "1" ~ "Masculino",
                          sexo == "2" ~ "Feminino")) %>% 
  mutate(mes = "maio")


# media de idade por sexo e tipo de trabalho

tipotrabalho_sexo_maio <- pnad_com_peso_maio %>%
  filter(UF == "52") %>%
  group_by(tipo_trabalho,sexo) %>%
  summarise(proportion_sexo = survey_mean(),
            total = survey_total()) %>% 
  mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                   tipo_trabalho == "06" ~ "Empregador",
                                   tipo_trabalho == "07" ~ "Trabalhador por conta propria"),
         sexo = case_when(sexo == "1" ~ "Masculino",
                          sexo == "2" ~ "Feminino")) %>% 
  mutate(mes = "maio")

# media de idade por sexo e tipo de trabalho

tipotrabalho_sexo_idade_maio <- pnad_com_peso_maio %>%
  filter(UF == "52") %>%
  group_by(tipo_trabalho,sexo) %>%
  summarise(idade = survey_mean(idade, vartype = "ci")) %>% 
  mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                   tipo_trabalho == "06" ~ "Empregador",
                                   tipo_trabalho == "07" ~ "Trabalhador por conta propria"),
         sexo = case_when(sexo == "1" ~ "Masculino",
                          sexo == "2" ~ "Feminino")) %>% 
  mutate(mes = "maio")


# escolaridade por tipo de trabalho

tipotrabalho_escolaridade_maio <- pnad_com_peso_maio %>%
  filter(UF == "52") %>%
  group_by(tipo_trabalho, sexo, escolaridade) %>%
  summarise(prop_escolaridade = survey_mean(),
            total_escolaridade = survey_total()) %>% 
  mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                   tipo_trabalho == "06" ~ "Empregador",
                                   tipo_trabalho == "07" ~ "Trabalhador por conta propria"),
          sexo = case_when(sexo == "1" ~ "Masculino",
                           sexo == "2" ~ "Feminino")) %>% 
mutate(mes = "maio")



join_escolaridade_maio <- tipotrabalho_escolaridade_maio %>% 
  left_join(escolaridade, by = c("escolaridade"="id")) %>% 
  mutate(ate_medio = if_else(escolaridade <= 5, "ate medio completo", "Superior Parcial ou mais")) %>% 
  group_by(tipo_trabalho, ate_medio,sexo) %>% 
  summarise(prop = sum(prop_escolaridade),
            total = sum(total_escolaridade)) %>% 
  mutate(mes = "maio")


# quantidades por tipo de trabalho e fun??es 

tipotrabalho_funcao_maio <- pnad_com_peso_maio %>%
  filter(UF == "52") %>%
  group_by(tipo_trabalho, sexo, funcao_trabalho) %>% 
  summarise(proportion = survey_mean(),
            total = survey_total()) %>% 
  mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                   tipo_trabalho == "06" ~ "Empregador",
                                   tipo_trabalho == "07" ~ "Trabalhador por conta propria"),
         sexo = case_when(sexo == "1" ~ "Masculino",
                          sexo == "2" ~ "Feminino")) %>% 
  left_join(trabalhadores, by = c("funcao_trabalho"="id")) %>% 
  mutate(mes = "maio")


top_funcoes_maio <- tipotrabalho_funcao_maio %>% 
  group_by(tipo_trabalho) %>% 
  slice_max(order_by = proportion, n = 10)%>%
mutate(mes = "maio")

# Renda 

tipotrabalho_renda_maio <- pnad_com_peso_maio %>%
  filter(UF == "52") %>%
  group_by(tipo_trabalho,sexo) %>%
  summarise(renda = survey_mean(rendimentos, na.rm = TRUE)) %>% 
  mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                   tipo_trabalho == "06" ~ "Empregador",
                                   tipo_trabalho == "07" ~ "Trabalhador por conta propria"),
         sexo = case_when(sexo == "1" ~ "Masculino",
                          sexo == "2" ~ "Feminino")) %>% 
  mutate(mes = "maio")


# Apoio ao sentir sintomas 
local_buscou_apoio_maio <- pnad_com_peso_maio %>%
  filter(UF == "52") %>%
  group_by(tipo_trabalho, sexo, apoio_posto) %>%
  summarise(prop_escolaridade = survey_mean(),
            total_escolaridade = survey_total()) %>% 
  mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                   tipo_trabalho == "06" ~ "Empregador",
                                   tipo_trabalho == "07" ~ "Trabalhador por conta propria"),
         sexo = case_when(sexo == "1" ~ "Masculino",
                          sexo == "2" ~ "Feminino"),
         apoio_posto = case_when(apoio_posto == "1" ~ "Sim",
                                 apoio_posto == "2" ~ "Nao",
                                 apoio_posto == "9" ~ "Ignorado")) %>% 
  mutate(mes = "maio")


# horas trabalhadas e horas de fato trabalhadas

tipotrabalho_horas_maio <- pnad_com_peso_maio %>%
  filter(UF == "52") %>%
  group_by(tipo_trabalho,sexo) %>%
  summarise(horas_trabalhava = survey_mean(horas_trabalhava, na.rm = TRUE),
            horas_trabalhou = survey_mean(horas_trabalhou, na.rm = TRUE)) %>% 
  mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                   tipo_trabalho == "06" ~ "Empregador",
                                   tipo_trabalho == "07" ~ "Trabalhador por conta propria"),
         sexo = case_when(sexo == "1" ~ "Masculino",
                          sexo == "2" ~ "Feminino"))%>% 
  mutate(mes = "maio")


tipo_trabalho_horas_maio <- tipotrabalho_horas_maio %>%
  select(tipo_trabalho, horas_trabalhava, 
         horas_trabalhou,sexo) %>% 
  gather(key = "trabalho", value = "horas", 
         horas_trabalhava, horas_trabalhou) %>% 
  mutate(mes = "maio")

#motivos para afastamento 
afastamento_maio <- pnad_com_peso_maio %>% 
  filter(UF == "52") %>%
  mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                   tipo_trabalho == "06" ~ "Empregador",
                                   tipo_trabalho == "07" ~ "Trabalhador por conta propria"),
         sexo = case_when(sexo == "1" ~ "Masculino",
                          sexo == "2" ~ "Feminino"),
         afastado = case_when(afastado == "1" ~ "Sim",
                              afastado == "2" ~ "Nao",
                              afastado == "NA" ~ "Não aplicável"))%>% 
  group_by(tipo_trabalho, sexo, afastado) %>% 
  summarise(afastado_prop = survey_mean(),
            afastado_total = survey_total()) %>% 
  mutate(mes = "maio")

motivo_afastamento_maio <- pnad_com_peso_maio %>%
  filter(UF == "52") %>%
  filter(afastado == 1) %>% 
  group_by(tipo_trabalho, sexo, motivo_afastamento) %>% 
  summarise(afastado_prop = survey_mean(),
            afastado_total = survey_total()) %>% 
  mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                   tipo_trabalho == "06" ~ "Empregador",
                                   tipo_trabalho == "07" ~ "Trabalhador por conta propria"),
         sexo = case_when(sexo == "1" ~ "Masculino",
                          sexo == "2" ~ "Feminino"))%>%
  left_join(mot_afast, by = c("motivo_afastamento" = "id")) %>% 
  mutate(afastado_prop = 100 * afastado_prop,
         afastado_prop = round(afastado_prop,2),
         mes = "maio")


# quantidades por tipo e auxilio


tipotrabalho_auxilio_maio <- pnad_com_peso_maio %>%
  filter(UF == "52") %>%
  group_by(tipo_trabalho, sexo, recebeu_auxilio) %>% 
  summarise(proportion = survey_mean(),
            total = survey_total()) %>% 
  mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                   tipo_trabalho == "06" ~ "Empregador",
                                   tipo_trabalho == "07" ~ "Trabalhador por conta propria"),
         sexo = case_when(sexo == "1" ~ "Masculino",
                          sexo == "2" ~ "Feminino"),
         recebeu_auxilio = case_when(recebeu_auxilio == "1" ~ "Sim",
                                     recebeu_auxilio == "2" ~ "Nao"))%>% 
  mutate(mes = "maio") 




# media de valores de aux?lio por tipo de trabalho 

tipotrabalho_rendimento_auxilio_maio <- pnad_com_peso_maio %>%
  filter(UF == "52") %>%
  group_by(tipo_trabalho,sexo) %>%
  summarise(rendimento = survey_mean(rendimento_auxilio, vartype = "ci", na.rm=TRUE)) %>% 
  mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                   tipo_trabalho == "06" ~ "Empregador",
                                   tipo_trabalho == "07" ~ "Trabalhador por conta propria"),
         sexo = case_when(sexo == "1" ~ "Masculino",
                          sexo == "2" ~ "Feminino"))%>% 
  mutate(mes = "maio")


# teletrabalho 

teletrabalho_maio <- pnad_com_peso_maio %>%
  filter(UF == "52") %>%
  group_by(tipo_trabalho, sexo, teletrabalho) %>% 
  summarise(proportion = survey_mean(),
            total = survey_total()) %>% 
  mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                   tipo_trabalho == "06" ~ "Empregador",
                                   tipo_trabalho == "07" ~ "Trabalhador por conta propria"),
         sexo = case_when(sexo == "1" ~ "Masculino",
                          sexo == "2" ~ "Feminino"),
         teletrabalho = case_when(teletrabalho == "1" ~ "Sim",
                                  teletrabalho == "2" ~ "Nao"))%>% 
  mutate(mes = "maio") 



teletrabalho2_maio <- pnad_com_peso_maio %>%
  filter(UF == "52") %>%
  group_by(tipo_trabalho,sexo) %>% 
  summarise(total_homeoffice = survey_total(teletrabalho == 1, na.rm = TRUE),
            total_trabalho = survey_total(trabalhou == 1, na.rm = TRUE)) %>% 
  mutate(trab_home = total_homeoffice/total_trabalho) %>% 
  mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                   tipo_trabalho == "06" ~ "Empregador",
                                   tipo_trabalho == "07" ~ "Trabalhador por conta propria"),
         sexo = case_when(sexo == "1" ~ "Masculino",
                          sexo == "2" ~ "Feminino")) %>% 
  mutate(mes = "maio")


# Empr?stimos (VARI?VEL INDISPON?VEL)


# Plano de saude 

plano_saude_maio <- pnad_com_peso_maio %>%
  filter(UF == "52") %>%
  group_by(tipo_trabalho, sexo, plano_saude) %>% 
  summarise(proportion = survey_mean(),
            total = survey_total()) %>% 
  mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                   tipo_trabalho == "06" ~ "Empregador",
                                   tipo_trabalho == "07" ~ "Trabalhador por conta propria"),
         sexo = case_when(sexo == "1" ~ "Masculino",
                          sexo == "2" ~ "Feminino"),
         plano_saude = case_when(plano_saude == "1" ~ "Sim",
                                 plano_saude == "2" ~ "Nao",
                                 plano_saude == "9" ~ "Ignorado")) %>% 
  mutate(mes = "maio") 


# teste (VARI?VEL INDISPON?VEL)


# itens de higiene (VARI?VEIS INDISPON?VEIS)


# Visuais  ----------------------------------------------------------------

# quantidades por tipo de trabalho

tipotrabalho_maio %>%  
  ggplot(aes(fct_reorder(tipo_trabalho,total,sexo), total, fill = tipo_trabalho)) + 
  geom_col() + coord_flip() + theme_minimal() + xlab("Tipo de trabalhador")


# escolaridade
join_escolaridade_maio %>% 
  mutate(prop = round(prop,4)) %>% 
  ggplot(aes(tipo_trabalho,prop,fill= ate_medio)) + geom_col(position = "dodge") +
  geom_label(aes(label = prop, vjust = -0.4), 
             position = position_dodge(width = 0.9)) +
  scale_y_continuous(breaks = seq(0,1,0.05)) + theme_minimal() 


# renda 
tipotrabalho_renda_maio %>% 
  ggplot(aes(tipo_trabalho, renda, fill=sexo)) + geom_col(position = "dodge") + 
  scale_y_continuous(breaks = seq(0,6500,500)) + theme_minimal()


# top_funcoes
top_empregador_maio <- top_funcoes_maio %>% 
  filter(tipo_trabalho == "Empregador") %>%  
  mutate(proporcao = proportion * 100) %>% 
  ggplot(aes(x = fct_reorder(descricao,proporcao), y = proporcao)) + 
  geom_col() + coord_flip() + ggtitle("Empregador") + ylab("proporção (em %)") + 
  xlab(" ") + scale_y_continuous(breaks = seq(0,50,2.5)) + guides(fill= "none") + 
  theme_minimal()

top_empregado_maio <- top_funcoes_maio %>% 
  filter(tipo_trabalho == "Empregado") %>%  
  mutate(proporcao = proportion * 100) %>% 
  ggplot(aes(x = fct_reorder(descricao,proporcao), y = proporcao)) + 
  geom_col() + coord_flip() + ggtitle("Empregado") + ylab("proporção (em %)") + 
  xlab(" ") + scale_y_continuous(breaks = seq(0,50,2.5)) + guides(fill= "none") + 
  theme_minimal()

top_tpcp_maio <- top_funcoes_maio %>% 
  filter(tipo_trabalho == "Trabalhador por conta propria") %>%  
  mutate(proporcao = proportion * 100) %>% 
  ggplot(aes(x = fct_reorder(descricao,proporcao), y = proporcao)) + 
  geom_col() + coord_flip() + ggtitle("Trabalhador por conta propria") + ylab("proporção (em %)") + 
  xlab(" ") + scale_y_continuous(breaks = seq(0,50,2.5)) + guides(fill= "none") + 
  theme_minimal()


top_tpcp_maio / top_empregado_maio / top_empregador_maio

# afastamento e motivo 

afastamento_maio %>% 
  mutate(afastado = if_else(afastado == 1, "afastado","nao se aplica")) %>% 
  ggplot(aes(tipo_trabalho, afastado_prop, fill = afastado)) + 
  geom_col(position = "dodge") + scale_y_continuous(breaks = seq(0,1,0.025)) +
  theme_minimal() 

motivo_afastamento_maio %>% 
  ggplot(aes(motivo, afastado_prop)) + 
  geom_col(position = "dodge") + geom_label(aes(label = afastado_prop), hjust = -0.02) +
  coord_flip() + theme_minimal() +
  facet_grid(~tipo_trabalho)

# horas trabalhadas e trabalhou

tipotrabalho_horas_maio %>% 
  ggplot(aes(tipo_trabalho, horas, fill = trabalho)) + geom_col(position = "dodge") + 
  theme_minimal() + scale_y_continuous(breaks = seq(0,50,2.5))


# Recebeu auxilio 

tipotrabalho_rendimento_auxilio_maio %>% 
  mutate(auxilio = case_when(recebeu_auxilio == 1 ~ "Sim",
                             recebeu_auxilio == 2 ~ "Nao")) %>% 
  ggplot(aes(x=tipo_trabalho, y = proportion, fill = auxilio)) + geom_col() + 
  coord_flip() + ggtitle("Proporções de beneficiários de auxílio") + theme_minimal() +
  xlab("Categoria de trabalho") + ylab("Proporcoes (%)") + 
  scale_y_continuous(breaks = seq(0,1,0.05))


# teletrabalho 

teletrabalho_maio %>%  
  mutate(teletrabalho = case_when(teletrabalho == "1" ~ "Sim",
                                  teletrabalho == "2" ~ "Nao",
                                  teletrabalho == "NA" ~ "Nao se aplica")) %>% 
  ggplot(aes(tipo_trabalho, proportion, fill = teletrabalho)) + 
  geom_col(position = "dodge") + theme_minimal() + 
  geom_text(aes(label = round(proportion,2)), position = position_dodge(width=0.9),
            vjust=-0.5)


# m?dia de idade por sexo e tipo de trabalho

tipotrabalho_sexo_idade_maio %>% 
  ggplot(aes(x=tipo_trabalho, y = idade, fill=sexo)) + geom_col(position = "dodge") +
  geom_text(aes(label = round(idade,2)), vjust=-0.5, position = position_dodge2(width=1)) +
  scale_y_continuous(breaks = seq(0,50,5)) + theme_minimal() 

# Empr?stimo (VARI?VEL INDISPON?VEL)


# Plano de sa?de

plano_saude_maio %>%  
  filter(plano_saude != "9") %>% 
  mutate(proporcao = 100 * proportion) %>% 
  mutate(plano_saude = case_when(plano_saude == 1 ~ "Sim",
                                 plano_saude == 2 ~ "Nao")) %>% 
  ggplot(aes(tipo_trabalho, round(proporcao,2), fill = plano_saude)) + 
  geom_col(position = "dodge") + theme_minimal() + coord_flip() + 
  geom_text(aes(label = round(proporcao,1)), position = position_dodge(width=0.9),
            hjust=-0.25) + ylab("Proporção (em %)") + xlab("Categoria profissional")


# Teste covid (VARI?VEL INDISPON?VEL)


## Tratamento Agosto

pnad_com_peso_agosto <- pnad_covid_agosto %>% 
  select(Ano, UF, V1032, Estrato, UPA, A002,
         A003, A004, A005, B0031, B0033, B0034, B0041, 
         B0042, B0043, B0044, B0045, B0046, B005, B006, B007, B008, 
         C001, C002, C003, C007, C007C, C008, C009, C01012, C013, D0013, D0023,
         D0033, D0051, D0053, D0063, D0073, E001, F002A1, F002A2,
         F002A2, F002A3, F002A4, F002A5,CAPITAL,RM_RIDE) %>%
  rename(idade = A002, sexo = A003, cor = A004, escolaridade = A005, 
         fica_casa = B0031, automedicamento = B0033, remedio_orientacao = B0034, 
         apoio_posto = B0041, apoio_upa = B0042, apoio_hospital_sus = B0043,
         amb_privado = B0044, ps_privado = B0045, hospital_privado = B0046, 
         internacao = B005, entubacao = B006, plano_saude = B007, 
         teste = B008, trabalhou = C001, afastado = C002, 
         motivo_afastamento = C003, tipo_trabalho = C007, funcao_trabalho = C007C,
         horas_trabalhava = C008, horas_trabalhou = C009, 
         rendimentos = C01012, teletrabalho = C013, rendimento_aposentadoria = D0013,
         rendimento_pensao = D0023, rendimento_pbf = D0033, recebeu_auxilio = D0051,
         rendimento_auxilio = D0053, rendimento_seguro = D0063, outros_rendimentos = D0073, 
         emprestimo = E001, sabao = F002A1, alcool = F002A2, mascara = F002A3, luvas = F002A4, 
         agua_sanitaria = F002A5) %>% 
  mutate(idade = as.numeric(idade), horas_trabalhava = as.numeric(horas_trabalhava),
         horas_trabalhou = as.numeric(horas_trabalhou), rendimentos = as.numeric(rendimentos),
         rendimento_aposentadoria = as.numeric(rendimento_aposentadoria),
         rendimento_pensao = as.numeric(rendimento_pensao),
         rendimento_pbf = as.numeric(rendimento_pbf),
         rendimento_auxilio = as.numeric(rendimento_auxilio),
         rendimento_seguro = as.numeric(rendimento_seguro),
         outros_rendimentos = as.numeric(outros_rendimentos)) %>% 
  as_survey_design(ids = UPA, strata = Estrato, weights = V1032, nest = TRUE) %>% 
  filter(tipo_trabalho %in% trabalho)

#teste <- (pnad_com_peso_agosto[["variables"]])


# analises agosto -------------------------------------------------------

# caracteriza??o dos usu?rios ---------------------------------------------
# quantidades por tipo de trabalho
tipotrabalho_agosto <- pnad_com_peso_agosto %>%
  filter(UF == "52") %>%
  group_by(tipo_trabalho,sexo) %>% 
  summarise(proportion = survey_mean(),
            total = survey_total()) %>% 
  mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                   tipo_trabalho == "06" ~ "Empregador",
                                   tipo_trabalho == "07" ~ "Trabalhador por conta propria"),
         sexo = case_when(sexo == "1" ~ "Masculino",
                          sexo == "2" ~ "Feminino"))%>% 
  mutate(mes = "agosto")


# m?dia de idade por sexo e tipo de trabalho

tipotrabalho_sexo_agosto <- pnad_com_peso_agosto %>%
  filter(UF == "52") %>%
  group_by(tipo_trabalho,sexo) %>%
  summarise(proportion_sexo = survey_mean(),
            total = survey_total()) %>% 
  mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                   tipo_trabalho == "06" ~ "Empregador",
                                   tipo_trabalho == "07" ~ "Trabalhador por conta propria"),
         sexo = case_when(sexo == "1" ~ "Masculino",
                          sexo == "2" ~ "Feminino")) %>% 
  mutate(mes = "agosto")

# m?dia de idade por sexo e tipo de trabalho

tipotrabalho_sexo_idade_agosto <- pnad_com_peso_agosto %>%
  filter(UF == "52") %>%
  group_by(tipo_trabalho,sexo) %>%
  summarise(idade = survey_mean(idade, vartype = "ci")) %>% 
  mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                   tipo_trabalho == "06" ~ "Empregador",
                                   tipo_trabalho == "07" ~ "Trabalhador por conta propria"),
         sexo = case_when(sexo == "1" ~ "Masculino",
                          sexo == "2" ~ "Feminino")) %>% 
  mutate(mes = "agosto")


# escolaridade por tipo de trabalho

tipotrabalho_escolaridade_agosto <- pnad_com_peso_agosto %>%
  filter(UF == "52") %>%
  group_by(tipo_trabalho, sexo, escolaridade) %>%
  summarise(prop_escolaridade = survey_mean(),
            total_escolaridade = survey_total()) %>% 
  mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                   tipo_trabalho == "06" ~ "Empregador",
                                   tipo_trabalho == "07" ~ "Trabalhador por conta propria"),
         sexo = case_when(sexo == "1" ~ "Masculino",
                          sexo == "2" ~ "Feminino"))%>% 
         mutate(mes = "agosto")

join_escolaridade_agosto <- tipotrabalho_escolaridade_agosto %>% 
  left_join(escolaridade, by = c("escolaridade"="id")) %>% 
  mutate(ate_medio = if_else(escolaridade <= 5, "Ate Medio completo", "Superior Parcial ou mais")) %>% 
  group_by(tipo_trabalho, sexo, ate_medio) %>% 
  summarise(prop = sum(prop_escolaridade),
            total = sum(total_escolaridade)) %>% 
  mutate(mes = "agosto")


# quantidades por tipo de trabalho e fun??es 

tipotrabalho_funcao_agosto <- pnad_com_peso_agosto %>%
  filter(UF == "52") %>%
  group_by(tipo_trabalho, sexo, funcao_trabalho) %>% 
  summarise(proportion = survey_mean(),
            total = survey_total()) %>% 
  mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                   tipo_trabalho == "06" ~ "Empregador",
                                   tipo_trabalho == "07" ~ "Trabalhador por conta propria"),
         sexo = case_when(sexo == "1" ~ "Masculino",
                          sexo == "2" ~ "Feminino"))%>% 
  left_join(trabalhadores, by = c("funcao_trabalho"="id")) %>% 
  mutate(mes = "agosto")


top_funcoes_agosto <- tipotrabalho_funcao_agosto %>%
  group_by(tipo_trabalho) %>% 
  slice_max(order_by = proportion, n = 10)%>%
mutate(mes = "agosto")

# Renda 

tipotrabalho_renda_agosto <- pnad_com_peso_agosto %>%
  filter(UF == "52") %>%
  group_by(tipo_trabalho,sexo) %>%
  summarise(renda = survey_mean(rendimentos, na.rm = TRUE)) %>% 
  mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                   tipo_trabalho == "06" ~ "Empregador",
                                   tipo_trabalho == "07" ~ "Trabalhador por conta propria"),
         sexo = case_when(sexo == "1" ~ "Masculino",
                          sexo == "2" ~ "Feminino")) %>% 
  mutate(mes = "agosto")


# Apoio ao sentir sintomas 
local_buscou_apoio_agosto <- pnad_com_peso_agosto %>%
  filter(UF == "52") %>%
  group_by(tipo_trabalho, sexo, apoio_posto) %>%
  summarise(prop_escolaridade = survey_mean(),
            total_escolaridade = survey_total()) %>% 
  mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                   tipo_trabalho == "06" ~ "Empregador",
                                   tipo_trabalho == "07" ~ "Trabalhador por conta propria"),
         sexo = case_when(sexo == "1" ~ "Masculino",
                          sexo == "2" ~ "Feminino"),
         apoio_posto = case_when(apoio_posto == "1" ~ "Sim",
                                 apoio_posto == "2" ~ "Nao",
                                 apoio_posto == "9" ~ "Ignorado")) %>% 
  mutate(mes = "agosto")


# horas trabalhadas e horas de fato trabalhadas

tipotrabalho_horas_agosto <- pnad_com_peso_agosto %>%
  filter(UF == "52") %>%
  group_by(tipo_trabalho,sexo) %>%
  summarise(horas_trabalhava = survey_mean(horas_trabalhava, na.rm = TRUE),
            horas_trabalhou = survey_mean(horas_trabalhou, na.rm = TRUE)) %>% 
  mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                   tipo_trabalho == "06" ~ "Empregador",
                                   tipo_trabalho == "07" ~ "Trabalhador por conta propria"),
         sexo = case_when(sexo == "1" ~ "Masculino",
                          sexo == "2" ~ "Feminino"))%>% 
  mutate(mes = "agosto")


tipo_trabalho_horas_agosto <- tipotrabalho_horas_agosto %>%
  select(tipo_trabalho, horas_trabalhava, 
         horas_trabalhou,sexo) %>% 
  gather(key = "trabalho", value = "horas", 
         horas_trabalhava, horas_trabalhou) %>% 
  mutate(mes = "agosto")




#motivos para afastamento 
afastamento_agosto <- pnad_com_peso_agosto %>%
  filter(UF == "52") %>%
  group_by(tipo_trabalho, sexo, afastado) %>% 
  summarise(afastado_prop = survey_mean(),
            afastado_total = survey_total()) %>% 
  mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                   tipo_trabalho == "06" ~ "Empregador",
                                   tipo_trabalho == "07" ~ "Trabalhador por conta propria"),
         sexo = case_when(sexo == "1" ~ "Masculino",
                          sexo == "2" ~ "Feminino"),
         afastado = case_when(afastado == "1" ~ "Sim",
                              afastado == "2" ~ "Nao"))%>% 
  mutate(mes = "agosto")

motivo_afastamento_agosto <- pnad_com_peso_agosto %>%
  filter(UF == "52") %>%
  filter(afastado == 1) %>% 
  group_by(tipo_trabalho, sexo, motivo_afastamento) %>% 
  summarise(afastado_prop = survey_mean(),
            afastado_total = survey_total()) %>% 
  mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                   tipo_trabalho == "06" ~ "Empregador",
                                   tipo_trabalho == "07" ~ "Trabalhador por conta propria"),
         sexo = case_when(sexo == "1" ~ "Masculino",
                          sexo == "2" ~ "Feminino")) %>%
  left_join(mot_afast, by = c("motivo_afastamento" = "id")) %>% 
  mutate(afastado_prop = 100 * afastado_prop,
         afastado_prop = round(afastado_prop,2),
         mes = "agosto")


# quantidades por tipo e auxilio


tipotrabalho_auxilio_agosto <- pnad_com_peso_agosto %>%
  filter(UF == "52") %>%
  group_by(tipo_trabalho, sexo, recebeu_auxilio) %>% 
  summarise(proportion = survey_mean(),
            total = survey_total()) %>% 
  mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                   tipo_trabalho == "06" ~ "Empregador",
                                   tipo_trabalho == "07" ~ "Trabalhador por conta propria"),
         sexo = case_when(sexo == "1" ~ "Masculino",
                          sexo == "2" ~ "Feminino"),
         recebeu_auxilio = case_when(recebeu_auxilio == "1" ~ "Sim",
                                     recebeu_auxilio == "2" ~ "Nao"))%>% 
  mutate(mes = "agosto") 




# m?dia de valores de aux?lio por tipo de trabalho 

tipotrabalho_rendimento_auxilio_agosto <- pnad_com_peso_agosto %>%
  filter(UF == "52") %>%
  group_by(tipo_trabalho,sexo) %>%
  summarise(rendimento = survey_mean(rendimento_auxilio, vartype = "ci", na.rm=TRUE)) %>% 
  mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                   tipo_trabalho == "06" ~ "Empregador",
                                   tipo_trabalho == "07" ~ "Trabalhador por conta propria"),
         sexo = case_when(sexo == "1" ~ "Masculino",
                          sexo == "2" ~ "Feminino"))%>% 
  mutate(mes = "agosto")


# teletrabalho 

teletrabalho_agosto <- pnad_com_peso_agosto %>%
  filter(UF == "52") %>%
  group_by(tipo_trabalho, sexo, teletrabalho) %>% 
  summarise(proportion = survey_mean(),
            total = survey_total()) %>% 
  mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                   tipo_trabalho == "06" ~ "Empregador",
                                   tipo_trabalho == "07" ~ "Trabalhador por conta propria"),
         sexo = case_when(sexo == "1" ~ "Masculino",
                          sexo == "2" ~ "Feminino"),
         teletrabalho = case_when(teletrabalho == "1" ~ "Sim",
                                  teletrabalho == "2" ~ "Nao"))%>% 
  mutate(mes = "agosto") 



teletrabalho2_agosto <- pnad_com_peso_agosto %>%
  filter(UF == "52") %>%
  group_by(tipo_trabalho,sexo) %>% 
  summarise(total_homeoffice = survey_total(teletrabalho == 1, na.rm = TRUE),
            total_trabalho = survey_total(trabalhou == 1, na.rm = TRUE)) %>% 
  mutate(trab_home = total_homeoffice/total_trabalho) %>% 
  mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                   tipo_trabalho == "06" ~ "Empregador",
                                   tipo_trabalho == "07" ~ "Trabalhador por conta propria"),
         sexo = case_when(sexo == "1" ~ "Masculino",
                          sexo == "2" ~ "Feminino"))%>% 
  mutate(mes = "agosto")




# Empr?stimos 

emprestimos_agosto <- pnad_com_peso_agosto %>%
  filter(UF == "52") %>%
  group_by(tipo_trabalho, sexo, emprestimo) %>% 
  summarise(proportion = survey_mean(),
            total = survey_total()) %>% 
  mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                   tipo_trabalho == "06" ~ "Empregador",
                                   tipo_trabalho == "07" ~ "Trabalhador por conta propria"),
         sexo = case_when(sexo == "1" ~ "Masculino",
                          sexo == "2" ~ "Feminino"),
         emprestimo = case_when( emprestimo == "1" ~	"Sim, e pelo menos um morador conseguiu", 
                                 emprestimo == "2"	~ "Sim, mas nenhum morador conseguiu",
                                 emprestimo == "3" ~ "Nao solicitou"))%>% 
  mutate(mes = "agosto") 


# Plano de saude 

plano_saude_agosto <- pnad_com_peso_agosto %>%
  filter(UF == "52") %>%
  group_by(tipo_trabalho, sexo, plano_saude) %>% 
  summarise(proportion = survey_mean(),
            total = survey_total()) %>% 
  mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                   tipo_trabalho == "06" ~ "Empregador",
                                   tipo_trabalho == "07" ~ "Trabalhador por conta propria"),
         sexo = case_when(sexo == "1" ~ "Masculino",
                          sexo == "2" ~ "Feminino"),
         plano_saude = case_when(plano_saude == "1" ~ "Sim",
                                 plano_saude == "2" ~ "Nao",
                                 plano_saude == "9" ~ "Ignorado"))%>% 
  mutate(mes = "agosto") 


# teste

teste_covid_agosto <- pnad_com_peso_agosto %>%
  filter(UF == "52") %>%
  group_by(tipo_trabalho, sexo, teste) %>% 
  summarise(proportion = survey_mean(),
            total = survey_total()) %>% 
  mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                   tipo_trabalho == "06" ~ "Empregador",
                                   tipo_trabalho == "07" ~ "Trabalhador por conta propria"),
         sexo = case_when(sexo == "1" ~ "Masculino",
                          sexo == "2" ~ "Feminino"),
        teste = case_when(teste == "1" ~ "Sim",
                          teste == "2" ~ "Não",
                          teste == "9" ~ "Ignorado"))%>% 
  mutate(mes = "agosto") 


# itens de higiene 

mascara_agosto <- pnad_com_peso_agosto %>%
  filter(UF == "52") %>%
  group_by(tipo_trabalho, sexo, mascara) %>% 
  summarise(proportion = survey_mean(),
            total = survey_total()) %>% 
  mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                   tipo_trabalho == "06" ~ "Empregador",
                                   tipo_trabalho == "07" ~ "Trabalhador por conta propria"),
         sexo = case_when(sexo == "1" ~ "Masculino",
                          sexo == "2" ~ "Feminino"),
         mascara = case_when(mascara == "1" ~ "Sim",
                             mascara == "2" ~ "Nao",
                             mascara == "3" ~ "Nao sabe")) %>% 
  mutate(mes = "agosto")


alcool_agosto <- pnad_com_peso_agosto %>%
  filter(UF == "52") %>%
  group_by(tipo_trabalho, sexo, alcool) %>% 
  summarise(proportion = survey_mean(),
            total = survey_total()) %>% 
  mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                   tipo_trabalho == "06" ~ "Empregador",
                                   tipo_trabalho == "07" ~ "Trabalhador por conta propria"),
         sexo = case_when(sexo == "1" ~ "Masculino",
                          sexo == "2" ~ "Feminino"),
         alcool = case_when(alcool == "1" ~ "Sim",
                            alcool == "2" ~ "Nao",
                            alcool == "3" ~ "Nao sabe")) %>% 
   mutate(mes = "agosto")

# Visuais  ----------------------------------------------------------------

# quantidades por tipo de trabalho

tipotrabalho_agosto %>%  
  ggplot(aes(fct_reorder(tipo_trabalho,total), total, fill = tipo_trabalho)) + 
  geom_col() + coord_flip() + theme_minimal() + xlab("Tipo de trabalhador")

# escolaridade
join_escolaridade_agosto %>% 
  mutate(prop = round(prop,4)) %>% 
  ggplot(aes(tipo_trabalho,prop,fill= ate_medio)) + geom_col(position = "dodge") +
  geom_label(aes(label = prop, vjust = -0.4), 
             position = position_dodge(width = 0.9)) +
  scale_y_continuous(breaks = seq(0,1,0.05)) + theme_minimal() 


# renda 
tipotrabalho_renda_agosto %>% 
  ggplot(aes(tipo_trabalho, renda, fill=sexo)) + geom_col(position = "dodge") + 
  scale_y_continuous(breaks = seq(0,6500,500)) + theme_minimal()


# top_funcoes
top_empregador_agosto <- top_funcoes_agosto %>% 
  filter(tipo_trabalho == "Empregador") %>%  
  mutate(proporcao = proportion * 100) %>% 
  ggplot(aes(x = fct_reorder(descricao,proporcao), y = proporcao)) + 
  geom_col() + coord_flip() + ggtitle("Empregador") + ylab("proporção (em %)") + 
  xlab(" ") + scale_y_continuous(breaks = seq(0,50,2.5)) + guides(fill= "none") + 
  theme_minimal()

top_empregado_agosto <- top_funcoes_agosto %>% 
  filter(tipo_trabalho == "Empregado") %>%  
  mutate(proporcao = proportion * 100) %>% 
  ggplot(aes(x = fct_reorder(descricao,proporcao), y = proporcao)) + 
  geom_col() + coord_flip() + ggtitle("Empregado") + ylab("proporção (em %)") + 
  xlab(" ") + scale_y_continuous(breaks = seq(0,50,2.5)) + guides(fill= "none") + 
  theme_minimal()

top_tpcp_agosto <- top_funcoes_agosto %>% 
  filter(tipo_trabalho == "Trabalhador por conta propria") %>%  
  mutate(proporcao = proportion * 100) %>% 
  ggplot(aes(x = fct_reorder(descricao,proporcao), y = proporcao)) + 
  geom_col() + coord_flip() + ggtitle("Trabalhador por conta propria") + ylab("proporção (em %)") + 
  xlab(" ") + scale_y_continuous(breaks = seq(0,50,2.5)) + guides(fill= "none") + 
  theme_minimal()


top_tpcp_agosto / top_empregado_agosto / top_empregador_agosto

# afastamento e motivo 

afastamento_agosto %>% 
  mutate(afastado = if_else(afastado == 1, "afastado","nao se aplica")) %>% 
  ggplot(aes(tipo_trabalho, afastado_prop, fill = afastado)) + 
  geom_col(position = "dodge") + scale_y_continuous(breaks = seq(0,1,0.025)) +
  theme_minimal() 

motivo_afastamento_agosto %>% 
  ggplot(aes(motivo, afastado_prop)) + 
  geom_col(position = "dodge") + geom_label(aes(label = afastado_prop), hjust = -0.02) +
  coord_flip() + theme_minimal() +
  facet_grid(~tipo_trabalho)

# horas trabalhadas e trabalhou

tipotrabalho_horas_agosto %>% 
  ggplot(aes(tipo_trabalho, horas, fill = trabalho)) + geom_col(position = "dodge") + 
  theme_minimal() + scale_y_continuous(breaks = seq(0,50,2.5))


# Recebeu auxilio 

tipotrabalho_rendimento_auxilio_agosto %>% 
  mutate(auxilio = case_when(recebeu_auxilio == 1 ~ "Sim",
                             recebeu_auxilio == 2 ~ "Nao")) %>% 
  ggplot(aes(x=tipo_trabalho, y = proportion, fill = auxilio)) + geom_col() + 
  coord_flip() + ggtitle("Proporções de beneficiários de auxílio") + theme_minimal() +
  xlab("Categoria de trabalho") + ylab("Proporcoes (%)") + 
  scale_y_continuous(breaks = seq(0,1,0.05))


# teletrabalho 

teletrabalho_agosto %>%  
  mutate(teletrabalho = case_when(teletrabalho == "1" ~ "Sim",
                                  teletrabalho == "2" ~ "Nao",
                                  teletrabalho == "NA" ~ "Nao se aplica")) %>% 
  ggplot(aes(tipo_trabalho, proportion, fill = teletrabalho)) + 
  geom_col(position = "dodge") + theme_minimal() + 
  geom_text(aes(label = round(proportion,2)), position = position_dodge(width=0.9),
            vjust=-0.5)


# m?dia de idade por sexo e tipo de trabalho

tipotrabalho_sexo_idade_agosto %>% 
  ggplot(aes(x=tipo_trabalho, y = idade, fill=sexo)) + geom_col(position = "dodge") +
  geom_text(aes(label = round(idade,2)), vjust=-0.5, position = position_dodge2(width=1)) +
  scale_y_continuous(breaks = seq(0,50,5)) + theme_minimal() 

# Empr?stimo

emprestimos_agosto %>%  
  mutate(proporcao = 100 * proportion) %>% 
  mutate(emprestimo = case_when(emprestimo == "1" ~ "Sim, e pelo menos um morador conseguiu",
                                emprestimo == "2" ~ "Sim, mas nenhum morador conseguiu",
                                emprestimo == "3" ~ "N?o solicitou")) %>% 
  ggplot(aes(tipo_trabalho, round(proporcao,2), fill = emprestimo)) + 
  geom_col(position = "dodge") + theme_minimal() + coord_flip() +
  geom_text(aes(label = round(proporcao,1)), position = position_dodge(width=1),
            hjust=-0.25) + ylab("Propor??o (em %)") + xlab("Categoria profissional")

# Plano de sa?de

plano_saude_agosto %>%  
  filter(plano_saude != "9") %>% 
  mutate(proporcao = 100 * proportion) %>% 
  mutate(plano_saude = case_when(plano_saude == 1 ~ "Sim",
                                 plano_saude == 2 ~ "Nao")) %>% 
  ggplot(aes(tipo_trabalho, round(proporcao,2), fill = plano_saude)) + 
  geom_col(position = "dodge") + theme_minimal() + coord_flip() + 
  geom_text(aes(label = round(proporcao,1)), position = position_dodge(width=0.9),
            hjust=-0.25) + ylab("Propor??o (em %)") + xlab("Categoria profissional")


# Teste covid 

teste_covid_agosto %>%  
  mutate(proporcao = 100 * proportion) %>% 
  mutate(teste_covid = case_when(teste == 1 ~ "Sim",
                                 teste == 2 ~ "Nao")) %>% 
  ggplot(aes(tipo_trabalho, round(proporcao,2), fill = teste_covid)) + 
  geom_col(position = "dodge") + theme_minimal() + coord_flip() + 
  geom_text(aes(label = round(proporcao,1)), position = position_dodge(width=0.9),
            hjust=-0.25) + ylab("Proporção (em %)") + xlab("Categoria profissional")

## Tratamento Novembro

pnad_com_peso_nov <- pnad_covid_nov %>% 
                        select(Ano, UF, V1032, Estrato, UPA, A002,
                               A003, A004, A005, B0031, B0033, B0034, B0041, 
                               B0042, B0043, B0044, B0045, B0046, B005, B006, B007, B008, 
                               C001, C002, C003, C007, C007C, C008, C009, C01012, C013, D0013, D0023,
                               D0033, D0051, D0053, D0063, D0073, E001, F002A1, F002A2,
                               F002A2, F002A3, F002A4, F002A5,CAPITAL,RM_RIDE) %>%
                        rename(idade = A002, sexo = A003, cor = A004, escolaridade = A005, 
                               fica_casa = B0031, automedicamento = B0033, remedio_orientacao = B0034, 
                               apoio_posto = B0041, apoio_upa = B0042, apoio_hospital_sus = B0043,
                               amb_privado = B0044, ps_privado = B0045, hospital_privado = B0046, 
                               internacao = B005, entubacao = B006, plano_saude = B007, 
                               teste = B008, trabalhou = C001, afastado = C002, 
                               motivo_afastamento = C003, tipo_trabalho = C007, funcao_trabalho = C007C,
                               horas_trabalhava = C008, horas_trabalhou = C009, 
                               rendimentos = C01012, teletrabalho = C013, rendimento_aposentadoria = D0013,
                               rendimento_pensao = D0023, rendimento_pbf = D0033, recebeu_auxilio = D0051,
                               rendimento_auxilio = D0053, rendimento_seguro = D0063, outros_rendimentos = D0073, 
                               emprestimo = E001, sabao = F002A1, alcool = F002A2, mascara = F002A3, luvas = F002A4, 
                               agua_sanitaria = F002A5) %>% 
                        mutate(idade = as.numeric(idade), horas_trabalhava = as.numeric(horas_trabalhava),
                               horas_trabalhou = as.numeric(horas_trabalhou), rendimentos = as.numeric(rendimentos),
                               rendimento_aposentadoria = as.numeric(rendimento_aposentadoria),
                               rendimento_pensao = as.numeric(rendimento_pensao),
                               rendimento_pbf = as.numeric(rendimento_pbf),
                               rendimento_auxilio = as.numeric(rendimento_auxilio),
                               rendimento_seguro = as.numeric(rendimento_seguro),
                               outros_rendimentos = as.numeric(outros_rendimentos)) %>% 
                        as_survey_design(ids = UPA, strata = Estrato, weights = V1032, nest = TRUE) %>% 
                        filter(tipo_trabalho %in% trabalho)

#teste <- (pnad_com_peso_nov [["variables"]])


# analises Novembro -------------------------------------------------------

# caracteriza??o dos usu?rios ---------------------------------------------
# quantidades por tipo de trabalho
tipotrabalho_nov <- pnad_com_peso_nov %>%
  filter(UF == "52") %>%
  group_by(tipo_trabalho,sexo) %>% 
  summarise(proportion = survey_mean(),
            total = survey_total()) %>% 
  mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                   tipo_trabalho == "06" ~ "Empregador",
                                   tipo_trabalho == "07" ~ "Trabalhador por conta propria"),
         sexo = case_when(sexo == "1" ~ "Masculino",
                          sexo == "2" ~ "Feminino"))%>% 
  mutate(mes = "novembro")


# m?dia de idade por sexo e tipo de trabalho

tipotrabalho_sexo_nov <- pnad_com_peso_nov %>%
  filter(UF == "52") %>%
  group_by(tipo_trabalho,sexo) %>%
  summarise(proportion_sexo = survey_mean(),
            total = survey_total()) %>% 
                      mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                                       tipo_trabalho == "06" ~ "Empregador",
                                                       tipo_trabalho == "07" ~ "Trabalhador por conta propria"),
         sexo = case_when(sexo == "1" ~ "Masculino",
                          sexo == "2" ~ "Feminino")) %>% 
  mutate(mes = "novembro")

# m?dia de idade por sexo e tipo de trabalho

tipotrabalho_sexo_idade_nov <- pnad_com_peso_nov %>%
  filter(UF == "52") %>%
  group_by(tipo_trabalho,sexo) %>%
  summarise(idade = survey_mean(idade, vartype = "ci")) %>% 
  mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                   tipo_trabalho == "06" ~ "Empregador",
                                   tipo_trabalho == "07" ~ "Trabalhador por conta propria"),
         sexo = case_when(sexo == "1" ~ "Masculino",
                          sexo == "2" ~ "Feminino")) %>% 
  mutate(mes = "novembro")


# escolaridade por tipo de trabalho

tipotrabalho_escolaridade_nov <- pnad_com_peso_nov %>%
  filter(UF == "52") %>%
  group_by(tipo_trabalho, sexo, escolaridade) %>%
  summarise(prop_escolaridade = survey_mean(),
            total_escolaridade = survey_total()) %>% 
  mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                   tipo_trabalho == "06" ~ "Empregador",
                                   tipo_trabalho == "07" ~ "Trabalhador por conta propria"),
         sexo = case_when(sexo == "1" ~ "Masculino",
                          sexo == "2" ~ "Feminino"))

join_escolaridade_nov <- tipotrabalho_escolaridade_nov %>%
  left_join(escolaridade, by = c("escolaridade"="id")) %>% 
  mutate(ate_medio = if_else(escolaridade <= 5, "Ate Medio completo", "Superior Parcial ou mais")) %>% 
  group_by(tipo_trabalho, sexo, ate_medio) %>% 
  summarise(prop = sum(prop_escolaridade),
            total = sum(total_escolaridade)) %>% 
  mutate(mes = "novembro")


# quantidades por tipo de trabalho e fun??es 

tipotrabalho_funcao_nov <- pnad_com_peso_nov %>%
  filter(UF == "52") %>%
  group_by(tipo_trabalho, sexo, funcao_trabalho) %>% 
  summarise(proportion = survey_mean(),
            total = survey_total()) %>% 
  mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                   tipo_trabalho == "06" ~ "Empregador",
                                   tipo_trabalho == "07" ~ "Trabalhador por conta propria"),
         sexo = case_when(sexo == "1" ~ "Masculino",
                          sexo == "2" ~ "Feminino")) %>% 
  left_join(trabalhadores, by = c("funcao_trabalho"="id")) %>% 
  mutate(mes = "novembro")


top_funcoes_nov <- tipotrabalho_funcao_nov %>% 
  group_by(tipo_trabalho) %>% 
  slice_max(order_by = proportion, n = 10)

# Renda 

tipotrabalho_renda_nov <- pnad_com_peso_nov %>%
  filter(UF == "52") %>%
  group_by(tipo_trabalho,sexo) %>%
  summarise(renda = survey_mean(rendimentos, na.rm = TRUE)) %>% 
  mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                   tipo_trabalho == "06" ~ "Empregador",
                                   tipo_trabalho == "07" ~ "Trabalhador por conta propria"),
         sexo = case_when(sexo == "1" ~ "Masculino",
                          sexo == "2" ~ "Feminino")) %>% 
  mutate(mes = "novembro")


# Apoio ao sentir sintomas 
local_buscou_apoio_nov <- pnad_com_peso_nov %>%
  filter(UF == "52") %>%
  group_by(tipo_trabalho, sexo, apoio_posto) %>%
  summarise(prop_escolaridade = survey_mean(),
            total_escolaridade = survey_total()) %>% 
  mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                   tipo_trabalho == "06" ~ "Empregador",
                                   tipo_trabalho == "07" ~ "Trabalhador por conta propria"),
         sexo = case_when(sexo == "1" ~ "Masculino",
                          sexo == "2" ~ "Feminino"))%>%
  mutate(mes = "novembro")


# horas trabalhadas e horas de fato trabalhadas

tipotrabalho_horas_nov <- pnad_com_peso_nov %>%
  filter(UF == "52") %>%
  group_by(tipo_trabalho,sexo) %>%
  summarise(horas_trabalhava = survey_mean(horas_trabalhava, na.rm = TRUE),
            horas_trabalhou = survey_mean(horas_trabalhou, na.rm = TRUE)) %>% 
  mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                   tipo_trabalho == "06" ~ "Empregador",
                                   tipo_trabalho == "07" ~ "Trabalhador por conta propria"),
         sexo = case_when(sexo == "1" ~ "Masculino",
                          sexo == "2" ~ "Feminino")) %>% 
  mutate(mes = "novembro")


tipo_trabalho_horas_nov <- tipotrabalho_horas_nov %>% 
                          select(tipo_trabalho, horas_trabalhava, 
                                 horas_trabalhou,sexo) %>% 
                          gather(key = "trabalho", value = "horas", 
                                 horas_trabalhava, horas_trabalhou)%>% 
           mutate(mes = "novembro")




#motivos para afastamento 
afastamento_nov <- pnad_com_peso_nov %>%
  filter(UF == "52") %>%
  group_by(tipo_trabalho, sexo, afastado) %>% 
  summarise(afastado_prop = survey_mean(),
            afastado_total = survey_total()) %>% 
  mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                   tipo_trabalho == "06" ~ "Empregador",
                                   tipo_trabalho == "07" ~ "Trabalhador por conta propria"),
         sexo = case_when(sexo == "1" ~ "Masculino",
                          sexo == "2" ~ "Feminino"),
         afastado = case_when(afastado == "1" ~ "Sim",
                              afastado == "2" ~ "Nao"))%>% 
  mutate(mes = "novembro")

motivo_afastamento_nov <- pnad_com_peso_nov %>%
  filter(UF == "52") %>%
  filter(afastado == 1) %>% 
  group_by(tipo_trabalho, sexo, motivo_afastamento) %>% 
  summarise(afastado_prop = survey_mean(),
            afastado_total = survey_total()) %>% 
  mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                   tipo_trabalho == "06" ~ "Empregador",
                                   tipo_trabalho == "07" ~ "Trabalhador por conta propria"),
         sexo = case_when(sexo == "1" ~ "Masculino",
                          sexo == "2" ~ "Feminino")) %>%
  left_join(mot_afast, by = c("motivo_afastamento" = "id")) %>% 
  mutate(afastado_prop = 100 * afastado_prop,
         afastado_prop = round(afastado_prop,2),
         mes = "novembro")


# quantidades por tipo e auxilio


tipotrabalho_auxilio_nov <- pnad_com_peso_nov %>%
  filter(UF == "52") %>%
  group_by(tipo_trabalho, sexo, recebeu_auxilio) %>% 
  summarise(proportion = survey_mean(),
            total = survey_total()) %>% 
  mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                   tipo_trabalho == "06" ~ "Empregador",
                                   tipo_trabalho == "07" ~ "Trabalhador por conta propria"),
         sexo = case_when(sexo == "1" ~ "Masculino",
                          sexo == "2" ~ "Feminino"),
         recebeu_auxilio = case_when(recebeu_auxilio == "1" ~ "Sim",
                                     recebeu_auxilio == "2" ~ "Nao"))%>% 
  mutate(mes = "novembro") 



         
# m?dia de valores de aux?lio por tipo de trabalho 

tipotrabalho_rendimento_auxilio_nov <- pnad_com_peso_nov %>%
  filter(UF == "52") %>%
  group_by(tipo_trabalho,sexo) %>%
  summarise(rendimento = survey_mean(rendimento_auxilio, vartype = "ci", na.rm=TRUE)) %>% 
  mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                   tipo_trabalho == "06" ~ "Empregador",
                                   tipo_trabalho == "07" ~ "Trabalhador por conta propria"),
         sexo = case_when(sexo == "1" ~ "Masculino",
                          sexo == "2" ~ "Feminino")) %>% 
  mutate(mes = "novembro")


# teletrabalho 

teletrabalho_nov <- pnad_com_peso_nov %>%
  filter(UF == "52") %>%
  group_by(tipo_trabalho, sexo, teletrabalho) %>% 
  summarise(proportion = survey_mean(),
            total = survey_total()) %>% 
  mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                   tipo_trabalho == "06" ~ "Empregador",
                                   tipo_trabalho == "07" ~ "Trabalhador por conta propria"),
         sexo = case_when(sexo == "1" ~ "Masculino",
                          sexo == "2" ~ "Feminino"),
         teletrabalho = case_when(teletrabalho == "1" ~ "Sim",
                                  teletrabalho == "2" ~ "Nao"))%>% 
  mutate(mes = "novembro") 



teletrabalho2_nov <- pnad_com_peso_nov %>%
  filter(UF == "52") %>%
  group_by(tipo_trabalho,sexo) %>% 
  summarise(total_homeoffice = survey_total(teletrabalho == 1, na.rm = TRUE),
            total_trabalho = survey_total(trabalhou == 1, na.rm = TRUE)) %>% 
  mutate(trab_home = total_homeoffice/total_trabalho) %>% 
  mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                   tipo_trabalho == "06" ~ "Empregador",
                                   tipo_trabalho == "07" ~ "Trabalhador por conta propria"),
         sexo = case_when(sexo == "1" ~ "Masculino",
                          sexo == "2" ~ "Feminino"))%>%
  mutate(mes = "novembro")




# Empr?stimos 

emprestimos_nov <- pnad_com_peso_nov %>%
  filter(UF == "52") %>%
  group_by(tipo_trabalho, sexo, emprestimo) %>% 
  summarise(proportion = survey_mean(),
            total = survey_total()) %>% 
  mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                   tipo_trabalho == "06" ~ "Empregador",
                                   tipo_trabalho == "07" ~ "Trabalhador por conta propria"),
         sexo = case_when(sexo == "1" ~ "Masculino",
                          sexo == "2" ~ "Feminino"),
         emprestimo = case_when( emprestimo == "1" ~	"Sim, e pelo menos um morador conseguiu", 
                                  emprestimo == "2"	~ "Sim, mas nenhum morador conseguiu",
                                  emprestimo == "3" ~ "Nao solicitou"))%>% 
  mutate(mes = "novembro") 


# Plano de saude 

plano_saude_nov <- pnad_com_peso_nov %>%
  filter(UF == "52") %>%
  group_by(tipo_trabalho, sexo, plano_saude) %>% 
  summarise(proportion = survey_mean(),
            total = survey_total()) %>% 
  mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                   tipo_trabalho == "06" ~ "Empregador",
                                   tipo_trabalho == "07" ~ "Trabalhador por conta propria"),
         sexo = case_when(sexo == "1" ~ "Masculino",
                          sexo == "2" ~ "Feminino"),
         plano_saude = case_when(plano_saude == "1" ~ "Sim",
                        plano_saude == "2" ~ "Nao",
                        plano_saude == "9" ~ "Ignorado")) %>%
         
  mutate(mes = "novembro") 


# teste

teste_covid_nov <- pnad_com_peso_nov %>%
  filter(UF == "52") %>%
  group_by(tipo_trabalho, sexo, teste) %>% 
  summarise(proportion = survey_mean(),
            total = survey_total()) %>% 
  mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                   tipo_trabalho == "06" ~ "Empregador",
                                   tipo_trabalho == "07" ~ "Trabalhador por conta propria"),
         sexo = case_when(sexo == "1" ~ "Masculino",
                          sexo == "2" ~ "Feminino"),
         teste = case_when(teste == "1" ~ "Sim",
                           teste == "2" ~ "Não",
                           teste == "9" ~ "Ignorado"))%>% 
  mutate(mes = "novembro") 


# itens de higiene 

mascara_nov <- pnad_com_peso_nov %>%
  filter(UF == "52") %>%
  group_by(tipo_trabalho, sexo, mascara) %>% 
  summarise(proportion = survey_mean(),
            total = survey_total()) %>% 
  mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                   tipo_trabalho == "06" ~ "Empregador",
                                   tipo_trabalho == "07" ~ "Trabalhador por conta propria"),
         sexo = case_when(sexo == "1" ~ "Masculino",
                          sexo == "2" ~ "Feminino"),
         mascara = case_when(mascara == "1" ~ "Sim",
                             mascara == "2" ~ "Nao",
                             mascara == "3" ~ "Nao sabe")) %>%
  mutate(mes = "novembro")


alcool_nov <- pnad_com_peso_nov %>%
  filter(UF == "52") %>%
  group_by(tipo_trabalho, sexo, alcool) %>% 
  summarise(proportion = survey_mean(),
            total = survey_total()) %>% 
  mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                   tipo_trabalho == "06" ~ "Empregador",
                                   tipo_trabalho == "07" ~ "Trabalhador por conta propria"),
         sexo = case_when(sexo == "1" ~ "Masculino",
                          sexo == "2" ~ "Feminino"),
         alcool = case_when(alcool == "1" ~ "Sim",
                            alcool == "2" ~ "Nao",
                            alcool == "3" ~ "Nao sabe")) %>% 
  mutate(mes = "novembro")



# Visuais  ----------------------------------------------------------------

# quantidades por tipo de trabalho

tipotrabalho_nov %>%  
  ggplot(aes(fct_reorder(tipo_trabalho,total), total, fill = tipo_trabalho)) + 
  geom_col() + coord_flip() + theme_minimal() + xlab("Tipo de trabalhador")

# escolaridade
join_escolaridade_nov %>% 
  mutate(prop = round(prop,4)) %>% 
  ggplot(aes(tipo_trabalho,prop,fill= ate_medio)) + geom_col(position = "dodge") +
  geom_label(aes(label = prop, vjust = -0.4), 
             position = position_dodge(width = 0.9)) +
  scale_y_continuous(breaks = seq(0,1,0.05)) + theme_minimal() 
  

# renda 
tipotrabalho_renda_nov %>% 
  ggplot(aes(tipo_trabalho, renda, fill=sexo)) + geom_col(position = "dodge") + 
  scale_y_continuous(breaks = seq(0,6500,500)) + theme_minimal()


# top_funcoes
top_empregador_nov <- top_funcoes %>% 
  filter(tipo_trabalho == "Empregador") %>%  
  mutate(proporcao = proportion * 100) %>% 
  ggplot(aes(x = fct_reorder(descricao,proporcao), y = proporcao)) + 
  geom_col() + coord_flip() + ggtitle("Empregador") + ylab("proporção (em %)") + 
  xlab(" ") + scale_y_continuous(breaks = seq(0,50,2.5)) + guides(fill= "none") + 
  theme_minimal()

top_empregado_nov <- top_funcoes %>% 
  filter(tipo_trabalho == "Empregado") %>%  
  mutate(proporcao = proportion * 100) %>% 
  ggplot(aes(x = fct_reorder(descricao,proporcao), y = proporcao)) + 
  geom_col() + coord_flip() + ggtitle("Empregado") + ylab("propor??o (em %)") + 
  xlab(" ") + scale_y_continuous(breaks = seq(0,50,2.5)) + guides(fill= "none") + 
  theme_minimal()

top_tpcp_nov <- top_funcoes %>% 
  filter(tipo_trabalho == "Trabalhador por conta propria") %>%  
  mutate(proporcao = proportion * 100) %>% 
  ggplot(aes(x = fct_reorder(descricao,proporcao), y = proporcao)) + 
  geom_col() + coord_flip() + ggtitle("Trabalhador por conta pr?pria") + ylab("proporção (em %)") + 
  xlab(" ") + scale_y_continuous(breaks = seq(0,50,2.5)) + guides(fill= "none") + 
  theme_minimal()


top_tpcp_nov / top_empregado_nov / top_empregador_nov

# afastamento e motivo 

afastamento_nov %>% 
  mutate(afastado = if_else(afastado == 1, "afastado","não se aplica")) %>% 
  ggplot(aes(tipo_trabalho, afastado_prop, fill = afastado)) + 
  geom_col(position = "dodge") + scale_y_continuous(breaks = seq(0,1,0.025)) +
  theme_minimal() 

motivo_afastamento_nov %>% 
  ggplot(aes(motivo, afastado_prop)) + 
  geom_col(position = "dodge") + geom_label(aes(label = afastado_prop), hjust = -0.02) +
  coord_flip() + theme_minimal() +
  facet_grid(~tipo_trabalho)
  
# horas trabalhadas e trabalhou

tipotrabalho_horas_nov %>% 
  ggplot(aes(tipo_trabalho, horas_trabalhava, fill = trabalho)) + geom_col(position = "dodge") + 
  theme_minimal() + scale_y_continuous(breaks = seq(0,50,2.5))


# Recebeu auxilio 

tipotrabalho_rendimento_auxilio_nov %>% 
  mutate(auxilio = case_when(recebeu_auxilio == 1 ~ "Sim",
                             recebeu_auxilio == 2 ~ "Nao")) %>% 
  ggplot(aes(x=tipo_trabalho, y = proportion, fill = auxilio)) + geom_col() + 
  coord_flip() + ggtitle("Proporções de beneficiarios de auxilio") + theme_minimal() +
  xlab("Categoria de trabalho") + ylab("Proporcoes (%)") + 
  scale_y_continuous(breaks = seq(0,1,0.05))
 
  
# teletrabalho 

teletrabalho_nov %>%  
  mutate(teletrabalho = case_when(teletrabalho == "1" ~ "Sim",
                                  teletrabalho == "2" ~ "Nao",
                                  teletrabalho == "NA" ~ "Nao se aplica")) %>% 
  ggplot(aes(tipo_trabalho, proportion, fill = teletrabalho)) + 
  geom_col(position = "dodge") + theme_minimal() + 
  geom_text(aes(label = round(proportion,2)), position = position_dodge(width=0.9),
            vjust=-0.5)


# m?dia de idade por sexo e tipo de trabalho

tipotrabalho_sexo_idade_nov %>% 
  ggplot(aes(x=tipo_trabalho, y = idade, fill=sexo)) + geom_col(position = "dodge") +
  geom_text(aes(label = round(idade,2)), vjust=-0.5, position = position_dodge2(width=1)) +
  scale_y_continuous(breaks = seq(0,50,5)) + theme_minimal() 

# Empr?stimo

emprestimos_nov %>%  
  mutate(proporcao = 100 * proportion) %>% 
  mutate(emprestimo = case_when(emprestimo == "1" ~ "Sim, e pelo menos um morador conseguiu",
                                emprestimo == "2" ~ "Sim, mas nenhum morador conseguiu",
                                emprestimo == "3" ~ "Nao solicitou")) %>% 
  ggplot(aes(tipo_trabalho, round(proporcao,2), fill = emprestimo)) + 
  geom_col(position = "dodge") + theme_minimal() + coord_flip() +
  geom_text(aes(label = round(proporcao,1)), position = position_dodge(width=1),
            hjust=-0.25) + ylab("Proporção (em %)") + xlab("Categoria profissional")

# Plano de sa?de

plano_saude_nov %>%  
  filter(plano_saude != "9") %>% 
  mutate(proporcao = 100 * proportion) %>% 
  mutate(plano_saude = case_when(plano_saude == 1 ~ "Sim",
                                plano_saude == 2 ~ "Nao")) %>% 
  ggplot(aes(tipo_trabalho, round(proporcao,2), fill = plano_saude)) + 
  geom_col(position = "dodge") + theme_minimal() + coord_flip() + 
  geom_text(aes(label = round(proporcao,1)), position = position_dodge(width=0.9),
            hjust=-0.25) + ylab("Proporção (em %)") + xlab("Categoria profissional")


# Teste covid 

teste_covid_nov %>%  
  mutate(proporcao = 100 * proportion) %>% 
  mutate(teste_covid = case_when(teste == 1 ~ "Sim",
                                 teste == 2 ~ "Nao")) %>% 
  ggplot(aes(tipo_trabalho, round(proporcao,2), fill = teste_covid)) + 
  geom_col(position = "dodge") + theme_minimal() + coord_flip() +
  geom_text(aes(label = round(proporcao,1)), position = position_dodge(width=0.9),
            hjust=-0.25) + ylab("Proporção (em %)") + xlab("Categoria profissional")



# juntando bases  ---------------------------------------------------------

# tipo_trabalho

tipo_trabalho <- rbind(tipotrabalho_maio,tipotrabalho_agosto,tipotrabalho_nov) 

write.csv(tipo_trabalho, "qtd_tipo_trabalho.csv")

# tipotrabalho_sexo 

tipo_trabalho_sexo <- rbind(tipotrabalho_sexo_maio,tipotrabalho_sexo_agosto,
                            tipotrabalho_sexo_nov)

write.csv(tipo_trabalho_sexo, "qtd_tipo_trabalho_sexo.csv")

# tipotrabalho_sexo_idade 

tipo_trabalho_sexo_idade <- rbind(tipotrabalho_sexo_idade_maio,
                                  tipotrabalho_sexo_idade_agosto,
                                  tipotrabalho_sexo_idade_nov)

write.csv(tipo_trabalho_sexo_idade, "tipo_trabalho_sexo_idade.csv")

# escolaridade por tipo de trabalho

tipotrabalho_escolaridade <- rbind(join_escolaridade_maio,
                                   join_escolaridade_agosto,
                                   join_escolaridade_nov)

write.csv(tipotrabalho_escolaridade, "tipotrabalho_escolaridade.csv")


# quantidades por tipo de trabalho e fun??es 

tipotrabalho_funcao <- rbind(tipotrabalho_funcao_maio,
                             tipotrabalho_funcao_agosto,
                             tipotrabalho_funcao_nov)

write.csv(tipotrabalho_funcao, "tipotrabalho_funcao.csv")

#renda 

tipotrabalho_renda <- rbind(tipotrabalho_renda_maio,
                            tipotrabalho_renda_agosto,
                            tipotrabalho_renda_nov)

write.csv(tipotrabalho_renda, "tipotrabalho_renda.csv")

# Apoio ao sentir sintomas 

local_apoio <- rbind(local_buscou_apoio_maio,
                     local_buscou_apoio_agosto,
                     local_buscou_apoio_nov)
glimpse (local_buscou_apoio_nov)

local_buscou_apoio_agosto$apoio_posto <-as.character(local_buscou_apoio_agosto$apoio_posto)
local_buscou_apoio_nov$apoio_posto <-as.character(local_buscou_apoio_nov$apoio_posto)

write.csv(local_apoio, "local_apoio.csv")
# horas trabalhadas e horas de fato trabalhadas

tipotrabalho_horas <- rbind(tipotrabalho_horas_maio,
                            tipotrabalho_horas_agosto,
                            tipotrabalho_horas_nov)
tipotrabalho_horas <- tipotrabalho_horas %>%
  select(tipo_trabalho, sexo, horas_trabalhava, horas_trabalhou, mes)

write.csv(tipotrabalho_horas, "tipotrabalho_horas.csv")
#afastamento 

afastamento <- rbind(afastamento_maio, afastamento_agosto,
                     afastamento_nov)

write.csv(afastamento, "afastamento.csv")

#motivos para afastamento 

motivos_afastamento <- rbind(motivo_afastamento_maio, motivo_afastamento_agosto,
                             motivo_afastamento_nov)

write.csv(motivos_afastamento, "motivos_afastamento.csv")

# quantidades por tipo e auxilio

qtd_auxilio <- rbind(tipotrabalho_auxilio_maio, tipotrabalho_auxilio_agosto,
                     tipotrabalho_auxilio_nov)

write.csv(qtd_auxilio, "qtd_auxilio.csv")

# teletrabalho 

teletrabalho <- rbind(teletrabalho_maio,teletrabalho_agosto,teletrabalho_nov)

teletrabalho2 <- rbind(teletrabalho2_maio,teletrabalho2_agosto,teletrabalho2_nov)

write.csv(teletrabalho, "teletrabalho.csv")
write.csv(teletrabalho2, "teletrabalho2.csv")
# Empr?stimos 

emprestimos <- rbind(emprestimos_agosto, emprestimos_nov)

write.csv(emprestimos, "emprestimos.csv")

# Plano de saude 

plano_saude <- rbind(plano_saude_maio, plano_saude_agosto, 
                     plano_saude_nov)

write.csv(plano_saude, "plano_saude.csv")

# teste

teste_covid <- rbind(teste_covid_agosto, teste_covid_nov)

write.csv(teste_covid, "teste_covid.csv")

#------------------------------------------------------------------

##Passando arquivos de interessa para o formato xlsx

export(teletrabalho, file = "teletrabalho.xlsx")
export(tipotrabalho_horas, file = "tipotrabalho_horas.xlsx")
export(qtd_auxilio, file = "qtd_auxilio.xlsx")
export(emprestimos, file = "emprestimos.xlsx")
export(teste_covid, file = "teste_covid.xlsx")
export(motivos_afastamento, file = "motivos_afastamento.xlsx")

