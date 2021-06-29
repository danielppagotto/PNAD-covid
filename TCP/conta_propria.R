library(srvyr); library(tidyverse); library(vroom); library(readxl); library(patchwork);
library(ggrepel)

options(scipen = 999)
setwd("~/GitHub/PNAD-covid/TCP")

# Bases  ------------------------------------------------------------------


trabalhadores <- read_excel("trabalhadores.xlsx")
escolaridade <- read_excel("trabalhadores.xlsx",sheet = "Planilha1")

pnad_covid_nov <- vroom("~/LAPEI/PNAD covid/PNAD_COVID_112020.csv")
pnad_covid_maio <- vroom("~/LAPEI/PNAD covid/PNAD_COVID_112020.csv")
pnad_covid_agosto <- vroom("~/LAPEI/PNAD covid/PNAD_COVID_112020.csv")

trabalho = c("04","06","07")

mot_afast <- read_excel("trabalhadores.xlsx", 
                        sheet = "afastamento", col_types = c("numeric", 
                                                             "text"))

# Tratamentos -------------------------------------------------------------

# Tratamento Agosto
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



# Tratamento Novembro
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

#teste <- (pnad_com_peso[["variables"]])


# analises Novembro -------------------------------------------------------

# caracterização dos usuários ---------------------------------------------
# quantidades por tipo de trabalho
tipotrabalho <- pnad_com_peso %>% 
  group_by(tipo_trabalho) %>% 
  summarise(proportion = survey_mean(),
            total = survey_total()) %>% 
  mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                   tipo_trabalho == "06" ~ "Empregador",
                                   tipo_trabalho == "07" ~ "Trabalhador por conta própria")) %>% 
  mutate(mes = "novembro")


# média de idade por sexo e tipo de trabalho

tipotrabalho_sexo <- pnad_com_peso %>%
  group_by(tipo_trabalho,sexo) %>%
  summarise(proportion_sexo = survey_mean(),
            total = survey_total()) %>% 
                      mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                                       tipo_trabalho == "06" ~ "Empregador",
                                                       tipo_trabalho == "07" ~ "Trabalhador por conta própria"),
         sexo = case_when(sexo == "1" ~ "Masculino",
                          sexo == "2" ~ "Feminino")) %>% 
  mutate(mes = "novembro")

# média de idade por sexo e tipo de trabalho

tipotrabalho_sexo_idade <- pnad_com_peso %>%
  group_by(tipo_trabalho,sexo) %>%
  summarise(idade = survey_mean(idade, vartype = "ci")) %>% 
  mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                   tipo_trabalho == "06" ~ "Empregador",
                                   tipo_trabalho == "07" ~ "Trabalhador por conta própria"),
         sexo = case_when(sexo == "1" ~ "Masculino",
                          sexo == "2" ~ "Feminino")) %>% 
  mutate(mes = "novembro")


# escolaridade por tipo de trabalho

tipotrabalho_escolaridade <- pnad_com_peso %>%
  group_by(tipo_trabalho,escolaridade) %>%
  summarise(prop_escolaridade = survey_mean(),
            total_escolaridade = survey_total()) %>% 
  mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                   tipo_trabalho == "06" ~ "Empregador",
                                   tipo_trabalho == "07" ~ "Trabalhador por conta própria"))

join_escolaridade <- tipotrabalho_escolaridade %>% 
  left_join(escolaridade, by = c("escolaridade"="id")) %>% 
  mutate(ate_medio = if_else(escolaridade <= 5, "Até Médio completo", "Superior Parcial ou mais")) %>% 
  group_by(tipo_trabalho, ate_medio) %>% 
  summarise(prop = sum(prop_escolaridade),
            total = sum(total_escolaridade)) %>% 
  mutate(mes = "novembro")


# quantidades por tipo de trabalho e funções 

tipotrabalho_funcao <- pnad_com_peso %>% 
  group_by(tipo_trabalho,funcao_trabalho) %>% 
  summarise(proportion = survey_mean(),
            total = survey_total()) %>% 
  mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                   tipo_trabalho == "06" ~ "Empregador",
                                   tipo_trabalho == "07" ~ "Trabalhador por conta própria")) %>% 
  left_join(trabalhadores, by = c("funcao_trabalho"="id")) %>% 
  mutate(mes = "novembro")


top_funcoes <- tipotrabalho_funcao %>% 
  group_by(tipo_trabalho) %>% 
  slice_max(order_by = proportion, n = 10)

# Renda 

tipotrabalho_renda <- pnad_com_peso %>%
  group_by(tipo_trabalho,sexo) %>%
  summarise(renda = survey_mean(rendimentos, na.rm = TRUE)) %>% 
  mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                   tipo_trabalho == "06" ~ "Empregador",
                                   tipo_trabalho == "07" ~ "Trabalhador por conta própria"),
         sexo = case_when(sexo == "1" ~ "Masculino",
                          sexo == "2" ~ "Feminino")) %>% 
  mutate(mes = "novembro")


# Apoio ao sentir sintomas 
local_buscou_apoio <- pnad_com_peso %>%
  group_by(tipo_trabalho,apoio_posto) %>%
  summarise(prop_escolaridade = survey_mean(),
            total_escolaridade = survey_total()) %>% 
  mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                   tipo_trabalho == "06" ~ "Empregador",
                                   tipo_trabalho == "07" ~ "Trabalhador por conta própria")) %>% 
  mutate(mes = "novembro")


# horas trabalhadas e horas de fato trabalhadas

tipotrabalho_horas <- pnad_com_peso %>%
  group_by(tipo_trabalho) %>%
  summarise(horas_trabalhava = survey_mean(horas_trabalhava, na.rm = TRUE),
            horas_trabalhou = survey_mean(horas_trabalhou, na.rm = TRUE)) %>% 
  mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                   tipo_trabalho == "06" ~ "Empregador",
                                   tipo_trabalho == "07" ~ "Trabalhador por conta própria")) %>% 
  mutate(mes = "novembro")


tipotrabalho_horas <- tipotrabalho_horas %>% 
                          select(tipo_trabalho, horas_trabalhava, 
                                 horas_trabalhou) %>% 
                          gather(key = "trabalho", value = "horas", 
                                 horas_trabalhava, horas_trabalhou) %>% 
                          mutate(mes = "novembro")




#motivos para afastamento 
afastamento <- pnad_com_peso %>% 
  group_by(tipo_trabalho, afastado) %>% 
  summarise(afastado_prop = survey_mean(),
            afastado_total = survey_total()) %>% 
  mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                   tipo_trabalho == "06" ~ "Empregador",
                                   tipo_trabalho == "07" ~ "Trabalhador por conta própria")) %>% 
  mutate(mes = "novembro")

motivo_afastamento <- pnad_com_peso %>% 
  filter(afastado == 1) %>% 
  group_by(tipo_trabalho, motivo_afastamento) %>% 
  summarise(afastado_prop = survey_mean(),
            afastado_total = survey_total()) %>% 
  mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                   tipo_trabalho == "06" ~ "Empregador",
                                   tipo_trabalho == "07" ~ "Trabalhador por conta própria")) %>%
  left_join(mot_afast, by = c("motivo_afastamento" = "id")) %>% 
  mutate(afastado_prop = 100 * afastado_prop,
         afastado_prop = round(afastado_prop,2),
         mes = "novembro")


# quantidades por tipo e auxilio


tipotrabalho_auxilio <- pnad_com_peso %>% 
  group_by(tipo_trabalho,recebeu_auxilio) %>% 
  summarise(proportion = survey_mean(),
            total = survey_total()) %>% 
  mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                   tipo_trabalho == "06" ~ "Empregador",
                                   tipo_trabalho == "07" ~ "Trabalhador por conta própria")) %>% 
  mutate(mes = "novembro") 



         
# média de valores de auxílio por tipo de trabalho 

tipotrabalho_rendimento_auxilio <- pnad_com_peso %>%
  group_by(tipo_trabalho) %>%
  summarise(rendimento = survey_mean(rendimento_auxilio, vartype = "ci", na.rm=TRUE)) %>% 
  mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                   tipo_trabalho == "06" ~ "Empregador",
                                   tipo_trabalho == "07" ~ "Trabalhador por conta própria")) %>% 
  mutate(mes = "novembro")


# teletrabalho 

teletrabalho <- pnad_com_peso %>% 
  group_by(tipo_trabalho,teletrabalho) %>% 
  summarise(proportion = survey_mean(),
            total = survey_total()) %>% 
  mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                   tipo_trabalho == "06" ~ "Empregador",
                                   tipo_trabalho == "07" ~ "Trabalhador por conta própria")) %>% 
  mutate(mes = "novembro") 



teletrabalho2 <- pnad_com_peso %>% 
  group_by(tipo_trabalho) %>% 
  summarise(total_homeoffice = survey_total(teletrabalho == 1, na.rm = TRUE),
            total_trabalho = survey_total(trabalhou == 1, na.rm = TRUE)) %>% 
  mutate(trab_home = total_homeoffice/total_trabalho) %>% 
  mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                   tipo_trabalho == "06" ~ "Empregador",
                                   tipo_trabalho == "07" ~ "Trabalhador por conta própria")) %>% 
  mutate(mes = "novembro")




# Empréstimos 

emprestimos <- pnad_com_peso %>% 
  group_by(tipo_trabalho,emprestimo) %>% 
  summarise(proportion = survey_mean(),
            total = survey_total()) %>% 
  mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                   tipo_trabalho == "06" ~ "Empregador",
                                   tipo_trabalho == "07" ~ "Trabalhador por conta própria")) %>% 
  mutate(mes = "novembro") 


# Plano de saude 

plano_saude <- pnad_com_peso %>% 
  group_by(tipo_trabalho,plano_saude) %>% 
  summarise(proportion = survey_mean(),
            total = survey_total()) %>% 
  mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                   tipo_trabalho == "06" ~ "Empregador",
                                   tipo_trabalho == "07" ~ "Trabalhador por conta própria")) %>% 
  mutate(mes = "novembro") 


# teste

teste_covid <- pnad_com_peso %>% 
  group_by(tipo_trabalho,teste) %>% 
  summarise(proportion = survey_mean(),
            total = survey_total()) %>% 
  mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                   tipo_trabalho == "06" ~ "Empregador",
                                   tipo_trabalho == "07" ~ "Trabalhador por conta própria")) %>% 
  mutate(mes = "novembro") 


# itens de higiene 

mascara <- pnad_com_peso %>% 
  group_by(tipo_trabalho, mascara) %>% 
  summarise(proportion = survey_mean(),
            total = survey_total()) %>% 
  mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                   tipo_trabalho == "06" ~ "Empregador",
                                   tipo_trabalho == "07" ~ "Trabalhador por conta própria")) %>% 
  mutate(mes = "novembro")


alcool <- pnad_com_peso %>% 
  group_by(tipo_trabalho, alcool) %>% 
  summarise(proportion = survey_mean(),
            total = survey_total()) %>% 
  mutate(tipo_trabalho = case_when(tipo_trabalho == "04" ~ "Empregado",
                                   tipo_trabalho == "06" ~ "Empregador",
                                   tipo_trabalho == "07" ~ "Trabalhador por conta própria")) %>% 
  mutate(mes = "novembro")



# Visuais  ----------------------------------------------------------------

# quantidades por tipo de trabalho

tipotrabalho %>%  
  ggplot(aes(fct_reorder(tipo_trabalho,total), total, fill = tipo_trabalho)) + 
  geom_col() + coord_flip() + theme_minimal() + xlab("Tipo de trabalhador")

# escolaridade
join_escolaridade %>% 
  mutate(prop = round(prop,4)) %>% 
  ggplot(aes(tipo_trabalho,prop,fill= ate_medio)) + geom_col(position = "dodge") +
  geom_label(aes(label = prop, vjust = -0.4), 
             position = position_dodge(width = 0.9)) +
  scale_y_continuous(breaks = seq(0,1,0.05)) + theme_minimal() 
  

# renda 
tipotrabalho_renda %>% 
  ggplot(aes(tipo_trabalho, renda, fill=sexo)) + geom_col(position = "dodge") + 
  scale_y_continuous(breaks = seq(0,6500,500)) + theme_minimal()


# top_funcoes
top_empregador <- top_funcoes %>% 
  filter(tipo_trabalho == "Empregador") %>%  
  mutate(proporcao = proportion * 100) %>% 
  ggplot(aes(x = fct_reorder(descricao,proporcao), y = proporcao)) + 
  geom_col() + coord_flip() + ggtitle("Empregador") + ylab("proporção (em %)") + 
  xlab(" ") + scale_y_continuous(breaks = seq(0,50,2.5)) + guides(fill= "none") + 
  theme_minimal()

top_empregado <- top_funcoes %>% 
  filter(tipo_trabalho == "Empregado") %>%  
  mutate(proporcao = proportion * 100) %>% 
  ggplot(aes(x = fct_reorder(descricao,proporcao), y = proporcao)) + 
  geom_col() + coord_flip() + ggtitle("Empregado") + ylab("proporção (em %)") + 
  xlab(" ") + scale_y_continuous(breaks = seq(0,50,2.5)) + guides(fill= "none") + 
  theme_minimal()

top_tpcp <- top_funcoes %>% 
  filter(tipo_trabalho == "Trabalhador por conta própria") %>%  
  mutate(proporcao = proportion * 100) %>% 
  ggplot(aes(x = fct_reorder(descricao,proporcao), y = proporcao)) + 
  geom_col() + coord_flip() + ggtitle("Trabalhador por conta própria") + ylab("proporção (em %)") + 
  xlab(" ") + scale_y_continuous(breaks = seq(0,50,2.5)) + guides(fill= "none") + 
  theme_minimal()


top_tpcp / top_empregado / top_empregador

# afastamento e motivo 

afastamento %>% 
  mutate(afastado = if_else(afastado == 1, "afastado","não se aplica")) %>% 
  ggplot(aes(tipo_trabalho, afastado_prop, fill = afastado)) + 
  geom_col(position = "dodge") + scale_y_continuous(breaks = seq(0,1,0.025)) +
  theme_minimal() 

motivo_afastamento %>% 
  ggplot(aes(motivo, afastado_prop)) + 
  geom_col(position = "dodge") + geom_label(aes(label = afastado_prop), hjust = -0.02) +
  coord_flip() + theme_minimal() +
  facet_grid(~tipo_trabalho)
  
# horas trabalhadas e trabalhou

tipotrabalho_horas %>% 
  ggplot(aes(tipo_trabalho, horas, fill = trabalho)) + geom_col(position = "dodge") + 
  theme_minimal() + scale_y_continuous(breaks = seq(0,50,2.5))


# Recebeu auxilio 

tipotrabalho_funcao_auxilio %>% 
  mutate(auxilio = case_when(recebeu_auxilio == 1 ~ "Sim",
                             recebeu_auxilio == 2 ~ "Não")) %>% 
  ggplot(aes(x=tipo_trabalho, y = proportion, fill = auxilio)) + geom_col() + 
  coord_flip() + ggtitle("Proporções de beneficiários de auxílio") + theme_minimal() +
  xlab("Categoria de trabalho") + ylab("Proporcoes (%)") + 
  scale_y_continuous(breaks = seq(0,1,0.05))
 
  
# teletrabalho 

teletrabalho %>%  
  mutate(teletrabalho = case_when(teletrabalho == "1" ~ "Sim",
                                  teletrabalho == "2" ~ "Não",
                                  teletrabalho == "NA" ~ "Não se aplica")) %>% 
  ggplot(aes(tipo_trabalho, proportion, fill = teletrabalho)) + 
  geom_col(position = "dodge") + theme_minimal() + 
  geom_text(aes(label = round(proportion,2)), position = position_dodge(width=0.9),
            vjust=-0.5)


# média de idade por sexo e tipo de trabalho

tipotrabalho_sexo_idade %>% 
  ggplot(aes(x=tipo_trabalho, y = idade, fill=sexo)) + geom_col(position = "dodge") +
  geom_text(aes(label = round(idade,2)), vjust=-0.5, position = position_dodge2(width=1)) +
  scale_y_continuous(breaks = seq(0,50,5)) + theme_minimal() 

# Empréstimo

emprestimos %>%  
  mutate(proporcao = 100 * proportion) %>% 
  mutate(emprestimo = case_when(emprestimo == "1" ~ "Sim, e pelo menos um morador conseguiu",
                                emprestimo == "2" ~ "Sim, mas nenhum morador conseguiu",
                                emprestimo == "3" ~ "Não solicitou")) %>% 
  ggplot(aes(tipo_trabalho, round(proporcao,2), fill = emprestimo)) + 
  geom_col(position = "dodge") + theme_minimal() + coord_flip() +
  geom_text(aes(label = round(proporcao,1)), position = position_dodge(width=1),
            hjust=-0.25) + ylab("Proporção (em %)") + xlab("Categoria profissional")

# Plano de saúde

plano_saude %>%  
  filter(plano_saude != "9") %>% 
  mutate(proporcao = 100 * proportion) %>% 
  mutate(plano_saude = case_when(plano_saude == 1 ~ "Sim",
                                plano_saude == 2 ~ "Não")) %>% 
  ggplot(aes(tipo_trabalho, round(proporcao,2), fill = plano_saude)) + 
  geom_col(position = "dodge") + theme_minimal() + coord_flip() + 
  geom_text(aes(label = round(proporcao,1)), position = position_dodge(width=0.9),
            hjust=-0.25) + ylab("Proporção (em %)") + xlab("Categoria profissional")


# Teste covid 

teste_covid %>%  
  mutate(proporcao = 100 * proportion) %>% 
  mutate(teste_covid = case_when(teste == 1 ~ "Sim",
                                 teste == 2 ~ "Não")) %>% 
  ggplot(aes(tipo_trabalho, round(proporcao,2), fill = teste_covid)) + 
  geom_col(position = "dodge") + theme_minimal() + coord_flip() + 
  geom_text(aes(label = round(proporcao,1)), position = position_dodge(width=0.9),
            hjust=-0.25) + ylab("Proporção (em %)") + xlab("Categoria profissional")
