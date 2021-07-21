library(tidyverse); library(forcats); library(ggrepel)


# quantidades -------------------------------------------------------------

qtd_por_tipo <- read.csv("https://raw.githubusercontent.com/danielppagotto/PNAD-covid/main/Dados/qtd_tipo_trabalho.csv") 

qtd_por_tipo <- qtd_por_tipo %>% 
                    select(-X)


qtd_por_tipo$mes <- factor(qtd_por_tipo$mes,
                levels = c("maio", "agosto", "novembro"))

qtd_por_tipo %>% 
  ggplot(aes(x = mes, y = total, fill=fct_reorder(tipo_trabalho,-proportion))) + 
  geom_col(position = "dodge") + theme_minimal() + xlab("Mês") + ylab("Proporções") +
  geom_text(aes(label = round(total)), position = position_dodge(width = 0.9),
            vjust = -0.5) + guides(fill=guide_legend("Tipo de trabalho")) + 
  ggtitle("Proporções por tipo de trabalho","Fonte: PNAD-Covid dos meses de Maio, Agosto e Novembro")


# idades ------------------------------------------------------------------

teletrabalho <- read.csv("https://raw.githubusercontent.com/danielppagotto/PNAD-covid/main/Dados/teletrabalho.csv") %>% 
                      select(-X)


teletrabalho$mes <- factor(teletrabalho$mes,
                           levels = c("maio", "agosto", "novembro"))

teletrabalho$teletrabalho <- teletrabalho$teletrabalho %>% 
                                                  replace_na(3)  
                                                  

teletrabalho <- teletrabalho %>% 
                      mutate(teletrabalho = case_when(teletrabalho == "1" ~ "Sim",
                                                      teletrabalho == "2" ~ "Não",
                                                      teletrabalho == "3" ~ "Não se aplica"))

teletrabalho %>%  
  ggplot(aes(x = fct_reorder(tipo_trabalho,proportion), y = proportion, fill = fct_reorder(teletrabalho,proportion))) + 
  geom_col(position = "dodge") + facet_wrap(~mes, nrow = 3) + theme_minimal() + scale_y_continuous(breaks = seq(0,1,0.10)) + 
  ggtitle("Proporções de profissionais por categoria que realizaram teletrabalho", 
          "Fonte: PNAD-Covid dos meses de maio, agosto e novembro de 2020")  +
  geom_text(aes(label = round(proportion, 2)), position = position_dodge(width = 1), vjust = -0.3) + 
  xlab("Perfil do trabalhador") + ylab("Proporção") + scale_fill_discrete(name = "status")
