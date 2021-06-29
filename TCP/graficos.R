library(tidyverse); library(forcats)


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


