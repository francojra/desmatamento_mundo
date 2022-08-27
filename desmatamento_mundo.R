
# Desmatamento no mundo --------------------------------------------------------------------------------------------------------------------
# Autora do script: Jeanne Franco ----------------------------------------------------------------------------------------------------------
# Data: 25/08/22 ---------------------------------------------------------------------------------------------------------------------------
# Referência: https://ourworldindata.org/working-hours -------------------------------------------------------------------------------------

# Sobre os dados ---------------------------------------------------------------------------------------------------------------------------

### A perda florestal líquida não é o mesmo que a desflorestação - ela mede a 
### desflorestação mais quaisquer ganhos na floresta durante um determinado 
### período.

### Durante a década desde 2010, a perda líquida nas florestas a nível 
### mundial foi de 4,7 milhões de hectares por ano. No entanto, as taxas 
### de desflorestação foram muito mais elevadas.

### A FAO da ONU estima que 10 milhões de hectares de floresta foram 
### abatidos todos os anos.

# Pacotes necessários para as análises -----------------------------------------------------------------------------------------------------

library(tidyverse)

# Carregar dados ---------------------------------------------------------------------------------------------------------------------------

desm <- read.csv("annual-deforestation.csv")
view(desm)
names(desm)

# Manipular dados --------------------------------------------------------------------------------------------------------------------------

desm1 <- desm %>%
  select(-Code) %>%
  filter(Entity %in% c("Brazil", "China", "Argentina",
                       "Bolivia", "India", "Australia")) %>%
  group_by(Entity) %>%
  summarise(media = mean(Deforestation),
            sd = sd(Deforestation), n = n(),
            se = sd/sqrt(n)) %>%
  view()

desm2 <- desm %>%
  select(-Code) %>%
  filter(Entity %in% c("China", "Argentina",
                       "Bolivia", "India", "Brazil")) %>%
  view()

# Gráficos ---------------------------------------------------------------------------------------------------------------------------------

options(scipen = 999)

ggplot(desm1, aes(x = fct_reorder(Entity, media), y = media)) +
  geom_col(fill = "black") +
  geom_errorbar(aes(x = Entity, y = media,
                    ymin = media - se, ymax = media + se),
                col = "white", width = 0.3, size = 0.8) +
  coord_flip() +
  labs(x = "Países", y = "Desmatamento médio líquido (hectares)") +
  theme_dark()

ggplot(desm2, aes(x = Year, y = Deforestation, 
                  group = Entity, color = Entity)) +
  geom_point(size = 2) +
  geom_line(size = 1.8) +
  labs(x = "Tempo (anos)", 
       y = "Desmatamento médio líquido (hectares)") +
  theme(legend.position = "none") +
  theme_get()
  