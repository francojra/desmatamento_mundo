
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
  filter(Entity %in% c("Brazil", "China",
                       "Bolivia", "India")) %>%
  group_by(Entity) %>%
  summarise(media = mean(Deforestation),
            sd = sd(Deforestation), n = n(),
            se = sd/sqrt(n)) %>%
  view()

# Gráficos ---------------------------------------------------------------------------------------------------------------------------------

ggplot(desm1, aes(x = Entity, y = media)) +
  geom_col()
  