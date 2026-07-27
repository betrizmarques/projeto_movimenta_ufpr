library(tidyverse)
library(janitor)
library(lme4)
library(lmerTest)
library(rstatix)
library(ggplot2)
library(readxl)

options(scipen = 999)

dados <- read_excel("Dados_06.07.xlsx") %>% clean_names() %>% 
  type.convert(as.is = TRUE)

dicionario <- read_excel("Dados_06.07.xlsx", sheet = 2) 

# validacao de paces semana 0
validacao_paces <- dados %>% 
  select(codigo, l1_s0, l2_s0, d_21km_s16_real_vm, maratona_km_h_real, vc_s4_real) %>% 
  drop_na() %>% 
  pivot_longer(cols = -codigo, names_to = "pace_tipo", values_to = "velocidade")

teste_friedman <- validacao_paces %>% friedman_test(velocidade ~ pace_tipo | codigo)
teste_friedman

pares_wilcox <- validacao_paces %>% 
  pairwise_wilcox_test(velocidade ~ pace_tipo, paired = TRUE, p.adjust.method = "bonferroni")
pares_wilcox

validacao_distancias <- dados %>%
  select(codigo, t1000_s4_km_h, t2000_s4_km_h, t3000_s4_km_h) %>%
  drop_na() %>%
  pivot_longer(cols = -codigo, names_to = "distancia", values_to = "velocidade")

friedman_dist <- validacao_distancias %>% friedman_test(velocidade ~ distancia | codigo)


# validacao de teste máximo
teste_esforco_s0 <- t.test(dados$x1km_fresco_vm_s0, dados$x1km_pos_vm_s0, paired = TRUE)
teste_esforco_s0

dados <- dados %>%
  mutate(esforco_valido_S0 = ifelse(x1km_fresco_vm_s0 > x1km_pos_vm_s0, "Válido", "Suspeito"))

table(dados$esforco_valido_S0)

dados_longos <- dados %>%
  select(codigo, grupo, 
         d_21km_s0_real, d_21km_s4_real, d_21km_s8_real, d_21km_s12_real, d_21km_s16_real_vm) %>%
  pivot_longer(
    cols = starts_with("d_21km_"),
    names_to = "semana",
    names_pattern = "d_21km_(s\\d+)_real",
    values_to = "velocidade_21k"
  ) %>%
  mutate(
    semana = factor(semana, levels = c("s0", "s4", "s8", "s12", "s16")),
    grupo = factor(grupo) 
  )

dados_longos


modelo_misto <- lmer(velocidade_21k ~ grupo * semana + (1 | codigo), data = dados_longos)
summary(modelo_misto)
anova(modelo_misto)



dados_longos %>%
  filter(!is.na(velocidade_21k)) %>%
  group_by(grupo, semana) %>%
  summarise(
    media = mean(velocidade_21k),
    erro_padrao = sd(velocidade_21k) / sqrt(n()),
    .groups = 'drop'
  ) %>%
  ggplot(aes(x = semana, y = media, group = grupo, color = grupo)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = media - erro_padrao, ymax = media + erro_padrao), width = 0.15) +
  labs(
    title = "Evolução da velocidade nos 21km por sequência de treino",
    x = "Semana",
    y = "Velocidade média nos 21km",
    color = "Sequência"
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")



dados_queda <- dados %>%
  select(codigo, grupo, queda_s0, queda_s8, queda_s16) %>%
  pivot_longer(cols = starts_with("queda_S"), names_to = "semana", values_to = "queda_1km") %>%
  mutate(semana = factor(semana, levels = c("queda_s0", "queda_s8", "queda_s16")),
         grupo = factor(grupo))

dados_queda

modelo_queda <- lmer(queda_1km ~ grupo * semana + (1 | codigo), data = dados_queda)
anova(modelo_queda)
