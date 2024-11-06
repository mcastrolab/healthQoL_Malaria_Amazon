#ARTIGO MALARIA QUALIDADE DE VIDA CAMPO

# Instalando de lendo os pacotes ------------------------------------------

library(tidyverse)
library(readxl)
library(magrittr)
library(ca)
library(factoextra)
library(MatchIt)
library(tableone)
library(geobr)
library(sf)
library(ggrepel)
library(ggplot2)
library(dplyr)
library(apyramid)
library(psych)
library(MatchIt)
library(lmtest)
library(sandwich)
library(ggspatial)

# Localização dos municípios da pesquisa ----------------------------------

municipios <- c(1100809, 1400175, 1600535, 1400308, 1301654, 1200336, 1301803, 1200203, 1400209)

estados <- read_state() %>% 
  filter(code_region == "1" | code_state == 21)

munic <- read_municipal_seat() %>% 
  filter(code_muni %in% municipios)

munic$x = st_coordinates(munic)[,1]
munic$y = st_coordinates(munic)[,2]

estados$x = st_coordinates(st_centroid(estados))[,1]
estados$y = st_coordinates(st_centroid(estados))[,2]

estados$x[estados$name_state == "Roraima"] = -62.39890
estados$y[estados$name_state == "Roraima"] = 3

ggplot() +
  geom_sf(data = estados) +
  geom_sf(data = munic, color = "darkred", size = 1.5) +
  geom_label(data = estados,
             mapping = aes(x = x, y = y, label = name_state),
             alpha = .2,
             color = "red") +
  geom_text_repel(data = munic,
                  mapping = aes(x = x, y = y, label = name_muni),
                  size = 3.9,
                  segment.alpha = 0.5,
                  fontface = "bold",
                  force_pull = 0,
                  force = 30,
                  seed = 10) +
  annotation_scale(location="br") +
  annotation_north_arrow(location = "br",
                         which_north = "true",
                         height = unit(2, "cm"),
                         width = unit(2, "cm"),
                         pad_x = unit(7.0, "in"),
                         pad_y = unit(0.0, "in"),
                         style = north_arrow_fancy_orienteering) +
  labs(x = NULL,
       y = NULL) +
  theme_bw() +
  theme(legend.position = c(.21, .1),
        axis.title = element_blank(),
        axis.text=element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.key.width=unit(2.5,"cm"))

ggsave("figuras/mapa_amostra.png", dpi = 300, width = 10, height = 5.4)
ggsave("figuras/mapa_amostra.pdf", width = 10, height = 5.4)


# Lendo as bases ----------------------------------------------------------

banco_final <- read_rds("banco_final.RDS")

score_mg <- read_csv2("scores.csv")

# Construção das variáveis  -----------------------------------------------

banco_final <- banco_final %>% 
  mutate(tratamento = case_when(FILTRO==1 | FILTRO==2 ~ "Tratamento",
                                FILTRO==3 ~ "Controle"),
         estados_saude = str_c(P72, P73, P74, P75, P76),
         estados_saude = as.numeric(estados_saude)) %>% 
  left_join(score_mg, by = c("estados_saude" = "estado_saude"))

#variaveis de domicilio

banco_final <- banco_final %>% 
  mutate(situa_dom = case_when(P11 == 96 ~ 4,
                               TRUE ~ P11),
         mater_dom = case_when(P12 == 96 ~ 9,
                               TRUE ~ P12),
         tem_calcamento = case_when(P13 == 9 ~ NA,
                                    TRUE ~ P13),
         agua_encanada = case_when(P14 == 1 ~ 1,
                                   P14 == 2 ~ 0),
         origem_agua = case_when(P15 == 96 ~ 3,
                                 P15 == 99 ~ NA,
                                 TRUE ~ P15),
         num_comodos = P16,
         num_banheiros = P17,
         escoad_banheiro = case_when(P18 == 96 ~ 7,
                                     P18 == 99 ~ NA,
                                     P17 == 0 ~ 0,
                                     TRUE ~ P18),
         descart_lixo = P19,
         tem_som = P20A,
         tem_internet = case_when(P20B == 1 ~ 1,
                                  P20B == 2 ~ 0),
         tem_tv = P20C,
         tem_dvd = P20D,
         tem_geladeira = P20E,
         tem_freezer = P20F,
         tem_automovel = P20G,
         tem_computador = P20H,
         tem_fogao = P20I,
         tem_microondas = P20J,
         tem_motocicleta = P20K,
         tem_celular = P20L,
         dono_gado = case_when(P20M == 1 ~ 1,
                               P20M == 2 ~ 0),
         dono_terra = case_when(P20N == 1 ~ 1,
                                P20N == 2 ~ 0),
         tem_energia = case_when(P21 == 1 ~ 1,
                                 P21 == 2 ~ 0),
         recebe_auxilio = case_when(P24 == 1 ~ 1,
                                    P24 == 2 ~ 0,
                                    P24 == 9 ~ NA),
         num_morad_dom = P1,
         escolaridade_dom = case_when(P28 == 99 ~ NA,
                                      TRUE ~ P28),
         id2 = row_number())


#selecionando as variaveis para construção do indicador através da análise de correspondência múltipla (MCA)

var_mca <- banco_final %>% 
  arrange(id2) %>% 
  select (situa_dom:escolaridade_dom) 
  
mca <- mjca(var_mca)

fviz_screeplot(mca, addlabels = TRUE, ylim = c(0, 100)) +
  theme_bw()
ggsave("figuras/screeplot.png", width = 10, height = 5.4, dpi = 300)
ggsave("figuras/screeplot.pdf", width = 10, height = 5.4)

tmp_mca <- data.frame(id2 = mca$rownames, ind = scales::rescale(mca$rowcoord[,1]*-1, to = c(0,100))) %>% 
  mutate(id2 = as.integer(id2))

banco_final <- banco_final %>% 
  left_join(tmp_mca, by = "id2") %>% 
  select(ID, morador, ind, everything()) %>% 
  mutate(quartil = case_when(ind <= 20 ~ "Muito pobre",
                              ind > 20 & ind <= 40 ~ "Pobre",
                              ind > 40 & ind <= 60 ~ "Médio",
                              ind > 60 & ind <= 80 ~ "Rico",
                              ind > 80 ~ "Muito rico"))

banco_final <- banco_final %>% 
  select(ID, morador, ind, quartil, everything())

banco_final <- banco_final %>% 
  mutate(treatment = ifelse(tratamento == 'Controle', 'Control', 'Treatment'))

#fazer um boxplot do indicador socioeconomico com as variaveis 

banco_final %>% 
  select(ID, ind, situa_dom:escolaridade_dom) %>% 
  pivot_longer(!c(ID, ind),
               names_to = "variavel",
               values_to = "val") %>% 
  mutate(val = as.factor(val)) %>% 
  ggplot(aes(x = val, y = ind)) +
  geom_boxplot() +
  xlab(NULL) +
  ylab("Indicador socio-economico") +
  facet_wrap(~variavel, scales = "free_x") +
  theme_bw()
ggsave("figuras/boxplot.png", dpi = 300, height = 5.4, width = 10)
ggsave("figuras/boxplot.pdf", dpi = 300, height = 5.4, width = 10)


# propensity score matching -----------------------------------------------

#tratamento = tratamento
#Outcome = score
#covariates = sexo, idade, ind, urbano, covid, diabetes, hipertensao, artrite, prob_respir, dengue, febre_amarela

banco_final <- banco_final %>% 
  mutate(female = case_when(P5 == 1 ~ 1,
                            P5 == 2 ~ 0),
         age = P4,
         covid = case_when(P78 ==1 ~ 1,
                           P78==2 ~ 0),
         diabetes = case_when(P81A == 1 ~ 1,
                              P81A == 2 ~ 0),
         hypertension = case_when(P81B == 1 ~ 1,
                                  P81B == 2 ~ 0),
         arthritis = case_when(P81C == 1 ~ 1,
                               P81C == 2 ~ 0),
         breathing_prob = case_when(P81D == 1 ~ 1,
                                    P81D == 2 ~ 0),
         dengue_fever = case_when(P82 == 1 ~ 1,
                                  P82 == 2 ~ 0),
         yellow_fever = case_when(P83 == 1 ~ 1,
                                  P83 == 2 ~ 0),
         urban = case_when(ZONA == 1 ~ 1,
                           ZONA == 2 ~ 0))

#Difference-in-means: pre-treatment covariates

psm_cov <- c("female", "age", "ind", "urban", "covid", "diabetes", "hypertension", "arthritis", "breathing_prob", "dengue_fever", "yellow_fever")

tab_pre_psm <- banco_final %>% 
  group_by(tratamento) %>% 
  select(one_of(psm_cov)) %>% 
  summarise_all(funs(mean(.,na.rm = TRUE)))
write_excel_csv2(tab_pre_psm,"tab_pre_psm.csv")

with(banco_final, t.test(female ~ tratamento))
with(banco_final, t.test(age ~ tratamento))
with(banco_final, t.test(ind ~ tratamento))
with(banco_final, t.test(urban ~ tratamento))
with(banco_final, t.test(covid ~ tratamento))
with(banco_final, t.test(diabetes ~ tratamento))
with(banco_final, t.test(hypertension ~ tratamento))
with(banco_final, t.test(arthritis ~ tratamento))
with(banco_final, t.test(breathing_prob ~ tratamento))
with(banco_final, t.test(dengue_fever ~ tratamento))
with(banco_final, t.test(yellow_fever ~ tratamento))

#Propensity score estimation (logit model)

banco_final <- banco_final %>% 
  mutate(treatment = as.factor(treatment))

psm <- glm(treatment ~ female + age + ind + urban + covid + diabetes + hypertension + arthritis + breathing_prob + dengue_fever + yellow_fever,
            family = binomial(), data = banco_final)
summary(psm)
exp(coef(psm)) #odds ratio

psm_df <- data.frame(pr_score = predict(psm, type = "response"),
                     treatment = psm$model$treatment)
head(psm_df)

#Examining the region of common support

psm_df %>%
  ggplot(aes(x = pr_score)) +
  geom_histogram(color = "white") +
  facet_wrap(~treatment, ncol = 1) +
  xlab("Probability of getting malaria") +
  theme_bw()
ggsave("figuras/commum_support.png", dpi = 300, width = 10, height = 5.4)
ggsave("figuras/commum_support.pdf", dpi = 300, width = 10, height = 5.4)

#Executing a matching algorithm

#nearest neighbor matching
mod_match <- matchit(treatment ~ female + age + ind + urban + covid + diabetes + hypertension + arthritis + breathing_prob + dengue_fever + yellow_fever,
                     data = banco_final, method = "nearest")
summary(mod_match)

banco_final_pos_psm <- match.data(mod_match)
dim(banco_final_pos_psm)

tab_pos_psm <- banco_final_pos_psm %>%
  group_by(treatment) %>%
  select(one_of(psm_cov)) %>%
  summarise_all(funs(mean))
write_excel_csv2(tab_pos_psm,"Tab_pos_psm.csv")

with(banco_final_pos_psm, t.test(female ~ tratamento))
with(banco_final_pos_psm, t.test(age ~ tratamento))
with(banco_final_pos_psm, t.test(ind ~ tratamento))
with(banco_final_pos_psm, t.test(urban ~ tratamento))
with(banco_final_pos_psm, t.test(covid ~ tratamento))
with(banco_final_pos_psm, t.test(diabetes ~ tratamento))
with(banco_final_pos_psm, t.test(hypertension ~ tratamento))
with(banco_final_pos_psm, t.test(arthritis ~ tratamento))
with(banco_final_pos_psm, t.test(breathing_prob ~ tratamento))
with(banco_final_pos_psm, t.test(dengue_fever ~ tratamento))
with(banco_final_pos_psm, t.test(yellow_fever ~ tratamento))

#Estimating treatment effects

#before matching

dif_mean_before <- banco_final %>% 
  mutate(score = as.numeric(score)) %>% 
  group_by(tratamento) %>% 
  summarise(n_patients = n(),
            mean = mean(score),
            std_error = sd(score)/sqrt(n_patients))
write_excel_csv2(dif_mean_before,"dif_mean_before.csv")

banco_final <- banco_final %>% 
  mutate(score = as.numeric(score),
         treatment = case_when(tratamento == 'Tratamento' ~ 1,
                               tratamento == 'Controle' ~ 0))
with(banco_final, t.test(score ~ tratamento))

#After matching

dif_mean_after <- banco_final_pos_psm %>% 
  mutate(score = as.numeric(score)) %>% 
  group_by(tratamento) %>% 
  summarise(n_patients = n(),
            mean = mean(score),
            std_error = sd(score)/sqrt(n_patients))
write_excel_csv2(dif_mean_after,"dif_mean_after.csv")

banco_final_pos_psm <- banco_final_pos_psm %>% 
  mutate(score = as.numeric(score),
         treatment = case_when(tratamento == 'Tratamento' ~ 1,
                               tratamento == 'Controle' ~ 0))
with(banco_final_pos_psm, t.test(score ~ treatment))


# EQ-5D-3L  ---------------------------------------------------------------

'Figura - Proporção de indivíduos em cada dimensão e nível do eq-5d para os grupos de tratamento e controle'

Fig_eq5d <- banco_final_pos_psm %>% 
  distinct(ID, .keep_all = TRUE) %>% 
  select(treatment, P72, P73, P74, P75, P76) %>% 
  filter(!is.na(P72)) %>% 
  pivot_longer(!treatment, names_to = "dimensao", values_to = "valor") %>% 
  group_by(treatment, dimensao, valor) %>%
  summarise(n = n()) %>% 
  group_by(treatment, dimensao) %>% 
  mutate(percent = n/sum(n)*100,
         Group = case_when(treatment == 0 ~ "Control",
                           treatment == 1 ~ "Treatment"),
         Group = as.factor(Group),
         Level = as.factor(valor),
         Dimension = case_when(dimensao == "P72" ~ "Mobility",
                               dimensao == "P73" ~ "Self-care",
                               dimensao == "P74" ~ "Usual activities",
                               dimensao == "P75" ~ "Pain/discomfort",
                               dimensao == "P76" ~ "Anxiety/depression"),
         Dimension = ordered(Dimension, levels = c("Mobility", "Self-care", "Usual activities", "Pain/discomfort", "Anxiety/depression")))

ggplot(Fig_eq5d, aes(x = Dimension, y = percent, fill = Level)) +
  geom_bar(stat="identity", position=position_dodge2()) +
  geom_text(aes(label=sprintf(percent, fmt = '%#.2f')), vjust=-0.6, color="black",
            position = position_dodge(0.9), size=3.5) +
  scale_fill_brewer(palette="Accent") +
  facet_wrap(~ Group, ncol = 1) +
  ylim(0,100) +
  ylab("Percentage (%)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave("figuras/dimensoes_group.png", dpi = 300, width = 10, height = 5.4)
ggsave("figuras/dimensoes_group.pdf", width = 10, height = 5.4)


'Figura - Distribuição do EAV por tratmento e controle'

vas <- banco_final_pos_psm %>%
  distinct(ID, .keep_all = TRUE)%>% 
  mutate(P77 = ifelse(P77 == 999, NA, P77),
         tratamento = as.factor(tratamento),
         Groups = (ifelse(tratamento == "Controle", "Control", "Treatment"))) %>% 
  filter(!is.na(P77)) %>% 
  group_by(Groups, P77) %>% 
  summarise(n = n()) %>% 
  group_by(Groups) %>% 
  mutate(tot = sum(n),
         percent = n/sum(n)*100,
         cumsuma = cumsum(percent)) %>% 
  ggplot(., aes(x=factor(P77), y = cumsuma))+
  geom_line(aes(group = Groups, color = Groups)) +
  labs(x = "VAS score",
       y = "Cumulative sum") +
  scale_fill_brewer(palette="Accent") +
  theme_bw()

ggsave("eav_group.png", dpi = 300, width = 10, height = 5.4)
ggsave("eav_group.pdf", width = 10, height = 5.4)
ggsave("eav_group.tif", width = 10, height = 5.4)

'Figura - Distribuição cumulativa dos estados de saude do eq-5d para os grupos de tratamento e controle'

tmp1 <- banco_final_pos_psm %>% 
  distinct(ID, .keep_all = TRUE) %>% 
  select(tratamento, P72,P73,P74,P75,P76) %>% 
  filter(!is.na(P72)) %>% 
  mutate(eq5d = as.integer(str_c(P72,P73,P74,P75,P76))) %>% 
  summarise(n = n(), .by = c(tratamento, eq5d)) %>% 
  left_join(score_mg, by = c("eq5d"= "estado_saude")) %>% 
  group_by(tratamento) %>% 
  arrange(desc(score)) %>% 
  mutate(score = as.numeric(score),
         percent = cumsum(n)/sum(n)*100,
         grupo = case_when(tratamento == "Tratamento"  ~ "Treatment",
                           tratamento == "Controle" ~ "Control")) %>% 
  ungroup()

factor <- tmp1 %>% 
  distinct(eq5d)

factor <- factor %>% 
  mutate(eq5d2 = ordered(eq5d, levels = factor %>% pull(eq5d))) %>% 
  select(eq5d, eq5d2)

tmp1 <- tmp1 %>% 
  left_join(factor, by = "eq5d")

means <- tmp1 %>% 
  summarise(mean = score*n,
            tot = sum(n, na.rm = TRUE),
            media = sum(mean)/tot,
            .by = 'grupo') %>% 
  distinct(grupo, .keep_all = TRUE) %>% 
  pull(media)

ggplot(tmp1, 
       aes(x = eq5d2, 
           y = percent)) +
  geom_line(aes(color = grupo,
                group = grupo)) +
  scale_x_discrete(breaks = c("11111", "11123", "11132","22211", "11331", "22222", "21331", "32221", "32321", "32331", "33322")) + 
  labs(colour = "Groups",
       y = "Percentage",
       x = "Health states") + 
  annotate("text", 
           x = 50, 
           y = 50, 
           label = paste("Utilities: \n Control: ",round(means[2],4)," \n Treatment: ", round(means[1],4))) +
  theme_bw() +
  theme(legend.position = "bottom") +
  theme()

ggsave("figuras/score_group.png", dpi = 300, width = 10, height = 5.4)
ggsave("figuras/score_group.pdf", width = 10, height = 5.4)


'Apêndice'

tmp <- banco_final %>% 
  select(tratamento, estados_saude, score) %>% 
  mutate(Grupo = tratamento) %>% 
  group_by(Grupo, estados_saude, score) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n), .by_group = TRUE) %>% 
  mutate(Percentual = n/sum(n)*100) %>% 
  select(Grupo, estados_saude, score, n, Percentual) %>% 
  write_excel_csv2("Resultados/tab_apendice_estados_saude.csv")



# piramide etária ---------------------------------------------------------


banco_final$grupo_idade <- cut(banco_final$P4, 
    breaks = c(-Inf, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, +Inf),
    labels = c("18-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70+"))

banco_final <- banco_final %>% 
  mutate(Gender = ifelse(P5 == 1, "Female", "Male"),
         Gender = ordered(Gender, levels = c("Male", "Female")))



piramide <- age_pyramid(banco_final, grupo_idade, split_by = Gender)
piramide +
  labs(y = "Count", x = "Age group") +
  theme_bw()

ggsave("figuras/piramide.png", dpi = 300, width = 10, height = 5.4)
ggsave("figuras/piramide.pdf", width = 10, height = 5.4)



# datas mínimas -----------------------------------------------------------

library(lubridate)

banco_final_pos_psm |> 
  filter(tratamento == "Controle") |> 
  mutate(data = ym(str_sub(P9, end = -3)),
         limite = ym("202201"),
         diff = limite-data) |>
  select(data, limite, diff) |> 
  summarise(media = mean(diff, na.rm = TRUE),
            mediana = median(diff, na.rm = TRUE),
            max = max(diff, na.rm = TRUE),
            min = min(diff, na.rm = TRUE))


banco_final_pos_psm |> 
  filter(tratamento == "Controle") |> 
  mutate(data = ym(str_sub(P9, end = -3)),
         limite = ym("202201"),
         diff = limite-data) |>
  select(data, limite, diff) |> 
  mutate(zero = ifelse(diff <= 60, 1, 0),
         qtd = sum(zero, na.rm = TRUE),
         prop = sum(zero, na.rm = TRUE)/n()*100)

