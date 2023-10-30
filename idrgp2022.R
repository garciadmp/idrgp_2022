library(tidyverse)
library(forcats)
library(readxl)
library(sf)
library(stringr)
install.packages("janitor")
library(janitor)
install.packages("ggsn")
library(ggsn)

options(scipen = 99)

# Definição dos problemas -------------------------------------------------

## Identificar qual é o valor liquidado pela Prefeitura de São Paulo, em cada
## Subprefeitura, no ano de 2022 e comparar esse gasto com o Índice de Distribuição
## Regional do Gasto Público (IDRGP)


# Coleta e processamento de dados -----------------------------------------

#conhecendo a planilha com as fontes dos dados
readxl::excel_sheets("Planilha IDRGP.xlsx")
read_xlsx("Planilha IDRGP.xlsx", sheet = "Monitoramento - Metas IDRGP", col_names = TRUE) #necessário pular 5 linhas
read_xlsx("Planilha IDRGP.xlsx", sheet = "obras_prioritarias", col_names = TRUE)
read_xlsx("Planilha IDRGP.xlsx", sheet = "orcamento_cidadao", col_names = TRUE) 
read_xlsx("Planilha IDRGP.xlsx", sheet = "base_da", col_names = TRUE) #necessário descartar dados a partir da linha 34
read_xlsx("Planilha IDRGP.xlsx", sheet = "ref_idrgp", col_names = TRUE)

#base cartográfica

(sp <- sf::st_read("SAD69-96_SHP_subprefeitura_polygon.shp", crs = 4326) %>% 
    dplyr::arrange(sp_nome)) #necessário criar uma coluna com abreviação dos nomes das SUBS


sf::sf_use_s2(FALSE)
## foi necessário utilizar esse código, pois mantendo como TRUE, ocorria o
## seguinte erro ao tentar gerar o mapa:

## Error in s2_geography_from_wkb(x, oriented = oriented, check = check) : 
## Evaluation error: Found 1 feature with invalid spherical geometry.
## [1] Loop 0 is not valid: Edge 0 is degenerate (duplicate vertex).
## In addition: Warning messages:
## 1: In st_is_longlat(x) :
## bounding box has potentially an invalid value range for longlat data
## 2: In st_is_longlat(x) :
## bounding box has potentially an invalid value range for longlat data


# Inserção das siglas
(siglas <- c("AF",
             "BT",
             "CL",
             "CS",
             "CV",
             "AD",
             "CT",
             "EM",
             "FB",
             "GS",
             "IP",
             "IT",
             "IQ",
             "JA",
             "JT",
             "LA",
             "MB",
             "MO",
             "PA",
             "PE",
             "PR",
             "PI",
             "PJ",
             "ST",
             "SA",
             "SM",
             "MP",
             "SB",
             "SE",
             "MG",
             "VM",
             "VP"))

(sp$sigla <- siglas)

(subprefeitura <- sp %>% 
    select(sp_nome, sigla, geometry))

#teste do mapa com siglas
subprefeitura %>% 
  ggplot() +
  geom_sf() +
  geom_sf_text(aes(label = siglas), color = "black", size = 1.5, fontface = "bold")

# população de são paulo (observasampa)

dados_observa <- "https://dados-abertos-observasampa.prefeitura.sp.gov.br/_temp/DadosAbertos/ObservaSampaDadosAbertosIndicadoresCSV.csv"

(pop_sp <- read_csv2(dados_observa) %>% 
    janitor::clean_names())

names(pop_sp)

(pop_sp <- pop_sp %>% 
    dplyr::filter(stringr::str_detect(regiao, "Subprefeitura")) %>% 
    filter(nome == "População total" & periodo == 2021) %>% 
    rename(sp_nome = regiao))


#função para padronizar nomes das subprefeituras
padronizar_nomes <- function(nomes, remover_acentos = TRUE) {
  nomes <- toupper(nomes)  # Converter para maiúsculas
  if (remover_acentos) {
    nomes <- iconv(nomes, to = "ASCII//TRANSLIT")  # Remover acentos
  }
  nomes <- gsub("[[:punct:][:digit:]]", "-", nomes)  # Substituir pontuação e dígitos por traços
  nomes <- gsub("-+", "-", nomes)  # Remover traços repetidos
  nomes <- gsub("^-|-$", "", nomes)  # Remover traços no início e no final
  nomes <- gsub("-$", "", nomes)  # Remover traço no final, se houver
  nomes <- gsub("SUBPREFEITURA ", "", nomes) # Remover termo subprefeitura
  nomes <- gsub("DE ", "", nomes) # Remover termo "de " nos casos "sub de...". Irá impactar "cidade tiradentes e cidade ademar"
  nomes <- gsub(" (subprefeitura)", "", nomes) #Retirar o termo "(subprefeitura)" do objeto pop_sp
  return(nomes)
}

## mesmo com a função, será necessário acrescentar linhas de código para padronizar os nomes das subprefeituras

#PdM 2021-2024: gerando um tibble subprefeitura x valores
(pdm <- readxl::read_xlsx("Planilha IDRGP.xlsx",
                          sheet = "Monitoramento - Metas IDRGP")) 

dplyr::glimpse(pdm)

(pdm <- readxl::read_xlsx("Planilha IDRGP.xlsx",
                          sheet = "Monitoramento - Metas IDRGP",
                          skip = 5) %>% 
    janitor::clean_names() %>% 
    filter(!is.na(liquidacao_2022)) %>% 
    mutate(meta = paste0("meta ", meta)))

(pdm_w <- pdm %>% 
    tidyr::pivot_wider(
      names_from = meta,
      values_from = liquidacao_2022)) 

(pdm_w <- pdm_w %>%
    dplyr::mutate(liquidacao_2022 = base::rowSums(pdm_w[ , 7:28], na.rm = TRUE)))

# gerando tibble para gerar mapa e classificando liquidado_2022 por quintis
(bd_pdm_mapa <- pdm_w %>%
    dplyr::select(subprefeitura, liquidacao_2022) %>% 
    group_by(subprefeitura) %>% 
    summarise(liquidacao_2022 = sum(liquidacao_2022)) %>% 
    mutate(perc = base::prop.table(liquidacao_2022)) %>%
    mutate(quintis = ntile(perc, 5)) %>% 
    rename(sp_nome = subprefeitura) %>% 
    arrange(sp_nome))

(bd_pdm_mapa$sp_nome <- padronizar_nomes(bd_pdm_mapa$sp_nome))
subprefeitura$sp_nome #comparando as duas linhas, observa-se divergência:
## aricanduva, casa verde, freguesia, mboi mirim, além da ordem entre as subs

bd_pdm_mapa$sp_nome[1] <- subprefeitura$sp_nome[1]
bd_pdm_mapa$sp_nome[5] <- subprefeitura$sp_nome[5]
bd_pdm_mapa$sp_nome[6] <- subprefeitura$sp_nome[6]
bd_pdm_mapa$sp_nome[7] <- subprefeitura$sp_nome[7]
bd_pdm_mapa$sp_nome[9] <- subprefeitura$sp_nome[9]
bd_pdm_mapa$sp_nome[18] <- subprefeitura$sp_nome[17]

(bd_pdm_mapa <- dplyr::left_join(bd_pdm_mapa, subprefeitura, by = "sp_nome"))

(bd_pdm_mapa <- bd_pdm_mapa %>%
    mutate(geometry = subprefeitura$geometry) %>% 
    sf::st_as_sf() %>% 
    st_transform(crs = "EPSG:4326"))

glimpse(bd_pdm_mapa)
str(bd_pdm_mapa)

# Criando colunas com os valores abrangidos nos intervalos de classe de cada quintil

(intervalos_pdm <- bd_pdm_mapa %>%
    summarise(
      Q1 = quantile(liquidacao_2022, probs = 0.20),
      Q2 = quantile(liquidacao_2022, probs = 0.40),
      Q3 = quantile(liquidacao_2022, probs = 0.60),
      Q4 = quantile(liquidacao_2022, probs = 0.80),
      Q5 = quantile(liquidacao_2022, probs = 1)
    ))

(bd_pdm_mapa <- bd_pdm_mapa %>% 
    mutate("Liquidação 2022" = case_when(
      quintis <= 1 ~ "0% a 20% (R$ 0 a R$ 2,9 mi)",
      quintis > 1 & quintis <= 2 ~ "20% a 40% (R$ 2,9 a R$ 11,4 mi)",
      quintis > 2 & quintis <= 3 ~ "40% a 60% (R$ 11,4 a R$ 24,6 mi)",
      quintis > 3 & quintis <= 4 ~ "60% a 80% (R$ 24,6 a R$ 53,1 mi)",
      quintis > 4  ~ "80% a 100% (R$ 53,1 a R$ 233,1 mi)")))

sum(bd_pdm_mapa$liquidacao_2022)

# teste do mapa
bd_pdm_mapa %>% 
  ggplot()+
  geom_sf(aes(fill = `Liquidação 2022`, geometry = geometry)) +
  scale_fill_manual(values = c("#7FC3FE", "#2B9CFE", "#086EC8", "#0A447F", "#1A1449")) +
  geom_sf_text(aes(label = siglas), color = "white", size = 2, fontface = "bold") +
  theme_minimal() +
  labs(title = "Distribuição regional dos gastos do Programa de Metas 2021-2024*",
       subtitle = "* Valores liquidados em 2022 (restos a pagar até maio/23), em metas selecionadas: R$ 1,185 bilhão",
       fill = "Quintis",
       caption = "Fonte: PMSP/SOF; SMADS; SME; SMS. Elaboração: SGM/SEPEP/CP.",
       x = NULL, y = NULL)


#orcamento cidadao
(o_c <- readxl::read_xlsx("Planilha IDRGP.xlsx",
                          sheet = "orcamento_cidadao") %>% 
    dplyr::filter(!is.na(`Liquidação 2022`) & `IDRGP` == "Sim") %>%
    janitor::clean_names() %>% 
    dplyr::select(subprefeitura, liquidacao_2022) %>% 
    dplyr::arrange(subprefeitura))

(o_c$subprefeitura <- padronizar_nomes(o_c$subprefeitura))
subprefeitura$sp_nome ## comparando, necessário ajustar m'boi mirim

(o_c$subprefeitura[4] <- subprefeitura$sp_nome[17])


(o_c <- o_c %>%
    mutate(liquidacao_2022 = as.numeric(o_c$liquidacao_2022, na.rm = TRUE)) %>% 
    rename(sp_nome = subprefeitura))


# gerando tibble para gerar mapa e classificando liquidado_2022 por quintis
(o_c_mapa <- dplyr::full_join(o_c, subprefeitura, by = NULL))

(o_c_mapa$liquidacao_2022 <- o_c_mapa$liquidacao_2022 %>% 
    tidyr::replace_na(0)) #para substituir NA por 0

(o_c_mapa <- o_c_mapa %>% 
    mutate(perc = base::prop.table(liquidacao_2022)) %>%
    mutate(quintis = ntile(perc, 5)) %>% 
    select(sp_nome, sigla, liquidacao_2022, perc, quintis, geometry) %>% 
    st_as_sf() %>% 
    st_transform(crs = "EPSG:4326"))

str(o_c_mapa)


# Criando colunas com os valores abrangidos nos intervalos de classe de cada quintil

(intervalos_o_c <- o_c_mapa %>%
    summarise(
      Q1 = quantile(liquidacao_2022, probs = 0.20),
      Q2 = quantile(liquidacao_2022, probs = 0.40),
      Q3 = quantile(liquidacao_2022, probs = 0.60),
      Q4 = quantile(liquidacao_2022, probs = 0.80),
      Q5 = quantile(liquidacao_2022, probs = 1)
    ))

(o_c_mapa <- o_c_mapa %>% 
    mutate("Liquidação 2022" = case_when(
      quintis <= 1 ~ "0% a 20% (R$ 0)",
      quintis > 1 & quintis <= 2 ~ "20% a 40% (R$ 0)",
      quintis > 2 & quintis <= 3 ~ "40% a 60% (R$ 0)",
      quintis > 3 & quintis <= 4 ~ "60% a 80% (R$ 0)",
      quintis > 4  ~ "80% a 100% (R$ 202,1 mil a R$ 1,0 mi)")))

sum(o_c_mapa$liquidacao_2022)

#teste mapa
o_c_mapa %>% 
  ggplot() +
  geom_sf(aes(fill = `Liquidação 2022`)) +
  geom_sf_text(aes(label = sigla))

#mapa
o_c_mapa %>%
  ggplot()+
  geom_sf(aes(fill = `Liquidação 2022`, geometry = geometry)) +
  scale_fill_manual(values = c("#7FC3FE", "#7FC3FE", "#7FC3FE", "#7FC3FE", "#1A1449")) +
  geom_sf_text(aes(label = sigla), color = "white", size = 2, fontface = "bold") + 
  theme_minimal() +
  labs(title = "Distribuição regional dos gastos do Orçamento Cidadão*",
       subtitle = "* Valores liquidados em 2022, em projetos selecionados: R$ 2,197 milhões",
       fill = "Quintis",
       caption = "Fonte: SF/SUPOM/DIAPRI. Elaboração: SGM/SEPEP/CP.",
       x = NULL, y = NULL)


#obras prioritarias
(obras <- readxl::read_xlsx("Planilha IDRGP.xlsx",
                            sheet = "obras_prioritarias"))

dplyr::glimpse(obras)

(obras <- readxl::read_xlsx("Planilha IDRGP.xlsx",
                            sheet = "obras_prioritarias") %>% 
    janitor::clean_names() %>% 
    dplyr::filter(idrgp == "IDRGP" & !is.na(valor_empenho_liquidado_proxy)) %>% 
    rename(liquidacao_2022 = valor_empenho_liquidado_proxy))

sum(obras$liquidacao_2022)

(obras$subprefeitura <- padronizar_nomes(obras$subprefeitura))


(obras_mapa <- obras %>% 
    select(subprefeitura, liquidacao_2022) %>%
    filter(!is.na(subprefeitura)) %>% 
    group_by(subprefeitura) %>% 
    summarise(liquidacao_2022 = sum(liquidacao_2022)) %>% 
    rename(sp_nome = subprefeitura))

obras_mapa$sp_nome
subprefeitura$sp_nome ## comparando, necessário ajustar ARICANDUVA, MBOI, SAO MIGUEL

(obras_mapa$sp_nome[1] <- subprefeitura$sp_nome[1])
(obras_mapa$sp_nome[6] <- subprefeitura$sp_nome[6])
(obras_mapa$sp_nome[7] <- subprefeitura$sp_nome[7])
(obras_mapa$sp_nome[17] <- subprefeitura$sp_nome[17])
(obras_mapa$sp_nome[27] <- subprefeitura$sp_nome[27])

(obras_mapa <- dplyr::full_join(subprefeitura, obras_mapa, by = NULL))

sum(obras_mapa$liquidacao_2022)

(obras_mapa$liquidacao_2022 <- obras_mapa$liquidacao_2022 %>% 
    tidyr::replace_na(0))


(obras_mapa <- obras_mapa %>% 
    mutate(perc = base::prop.table(liquidacao_2022)) %>% 
    mutate(quintis = ntile(perc, 5)) %>% 
    select(sp_nome, sigla, liquidacao_2022, perc, quintis, geometry) %>% 
    st_as_sf() %>% 
    st_transform(crs = "EPSG:4326"))


(intervalos_obras <- obras_mapa %>%
    summarise(
      Q1 = quantile(liquidacao_2022, probs = 0.20),
      Q2 = quantile(liquidacao_2022, probs = 0.40),
      Q3 = quantile(liquidacao_2022, probs = 0.60),
      Q4 = quantile(liquidacao_2022, probs = 0.80),
      Q5 = quantile(liquidacao_2022, probs = 1)
    ))

(obras_mapa <- obras_mapa %>% 
    mutate("Liquidação 2022" = case_when(
      quintis <= 1 ~ "0% a 20% (R$ 0 a R$ 9,5 mi",
      quintis > 1 & quintis <= 2 ~ "20% a 40% (R$ 9,5 mi a R$ 20,6 mi)",
      quintis > 2 & quintis <= 3 ~ "40% a 60% (R$ 20,6 a R$ 27,8 mi)",
      quintis > 3 & quintis <= 4 ~ "60% a 80% (R$ 27,8 a R$ 38,9 mi)",
      quintis > 4  ~ "80% a 100% (R$ 38,9 mil a R$ 217,5 mi)")))

sum(obras_mapa$liquidacao_2022)

#mapa
obras_mapa %>%
  ggplot()+
  geom_sf(aes(fill = `Liquidação 2022`, geometry = geometry)) +
  scale_fill_manual(values = c("#7FC3FE", "#2B9CFE", "#086EC8", "#0A447F", "#1A1449")) +
  geom_sf_text(aes(label = sigla), color = "white", size = 2, fontface = "bold") + 
  theme_minimal() +
  labs(title = "Distribuição regional dos gastos com Obras Monitoradoas*",
       subtitle = "* Valores liquidados em 2022, em projetos selecionados: R$ 1,231 bilhão",
       fill = "Quintis",
       caption = "Fonte: PMSP/SOF; SGM/SEPEP/Unidade de Entregas. Elaboração: SGM/SEPEP/CP.",
       x = NULL, y = NULL)

#análises obras
obras %>% 
  group_by(subprefeitura, secretaria) %>% 
  summarise(sum(liquidacao_2022)) %>% 
  arrange(desc(`sum(liquidacao_2022)`))

obras %>% 
  group_by(secretaria, subprefeitura) %>% 
  summarise(sum(liquidacao_2022)) %>% 
  arrange(desc(`sum(liquidacao_2022)`))


#base do detalhamento da ação
(da <- readxl::read_xlsx("Planilha IDRGP.xlsx",
                         sheet = "base_da"))

glimpse(da)

(da <- readxl::read_xlsx("Planilha IDRGP.xlsx",
                         sheet = "base_da",
                         range = "A1:H33") %>% 
    janitor::clean_names() %>% 
    dplyr::rename(sp_nome = rotulos_de_linha,
                  liquidacao_2022 = total_com_rateio))

(da$sp_nome <- padronizar_nomes(da$sp_nome))

(subprefeitura$sp_nome) #corrigir cidade ademar, cidade tiradentes, mboi, perus, sao miguel

(da$sp_nome[6] <- subprefeitura$sp_nome[6])
(da$sp_nome[7] <- subprefeitura$sp_nome[7])
(da$sp_nome[18] <- subprefeitura$sp_nome[17])
(da$sp_nome[22] <- subprefeitura$sp_nome[21])
(da$sp_nome[28] <- subprefeitura$sp_nome[27])

(da_mapa <- dplyr::full_join(subprefeitura, da, by = NULL))

glimpse(da)

(da_mapa <- da_mapa %>% 
    mutate(perc = base::prop.table(liquidacao_2022)) %>% 
    mutate(quintis = ntile(perc, 5)) %>% 
    select(sp_nome, liquidacao_2022, perc, quintis, geometry) %>% 
    st_as_sf() %>% 
    st_transform(crs = "EPSG:4326"))

(intervalos_da <- da %>%
    summarise(
      Q1 = quantile(liquidacao_2022, probs = 0.20),
      Q2 = quantile(liquidacao_2022, probs = 0.40),
      Q3 = quantile(liquidacao_2022, probs = 0.60),
      Q4 = quantile(liquidacao_2022, probs = 0.80),
      Q5 = quantile(liquidacao_2022, probs = 1)
    ))


(da_mapa <- da_mapa %>% 
    mutate("Liquidação 2022" = case_when(
      quintis <= 1 ~ "0% a 20% (R$ 0 a R$ 429,3 mi)",
      quintis > 1 & quintis <= 2 ~ "20% a 40% (R$ 429,3 a R$ 520,2 mi)",
      quintis > 2 & quintis <= 3 ~ "40% a 60% (R$ 520,2 a R$ 657,1 mi)",
      quintis > 3 & quintis <= 4 ~ "60% a 80% (R$ 657,1 a R$ 757,0 mi)",
      quintis > 4  ~ "80% a 100% (R$ 757,0 mi a R$ 3,978 bi)")))

sum(da$liquidacao_2022)

da_mapa %>%
  ggplot()+
  geom_sf(aes(fill = `Liquidação 2022`, geometry = geometry)) +
  scale_fill_manual(values = c("#7FC3FE", "#2B9CFE", "#086EC8", "#0A447F", "#1A1449")) +
  geom_sf_text(aes(label = siglas), color = "white", size = 2, fontface = "bold") + 
  theme_minimal() +
  labs(title = "Distribuição regional dos investimentos e custeios da PMSP*",
       subtitle = "* Valores liquidados em 2022, regilonalizados pelo Detalhamento da Ação (DA): R$ 22,981 bi",
       fill = "Quintis",
       caption = "Fonte: PMSP/SOF. Elaboração: SGM/SEPEP/CP.",
       x = NULL, y = NULL)

# Acrescentando DA executado per capta

(pop_sp$sp_nome <- padronizar_nomes(pop_sp$sp_nome))

(pop_sp$sp_nome <- gsub(" -SUBPREFEITURA", "", pop_sp$sp_nome))
subprefeitura$sp_nome #corrigir cidade ademar, cidade tiradentes, mboi

(pop_sp$sp_nome[6] <- subprefeitura$sp_nome[6])
(pop_sp$sp_nome[7] <- subprefeitura$sp_nome[7])
(pop_sp$sp_nome[17] <- subprefeitura$sp_nome[17])

(da_pc <- dplyr::full_join(da_mapa, pop_sp, by = NULL))

(da_pc <- da_pc %>% 
    rename(populacao = resultado) %>% 
    mutate(gasto_per_capta = liquidacao_2022 / populacao) %>%
    mutate(perc = base::prop.table(gasto_per_capta)) %>% 
    mutate(quintis = ntile(perc, 5)) %>% 
    select(sp_nome, liquidacao_2022, populacao, gasto_per_capta, perc, quintis))

(intervalos_da_pc <- da_pc %>%
    summarise(
      Q1 = quantile(gasto_per_capta, probs = 0.20),
      Q2 = quantile(gasto_per_capta, probs = 0.40),
      Q3 = quantile(gasto_per_capta, probs = 0.60),
      Q4 = quantile(gasto_per_capta, probs = 0.80),
      Q5 = quantile(gasto_per_capta, probs = 1)
    ))


(da_pc <- da_pc %>% 
    mutate("Liquidação per capta 2022" = case_when(
      quintis <= 1 ~ "0% a 20% (R$ 0 a R$ 1,362 mil)",
      quintis > 1 & quintis <= 2 ~ "20% a 40% (R$ 1,362 a R$ 1,442 mil)",
      quintis > 2 & quintis <= 3 ~ "40% a 60% (R$ 1,442 a R$ 1,818 mil)",
      quintis > 3 & quintis <= 4 ~ "60% a 80% (R$ 1,818 a R$ 2,101 mil)",
      quintis > 4  ~ "80% a 100% (R$ 2,101 a R$ 8,628 mil)")))

da_pc %>%
  ggplot()+
  geom_sf(aes(fill = `Liquidação per capta 2022`, geometry = geometry)) +
  scale_fill_manual(values = c("#7FC3FE", "#2B9CFE", "#086EC8", "#0A447F", "#1A1449")) +
  geom_sf_text(aes(label = siglas), color = "white", size = 2, fontface = "bold") + 
  theme_minimal() +
  labs(title = "Distribuição regional dos investimentos e custeios da PMSP*, per capta",
       subtitle = "* Valores liquidados em 2022, per capta, regilonalizados pelo Detalhamento da Ação (DA)",
       fill = "Quintis",
       caption = "Fonte: PMSP/SOF; Fundação Sistema Estadual de Análise de Dados (SEADE); Observasampa. Elaboração: SGM/SEPEP/CP.",
       x = NULL, y = NULL)


#referencia do idrgp
(ref_idrgp <- read_xlsx("Planilha IDRGP.xlsx",
                        sheet = "ref_idrgp"))

glimpse(ref_idrgp)

(ref_idrgp <- read_xlsx("Planilha IDRGP.xlsx",
                        sheet = "ref_idrgp") %>% 
    janitor::clean_names() %>% 
    dplyr::arrange(subprefeitura) %>% 
    rename(sp_nome = subprefeitura))

ref_idrgp$sp_nome
subprefeitura$sp_nome # comparando, ajustar são mateus, são miguel e sapopemba

(ref_idrgp$sp_nome[1:25] <- subprefeitura$sp_nome[1:25])
(ref_idrgp$sp_nome[26] <- subprefeitura$sp_nome[28])
(ref_idrgp$sp_nome[27] <- subprefeitura$sp_nome[26])
(ref_idrgp$sp_nome[28] <- subprefeitura$sp_nome[27])
(ref_idrgp$sp_nome[29:32] <- subprefeitura$sp_nome[29:32])

(ref_idrgp_mapa <- dplyr::full_join(subprefeitura, ref_idrgp, by = NULL))

(ref_idrgp_mapa <- ref_idrgp_mapa %>% 
    mutate(perc = base::prop.table(valor_por_ano)) %>% 
    mutate(quintis = ntile(perc, 5)) %>% 
    select(sp_nome, valor_por_ano, perc, quintis, geometry) %>% 
    st_as_sf() %>% 
    st_transform(crs = "EPSG:4326"))

(intervalos_idrgp_ref <- ref_idrgp_mapa %>%
    summarise(
      Q1 = quantile(valor_por_ano, probs = 0.20),
      Q2 = quantile(valor_por_ano, probs = 0.40),
      Q3 = quantile(valor_por_ano, probs = 0.60),
      Q4 = quantile(valor_por_ano, probs = 0.80),
      Q5 = quantile(valor_por_ano, probs = 1)
    ))
# deu erro:
## Error in wk_handle.wk_wkb(wkb, s2_geography_writer(oriented = oriented,  : 
## Loop 0 is not valid: Edge 438 crosses edge 889
## In addition: Warning messages:
##  1: In st_is_longlat(x) :
##  bounding box has potentially an invalid value range for longlat data
## 2: In st_is_longlat(x) :
##  bounding box has potentially an invalid value range for longlat data

quantile(ref_idrgp_mapa$valor_por_ano, probs = 0.20)
quantile(ref_idrgp_mapa$valor_por_ano, probs = 0.40)
quantile(ref_idrgp_mapa$valor_por_ano, probs = 0.60)
quantile(ref_idrgp_mapa$valor_por_ano, probs = 0.80)
quantile(ref_idrgp_mapa$valor_por_ano, probs = 1)

(ref_idrgp_mapa <- ref_idrgp_mapa %>% 
    mutate("Referência de gasto" = case_when(
      quintis <= 1 ~ "0% a 20% (R$ 0 a R$ 18,9 mi)",
      quintis > 1 & quintis <= 2 ~ "20% a 40% (R$ 18,9 a R$ 31,7 mi)",
      quintis > 2 & quintis <= 3 ~ "40% a 60% (R$ 31,7 a R$ 43,8 mi)",
      quintis > 3 & quintis <= 4 ~ "60% a 80% (R$ 43,8 a R$ 56,1 mi)",
      quintis > 4  ~ "80% a 100% (R$ 56,1 a R$ 88,4 mil)")))

ref_idrgp_mapa %>%
  ggplot()+
  geom_sf(aes(fill = `Referência de gasto`, geometry = geometry)) +
  scale_fill_manual(values = c("#7FC3FE", "#2B9CFE", "#086EC8", "#0A447F", "#1A1449")) +
  geom_sf_text(aes(label = siglas), color = "white", size = 2, fontface = "bold") + 
  theme_minimal() +
  labs(title = "Previsão de Distribuição Regional do Gasto Público*",
       subtitle = "* R$ 1,250 bi",
       fill = "Quintis",
       caption = "Fonte: Lei nº 17.729/2021 (PPA 2022-2025). Elaboração: SGM/SEPEP/CP.",
       x = NULL, y = NULL)

## Gráfico

(ref_idrgp_mapa <- ref_idrgp_mapa %>% 
  arrange(sp_nome))

(bd_pdm_mapa <- bd_pdm_mapa %>% 
  arrange(sp_nome))

(obras_mapa <- obras_mapa %>% 
  arrange(sp_nome))

(o_c_mapa <- o_c_mapa %>% 
  arrange(sp_nome))

(gp_idrgp <- base::cbind(ref_idrgp_mapa, bd_pdm_mapa, obras_mapa, o_c_mapa) %>% 
    select(sp_nome, valor_por_ano, liquidacao_2022, liquidacao_2022.1, liquidacao_2022.2) %>% 
    rename(liq_pdm = liquidacao_2022,
           liq_obras = liquidacao_2022.1,
           liq_oc = liquidacao_2022.2) %>% 
    mutate(idrgp_2022 = liq_pdm + liq_obras + liq_oc) %>% 
    mutate(diferenca = idrgp_2022 - valor_por_ano))

sum(gp_idrgp$liq_obras)

glimpse(gp_idrgp)

gp_idrgp %>% 
  mutate("Status" = case_when(
    gp_idrgp$diferenca < 0 ~ "Execução MENOR do que a referência pelo IDRGP",
    gp_idrgp$diferenca >= 0 ~ "Execução MAIOR do que a referência pelo IDRGP"
  )) %>% 
  ggplot() +
  geom_col(aes(x = idrgp_2022,
               y = forcats::fct_reorder(sp_nome, idrgp_2022),
               fill = Status),
           show.legend = TRUE) +
  scale_fill_manual(values = c("#0A447F", "orange")) +
  scale_x_continuous(name = NULL,
                     labels = scales::comma_format(big.mark = ".",
                                                   decimal.mark = ",")) +
  geom_point(mapping = aes(x = valor_por_ano,
                           y = sp_nome,
                           colour = "Referência pelo IDRGP"),
             show.legend = TRUE) +
  labs(title = "IDRGP 2022",
       y = "") +
  theme_minimal()+
  theme(legend.title = element_blank(),
        legend.position = "bottom")


gp_idrgp %>% 
  mutate("Status" = case_when(
    gp_idrgp$diferenca < 0 ~ "Execução MENOR do que a referência pelo IDRGP",
    gp_idrgp$diferenca >= 0 ~ "Execução MAIOR do que a referência pelo IDRGP"
  )) %>% 
  ggplot() +
  geom_col(aes(x = idrgp_2022,
               y = forcats::fct_reorder(sp_nome, valor_por_ano),
               fill = Status),
           show.legend = TRUE) +
  scale_fill_manual(values = c("#0A447F", "orange")) +
  scale_x_continuous(name = NULL,
                     labels = scales::comma_format(big.mark = ".",
                                                   decimal.mark = ",")) +
  geom_point(mapping = aes(x = valor_por_ano,
                           y = sp_nome,
                           colour = "Referência pelo IDRGP"),
             show.legend = TRUE) +
  labs(title = "IDRGP 2022",
       y = "") +
  theme_minimal()+
  theme(legend.title = element_blank(),
        legend.position = "bottom")

gp_idrgp %>% write.csv2("planilha_idrgp.csv")

names(gp_idrgp)

(gp_idrgp_print <- as_tibble(gp_idrgp) %>%  
    select(-geometry) %>% 
    write.csv2("planilha_idrgp.csv"))



## Análise temática do gasto público

### padronizar e juntar bases pdm, o_c e obras, para entender o padrão de liquidação por órgão e por região.

#pdm
print(geral_pdm <- pdm %>% 
    select(subprefeitura, secretaria_responsavel, liquidacao_2022) %>% 
    rename (sp_nome = subprefeitura,
            secretaria = secretaria_responsavel) %>% 
    group_by(sp_nome, secretaria) %>%
    filter(liquidacao_2022 != 0) %>%
    summarise(liquidacao_2022 = sum(liquidacao_2022)) %>% 
    mutate(tipo = "Programa de Metas 2021-2024"), n = 167)


(geral_pdm$sp_nome <- padronizar_nomes(geral_pdm$sp_nome)) #ajustes: aricanduva, casa verde, cidade ademar, cidade tiradentes, frequesia e NA [92:97]

(geral_pdm <- geral_pdm %>% 
  mutate(sp_nome = case_when(
    sp_nome == "ARICANDUVA" ~ "ARICANDUVA-FORMOSA-CARRAO",
    sp_nome == "ARICANDUVA-CARRAO-FORMOSA" ~ "ARICANDUVA-FORMOSA-CARRAO",
    sp_nome == "CIDAADEMAR" ~ "CIDADE ADEMAR",
    sp_nome == "CASA VERDE" ~ "CASA VERDE-CACHOEIRINHA",
    sp_nome == "CIDATIRADENTES" ~ "CIDADE TIRADENTES",
    sp_nome == "FREGUESIA DO O" ~ "FREGUESIA-BRASILANDIA",
    sp_nome == "M-BOI MIRIM" ~ "M'BOI MIRIM",
    sp_nome == "SAO MIGUEL PAULISTA" ~ "SAO MIGUEL",
    TRUE ~ sp_nome)))

geral_pdm$sp_nome[92:97] <- "M'BOI MIRIM"

print(geral_pdm, n=167)

sum(geral_pdm$liquidacao_2022)
sum(pdm$liquidacao_2022)
sum(gp_idrgp$liq_pdm)

geral_pdm %>% 
  group_by(secretaria) %>% 
  summarise (liquidacao_2022 = sum(liquidacao_2022)) %>% 
  mutate("%" = liquidacao_2022/sum(liquidacao_2022)) %>% 
  arrange(desc(liquidacao_2022))

#obras
(geral_obras <- obras %>% 
    select(subprefeitura,secretaria, liquidacao_2022) %>% 
    rename(sp_nome = subprefeitura) %>% 
    filter(liquidacao_2022 != 0) %>% 
    group_by(sp_nome, secretaria) %>% 
    summarise(liquidacao_2022 = sum(liquidacao_2022)) %>% 
    mutate(tipo = "Obras Monitoradas"))

sum(geral_obras$liquidacao_2022)
sum(obras$liquidacao_2022)
sum(gp_idrgp$liq_obras)

geral_obras %>% 
  filter(is.na(liquidacao_2022))

geral_obras$sp_nome

(geral_obras$sp_nome <- padronizar_nomes(geral_obras$sp_nome)) #ajustes: aricanduva, cidade ademar, cidade tiradentes, são miguel

(geral_obras <- geral_obras %>% 
    mutate(sp_nome = case_when(
      sp_nome == "ARICANDUVA" ~ "ARICANDUVA-FORMOSA-CARRAO",
      sp_nome == "ARICANDUVA-CARRAO-FORMOSA" ~ "ARICANDUVA-FORMOSA-CARRAO",
      sp_nome == "CIDAADEMAR" ~ "CIDADE ADEMAR",
      sp_nome == "CASA VERDE" ~ "CASA VERDE-CACHOEIRINHA",
      sp_nome == "CIDATIRADENTES" ~ "CIDADE TIRADENTES",
      sp_nome == "FREGUESIA DO O" ~ "FREGUESIA-BRASILANDIA",
      sp_nome == "M-BOI MIRIM" ~ "M'BOI MIRIM",
      sp_nome == "SAO MIGUEL PAULISTA" ~ "SAO MIGUEL",
      TRUE ~ sp_nome)))

(analise_obras <- obras %>% 
  group_by(secretaria, liquidacao_2022) %>% 
  filter(idrgp == "IDRGP") %>% 
  select(secretaria, obra_intervencao, tipo_de_obra_intervencao, subprefeitura, liquidacao_2022) %>% 
  arrange(desc(liquidacao_2022)))

(analise_obras %>% 
  group_by(secretaria) %>% 
  summarise(liquidacao_2022 = sum(liquidacao_2022)) %>% 
  arrange(desc(liquidacao_2022)) %>% 
  mutate("%" = liquidacao_2022/sum(liquidacao_2022)))


#orçamento cidadão
(geral_oc <- readxl::read_xlsx("Planilha IDRGP.xlsx",
                               sheet = "orcamento_cidadao") %>%
    dplyr::filter(!is.na(`Liquidação 2022`) & `IDRGP` == "Sim") %>%
    janitor::clean_names() %>% 
    dplyr::select(subprefeitura, secretaria, liquidacao_2022) %>% 
    dplyr::arrange(subprefeitura) %>% 
    rename(sp_nome = subprefeitura) %>%
    filter(liquidacao_2022 != 0) %>% 
    mutate(tipo = "Orçamento Cidadão 2022"))

(geral_oc$sp_nome <- padronizar_nomes(geral_oc$sp_nome))

(geral_oc$liquidacao_2022 <- as.numeric(geral_oc$liquidacao_2022))

geral_oc %>% 
  group_by(secretaria) %>% 
  summarise(liquidacao_2022 = sum(liquidacao_2022)) %>% 
  mutate("%" = liquidacao_2022/sum(liquidacao_2022)) %>% 
  arrange(desc(liquidacao_2022))

sum(geral_oc$liquidacao_2022)
sum(o_c$liquidacao_2022)
sum(gp_idrgp$liq_oc)

(geral_oc$sp_nome <- padronizar_nomes(geral_oc$sp_nome))


(geral <- dplyr::bind_rows(geral_pdm, geral_obras, geral_oc))

#chegagem se houve problemas na junção das bases
sum(geral$liquidacao_2022) == sum(gp_idrgp$idrgp_2022)

(geral %>% 
  arrange(desc(liquidacao_2022)))

(geral$secretaria <- reorder(geral$secretaria, -geral$liquidacao_2022))

#gráfico
ggplot(geral, aes(x = secretaria, y = liquidacao_2022, fill = tipo)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Órgão", y = "Valor Liquidado") +
  facet_wrap(~sp_nome, scales = "free_x") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = c(0, 220500000))

## ajustes:
# subprefeituras com problemas na nomenclatura: ARICANDUVA, CASA VERDE, CIDADE ADEMAR, CIDADE TIRADENTES, FREGUESIA, SÃO MIGUEL
# incluir linha com a referência idrgp
# separar em 3 blocos: 10s primeiros/ 10 útlimos / 12 intermediários ref. idrgp
# avaliar se com escala logarítmica a visualização fica melhor


# agrupando as subprefeituras pelo ranking do idrgp (10-12-10)

(gp_idrgp <- gp_idrgp %>%
    arrange(desc(valor_por_ano)) %>% 
    mutate(grupo = c(rep(1, 10), c(rep(2, 12), rep(3,10)))))

(geral <- geral %>%
    left_join(gp_idrgp, by = c("sp_nome" = "sp_nome")))


##gráfico grupo 1:
(geral_g1 <- geral %>% 
    filter(grupo == 1))

(geral_g1$secretaria <- reorder(geral_g1$secretaria, -geral_g1$liquidacao_2022))

ggplot(geral_g1, aes(x = secretaria, y = liquidacao_2022, fill = tipo)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_line(aes(x = secretaria, y = valor_por_ano, group = sp_nome, color = "Referência IDRGP"), size = 0.25, linetype = "dashed") +
  geom_line(aes(x = secretaria, y = idrgp_2022, group = sp_nome, color = "Liquidado 2022"), size = 0.25, linetype = "dashed") +
  labs(x = NULL, y = NULL) +
  facet_wrap(~sp_nome, scales = "free_x") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
       panel.grid.major = element_blank(),
       panel.grid.minor = element_blank(),
       legend.position = "bottom") +
  labs(fill = NULL, color = NULL) + 
  scale_color_manual(values = c("Referência IDRGP" = "#0A447F", "Liquidado 2022" = "orange"))


##gráfico grupo 2:
(geral_g2 <- geral %>% 
    filter(grupo == 2))

(geral_g2$secretaria <- reorder(geral_g2$secretaria, -geral_g2$liquidacao_2022))

ggplot(geral_g2, aes(x = secretaria, y = liquidacao_2022, fill = tipo)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_line(aes(x = secretaria, y = valor_por_ano, group = sp_nome, color = "Referência IDRGP"), size = 0.25, linetype = "dashed") +
  geom_line(aes(x = secretaria, y = idrgp_2022, group = sp_nome, color = "Liquidado 2022"), size = 0.25, linetype = "dashed") +
  labs(x = NULL, y = NULL) +
  facet_wrap(~sp_nome, scales = "free_x") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom") +
  labs(fill = NULL, color = NULL) + 
  scale_color_manual(values = c("Referência IDRGP" = "#0A447F", "Liquidado 2022" = "orange"))

##gráfico grupo 3:
(geral_g3 <- geral %>% 
    filter(grupo == 3))

(geral_g3$secretaria <- reorder(geral_g3$secretaria, -geral_g3$liquidacao_2022))

ggplot(geral_g3, aes(x = secretaria, y = liquidacao_2022, fill = tipo)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_line(aes(x = secretaria, y = valor_por_ano, group = sp_nome, color = "Referência IDRGP"), size = 0.25, linetype = "dashed") +
  geom_line(aes(x = secretaria, y = idrgp_2022, group = sp_nome, color = "Liquidado 2022"), size = 0.25, linetype = "dashed") +
  labs(x = NULL, y = NULL) +
  facet_wrap(~sp_nome, scales = "free_x") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom") +
  labs(fill = NULL, color = NULL) + 
  scale_color_manual(values = c("Referência IDRGP" = "#0A447F", "Liquidado 2022" = "orange"))

## Gráfico da distribuição territorial dos órgãos que mais investiram

(liq_org <- geral %>% 
  group_by(sp_nome, secretaria) %>% 
  summarise(liq_por_orgao = sum(liquidacao_2022)) %>% 
    mutate(perc = base::prop.table(liq_por_orgao)) %>% 
    mutate(quintis = ntile(perc, 5)) %>% 
    left_join(gp_idrgp, by = "sp_nome") %>% 
    left_join(subprefeitura, by = "sp_nome") %>% 
    select(-geometry.y) %>% 
    rename(geometry = geometry.x))


liq_org %>%
  summarise(
    Q1 = quantile(liq_por_orgao, probs = 0.20),
    Q2 = quantile(liq_por_orgao, probs = 0.40),
    Q3 = quantile(liq_por_orgao, probs = 0.60),
    Q4 = quantile(liq_por_orgao, probs = 0.80),
    Q5 = quantile(liq_por_orgao, probs = 1))


liq_org %>% 
  filter(is.na(liq_por_orgao))

glimpse(liq_org)
glimpse(gp_idrgp)

sum(liq_org$liq_por_orgao)
sum(gp_idrgp$idrgp_2022)


liq_org %>%
  ggplot() +
  geom_sf(aes(fill = quintis, geometry = geometry)) +
  geom_sf_text(aes(label = sigla), color = "black", size = 1.5, fontface = "bold")+
  facet_wrap(~secretaria) +
  theme_minimal()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")

## Ajustes no gráfico: remover eixos x e y, adaptar objeto para que a geometria de todasas subprefeituras seja reproduzida

(liq_org %>% 
  group_by(secretaria, sp_nome) %>% 
  arrange(secretaria))

(bd_org <- as_tibble(unique(liq_org$secretaria)) %>% 
    rename(secretaria = value) %>% 
    tidyr::uncount(32) %>% 
    mutate(sp_nome = rep(bd_sp_nome, 15)))

(bd_org$secretaria <- as.character(bd_org$secretaria))

(bd_org <- bd_org %>% 
    mutate(chave = paste(secretaria, sp_nome, sep = "-")))

(liq_org <- liq_org %>% 
  mutate(chave = paste(secretaria, sp_nome, sep = "-")))

(bd_org <- bd_org %>% 
  left_join(liq_org, by = "chave"))

names(bd_org)

(bd_org <- bd_org %>% 
    select(secretaria.x, sp_nome.x, liq_por_orgao, valor_por_ano, idrgp_2022, diferenca) %>% 
    rename(secretaria = secretaria.x,
           sp_nome = sp_nome.x) %>% 
    left_join(subprefeitura, by = "sp_nome") %>% 
    mutate(liq_por_orgao = replace_na(liq_por_orgao, 0)) %>%
    mutate(perc = base::prop.table(liq_por_orgao)) %>% 
    mutate(quintis = ntile(perc, 5)) %>% 
    mutate("Quintis" = case_when(
      quintis <= 1 ~ "1º",
      quintis > 1 & quintis <= 2 ~ "2º",
      quintis > 2 & quintis <= 3 ~ "3º",
      quintis > 3 & quintis <= 4 ~ "4º",
      quintis > 4  ~ "5º")) %>% 
    sf::st_as_sf() %>% 
    st_transform(crs = "EPSG:4326"))

str(bd_org)

# gráfico ajustado
bd_org %>%
  ggplot() +
  geom_sf(aes(fill = Quintis, geometry = geometry)) +
  geom_sf_text(aes(label = sigla), color = "white", size = 1.25)+
  scale_fill_manual(values = c("#7FC3FE", "#2B9CFE", "#086EC8", "#0A447F", "#1A1449")) +
  facet_wrap(~secretaria) +
  theme_minimal()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom") +
  labs(x = NULL, y = NULL)


#tabela
names(liq_org)

geral %>% 
  group_by(secretaria) %>% 
  summarise(liq_2022 = sum(idrgp_2022)) %>% 
  arrange(desc(liq_2022)) %>% 
  select(secretaria, liq_2022)

names(geral)

(geral %>%
  select(secretaria, tipo, liquidacao_2022) %>%
  group_by(secretaria, tipo) %>%
  summarise(liquidacao_2022 = sum(liquidacao_2022), .groups = "keep") %>%
  arrange(desc(liquidacao_2022)) %>% 
  ggplot() +
  geom_col(aes(x = liquidacao_2022,
               y = forcats::fct_reorder(secretaria, liquidacao_2022),
               fill = tipo))+
  theme_minimal()+
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom") +
  labs(x = NULL, y = NULL))

geral %>% 
  group_by(secretaria) %>% 
  summarise(liquidacao_2022 = sum(liquidacao_2022)) %>% 
  arrange(desc(liquidacao_2022)) %>% 
  mutate("%" = (liquidacao_2022/sum(liquidacao_2022))*100)

sum(liq_org$liq_por_orgao)
sum(gp_idrgp$idrgp_2022)

(liq_org_top <- liq_org %>%
  filter(secretaria %in% c("SEHAB", "SIURB", "SEME", "SMS")) %>% 
  mutate(perc = base::prop.table(liq_por_orgao)) %>% 
  mutate(quintis = ntile(perc, 5)) %>% 
  group_by(secretaria) %>% 
  summarise(liq_por_orgao = sum(liq_por_orgao)))

liq_org %>% 
  group_by(secretaria) %>% 
  summarise(sum(idrgp_2022)) %>% 
  arrange(desc(`sum(idrgp_2022)`))

## gráficos de dispersão IDRGP vs DA

names(gp_idrgp)
names(da_mapa)

(disp <- gp_idrgp %>% 
  mutate(da = da_mapa$liquidacao_2022))

(disp %>% 
  ggplot()+
  geom_point(aes(x = idrgp_2022, y = valor_por_ano)))

r2_idrgp <- lm(valor_por_ano ~ idrgp_2022, data = disp)
summary(r2_idrgp)

## removendo outliers de idrgp_2022

# Calcula o primeiro e terceiro quartis
Q1 <- quantile(disp$idrgp_2022, 0.25)
Q3 <- quantile(disp$idrgp_2022, 0.75)

# Calcula o intervalo interquartil (IQR)
(IQR <- Q3 - Q1)

# Define os limites inferior e superior
(limite_inferior <- Q1 - 1.5 * IQR)
(limite_superior <- Q3 + 1.5 * IQR)


(disp$idrgp_2022[disp$idrgp_2022 >= limite_inferior & disp$idrgp_2022 <= limite_superior])

disp$idrgp_2022

# Remove os outliers
(disp_b <- disp[-c(5,13), ])

(disp_b %>% 
    ggplot()+
    geom_point(aes(x = idrgp_2022, y = valor_por_ano)))

r2_idrgp_a <- lm(valor_por_ano ~ idrgp_2022, data = disp_b)
summary(r2_idrgp_a)


(disp_a %>% 
    ggplot()+
    geom_point(aes(x = da, y = valor_por_ano)))

r2_da_a <- lm(valor_por_ano ~ da, data = disp_a)
summary(r2_da_a)




(disp %>% 
  ggplot()+
  geom_point(aes(x = da, y = valor_por_ano)))

r2_da <- lm(valor_por_ano ~ da, data = disp)
summary(r2_da)

## removendo outliers de da
# Calcula o primeiro e terceiro quartis
Q1 <- quantile(disp$da, 0.25)
Q3 <- quantile(disp$da, 0.75)

# Calcula o intervalo interquartil (IQR)
(IQR <- Q3 - Q1)

# Define os limites inferior e superior
(limite_inferior <- Q1 - 1.5 * IQR)
(limite_superior <- Q3 + 1.5 * IQR)

# Remove os outliers
(disp_a <- disp[-c(1,3,23), ])

(disp_a %>% 
    ggplot()+
    geom_point(aes(x = da, y = valor_por_ano)))

r2_da_a <- lm(valor_por_ano ~ da, data = disp_a)
summary(r2_da_a)


disp <- disp %>% 
  select(-da)

da_disp <- da %>% 
  select(sp_nome, liquidacao_2022)

(disp <- disp %>% 
  left_join(da_disp, by = "sp_nome") %>% 
  rename(da = liquidacao_2022))

### tabela

(a <- sum(gp_idrgp$liq_obras))
(b <- sum(gp_idrgp$liq_pdm))
(c <- sum(gp_idrgp$liq_oc))
(d <- a+b+c)
(e <- (a/d)*100)
(f <- (b/d)*100)
(g <- (c/d)*100)
(h <- e+f+g)

(tab <- tibble(
  "Componente" = c("Obras monitoradas", "Programa de Metas 2021-2024 Versão Final Participativa", "Orçamento Cidadão 2022", "Total"),
  "Valor Liquidado" = c(a, b, c, d),
  "%" = c(e, f, g, h)))

#mapa SIURB
(siurb <- readxl::read_xlsx("Planilha IDRGP.xlsx",
                            sheet = "obras_prioritarias") %>% 
    janitor::clean_names() %>% 
    dplyr::filter(idrgp == "IDRGP" & secretaria == "SIURB" & !is.na(valor_empenho_liquidado_proxy)) %>% 
    rename(liquidacao_2022 = valor_empenho_liquidado_proxy))

sum(siurb$liquidacao_2022)

obras %>% 
  filter(secretaria == "SIURB") %>% 
  summarise(sum(liquidacao_2022)) ## para comparar com sum(siurb$liquidacao_2022)


(siurb$subprefeitura <- padronizar_nomes(siurb$subprefeitura))


(siurb_mapa <- siurb %>% 
    select(subprefeitura, liquidacao_2022) %>%
    filter(!is.na(subprefeitura)) %>% 
    group_by(subprefeitura) %>% 
    summarise(liquidacao_2022 = sum(liquidacao_2022)) %>% 
    rename(sp_nome = subprefeitura))

siurb_mapa$sp_nome
subprefeitura$sp_nome ## comparando, necessário ajustar ARICANDUVA, CIDADE ADEMAR, CIDADE TIRADENTES,MBOI, SAO MIGUEL

(siurb_mapa$sp_nome[1] <- subprefeitura$sp_nome[1])
(siurb_mapa$sp_nome[6] <- subprefeitura$sp_nome[6])
(siurb_mapa$sp_nome[7] <- subprefeitura$sp_nome[7])
(siurb_mapa$sp_nome[17] <- subprefeitura$sp_nome[17])
(siurb_mapa$sp_nome[27] <- subprefeitura$sp_nome[27])

(siurb_mapa <- dplyr::full_join(subprefeitura, siurb_mapa, by = NULL))

sum(siurb_mapa$liquidacao_2022)

(siurb_mapa$liquidacao_2022 <- siurb_mapa$liquidacao_2022 %>% 
    tidyr::replace_na(0))


(siurb_mapa <- siurb_mapa %>% 
    mutate(perc = base::prop.table(liquidacao_2022)) %>% 
    mutate(quintis = ntile(perc, 5)) %>% 
    select(sp_nome, sigla, liquidacao_2022, perc, quintis, geometry) %>% 
    st_as_sf() %>% 
    st_transform(crs = "EPSG:4326"))

(intervalos_siurb <- siurb_mapa %>%
    summarise(
      Q1 = quantile(liquidacao_2022, probs = 0.20),
      Q2 = quantile(liquidacao_2022, probs = 0.40),
      Q3 = quantile(liquidacao_2022, probs = 0.60),
      Q4 = quantile(liquidacao_2022, probs = 0.80),
      Q5 = quantile(liquidacao_2022, probs = 1)
    ))

(siurb_mapa <- siurb_mapa %>% 
    mutate("Liquidação 2022" = case_when(
      quintis <= 1 ~ "0% a 20% (R$ 0 a R$ 4,5 mi)",
      quintis > 1 & quintis <= 2 ~ "20% a 40% (R$ 4,5 mi a R$ 11,7 mi)",
      quintis > 2 & quintis <= 3 ~ "40% a 60% (R$ 11,7 a R$ 21,2 mi)",
      quintis > 3 & quintis <= 4 ~ "60% a 80% (R$ 21,2 a R$ 28,7 mi)",
      quintis > 4  ~ "80% a 100% (R$ 28,7 mil a R$ 189,7 mi)")))

#inclusão das projeções
(siurb_mapa <- siurb_mapa %>% 
    mutate("projecao" = case_when(
      sp_nome == "ARICANDUVA-FORMOSA-CARRAO" ~ 294734000,
      sp_nome == "BUTANTA" ~ 2068860.75,
      sp_nome == "CAMPO LIMPO" ~ 397286282.32,
      sp_nome == "CAPELA DO SOCORRO" ~ 14478313.65,
      sp_nome == "CASA VERDE-CACHOEIRINHA" ~ 7578759,
      sp_nome == "FREGUESIA-BRASILANDIA" ~ 38000000,
      sp_nome == "GUAIANASES" ~ 144000000,
      sp_nome == "IPIRANGA" ~ 11000000,
      sp_nome == "M'BOI MIRIM" ~ 248383970.65,
      sp_nome == "MOOCA" ~ 192797000,
      sp_nome == "PERUS" ~ 140066013.335,
      sp_nome == "SANTO AMARO" ~ 15000000,
      sp_nome == "SAO MATEUS" ~ 2332284.72,
      sp_nome == "SAO MIGUEL" ~ 66613780,
      sp_nome == "SAPOPEMBA" ~ 1835000,
      sp_nome == "VILA MARIA-VILA GUILHERME" ~ 7028624.44,
      sp_nome == "VILA MARIANA" ~ 172000000)))

(siurb_mapa$projecao <- siurb_mapa$projecao %>% 
    tidyr::replace_na(0))

sum(siurb_mapa$projecao)

(siurb_mapa <- siurb_mapa %>% 
    mutate(perc = base::prop.table(projecao)) %>% 
    mutate(quintis_p = ntile(perc, 5)) %>% 
    select(sp_nome, sigla, liquidacao_2022, perc, quintis, projecao, quintis_p, geometry) %>% 
    st_as_sf() %>% 
    st_transform(crs = "EPSG:4326"))

(intervalos_siurb_p <- siurb_mapa %>%
    summarise(
      Q1 = quantile(projecao, probs = 0.20),
      Q2 = quantile(projecao, probs = 0.40),
      Q3 = quantile(projecao, probs = 0.60),
      Q4 = quantile(projecao, probs = 0.80),
      Q5 = quantile(projecao, probs = 1)
    ))

(siurb_mapa <- siurb_mapa %>% 
    mutate("Projeções 2023-2026" = case_when(
      quintis_p <= 1 ~ "0% a 20% (R$ 0)",
      quintis_p > 1 & quintis_p <= 2 ~ "20% a 40% (R$ 0)",
      quintis_p > 2 & quintis_p <= 3 ~ "40% a 60% (R$ 0 a R$ 7,4 mi)",
      quintis_p > 3 & quintis_p <= 4 ~ "60% a 80% (R$ 7,4 a R$ 125,4 mi)",
      quintis_p > 4  ~ "80% a 100% (R$ 125,4 mil a R$ 397,3 mi)")))


# mapa siurb realizado
siurb_mapa %>%
  ggplot()+
  geom_sf(aes(fill = `Liquidação 2022`, geometry = geometry)) +
  scale_fill_manual(values = c("#7FC3FE", "#2B9CFE", "#086EC8", "#0A447F", "#1A1449")) +
  geom_sf_text(aes(label = sigla), color = "white", size = 2, fontface = "bold") + 
  theme_minimal() +
  labs(title = "Distribuição regional dos gastos de SIURB",
       subtitle = "* Valores liquidados em 2022, em projetos selecionados: R$ 985 milhões",
       fill = "Quintis",
       caption = "Fonte: PMSP/SOF; SGM/SEPEP/Unidade de Entregas. Elaboração: SGM/SEPEP/CP.",
       x = NULL, y = NULL)

# mapa siurb projeção
siurb_mapa %>%
  ggplot()+
  geom_sf(aes(fill = `Projeções 2023-2026`, geometry = geometry)) +
  scale_fill_manual(values = c("#7FC3FE", "#2B9CFE", "#086EC8", "#0A447F", "#1A1449")) +
  geom_sf_text(aes(label = sigla), color = "white", size = 2, fontface = "bold") + 
  theme_minimal() +
  labs(title = "Distribuição regional dos gastos projetados de SIURB",
       subtitle = "* Valores contratados para obras de drenagem, canalização e reservatórios (2023 a 2026): R$ 1,755 bilhão",
       fill = "Quintis",
       caption = "Fonte: SIURB. Elaboração: SGM/SEPEP/CP.",
       x = NULL, y = NULL)

## arquivo com dados de SIURB
(plan_siurb <- gp_idrgp %>%
    as_tibble() %>% 
    mutate("siurb_proj" = siurb_mapa$projecao,
           "siurb_exec" = siurb_mapa$liquidacao_2022) %>% 
    select(-geometry))

sum(plan_siurb$siurb_exec)
sum(plan_siurb$siurb_proj)

plan_siurb %>% 
  write.csv2("planilha_idrgp_com_siurb.csv")


### Mapa SVMA
install.packages("readODS")
library(readODS)

(svma <- readODS::read_ods("svma.ods") %>% 
    janitor::clean_names())

(svma_mapa <- svma %>% 
    mutate(perc = base::prop.table(liquidacao_2022)) %>% 
    mutate(quintis = ntile(perc, 5)) %>%
    mutate(geometry = subprefeitura$geometry) %>% 
    mutate(sigla = subprefeitura$sigla) %>% 
    select(sp_nome, sigla, liquidacao_2022, perc, quintis, geometry) %>% 
    st_as_sf() %>% 
    st_transform(crs = "EPSG:4326"))

(intervalos_svma <- svma_mapa %>%
    summarise(
      Q1 = quantile(liquidacao_2022, probs = 0.20),
      Q2 = quantile(liquidacao_2022, probs = 0.40),
      Q3 = quantile(liquidacao_2022, probs = 0.60),
      Q4 = quantile(liquidacao_2022, probs = 0.80),
      Q5 = quantile(liquidacao_2022, probs = 1)
    ))

(svma_mapa <- svma_mapa %>% 
    mutate("Liquidação 2022" = case_when(
      quintis <= 1 ~ "0% a 20% (R$ 0 a R$ 1,9 mi)",
      quintis > 1 & quintis <= 2 ~ "20% a 40% (R$ 1,9 a R$ 4,8 mi)",
      quintis > 2 & quintis <= 3 ~ "40% a 60% (R$ 4,8 a R$ 7,8 mi)",
      quintis > 3 & quintis <= 4 ~ "60% a 80% (R$ 7,8 a R$ 10,5 mi)",
      quintis > 4  ~ "80% a 100% (R$ 10,5 mil a R$ 28,0 mi)")))

sum(svma_mapa$liquidacao_2022)

svma_mapa %>%
  ggplot()+
  geom_sf(aes(fill = `Liquidação 2022`, geometry = geometry)) +
  scale_fill_manual(values = c("#7FC3FE", "#2B9CFE", "#086EC8", "#0A447F", "#1A1449")) +
  geom_sf_text(aes(label = sigla), color = "white", size = 2, fontface = "bold") + 
  theme_minimal() +
  labs(title = "Distribuição regional dos gastos de SVMA (Detalhamento da Ação)",
       subtitle = "* Valores liquidados em 2022, classificados pelo DA: R$ 243,9 milhões",
       fill = "Quintis",
       caption = "Fonte: PMSP/SOF. Elaboração: SGM/SEPEP/CP.",
       x = NULL, y = NULL)
