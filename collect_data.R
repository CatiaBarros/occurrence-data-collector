# === Carregar pacotes necessários ===
if (!require(httr)) install.packages("httr")
if (!require(jsonlite)) install.packages("jsonlite")
if (!require(dplyr)) install.packages("dplyr")
if (!require(sf)) install.packages("sf")
if (!require(tools)) install.packages("tools")
if (!require(readr)) install.packages("readr")

library(httr)
library(jsonlite)
library(dplyr)
library(sf)
library(tools)
library(readr)

# === URL da API ===
url <- "https://prociv.gov.pt/occurrences/"

# === Função com tentativas de retry ===
get_data_with_retry <- function(url, retries = 3, delay = 5) {
  for (i in 1:retries) {
    response <- GET(url, add_headers(`User-Agent` = "Mozilla/5.0"))
    if (status_code(response) == 200) return(response)
    cat("Tentativa", i, "falhou com status:", status_code(response), "\n")
    Sys.sleep(delay)
  }
  write.csv(data.frame(), "dados_prociv_expanded.csv", row.names = FALSE)
  stop("❌ Falha ao obter dados da API após várias tentativas.")
}

# === Obter resposta da API ===
response <- get_data_with_retry(url)

# === Processar resposta JSON ===
if (status_code(response) == 200) {
  if (grepl("application/json", headers(response)$`content-type`)) {
    
    json_data <- content(response, "text", encoding = "UTF-8")
    data <- fromJSON(json_data, flatten = TRUE)
    main_df <- as.data.frame(data)
    types_df <- do.call(rbind, lapply(main_df$types, function(x) as.data.frame(x, stringsAsFactors = FALSE)))
    final_df <- main_df %>% select(-types) %>% cbind(types_df)
    
    # === Ler GeoJSON com concelhos ===
    concelhos <- st_read("ContinenteConcelhos.geojson")
    if (!st_is_longlat(concelhos)) {
      concelhos <- st_transform(concelhos, 4326)
    }
    
    # === Transformar em objeto sf ===
    final_sf <- st_as_sf(final_df, coords = c("coordenates.longitude", "coordenates.latitude"), crs = 4326)
    final_sf <- st_join(final_sf, concelhos[c("Concelho", "Distrito", "NUTII_DSG")], join = st_within, left = TRUE)
    
    # === Padronizar nomes ===
    final_sf$Concelho <- ifelse(is.na(final_sf$Concelho), NA, toTitleCase(tolower(final_sf$Concelho)))
    final_sf$Distrito <- ifelse(is.na(final_sf$Distrito), NA, toTitleCase(tolower(final_sf$Distrito)))
    final_sf$NUTII_DSG <- ifelse(is.na(final_sf$NUTII_DSG), NA, toTitleCase(tolower(final_sf$NUTII_DSG)))
    
    # === Separar coordenadas ===
    final_sf$longitude <- st_coordinates(final_sf)[, 1]
    final_sf$latitude <- st_coordinates(final_sf)[, 2]
    
    final_sf <- st_set_geometry(final_sf, NULL)
    final_df <- as.data.frame(final_sf)

    # === Criar coluna state_2 antes de salvar ===
    final_df$state_2 <- ifelse(
      final_df$state %in% c("Em resolução", "Em conclusão"),
      "Em resolução/Em conclusão",
      final_df$state
    )
    
    write.csv(final_df, "dados_prociv_expanded.csv", row.names = FALSE)
    cat("✅ Arquivo 'dados_prociv_expanded.csv' salvo com sucesso!\n")
  } else {
    stop("❌ Conteúdo retornado não é JSON.")
  }
} else {
  stop("❌ Falha na requisição da API.")
}

# === Ler dados salvos ===
if (file.exists("dados_prociv_expanded.csv")) {
  dados_prociv <- read_csv("dados_prociv_expanded.csv")
} else {
  stop("❌ Arquivo 'dados_prociv_expanded.csv' não encontrado.")
}

# === Criar coluna state_2 após leitura ===
dados_prociv$state_2 <- ifelse(
  dados_prociv$state %in% c("Em Resolução", "Em Conclusão"),
  "Em Resolução/Em Conclusão",
  dados_prociv$state
)

# === Códigos de incêndio definidos manualmente ===
codigos_incendio <- c("3105", "3107", "3109", "3103", "3101", "3111")

# === Garantir tipo e formato certo ===
dados_prociv$nature <- trimws(as.character(dados_prociv$nature))

# === FILTRAR OCORRÊNCIAS DE INCÊNDIO ===
ocorrencias_incendio <- dados_prociv %>%
  filter(nature %in% codigos_incendio)

# === Salvar arquivo se houver ocorrências ===
if (nrow(ocorrencias_incendio) == 0) {
  cat("⚠️ Nenhuma ocorrência de incêndio foi encontrada com os códigos fornecidos.\n")
} else {
  write_csv(ocorrencias_incendio, "Ocorrencias_incendio.csv")
  cat("✅ Ocorrencias_incendio.csv salvo com", nrow(ocorrencias_incendio), "linhas!\n")
}

# === Criar ficheiro JSON com a data/hora da última atualização ===
ultima_atualizacao <- format(Sys.time(), "%Hh%M de %d/%m/%Y")

metadata <- list(
  annotate = list(
    notes = paste0("Última atualização às ", ultima_atualizacao)
  )
)

write_json(metadata, "metadata_prociv.json", pretty = TRUE, auto_unbox = TRUE)
cat("✅ metadata_prociv.json criado com sucesso!\n")

