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

# === Códigos de incêndio definidos manualmente ===
codigos_incendio <- c(
  "2101", "2103", "2105", "2107", "2109", "2111", "2113", 
  "2115", "2117", "2119", "2121", "2123", "2125", "2127", 
  "2129", "2201", "2203", "2301", "2303", "2305", "2307", 
  "3101", "3103", "3105", "3107", "3109", "3111", "3201", "3203"
)


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

