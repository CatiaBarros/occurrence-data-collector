# Carregar os pacotes necessários
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

# URL da API
url <- "https://prociv.gov.pt/occurrences/"

# Função para fazer a requisição com tentativas de retry
get_data_with_retry <- function(url, retries = 3, delay = 5) {
  for (i in 1:retries) {
    response <- GET(url, add_headers(`User-Agent` = "Mozilla/5.0"))
    if (status_code(response) == 200) {
      return(response)
    } else {
      cat("Tentativa", i, "falhou com o código de status:", status_code(response), "\n")
      Sys.sleep(delay)
    }
  }
  # Criar um arquivo vazio ou parar o script em caso de falha final
  write.csv(data.frame(), "dados_prociv_expanded.csv", row.names = FALSE)
  stop("Falha ao obter os dados da API após várias tentativas.")
}

# Usar a função de retry para fazer a requisição
response <- get_data_with_retry(url)

# Verificar se a requisição foi bem-sucedida
if (status_code(response) == 200) {
  content_type <- headers(response)$`content-type`
  
  if (grepl("application/json", content_type)) {
    # Processar JSON
    json_data <- content(response, "text", encoding = "UTF-8")
    data <- fromJSON(json_data, flatten = TRUE)
    
    # Converter em data frame principal e expandir a coluna 'types' em um data frame separado
    main_df <- as.data.frame(data)
    
    # Expandir a coluna 'types' em uma tabela separada
    types_df <- do.call(rbind, lapply(main_df$types, function(x) as.data.frame(x, stringsAsFactors = FALSE)))
    
    # Combinar dados principais com a nova tabela 'types_df'
    final_df <- main_df %>% select(-types) %>% cbind(types_df)
    
    # Carregar o arquivo GeoJSON com as informações dos concelhos
    concelhos <- st_read("ContinenteConcelhos.geojson")
    
    # Verificar e ajustar o sistema de coordenadas
    if (!st_is_longlat(concelhos)) {
      concelhos <- st_transform(concelhos, 4326)  # Converter para WGS84
    }
    
    # Converter final_df para um objeto sf usando coordenadas
    final_sf <- st_as_sf(final_df, coords = c("coordenates.longitude", "coordenates.latitude"), crs = 4326)
    
    # Realizar a junção espacial para obter Concelho, Distrito e NUTII_DSG
    final_sf <- st_join(final_sf, concelhos[c("Concelho", "Distrito", "NUTII_DSG")], join = st_within, left = TRUE)
    
    # Padronizar os nomes
    final_sf$Concelho <- ifelse(is.na(final_sf$Concelho), NA, toTitleCase(tolower(final_sf$Concelho)))
    final_sf$Distrito <- ifelse(is.na(final_sf$Distrito), NA, toTitleCase(tolower(final_sf$Distrito)))
    final_sf$NUTII_DSG <- ifelse(is.na(final_sf$NUTII_DSG), NA, toTitleCase(tolower(final_sf$NUTII_DSG)))
    
    # Separar coordenadas
    final_sf$longitude <- st_coordinates(final_sf)[, 1]
    final_sf$latitude <- st_coordinates(final_sf)[, 2]
    
    # Remover geometria
    final_sf <- st_set_geometry(final_sf, NULL)
    
    # Converter para data.frame
    final_df <- as.data.frame(final_sf)
    
    # Salvar CSV
    write.csv(final_df, file = "dados_prociv_expanded.csv", row.names = FALSE)
    
    cat("O arquivo 'dados_prociv_expanded.csv' com Concelho, Distrito, NUTII_DSG, latitude e longitude foi salvo com sucesso!\n")
  } else {
    stop("O conteúdo retornado não é JSON.")
  }
} else {
  stop("Falha ao obter os dados da API.")
}

# Verificar se o arquivo final foi criado
if (file.exists("dados_prociv_expanded.csv")) {
  dados_prociv <- read_csv("dados_prociv_expanded.csv")
} else {
  stop("O arquivo 'dados_prociv_expanded.csv' não foi criado. Interrompendo o script.")
}

# Carregar o dicionário
if (file.exists("Dicionario_ocorrencias.csv")) {
  dicionario <- read_csv("Dicionario_ocorrencias.csv")
} else {
  stop("O arquivo 'Dicionario_ocorrencias.csv' não foi encontrado. Interrompendo o script.")
}

# === FILTRAR MAU TEMPO ===
ocorrencias_mau_tempo <- dados_prociv %>%
  filter(nature %in% dicionario$`Código.Operacional`)

write_csv(ocorrencias_mau_tempo, "Ocorrencias_mau_tempo.csv")
cat("O arquivo 'Ocorrencias_mau_tempo.csv' foi salvo com sucesso!\n")

# === FILTRAR INCÊNDIOS ===
ocorrencias_incendio <- dados_prociv %>%
  filter(nature %in% dicionario$`Código.Operacional`[grepl("incêndio", tolower(dicionario$Descrição), ignore.case = TRUE)])

write_csv(ocorrencias_incendio, "Ocorrencias_incendio.csv")
cat("O arquivo 'Ocorrencias_incendio.csv' foi salvo com sucesso!\n")
