# === Script para recolher ocorrências da Proteção Civil ===
# Gera dois ficheiros: Ocorrencias_mau_tempo.csv e Ocorrencias_incendio.csv
# Usa a API ArcGIS da Proteção Civil

# === Carregar pacotes necessários ===
if (!require(httr)) install.packages("httr", repos = "https://cloud.r-project.org")
if (!require(jsonlite)) install.packages("jsonlite", repos = "https://cloud.r-project.org")
if (!require(dplyr)) install.packages("dplyr", repos = "https://cloud.r-project.org")
if (!require(sf)) install.packages("sf", repos = "https://cloud.r-project.org")
if (!require(tools)) install.packages("tools", repos = "https://cloud.r-project.org")
if (!require(readr)) install.packages("readr", repos = "https://cloud.r-project.org")

library(httr)
library(jsonlite)
library(dplyr)
library(sf)
library(tools)
library(readr)

# === Códigos de ocorrências ===

# Mau tempo
codigos_mau_tempo <- c(
 "1101",
 "1103",
 "1107",
 "1111",
 "1115",
 "1125",
 "3301",
 "3305",
 "3309",
 "3313",
 "3315",
 "3317",
 "3319",
 "3321",
 "3329",
 "3333"
)

# Incêndios
codigos_incendio <- c(
 "3101",
 "3103",
 "3105",
 "3111"
)

# === URL da API ArcGIS ===
base_url <- "https://services-eu1.arcgis.com/VlrHb7fn5ewYhX6y/arcgis/rest/services/OcorrenciasSite/FeatureServer/0/query"

# === Função para formatar nomes (primeira letra maiúscula apenas) ===
formato_nome <- function(x) {
 if (is.na(x)) return(NA)
 # Converter para minúsculas e depois capitalizar primeira letra de cada palavra
 palavras <- strsplit(tolower(x), " ")[[1]]
 palavras <- sapply(palavras, function(p) {
   if (nchar(p) > 0) {
     paste0(toupper(substr(p, 1, 1)), substr(p, 2, nchar(p)))
   } else {
     p
   }
 })
 paste(palavras, collapse = " ")
}

# === Função para obter dados com paginação ===
get_all_data <- function(base_url, where_clause = "1=1", retries = 3, delay = 5) {
 all_data <- list()
 offset <- 0
 batch_size <- 2000
 
 repeat {
   cat("A obter registos a partir de", offset, "...\n")
   
   params <- list(
     f = "json",
     where = where_clause,
     outFields = "*",
     returnGeometry = "true",
     resultOffset = offset,
     resultRecordCount = batch_size,
     outSR = "4326"
   )
   
   for (attempt in 1:retries) {
     response <- tryCatch(
       GET(base_url, 
           query = params,
           add_headers(
             `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
             `Accept` = "application/json"
           ),
           timeout(120)),
       error = function(e) {
         cat("Tentativa", attempt, "falhou:", conditionMessage(e), "\n")
         return(NULL)
       }
     )
     
     if (!is.null(response) && status_code(response) == 200) break
     if (attempt < retries) Sys.sleep(delay * attempt)
   }
   
   if (is.null(response) || status_code(response) != 200) {
     stop("Falha ao obter dados após várias tentativas")
   }
   
   json_text <- content(response, "text", encoding = "UTF-8")
   data <- fromJSON(json_text, flatten = TRUE)
   
   if (is.null(data$features) || length(data$features) == 0) {
     cat("Não há mais registos.\n")
     break
   }
   
   all_data <- c(all_data, list(data$features))
   n_records <- nrow(data$features)
   cat("Obtidos", n_records, "registos\n")
   
   if (n_records < batch_size) break
   
   offset <- offset + batch_size
   Sys.sleep(1)
 }
 
 if (length(all_data) == 0) {
   return(data.frame())
 }
 
 final_df <- bind_rows(all_data)
 return(final_df)
}

# === Obter todas as ocorrências ===
cat("=== A obter ocorrências da Proteção Civil ===\n")
all_occurrences <- get_all_data(base_url)

if (nrow(all_occurrences) == 0) {
 stop("Não foram obtidas ocorrências")
}

cat("Total de ocorrências obtidas:", nrow(all_occurrences), "\n")

# === Processar dados ===

# Extrair coordenadas da geometria
if ("geometry.x" %in% names(all_occurrences)) {
 all_occurrences$longitude <- all_occurrences$geometry.x
 all_occurrences$latitude <- all_occurrences$geometry.y
}

# Mapeamento de colunas
col_mapping <- list(
 "attributes.Numero" = "id",
 "attributes.Localidade" = "address",
 "attributes.Regiao" = "subregion",
 "attributes.CodConcelho" = "neighborhood",
 "attributes.DataOcorrencia" = "date",
 "attributes.Data" = "date.1",
 "attributes.EstadoOcorrencia" = "state",
 "attributes.Natureza" = "nature",
 "attributes.MeiosAereos" = "aerial_means",
 "attributes.MeiosTerrestres" = "terrestrial_means",
 "attributes.MeiosAquaticos" = "aquatic_means",
 "attributes.Operacionais" = "operational",
 "attributes.Distrito" = "Distrito",
 "attributes.Concelho" = "Concelho"
)

for (old_name in names(col_mapping)) {
 new_name <- col_mapping[[old_name]]
 if (old_name %in% names(all_occurrences)) {
   all_occurrences[[new_name]] <- all_occurrences[[old_name]]
 }
}

# Converter timestamp para datetime
if ("date" %in% names(all_occurrences) && is.numeric(all_occurrences$date)) {
 all_occurrences$date <- as.POSIXct(all_occurrences$date / 1000, origin = "1970-01-01", tz = "UTC")
 all_occurrences$date <- format(all_occurrences$date, "%Y-%m-%dT%H:%M:%SZ")
}

if ("date.1" %in% names(all_occurrences) && is.numeric(all_occurrences$`date.1`)) {
 all_occurrences$`date.1` <- as.POSIXct(all_occurrences$`date.1` / 1000, origin = "1970-01-01", tz = "UTC")
 all_occurrences$`date.1` <- format(all_occurrences$`date.1`, "%Y-%m-%dT%H:%M:%SZ")
}

# === Padronizar estado ===
if ("state" %in% names(all_occurrences)) {
 all_occurrences$state <- tolower(trimws(all_occurrences$state))
 all_occurrences$state <- tools::toTitleCase(all_occurrences$state)
 
 # Juntar resolução e conclusão
 all_occurrences$state <- ifelse(
   all_occurrences$state %in% c("Em Resolução", "Em Conclusão"),
   "Em resolução/conclusão",
   all_occurrences$state
 )
 
 # Criar coluna state_2 (sem o "Em")
 all_occurrences$state_2 <- tools::toTitleCase(gsub("^Em ", "", all_occurrences$state))
}

# === Fazer spatial join com concelhos (se o ficheiro existir) ===
if (file.exists("ContinenteConcelhos.geojson") && 
   "longitude" %in% names(all_occurrences) && 
   "latitude" %in% names(all_occurrences)) {
 
 cat("A fazer join espacial com concelhos...\n")
 
 concelhos <- st_read("ContinenteConcelhos.geojson", quiet = TRUE)
 
 valid_coords <- !is.na(all_occurrences$longitude) & !is.na(all_occurrences$latitude)
 
 if (sum(valid_coords) > 0) {
   
   sample_lon <- all_occurrences$longitude[valid_coords][1]
   sample_lat <- all_occurrences$latitude[valid_coords][1]
   
   if (abs(sample_lon) > 180 || abs(sample_lat) > 90) {
     cat("Coordenadas em Web Mercator detectadas, a converter para WGS84...\n")
     final_sf <- st_as_sf(all_occurrences[valid_coords, ], 
                          coords = c("longitude", "latitude"), 
                          crs = 3857)
     final_sf <- st_transform(final_sf, 4326)
   } else {
     final_sf <- st_as_sf(all_occurrences[valid_coords, ], 
                          coords = c("longitude", "latitude"), 
                          crs = 4326)
   }
   
   if (!st_is_longlat(concelhos)) {
     concelhos <- st_transform(concelhos, 4326)
   }
   
   concelhos_cols <- names(concelhos)
   
   concelho_col <- intersect(c("Concelho", "CONCELHO", "concelho", "NAME_2", "name"), concelhos_cols)[1]
   distrito_col <- intersect(c("Distrito", "DISTRITO", "distrito", "NAME_1", "district"), concelhos_cols)[1]
   nut_col <- intersect(c("NUTII_DSG", "NUT2", "NUTS2", "nut2"), concelhos_cols)[1]
   
   cols_to_join <- c(concelho_col, distrito_col, nut_col)
   cols_to_join <- cols_to_join[!is.na(cols_to_join)]
   
   if (length(cols_to_join) > 0) {
     # Remover colunas que vão ser adicionadas pelo join (evitar duplicados)
     for (col in cols_to_join) {
       if (col %in% names(final_sf)) {
         final_sf[[col]] <- NULL
       }
     }
     
     final_sf <- st_join(final_sf, concelhos[cols_to_join], join = st_within, left = TRUE)
     
     # Renomear se necessário
     if (!is.null(concelho_col) && concelho_col != "Concelho" && concelho_col %in% names(final_sf)) {
       final_sf$Concelho <- final_sf[[concelho_col]]
     }
     if (!is.null(distrito_col) && distrito_col != "Distrito" && distrito_col %in% names(final_sf)) {
       final_sf$Distrito <- final_sf[[distrito_col]]
     }
     if (!is.null(nut_col) && nut_col != "NUTII_DSG" && nut_col %in% names(final_sf)) {
       final_sf$NUTII_DSG <- final_sf[[nut_col]]
     }
     
     # Formatar nomes (primeira letra maiúscula apenas)
     if ("Concelho" %in% names(final_sf)) {
       final_sf$Concelho <- sapply(final_sf$Concelho, formato_nome)
     }
     if ("Distrito" %in% names(final_sf)) {
       final_sf$Distrito <- sapply(final_sf$Distrito, formato_nome)
     }
     if ("NUTII_DSG" %in% names(final_sf)) {
       final_sf$NUTII_DSG <- sapply(final_sf$NUTII_DSG, formato_nome)
     }
   }
   
   coords <- st_coordinates(final_sf)
   final_sf$longitude <- coords[, 1]
   final_sf$latitude <- coords[, 2]
   
   final_sf <- st_set_geometry(final_sf, NULL)
   all_occurrences <- as.data.frame(final_sf)
   
   cat("Join espacial concluído\n")
 }
} else {
 cat("Ficheiro ContinenteConcelhos.geojson não encontrado, a continuar sem join espacial...\n")
}

# === Separar código e descrição da natureza ===
all_occurrences$nature <- trimws(as.character(all_occurrences$nature))
all_occurrences$nature_description <- sub("^\\d+\\s*-\\s*", "", all_occurrences$nature)
all_occurrences$nature <- sub("^(\\d+).*", "\\1", all_occurrences$nature)

# === FILTRAR E GUARDAR OCORRÊNCIAS DE MAU TEMPO ===
cat("\n=== Mau Tempo ===\n")

ocorrencias_mau_tempo <- all_occurrences %>%
 filter(nature %in% codigos_mau_tempo)

if (nrow(ocorrencias_mau_tempo) == 0) {
 cat("Nenhuma ocorrência de mau tempo encontrada.\n")
} else {
 write_csv(ocorrencias_mau_tempo, "Ocorrencias_mau_tempo.csv")
 cat("Ocorrencias_mau_tempo.csv guardado com", nrow(ocorrencias_mau_tempo), "linhas\n")
 
 cat("\nDistribuição por tipo:\n")
 print(table(ocorrencias_mau_tempo$nature_description))
}

# === FILTRAR E GUARDAR OCORRÊNCIAS DE INCÊNDIO ===
cat("\n=== Incêndios ===\n")

ocorrencias_incendio <- all_occurrences %>%
 filter(nature %in% codigos_incendio)

if (nrow(ocorrencias_incendio) == 0) {
 cat("Nenhuma ocorrência de incêndio encontrada.\n")
} else {
 write_csv(ocorrencias_incendio, "Ocorrencias_incendio.csv")
 cat("Ocorrencias_incendio.csv guardado com", nrow(ocorrencias_incendio), "linhas\n")
 
 cat("\nDistribuição por tipo:\n")
 print(table(ocorrencias_incendio$nature_description))
}

# === Criar ficheiro JSON com metadados ===
ultima_atualizacao <- format(
 as.POSIXct(Sys.time(), tz = "UTC"),
 tz = "Europe/Lisbon",
 usetz = FALSE,
 format = "%Hh%M de %d/%m/%Y"
)

nota_html <- paste0(
 "<details>",
 "<summary>Nota: </summary><br>",
 "Em Despacho – Meios em trânsito para o teatro de operações<br>",
 "Em Curso – Ocorrência em evolução sem limitação de área<br>",
 "Em Resolução/Conclusão – Ocorrência sem perigo de propagação ou extinta, em observação<br><br>",
 "</details>",
 "Última atualização às ", ultima_atualizacao
)

metadata <- list(
 annotate = list(
   notes = nota_html
 )
)

write_json(metadata, "metadata_prociv.json", pretty = TRUE, auto_unbox = TRUE)
cat("\nmetadata_prociv.json criado\n")

# === Resumo final ===
cat("\n=== RESUMO FINAL ===\n")
cat("Total de ocorrências:", nrow(all_occurrences), "\n")
cat("Ocorrências de mau tempo:", nrow(ocorrencias_mau_tempo), "\n")
cat("Ocorrências de incêndio:", nrow(ocorrencias_incendio), "\n")
