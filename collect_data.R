# === Script para recolher ocorrências da Proteção Civil ===
# Gera: Ocorrencias_mau_tempo.csv, Ocorrencias_incendio.csv, dados_prociv_expanded.csv

# === Carregar pacotes ===
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
codigos_mau_tempo <- c("1101", "1103", "1107", "1111", "1115", "1125", "3301", "3303", "3305", "3309", "3311", "3313", "3315", "3317", "3319", "3321", "3329", "3333")
codigos_incendio <- c("3101", "3103", "3105", "3111")

# === Função para formatar nomes ===
formato_nome <- function(x) {
  if (is.na(x) || x == "") return(NA)
  palavras <- strsplit(tolower(x), " ")[[1]]
  palavras <- sapply(palavras, function(p) {
    if (nchar(p) > 0) paste0(toupper(substr(p, 1, 1)), substr(p, 2, nchar(p))) else p
  })
  paste(palavras, collapse = " ")
}

# === Função para obter dados ===
get_ocorrencias <- function() {
  base_url <- "https://services-eu1.arcgis.com/VlrHb7fn5ewYhX6y/arcgis/rest/services/OcorrenciasSite/FeatureServer/0/query"
  
  all_data <- list()
  offset <- 0
  batch_size <- 1000
  max_attempts <- 10
  
  repeat {
    cat("A obter registos a partir de", offset, "...\n")
    
    # Construir URL manualmente para evitar problemas de encoding
    query_url <- paste0(
      base_url,
      "?f=json",
      "&where=1%3D1",
      "&outFields=*",
      "&returnGeometry=true",
      "&resultOffset=", offset,
      "&resultRecordCount=", batch_size
    )
    
    response <- NULL
    for (attempt in 1:max_attempts) {
      cat("  Tentativa", attempt, "...\n")
      
      response <- tryCatch({
        GET(
          query_url,
          add_headers(
            `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36",
            `Accept` = "application/json",
            `Accept-Encoding` = "gzip, deflate",
            `Connection` = "keep-alive"
          ),
          timeout(300)
        )
      }, error = function(e) {
        cat("  Erro:", conditionMessage(e), "\n")
        NULL
      })
      
      if (!is.null(response) && status_code(response) == 200) {
        break
      }
      
      if (attempt < max_attempts) {
        wait_time <- 5 * attempt
        cat("  A aguardar", wait_time, "segundos...\n")
        Sys.sleep(wait_time)
      }
    }
    
    if (is.null(response) || status_code(response) != 200) {
      cat("ERRO: Não foi possível obter dados após", max_attempts, "tentativas\n")
      if (!is.null(response)) cat("Status:", status_code(response), "\n")
      break
    }
    
    json_text <- content(response, "text", encoding = "UTF-8")
    
    data <- tryCatch({
      fromJSON(json_text, flatten = TRUE)
    }, error = function(e) {
      cat("Erro ao processar JSON:", conditionMessage(e), "\n")
      NULL
    })
    
    if (is.null(data)) break
    
    if (!is.null(data$error)) {
      cat("Erro da API:", data$error$message, "\n")
      break
    }
    
    if (is.null(data$features) || length(data$features) == 0 || nrow(data$features) == 0) {
      if (offset == 0) {
        cat("AVISO: Nenhum registo retornado\n")
      } else {
        cat("Fim dos registos.\n")
      }
      break
    }
    
    all_data <- c(all_data, list(data$features))
    n_records <- nrow(data$features)
    cat("  Obtidos", n_records, "registos\n")
    
    if (n_records < batch_size) break
    
    offset <- offset + batch_size
    Sys.sleep(2)
  }
  
  if (length(all_data) == 0) {
    return(data.frame())
  }
  
  bind_rows(all_data)
}

# === Obter dados ===
cat("=== A obter ocorrências da Proteção Civil ===\n")
cat("Hora:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

all_occurrences <- get_ocorrencias()

if (nrow(all_occurrences) == 0) {
  cat("\nERRO: Não foram obtidas ocorrências. A criar ficheiros vazios...\n")
  write_csv(data.frame(), "dados_prociv_expanded.csv")
  write_csv(data.frame(), "Ocorrencias_mau_tempo.csv")
  write_csv(data.frame(), "Ocorrencias_incendio.csv")
  quit(status = 1)
}

cat("\nTotal de ocorrências obtidas:", nrow(all_occurrences), "\n")

# === Processar dados ===

# Extrair coordenadas
if ("geometry.x" %in% names(all_occurrences)) {
  all_occurrences$longitude <- all_occurrences$geometry.x
  all_occurrences$latitude <- all_occurrences$geometry.y
}

# Renomear colunas
rename_map <- c(
  "attributes.Numero" = "id",
  "attributes.Localidade" = "address",
  "attributes.Regiao" = "subregion",
  "attributes.DICO" = "neighborhood",
  "attributes.DataOcorrencia" = "date",
  "attributes.Data" = "date.1",
  "attributes.EstadoOcorrencia" = "state",
  "attributes.Natureza" = "nature",
  "attributes.MeiosAereos" = "aerial_means",
  "attributes.MeiosTerrestres" = "terrestrial_means",
  "attributes.Operacionais" = "operational",
  "attributes.Distrito" = "Distrito",
  "attributes.Concelho" = "Concelho"
)

for (old_name in names(rename_map)) {
  new_name <- rename_map[old_name]
  if (old_name %in% names(all_occurrences)) {
    all_occurrences[[new_name]] <- all_occurrences[[old_name]]
  }
}

# Usar Concelho da API se existir
if ("attributes.Concelho" %in% names(all_occurrences)) {
  all_occurrences$Concelho <- all_occurrences$`attributes.Concelho`
}

# Converter timestamps
if ("date" %in% names(all_occurrences) && is.numeric(all_occurrences$date)) {
  all_occurrences$date <- as.POSIXct(all_occurrences$date / 1000, origin = "1970-01-01", tz = "UTC")
  all_occurrences$date <- format(all_occurrences$date, "%Y-%m-%dT%H:%M:%SZ")
}

# Padronizar estado
if ("state" %in% names(all_occurrences)) {
  all_occurrences$state <- tolower(trimws(all_occurrences$state))
  all_occurrences$state <- tools::toTitleCase(all_occurrences$state)
  all_occurrences$state <- ifelse(
    all_occurrences$state %in% c("Em Resolução", "Em Conclusão"),
    "Em resolução/conclusão",
    all_occurrences$state
  )
  all_occurrences$state_2 <- tools::toTitleCase(gsub("^Em ", "", all_occurrences$state))
}

# === Spatial join (opcional) ===
if (file.exists("ContinenteConcelhos.geojson") && 
    "longitude" %in% names(all_occurrences) && 
    "latitude" %in% names(all_occurrences)) {
  
  cat("\nA fazer join espacial com concelhos...\n")
  
  tryCatch({
    concelhos <- st_read("ContinenteConcelhos.geojson", quiet = TRUE)
    
    valid_coords <- !is.na(all_occurrences$longitude) & !is.na(all_occurrences$latitude)
    
    if (sum(valid_coords) > 0) {
      final_sf <- st_as_sf(all_occurrences[valid_coords, ], 
                           coords = c("longitude", "latitude"), 
                           crs = 4326)
      
      if (!st_is_longlat(concelhos)) {
        concelhos <- st_transform(concelhos, 4326)
      }
      
      # Remover colunas duplicadas antes do join
      cols_to_remove <- intersect(c("Concelho", "Distrito", "NUTII_DSG"), names(final_sf))
      for (col in cols_to_remove) {
        final_sf[[col]] <- NULL
      }
      
      cols_available <- intersect(c("Concelho", "Distrito", "NUTII_DSG"), names(concelhos))
      if (length(cols_available) > 0) {
        final_sf <- st_join(final_sf, concelhos[cols_available], join = st_within, left = TRUE)
        
        # Formatar nomes
        for (col in cols_available) {
          if (col %in% names(final_sf)) {
            final_sf[[col]] <- sapply(final_sf[[col]], formato_nome)
          }
        }
      }
      
      coords <- st_coordinates(final_sf)
      final_sf$longitude <- coords[, 1]
      final_sf$latitude <- coords[, 2]
      
      final_sf <- st_set_geometry(final_sf, NULL)
      all_occurrences <- as.data.frame(final_sf)
      
      cat("Join espacial concluído\n")
    }
  }, error = function(e) {
    cat("Erro no join espacial:", conditionMessage(e), "\n")
    cat("A continuar sem join espacial...\n")
  })
}

# === Separar código e descrição ===
all_occurrences$nature <- trimws(as.character(all_occurrences$nature))
all_occurrences$nature_description <- sub("^\\d+\\s*-\\s*", "", all_occurrences$nature)
all_occurrences$nature <- sub("^(\\d+).*", "\\1", all_occurrences$nature)

# === Guardar dados completos ===
write_csv(all_occurrences, "dados_prociv_expanded.csv")
cat("\ndados_prociv_expanded.csv guardado com", nrow(all_occurrences), "linhas\n")

# === Filtrar mau tempo ===
cat("\n=== Mau Tempo ===\n")
ocorrencias_mau_tempo <- all_occurrences %>% filter(nature %in% codigos_mau_tempo)

if (nrow(ocorrencias_mau_tempo) == 0) {
  cat("Nenhuma ocorrência de mau tempo\n")
  write_csv(data.frame(), "Ocorrencias_mau_tempo.csv")
} else {
  write_csv(ocorrencias_mau_tempo, "Ocorrencias_mau_tempo.csv")
  cat("Ocorrencias_mau_tempo.csv guardado com", nrow(ocorrencias_mau_tempo), "linhas\n")
  print(table(ocorrencias_mau_tempo$nature_description))
}

# === Filtrar incêndios ===
cat("\n=== Incêndios ===\n")
ocorrencias_incendio <- all_occurrences %>% filter(nature %in% codigos_incendio)

if (nrow(ocorrencias_incendio) == 0) {
  cat("Nenhuma ocorrência de incêndio\n")
  write_csv(data.frame(), "Ocorrencias_incendio.csv")
} else {
  write_csv(ocorrencias_incendio, "Ocorrencias_incendio.csv")
  cat("Ocorrencias_incendio.csv guardado com", nrow(ocorrencias_incendio), "linhas\n")
  print(table(ocorrencias_incendio$nature_description))
}

# === Metadata ===
ultima_atualizacao <- format(Sys.time(), tz = "Europe/Lisbon", format = "%Hh%M de %d/%m/%Y")

metadata <- list(
  annotate = list(
    notes = paste0(
      "<details><summary>Nota: </summary><br>",
      "Em Despacho – Meios em trânsito<br>",
      "Em Curso – Ocorrência em evolução<br>",
      "Em Resolução/Conclusão – Ocorrência controlada ou extinta<br></details>",
      "Última atualização às ", ultima_atualizacao
    )
  )
)

write_json(metadata, "metadata_prociv.json", pretty = TRUE, auto_unbox = TRUE)

# === Resumo ===
cat("\n=== RESUMO ===\n")
cat("Total:", nrow(all_occurrences), "\n")
cat("Mau tempo:", nrow(ocorrencias_mau_tempo), "\n")
cat("Incêndios:", nrow(ocorrencias_incendio), "\n")
cat("Concluído às", format(Sys.time(), "%H:%M:%S"), "\n")
