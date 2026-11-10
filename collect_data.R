# === Script para recolher ocorrências da Proteção Civil ===
# Gera: Ocorrencias_mau_tempo.csv, Ocorrencias_incendio.csv, dados_prociv_expanded.csv, metadata_prociv.json
#
# Se a API falhar (ou devolver dados incompletos), o script termina com erro SEM escrever
# nenhum ficheiro: assim mantém-se publicada a última versão boa.

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(dplyr)
  library(sf)
  library(readr)
})

# === Códigos de ocorrências ===
codigos_mau_tempo <- c(
  # Fenómenos naturais
  "1101", # Cheia
  "1103", # Ventos fortes
  "1107", # Nevões
  "1111", # Ondas de frio
  "1115", # Inundação por galgamento costeiro
  "1117", # Erosão costeira
  "1125", # Enxurrada / Aluvião
  # Infraestruturas e vias de comunicação
  "3301", # Queda de árvore
  "3303", # Corte de abastecimento de água
  "3305", # Corte de abastecimento elétrico
  "3309", # Desabamento de estruturas edificadas
  "3311", # Queda de elementos de construção em estruturas edificadas
  "3313", # Movimento de massa
  "3315", # Inundação de estruturas ou superfícies por precipitação intensa
  "3319", # Desentupimento / Tamponamento
  "3321", # Dano ou queda de redes de fornecimento elétrico
  "3329", # Queda de estruturas temporárias ou móveis
  "3333", # Rutura de barragens
  # Outras ocorrências de proteção e socorro
  "4305", # Limpeza de via e sinalização de perigo (genérico: inclui também derrames, etc.)
  "4339"  # Corte ou remoção de elementos em perigo de queda
)
codigos_incendio <- c("3101", "3103", "3105", "3111")

# === Configuração da API ===
base_url   <- "https://services-eu1.arcgis.com/VlrHb7fn5ewYhX6y/arcgis/rest/services/OcorrenciasSite/FeatureServer/0/query"
batch_size <- 1000
max_pages  <- 20

# === Função para formatar nomes ===
formato_nome <- function(x) {
  if (is.na(x) || x == "") return(NA)
  palavras <- strsplit(tolower(x), " ")[[1]]
  palavras <- sapply(palavras, function(p) {
    if (nchar(p) > 0) paste0(toupper(substr(p, 1, 1)), substr(p, 2, nchar(p))) else p
  })
  paste(palavras, collapse = " ")
}

# === Obter uma página da API (com novas tentativas) ===
# Devolve a lista com os dados ou lança erro.
get_pagina <- function(offset) {
  resp <- RETRY(
    "GET", base_url,
    query = list(
      f = "json",
      where = "1=1",
      outFields = "*",
      returnGeometry = "true",
      outSR = "4326",
      resultOffset = offset,
      resultRecordCount = batch_size
    ),
    add_headers(
      `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36",
      `Accept` = "application/json"
    ),
    timeout(60),
    times = 5,          # até 5 tentativas
    pause_base = 5,     # espera crescente entre tentativas (5s, 10s, 20s...)
    pause_cap = 60,
    terminate_on = c(400, 401, 403, 404),
    quiet = FALSE
  )

  if (status_code(resp) != 200) {
    stop("A API respondeu com o código HTTP ", status_code(resp))
  }

  data <- fromJSON(content(resp, "text", encoding = "UTF-8"), flatten = TRUE)

  # O ArcGIS devolve erros com código 200 e um campo "error" no JSON
  if (!is.null(data$error)) {
    stop("Erro da API: ", data$error$message)
  }
  if (is.null(data$features)) {
    stop("Resposta da API sem o campo 'features'")
  }

  data
}

# === Obter todas as ocorrências ===
# Se alguma página falhar, falha tudo (nunca devolve dados parciais).
get_ocorrencias <- function() {
  paginas <- list()
  offset <- 0

  for (pagina in seq_len(max_pages)) {
    cat("A obter registos a partir de", offset, "...\n")
    data <- get_pagina(offset)

    n <- if (is.data.frame(data$features)) nrow(data$features) else 0
    cat("  Obtidos", n, "registos\n")
    if (n > 0) paginas[[length(paginas) + 1]] <- data$features

    # O ArcGIS indica com exceededTransferLimit quando há mais registos por obter
    if (!isTRUE(data$exceededTransferLimit) || n == 0) {
      return(bind_rows(paginas))
    }

    offset <- offset + n
    Sys.sleep(1)
  }

  stop("Atingido o limite de ", max_pages, " páginas; os dados podem estar incompletos")
}

# === Estado da ocorrência ===
# Agrupa os estados da ANEPC nos três que a legenda explica.
# Usa o campo EstadoAgrupado da API e, se faltar, o EstadoOcorrencia.
agrupar_estado <- function(x) {
  x0 <- tolower(trimws(x))
  case_when(
    grepl("despacho|chegada", x0)               ~ "Em despacho",
    grepl("curso", x0)                          ~ "Em curso",
    grepl("resolu|conclus|vigil|encerr", x0)    ~ "Em resolução/conclusão",
    TRUE                                        ~ trimws(x)
  )
}

# ============================================================
# 1. Obter dados (sem escrever nada se falhar)
# ============================================================
cat("=== A obter ocorrências da Proteção Civil ===\n")
cat("Hora:", format(Sys.time(), tz = "Europe/Lisbon", "%Y-%m-%d %H:%M:%S"), "\n\n")

all_occurrences <- tryCatch(
  get_ocorrencias(),
  error = function(e) {
    cat("\nERRO ao obter dados:", conditionMessage(e), "\n")
    NULL
  }
)

if (is.null(all_occurrences) || nrow(all_occurrences) == 0) {
  cat("Sem dados válidos. Os ficheiros anteriores ficam como estão.\n")
  quit(status = 1)
}

colunas_necessarias <- c("attributes.Natureza", "attributes.EstadoOcorrencia")
em_falta <- setdiff(colunas_necessarias, names(all_occurrences))
if (length(em_falta) > 0) {
  cat("ERRO: a API deixou de enviar as colunas", paste(em_falta, collapse = ", "),
      "\nOs ficheiros anteriores ficam como estão.\n")
  quit(status = 1)
}

cat("\nTotal de ocorrências obtidas:", nrow(all_occurrences), "\n")

# ============================================================
# 2. Processar dados
# ============================================================

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

# Converter timestamps
if ("date" %in% names(all_occurrences) && is.numeric(all_occurrences$date)) {
  all_occurrences$date <- as.POSIXct(all_occurrences$date / 1000, origin = "1970-01-01", tz = "UTC")
  all_occurrences$date <- format(all_occurrences$date, "%Y-%m-%dT%H:%M:%SZ")
}

# Estado: state = grupo usado na legenda; state_detail = estado original da ANEPC
estado_origem <- if ("attributes.EstadoAgrupado" %in% names(all_occurrences)) {
  all_occurrences$`attributes.EstadoAgrupado`
} else {
  all_occurrences$`attributes.EstadoOcorrencia`
}
all_occurrences$state <- agrupar_estado(estado_origem)
all_occurrences$state_detail <- trimws(all_occurrences$`attributes.EstadoOcorrencia`)
all_occurrences$state_2 <- sub("^Em ", "", all_occurrences$state)
all_occurrences$state_2 <- paste0(toupper(substr(all_occurrences$state_2, 1, 1)),
                                  substr(all_occurrences$state_2, 2, nchar(all_occurrences$state_2)))

estados_conhecidos <- c("Em despacho", "Em curso", "Em resolução/conclusão")
estados_novos <- setdiff(unique(all_occurrences$state), estados_conhecidos)
if (length(estados_novos) > 0) {
  cat("AVISO: estados não previstos na legenda:", paste(estados_novos, collapse = ", "), "\n")
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

# === Filtrar ===
ocorrencias_mau_tempo <- all_occurrences %>% filter(nature %in% codigos_mau_tempo)
ocorrencias_incendio  <- all_occurrences %>% filter(nature %in% codigos_incendio)

# ============================================================
# 3. Escrever ficheiros (só chegamos aqui com dados válidos)
# ============================================================
# Se não houver ocorrências de um tipo, escreve o ficheiro só com os cabeçalhos,
# para que os gráficos continuem a reconhecer as colunas.
write_csv(all_occurrences, "dados_prociv_expanded.csv")
cat("\ndados_prociv_expanded.csv guardado com", nrow(all_occurrences), "linhas\n")

cat("\n=== Mau Tempo ===\n")
write_csv(ocorrencias_mau_tempo, "Ocorrencias_mau_tempo.csv")
cat("Ocorrencias_mau_tempo.csv guardado com", nrow(ocorrencias_mau_tempo), "linhas\n")
if (nrow(ocorrencias_mau_tempo) > 0) print(table(ocorrencias_mau_tempo$nature_description))

cat("\n=== Incêndios ===\n")
write_csv(ocorrencias_incendio, "Ocorrencias_incendio.csv")
cat("Ocorrencias_incendio.csv guardado com", nrow(ocorrencias_incendio), "linhas\n")
if (nrow(ocorrencias_incendio) > 0) print(table(ocorrencias_incendio$nature_description))

# === Metadata ===
ultima_atualizacao <- format(Sys.time(), tz = "Europe/Lisbon", format = "%Hh%M de %d/%m/%Y")

metadata <- list(
  annotate = list(
    notes = paste0(
      "<details><summary>Nota: </summary><br>",
      "Em despacho – Meios acionados ou a chegar ao local<br>",
      "Em curso – Ocorrência em evolução<br>",
      "Em resolução/conclusão – Situação controlada ou em fase final<br></details>",
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
print(table(all_occurrences$state, useNA = "ifany"))
cat("Concluído às", format(Sys.time(), tz = "Europe/Lisbon", "%H:%M:%S"), "\n")
