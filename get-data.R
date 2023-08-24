library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)



quitar_acentos <- function(s) {
  s <- chartr("áéíóúÁÉÍÓÚ", "aeiouAEIOU", s)
  s <- chartr("àèìòùÀÈÌÒÙ", "aeiouAEIOU", s)
  s <- chartr("âêîôûÂÊÎÔÛ", "aeiouAEIOU", s)
  s <- chartr("äëïöüÄËÏÖÜ", "aeiouAEIOU", s)
  s <- chartr("ãõñÃÕÑ", "aonAON", s)
  s <- gsub("ç", "c", s)
  s <- gsub("Ç", "C", s)
  return(s)
}
# url <- "https://cms.flip.datasketch.co/api/agresiones"
get_data <- function(url) {
respose <- GET( url )
result <- fromJSON(content(respose, as = 'text'))
result$fecha_agresion <- lubridate::ymd(result$fecha_agresion)
result$anio_agresion <- lubridate::year(result$fecha_agresion)
result$anio_mes_agresion <- format(result$fecha_agresion, "%Y-%m")
result$departamento <- case_match(result$departamento,
                                    "SanAndresProvidencia" ~ "San Andrés y Providencia",
                                    "NortedeSantander" ~ "Norte de Santander",
                                    "San Andrés" ~ "San Andrés y Providencia",
                                    "Valle" ~ "Valle del Cauca",
                                    .default = result$departamento)
result$departamento <- quitar_acentos(result$departamento)
result$departamento[result$departamento == "Narino"] <- "Nariño"
result$departamento[is.na(result$departamento)] <- "Sin información"
result$genero[is.na(result$genero)] <- "Sin información"
result$alerta_genero[is.na(result$alerta_genero)] <- "N/A"
result$sucedio_en_internet[is.na(result$sucedio_en_internet)] <- "N/A"
result$cargo[is.na(result$cargo)] <- "N/A"
result$presunto_autor <- case_match(result$presunto_autor,
                                    "Delincuencia común" ~ "Delincuencia Común",
                                    .default = result$presunto_autor)
result$presunto_autor[is.na(result$presunto_autor)] <- "N/A"
num_na <- sum(is.na(result$id))
result$id[is.na(result$id)] <- paste0("id_", sample.int(1e9, num_na))
func_paste <- function(x) paste(unique(x), collapse = ';')
# result2 <- result  |>
#   group_by(id) |>
#   summarise_each(list(func_paste))

agresiones <- read_csv("aux/agresiones.csv") |>
  drop_na()
tipo_agresion <- result |>
  select(id, tipo_agresion) |>
  separate_rows(tipo_agresion, sep = ";|(?<!y)/o\\b|\\by(?!/o\\b)")

tipo_agresion$tipo_agresion <- trimws(tipo_agresion$tipo_agresion)

tipo_agresion$tipo_agresion <- case_match(tipo_agresion$tipo_agresion,
                                          "ViolenciaSexual" ~ "Violencia sexual",
                                          "HeridoCubrimiento" ~ "Herido en cubrimiento",
                                          "TentativaHomicidio" ~ "Intento de Homicidio",
                                          "ObstruccionTrabajo" ~ "Obstrucción al trabajo periodístico",
                                          "DetencionIlegal" ~ "Detención ilegal",
                                          "Estigmatizacion" ~ "Estigmatización",
                                          "Acoso Judicial" ~ "Acoso judicial",
                                          "Intento de Homicidio" ~ "Intento de homicidio",
                                          "Acciones arbitrarias de redes sociales" ~ "Acciones arbitrarias en redes sociales",
                                          "AtentadoContraInfraestructura" ~ "Atentado contra infraestructura",
                                          "Obstrucción Trabajo Periodistico" ~ "Obstrucción al trabajo periodístico",
                                          "Daño a la infraestructura;" ~ "Daño a infraestructura",
                                          "Exlusión" ~ "Exclusión",
                                          "Trato Inhumano o degradante" ~ "Trato Inhumano o Degradante",
                                          .default = tipo_agresion$tipo_agresion)
tipo_agresion$tipo_agresion[is.na(tipo_agresion$tipo_agresion)] <- "Sin información"
tipo_agresion <- tipo_agresion |> filter(tipo_agresion != "Atención")

tipo_agresion <- tipo_agresion |>
  left_join(agresiones, by = "tipo_agresion")
tipo_agresion$agresion <- coalesce(tipo_agresion$`Nombre agresión actual`, tipo_agresion$tipo_agresion)
tipo_agresion <- tipo_agresion |>
  select(id, agresion) |>
  group_by(id) |>
  summarise_each(list(func_paste))
result <- result |>
  left_join(tipo_agresion, by = "id") |>
  select(-tipo_agresion)
result <- result |> rename(tipo_agresion = agresion)
result <- result |> drop_na(tipo_agresion)
result
}

data <- get_data("https://cms.flip.datasketch.co/api/agresiones")
write_csv(data, "data/agresiones.csv")

