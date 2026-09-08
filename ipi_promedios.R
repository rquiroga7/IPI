# Average IPI by presidency — v1 (calendar years) vs v2 (Dec-transition)
# Uses local ipi-manufacturero.csv (INDEC via SSPM). Averages of s.a. series (main),
# plus original and trend-cycle for reference. Base 2004=100.
# V1: Macri 2016-2019, Alberto 2020-2023, Milei 2024-
# V2: Macri dic-2015/nov-2019, Alberto dic-2019/nov-2023, Milei desde dic-2023.
# Run: & "C:\Program Files\R\R-4.3.1\bin\Rscript.exe" ipi_promedios.R

library(dplyr)
library(readr)

csv_file <- "ipi-manufacturero.csv"
df <- read_csv(csv_file, show_col_types = FALSE) %>%
  mutate(fecha = as.Date(indice_tiempo)) %>%
  arrange(fecha)

assign_v1 <- function(fecha) {
  dplyr::case_when(
    fecha >= as.Date("2016-01-01") & fecha <= as.Date("2019-12-31") ~ "M. Macri (2016-2019)",
    fecha >= as.Date("2020-01-01") & fecha <= as.Date("2023-12-31") ~ "A. Fernández (2020-2023)",
    fecha >= as.Date("2024-01-01") & fecha <= as.Date("2027-12-31") ~ "J. Milei (2024-2027)",
    TRUE ~ NA_character_
  )
}
assign_v2 <- function(fecha) {
  dplyr::case_when(
    fecha >= as.Date("2016-01-01") & fecha <= as.Date("2019-11-30") ~ "M. Macri (dic-2015 a nov-2019)",
    fecha >= as.Date("2019-12-01") & fecha <= as.Date("2023-11-30") ~ "A. Fernández (dic-2019 a nov-2023)",
    fecha >= as.Date("2023-12-01") & fecha <= as.Date("2027-11-30") ~ "J. Milei (desde dic-2023)",
    TRUE ~ NA_character_
  )
}

avg_table <- function(version) {
  df %>%
    mutate(presidency = if (version == "v1") assign_v1(fecha) else assign_v2(fecha)) %>%
    filter(!is.na(presidency)) %>%
    group_by(presidency) %>%
    summarise(
      n_months = n(),
      start = min(fecha),
      end = max(fecha),
      avg_sa = mean(serie_desestacionalizada, na.rm = TRUE),
      avg_original = mean(serie_original, na.rm = TRUE),
      avg_trend = mean(serie_tendencia_ciclo, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(start)
}

t_v1 <- avg_table("v1")
t_v2 <- avg_table("v2")

cat("\n=== TABLE V1 (calendar years) — average IPI ===\n")
print(as.data.frame(t_v1), digits = 6)
cat("\n=== TABLE V2 (Dec-transition) — average IPI ===\n")
print(as.data.frame(t_v2), digits = 6)

write_csv(t_v1, "ipi_promedio_presidencia_v1.csv")
write_csv(t_v2, "ipi_promedio_presidencia_v2.csv")
message("Saved ipi_promedio_presidencia_v1.csv and ipi_promedio_presidencia_v2.csv")
