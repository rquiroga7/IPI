# IPI manufacturero (INDEC) by presidency — ggplot2
# Source: INDEC via SSPM / datos.gob.ar
# CSV: https://infra.datos.gob.ar/catalog/sspm/dataset/453/distribution/453.1/download/ipi-manufacturero.csv
# Series: base 2004=100, monthly. Uses seasonally-adjusted series (serie_desestacionalizada)
# for level and rebased plots (avoids seasonal distortion).
#
# Presidencias (bloques de años calendario):
#   2016-2019 dorado      -> Mauricio Macri
#   2020-2023 verde       -> Alberto Fernández
#   2024-2027 violeta     -> Javier Milei
#
# Muestra disponible: ene-2016 en adelante.
#
# Run: & "C:\Program Files\R\R-4.3.1\bin\Rscript.exe" ipi_industrial.R

library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)
library(scales)

url <- "https://infra.datos.gob.ar/catalog/sspm/dataset/453/distribution/453.1/download/ipi-manufacturero.csv"
csv_file <- "ipi-manufacturero.csv"

download.file(url, destfile = csv_file, mode = "wb")
message("Downloaded to: ", normalizePath(csv_file))

df <- read_csv(csv_file, show_col_types = FALSE)
names(df)
# Expected: indice_tiempo, serie_original, serie_desestacionalizada, serie_tendencia_ciclo

df <- df %>%
  mutate(
    fecha = as.Date(indice_tiempo),
    anio = year(fecha)
  ) %>%
  arrange(fecha) %>%
  mutate(
    periodo = case_when(
      fecha >= as.Date("2016-01-01") & fecha <= as.Date("2019-12-31") ~ "macri",
      fecha >= as.Date("2020-01-01") & fecha <= as.Date("2023-12-31") ~ "alberto",
      fecha >= as.Date("2024-01-01") & fecha <= as.Date("2027-12-31") ~ "milei",
      TRUE ~ NA_character_
    ),
    periodo = factor(periodo, levels = c("macri", "alberto", "milei"))
  )

# Labels + colors (exact mapping requested)
period_labels <- c(
  macri    = "M. Macri (2016-2019)",
  alberto  = "A. Fernández (2020-2023)",
  milei    = "J. Milei (2024-2027)"
)
period_colors <- c(
  macri    = "#C9A227",  # dorado
  alberto  = "#2E9D5B",  # verde
  milei    = "#7B3FA0"   # violeta
)

message("Range: ", min(df$fecha), " to ", max(df$fecha))
print(tail(df, 3))

# ---- Plot 1: index level, 2016 to last available, disconnected by period ----
df_plot1 <- df %>% filter(fecha >= as.Date("2016-01-01"))

p1 <- ggplot(df_plot1, aes(x = fecha, y = serie_desestacionalizada,
                           color = periodo, group = periodo)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = period_colors, labels = period_labels,
                     name = "Presidencia", drop = TRUE) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(0.01, 0.01)) +
  labs(
    title = "Argentina: Índice de Producción Industrial manufacturero (IPI, INDEC)",
    subtitle = "Serie desestacionalizada, base 2004=100. Ene-2016 hasta el último dato disponible. Tramos desconectados por presidencia.",
    x = NULL,
    y = "Índice (2004=100, desest.)",
    caption = "Fuente: INDEC (vía SSPM/datos.gob.ar, distr. 453.1)."
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", plot.caption = element_text(hjust = 0),
        plot.background = element_rect(fill = "white", colour = "white"),
        panel.background = element_rect(fill = "white", colour = "white"),
        legend.background = element_rect(fill = "white", colour = "white"))

ggsave("ipi_nivel_2016_2026.png", p1, width = 10, height = 6, dpi = 300, bg = "white")
message("Saved ipi_nivel_2016_2026.png")

# ---- Plot 2: cumulative % change vs first month of each presidency ----
# Ventanas con base = último mes de la presidencia anterior (nivel heredado);
# Macri sin antecesor en la muestra -> base ene-2016. Fernández cierra en nov-23.
wins <- list(macri = c(as.Date("2016-01-01"), as.Date("2019-12-01")),
             alberto = c(as.Date("2019-12-01"), as.Date("2023-11-01")),
             milei = c(as.Date("2023-11-01"), max(df$fecha)))
df_rebased <- bind_rows(lapply(names(wins), function(p)
  df %>% filter(fecha >= wins[[p]][1], fecha <= wins[[p]][2]) %>%
    arrange(fecha) %>%
    mutate(periodo = p, mes_n = row_number() - 1L,
           base = first(serie_desestacionalizada),
           var_pct = 100 * (serie_desestacionalizada / base - 1)))) %>%
  mutate(periodo = factor(periodo, levels = c("macri", "alberto", "milei")))

# show bases used
print(df_rebased %>% group_by(periodo) %>% slice(1) %>%
        select(periodo, fecha, base) %>% as.data.frame())

p2 <- ggplot(df_rebased, aes(x = mes_n, y = var_pct, color = periodo, group = periodo)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_line(linewidth = 1) +
  scale_color_manual(values = period_colors, labels = period_labels,
                     name = "Presidencia", drop = TRUE) +
  labs(
    title = "IPI manufacturero: variación acumulada vs. el nivel heredado",
    subtitle = "Nivel heredado: dic-19 (Fernández) y nov-23 (Milei). Fernández cierra en nov-23.",
    x = "Meses desde el nivel heredado (mes 0)",
    y = "% de variación vs. nivel heredado (desest.)",
    caption = "Fuente: INDEC (vía SSPM/datos.gob.ar). IPI desestacionalizado, base 2004=100."
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", plot.caption = element_text(hjust = 0),
        plot.background = element_rect(fill = "white", colour = "white"),
        panel.background = element_rect(fill = "white", colour = "white"),
        legend.background = element_rect(fill = "white", colour = "white"))

ggsave("ipi_variacion_vs_inicio.png", p2, width = 10, height = 6, dpi = 300, bg = "white")
message("Saved ipi_variacion_vs_inicio.png")

message("Last available observation: ", max(df$fecha))
