# IPI manufacturero (INDEC) by presidency — ggplot2 — VERSION 2 (Dec-transition cutoffs)
# V2: cada presidencia INCLUYE el diciembre de su año de inicio y EXCLUYE el diciembre de su año final.
# Es decir, los mandatos son [dic(año inicio), nov(año fin)]:
#   Macri    : 2015-12 a 2019-11
#   Alberto  : 2019-12 a 2023-11
#   Milei    : 2023-12 en adelante
# Esto refleja las asunciones argentinas (10 de diciembre).
#
# Source: INDEC via SSPM / datos.gob.ar
# CSV: https://infra.datos.gob.ar/catalog/sspm/dataset/453/distribution/453.1/download/ipi-manufacturero.csv
# Series: seasonally-adjusted (serie_desestacionalizada), base 2004=100.
#
# Run: & "C:\Program Files\R\R-4.3.1\bin\Rscript.exe" ipi_industrial_v2.R

library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)
library(scales)

url <- "https://infra.datos.gob.ar/catalog/sspm/dataset/453/distribution/453.1/download/ipi-manufacturero.csv"
csv_file <- "ipi-manufacturero.csv"
if (!file.exists(csv_file)) {
  download.file(url, destfile = csv_file, mode = "wb")
}
message("Using: ", normalizePath(csv_file))

df <- read_csv(csv_file, show_col_types = FALSE)

df <- df %>%
  mutate(fecha = as.Date(indice_tiempo)) %>%
  arrange(fecha) %>%
  mutate(
    periodo = case_when(
      fecha >= as.Date("2016-01-01") & fecha <= as.Date("2019-11-30") ~ "macri",
      fecha >= as.Date("2019-12-01") & fecha <= as.Date("2023-11-30") ~ "alberto",
      fecha >= as.Date("2023-12-01") & fecha <= as.Date("2027-11-30") ~ "milei",
      TRUE ~ NA_character_
    ),
    periodo = factor(periodo, levels = c("macri", "alberto", "milei"))
  )

period_labels <- c(
  macri    = "M. Macri (dic-2015 a nov-2019)",
  alberto  = "A. Fernández (dic-2019 a nov-2023)",
  milei    = "J. Milei (desde dic-2023)"
)
period_colors <- c(
  macri    = "#C9A227",
  alberto  = "#1F5FA8",
  milei    = "#7B3FA0"
)

message("Range: ", min(df$fecha), " to ", max(df$fecha))

# ---- Plot 1 v2: index level, full sample, disconnected by period ----
p1 <- ggplot(df, aes(x = fecha, y = serie_desestacionalizada,
                     color = periodo, group = periodo)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = period_colors, labels = period_labels,
                     name = "Presidencia", drop = TRUE) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(0.01, 0.01)) +
  labs(
    title = "Argentina: Índice de Producción Industrial manufacturero (IPI, INDEC) [v2]",
    subtitle = "Serie desest., base 2004=100. Cortes v2: cada mandato incluye su diciembre inicial y excluye su diciembre final.",
    x = NULL,
    y = "Índice (2004=100, desest.)",
    caption = "Fuente: INDEC (vía SSPM/datos.gob.ar). Cortes v2 de transición de diciembre (ver leyenda). Por Rodrigo Quiroga. Ver github.com/rquiroga7/IPI"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", plot.caption = element_text(hjust = 0),
        plot.background = element_rect(fill = "white", colour = "white"),
        panel.background = element_rect(fill = "white", colour = "white"),
        legend.background = element_rect(fill = "white", colour = "white"))

ggsave("ipi_nivel_2016_2026_v2.png", p1, width = 10, height = 6, dpi = 300, bg = "white")
message("Saved ipi_nivel_2016_2026_v2.png")

# ---- Plot 2 v2: cumulative % change vs first month (December) of each presidency ----
# Ventanas v2 con base = último mes de la presidencia anterior (nivel heredado);
# Macri sin antecesor en la muestra -> base ene-2016. Fernández cierra en nov-23.
wins <- list(macri = c(as.Date("2016-01-01"), as.Date("2019-11-01")),
             alberto = c(as.Date("2019-11-01"), as.Date("2023-11-01")),
             milei = c(as.Date("2023-11-01"), max(df$fecha)))
df_rebased <- bind_rows(lapply(names(wins), function(p)
  df %>% filter(fecha >= wins[[p]][1], fecha <= wins[[p]][2]) %>%
    arrange(fecha) %>%
    mutate(periodo = p, mes_n = row_number() - 1L,
           base = first(serie_desestacionalizada),
           var_pct = 100 * (serie_desestacionalizada / base - 1)))) %>%
  mutate(periodo = factor(periodo, levels = c("macri", "alberto", "milei")))

print(df_rebased %>% group_by(periodo) %>% slice(1) %>%
        select(periodo, fecha, base) %>% as.data.frame())

p2 <- ggplot(df_rebased, aes(x = mes_n, y = var_pct, color = periodo, group = periodo)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_line(linewidth = 1) +
  scale_color_manual(values = period_colors, labels = period_labels,
                     name = "Presidencia", drop = TRUE) +
  labs(
    title = "IPI manufacturero: variación acumulada vs. el nivel heredado [v2]",
    subtitle = "Nivel heredado: nov-19 (Fernández) y nov-23 (Milei). Fernández cierra en nov-23.",
    x = "Meses desde el nivel heredado (mes 0)",
    y = "% de variación vs. nivel heredado (desest.)",
    caption = "Fuente: INDEC (vía SSPM/datos.gob.ar). IPI desest., base 2004=100. Cortes v2 de transición de diciembre. Por Rodrigo Quiroga. Ver github.com/rquiroga7/IPI"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", plot.caption = element_text(hjust = 0),
        plot.background = element_rect(fill = "white", colour = "white"),
        panel.background = element_rect(fill = "white", colour = "white"),
        legend.background = element_rect(fill = "white", colour = "white"))

ggsave("ipi_variacion_vs_inicio_v2.png", p2, width = 10, height = 6, dpi = 300, bg = "white")
message("Saved ipi_variacion_vs_inicio_v2.png")
message("Last available observation: ", max(df$fecha))
