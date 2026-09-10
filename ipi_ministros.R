# IPI total — copia secundaria de variación v1/v2 con Fernández dividido por ministro
# Corte en jul-22: Guzmán (hasta jul-22) y Massa (desde jul-22, incluye interinato Batakis).
# Cada tramo arranca en 0 (base propia). V1: Guzmán dic-19→dic-23? no: Guzmán dic-19→jul-22,
# Massa jul-22→dic-23. V2: Guzmán nov-19→jul-22, Massa jul-22→nov-23.
# Run: & "C:\Program Files\R\R-4.3.1\bin\Rscript.exe" ipi_ministros.R

library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)
library(scales)

f <- "ipi-manufacturero.csv"
if (!file.exists(f)) download.file(
  "https://infra.datos.gob.ar/catalog/sspm/dataset/453/distribution/453.1/download/ipi-manufacturero.csv",
  f, mode = "wb")
df <- read_csv(f, show_col_types = FALSE) %>% mutate(fecha = as.Date(indice_tiempo)) %>% arrange(fecha)

cols <- c(macri = "#C9A227", guzman = "#1F5FA8", massa = "#87CEEB", milei = "#7B3FA0")
lab_v1 <- c(macri = "Macri (2016-2019)",
            guzman = "Fernández/Guzmán (dic-19–jul-22)",
            massa = "Fernández/Massa (jul-22–dic-23)",
            milei = "Milei (2024-2027)")
lab_v2 <- c(macri = "Macri (2016-2019)",
            guzman = "Fernández/Guzmán (nov-19–jul-22)",
            massa = "Fernández/Massa (jul-22–nov-23)",
            milei = "Milei (desde dic-2023)")
wins_v1 <- list(macri = c(as.Date("2016-01-01"), as.Date("2019-12-01")),
                guzman = c(as.Date("2019-12-01"), as.Date("2022-07-01")),
                massa = c(as.Date("2022-07-01"), as.Date("2023-12-01")),
                milei = c(as.Date("2023-12-01"), max(df$fecha)))
wins_v2 <- list(macri = c(as.Date("2016-01-01"), as.Date("2019-11-01")),
                guzman = c(as.Date("2019-11-01"), as.Date("2022-07-01")),
                massa = c(as.Date("2022-07-01"), as.Date("2023-11-01")),
                milei = c(as.Date("2023-11-01"), max(df$fecha)))

white_theme <- theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", plot.caption = element_text(hjust = 0),
        legend.text = element_text(size = 10),
        plot.background = element_rect(fill = "white", colour = "white"),
        panel.background = element_rect(fill = "white", colour = "white"),
        legend.background = element_rect(fill = "white", colour = "white"))

for (v in c("v1", "v2")) {
  wins <- if (v == "v1") wins_v1 else wins_v2
  labs <- if (v == "v1") lab_v1 else lab_v2
  dr <- bind_rows(lapply(names(wins), function(p)
    df %>% filter(fecha >= wins[[p]][1], fecha <= wins[[p]][2]) %>%
      arrange(fecha) %>%
      mutate(seg = p, mes_n = row_number() - 1L,
             base = first(serie_desestacionalizada),
             var_pct = 100 * (serie_desestacionalizada / base - 1)))) %>%
    mutate(seg = factor(seg, levels = names(cols)))
  p <- ggplot(dr, aes(mes_n, var_pct, color = seg, group = seg)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
    geom_line(linewidth = 1) +
    scale_color_manual(values = cols, labels = labs, name = "Presidencia / ministro", drop = TRUE) +
    labs(title = paste0("IPI: variación vs. nivel heredado, Fernández por ministro [", v, "]"),
         subtitle = "Guzmán hasta jul-22 y Massa desde jul-22 (incluye interinato Batakis); cada tramo arranca en 0.",
         x = "Meses desde la base del tramo (mes 0)", y = "% de variación vs. base del tramo (desest.)",
          caption = "Fuente: INDEC (vía SSPM/datos.gob.ar). IPI desestacionalizado, base 2004=100. Por Rodrigo Quiroga. Ver github.com/rquiroga7/IPI") +
    white_theme
  fn <- paste0("ipi_variacion_ministros_", v, ".png")
  ggsave(fn, p, width = 11, height = 6, dpi = 300, bg = "white")
  message("Guardado ", fn)
}
