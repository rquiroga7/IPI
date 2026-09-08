# IPI manufacturero by group: Alimentos / Petroleo+Quimicos / Resto — ggplot2
# Groups (original series, base 2004=100):
#   Alimentos      = alimentos_bebidas (w=24.46)
#   Petro+Quimicos = (4.17*refinacion + 12.71*sustancias)/16.88  (weighted, INDEC 2004 weights)
#   Resto          = (100*nivel_general - 24.46*alim - 4.17*ref - 12.71*qui)/58.66  (Laspeyres residual)
# Weights: INDEC metodologia_ipi_manufacturero_2019 (VABpb 2004). Sum published = 99.98 (rounding); use 100.
# Sector file 453.2 columns are original indices; its last column (labelled recliclamiento_...)
# is numerically identical to nivel general original (max diff < 1e-12), so total taken from 453.1.
# Presidency cutoffs v1 (calendar) and v2 (Dec-transition) as in prior scripts.
# Run: & "C:\Program Files\R\R-4.3.1\bin\Rscript.exe" ipi_grupos.R

library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)

url_ng <- "https://infra.datos.gob.ar/catalog/sspm/dataset/453/distribution/453.1/download/ipi-manufacturero.csv"
url_se <- "https://infra.datos.gob.ar/catalog/sspm/dataset/453/distribution/453.2/download/ipi-manufacturero-sectores.csv"
f_ng <- "ipi-manufacturero.csv"
f_se <- "ipi-manufacturero-sectores.csv"
if (!file.exists(f_ng)) download.file(url_ng, f_ng, mode = "wb")
if (!file.exists(f_se)) download.file(url_se, f_se, mode = "wb")

w_ali <- 24.46; w_ref <- 4.17; w_qui <- 12.71
w_pq <- w_ref + w_qui
w_rest <- 100 - w_ali - w_ref - w_qui

ng <- read_csv(f_ng, show_col_types = FALSE) %>% mutate(fecha = as.Date(indice_tiempo)) %>% arrange(fecha)
se <- read_csv(f_se, show_col_types = FALSE) %>% mutate(fecha = as.Date(indice_tiempo)) %>% arrange(fecha)

g <- ng %>% select(fecha, ng_original = serie_original) %>%
  left_join(se %>% select(fecha, alimentos_bebidas, refinacion_petroleo_coque_combustible_nuclear,
                          sustancias_productos_quimicos), by = "fecha") %>%
  mutate(
    alimentos = alimentos_bebidas,
    petro_quim = (w_ref * refinacion_petroleo_coque_combustible_nuclear + w_qui * sustancias_productos_quimicos) / w_pq,
    resto = (100 * ng_original - w_ali * alimentos_bebidas - w_ref * refinacion_petroleo_coque_combustible_nuclear -
               w_qui * sustancias_productos_quimicos) / w_rest
  ) %>%
  select(fecha, alimentos, petro_quim, resto, ng_original)

cat("Group means (full sample):\n")
print(colMeans(g[, c("alimentos", "petro_quim", "resto", "ng_original")], na.rm = TRUE))

write_csv(g, "ipi-grupos.csv")
message("Saved ipi-grupos.csv")

gl <- g %>% select(-ng_original) %>%
  pivot_longer(c(alimentos, petro_quim, resto), names_to = "grupo_raw", values_to = "indice") %>%
  mutate(grupo = factor(dplyr::recode(grupo_raw, alimentos = "Alimentos y bebidas",
                                      petro_quim = "Petróleo + Químicos", resto = "Resto"),
                        levels = c("Alimentos y bebidas", "Petróleo + Químicos", "Resto")))

period_colors <- c(macri = "#C9A227", alberto = "#2E9D5B", milei = "#7B3FA0")
lab_v1 <- c(macri = "M. Macri (2016-2019)",
            alberto = "A. Fernández (2020-2023)", milei = "J. Milei (2024-2027)")
lab_v2 <- c(macri = "M. Macri (dic-2015/nov-2019)", alberto = "A. Fernández (dic-2019/nov-2023)",
            milei = "J. Milei (desde dic-2023)")

add_periods <- function(d, v) {
  if (v == "v1") d %>% mutate(periodo = dplyr::case_when(
      fecha >= as.Date("2016-01-01") & fecha <= as.Date("2019-12-31") ~ "macri",
      fecha >= as.Date("2020-01-01") & fecha <= as.Date("2023-12-31") ~ "alberto",
      fecha >= as.Date("2024-01-01") & fecha <= as.Date("2027-12-31") ~ "milei",
      TRUE ~ NA_character_))
  else d %>% mutate(periodo = dplyr::case_when(
      fecha >= as.Date("2016-01-01") & fecha <= as.Date("2019-11-30") ~ "macri",
      fecha >= as.Date("2019-12-01") & fecha <= as.Date("2023-11-30") ~ "alberto",
      fecha >= as.Date("2023-12-01") & fecha <= as.Date("2027-11-30") ~ "milei",
      TRUE ~ NA_character_))
}

white_theme <- theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", plot.caption = element_text(hjust = 0, size = 9),
        plot.background = element_rect(fill = "white", colour = "white"),
        panel.background = element_rect(fill = "white", colour = "white"),
        legend.background = element_rect(fill = "white", colour = "white"))

cap_nivel <- paste0("Fuente: INDEC 453.1 + 453.2 vía SSPM.\n",
  "Alimentos y bebidas = div. 15 (carnes, lácteos, molienda, panadería, azúcar, yerba, bebidas, vino y otros).\n",
  "Petróleo+Químicos = div. 23 (refinación: naftas, gasoil, fueloil, asfaltos) + div. 24 ",
  "(químicos: básicos, agroquímicos, farmacia, pinturas, detergentes y otros).\n",
  "Resto = otras 13 divisiones (tabaco, textiles, vestimenta y calzado, madera y papel, caucho y plástico,\n",
  "minerales no metálicos, metálicas básicas, metal, maquinaria, otros equipos, automotores y autopartes, ",
  "otro transporte, muebles y otras).")
cap_var <- paste0("Fuente: INDEC vía SSPM. Índices originales (estacionales); el rebaseo no elimina la estacionalidad.\n",
  "Alimentos = div. 15 (carnes, lácteos, molienda, panadería, azúcar, yerba, bebidas, vino y otros). ",
  "Petro+Quím = div. 23 (refinación) + div. 24 (químicos, farmacia, pinturas, detergentes y otros).\n",
  "Resto = otras 13 divisiones (tabaco, textiles, vestimenta y calzado, madera y papel, caucho y plástico,\n",
  "minerales no metálicos, metálicas básicas, metal, maquinaria, otros equipos, automotores, otro transporte, ",
  "muebles y otras).")

for (v in c("v1", "v2")) {
  labs <- if (v == "v1") lab_v1 else lab_v2
  dlev <- gl %>% add_periods(v) %>%
    mutate(periodo = factor(periodo, levels = names(period_colors))) %>% filter(!is.na(periodo))
  p1 <- ggplot(dlev, aes(fecha, indice, color = periodo, group = periodo)) +
    geom_line(linewidth = 0.9) +
    facet_wrap(~grupo, scales = "free_y") +
    scale_color_manual(values = period_colors, labels = labs, name = "Presidencia", drop = TRUE) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    labs(title = paste0("IPI por grupo, serie original (base 2004=100) [", v, "]"),
         subtitle = "Petro+Quím = ponderado (4,17/12,71); Resto = residuo Laspeyres. Tramos desconectados por presidencia.",
         x = NULL, y = "Índice (original)",
         caption = cap_nivel) +
    white_theme
  fn1 <- paste0("ipi_grupos_nivel_", v, ".png")
  ggsave(fn1, p1, width = 12, height = 6, dpi = 300, bg = "white")
  message("Saved ", fn1)

  dreb <- dlev %>% group_by(grupo, periodo) %>% arrange(fecha) %>%
    mutate(mes_n = row_number() - 1L, base = first(indice), var_pct = 100 * (indice / base - 1)) %>% ungroup()
  p2 <- ggplot(dreb, aes(mes_n, var_pct, color = periodo, group = periodo)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
    geom_line(linewidth = 0.9) +
    facet_wrap(~grupo, scales = "free_y") +
    scale_color_manual(values = period_colors, labels = labs, name = "Presidencia", drop = TRUE) +
    labs(title = paste0("IPI por grupo: % acumulado vs. primer mes [", v, "]"),
         x = "Meses desde el primer mes del período", y = "% vs. primer mes (original)",
         caption = cap_var) +
    white_theme
  fn2 <- paste0("ipi_grupos_variacion_", v, ".png")
  ggsave(fn2, p2, width = 12, height = 6, dpi = 300, bg = "white")
  message("Saved ", fn2)

  tab <- dlev %>% group_by(periodo, grupo) %>%
    summarise(n = n(), start = min(fecha), end = max(fecha), avg = mean(indice, na.rm = TRUE), .groups = "drop") %>%
    arrange(grupo, start)
  fn3 <- paste0("ipi_grupos_promedio_", v, ".csv")
  write_csv(tab, fn3)
  message("Saved ", fn3)
  print(as.data.frame(tab), digits = 5)
}
