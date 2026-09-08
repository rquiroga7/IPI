# IPI grupos desestacionalizados — MÉTODO INDEC (indirecto, nota metodológica 2025)
# Nivel general INDEC = agregación de las 16 divisiones ajustadas por separado.
# Aquí: se ajusta cada una de las 16 divisiones con su especificación INDEC 2025
# (transformación, modo aditivo/multiplicativo, ARIMA, Pascua, bisiesto, días de
# actividad, outliers, filtros estacionales 3xN y de tendencia H9/H13) y luego se agrega:
#   alimentos_sa  = SA(div.15)
#   petro_quim_sa = (4.17*SA23 + 12.71*SA24)/16.88
#   resto_sa      = promedio ponderado de las SA de las otras 13 divisiones
# Diferencias vs. INDEC oficial: ponderadores publicados redondeados (suman 99.98),
# muestra hasta jun-2026 (INDEC calibró a ene-2025) y versión de X-13.
# Supera al script anterior (ajuste directo con spec genérica log/multiplicativa).
# Run: & "C:\Program Files\R\R-4.3.1\bin\Rscript.exe" ipi_grupos_sa.R

library(seasonal)
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
source("md_helper.R")

se <- read_csv("ipi-manufacturero-sectores.csv", show_col_types = FALSE) %>% arrange(indice_tiempo)
ng <- read_csv("ipi-manufacturero.csv", show_col_types = FALSE) %>% arrange(indice_tiempo)
fechas <- as.Date(se$indice_tiempo)
message("Muestra: ", min(fechas), " a ", max(fechas), " (n=", nrow(se), ")")

# Especificación INDEC 2025 por división (en orden de columnas del CSV)
spec <- tibble::tribble(
  ~col, ~w, ~trans, ~mode, ~arima, ~easter, ~lpyear, ~td, ~seasf, ~trend,
  "alimentos_bebidas", 24.46, "none", "add", "(0 1 1)(0 1 1)", FALSE, FALSE, TRUE, "s3x5", 13,
  "productos_tabaco", 0.69, "none", "add", "(0 0 1)(0 1 1)", TRUE, FALSE, TRUE, "s3x3", 9,
  "productos_textiles", 2.93, "none", "add", "(1 0 1)(0 1 1)", FALSE, FALSE, TRUE, "s3x3", 9,
  "prendas_vestir_cuero_calzado", 6.12, "none", "add", "(1 0 0)(0 1 1)", TRUE, FALSE, TRUE, "s3x3", 13,
  "madera_papel_edicion_impresion", 9.40, "none", "add", "(0 1 1)(0 1 1)", FALSE, TRUE, TRUE, "s3x5", 13,
  "refinacion_petroleo_coque_combustible_nuclear", 4.17, "none", "add", "(0 1 1)(0 1 1)", FALSE, TRUE, TRUE, "s3x9", 9,
  "sustancias_productos_quimicos", 12.71, "none", "add", "(0 1 1)(1 1 0)", FALSE, TRUE, TRUE, "s3x9", 13,
  "productos_caucho_plastico", 4.93, "none", "add", "(0 1 1)(0 1 1)", FALSE, FALSE, TRUE, "s3x5", 9,
  "productos_minerales_no_metalicos", 3.68, "none", "add", "(0 1 1)(0 1 1)", FALSE, TRUE, TRUE, "s3x9", 9,
  "industrias_metalicas_basicas", 9.05, "log", "mult", "(0 1 1)(1 1 0)", FALSE, FALSE, FALSE, "s3x9", 13,
  "productos_metal", 5.15, "none", "add", "(0 1 1)(1 1 0)", FALSE, TRUE, TRUE, "s3x9", 13,
  "maquinaria_equipo", 4.58, "none", "add", "(0 1 1)(0 1 1)", TRUE, FALSE, TRUE, "s3x5", 13,
  "otros_equipos_aparatos_instrumentos", 3.36, "log", "mult", "(1 0 1)(1 1 0)", FALSE, FALSE, TRUE, "s3x9", 13,
  "vehiculos_automotores_carrocerias_remolques_autopartes", 3.95, "none", "add", "(0 1 1)(0 1 1)", FALSE, FALSE, TRUE, "s3x5", 13,
  "otro_equipo_de_transporte", 0.58, "none", "add", "(0 1 1)(1 1 0)", FALSE, FALSE, TRUE, "s3x9", 13,
  "muebles_colchones_otras_industrias_manufactureras", 4.22, "none", "add", "(0 1 1)(0 1 1)", FALSE, FALSE, TRUE, "s3x5", 13
)

sa_list <- list()
fallback <- c()
for (i in seq_len(nrow(spec))) {
  s <- spec[i, ]
  tsx <- ts(se[[s$col]], start = c(2016, 1), frequency = 12)
  # INDEC pide días de actividad + bisiesto en varias divisiones: en X-13 eso es
  # tdnolpyear + lpyear (td solo ya incluye tratamiento propio y rechaza lpyear).
  regs <- c(if (s$td && s$lpyear) c("tdnolpyear", "lpyear") else if (s$td) "td",
            if (s$easter) "easter[8]")
  if (length(regs) == 0) regs <- NULL
  fit <- tryCatch(
    seas(tsx, transform.function = s$trans, regression.variables = regs, regression.aictest = NULL,
         outlier.types = "all", arima.model = s$arima,
         x11 = "", x11.mode = s$mode, x11.seasonalma = s$seasf, x11.trendma = s$trend),
    error = function(e) NULL
  )
  if (is.null(fit)) {
    message("FALLO spec INDEC en ", s$col, " -> fallback X-11+log automático")
    fit <- seas(tsx, transform.function = "log", x11 = "")
    fallback <- c(fallback, s$col)
  }
  sa_list[[s$col]] <- as.numeric(final(fit))
  message(s$col, ": ok")
}
if (length(fallback) > 0) message("Fallback usado en: ", paste(fallback, collapse = ", ")) else message("Sin fallbacks: las 16 divisiones con spec INDEC.")

SA <- as_tibble(sa_list)
w <- spec$w
is_pq <- spec$col %in% c("refinacion_petroleo_coque_combustible_nuclear", "sustancias_productos_quimicos")
is_ali <- spec$col == "alimentos_bebidas"
w_pq <- sum(w[is_pq]); w_rest <- sum(w[!is_ali & !is_pq])

gsa <- tibble(
  fecha = fechas,
  alimentos_sa = SA$alimentos_bebidas,
  petro_quim_sa = (w[6] * SA[[6]] + w[7] * SA[[7]]) / w_pq,
  resto_sa = as.numeric(as.matrix(SA[, !is_ali & !is_pq]) %*% (w[!is_ali & !is_pq] / w_rest))
)
write_csv(gsa, "ipi-grupos-sa.csv")
message("Guardado ipi-grupos-sa.csv (método indirecto INDEC)")

# Validación: NG indirecto (16 divisiones) vs SA oficial
ng_ind <- as.numeric(as.matrix(SA) %*% (w / 100))
cmp <- tibble(fecha = fechas, oficial = ng$serie_desestacionalizada, indirecto = ng_ind)
cat("Corr. NG indirecto vs oficial:", cor(cmp$oficial, cmp$indirecto),
    " | dif. abs. media:", mean(abs(cmp$oficial - cmp$indirecto)), "\n")

gl <- gsa %>%
  pivot_longer(c(alimentos_sa, petro_quim_sa, resto_sa), names_to = "grupo_raw", values_to = "indice_sa") %>%
  mutate(grupo = factor(dplyr::recode(grupo_raw, alimentos_sa = "Alimentos y bebidas",
                                      petro_quim_sa = "Petróleo + Químicos", resto_sa = "Resto"),
                        levels = c("Alimentos y bebidas", "Petróleo + Químicos", "Resto")))

period_colors <- c(macri = "#C9A227", alberto = "#1F5FA8", milei = "#7B3FA0")
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

cap_sa_nivel <- paste0("Fuente: INDEC 453.1+453.2 vía SSPM. Desest. indirecto: X-13 por división con especificación INDEC (nota 2025), agregación por ponderadores.\n",
  "Alimentos y bebidas = div. 15 (carnes, lácteos, molienda, panadería, azúcar, yerba, bebidas, vino y otros).\n",
  "Petróleo+Químicos = div. 23 (refinación: naftas, gasoil, fueloil, asfaltos) + div. 24 ",
  "(químicos: básicos, agroquímicos, farmacia, pinturas, detergentes y otros).\n",
  "Resto = otras 13 divisiones (tabaco, textiles, vestimenta y calzado, madera y papel, caucho y plástico,\n",
  "minerales no metálicos, metálicas básicas, metal, maquinaria, otros equipos, automotores y autopartes, ",
  "otro transporte, muebles y otras).")
cap_sa_var <- paste0("Fuente: INDEC vía SSPM. Desest. indirecto X-13 por división (spec INDEC 2025).\n",
  "Alimentos = div. 15 (carnes, lácteos, molienda, panadería, azúcar, yerba, bebidas, vino y otros). ",
  "Petro+Quím = div. 23 (refinación) + div. 24 (químicos, farmacia, pinturas, detergentes y otros).\n",
  "Resto = otras 13 divisiones (tabaco, textiles, vestimenta y calzado, madera y papel, caucho y plástico,\n",
  "minerales no metálicos, metálicas básicas, metal, maquinaria, otros equipos, automotores, otro transporte, ",
  "muebles y otras).")

tabs <- list(); punts <- list()
for (v in c("v1", "v2")) {
  labs <- if (v == "v1") lab_v1 else lab_v2
  dlev <- gl %>% add_periods(v) %>%
    mutate(periodo = factor(periodo, levels = names(period_colors))) %>% filter(!is.na(periodo))
  p1 <- ggplot(dlev, aes(fecha, indice_sa, color = periodo, group = periodo)) +
    geom_line(linewidth = 0.9) +
    facet_wrap(~grupo, scales = "free_y") +
    scale_color_manual(values = period_colors, labels = labs, name = "Presidencia", drop = TRUE) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    labs(title = paste0("IPI por grupo, desestacionalizado método INDEC (base 2004=100) [", v, "]"),
         subtitle = "Ajuste indirecto: X-13 por división (spec INDEC 2025) y agregación por ponderadores. Tramos desconectados.",
         x = NULL, y = "Índice (desest.)",
         caption = cap_sa_nivel) +
    white_theme
  fn1 <- paste0("ipi_grupos_sa_nivel_", v, ".png")
  ggsave(fn1, p1, width = 12, height = 7, dpi = 300, bg = "white")
  message("Guardado ", fn1)

  # Variación vs. nivel heredado (último mes de la presidencia anterior); Fernández cierra en nov-23
  wins <- if (v == "v1") list(macri = c(as.Date("2016-01-01"), as.Date("2019-12-01")),
                              alberto = c(as.Date("2019-12-01"), as.Date("2023-12-01")),
                              milei = c(as.Date("2023-12-01"), max(dlev$fecha))) else
                        list(macri = c(as.Date("2016-01-01"), as.Date("2019-11-01")),
                              alberto = c(as.Date("2019-11-01"), as.Date("2023-11-01")),
                              milei = c(as.Date("2023-11-01"), max(dlev$fecha)))
  dreb <- bind_rows(lapply(names(wins), function(p)
    dlev %>% filter(fecha >= wins[[p]][1], fecha <= wins[[p]][2]) %>%
      arrange(fecha) %>% mutate(periodo = p))) %>%
    mutate(periodo = factor(periodo, levels = names(period_colors))) %>%
    group_by(grupo, periodo) %>% arrange(fecha) %>%
    mutate(mes_n = row_number() - 1L, base = first(indice_sa), var_pct = 100 * (indice_sa / base - 1)) %>% ungroup()
  p2 <- ggplot(dreb, aes(mes_n, var_pct, color = periodo, group = periodo)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
    geom_line(linewidth = 0.9) +
    facet_wrap(~grupo, scales = "free_y") +
    scale_color_manual(values = period_colors, labels = labs, name = "Presidencia", drop = TRUE) +
    labs(title = paste0("IPI por grupo (desest. INDEC): % acumulado vs. nivel heredado [", v, "]"),
         subtitle = if (v == "v1") "Base = último mes heredado (dic-19 y dic-23)." else
           "Base = último mes heredado (nov-19 y nov-23). Fernández cierra en nov-23.",
         x = "Meses desde el nivel heredado (mes 0)", y = "% vs. nivel heredado (desest.)",
         caption = cap_sa_var) +
    white_theme
  fn2 <- paste0("ipi_grupos_sa_variacion_", v, ".png")
  ggsave(fn2, p2, width = 12, height = 7, dpi = 300, bg = "white")
  message("Guardado ", fn2)

  tab <- dlev %>% group_by(periodo, grupo) %>%
    summarise(n = n(), start = min(fecha), end = max(fecha), avg_sa = mean(indice_sa, na.rm = TRUE), .groups = "drop") %>%
    arrange(grupo, start)
  tabs[[v]] <- tab
  punts[[v]] <- dreb %>% group_by(periodo, grupo) %>% arrange(fecha) %>%
    summarise(Base = first(fecha), Cierre = last(fecha),
              Nivel_heredado = first(indice_sa), Nivel_cierre = last(indice_sa),
              Variación = 100 * (last(indice_sa) / first(indice_sa) - 1), .groups = "drop")
  message("Tablas promedio + punta a punta ", v, " listas")
}

gov <- c(macri = "M. Macri", alberto = "A. Fernández", milei = "J. Milei")
grs <- c("Alimentos y bebidas", "Petróleo + Químicos", "Resto")
govs <- c("macri", "alberto", "milei")
wide_gr <- function(tab, valcol) {
  mat <- sapply(govs, function(g) vapply(grs, function(gr) tab[[valcol]][tab$periodo == g & tab$grupo == gr][1], numeric(1)))
  idx <- match(govs, tab$periodo)
  list(mat = mat, starts = tab$start[idx], ends = tab$end[idx], ns = tab$n[idx])
}
srcsa <- "Fuente: INDEC 453.1+453.2 vía SSPM. Desest. indirecto X-13 por división (spec INDEC 2025). Base 2004=100."
w1 <- wide_gr(tabs$v1, "avg_sa"); w2 <- wide_gr(tabs$v2, "avg_sa")
gov_wide_md("Grupo", grs, w1$mat, w1$starts, w1$ends, w1$ns,
  "Nivel promedio por gobierno y grupo, desestacionalizado (V1)", srcsa,
  "tabla_promedio_grupos_sa.md", append = FALSE)
gov_wide_md("Grupo", grs, w2$mat, w2$starts, w2$ends, w2$ns,
  "Nivel promedio por gobierno y grupo, desestacionalizado (V2)", srcsa,
  "tabla_promedio_grupos_sa.md", append = TRUE)

wide_punta <- function(tab) {
  mat <- sapply(govs, function(g) vapply(grs, function(gr) tab$Variación[tab$periodo == g & tab$grupo == gr][1], numeric(1)))
  idx <- match(govs, tab$periodo)
  n_int <- mapply(function(b, e) (as.integer(format(e, "%Y")) - as.integer(format(b, "%Y"))) * 12L +
    (as.integer(format(e, "%m")) - as.integer(format(b, "%m"))),
    tab$Base[idx], tab$Cierre[idx])
  list(mat = mat, starts = tab$Base[idx], ends = tab$Cierre[idx], ns = n_int)
}
srcp <- "Fuente: INDEC vía SSPM, desest. INDEC. Variación punta a punta vs. último mes de la presidencia anterior (nivel heredado)."
p1 <- wide_punta(punts$v1); p2 <- wide_punta(punts$v2)
gov_wide_md("Grupo", grs, p1$mat, p1$starts, p1$ends, p1$ns,
  "Diferencia punta a punta por gobierno y grupo (V1, % vs. nivel heredado)", srcp,
  "tabla_punta_a_punta.md", append = FALSE, suffix = "%", signed = TRUE)
gov_wide_md("Grupo", grs, p2$mat, p2$starts, p2$ends, p2$ns,
  "Diferencia punta a punta por gobierno y grupo (V2, % vs. nivel heredado)", srcp,
  "tabla_punta_a_punta.md", append = TRUE, suffix = "%", signed = TRUE)
