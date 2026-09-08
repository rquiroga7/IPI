# Helper: tablas markdown con estilo papel profesional (tabla HTML + filas alternadas)
# Uso: source("md_helper.R"); write_md_table(df, col_labels, num_cols, title, source, file, append)
# - col_labels: nombres de columna a mostrar (mismo orden que df)
# - num_cols: columnas numéricas a formatear con 1 decimal y coma
# - append: TRUE agrega sección al archivo existente

fmt1 <- function(x) format(round(as.numeric(x), 1), nsmall = 1, decimal.mark = ",", big.mark = ".")

# Meses en español para los rangos (ene, feb, mar, abr, may, jun, jul, ago, sep, oct, nov, dic)
es_fmt <- function(d) {
  mm <- c("ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep", "oct", "nov", "dic")
  paste0(mm[as.integer(format(as.Date(d), "%m"))], format(as.Date(d), "-%y"))
}

write_md_table <- function(df, col_labels, num_cols = c(), title = "", source = "", file, append = FALSE) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  for (cc in num_cols) df[[cc]] <- fmt1(df[[cc]])
  hdr <- paste0(vapply(col_labels, function(h)
    sprintf('<th style="padding:8px 12px;text-align:left;">%s</th>', h), character(1)), collapse = "")
  rows <- vapply(seq_len(nrow(df)), function(i) {
    bg <- if (i %% 2 == 1) "#ffffff" else "#f5f1e8"
    tds <- vapply(names(df), function(cn) {
      align <- if (cn %in% num_cols) "right" else "left"
      sprintf('<td style="padding:7px 12px;text-align:%s;">%s</td>', align, df[i, cn])
    }, character(1))
    sprintf('<tr style="background-color:%s;">%s</tr>', bg, paste0(tds, collapse = ""))
  }, character(1))
  out <- c(
    if (!append) c("# IPI manufacturero — tablas", ""),
    sprintf("### %s", title), "",
    '<table style="border-collapse:collapse;font-family:Georgia,\'Times New Roman\',serif;font-size:14px;margin:1em 0;border:1px solid #c9c2b2;">',
    sprintf('<thead><tr style="background-color:#2f3e55;color:#ffffff;">%s</tr></thead>', hdr),
    "<tbody>", rows, "</tbody>", "</table>",
    sprintf("*%s*", source), ""
  )
  cat(paste0(out, collapse = "\n"), file = file, append = append)
  message("Tabla '", title, "' -> ", file)
}
