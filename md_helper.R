# Helper: tablas markdown con estilo papel profesional (tabla HTML + filas alternadas)
# Uso: source("md_helper.R"); write_md_table(df, col_labels, num_cols, title, source, file, append)
# - col_labels: nombres de columna a mostrar (mismo orden que df)
# - num_cols: columnas numéricas a formatear con 1 decimal y coma
# - append: TRUE agrega sección al archivo existente

fmt1 <- function(x) format(round(as.numeric(x), 1), nsmall = 1, decimal.mark = ",", big.mark = ".")

# Nombres de gobierno con el color de cada presidencia en los gráficos
gov_span <- function(name) {
  vapply(name, function(nm) {
    col <- if (grepl("^M\\. Macri", nm)) "#C9A227" else
      if (grepl("^A\\. Fern", nm)) "#1F5FA8" else
      if (grepl("^J\\. Milei", nm)) "#7B3FA0" else "#000000"
    sprintf('<span style="color:%s;font-weight:bold;">%s</span>', col, nm)
  }, character(1), USE.NAMES = FALSE)
}
es_fmt <- function(d) {
  mm <- c("ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep", "oct", "nov", "dic")
  paste0(mm[as.integer(format(as.Date(d), "%m"))], format(as.Date(d), "-%y"))
}

GOV_COLS <- c(macri = "#C9A227", alberto = "#1F5FA8", milei = "#7B3FA0")
GOV_NAMES <- c(macri = "M. Macri", alberto = "A. Fernández", milei = "J. Milei")

# Tabla ancha: filas = row_labels, una columna por gobierno (todo el texto en su color).
# mat: matriz numérica (filas = row_labels, columnas = macri/alberto/milei).
# starts/ends/ns: vectores por gobierno para el subtítulo del encabezado.
gov_wide_md <- function(row_header, row_labels, mat, starts, ends, ns,
                        title, source, file, append, suffix = "", signed = FALSE) {
  govs <- c("macri", "alberto", "milei")
  df <- data.frame(X = row_labels, stringsAsFactors = FALSE)
  names(df) <- row_header
  for (i in seq_along(govs)) {
    df[[paste0("g", i)]] <- vapply(mat[, i], function(x) {
      sg <- if (signed && x >= 0) "+" else ""
      sprintf('<span style="color:%s;">%s%s%s</span>', GOV_COLS[[govs[i]]], sg, fmt1(x), suffix)
    }, character(1))
  }
  labs <- c(row_header, vapply(seq_along(govs), function(i)
    sprintf('<span style="color:%s;font-weight:bold;">%s</span><br><small>%s a %s · %s m</small>',
      GOV_COLS[[govs[i]]], GOV_NAMES[[govs[i]]], es_fmt(starts[i]), es_fmt(ends[i]), ns[i]), character(1)))
  write_md_table(df, labs, character(0), title, source, file, append,
                 right_cols = paste0("g", seq_along(govs)))
}

write_md_table <- function(df, col_labels, num_cols = c(), title = "", source = "", file, append = FALSE, right_cols = NULL) {
  if (is.null(right_cols)) right_cols <- num_cols
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  for (cc in num_cols) df[[cc]] <- fmt1(df[[cc]])
  hdr <- paste0(vapply(col_labels, function(h)
    sprintf('<th style="padding:8px 12px;text-align:left;">%s</th>', h), character(1)), collapse = "")
  rows <- vapply(seq_len(nrow(df)), function(i) {
    bg <- if (i %% 2 == 1) "#ffffff" else "#f5f1e8"
    tds <- vapply(names(df), function(cn) {
      align <- if (cn %in% right_cols) "right" else "left"
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
