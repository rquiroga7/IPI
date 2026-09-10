# Download latest IPI data (INDEC via SSPM) — with skip-if-current check
# Sources:
#   ipi-manufacturero.csv          <- 453.1 (nivel general: original, desest., tendencia-ciclo)
#   ipi-manufacturero-sectores.csv <- 453.2 (16 divisiones, indices originales)
# Usage:
#   Rscript download_ipi.R          # download only if remote changed (default)
#   Rscript download_ipi.R --force   # force re-download even if up to date
#
# How the "already updated" check works (two layers, base R only):
#   1) Fast path: HEADERS via curlGetHeaders(). If the remote ETag + Last-Modified
#      match the values stored in `.ipi_download_meta.rds` from the last successful
#      download AND the local file size is unchanged -> SKIP, no bytes downloaded.
#   2) Safe path: otherwise download to a temp file and compare md5 (tools::md5sum)
#      against the local file. If identical -> keep local file, just refresh metadata
#      (reports "up to date"). Only if md5 differs is the local file replaced.
# Running `Rscript download_ipi.R` + `Rscript run_all.R` in a row always refreshes
# data and plots, without wasteful re-downloads.

files <- list(
  list(
    local = "ipi-manufacturero.csv",
    url = "https://infra.datos.gob.ar/catalog/sspm/dataset/453/distribution/453.1/download/ipi-manufacturero.csv"
  ),
  list(
    local = "ipi-manufacturero-sectores.csv",
    url = "https://infra.datos.gob.ar/catalog/sspm/dataset/453/distribution/453.2/download/ipi-manufacturero-sectores.csv"
  )
)

meta_file <- ".ipi_download_meta.rds"
args <- commandArgs(trailingOnly = TRUE)
force <- "--force" %in% args

# Work from the script's own directory so it runs from anywhere.
cmd_args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", cmd_args, value = TRUE)
if (length(file_arg) > 0) {
  script_dir <- dirname(normalizePath(sub("^--file=", "", file_arg[1])))
  setwd(script_dir)
}

load_meta <- function() {
  if (file.exists(meta_file)) {
    tryCatch(readRDS(meta_file), error = function(e) list())
  } else {
    list()
  }
}

save_meta <- function(meta) saveRDS(meta, meta_file)

parse_headers <- function(hdrs) {
  out <- list(etag = NA_character_, last_modified = NA_character_, content_length = NA_integer_)
  if (is.null(hdrs) || length(hdrs) == 0) return(out)
  for (h in hdrs) {
    h <- trimws(gsub("[\r\n]+$", "", h))
    if (grepl("^ETag:", h, ignore.case = TRUE)) {
      out$etag <- trimws(sub("^ETag:", "", h, ignore.case = TRUE))
    } else if (grepl("^Last-Modified:", h, ignore.case = TRUE)) {
      out$last_modified <- trimws(sub("^Last-Modified:", "", h, ignore.case = TRUE))
    } else if (grepl("^Content-Length:", h, ignore.case = TRUE)) {
      out$content_length <- suppressWarnings(as.integer(trimws(sub("^Content-Length:", "", h, ignore.case = TRUE))))
    }
  }
  out
}

get_remote_meta <- function(url) {
  tryCatch(
    {
      hdrs <- curlGetHeaders(url)
      status <- suppressWarnings(as.integer(attr(hdrs, "status")))
      c(parse_headers(hdrs), list(status = status))
    },
    error = function(e) {
      message("  WARNING: could not fetch headers: ", conditionMessage(e))
      list(etag = NA_character_, last_modified = NA_character_, content_length = NA_integer_, status = NA_integer_)
    }
  )
}

max_fecha <- function(csv) {
  tryCatch(
    {
      df <- utils::read.csv(csv, nrows = 100000L, stringsAsFactors = FALSE)
      if (!"indice_tiempo" %in% names(df)) return(NA_character_)
      as.character(max(as.Date(df$indice_tiempo), na.rm = TRUE))
    },
    error = function(e) NA_character_
  )
}

meta <- load_meta()
n_updated <- 0L
n_skipped <- 0L
n_failed <- 0L

for (f in files) {
  local <- f$local
  url <- f$url
  message("=== ", local, " ===")

  remote <- get_remote_meta(url)
  prev <- meta[[local]]
  local_exists <- file.exists(local)
  local_size <- if (local_exists) as.numeric(file.size(local)) else NA_real_
  local_md5 <- if (local_exists) unname(tools::md5sum(local)) else NA_character_

  # Layer 1: skip without downloading if ETag + Last-Modified match last download.
  if (!force && local_exists && !is.null(prev) &&
      !is.na(remote$etag) && !is.na(prev$etag) &&
      !is.na(remote$last_modified) && !is.na(prev$last_modified) &&
      identical(remote$etag, prev$etag) &&
      identical(remote$last_modified, prev$last_modified) &&
      !is.na(local_size) && !is.na(prev$local_size) &&
      identical(as.numeric(local_size), as.numeric(prev$local_size))) {
    message("  SKIP: already up to date (ETag ", remote$etag, " match). No download.")
    message("  Local max indice_tiempo: ", max_fecha(local))
    n_skipped <- n_skipped + 1L
    next
  }

  # Layer 2: download to temp, replace only if content differs.
  tmp <- tempfile(pattern = paste0(tools::file_path_sans_ext(basename(local)), "_"), fileext = ".csv")
  ok <- tryCatch(
    {
      utils::download.file(url, destfile = tmp, mode = "wb", quiet = TRUE)
      TRUE
    },
    error = function(e) {
      message("  ERROR downloading: ", conditionMessage(e))
      FALSE
    }
  )
  if (!ok || !file.exists(tmp) || file.size(tmp) == 0) {
    message("  ERROR: download failed, keeping local file (if any).")
    if (file.exists(tmp)) try(unlink(tmp), silent = TRUE)
    n_failed <- n_failed + 1L
    next
  }

  new_md5 <- unname(tools::md5sum(tmp))
  new_size <- as.numeric(file.size(tmp))

  if (!force && local_exists && !is.na(local_md5) && identical(new_md5, local_md5)) {
    message("  SKIP: remote content identical (md5 ", new_md5, "). Local file untouched.")
    message("  Local max indice_tiempo: ", max_fecha(local))
    meta[[local]] <- list(
      url = url, etag = remote$etag, last_modified = remote$last_modified,
      content_length = remote$content_length, local_md5 = local_md5,
      local_size = local_size, downloaded_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
    )
    n_skipped <- n_skipped + 1L
    unlink(tmp)
  } else {
    if (!file.rename(tmp, local)) {
      # Cross-device fallback.
      file.copy(tmp, local, overwrite = TRUE)
      unlink(tmp)
    }
    if (force && local_exists && !is.na(local_md5) && identical(new_md5, local_md5)) {
      message("  FORCED re-download: content identical (md5 ", new_md5, "), file rewritten.")
    } else if (!local_exists) {
      message("  DOWNLOADED (new file): ", new_size, " bytes, md5 ", new_md5)
    } else {
      message("  UPDATED: ", local_size, " -> ", new_size, " bytes (md5 ", local_md5, " -> ", new_md5, ")")
    }
    message("  Local max indice_tiempo: ", max_fecha(local))
    meta[[local]] <- list(
      url = url, etag = remote$etag, last_modified = remote$last_modified,
      content_length = remote$content_length, local_md5 = new_md5,
      local_size = as.numeric(file.size(local)), downloaded_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
    )
    n_updated <- n_updated + 1L
  }
}

save_meta(meta)
message(sprintf("Done: %d updated, %d already up to date (skipped), %d failed.", n_updated, n_skipped, n_failed))
if (n_failed > 0) quit(status = 1, save = "no")
