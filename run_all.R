# Orchestrate all IPI analysis scripts in dependency order (base R only)
# Full refresh workflow:
#   Rscript download_ipi.R   # 1) fetch latest CSVs (skips download if already current)
#   Rscript run_all.R         # 2) rebuild derived CSVs, plots and markdown tables
#
# Usage:
#   Rscript run_all.R                    # run everything, stop on first error
#   Rscript run_all.R --skip-sa          # skip ipi_grupos_sa.R (slow X-13 seasonal adj.)
#   Rscript run_all.R --continue-on-error# run all scripts even if one fails
#   Rscript run_all.R --dry-run          # just list what would run
#
# Each step runs in a separate Rscript process so one script's session state
# cannot leak into the next, and failures are reported per script.

args <- commandArgs(trailingOnly = TRUE)
skip_sa <- "--skip-sa" %in% args
continue_on_error <- "--continue-on-error" %in% args
dry_run <- "--dry-run" %in% args

# Work from the script's own directory so it runs from anywhere.
cmd_args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", cmd_args, value = TRUE)
if (length(file_arg) > 0) {
  script_dir <- dirname(normalizePath(sub("^--file=", "", file_arg[1])))
  setwd(script_dir)
}

# Order matters: general level first, then groups, then splits/averages,
# slow indirect-SA (X-13 over 16 divisions) last.
scripts <- c(
  "ipi_industrial.R",    # nivel general v1 (needs ipi-manufacturero.csv)
  "ipi_industrial_v2.R", # nivel general v2
  "ipi_grupos.R",        # grupos originales -> ipi-grupos.csv + 4 png + tabla
  "ipi_ministros.R",     # corte por ministro (Guzmán/Massa)
  "ipi_promedios.R",     # promedios por presidencia -> tabla nivel general
  "ipi_grupos_sa.R"      # grupos desest. método INDEC (slow) -> csv + 4 png + 2 tablas
)
if (skip_sa) scripts <- setdiff(scripts, "ipi_grupos_sa.R")

required_inputs <- c("ipi-manufacturero.csv", "ipi-manufacturero-sectores.csv", "md_helper.R")
missing <- required_inputs[!file.exists(required_inputs)]
if (length(missing) > 0 && !dry_run) {
  message("ERROR: missing input files: ", paste(missing, collapse = ", "))
  message("Run `Rscript download_ipi.R` first to fetch the data.")
  quit(status = 1, save = "no")
}

if (dry_run) {
  message("Would run (in order):")
  for (s in scripts) message("  - ", s)
  quit(status = 0, save = "no")
}

rscript <- file.path(R.home("bin"), "Rscript")
message("Working dir: ", normalizePath(getwd()))
message("Scripts to run (", length(scripts), "): ", paste(scripts, collapse = ", "))

failures <- c()
t0 <- Sys.time()
for (s in scripts) {
  message(sprintf("\n===== [%s] %s =====", format(Sys.time(), "%H:%M:%S"), s))
  if (!file.exists(s)) {
    message("ERROR: script not found: ", s)
    failures <- c(failures, s)
    if (!continue_on_error) break else next
  }
  rc <- system2(rscript, shQuote(s), stdout = "", stderr = "")
  if (!isTRUE(rc == 0)) {
    message(sprintf("FAILED: %s (exit code %s)", s, rc))
    failures <- c(failures, s)
    if (!continue_on_error) break
  } else {
    message(sprintf("OK: %s", s))
  }
}
message(sprintf("\nTotal time: %.1f min", difftime(Sys.time(), t0, units = "mins")))

if (length(failures) > 0) {
  message("FAILED scripts: ", paste(failures, collapse = ", "))
  quit(status = 1, save = "no")
}
message("All scripts completed successfully.")
