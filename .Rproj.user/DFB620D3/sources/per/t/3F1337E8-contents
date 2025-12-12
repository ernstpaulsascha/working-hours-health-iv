## ---- code/0-setup.R ---------------------------------------------------------
# --- Benötigte Libraries ---
libraries <- c("here", "data.table", "tidyverse", "haven", "fixest", 
               "modelsummary", "survey")

to_install <- libraries[!libraries %in% installed.packages()[, "Package"]]
if (length(to_install)) install.packages(to_install, dependencies = TRUE)
invisible(lapply(libraries, library, character.only = TRUE))

# --- Pfadeinrichtung --- 
DIR_DATA_RAW     <- here::here("data", "raw")
DIR_DATA_DERIVED <- here::here("data", "derived")
DIR_OUT_FIG      <- here::here("output", "figures")
DIR_OUT_TAB      <- here::here("output", "tables")

dirs <- c(DIR_DATA_RAW, DIR_DATA_DERIVED, DIR_OUT_FIG, DIR_OUT_TAB)
invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))

# --- Tabellen exportieren ---
write_tex_rows <- function(df, file_base, dir_out = DIR_OUT_TAB) {
  stopifnot(is.data.frame(df), ncol(df) >= 2)
  
  # Erzeuge Zeilen über alle Spalten
  tex_lines <- apply(df, 1, function(row) {
    paste(paste(row, collapse = " & "), "\\\\")})
  
  tex_path <- file.path(dir_out, paste0(file_base, ".tex"))
  readr::write_lines(tex_lines, tex_path)}