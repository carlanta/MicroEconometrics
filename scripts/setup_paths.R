# ============================================================
# setup_paths.R — Detección automática de rutas del proyecto
# Incluir al inicio de cada script: source("setup_paths.R")
# ============================================================
local({
  # Detectar dónde está este fichero (funciona con source() y Rscript)
  this_dir <- tryCatch(
    dirname(sys.frame(1)$ofile),
    error = function(e) {
      args <- commandArgs(trailingOnly = FALSE)
      m <- grep("--file=", args, value = TRUE)
      if (length(m)) dirname(sub("--file=", "", m[1])) else getwd()
    }
  )
  # Resolver a ruta absoluta
  this_dir <- normalizePath(this_dir, mustWork = FALSE)
  proj_dir <- normalizePath(file.path(this_dir, ".."), mustWork = FALSE)
  
  assign("DATA_DIR",   file.path(proj_dir, "data"),   envir = .GlobalEnv)
  assign("OUTPUT_DIR", file.path(proj_dir, "output"),  envir = .GlobalEnv)
  
  if (!dir.exists(.GlobalEnv$OUTPUT_DIR))
    dir.create(.GlobalEnv$OUTPUT_DIR, recursive = TRUE)
})
