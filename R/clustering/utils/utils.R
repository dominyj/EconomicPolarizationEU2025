# Funktion zum Erstellen eines sinnvollen Dateinamens und Ordnerstruktur
create_filepath <- function(vars, config, base_path = "plots") {
  # Bestimme Hauptordner basierend auf geclusterten Typen
  type_folder <- paste(config$panel$coef_select, collapse = "_")

  # Cluster-Methoden Unterordner
  method_folder <- config$clustering$cluster_method

  # Erstelle kompletten Pfad
  full_path <- file.path(base_path, type_folder, method_folder)
  dir.create(full_path, recursive = TRUE, showWarnings = FALSE)

  # Erstelle Dateiname
  prefix <- sprintf("%02d", length(vars))
  var_part <- paste(substr(vars, 1, 3), collapse = "_")

  # Füge Clustering-Informationen hinzu
  method_part <- sprintf("%s_%s", config$clustering$dist_method, config$clustering$cluster_method)

  filename <- file.path(full_path,
                        paste0(prefix, "_", var_part, "_", method_part, ".pdf"))

  return(filename)
}
