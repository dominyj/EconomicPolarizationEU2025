custom_countrycode <- function(sourcevar, origin, destination, ...) {
  # Fall 1: AMECO als Quelle
  if(origin == "ameco") {
    # Erst ROM/rom zu ROU konvertieren
    result <- sourcevar
    result[toupper(result) == "ROM"] <- "ROU"
    # Dann normal von iso3c zum gewünschten Format konvertieren
    result <- countrycode::countrycode(result, "iso3c", destination, ...)
  }
  # Fall 2: AMECO als Ziel
  else if(destination == "ameco") {
    # Erst zum iso3c Format konvertieren
    result <- countrycode::countrycode(sourcevar, origin, "iso3c", ...)
    # Dann ROU zu ROM ändern
    result[result == "ROU"] <- "ROM"
  }
  # Fall 3: Weder AMECO als Quelle noch als Ziel
  else {
    result <- countrycode::countrycode(sourcevar, origin, destination, ...)
  }

  return(result)
}
