
required <- c("shiny","DT","readxl","openxlsx")
missing <- required[!vapply(required,requireNamespace,logical(1),quietly=TRUE)]
if(length(missing)) install.packages(missing)
if(!requireNamespace("ERBioR",quietly=TRUE))
  stop("ERBioR no está instalado. Instale primero ERBioR 0.9.0.9000.",call.=FALSE)
cat("Dependencias disponibles.\n")
cat("ERBioR: ",as.character(utils::packageVersion("ERBioR")),"\n",sep="")
