# ERBioR workplace example
# Underlying method: Aguilar-Elena (2015), doi:10.5281/zenodo.22069658

library(ERBioR)

responses <- list(
  audit = rep("Si", 32),
  general = rep("Si", 48),
  workers = rep("Si", 34),
  sec_wastewater = rep("Si", 12)
)

assessment <- erbio_assess_workplace(
  activity = "Tratamiento de aguas residuales",
  agents = "Mycobacterium tuberculosis",
  questionnaire_responses = responses,
  exposure = "Frecuente"
)

print(assessment)
cat(erbio_render_workplace_report(assessment))
