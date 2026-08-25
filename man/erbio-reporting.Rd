\name{erbio_render_workplace_report}
\alias{erbio_render_workplace_report}
\alias{erbio_write_workplace_report}
\alias{erbio_export_workplace_bundle}
\title{Render and Export Reproducible ERBio Workplace Reports}
\description{
Render a workplace assessment to Markdown or export the full reproducibility
bundle to files.
}
\usage{
erbio_render_workplace_report(x, max_plan_rows = 100L)
erbio_write_workplace_report(x, file = "ERBioR_workplace_report.md",
  max_plan_rows = 100L)
erbio_export_workplace_bundle(x, dir, overwrite = FALSE,
  max_plan_rows = 100L)
}
\arguments{
  \item{x}{An \code{erbio_workplace_assessment} object.}
  \item{max_plan_rows}{Maximum number of preventive-plan rows printed in the Markdown report. Exported CSV data retain all rows.}
  \item{file}{Output Markdown file.}
  \item{dir}{Directory for the reproducibility bundle.}
  \item{overwrite}{Whether existing bundle files may be replaced.}
}
\value{
\code{erbio_render_workplace_report()} returns a character string;
\code{erbio_write_workplace_report()} returns the normalized report path;
\code{erbio_export_workplace_bundle()} returns a named character vector of
written artifact paths.
}
\examples{
responses <- list(
  audit = rep("Si", 32),
  general = rep("Si", 48),
  workers = rep("Si", 34),
  sec_wastewater = rep("Si", 12)
)
x <- erbio_assess_workplace(
  "Tratamiento de aguas residuales",
  "Mycobacterium tuberculosis",
  responses,
  "Frecuente"
)
substr(erbio_render_workplace_report(x), 1, 120)
}
\keyword{methods}
