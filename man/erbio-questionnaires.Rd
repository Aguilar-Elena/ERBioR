\name{erbio_score_questionnaire}
\alias{erbio_load_question_bank}
\alias{erbio_load_questionnaire_registry}
\alias{erbio_questionnaire_catalog}
\alias{erbio_get_questionnaire}
\alias{erbio_validate_question_bank}
\alias{erbio_score_questionnaire}
\alias{erbio_assess_questionnaire}
\alias{erbio_questionnaire_rules}
\title{ERBio Questionnaire Data and Assessment}
\description{
Access, validate, score and assess versioned ERBio questionnaires while
preserving item identifiers, source provenance and validation status.
}
\usage{
erbio_load_question_bank(path = NULL)
erbio_load_questionnaire_registry(path = NULL)
erbio_questionnaire_catalog(path = NULL)
erbio_get_questionnaire(questionnaire_id, version = NULL, path = NULL)
erbio_validate_question_bank(path = NULL)
erbio_score_questionnaire(questionnaire_id, responses, version = NULL,
  scoring = c("default", "binary", "audit_0_4"), path = NULL,
  language = erbio_get_language())
erbio_assess_questionnaire(questionnaire_id, responses, reference_level,
  exposure, version = NULL,
  scoring = c("default", "binary", "audit_0_4"), path = NULL,
  compliance_boundary_policy = c("prose_2015", "table45_intervals", "strict"),
  full_compliance_rule = c("flag_only", "thesis_override"))
erbio_questionnaire_rules(questionnaire_id = NULL)
}
\arguments{
  \item{path}{Optional directory or file override. If omitted, installed package data are used.}
  \item{questionnaire_id}{Stable ERBio questionnaire identifier such as \code{"general"} or \code{"sec_wastewater"}.}
  \item{version}{Optional source-version identifier.}
  \item{responses}{Response vector. Named responses may use stable item IDs or final item numbers.}
  \item{scoring}{Requested scoring semantics. \code{"default"} uses the instrument metadata.}
  \item{language}{Presentation language, \code{"es"} or \code{"en"}. This does not alter the scientific calculation.}
  \item{reference_level}{ERBio reference level.}
  \item{exposure}{ERBio exposure category.}
  \item{compliance_boundary_policy}{Policy used to classify compliance at source-boundary ambiguities.}
  \item{full_compliance_rule}{Policy for the complete-compliance source statement.}
}
\value{
Load/catalog functions return data frames. Validation returns a structured QA
list. \code{erbio_score_questionnaire()} returns an
\code{erbio_questionnaire_score}; \code{erbio_assess_questionnaire()} returns
an \code{erbio_questionnaire_assessment} including failed controls and source
warnings.
}
\details{
Not-applicable items are excluded from the compliance denominator. Core and
sector-specific questionnaires retain their documented validation status rather
than being labelled uniformly.
}
\examples{
cat <- erbio_questionnaire_catalog()
head(cat)

general <- erbio_get_questionnaire("general")
nrow(general)

score <- erbio_score_questionnaire("general", rep("Si", 48))
score

assessment <- erbio_assess_questionnaire(
  "general", rep("Si", 48), reference_level = 3, exposure = "Frecuente"
)
assessment
}
\references{
Aguilar-Elena, R. (2015). \emph{Riesgos biológicos laborales: ERBio, un nuevo
método de evaluación teórica}. \doi{10.5281/zenodo.22107465}.
}
\keyword{methods}
