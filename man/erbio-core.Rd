\name{erbio_assess}
\alias{erbio_probability_matrix}
\alias{erbio_risk_matrix}
\alias{erbio_risk_actions}
\alias{erbio_exposure_from_cadence}
\alias{erbio_exposure_level}
\alias{erbio_compliance_binary}
\alias{erbio_compliance_audit_0_4}
\alias{erbio_compliance_class}
\alias{erbio_probability}
\alias{erbio_risk_score}
\alias{erbio_risk_class}
\alias{erbio_action}
\alias{erbio_failed_controls}
\alias{erbio_assess}
\alias{erbio_assess_set}
\title{Core ERBio Calculation Functions}
\description{
Functions implementing the deterministic ERBio calculation layer: exposure,
control compliance, probability, risk score (GRB), risk class and source-aware
assessment aggregation.
}
\usage{
erbio_probability_matrix()
erbio_risk_matrix()
erbio_risk_actions()
erbio_exposure_from_cadence(cadence)
erbio_exposure_level(exposure = NULL, time_percent = NULL,
  boundary_policy = c("strict", "upper_category", "lower_category"))
erbio_compliance_binary(responses)
erbio_compliance_audit_0_4(scores)
erbio_compliance_class(percent,
  boundary_policy = c("prose_2015", "table45_intervals", "strict"))
erbio_probability(compliance_class, exposure)
erbio_risk_score(probability, reference_level)
erbio_risk_class(score)
erbio_action(risk_class)
erbio_failed_controls(responses, item_text = NULL,
  scoring = c("binary", "audit_0_4"))
erbio_assess(reference_level, exposure, compliance_percent = NULL,
  responses = NULL, scoring = c("binary", "audit_0_4"), item_text = NULL,
  compliance_boundary_policy = c("prose_2015", "table45_intervals", "strict"),
  full_compliance_rule = c("flag_only", "thesis_override"))
erbio_assess_set(assessments,
  aggregation_rule = c("unresolved", "max_risk_extension"))
}
\arguments{
  \item{cadence}{Exposure cadence expressed using an ERBio cadence label.}
  \item{exposure}{Exposure category.}
  \item{time_percent}{Optional percentage of working time used to derive an exposure category.}
  \item{boundary_policy}{Explicit policy for source-boundary ambiguities.}
  \item{responses}{Questionnaire/control responses. Binary scoring accepts affirmative, negative and not-applicable responses.}
  \item{scores}{Audit scores on the source-preserved 0--4 scale.}
  \item{percent}{Compliance percentage.}
  \item{compliance_class}{ERBio compliance class.}
  \item{probability}{Probability level from 1 to 4.}
  \item{reference_level}{ERBio reference level from 1 to 4.}
  \item{score}{GRB risk score.}
  \item{risk_class}{ERBio risk class.}
  \item{item_text}{Optional item labels associated with responses.}
  \item{scoring}{Scoring semantics.}
  \item{compliance_percent}{Optional precomputed compliance percentage.}
  \item{compliance_boundary_policy}{Policy used to classify compliance at ambiguous boundaries.}
  \item{full_compliance_rule}{How the source statement concerning complete compliance is handled. The default flags rather than silently overrides the matrix.}
  \item{assessments}{A list of ERBio assessment objects.}
  \item{aggregation_rule}{Aggregation policy. The default preserves unresolved discordance; \code{"max_risk_extension"} is an explicit computational extension rather than a source rule.}
}
\value{
Matrix/reference functions return data frames or lookup values. Scoring functions
return numeric or structured results. \code{erbio_assess()} returns an
\code{erbio_assessment} object. \code{erbio_assess_set()} returns a structured
list that preserves source-defined and unresolved aggregation status.
}
\details{
ERBioR preserves documented ambiguities rather than silently resolving them. In
particular, the default aggregation policy does not invent a global risk class
when questionnaire-level classes disagree.
}
\examples{
erbio_probability("Aceptable", "Frecuente")
erbio_risk_score(probability = 2, reference_level = 3)
erbio_risk_class(6)

a <- erbio_assess(
  reference_level = 3,
  exposure = "Muy frecuente",
  compliance_percent = 82.5
)
a
}
\references{
Aguilar-Elena, R. (2015). \emph{Riesgos biológicos laborales: ERBio, un nuevo
método de evaluación teórica}. \doi{10.5281/zenodo.22107465}.
}
\keyword{methods}
