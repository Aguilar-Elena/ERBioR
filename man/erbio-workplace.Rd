\name{erbio_assess_workplace}
\alias{erbio_sector_questionnaires}
\alias{erbio_validate_workplace_questionnaires}
\alias{erbio_build_preventive_plan}
\alias{erbio_workplace_rules}
\alias{erbio_assess_workplace}
\title{Integral Workplace-Level ERBio Assessment}
\description{
Validate the required questionnaire set, evaluate one or more biological
agents, preserve questionnaire and agent-level results, and build preventive
planning candidates.
}
\usage{
erbio_sector_questionnaires(path = NULL)
erbio_validate_workplace_questionnaires(questionnaire_responses, path = NULL,
  require_core = TRUE, require_sector = TRUE)
erbio_build_preventive_plan(questionnaire_scores, risk_results,
  priority_policy = c("context_only", "max_observed_extension"))
erbio_workplace_rules(rule_id = NULL, path = NULL)
erbio_assess_workplace(activity, agents, questionnaire_responses, exposure,
  versions = NULL, scoring = NULL, path = NULL, require_core = TRUE,
  require_sector = TRUE,
  questionnaire_aggregation_rule = c("unresolved", "max_risk_extension"),
  workplace_aggregation_rule = c("unresolved", "max_risk_extension"),
  preventive_priority_policy = c("context_only", "max_observed_extension"),
  compliance_boundary_policy = c("prose_2015", "table45_intervals", "strict"),
  full_compliance_rule = c("flag_only", "thesis_override"))
}
\arguments{
  \item{path}{Optional data-directory override.}
  \item{questionnaire_responses}{Named list of questionnaire response vectors.}
  \item{require_core}{Require audit, general and workers questionnaires.}
  \item{require_sector}{Require at least one sector-specific questionnaire.}
  \item{questionnaire_scores}{Questionnaire score objects used to derive failed controls.}
  \item{risk_results}{Risk-result data used as planning context.}
  \item{priority_policy}{Preventive-priority policy. The max-observed option is an explicit extension.}
  \item{rule_id}{Optional workplace source-rule identifier.}
  \item{activity}{Activity or workplace description.}
  \item{agents}{One or more biological agents.}
  \item{exposure}{One exposure category or a named vector aligned to agents.}
  \item{versions}{Optional named questionnaire-version overrides.}
  \item{scoring}{Optional named scoring overrides.}
  \item{questionnaire_aggregation_rule}{Questionnaire-level aggregation policy.}
  \item{workplace_aggregation_rule}{Workplace-level aggregation policy.}
  \item{preventive_priority_policy}{Policy used to contextualize planning candidates.}
  \item{compliance_boundary_policy}{Compliance-boundary policy.}
  \item{full_compliance_rule}{Complete-compliance policy.}
}
\value{
\code{erbio_assess_workplace()} returns an \code{erbio_workplace_assessment}
containing agent summaries, questionnaire summaries, risk results, unresolved
items, warnings, provenance and a deduplicated preventive plan.
}
\details{
The default workplace policy preserves discordant questionnaire classes and
multi-agent results rather than manufacturing a single global class. The
highest observed risk class is reported separately as a descriptive field.
Responsible person, target date and resources in the preventive plan are not
inferred automatically.
}
\examples{
responses <- list(
  audit = rep("Si", 32),
  general = rep("Si", 48),
  workers = rep("Si", 34),
  sec_wastewater = rep("Si", 12)
)

x <- erbio_assess_workplace(
  activity = "Tratamiento de aguas residuales",
  agents = "Mycobacterium tuberculosis",
  questionnaire_responses = responses,
  exposure = "Frecuente"
)
x
}
\references{
Aguilar-Elena, R. (2015). \emph{Riesgos biológicos laborales: ERBio, un nuevo
método de evaluación teórica}. \doi{10.5281/zenodo.22107465}.
}
\keyword{methods}
