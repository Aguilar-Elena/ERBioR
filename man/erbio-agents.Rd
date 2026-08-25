\name{erbio_agent_lookup}
\alias{erbio_load_agent_registry}
\alias{erbio_load_basebio_index}
\alias{erbio_agent_rules}
\alias{erbio_validate_agent_registry}
\alias{erbio_agent_lookup}
\alias{erbio_reference_level_from_agent}
\alias{erbio_unclassified_human_virus}
\alias{erbio_reference_level_from_agents}
\alias{erbio_assess_agent_questionnaire}
\title{Biological-Agent Registry and ERBio Reference Levels}
\description{
Query the regulatory biological-agent registry, inspect source rules and derive
ERBio reference levels without silently treating unlisted agents as group 1.
}
\usage{
erbio_load_agent_registry(path = NULL)
erbio_load_basebio_index(path = NULL)
erbio_agent_rules(rule_id = NULL, path = NULL)
erbio_validate_agent_registry(path = NULL)
erbio_agent_lookup(query, type = NULL, exact = TRUE, path = NULL)
erbio_reference_level_from_agent(agent, path = NULL, allow_partial = FALSE)
erbio_unclassified_human_virus(agent_name)
erbio_reference_level_from_agents(agents,
  policy = c("report_all", "explicit_max_group"), path = NULL)
erbio_assess_agent_questionnaire(agent, questionnaire_id, responses, exposure,
  version = NULL, scoring = NULL, path = NULL,
  compliance_boundary_policy = c("prose_2015", "table45_intervals", "strict"),
  full_compliance_rule = c("flag_only", "thesis_override"))
}
\arguments{
  \item{path}{Optional data-directory override.}
  \item{rule_id}{Optional machine-readable source-rule identifier.}
  \item{query}{Agent name, alias or stable identifier to search.}
  \item{type}{Optional agent type filter.}
  \item{exact}{Whether the lookup must be exact.}
  \item{agent}{Single biological-agent name or identifier.}
  \item{allow_partial}{Allow non-exact matching when resolving one agent.}
  \item{agent_name}{Name of an unclassified human virus.}
  \item{agents}{Character vector of biological agents.}
  \item{policy}{Multi-agent policy. \code{"report_all"} preserves discordant groups; \code{"explicit_max_group"} is an explicit conservative extension.}
  \item{questionnaire_id}{Questionnaire identifier used for the integrated assessment.}
  \item{responses}{Questionnaire responses.}
  \item{exposure}{ERBio exposure category.}
  \item{version}{Optional questionnaire version.}
  \item{scoring}{Optional scoring override.}
  \item{compliance_boundary_policy}{Compliance-boundary policy.}
  \item{full_compliance_rule}{Complete-compliance policy.}
}
\value{
Registry functions return data frames or QA lists. Reference-level functions
return structured agent-reference objects or multi-agent result lists.
\code{erbio_assess_agent_questionnaire()} returns an integrated agent and
questionnaire assessment.
}
\details{
The legal group is taken from the Spanish regulatory layer. In the v0.7 audited
data layer, the primary row source is Order TES/1287/2021 (BOE-A-2021-19371),
which replaced Annex II of Real Decreto 664/1997. The consolidated real decree
is retained as a secondary convenience reference. BASEBiO is retained as a
technical enrichment layer and does not override the legal group. Agents not
present in the classified list are not implicitly assigned to group 1.
Independent row-by-row EUR-Lex concordance is not claimed until the dedicated
EU audit is completed.
}
\examples{
erbio_agent_lookup("Bacillus anthracis")
erbio_reference_level_from_agent("Mycobacterium tuberculosis")
erbio_reference_level_from_agents(
  c("Bacillus anthracis", "Coxiella burnetii")
)
}
\references{
Orden TES/1287/2021, de 22 de noviembre, BOE-A-2021-19371, Annex II replacement.

Real Decreto 664/1997, de 12 de mayo, consolidated reference.

Commission Directive (EU) 2019/1833, Annex III update, and Commission Directive
(EU) 2020/739 for SARS-CoV-2.

INSST BASEBiO biological-agent database.
}
\keyword{methods}
