\name{ERBioR-package}
\alias{ERBioR}
\alias{ERBioR-package}
\docType{package}
\title{ERBioR: Reproducible Occupational Biological Risk Assessment}
\description{
A source-traceable computational implementation of the ERBio method for
occupational biological-risk assessment.
}
\details{
ERBioR separates four layers: the original ERBio deterministic method,
versioned questionnaire instruments, regulatory biological-agent data, and
technical enrichment. Explicit computational extensions are labelled as such
and are not silently treated as source rules.

The underlying ERBio method is described by Aguilar-Elena (2015),
\doi{10.5281/zenodo.22107465}.
}
\references{
Aguilar-Elena, R. (2015). \emph{Riesgos biológicos laborales: ERBio, un nuevo
método de evaluación teórica}. Doctoral thesis, Universidad Pública de Navarra.
\doi{10.5281/zenodo.22107465}.

Orden TES/1287/2021, de 22 de noviembre, BOE-A-2021-19371, por la que se
sustituye el anexo II del Real Decreto 664/1997.

Directive 2000/54/EC of the European Parliament and of the Council, as updated
for Annex III by Commission Directive (EU) 2019/1833 and Commission Directive
(EU) 2020/739 for SARS-CoV-2.
}
\seealso{
\code{\link{erbio_assess}}, \code{\link{erbio_assess_questionnaire}},
\code{\link{erbio_reference_level_from_agent}},
\code{\link{erbio_assess_workplace}}
}
\keyword{package}
