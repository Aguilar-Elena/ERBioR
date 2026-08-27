\name{erbio-v09-bilingual}
\alias{erbio_set_language}
\alias{erbio_get_language}
\alias{erbio_load_translation_registry}
\alias{erbio_validate_translation_registry}
\alias{erbio_load_preventive_action_registry}
\alias{erbio_validate_preventive_action_registry}
\alias{erbio_load_ui_dictionary}
\alias{erbio_translate_ui}
\alias{erbio_get_questionnaire_i18n}
\alias{erbio_bilingual_invariance_check}
\alias{erbio_preventive_action}
\alias{erbio_run_app}
\alias{erbio_shiny_build_id}

\title{ERBioR Bilingual, Preventive-Action and Shiny Utilities}

\description{
Utilities supporting the Spanish-English presentation layer introduced in
ERBioR 0.9, including language selection, approved scientific translations,
preventive-action text, questionnaire internationalisation and the bundled
Shiny application. Scientific calculations remain language-neutral.
}

\usage{
erbio_set_language(language = c("es", "en"))

erbio_get_language()

erbio_load_translation_registry(path = NULL)

erbio_validate_translation_registry(path = NULL)

erbio_load_preventive_action_registry(path = NULL)

erbio_validate_preventive_action_registry(path = NULL)

erbio_load_ui_dictionary(path = NULL)

erbio_translate_ui(key, language = erbio_get_language(), path = NULL)

erbio_get_questionnaire_i18n(
  questionnaire_id,
  language = erbio_get_language(),
  version = NULL,
  path = NULL
)

erbio_bilingual_invariance_check(
  questionnaire_id,
  responses,
  version = NULL,
  scoring = "default",
  path = NULL
)

erbio_preventive_action(
  item_id,
  language = erbio_get_language(),
  path = NULL
)

erbio_run_app(launch.browser = TRUE, ...)

erbio_shiny_build_id()
}

\arguments{
  \item{language}{
  Presentation language. Supported values are \code{"es"} for Spanish and
  \code{"en"} for English.
  }

  \item{path}{
  Optional directory or file override. If omitted, installed ERBioR package
  data are used.
  }

  \item{key}{
  Stable key identifying an entry in the ERBioR user-interface dictionary.
  }

  \item{questionnaire_id}{
  Stable ERBio questionnaire identifier.
  }

  \item{version}{
  Optional questionnaire source-version identifier.
  }

  \item{responses}{
  Questionnaire response vector used to compare scientific results across
  presentation languages.
  }

  \item{scoring}{
  Questionnaire scoring semantics. The default preserves the scoring defined
  for the corresponding ERBioR instrument.
  }

  \item{item_id}{
  Stable ERBioR questionnaire item identifier.
  }

  \item{launch.browser}{
  Logical value passed to the Shiny application launcher indicating whether a
  browser should be opened.
  }

  \item{...}{
  Additional arguments passed to the Shiny application launcher.
  }
}

\details{
The bilingual layer changes presentation text only. Questionnaire identifiers,
item identifiers, scoring rules, probability and risk calculations remain
language-neutral.

The translation and preventive-action registries are versioned package data.
Their validation functions report structural and approval-state checks without
changing the underlying ERBio scientific logic.

\code{erbio_bilingual_invariance_check()} is intended to verify that equivalent
Spanish and English presentations yield the same scientific result.

\code{erbio_run_app()} launches the Shiny application bundled with ERBioR.
}

\value{
\code{erbio_set_language()} sets the active presentation language.

\code{erbio_get_language()} returns the active language.

Registry loading functions return data frames. Registry validation functions
return structured validation results.

\code{erbio_translate_ui()} returns translated interface text.

\code{erbio_get_questionnaire_i18n()} returns a questionnaire with the selected
presentation language.

\code{erbio_bilingual_invariance_check()} returns the result of the
cross-language scientific invariance check.

\code{erbio_preventive_action()} returns the approved preventive-action
information associated with an ERBioR item.

\code{erbio_run_app()} launches the bundled Shiny application.

\code{erbio_shiny_build_id()} returns the identifier of the bundled Shiny build.
}

\examples{
old_lang <- erbio_get_language()
erbio_set_language("en")
erbio_get_language()
erbio_set_language(old_lang)

dict <- erbio_load_ui_dictionary()
head(dict)

tr <- erbio_load_translation_registry()
head(tr)
}

\references{
Aguilar-Elena, R. (2015). \emph{Riesgos biológicos laborales: ERBio, un nuevo
método de evaluación teórica}. \doi{10.5281/zenodo.22107465}.
}

\keyword{methods}
