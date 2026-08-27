
source(file.path("R", "excel_io.R"))
erbio_require_app_packages()

library(shiny)
library(DT)

sector_df <- ERBioR::erbio_sector_questionnaires()
sector_choices <- stats::setNames(
  sector_df$questionnaire_id,
  sector_df$questionnaire_name
)

.erbio_display_status <- function(x, language = ERBioR::erbio_get_language()) {
  maps <- list(
    es = c(
      "matrix_assessment" = "Evaluación mediante matriz ERBio",
      "completed_with_documented_workers_group_extension" =
        "Evaluación completada con agregación grupal documentada de trabajadores"
    ),
    en = c(
      "matrix_assessment" = "Assessment using the ERBio matrix",
      "completed_with_documented_workers_group_extension" =
        "Assessment completed with documented group aggregation of workers"
    )
  )
  map <- maps[[language]]
  val <- as.character(x)[1]
  if (!is.na(val) && val %in% names(map)) unname(map[[val]]) else val
}

.erbio_print_quick_assessment <- function(x, language = ERBioR::erbio_get_language()) {
  if (identical(language, "en")) {
    cat("ERBioR — rapid assessment\n")
    cat("-------------------------\n")
    cat("Reference level:            ", x$reference_level, "\n", sep = "")
    cat("Exposure:                   ", x$exposure, "\n", sep = "")
    cat("Preventive compliance:      ", sprintf("%.2f%%", x$compliance_percent), " (", x$compliance_class, ")\n", sep = "")
    cat("Probability:                ", x$probability_value, " (", x$probability_label, ")\n", sep = "")
    cat("Risk score:                 ", x$risk_score, "\n", sep = "")
    cat("Risk class:                 ", x$risk_class, "\n", sep = "")
    cat("Priority:                   ", x$priority, "\n", sep = "")
    cat("Status:                     ", .erbio_display_status(x$assessment_status, language), "\n", sep = "")
  } else {
    cat("ERBioR — evaluación rápida\n")
    cat("--------------------------\n")
    cat("Nivel de referencia:         ", x$reference_level, "\n", sep = "")
    cat("Exposición:                   ", x$exposure, "\n", sep = "")
    cat("Cumplimiento preventivo:      ", sprintf("%.2f%%", x$compliance_percent), " (", x$compliance_class, ")\n", sep = "")
    cat("Probabilidad:                 ", x$probability_value, " (", x$probability_label, ")\n", sep = "")
    cat("Puntuación de riesgo:         ", x$risk_score, "\n", sep = "")
    cat("Clase de riesgo:              ", x$risk_class, "\n", sep = "")
    cat("Prioridad:                    ", x$priority, "\n", sep = "")
    cat("Estado:                       ", .erbio_display_status(x$assessment_status, language), "\n", sep = "")
  }
}

.app_sector_choices <- function(language = ERBioR::erbio_get_language()) {
  sector_df <- ERBioR::erbio_sector_questionnaires()
  labels <- sector_df$questionnaire_name
  if (identical(language, "en")) {
    tr <- ERBioR::erbio_load_translation_registry()
    em <- unique(tr[c("questionnaire_id", "questionnaire_name_en")])
    idx <- match(sector_df$questionnaire_id, em$questionnaire_id)
    ok <- !is.na(idx) & nzchar(em$questionnaire_name_en[idx])
    labels[ok] <- em$questionnaire_name_en[idx[ok]]
  }
  stats::setNames(sector_df$questionnaire_id, labels)
}

sector_choices <- .app_sector_choices("es")

.app_sector_label <- function(sector_id, language = "es") {
  choices <- .app_sector_choices(language)
  idx <- match(as.character(sector_id)[1], unname(choices))
  if (!is.na(idx)) names(choices)[idx] else as.character(sector_id)[1]
}

.app_activity_label <- function(payload, language = "es") {
  # For a known sector questionnaire, present the approved localized sector name.
  # The workbook activity remains scientific/input metadata and is not used to alter calculations.
  sid <- payload$sector_id
  lab <- .app_sector_label(sid, language)
  if (!is.na(lab) && nzchar(lab) && !identical(lab, sid)) lab else payload$activity
}

ui <- navbarPage(
  title = tagList(
    tags$img(
      src = "ERBioR.png",
      height = "34px",
      style = "margin-right:8px;"
    ),
    tags$span("ERBioR")
  ),
  id = "main_nav",
  header = tags$head(
    tags$style(HTML("
      body{background:#f4f6f8}
      .navbar{background:#17365D!important}
      .navbar-default .navbar-brand,
      .navbar-default .navbar-nav>li>a{color:white!important}
      .navbar-default .navbar-nav>.active>a{
        background:#2E75B6!important;color:white!important
      }
      .er-card{
        background:white;border:1px solid #dfe4ea;border-radius:10px;
        padding:18px;margin:12px 0;box-shadow:0 2px 8px rgba(0,0,0,.04)
      }
      .er-title{color:#17365D;font-weight:700}
      .er-kpi{
        background:#eef4fb;border-left:5px solid #2E75B6;
        padding:12px;margin:8px 0
      }
      .btn-primary{background:#2E75B6;border-color:#2E75B6}
      .btn-success{background:#38761D;border-color:#38761D}
      .alert-erbio{
        background:#fff8e1;border:1px solid #e2c66a;
        border-radius:6px;padding:12px
      }
      .er-help{font-size:13px;color:#5f6b78;margin-top:5px}
      .er-candidates{
        background:#f7f9fb;border:1px solid #dfe4ea;
        border-radius:6px;padding:10px;margin-top:12px
      }
      .er-summary-card{
        background:white;border:1px solid #dfe4ea;border-radius:10px;
        padding:16px;margin:8px 0;min-height:150px;
        box-shadow:0 2px 8px rgba(0,0,0,.04)
      }
      .er-summary-label{font-size:12px;color:#667085;text-transform:uppercase}
      .er-summary-value{font-size:22px;font-weight:700;color:#17365D;margin:4px 0}
      .er-note{background:#eef4fb;border-left:5px solid #2E75B6;padding:12px;margin:10px 0}
      .er-warning{background:#fff8e1;border-left:5px solid #c49a00;padding:12px;margin:10px 0}
    ")),
      tags$script(src = "i18n.js")
  ),

  tabPanel(
    "Inicio",
    fluidPage(
      div(
        class = "er-card",
        uiOutput("home_intro_ui")
      )
    )
  ),

  tabPanel(
    "Agente biológico",
    fluidPage(
      fluidRow(
        column(
          5,
          div(
            class = "er-card",
            h3(class = "er-title", "Buscar agente biológico"),
            textInput(
              "agent_query",
              "Nombre, género o parte del nombre",
              placeholder = "Ej.: Brucella o Brucella abortus"
            ),
            div(
              class = "er-help",
              "Un género muestra todas las coincidencias; ",
              "un nombre completo se resuelve directamente cuando existe."
            ),
            actionButton("agent_btn", "Buscar", class = "btn-primary"),
            uiOutput("agent_candidate_ui")
          )
        ),
        column(
          7,
          div(
            class = "er-card",
            h3(class = "er-title", "Resultado"),
            verbatimTextOutput("agent_result"),
            DTOutput("agent_matches")
          )
        )
      )
    )
  ),

  tabPanel(
    "Calculadora ERBio",
    fluidPage(
      fluidRow(
        column(
          4,
          div(
            class = "er-card",
            h3(class = "er-title", "Datos"),
            selectInput(
              "quick_nr", "Nivel de referencia",
              choices = 1:4, selected = 3
            ),
            selectInput(
              "quick_exposure", "Exposición",
              choices = c(
                "Ocasional", "Irregular", "Frecuente",
                "Muy frecuente", "Continua"
              ),
              selected = "Frecuente"
            ),
            numericInput(
              "quick_compliance",
              "Cumplimiento de controles preventivos aplicables (%)",
              75, min = 0, max = 100, step = .1
            ),
            div(
              class = "er-help",
              "Sí / (Sí + No) × 100; «No procede» se excluye. ",
              "En la evaluación completa se calcula automáticamente."
            ),
            actionButton("quick_btn", "Calcular", class = "btn-primary")
          )
        ),
        column(
          8,
          div(
            class = "er-card",
            h3(class = "er-title", "Resultado de la calculadora ERBio"),
            verbatimTextOutput("quick_result")
          )
        )
      )
    )
  ),

  tabPanel(
    "Evaluación completa",
    fluidPage(
      fluidRow(
        column(
          5,
          div(
            class = "er-card",
            h3(class = "er-title", "1. Descargar plantilla"),
            selectInput(
              "sector_id",
              "Cuestionario sectorial",
              choices = sector_choices
            ),
            numericInput(
              "n_workers",
              "Número de trabajadores que responderán",
              value = 1, min = 1, max = 1000, step = 1
            ),
            div(
              class = "er-help",
              "La hoja 05_TRABAJADORES contendrá 34 filas por trabajador ",
              "con un worker_id numérico anónimo."
            ),
            downloadButton(
              "download_template",
              "Descargar Excel",
              class = "btn-primary"
            ),
            tags$hr(),
            h3(class = "er-title", "2. Subir evaluación"),
            fileInput(
              "evaluation_xlsx",
              "Archivo .xlsx",
              accept = ".xlsx"
            ),
            actionButton(
              "process_excel",
              "Validar y evaluar",
              class = "btn-success"
            ),
            uiOutput("continue_evaluation_ui")
          )
        ),
        column(
          7,
          div(
            class = "er-card",
            h3(class = "er-title", "Estado"),
            verbatimTextOutput("excel_status"),
            uiOutput("excel_summary"),
            uiOutput("excel_agent_resolution_ui")
          )
        )
      )
    )
  ),

  tabPanel(
    "Resumen ejecutivo",
    fluidPage(
      uiOutput("summary_state"),
      uiOutput("executive_cards"),
      div(
        class = "er-card",
        uiOutput("executive_risk_intro_ui"),
        DTOutput("agent_risk_summary_table")
      ),
      div(
        class = "er-card",
        uiOutput("executive_questionnaire_heading_ui"),
        DTOutput("executive_questionnaire_table")
      ),
      div(
        class = "er-card",
        uiOutput("executive_review_heading_ui"),
        uiOutput("professional_review_summary")
      )
    )
  ),

  tabPanel(
    "Resultados",
    fluidPage(
      uiOutput("results_state"),
      uiOutput("results_kpis"),
      div(
        class = "er-card",
        uiOutput("results_agents_heading_ui"),
        DTOutput("agent_table")
      ),
      div(
        class = "er-card",
        uiOutput("results_questionnaires_heading_ui"),
        DTOutput("questionnaire_table")
      ),
      div(
        class = "er-card",
        uiOutput("results_risk_heading_ui"),
        DTOutput("risk_table")
      )
    )
  ),

  tabPanel(
    "Trabajadores",
    fluidPage(
      uiOutput("workers_state"),
      uiOutput("worker_kpis"),
      div(
        class = "er-card",
        uiOutput("workers_group_intro_ui"),
        DTOutput("worker_group_table")
      ),
      div(
        class = "er-card",
        uiOutput("workers_distribution_heading_ui"),
        DTOutput("worker_class_distribution_table")
      ),
      div(
        class = "er-card",
        uiOutput("workers_deficiencies_intro_ui"),
        plotOutput("worker_top_plot", height = "380px"),
        DTOutput("worker_top_table")
      ),
      div(
        class = "er-card",
        uiOutput("workers_individual_heading_ui"),
        DTOutput("worker_individual_table")
      ),
      div(
        class = "er-card",
        uiOutput("workers_item_heading_ui"),
        DTOutput("worker_item_table")
      )
    )
  ),

  tabPanel(
    "Planificación",
    fluidPage(
      uiOutput("plan_state"),
      uiOutput("planning_kpis"),
      div(
        class = "er-card",
        uiOutput("planning_intro_ui"),
        tabsetPanel(
          tabPanel("Acciones", DTOutput("plan_actions_table")),
          tabPanel("Análisis causal", DTOutput("plan_causes_table")),
          tabPanel(
            "Revisión profesional",
            uiOutput("planning_review_note_ui"),
            DTOutput("plan_review_table")
          )
        )
      )
    )
  ),

  tabPanel(
    "Trazabilidad y exportación",
    fluidPage(
      fluidRow(
        column(
          5,
          div(
            class = "er-card",
            h3(class = "er-title", "Exportar"),
            downloadButton(
              "download_report",
              "Informe profesional Markdown",
              class = "btn-primary"
            ),
            br(), br(),
            downloadButton(
              "download_bundle",
              "Bundle reproducible ZIP",
              class = "btn-success"
            )
          )
        ),
        column(
          7,
          div(
            class = "er-card",
            h3(class = "er-title", "Advertencias"),
            verbatimTextOutput("warnings_text")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {

  language <- reactiveVal("es")
  observeEvent(input$language, {
    lang <- if (identical(input$language, "en")) "en" else "es"
    language(lang)
    ERBioR::erbio_set_language(lang)
    if (!is.null(input$sector_id)) {
      updateSelectInput(session, "sector_id", choices = .app_sector_choices(lang), selected = input$sector_id)
    }
    session$sendCustomMessage("erbio_retranslate", lang)
  }, ignoreInit = FALSE)

  # C8: server-side i18n for all visible reactive/static content.
  .L <- function(es, en) if (identical(language(), "en")) en else es

  output$home_intro_ui <- renderUI({
    tagList(
      h2(class = "er-title", .L("Evaluación de riesgos biológicos con ERBioR", "Biological risk assessment with ERBioR")),
      p(.L(
        "La Shiny es la interfaz; los cálculos matemáticos se realizan mediante el paquete ERBioR.",
        "Shiny is the interface; mathematical calculations are performed by the ERBioR package."
      )),
      tags$ul(
        tags$li(.L("Consulta flexible de agentes biológicos.", "Flexible biological-agent lookup.")),
        tags$li(.L("Evaluación rápida del núcleo ERBio.", "Rapid ERBio core assessment.")),
        tags$li(.L("Evaluación integral mediante Excel.", "Comprehensive assessment using Excel.")),
        tags$li(.L("Encuesta personal de múltiples trabajadores.", "Personal survey for multiple workers.")),
        tags$li(.L("Planificación preventiva experta.", "Expert preventive planning.")),
        tags$li(.L("Informe y bundle reproducible.", "Report and reproducibility bundle."))
      ),
      div(class = "alert-erbio",
          strong(.L("Trabajadores: ", "Workers: ")),
          .L(
            "cada persona conserva un resultado individual. La vista grupal se etiqueta como una agregación computacional documentada.",
            "each person retains an individual result. The group view is explicitly labelled as a documented computational aggregation."
          ))
    )
  })

  output$executive_risk_intro_ui <- renderUI({
    tagList(
      h3(class = "er-title", .L("Mayor riesgo observado por agente", "Highest observed risk by agent")),
      div(class = "er-note",
          strong(.L("Criterio de interpretación: ", "Interpretation criterion: ")),
          .L(
            "se muestra el mayor riesgo observado entre los cuestionarios para cada agente. Es un descriptor de apoyo a la decisión profesional y no una agregación global.",
            "the highest observed risk across questionnaires is shown for each agent. This is a descriptor to support professional judgement, not a global aggregation."
          ))
    )
  })
  output$executive_questionnaire_heading_ui <- renderUI(h3(class="er-title", .L("Situación de los cuestionarios", "Questionnaire status")))
  output$executive_review_heading_ui <- renderUI(h3(class="er-title", .L("Avisos de revisión profesional", "Professional review notices")))

  output$results_agents_heading_ui <- renderUI(h3(class="er-title", .L("Agentes", "Agents")))
  output$results_questionnaires_heading_ui <- renderUI(h3(class="er-title", .L("Cuestionarios", "Questionnaires")))
  output$results_risk_heading_ui <- renderUI(h3(class="er-title", .L("Riesgo por agente y cuestionario", "Risk by agent and questionnaire")))

  output$workers_group_intro_ui <- renderUI(tagList(
    h3(class="er-title", .L("Resumen grupal", "Group summary")),
    p(.L(
      "Se conserva el resultado de cada trabajador y se muestra además la agregación grupal documentada.",
      "Each worker's result is retained and the documented group aggregation is also shown."
    ))
  ))
  output$workers_distribution_heading_ui <- renderUI(h3(class="er-title", .L("Distribución por clase de cumplimiento", "Distribution by compliance class")))
  output$workers_deficiencies_intro_ui <- renderUI(tagList(
    h3(class="er-title", .L("Principales deficiencias percibidas", "Main perceived deficiencies")),
    p(.L(
      "Ordenadas por porcentaje de respuestas «No» entre los trabajadores con respuesta aplicable.",
      "Ordered by percentage of No responses among workers with an applicable response."
    ))
  ))
  output$workers_individual_heading_ui <- renderUI(h3(class="er-title", .L("Resultados individuales", "Individual results")))
  output$workers_item_heading_ui <- renderUI(h3(class="er-title", .L("Respuesta grupal por ítem", "Group response by item")))

  output$planning_intro_ui <- renderUI(tagList(
    h3(class="er-title", .L("Planificación preventiva experta", "Expert preventive planning")),
    p(.L(
      "La salida separa la causa, la acción correctora, los requisitos de implantación y la verificación de eficacia. Responsable, plazo y recursos siguen requiriendo cumplimentación profesional.",
      "The output separates the cause, corrective action, implementation requirements and effectiveness verification. Responsible person, deadline and resources still require professional completion."
    ))
  ))
  output$planning_review_note_ui <- renderUI(div(
    class="er-note",
    .L(
      "Esta pestaña muestra dos avisos distintos: (1) preguntas condicionadas o históricas en las que debe confirmarse si realmente procedía responder «No» o «No procede»; y (2) medidas cuya redacción contiene fórmulas genéricas que conviene depurar. Ninguno de los dos avisos modifica el cálculo del riesgo ni constituye una nueva deficiencia.",
      "This tab shows two different notices: (1) conditional or historical questions for which it should be confirmed whether No or Not applicable was appropriate; and (2) measures whose wording contains generic formulas that should be refined. Neither notice changes the risk calculation or constitutes a new deficiency."
    )
  ))

  evaluated <- reactiveVal(NULL)
  validated_payload <- reactiveVal(NULL)
  excel_agent_resolution <- reactiveVal(NULL)
  process_message <- reactiveVal("Suba el Excel y pulse «Validar y evaluar».")
  observeEvent(language(), {
    if (is.null(validated_payload())) {
      process_message(if (identical(language(), "en"))
        "Upload the Excel file and click Validate and assess." else
        "Suba el Excel y pulse «Validar y evaluar».")
    }
  }, ignoreInit = FALSE)

  # Flexible agent lookup.
  agent_search <- reactiveVal(NULL)
  agent_exact <- reactiveVal(NULL)

  observeEvent(input$agent_btn, {
    query <- trimws(input$agent_query)
    req(nzchar(query))

    exact_hit <- tryCatch(
      ERBioR::erbio_agent_lookup(query, exact = TRUE),
      error = function(e) e
    )
    partial_hit <- tryCatch(
      ERBioR::erbio_agent_lookup(query, exact = FALSE),
      error = function(e) e
    )

    if (inherits(exact_hit, "error")) {
      agent_exact(exact_hit)
      agent_search(NULL)
    } else if (nrow(exact_hit) == 1L) {
      ref <- tryCatch(
        ERBioR::erbio_reference_level_from_agent(
          exact_hit$agent_name[[1]]
        ),
        error = function(e) e
      )
      agent_exact(ref)
      agent_search(exact_hit)
    } else {
      agent_exact(NULL)
      agent_search(partial_hit)
    }
  })

  output$agent_candidate_ui <- renderUI({
    hits <- agent_search()
    if (
      is.null(hits) ||
      inherits(hits, "error") ||
      nrow(hits) <= 1L
    ) return(NULL)

    choices <- stats::setNames(
      hits$agent_id,
      paste0(
        hits$agent_name,
        if (identical(language(), "en")) "  [Group " else "  [Grupo ", hits$risk_group, "; ", hits$agent_type, "]"
      )
    )

    div(
      class = "er-candidates",
      strong(if (identical(language(), "en")) "Several matches were found." else "Se han encontrado varias coincidencias."),
      p(if (identical(language(), "en")) "Select the specific agent:" else "Seleccione el agente concreto:"),
      selectInput(
        "agent_candidate",
        if (identical(language(), "en")) "Agent" else "Agente",
        choices = choices
      ),
      actionButton(
        "agent_candidate_btn",
        if (identical(language(), "en")) "Open selected agent" else "Consultar seleccionado",
        class = "btn-success"
      )
    )
  })

  observeEvent(input$agent_candidate_btn, {
    req(input$agent_candidate)
    hits <- agent_search()
    hit <- hits[
      hits$agent_id == input$agent_candidate,
      ,
      drop = FALSE
    ]
    req(nrow(hit) == 1L)
    agent_exact(
      ERBioR::erbio_reference_level_from_agent(
        hit$agent_name[[1]]
      )
    )
  })

  output$agent_result <- renderPrint({
    x <- agent_exact()
    hits <- agent_search()

    if (inherits(x, "error")) {
      cat(conditionMessage(x))
    } else if (!is.null(x)) {
      print(x)
    } else if (
      is.null(hits) ||
      inherits(hits, "error") ||
      !nrow(hits)
    ) {
      cat("No se han encontrado coincidencias en el registro ERBioR.")
    } else if (nrow(hits) > 1L) {
      cat(
        "Se han encontrado ", nrow(hits),
        " coincidencias. Seleccione una de la lista.",
        sep = ""
      )
    }
  })

  output$agent_matches <- renderDT({
    hits <- agent_search()
    if (
      is.null(hits) ||
      inherits(hits, "error") ||
      nrow(hits) <= 1L
    ) return(NULL)

    cols <- intersect(
      c(
        "agent_name", "agent_type", "risk_group",
        "regulatory_status", "agent_id"
      ),
      names(hits)
    )

    datatable(
      hits[, cols, drop = FALSE],
      rownames = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        searching = FALSE
      )
    )
  })

  observeEvent(input$quick_btn, {
    x <- tryCatch(
      ERBioR::erbio_assess(
        reference_level = as.integer(input$quick_nr),
        exposure = input$quick_exposure,
        compliance_percent = as.numeric(input$quick_compliance)
      ),
      error = function(e) e
    )
    output$quick_result <- renderPrint({
      if (inherits(x, "error")) {
        cat(conditionMessage(x))
      } else {
        .erbio_print_quick_assessment(x, language())
      }
    })
  })

  output$download_template <- downloadHandler(
    filename = function() {
      paste0(
        "ERBioR_Plantilla_",
        input$sector_id,
        "_", input$n_workers, "_trabajadores.xlsx"
      )
    },
    content = function(file) {
      erbio_make_excel_template(
        input$sector_id,
        file,
        n_workers = input$n_workers
      )
    }
  )

  .selected_agent_mapping <- function(payload, resolution) {
    selected <- stats::setNames(
      rep("", length(payload$agents)),
      payload$agents
    )

    for (i in seq_along(payload$agents)) {
      z <- resolution[[i]]
      input_agent <- payload$agents[[i]]

      if (identical(z$status, "exact")) {
        selected[[input_agent]] <- z$resolved_agent
      } else {
        input_id <- paste0("excel_agent_choice_", i)
        val <- input[[input_id]]
        if (!is.null(val)) {
          selected[[input_agent]] <- trimws(as.character(val))
        }
      }
    }

    selected
  }

  .run_validated_payload <- function(payload, resolution) {
    selected <- .selected_agent_mapping(payload, resolution)

    missing_selection <- names(selected)[!nzchar(selected)]
    if (length(missing_selection)) {
      return(structure(
        list(
          message = paste0(
            if (identical(language(), "en")) "Select the correct agent for: " else "Seleccione el agente correcto para: ",
            paste(missing_selection, collapse = ", "),
            "."
          )
        ),
        class = "erbio_app_pending_selection"
      ))
    }

    tryCatch(
      erbio_evaluate_payload(
        payload,
        selected_agents = selected,
        prepared_resolution = resolution,
        language = language()
      ),
      error = function(e) e
    )
  }

  observeEvent(input$process_excel, {
    req(input$evaluation_xlsx)

    evaluated(NULL)
    validated_payload(NULL)
    excel_agent_resolution(NULL)

    payload <- tryCatch(
      erbio_read_excel_evaluation(
        input$evaluation_xlsx$datapath
      ),
      error = function(e) e
    )

    if (inherits(payload, "error")) {
      process_message(
        paste0(
          if (identical(language(), "en")) "VALIDATION ERROR\n\n" else "ERROR DE VALIDACIÓN\n\n",
          conditionMessage(payload)
        )
      )
      return()
    }

    resolution <- tryCatch(
      erbio_prepare_agent_resolution(payload$agents),
      error = function(e) e
    )

    if (inherits(resolution, "error")) {
      process_message(
        paste0(
          if (identical(language(), "en")) "ERROR WHILE CHECKING AGENTS\n\n" else "ERROR AL COMPROBAR LOS AGENTES\n\n",
          conditionMessage(resolution)
        )
      )
      return()
    }

    validated_payload(payload)
    excel_agent_resolution(resolution)

    input_lang <- if (!is.null(payload$input_language)) payload$input_language else "unknown"
    output_lang <- language()
    language_notice <- if (identical(output_lang, "en")) {
      if (identical(input_lang, "es")) {
        "Input file detected: Spanish. Interface and output language: English. Responses have been normalized; results and preventive measures will be displayed in English."
      } else if (identical(input_lang, "en")) {
        "Input file detected: English. Interface and output language: English."
      } else {
        "Input file language could not be determined reliably. Stable item IDs and recognized response values will be used for import; output language is English."
      }
    } else {
      if (identical(input_lang, "en")) {
        "Archivo de entrada detectado: inglés. Idioma de interfaz y resultados: español. Las respuestas se han normalizado; los resultados y medidas preventivas se mostrarán en español."
      } else if (identical(input_lang, "es")) {
        "Archivo de entrada detectado: español. Idioma de interfaz y resultados: español."
      } else {
        "No se ha podido determinar con fiabilidad el idioma del archivo. La importación utilizará los item_id estables y las respuestas reconocidas; el idioma de salida es español."
      }
    }

    summary <- erbio_agent_resolution_summary(resolution)
    n_exact <- sum(summary$status == "exact")
    n_pending <- sum(summary$status != "exact")

    if (n_pending > 0L) {
      process_message(
        paste0(
          if (identical(language(), "en")) "EXCEL VALIDATED\n\n" else "EXCEL VALIDADO\n\n",
          language_notice, "\n\n",
          if (identical(language(), "en")) "Activity: " else "Actividad: ", .app_activity_label(payload, language()), "\n",
          if (identical(language(), "en")) "Sector: " else "Sector: ", payload$sector_id, "\n",
          if (identical(language(), "en")) "Workers: " else "Trabajadores: ",
          nrow(payload$workers$individual_summary), "\n",
          if (identical(language(), "en")) "Agents resolved exactly: " else "Agentes resueltos exactamente: ",
          n_exact, "/", nrow(summary), "\n\n",
          if (identical(language(), "en")) "Pending resolution: " else "Falta resolver ", n_pending,
          if (identical(language(), "en"))
            " agent(s). Select the correct name below and click Continue and calculate results." else
            " agente(s). Seleccione debajo la denominación correcta y pulse «Continuar y calcular resultados»."
        )
      )
      return()
    }

    result <- .run_validated_payload(payload, resolution)

    if (inherits(result, "error")) {
      process_message(
        paste0(
          if (identical(language(), "en")) "ASSESSMENT ERROR\n\n" else "ERROR EN LA EVALUACIÓN\n\n",
          conditionMessage(result)
        )
      )
      return()
    }

    evaluated(result)
    session$sendCustomMessage("erbio_retranslate", language())
    process_message(
      paste0(
        if (identical(language(), "en")) "ERBioR ASSESSMENT COMPLETED\n\n" else "EVALUACIÓN ERBioR COMPLETADA\n\n",
        language_notice, "\n\n",
        if (identical(language(), "en")) "Activity: " else "Actividad: ", .app_activity_label(result, language()), "\n",
        if (identical(language(), "en")) "Agents: " else "Agentes: ", nrow(result$agent_summary), "\n",
        if (identical(language(), "en")) "Workers: " else "Trabajadores: ",
        nrow(result$worker_individual_summary), "\n",
        if (identical(language(), "en")) "Worker group compliance: " else "Cumplimiento grupal trabajadores: ",
        sprintf(
          "%.2f%%",
          result$worker_group_summary$compliance_percent
        ),
        " (",
        result$worker_group_summary$compliance_class,
        ")\n",
        if (identical(language(), "en")) "Risk results: " else "Resultados de riesgo: ",
        nrow(result$risk_results), "\n",
        if (identical(language(), "en")) "Preventive measures: " else "Medidas preventivas: ",
        nrow(result$preventive_plan),
        "\n\n",
        if (identical(language(), "en")) "Calculation completed. Results, Workers and Preventive planning are now available." else "Cálculo finalizado. Se han habilitado Resultados, Trabajadores y Planificación."
      )
    )

    updateNavbarPage(
      session,
      "main_nav",
      selected = "Resumen ejecutivo"
    )
  })

  output$continue_evaluation_ui <- renderUI({
    payload <- validated_payload()
    resolution <- excel_agent_resolution()

    if (is.null(payload) || is.null(resolution)) return(NULL)

    summary <- erbio_agent_resolution_summary(resolution)
    if (!any(summary$status != "exact")) return(NULL)

    actionButton(
      "continue_evaluation",
      if (identical(language(), "en")) "Continue and calculate results" else "Continuar y calcular resultados",
      class = "btn-success",
      onclick = "this.disabled=true; this.classList.add('disabled'); this.textContent=(document.getElementById('erbio_language_select') && document.getElementById('erbio_language_select').value==='en') ? 'Processing…' : 'Procesando…';"
    )
  })

  output$excel_agent_resolution_ui <- renderUI({
    payload <- validated_payload()
    resolution <- excel_agent_resolution()

    if (is.null(payload) || is.null(resolution)) return(NULL)

    blocks <- lapply(seq_along(resolution), function(i) {
      z <- resolution[[i]]

      if (identical(z$status, "exact")) {
        return(
          div(
            class = "er-kpi",
            strong(z$input),
            tags$br(),
            tags$span(
              paste0(if (identical(language(), "en")) "Resolved as: " else "Resuelto como: ", z$resolved_agent)
            )
          )
        )
      }

      if (
        identical(z$status, "not_found") ||
        is.null(z$candidates) ||
        !nrow(z$candidates)
      ) {
        return(
          div(
            class = "alert-erbio",
            strong(z$input),
            tags$br(),
            if (identical(language(), "en")) "No candidates were found. " else "No se han encontrado candidatos. ",
            if (identical(language(), "en")) "Review the agent name in the Excel file." else "Revise la denominación en el Excel."
          )
        )
      }

      choices <- stats::setNames(
        c("", z$candidates$agent_name),
        c(
          if (identical(language(), "en")) "— Select the correct agent —" else "— Seleccione el agente correcto —",
          paste0(
            z$candidates$agent_name,
            if (identical(language(), "en")) " [Group " else " [Grupo ",
            z$candidates$risk_group,
            "]"
          )
        )
      )

      div(
        class = "er-candidates",
        strong(
          paste0(
            if (identical(language(), "en")) "Agent entered: " else "Agente introducido: ",
            z$input
          )
        ),
        p(
          if (identical(language(), "en")) "No exact match was found. " else "No existe coincidencia exacta. ",
          if (identical(language(), "en")) "Select one of the suggested matches:" else "Seleccione una de las coincidencias sugeridas:"
        ),
        selectInput(
          paste0("excel_agent_choice_", i),
          if (identical(language(), "en")) "ERBioR agent" else "Agente ERBioR",
          choices = choices,
          selected = ""
        )
      )
    })

    tagList(blocks)
  })

  observeEvent(input$continue_evaluation, {
    on.exit(session$sendCustomMessage("erbio_processing_done", language()), add = TRUE)
    payload <- validated_payload()
    resolution <- excel_agent_resolution()
    req(payload, resolution)

    result <- .run_validated_payload(
      payload,
      resolution
    )

    if (inherits(result, "erbio_app_pending_selection")) {
      process_message(
        paste0(
          if (identical(language(), "en")) "INCOMPLETE SELECTION\n\n" else "SELECCIÓN INCOMPLETA\n\n",
          result$message
        )
      )
      return()
    }

    if (inherits(result, "error")) {
      process_message(
        paste0(
          if (identical(language(), "en")) "ASSESSMENT ERROR\n\n" else "ERROR EN LA EVALUACIÓN\n\n",
          conditionMessage(result)
        )
      )
      return()
    }

    evaluated(result)
    session$sendCustomMessage("erbio_retranslate", language())
    process_message(
      paste0(
        if (identical(language(), "en")) "ERBioR ASSESSMENT COMPLETED\n\n" else "EVALUACIÓN ERBioR COMPLETADA\n\n",
        if (identical(language(), "en")) {
          paste0("Input language: ", if (identical(result$input_language, "es")) "Spanish" else if (identical(result$input_language, "en")) "English" else "undetermined", ". Output language: English.\n\n")
        } else {
          paste0("Idioma de entrada: ", if (identical(result$input_language, "es")) "español" else if (identical(result$input_language, "en")) "inglés" else "no determinado", ". Idioma de salida: español.\n\n")
        },
        if (identical(language(), "en")) "Activity: " else "Actividad: ", .app_activity_label(result, language()), "\n",
        if (identical(language(), "en")) "Agents: " else "Agentes: ", nrow(result$agent_summary), "\n",
        if (identical(language(), "en")) "Workers: " else "Trabajadores: ",
        nrow(result$worker_individual_summary), "\n",
        if (identical(language(), "en")) "Worker group compliance: " else "Cumplimiento grupal trabajadores: ",
        sprintf(
          "%.2f%%",
          result$worker_group_summary$compliance_percent
        ),
        " (",
        result$worker_group_summary$compliance_class,
        ")\n",
        if (identical(language(), "en")) "Risk results: " else "Resultados de riesgo: ",
        nrow(result$risk_results), "\n",
        if (identical(language(), "en")) "Preventive measures: " else "Medidas preventivas: ",
        nrow(result$preventive_plan),
        "\n\n",
        if (identical(language(), "en")) "Calculation completed. Results, Workers and Preventive planning are now available." else "Cálculo finalizado. Se han habilitado Resultados, Trabajadores y Planificación."
      )
    )

    updateNavbarPage(
      session,
      "main_nav",
      selected = "Resumen ejecutivo"
    )
  })

  output$excel_status <- renderPrint({
    cat(process_message())
  })

  output$excel_summary <- renderUI({
    payload <- validated_payload()
    if (is.null(payload)) return(NULL)

    sector_note <- if (
      !identical(payload$sector_id, input$sector_id)
    ) {
      div(
        class = "alert-erbio",
        strong(if (identical(language(), "en")) "Note: " else "Nota: "),
        if (identical(language(), "en"))
          "The sector currently selected on screen does not match the sector in the Excel file. The assessment uses the sector encoded in the uploaded file." else
          "El sector seleccionado actualmente en la pantalla no coincide con el sector del Excel. La evaluación utiliza el sector codificado dentro del archivo."
      )
    } else NULL

    tagList(
      sector_note,
      div(
        class = "er-kpi",
        strong(if (identical(language(), "en")) "Activity: " else "Actividad: "),
        .app_activity_label(payload, language())
      ),
      div(
        class = "er-kpi",
        strong(if (identical(language(), "en")) "Agents entered: " else "Agentes introducidos: "),
        paste(payload$agents, collapse = ", ")
      ),
      div(
        class = "er-kpi",
        strong(if (identical(language(), "en")) "Workers: " else "Trabajadores: "),
        nrow(payload$workers$individual_summary)
      )
    )
  })

  assessment <- reactive({
    x <- evaluated()
    req(!is.null(x))
    req(!inherits(x, "error"))
    x
  })

  output$results_state <- renderUI({
    x <- evaluated()
    if (!is.null(x) && !inherits(x, "error")) return(NULL)

    if (identical(language(), "en")) {
      div(class = "alert-erbio", strong("No results yet. "),
          "The assessment must be completed successfully in Full assessment. ",
          "Click Validate and assess; if an agent is ambiguous, select it and continue.")
    } else {
      div(class = "alert-erbio", strong("Sin resultados todavía. "),
          "La evaluación debe finalizar correctamente en la pestaña «Evaluación completa». ",
          "Pulse «Validar y evaluar»; si algún agente es ambiguo, selecciónelo y continúe.")
    }
  })

  output$workers_state <- renderUI({
    x <- evaluated()
    if (!is.null(x) && !inherits(x, "error")) return(NULL)

    if (identical(language(), "en")) {
      div(class = "alert-erbio", strong("No worker results yet. "),
          "They will be displayed when the full assessment has finished.")
    } else {
      div(class = "alert-erbio", strong("Sin resultados de trabajadores todavía. "),
          "Se mostrarán cuando la evaluación completa haya terminado.")
    }
  })

  output$plan_state <- renderUI({
    x <- evaluated()
    if (!is.null(x) && !inherits(x, "error")) return(NULL)

    if (identical(language(), "en")) {
      div(class = "alert-erbio", strong("No preventive planning yet. "),
          "Preventive measures are generated after completing the assessment.")
    } else {
      div(class = "alert-erbio", strong("Sin planificación todavía. "),
          "Las medidas preventivas se generan después de completar la evaluación.")
    }
  })

  output$summary_state <- renderUI({
    x <- evaluated()
    if (!is.null(x) && !inherits(x, "error")) return(NULL)
    if (identical(language(), "en")) {
      div(class = "er-warning", strong("Executive summary not available yet. "),
          "Complete the assessment to generate the executive interpretation.")
    } else {
      div(class = "er-warning", strong("Resumen no disponible todavía. "),
          "Complete la evaluación para generar la interpretación ejecutiva.")
    }
  })

  output$executive_cards <- renderUI({
    x <- evaluated()
    if (is.null(x) || inherits(x, "error")) return(NULL)

    cards <- lapply(seq_len(nrow(x$agent_risk_summary)), function(i) {
      z <- x$agent_risk_summary[i, , drop = FALSE]
      column(
        width = if (nrow(x$agent_risk_summary) <= 2) 6 else 4,
        div(
          class = "er-summary-card",
          div(class = "er-summary-label", if (identical(language(), "en")) "Agent" else "Agente"),
          div(class = "er-summary-value", z$agent_name[[1]]),
          p(strong(if (identical(language(), "en")) "Highest observed class: " else "Mayor clase observada: "), z$highest_observed_risk_class[[1]]),
          p(strong(if (identical(language(), "en")) "Priority: " else "Prioridad: "), z$highest_observed_priority[[1]]),
          p(strong(if (identical(language(), "en")) "Exposure: " else "Exposición: "), z$exposure[[1]]),
          p(strong(if (identical(language(), "en")) "Determining questionnaire(s): " else "Cuestionario(s) determinante(s): "), z$questionnaires_at_highest_observed[[1]])
        )
      )
    })
    do.call(fluidRow, cards)
  })

  output$agent_risk_summary_table <- renderDT({
    x <- assessment()
    datatable(.erbio_round_display(x$agent_risk_summary, 2L), rownames = FALSE,
              options = list(pageLength = 10, scrollX = TRUE))
  })

  output$executive_questionnaire_table <- renderDT({
    x <- assessment()
    datatable(.erbio_round_display(x$questionnaire_summary, 2L), rownames = FALSE,
              options = list(pageLength = 10, scrollX = TRUE))
  })

  output$professional_review_summary <- renderUI({
    x <- assessment()
    if (x$n_editorial_flags == 0L && x$n_conditional_flags == 0L) {
      return(div(class = "er-note",
                 strong(.L("Sin avisos adicionales. ", "No additional notices. ")),
                 .L(
                   "No se han detectado patrones genéricos de redacción ni ítems condicionados entre las deficiencias activas.",
                   "No generic wording patterns or conditional items were detected among the active deficiencies."
                 )))
    }
    tagList(
      if (x$n_conditional_flags > 0L) div(
        class = "er-warning",
        strong(paste0(x$n_conditional_flags, .L(" ítem(s) para revisar aplicabilidad. ", " item(s) requiring applicability review. "))),
        .L(
          "Son preguntas condicionadas, históricas o dependientes de una situación concreta que fueron respondidas «No». No son deficiencias adicionales: confirme si el supuesto era aplicable o si correspondía «No procede».",
          "These are conditional, historical or situation-dependent questions answered No. They are not additional deficiencies: confirm whether the condition was applicable or whether Not applicable should have been selected."
        )
      ) else NULL,
      if (x$n_editorial_flags > 0L) div(
        class = "er-warning",
        strong(paste0(x$n_editorial_flags, .L(" medida(s) para revisión de redacción. ", " measure(s) flagged for wording review. "))),
        .L(
          "La heurística ha encontrado fórmulas genéricas en el texto preventivo. No significa que existan nuevos riesgos ni que el cálculo sea incorrecto; sirve para depurar la redacción de la planificación.",
          "The heuristic found generic wording in preventive text. This does not indicate new risks or an incorrect calculation; it is a wording-quality flag for preventive planning."
        )
      ) else NULL
    )
  })


  output$results_kpis <- renderUI({
    x <- evaluated()
    if (is.null(x) || inherits(x, "error")) return(NULL)

    n_agents <- nrow(x$agent_summary)
    n_questionnaires <- length(unique(x$risk_results$questionnaire_id))

    max_score <- max(x$risk_results$risk_score, na.rm = TRUE)
    max_rows <- x$risk_results[
      x$risk_results$risk_score == max_score,
      ,
      drop = FALSE
    ]
    max_class <- paste(unique(max_rows$risk_class), collapse = " / ")

    fluidRow(
      column(
        3,
        div(
          class = "er-summary-card",
          div(class = "er-summary-label", if (identical(language(), "en")) "Agents assessed" else "Agentes evaluados"),
          div(class = "er-summary-value", n_agents)
        )
      ),
      column(
        3,
        div(
          class = "er-summary-card",
          div(class = "er-summary-label", if (identical(language(), "en")) "Questionnaires" else "Cuestionarios"),
          div(class = "er-summary-value", n_questionnaires),
          p(if (identical(language(), "en")) "Assessed for each agent." else "Evaluados por cada agente.")
        )
      ),
      column(
        3,
        div(
          class = "er-summary-card",
          div(class = "er-summary-label", if (identical(language(), "en")) "Highest observed risk" else "Mayor riesgo observado"),
          div(class = "er-summary-value", max_class),
          p(if (identical(language(), "en")) "Descriptor; not a global aggregation." else "Descriptor; no agregación global.")
        )
      ),
      column(
        3,
        div(
          class = "er-summary-card",
          div(class = "er-summary-label", if (identical(language(), "en")) "Immediate priority" else "Prioridad inmediata"),
          div(class = "er-summary-value", x$n_immediate_results),
          p(if (identical(language(), "en")) "Agent-questionnaire results." else "Resultados agente-cuestionario.")
        )
      )
    )
  })

  output$planning_kpis <- renderUI({
    x <- evaluated()
    if (is.null(x) || inherits(x, "error")) return(NULL)

    fluidRow(
      column(
        3,
        div(
          class = "er-summary-card",
          div(class = "er-summary-label", if (identical(language(), "en")) "Active measures" else "Medidas activas"),
          div(class = "er-summary-value", x$n_preventive_measures),
          p(if (identical(language(), "en")) "Derived from No responses." else "Derivadas de respuestas No.")
        )
      ),
      column(
        3,
        div(
          class = "er-summary-card",
          div(class = "er-summary-label", if (identical(language(), "en")) "Wording review" else "Revisión de redacción"),
          div(class = "er-summary-value", x$n_editorial_flags),
          p(if (identical(language(), "en")) "Editorial flag; not a scientific failure." else "Aviso editorial; no fallo científico.")
        )
      ),
      column(
        3,
        div(
          class = "er-summary-card",
          div(class = "er-summary-label", if (identical(language(), "en")) "Review applicability" else "Revisar aplicabilidad"),
          div(class = "er-summary-value", x$n_conditional_flags),
          p(if (identical(language(), "en")) "Conditional/historical items." else "Ítems condicionados/históricos.")
        )
      ),
      column(
        3,
        div(
          class = "er-summary-card",
          div(class = "er-summary-label", if (identical(language(), "en")) "Worker-derived measures" else "Medidas de trabajadores"),
          div(class = "er-summary-value", x$n_worker_measures),
          p(if (identical(language(), "en")) "Items with at least one No." else "Ítems con al menos un No.")
        )
      )
    )
  })

  output$worker_kpis <- renderUI({
    x <- evaluated()
    if (is.null(x) || inherits(x, "error")) return(NULL)
    wg <- x$worker_group_summary
    n_items_no <- sum(x$worker_item_summary$n_no > 0, na.rm = TRUE)
    fluidRow(
      column(3, div(class = "er-summary-card",
                    div(class = "er-summary-label", if (identical(language(), "en")) "Workers" else "Trabajadores"),
                    div(class = "er-summary-value", wg$n_workers[[1]]))),
      column(3, div(class = "er-summary-card",
                    div(class = "er-summary-label", if (identical(language(), "en")) "Group compliance" else "Cumplimiento grupal"),
                    div(class = "er-summary-value", sprintf("%.2f%%", wg$compliance_percent[[1]])),
                    p(wg$compliance_class[[1]]))),
      column(3, div(class = "er-summary-card",
                    div(class = "er-summary-label", if (identical(language(), "en")) "Individual mean" else "Media individual"),
                    div(class = "er-summary-value", sprintf("%.2f%%", wg$mean_individual_compliance[[1]])))),
      column(3, div(class = "er-summary-card",
                    div(class = "er-summary-label", if (identical(language(), "en")) "Items with at least one No" else "Ítems con algún No"),
                    div(class = "er-summary-value", n_items_no),
                    p(if (identical(language(), "en")) "The ranking shows up to 10." else "El ranking muestra hasta 10.")))
    )
  })

  output$worker_class_distribution_table <- renderDT({
    x <- assessment()
    datatable(.erbio_round_display(x$worker_class_distribution, 2L), rownames = FALSE,
              options = list(dom = "t", scrollX = TRUE))
  })

  output$worker_top_table <- renderDT({
    x <- assessment()
    datatable(.erbio_round_display(x$worker_top_deficiencies, 2L), rownames = FALSE,
              options = list(pageLength = 10, scrollX = TRUE))
  })

  output$worker_top_plot <- renderPlot({
    x <- assessment()
    z <- x$worker_top_deficiencies
    if (is.null(z) || !nrow(z)) {
      plot.new()
      text(0.5, 0.5, if (identical(language(), "en")) "There are no No responses in the worker questionnaire." else "No hay respuestas «No» en el cuestionario de trabajadores.")
      return(invisible())
    }
    z <- z[seq_len(min(10L, nrow(z))), , drop = FALSE]
    labels <- paste0(z$numero, ". ", z$item_id)
    op <- par(mar = c(5, 13, 4, 2) + 0.1)
    on.exit(par(op), add = TRUE)
    barplot(
      rev(z$pct_no_applicable),
      names.arg = rev(labels),
      horiz = TRUE,
      las = 1,
      xlab = if (identical(language(), "en")) "% No responses among applicable responses" else "% de respuestas No entre respuestas aplicables",
      main = if (identical(language(), "en")) "Main deficiencies perceived by workers" else "Principales deficiencias percibidas por trabajadores"
    )
  })

  output$plan_actions_table <- renderDT({
    x <- assessment(); p <- x$preventive_plan
    if (!nrow(p)) return(datatable(data.frame(Message = if (identical(language(), "en")) "No active preventive measures." else "Sin medidas preventivas activas."), rownames = FALSE))
    keep <- c("questionnaire_id", "item_id", "deficiency", "n_workers_affected", "pct_workers_no",
              "corrective_action", "implementation_requirements", "verification_criteria",
              "preventive_prescription_level", "conditional_item_flag")
    datatable(.erbio_round_display(p[, keep, drop = FALSE], 2L), rownames = FALSE,
              options = list(pageLength = 15, scrollX = TRUE))
  })

  output$plan_causes_table <- renderDT({
    x <- assessment(); p <- x$preventive_plan
    if (!nrow(p)) return(datatable(data.frame(Message = if (identical(language(), "en")) "No active preventive measures." else "Sin medidas preventivas activas."), rownames = FALSE))
    keep <- c("questionnaire_id", "item_id", "deficiency", "cause_assessment",
              "preventive_taxonomy", "preventive_prescription_level")
    datatable(p[, keep, drop = FALSE], rownames = FALSE,
              options = list(pageLength = 15, scrollX = TRUE))
  })

  output$plan_review_table <- renderDT({
    x <- assessment(); p <- x$preventive_plan
    if (!nrow(p)) return(datatable(data.frame(Message = if (identical(language(), "en")) "No active preventive measures." else "Sin medidas preventivas activas."), rownames = FALSE))
    z <- p[p$editorial_review_flag != "OK" | p$conditional_item_flag != "OK", , drop = FALSE]
    if (!nrow(z)) return(datatable(data.frame(Message = if (identical(language(), "en")) "No additional applicability or generic-wording notices." else "No hay avisos adicionales de aplicabilidad o redacción genérica."),
                                   rownames = FALSE, options = list(dom = "t")))
    keep <- c("questionnaire_id", "item_id", "deficiency", "conditional_item_flag",
              "editorial_review_flag", "editorial_review_reason")
    datatable(z[, keep, drop = FALSE], rownames = FALSE,
              options = list(pageLength = 15, scrollX = TRUE))
  })

  output$agent_table <- renderDT({
    datatable(
      assessment()$agent_summary,
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })

  output$questionnaire_table <- renderDT({
    datatable(
      assessment()$questionnaire_summary,
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })

  output$risk_table <- renderDT({
    datatable(
      assessment()$risk_results,
      rownames = FALSE,
      options = list(pageLength = 15, scrollX = TRUE)
    )
  })

  output$worker_group_table <- renderDT({
    datatable(
      assessment()$worker_group_summary,
      rownames = FALSE,
      options = list(dom = "t", scrollX = TRUE)
    )
  })

  output$worker_individual_table <- renderDT({
    datatable(
      assessment()$worker_individual_summary,
      rownames = FALSE,
      options = list(pageLength = 20, scrollX = TRUE)
    )
  })

  output$worker_item_table <- renderDT({
    datatable(
      assessment()$worker_item_summary,
      rownames = FALSE,
      options = list(pageLength = 34, scrollX = TRUE)
    )
  })

  output$warnings_text <- renderPrint({
    w <- assessment()$warnings
    if (!length(w)) {
      cat(if (identical(language(), "en")) "No additional warnings." else "Sin advertencias adicionales.")
    } else {
      cat(paste0("- ", unique(w), collapse = "\n"))
    }
  })

  output$download_report <- downloadHandler(
    filename = function() {
      paste0(
        "ERBioR_informe_",
        format(Sys.Date(), "%Y%m%d"),
        ".md"
      )
    },
    content = function(file) {
      writeLines(
        erbio_render_app_report(assessment()),
        file,
        useBytes = TRUE
      )
    }
  )

  output$download_bundle <- downloadHandler(
    filename = function() {
      paste0(
        "ERBioR_bundle_",
        format(Sys.Date(), "%Y%m%d"),
        ".zip"
      )
    },
    content = function(file) {
      td <- tempfile("erbio_bundle_")
      dir.create(td)
      erbio_export_app_bundle(assessment(), td)
      old <- setwd(td)
      on.exit(setwd(old), add = TRUE)
      utils::zip(
        file,
        files = list.files(
          ".",
          recursive = TRUE,
          all.files = FALSE
        )
      )
    }
  )
}

shinyApp(ui, server)
