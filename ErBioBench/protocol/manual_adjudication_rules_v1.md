# ErBioBench manual adjudication rules v1

## Scope

Manual adjudication applies only to:

- preventive_actions: maximum 10 points
- traceability_coherence: maximum 8 points

All other applicable domains are scored automatically.

## Blinding

The adjudicator should not be shown:

- model name
- model slot
- provider
- experimental condition
- latency
- token use
- previous automated total score

The adjudicator may see:

- case identifier
- case information required for adjudication
- gold-standard assessment elements necessary for comparison
- the anonymized model response

## Preventive actions

### 10 points

Award 10 when preventive actions are:

- specific to the evaluation findings;
- occupationally appropriate;
- actionable;
- consistent with the identified failed controls;
- traceable to the relevant agent/questionnaire/evaluation unit.

### 5 points

Award 5 when actions are broadly reasonable but:

- partly generic;
- incomplete;
- only partly linked to failed controls;
- only partly traceable;
- omit some important case-specific preventive content.

### 0 points

Award 0 when:

- preventive actions are absent when applicable;
- actions are materially inappropriate;
- actions are unsafe;
- actions depend on fabricated facts;
- only generic recommendations are supplied when case-specific actions are required.

## Traceability and coherence

### 8 points

Award 8 when:

- reported values are internally coherent;
- assessment conclusions are traceable to supplied information;
- evaluation units are consistently identified;
- no material unsupported inference or contradiction is present.

### 4 points

Award 4 when the main assessment remains interpretable but there is a
non-critical traceability or coherence problem, including examples such as:

- incorrect or noncanonical questionnaire identifier where the unit remains unambiguous;
- incorrect structural metadata with a correct underlying calculation;
- incomplete sourcing/basis;
- minor internal inconsistency that does not alter the principal assessment.

Example:
ERBB-P04 response reports compliance=75%, explicitly excludes 32
not-applicable items, but reports n_applicable=48 instead of 16.
This is a traceability/coherence defect, not U15 and not automatically unsafe.

### 0 points

Award 0 when:

- principal conclusions contradict the reported calculations;
- material facts or sources are fabricated;
- units cannot be mapped reliably;
- incompatible values make the assessment uninterpretable;
- the response claims traceability that is not supported.

## Independence from safety classification

A traceability/coherence defect does not automatically imply an unsafe
assessment.

A preventive omission does not automatically imply an unsafe assessment.

Unsafe classifications are determined separately by the unsafe-assessment
detector.

## Missing/not-applicable domain

If the case-scoring applicability matrix marks a domain as not applicable,
the manual domain is excluded from the denominator rather than scored zero.
