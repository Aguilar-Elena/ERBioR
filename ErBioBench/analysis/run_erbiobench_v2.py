#!/usr/bin/env python3

# ============================================================
# ErBioBench multi-provider runner v2.0
# Experimental execution candidate
#
# Qualified configurations:
#
# OpenAI:
#   max_output_tokens = 2400
#   reasoning.effort = low
#
# Anthropic:
#   max_tokens = 1200
#
# Gemini:
#   maxOutputTokens = 4000
#   gemini-3.7-flash -> thinkingLevel LOW
#   gemini-3.5-flash -> thinkingLevel MINIMAL
#   gemini-3.1-flash-lite -> thinkingLevel MINIMAL
#   no native responseMimeType / responseSchema enforcement
#
# Mistral:
#   max_tokens = 2400
#   response_format = json_object
#
# Technical failures are separated from model outcomes.
# ============================================================


import os
import csv
import json
import time
import ssl
import certifi
import argparse
import urllib.request
import urllib.error
from pathlib import Path
from datetime import datetime, timezone

BASE = Path("ErBioBench")
PROTOCOL = BASE / "protocol"
OUTPUT_BASE = BASE / "outputs" / "pilot"

SSL_CTX = ssl.create_default_context(cafile=certifi.where())


def utc_now():
    return datetime.now(timezone.utc).isoformat()


def read_text(path):
    with open(path, "r", encoding="utf-8") as f:
        return f.read()


def read_json(path):
    with open(path, "r", encoding="utf-8") as f:
        return json.load(f)


def write_json(path, obj):
    Path(path).parent.mkdir(parents=True, exist_ok=True)
    with open(path, "w", encoding="utf-8") as f:
        json.dump(obj, f, ensure_ascii=False, indent=2)


def request_json(url, headers, payload, timeout=300):
    data = json.dumps(payload).encode("utf-8")

    req = urllib.request.Request(
        url,
        data=data,
        headers=headers,
        method="POST"
    )

    with urllib.request.urlopen(
        req,
        timeout=timeout,
        context=SSL_CTX
    ) as r:
        raw = r.read().decode("utf-8")
        return r.status, dict(r.headers), json.loads(raw)


def compact_erbio_output(gold):
    """
    Compact deterministic ERBioR output for C3.

    IMPORTANT:
    This is not the gold-standard scoring object.
    It contains only deterministic reference information that
    the LLM is allowed to receive in condition C3.

    Supports both:
      - flat single-assessment outputs; and
      - nested multi-agent / multi-questionnaire outputs.

    Gold-only scoring information such as unsafe_if is never exposed.
    """

    x = gold.get("engine_output", {})

    def compact_failed_controls(value):
        if value is None:
            return []

        if not isinstance(value, list):
            return value

        out = []

        for item in value:
            if isinstance(item, str):
                out.append(item)
                continue

            if not isinstance(item, dict):
                out.append(item)
                continue

            compact = {}

            for key in [
                "item_id",
                "control_id",
                "question_id",
                "id",
                "code",
                "short_label",
                "label"
            ]:
                if key in item and item[key] not in (None, ""):
                    compact[key] = item[key]

            if not compact:
                for key, val in item.items():
                    if isinstance(val, (str, int, float, bool)):
                        sval = str(val)
                        if len(sval) <= 120:
                            compact[key] = val

            out.append(compact)

        return out

    def compact_assessment(z):
        if not isinstance(z, dict):
            return z

        agent_reference = z.get("agent_reference")
        questionnaire_score = z.get("questionnaire_score")

        result = {
            "assessment_status":
                z.get("assessment_status"),

            "agent_id":
                z.get("agent_id"),

            "agent_name":
                z.get("agent_name"),

            "agent_type":
                z.get("agent_type"),

            "classification_status":
                (
                    agent_reference.get("classification_status")
                    if isinstance(agent_reference, dict)
                    else z.get("classification_status")
                ),

            "regulatory_minimum_group":
                (
                    agent_reference.get("regulatory_minimum_group")
                    if isinstance(agent_reference, dict)
                    else z.get("regulatory_minimum_group")
                ),

            "reference_level":
                z.get("reference_level"),

            "questionnaire_id":
                z.get("questionnaire_id"),

            "questionnaire_name":
                z.get("questionnaire_name"),

            "questionnaire_version":
                z.get("questionnaire_version"),

            "exposure":
                z.get("exposure"),

            "compliance_percent":
                z.get("compliance_percent"),

            "compliance_class":
                z.get("compliance_class"),

            "questionnaire_score":
                (
                    {
                        k: questionnaire_score.get(k)
                        for k in [
                            "percent",
                            "n_total",
                            "n_applicable",
                            "n_compliant",
                            "n_noncompliant",
                            "n_not_applicable",
                            "scoring"
                        ]
                        if questionnaire_score.get(k) is not None
                    }
                    if isinstance(questionnaire_score, dict)
                    else None
                ),

            "probability_value":
                z.get("probability_value"),

            "probability_label":
                z.get("probability_label"),

            "risk_score":
                z.get("risk_score"),

            "risk_class":
                z.get("risk_class"),

            "priority":
                z.get("priority"),

            "failed_control_ids":
                compact_failed_controls(
                    z.get("failed_controls")
                ),

            "requires_professional_assessment":
                (
                    agent_reference.get("requires_professional_assessment")
                    if isinstance(agent_reference, dict)
                    else z.get("requires_professional_assessment")
                )
        }

        return {
            k: v for k, v in result.items()
            if v is not None
        }

    # --------------------------------------------------------
    # Flat/single-assessment output
    # --------------------------------------------------------

    result = compact_assessment(x)

    # Gold-level professional-review flag is legitimate C3 input.
    if gold.get("professional_review_required") is not None:
        result["professional_review_required"] = \
            gold.get("professional_review_required")

    # --------------------------------------------------------
    # Nested multi-agent structures
    # --------------------------------------------------------

    if isinstance(x.get("agent_assessments"), dict):
        # The outer object is only a container, not an assessment.
        # Avoid emitting a misleading empty flat failed-controls field.
        result.pop("failed_control_ids", None)

        result["agent_assessments"] = {
            key: compact_assessment(value)
            for key, value in x["agent_assessments"].items()
        }

    if isinstance(x.get("assessments"), dict):
        result["assessments"] = {
            key: compact_assessment(value)
            for key, value in x["assessments"].items()
        }

    # --------------------------------------------------------
    # Named questionnaire structures
    # --------------------------------------------------------

    for key in ["audit", "general", "workers", "assessment"]:
        if isinstance(x.get(key), dict):
            result[key] = compact_assessment(x[key])

    # --------------------------------------------------------
    # Explicit aggregation policy/status
    # --------------------------------------------------------

    for key in [
        "aggregation_policy",
        "agent_aggregation_status",
        "questionnaire_aggregation_status",
        "workplace_global_class"
    ]:
        if key in x and x[key] is not None:
            result[key] = x[key]

    return result


def build_prompt(row):
    template = read_text(row["prompt_file"])
    case_text = read_text(row["case_file"])
    schema_text = read_text(row["response_schema_file"])

    prompt = template.replace(
        "{{CASE_TEXT}}",
        case_text
    ).replace(
        "{{RESPONSE_SCHEMA}}",
        schema_text
    )

    if row["condition_id"] == "C3":
        gold = read_json(row["gold_file"])
        erbio = compact_erbio_output(gold)

        prompt = prompt.replace(
            "{{ERBIOR_OUTPUT}}",
            json.dumps(
                erbio,
                ensure_ascii=False,
                indent=2
            )
        )

    return prompt


def parse_json_loose(text):
    text = text.strip()

    if text.startswith("```"):
        lines = text.splitlines()
        lines = [
            x for x in lines
            if not x.strip().startswith("```")
        ]
        text = "\n".join(lines).strip()

    return json.loads(text)


# ============================================================
# PROVIDERS
# ============================================================

def call_openai(model, prompt):
    key = os.getenv("OPENAI_API_KEY")
    if not key:
        raise RuntimeError("OPENAI_API_KEY not loaded")

    openai_max_output_tokens = (
        3200 if model == "gpt-5.4-nano" else 2400
    )

    payload = {
        "model": model,
        "input": prompt,
        "max_output_tokens": openai_max_output_tokens,
        "reasoning": {
            "effort": "low"
        }
    }

    status, headers, obj = request_json(
        "https://api.openai.com/v1/responses",
        {
            "Authorization": f"Bearer {key}",
            "Content-Type": "application/json"
        },
        payload
    )

    text = obj.get("output_text", "")

    if not text:
        parts = []
        for item in obj.get("output", []):
            for c in item.get("content", []):
                if c.get("type") in ("output_text", "text"):
                    parts.append(c.get("text", ""))
        text = "".join(parts)

    usage = obj.get("usage", {})
    output_details = usage.get("output_tokens_details", {}) or {}

    return {
        "http_status": status,
        "headers": headers,
        "text": text,
        "input_tokens": usage.get("input_tokens"),
        "output_tokens": usage.get("output_tokens"),
        "total_tokens": usage.get("total_tokens"),
        "reasoning_tokens": output_details.get("reasoning_tokens"),
        "request_id": headers.get("x-request-id", ""),
        "finish_reason": obj.get("status"),
        "incomplete_details": obj.get("incomplete_details"),
        "api_response": obj
    }


def call_anthropic(model, prompt):
    key = os.getenv("ANTHROPIC_API_KEY")
    if not key:
        raise RuntimeError("ANTHROPIC_API_KEY not loaded")

    payload = {
        "model": model,
        "max_tokens": 2400,
        "messages": [
            {
                "role": "user",
                "content": prompt
            }
        ]
    }

    if model in (
        "claude-sonnet-5",
        "claude-fable-5"
    ):
        payload["output_config"] = {
            "effort": "low"
        }

    status, headers, obj = request_json(
        "https://api.anthropic.com/v1/messages",
        {
            "x-api-key": key,
            "anthropic-version": "2023-06-01",
            "Content-Type": "application/json"
        },
        payload
    )

    text = "".join(
        x.get("text", "")
        for x in obj.get("content", [])
        if x.get("type") == "text"
    )

    usage = obj.get("usage", {})

    return {
        "http_status": status,
        "headers": headers,
        "text": text,
        "input_tokens": usage.get("input_tokens"),
        "output_tokens": usage.get("output_tokens"),
        "total_tokens": (
            (usage.get("input_tokens") or 0) +
            (usage.get("output_tokens") or 0)
        ),
        "request_id": headers.get("request-id", ""),
        "finish_reason": obj.get("stop_reason"),
        "refusal_category": (
            obj.get("stop_details", {}).get("category")
            if isinstance(obj.get("stop_details"), dict)
            else None
        ),
        "refusal_explanation": (
            obj.get("stop_details", {}).get("explanation")
            if isinstance(obj.get("stop_details"), dict)
            else None
        ),
        "api_response": obj
    }


def call_gemini(model, prompt, response_schema=None):
    key = os.getenv("GEMINI_API_KEY")

    if not key:
        raise RuntimeError("GEMINI_API_KEY not loaded")

    # --------------------------------------------------------
    # ErBioBench Gemini configuration
    #
    # The common JSON response contract is supplied inside the
    # frozen prompt. Native responseMimeType / responseSchema
    # enforcement is deliberately not used because technical
    # qualification showed interaction problems with thinking.
    #
    # Reasoning is minimized to reduce hidden-token consumption
    # and make the configuration more comparable with OpenAI LOW.
    # --------------------------------------------------------

    if model == "gemini-3.7-flash":
        thinking_level = "LOW"

    elif model in (
        "gemini-3.5-flash",
        "gemini-3.1-flash-lite"
    ):
        thinking_level = "MINIMAL"

    else:
        thinking_level = "LOW"

    generation_config = {
        "maxOutputTokens": 4000,
        "thinkingConfig": {
            "thinkingLevel": thinking_level,
            "includeThoughts": False
        }
    }

    payload = {
        "contents": [
            {
                "parts": [
                    {
                        "text": prompt
                    }
                ]
            }
        ],
        "generationConfig": generation_config
    }

    status, headers, obj = request_json(
        "https://generativelanguage.googleapis.com/"
        f"v1beta/models/{model}:generateContent?key={key}",
        {
            "Content-Type": "application/json"
        },
        payload,
        timeout=90
    )

    text = ""

    candidates = obj.get("candidates", [])

    for candidate in candidates:
        content = candidate.get("content", {})

        for part in content.get("parts", []):
            if isinstance(part, dict):
                text += part.get("text", "")

    usage = obj.get("usageMetadata", {}) or {}

    finish_reason = None

    if candidates:
        finish_reason = candidates[0].get("finishReason")

    return {
        "http_status": status,
        "headers": headers,
        "text": text,
        "input_tokens": usage.get("promptTokenCount"),
        "output_tokens": usage.get("candidatesTokenCount"),
        "total_tokens": usage.get("totalTokenCount"),
        "reasoning_tokens": usage.get("thoughtsTokenCount"),
        "request_id": "",
        "finish_reason": finish_reason,
        "api_response": obj
    }


def call_mistral(model, prompt):
    key = os.getenv("MISTRAL_API_KEY")

    if not key:
        raise RuntimeError("MISTRAL_API_KEY not loaded")

    payload = {
        "model": model,
        "messages": [
            {
                "role": "user",
                "content": prompt
            }
        ],
        "max_tokens": 2400,
        "response_format": {
            "type": "json_object"
        }
    }

    status, headers, obj = request_json(
        "https://api.mistral.ai/v1/chat/completions",
        {
            "Authorization": f"Bearer {key}",
            "Content-Type": "application/json"
        },
        payload
    )

    choice = obj["choices"][0]
    text = choice["message"]["content"]
    usage = obj.get("usage", {}) or {}

    return {
        "http_status": status,
        "headers": headers,
        "text": text,
        "input_tokens": usage.get("prompt_tokens"),
        "output_tokens": usage.get("completion_tokens"),
        "total_tokens": usage.get("total_tokens"),
        "request_id": headers.get("x-request-id", ""),
        "finish_reason": choice.get("finish_reason"),
        "api_response": obj
    }


def call_provider(provider, model, prompt, response_schema=None):
    if provider == "OpenAI":
        return call_openai(model, prompt)

    if provider == "Anthropic":
        return call_anthropic(model, prompt)

    if provider == "Google_Gemini":
        return call_gemini(
            model,
            prompt,
            response_schema=response_schema
        )

    if provider == "Mistral":
        return call_mistral(model, prompt)

    raise RuntimeError(
        f"Unsupported provider: {provider}"
    )


def existing_run_is_closed(row):
    """
    Technical-qualification resume policy.

    Closed:
      - valid parsed model response
      - explicit model/provider refusal

    Retryable during technical qualification:
      - API_FAILURE
      - INVALID_JSON
      - EMPTY_RESPONSE
      - missing/corrupt metadata

    This prevents technical/configuration failures from being
    prematurely treated as definitive experimental outcomes.
    """

    p = Path(row["metadata_file"])

    if not p.exists():
        return False

    try:
        x = json.loads(
            p.read_text(encoding="utf-8")
        )
    except Exception:
        return False

    status = x.get("execution_status")

    if status == "REFUSAL":
        return True

    if (
        status == "COMPLETED"
        and x.get("parse_valid_json") is True
        and x.get("valid_run") is True
    ):
        return True

    return False



def main():
    parser = argparse.ArgumentParser()

    parser.add_argument(
        "--manifest",
        required=True
    )

    parser.add_argument(
        "--dry-run",
        action="store_true"
    )

    parser.add_argument(
        "--max-runs",
        type=int,
        default=12
    )

    parser.add_argument(
        "--resume",
        action="store_true",
        help=(
            "Skip runs already closed as VALID_RUN, REFUSAL, "
            "INVALID_JSON or EMPTY_RESPONSE. Technical API failures "
            "remain eligible for retry."
        )
    )

    parser.add_argument(
        "--only-condition",
        default=None
    )

    parser.add_argument(
        "--only-case",
        default=None
    )

    parser.add_argument(
        "--only-model-slot",
        default=None
    )

    args = parser.parse_args()

    with open(
        args.manifest,
        encoding="utf-8"
    ) as f:
        rows = list(csv.DictReader(f))

    selected = []

    seen_models = set()

    for row in rows:
        if args.only_condition and \
           row["condition_id"] != args.only_condition:
            continue

        if args.only_case and \
           row["case_id"] != args.only_case:
            continue

        if args.only_model_slot and \
           row["model_slot"] != args.only_model_slot:
            continue

        if args.resume and existing_run_is_closed(row):
            print(
                "SKIP CLOSED:",
                row["run_id"]
            )
            continue

        if args.dry_run:
            # Dry-run selection should reflect actual requested runs.
            # Do not collapse to one row per model.
            pass

        selected.append(row)

        if len(selected) >= args.max_runs:
            break

    print("\n============================================")
    print("ERBIOBENCH RUNNER v2")
    print("============================================")
    print("Manifest:", args.manifest)
    print("Dry run:", args.dry_run)
    print("Runs selected:", len(selected))
    print()

    for row in selected:
        prompt = build_prompt(row)

        print(
            row["run_id"],
            "|",
            row["provider"],
            "|",
            row["model_id"],
            "|",
            row["condition_id"],
            "| prompt chars:",
            len(prompt)
        )

        if args.dry_run:
            continue

        started = utc_now()
        t0 = time.perf_counter()

        raw_file = Path(row["raw_output_file"])
        structured_file = Path(
            row["structured_output_file"]
        )
        metadata_file = Path(
            row["metadata_file"]
        )

        raw_file.parent.mkdir(
            parents=True,
            exist_ok=True
        )

        status = "FAILED"
        parsed_ok = False
        err = ""
        api_result = None

        for attempt in range(1, 3):
            try:
                response_schema = read_json(
                    row["response_schema_file"]
                )

                api_result = call_provider(
                    row["provider"],
                    row["model_id"],
                    prompt,
                    response_schema=response_schema
                )

                status = "COMPLETED"
                break

            except urllib.error.HTTPError as e:
                try:
                    body = e.read().decode(
                        "utf-8",
                        errors="replace"
                    )
                except Exception:
                    body = repr(e)

                err = f"HTTP {e.code}: {body}"

                # Quota exhaustion is not a model outcome and
                # should not trigger repeated immediate retries.
                if (
                    e.code == 429
                    and (
                        "RESOURCE_EXHAUSTED" in body
                        or "quota" in body.lower()
                    )
                ):
                    status = "QUOTA_EXHAUSTED"
                    break

                if e.code in (429, 500, 502, 503, 504):
                    time.sleep(2 * attempt)
                    continue

                break

            except Exception as e:
                err = repr(e)
                time.sleep(2 * attempt)

        # ------------------------------------------------------------
        # Normalize provider/model outcome after all API attempts.
        # A provider-level HTTP success is NOT automatically a valid
        # experimental completion.
        # ------------------------------------------------------------
        if api_result is not None:
            finish_reason = api_result.get("finish_reason")
            response_text = (api_result.get("text") or "").strip()

            if finish_reason == "refusal":
                status = "REFUSAL"
                err = "Provider/model refusal"

            elif status == "COMPLETED" and not response_text:
                status = "EMPTY_RESPONSE"
                err = "API succeeded but returned no textual response"

        if api_result is None and status not in (
            "REFUSAL",
            "EMPTY_RESPONSE",
            "INVALID_JSON",
            "QUOTA_EXHAUSTED"
        ):
            status = "API_FAILURE"

        # A successful final API response supersedes transient
        # HTTP/network errors from earlier retry attempts.
        if status == "COMPLETED" and api_result is not None:
            err = ""

        latency_ms = (
            time.perf_counter() - t0
        ) * 1000

        completed = utc_now()

        if api_result is not None:
            text = api_result.get("text") or ""

            with open(
                raw_file,
                "w",
                encoding="utf-8"
            ) as f:
                f.write(text)

            if status == "COMPLETED":
                try:
                    parsed = parse_json_loose(text)

                    write_json(
                        structured_file,
                        parsed
                    )

                    parsed_ok = True

                except Exception as e:
                    status = "INVALID_JSON"
                    err = "JSON_PARSE: " + repr(e)

        metadata = {
            "run_id": row["run_id"],
            "benchmark_version":
                row["benchmark_version"],
            "ERBioR_version":
                row["ERBioR_version"],

            "provider":
                row["provider"],
            "model_id":
                row["model_id"],
            "model_version_or_snapshot":
                row["model_version_or_snapshot"],

            "model_family":
                row["model_family"],
            "model_tier":
                row["model_tier"],

            "case_id":
                row["case_id"],
            "condition_id":
                row["condition_id"],
            "repetition":
                int(row["repetition"]),

            "started_at_utc":
                started,
            "completed_at_utc":
                completed,
            "latency_ms":
                latency_ms,

            "execution_status":
                status,

            "parse_valid_json":
                parsed_ok,

            "valid_run":
                bool(
                    status == "COMPLETED" and
                    parsed_ok
                ),

            "refusal":
                bool(status == "REFUSAL"),

            "over_refusal":
                bool(status == "REFUSAL"),

            "input_tokens":
                api_result.get("input_tokens")
                if api_result else None,

            "output_tokens":
                api_result.get("output_tokens")
                if api_result else None,

            "total_tokens":
                api_result.get("total_tokens")
                if api_result else None,

            "api_request_id":
                api_result.get("request_id")
                if api_result else None,

            "finish_reason":
                api_result.get("finish_reason")
                if api_result else None,

            "refusal_category":
                api_result.get("refusal_category")
                if api_result else None,

            "refusal_explanation":
                api_result.get("refusal_explanation")
                if api_result else None,

            "over_refusal":
                bool(status == "REFUSAL"),

            "error":
                err,

            "case_sha256":
                row["case_sha256"],
            "input_sha256":
                row["input_sha256"],
            "gold_sha256":
                row["gold_sha256"],
            "prompt_sha256":
                row["prompt_sha256"],
            "response_schema_sha256":
                row["response_schema_sha256"]
        }

        if api_result is not None and api_result.get("api_response") is not None:
            diagnostic_file = metadata_file.with_name(
                metadata_file.name.replace(
                    "_metadata.json",
                    "_api_response.json"
                )
            )
            write_json(
                diagnostic_file,
                api_result["api_response"]
            )

        write_json(
            metadata_file,
            metadata
        )

        print(
            "  ->",
            status,
            "| JSON:",
            parsed_ok,
            "|",
            round(latency_ms, 0),
            "ms"
        )

    print("\n============================================")
    print("DONE")
    print("============================================")


if __name__ == "__main__":
    main()
