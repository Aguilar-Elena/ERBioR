#!/usr/bin/env python3

import csv
import json
from pathlib import Path
from collections import Counter, defaultdict


SOURCE = Path(
    "ErBioBench/protocol/pilot_run_manifest_smoke_v2.csv"
)

OUT_ELIGIBLE = Path(
    "ErBioBench/protocol/SMOKE_RETRY_ELIGIBLE_v1.csv"
)

OUT_REVIEW = Path(
    "ErBioBench/protocol/SMOKE_API_FAILURE_REVIEW_v1.csv"
)


def classify(meta):
    status = meta.get("execution_status")
    finish = meta.get("finish_reason")
    error = meta.get("error") or ""

    if status == "QUOTA_EXHAUSTED":
        return "QUOTA_EXHAUSTED"

    if (
        status == "API_FAILURE"
        and (
            "429" in error
            or "rate limit" in error.lower()
        )
    ):
        return "RATE_LIMIT_429"

    if status == "INVALID_JSON":
        if finish in (
            "length",
            "max_tokens",
            "incomplete",
        ):
            return "OUTPUT_TRUNCATION"

        return "INVALID_JSON_OTHER"

    if status == "API_FAILURE":
        return "API_FAILURE_OTHER"

    if status == "REFUSAL":
        return "REFUSAL"

    if (
        status == "COMPLETED"
        and meta.get("parse_valid_json") is True
    ):
        return "SCIENTIFIC_VALID"

    return f"OTHER_{status}_{finish}"


with SOURCE.open(
    newline="",
    encoding="utf-8-sig",
) as fh:
    rows = list(csv.DictReader(fh))

if len(rows) != 540:
    raise SystemExit(
        f"ERROR: expected 540 source rows, found {len(rows)}"
    )

eligible = []
review = []

overall = Counter()
by_model = defaultdict(Counter)
by_condition = defaultdict(Counter)

for row in rows:

    run_id = row["run_id"]

    meta_path = Path(
        row["metadata_file"]
    )

    if not meta_path.exists():
        category = "NEVER_ATTEMPTED"
        meta = {}
    else:
        meta = json.loads(
            meta_path.read_text()
        )
        category = classify(meta)

    overall[category] += 1
    by_model[row["model_slot"]][category] += 1
    by_condition[row["condition_id"]][category] += 1

    augmented = dict(row)

    augmented["current_category"] = category
    augmented["current_execution_status"] = (
        meta.get("execution_status", "")
    )
    augmented["current_finish_reason"] = (
        meta.get("finish_reason", "")
        or ""
    )
    augmented["current_error"] = (
        meta.get("error", "")
        or ""
    )

    if category in {
        "NEVER_ATTEMPTED",
        "QUOTA_EXHAUSTED",
        "RATE_LIMIT_429",
    }:

        augmented["retry_eligibility"] = "ELIGIBLE"

        augmented["retry_reason"] = {
            "NEVER_ATTEMPTED":
                "never_attempted",
            "QUOTA_EXHAUSTED":
                "external_provider_quota",
            "RATE_LIMIT_429":
                "external_provider_rate_limit",
        }[category]

        eligible.append(
            augmented
        )

    elif category == "API_FAILURE_OTHER":

        augmented["retry_eligibility"] = (
            "MANUAL_REVIEW_REQUIRED"
        )

        augmented["retry_reason"] = (
            "api_failure_other"
        )

        review.append(
            augmented
        )


# ------------------------------------------------------------
# Write manifests
# ------------------------------------------------------------

extra_fields = [
    "current_category",
    "current_execution_status",
    "current_finish_reason",
    "current_error",
    "retry_eligibility",
    "retry_reason",
]

fieldnames = list(rows[0].keys()) + extra_fields


with OUT_ELIGIBLE.open(
    "w",
    newline="",
    encoding="utf-8",
) as fh:

    writer = csv.DictWriter(
        fh,
        fieldnames=fieldnames,
    )

    writer.writeheader()
    writer.writerows(
        eligible
    )


with OUT_REVIEW.open(
    "w",
    newline="",
    encoding="utf-8",
) as fh:

    writer = csv.DictWriter(
        fh,
        fieldnames=fieldnames,
    )

    writer.writeheader()
    writer.writerows(
        review
    )


# ------------------------------------------------------------
# Audit
# ------------------------------------------------------------

print("========================================")
print("ERBIOBENCH SMOKE RETRY MANIFEST AUDIT")
print("========================================")

print("\nSOURCE RUNS:", len(rows))

print("\nCURRENT CATEGORIES")
for k, v in sorted(overall.items()):
    print(f"{k}: {v}")

print("\nRETRY ELIGIBLE:", len(eligible))

eligible_counts = Counter(
    r["current_category"]
    for r in eligible
)

for k, v in sorted(
    eligible_counts.items()
):
    print(f"  {k}: {v}")

print("\nELIGIBLE BY MODEL")

for model in sorted(
    {
        r["model_slot"]
        for r in eligible
    }
):
    z = [
        r
        for r in eligible
        if r["model_slot"] == model
    ]

    c = Counter(
        r["current_category"]
        for r in z
    )

    print(
        model,
        "| total =", len(z),
        "| never =", c["NEVER_ATTEMPTED"],
        "| quota =", c["QUOTA_EXHAUSTED"],
        "| rate429 =", c["RATE_LIMIT_429"],
    )

print("\nELIGIBLE BY CONDITION")

for cond in ["C1", "C2", "C3"]:

    z = [
        r
        for r in eligible
        if r["condition_id"] == cond
    ]

    c = Counter(
        r["current_category"]
        for r in z
    )

    print(
        cond,
        "| total =", len(z),
        "| never =", c["NEVER_ATTEMPTED"],
        "| quota =", c["QUOTA_EXHAUSTED"],
        "| rate429 =", c["RATE_LIMIT_429"],
    )

print("\nAPI_FAILURE_OTHER REVIEW:", len(review))

for r in review:
    print(
        r["run_id"],
        "| model =", r["model_slot"],
        "| condition =", r["condition_id"],
        "| finish =", r["current_finish_reason"],
        "| error =", r["current_error"][:160],
    )

# ------------------------------------------------------------
# Integrity checks
# ------------------------------------------------------------

assert len({
    r["run_id"]
    for r in eligible
}) == len(eligible)

assert len({
    r["run_id"]
    for r in review
}) == len(review)

assert not (
    {
        r["run_id"]
        for r in eligible
    }
    &
    {
        r["run_id"]
        for r in review
    }
)

assert all(
    r["current_category"]
    in {
        "NEVER_ATTEMPTED",
        "QUOTA_EXHAUSTED",
        "RATE_LIMIT_429",
    }
    for r in eligible
)

print("\nOUTPUTS")
print(OUT_ELIGIBLE)
print(OUT_REVIEW)

print("\nSMOKE_RETRY_MANIFEST_AUDIT_OK")
