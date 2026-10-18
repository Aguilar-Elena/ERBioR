#!/usr/bin/env python3

import csv
import json
import hashlib
from pathlib import Path
from collections import Counter, defaultdict
from datetime import datetime, timezone


MANIFEST = Path(
    "ErBioBench/protocol/pilot_run_manifest_smoke_v2.csv"
)

OUTDIR = Path("ErBioBench/protocol")
RESULTDIR = Path("ErBioBench/results/pilot_1_2")

CHECKPOINT = OUTDIR / "SMOKE_RECOVERY_CHECKPOINT_2026-10-18.csv"
SUMMARY = OUTDIR / "SMOKE_RECOVERY_CHECKPOINT_2026-10-18.txt"

G1_OUT = OUTDIR / "SMOKE_RECOVERY_G1_v1.csv"
G2_OUT = OUTDIR / "SMOKE_RECOVERY_G2_v1.csv"
M1_HOLD = OUTDIR / "SMOKE_HOLD_M1_PROVIDER_ACCESS_v1.csv"
M2_HOLD = OUTDIR / "SMOKE_HOLD_M2_RATE_LIMIT_v1.csv"

FAILURE_ARCHIVE = (
    RESULTDIR /
    "SMOKE_PRE_RETRY_FAILURE_PROVENANCE_2026-10-18.csv"
)


def sha256_file(path):
    h = hashlib.sha256()
    with path.open("rb") as f:
        for block in iter(lambda: f.read(1024 * 1024), b""):
            h.update(block)
    return h.hexdigest()


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

    if status == "API_FAILURE":
        if "503" in error:
            return "HTTP_503_TRANSIENT"
        if (
            "403" in error
            and "tier_not_allowed" in error
        ):
            return "TIER_NOT_ALLOWED"
        return "API_FAILURE_OTHER"

    if status == "INVALID_JSON":
        if finish in (
            "length",
            "max_tokens",
            "incomplete",
        ):
            return "OUTPUT_TRUNCATION"
        return "INVALID_JSON_OTHER"

    if status == "REFUSAL":
        return "REFUSAL"

    if (
        status == "COMPLETED"
        and meta.get("parse_valid_json") is True
    ):
        return "SCIENTIFIC_VALID"

    return f"OTHER_{status}_{finish}"


with MANIFEST.open(
    newline="",
    encoding="utf-8-sig",
) as f:
    rows = list(csv.DictReader(f))

assert len(rows) == 540
assert len({r["run_id"] for r in rows}) == 540

checkpoint = []
failure_provenance = []

overall = Counter()
by_model = defaultdict(Counter)

for r in rows:

    meta_path = Path(r["metadata_file"])

    if not meta_path.exists():
        meta = {}
        category = "NEVER_ATTEMPTED"
        meta_hash = ""
    else:
        meta = json.loads(meta_path.read_text())
        category = classify(meta)
        meta_hash = sha256_file(meta_path)

    overall[category] += 1
    by_model[r["model_slot"]][category] += 1

    z = dict(r)

    z.update({
        "current_category": category,
        "current_execution_status":
            meta.get("execution_status", ""),
        "current_finish_reason":
            meta.get("finish_reason") or "",
        "current_error":
            meta.get("error") or "",
        "metadata_sha256": meta_hash,
    })

    checkpoint.append(z)

    if category in {
        "QUOTA_EXHAUSTED",
        "RATE_LIMIT_429",
        "HTTP_503_TRANSIENT",
        "TIER_NOT_ALLOWED",
        "API_FAILURE_OTHER",
    }:
        failure_provenance.append(z)


extra = [
    "current_category",
    "current_execution_status",
    "current_finish_reason",
    "current_error",
    "metadata_sha256",
]

fields = list(rows[0].keys()) + extra


def write_csv(path, data):
    with path.open(
        "w",
        newline="",
        encoding="utf-8",
    ) as f:
        w = csv.DictWriter(
            f,
            fieldnames=fields,
        )
        w.writeheader()
        w.writerows(data)


write_csv(CHECKPOINT, checkpoint)

# The pre-retry failure provenance is an immutable baseline snapshot.
# Once created, later recovery runs must not overwrite it.
if not FAILURE_ARCHIVE.exists():
    write_csv(FAILURE_ARCHIVE, failure_provenance)


# ------------------------------------------------------------
# Future recovery batches
# ------------------------------------------------------------

g1 = [
    r for r in checkpoint
    if (
        r["model_slot"] == "G1"
        and r["current_category"] in {
            "NEVER_ATTEMPTED",
            "QUOTA_EXHAUSTED",
            "HTTP_503_TRANSIENT",
        }
    )
]

g2 = [
    r for r in checkpoint
    if (
        r["model_slot"] == "G2"
        and r["current_category"] in {
            "NEVER_ATTEMPTED",
            "QUOTA_EXHAUSTED",
        }
    )
]

m1 = [
    r for r in checkpoint
    if (
        r["model_slot"] == "M1"
        and r["current_category"] in {
            "NEVER_ATTEMPTED",
            "TIER_NOT_ALLOWED",
        }
    )
]

m2 = [
    r for r in checkpoint
    if (
        r["model_slot"] == "M2"
        and r["current_category"] ==
            "RATE_LIMIT_429"
    )
]

write_csv(G1_OUT, g1)
write_csv(G2_OUT, g2)
write_csv(M1_HOLD, m1)
write_csv(M2_HOLD, m2)


# ------------------------------------------------------------
# Integrity expectations
# ------------------------------------------------------------

# Structural integrity checks.
# Recovery counts are intentionally dynamic because successful
# recoveries reduce the pending batches over time.

assert len(g1) == sum(
    1 for r in checkpoint
    if r["model_slot"] == "G1"
    and r["current_category"] in {
        "NEVER_ATTEMPTED",
        "QUOTA_EXHAUSTED",
        "HTTP_503_TRANSIENT",
    }
)

assert len(g2) == sum(
    1 for r in checkpoint
    if r["model_slot"] == "G2"
    and r["current_category"] in {
        "NEVER_ATTEMPTED",
        "QUOTA_EXHAUSTED",
    }
)

assert len(m1) == sum(
    1 for r in checkpoint
    if r["model_slot"] == "M1"
    and r["current_category"] in {
        "NEVER_ATTEMPTED",
        "TIER_NOT_ALLOWED",
    }
)

assert len(m2) == sum(
    1 for r in checkpoint
    if r["model_slot"] == "M2"
    and r["current_category"] == "RATE_LIMIT_429"
)

assert sum(overall.values()) == 540
assert overall["REFUSAL"] == 59
assert overall["INVALID_JSON_OTHER"] == 13
assert overall["OUTPUT_TRUNCATION"] == 6


# ------------------------------------------------------------
# Human-readable checkpoint
# ------------------------------------------------------------

with SUMMARY.open("w", encoding="utf-8") as f:

    f.write(
        "ERBIOBENCH SMOKE RECOVERY CHECKPOINT\n"
    )
    f.write(
        "Date: 2026-10-18\n\n"
    )

    f.write(
        "Nominal smoke runs: 540\n"
    )

    for k, v in sorted(overall.items()):
        f.write(f"{k}: {v}\n")

    f.write("\nMODEL STATUS\n")

    for model in sorted(by_model):
        c = by_model[model]

        f.write(
            f"{model}: "
            f"valid={c['SCIENTIFIC_VALID']}; "
            f"refusal={c['REFUSAL']}; "
            f"quota={c['QUOTA_EXHAUSTED']}; "
            f"rate429={c['RATE_LIMIT_429']}; "
            f"http503={c['HTTP_503_TRANSIENT']}; "
            f"tier_not_allowed={c['TIER_NOT_ALLOWED']}; "
            f"invalid_json={c['INVALID_JSON_OTHER']}; "
            f"truncation={c['OUTPUT_TRUNCATION']}; "
            f"never={c['NEVER_ATTEMPTED']}\n"
        )

    f.write("\nFUTURE RECOVERY BATCHES\n")
    f.write(f"G1 actionable after quota reset: {len(g1)}\n")
    f.write(f"G2 actionable after quota reset: {len(g2)}\n")
    f.write(f"M1 provider-access hold: {len(m1)}\n")
    f.write(f"M2 rate-limit hold: {len(m2)}\n")

    f.write(
        "\nNo REFUSAL, INVALID_JSON_OTHER or "
        "OUTPUT_TRUNCATION run is eligible for retry.\n"
    )

    f.write(
        "Scientific-valid runs are closed.\n"
    )

    f.write(
        "Before future recovery execution, "
        "the provider must be checked with a single probe.\n"
    )


print("========================================")
print("SMOKE RECOVERY PREPARATION")
print("========================================")

print("Nominal:", len(rows))

for k, v in sorted(overall.items()):
    print(f"{k}: {v}")

print("\nBATCHES")
print("G1 recovery:", len(g1))
print("G2 recovery:", len(g2))
print("M1 hold:", len(m1))
print("M2 hold:", len(m2))

print("\nFailure provenance rows:",
      len(failure_provenance))

print("\nOUTPUTS")
for p in [
    CHECKPOINT,
    SUMMARY,
    G1_OUT,
    G2_OUT,
    M1_HOLD,
    M2_HOLD,
    FAILURE_ARCHIVE,
]:
    print(p)

print("\nSMOKE_RECOVERY_PREPARATION_OK")
