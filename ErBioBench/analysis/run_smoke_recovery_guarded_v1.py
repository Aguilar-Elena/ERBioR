#!/usr/bin/env python3

import argparse
import csv
import json
import shutil
import subprocess
import tempfile
from datetime import datetime, timezone
from pathlib import Path


ROOT = Path(".")
RUNNER = Path(
    "ErBioBench/analysis/run_erbiobench_v2.py"
)

ARCHIVE_ROOT = Path(
    "ErBioBench/results/pilot_1_2/recovery_attempts"
)

ALLOWED_MANIFESTS = {
    "G1": Path(
        "ErBioBench/protocol/SMOKE_RECOVERY_G1_v1.csv"
    ),
    "G2": Path(
        "ErBioBench/protocol/SMOKE_RECOVERY_G2_v1.csv"
    ),
    "M1": Path(
        "ErBioBench/protocol/SMOKE_HOLD_M1_PROVIDER_ACCESS_v1.csv"
    ),
    "M2": Path(
        "ErBioBench/protocol/SMOKE_HOLD_M2_RATE_LIMIT_v1.csv"
    ),
}


def read_csv(path):
    with path.open(
        newline="",
        encoding="utf-8-sig",
    ) as f:
        return list(csv.DictReader(f))


def read_meta(row):
    p = Path(row["metadata_file"])

    if not p.exists():
        return None

    try:
        return json.loads(p.read_text())
    except Exception:
        return None


def current_state(row):
    meta = read_meta(row)

    if meta is None:
        return "NEVER_ATTEMPTED"

    status = meta.get("execution_status")
    error = (meta.get("error") or "").lower()
    finish = meta.get("finish_reason")

    if (
        status == "COMPLETED"
        and meta.get("parse_valid_json") is True
    ):
        return "CLOSED_COMPLETED"

    if status == "REFUSAL":
        return "CLOSED_REFUSAL"

    if status == "INVALID_JSON":
        return "CLOSED_INVALID_JSON"

    if status == "QUOTA_EXHAUSTED":
        return "OPEN_QUOTA_EXHAUSTED"

    if status == "API_FAILURE":

        if (
            "429" in error
            or "rate limit" in error
        ):
            return "OPEN_RATE_LIMIT_429"

        if (
            "403" in error
            or "tier_not_allowed" in error
        ):
            return "OPEN_TIER_NOT_ALLOWED"

        if "503" in error:
            return "OPEN_HTTP_503"

        return "OPEN_API_FAILURE_OTHER"

    return f"OTHER_{status}_{finish}"


def is_open_for_recovery(row, model_slot):
    state = current_state(row)

    if model_slot == "G1":
        return state in {
            "NEVER_ATTEMPTED",
            "OPEN_QUOTA_EXHAUSTED",
            "OPEN_HTTP_503",
        }

    if model_slot == "G2":
        return state in {
            "NEVER_ATTEMPTED",
            "OPEN_QUOTA_EXHAUSTED",
        }

    if model_slot == "M1":
        return state in {
            "NEVER_ATTEMPTED",
            "OPEN_TIER_NOT_ALLOWED",
        }

    if model_slot == "M2":
        return state in {
            "OPEN_RATE_LIMIT_429",
        }

    return False


def provider_block(meta):
    if meta is None:
        return None

    status = meta.get("execution_status")
    error = (meta.get("error") or "").lower()

    if status == "QUOTA_EXHAUSTED":
        return "QUOTA_EXHAUSTED"

    if status == "API_FAILURE":

        if (
            "429" in error
            or "rate limit" in error
        ):
            return "RATE_LIMIT_429"

        if (
            "403" in error
            or "tier_not_allowed" in error
        ):
            return "TIER_NOT_ALLOWED"

        if "503" in error:
            return "HTTP_503"

    return None


def next_attempt_number(run_id):
    d = ARCHIVE_ROOT / run_id

    if not d.exists():
        return 1

    nums = []

    for p in d.glob("attempt_*"):
        try:
            nums.append(
                int(p.name.split("_")[1])
            )
        except Exception:
            pass

    return max(nums, default=0) + 1


def archive_existing(row, attempt_no):
    run_id = row["run_id"]

    d = (
        ARCHIVE_ROOT
        / run_id
        / f"attempt_{attempt_no:02d}"
    )

    d.mkdir(
        parents=True,
        exist_ok=False,
    )

    candidates = [
        row.get("metadata_file"),
        row.get("raw_output_file"),
        row.get("structured_output_file"),
    ]

    structured = row.get(
        "structured_output_file",
        ""
    )

    if structured:
        sp = Path(structured)

        candidates.append(
            str(
                sp.with_name(
                    sp.stem
                    + "_api_response.json"
                )
            )
        )

    copied = []

    for x in candidates:
        if not x:
            continue

        p = Path(x)

        if p.exists():
            dest = d / (
                "pre_" + p.name
            )
            shutil.copy2(p, dest)
            copied.append(str(dest))

    manifest_info = {
        "run_id": run_id,
        "attempt_no": attempt_no,
        "archived_at_utc":
            datetime.now(
                timezone.utc
            ).isoformat(),
        "pre_files": copied,
        "pre_state":
            current_state(row),
    }

    (
        d / "attempt_manifest.json"
    ).write_text(
        json.dumps(
            manifest_info,
            indent=2,
            ensure_ascii=False,
        )
    )

    return d


def archive_post(row, attempt_dir):
    candidates = [
        row.get("metadata_file"),
        row.get("raw_output_file"),
        row.get("structured_output_file"),
    ]

    structured = row.get(
        "structured_output_file",
        ""
    )

    if structured:
        sp = Path(structured)

        candidates.append(
            str(
                sp.with_name(
                    sp.stem
                    + "_api_response.json"
                )
            )
        )

    for x in candidates:
        if not x:
            continue

        p = Path(x)

        if p.exists():
            shutil.copy2(
                p,
                attempt_dir
                / ("post_" + p.name),
            )


def one_row_manifest(row, fieldnames):
    f = tempfile.NamedTemporaryFile(
        mode="w",
        newline="",
        encoding="utf-8",
        suffix=".csv",
        prefix="erbiobench_recovery_",
        delete=False,
    )

    with f:
        w = csv.DictWriter(
            f,
            fieldnames=fieldnames,
        )
        w.writeheader()
        w.writerow(row)

    return Path(f.name)


def main():
    ap = argparse.ArgumentParser()

    ap.add_argument(
        "--model-slot",
        required=True,
        choices=["G1", "G2", "M1", "M2"],
    )

    ap.add_argument(
        "--max-runs",
        type=int,
        default=1,
    )

    ap.add_argument(
        "--execute",
        action="store_true",
        help=(
            "Actually call the provider. "
            "Without this flag, plan only."
        ),
    )

    args = ap.parse_args()

    manifest = ALLOWED_MANIFESTS[
        args.model_slot
    ]

    rows = read_csv(manifest)

    if not rows:
        raise SystemExit(
            "ERROR: empty recovery manifest"
        )

    fieldnames = list(rows[0].keys())

    eligible = [
        r for r in rows
        if is_open_for_recovery(
            r,
            args.model_slot,
        )
    ]

    print(
        "========================================"
    )
    print(
        "ERBIOBENCH GUARDED SMOKE RECOVERY"
    )
    print(
        "========================================"
    )

    print(
        "Model slot:",
        args.model_slot,
    )
    print(
        "Manifest:",
        manifest,
    )
    print(
        "Eligible now:",
        len(eligible),
    )
    print(
        "Requested max runs:",
        args.max_runs,
    )
    print(
        "Execute:",
        args.execute,
    )

    print("\nNEXT RUNS")

    for r in eligible[
        :args.max_runs
    ]:
        print(
            r["run_id"],
            "|",
            current_state(r),
        )

    if not args.execute:
        print(
            "\nPLAN ONLY — NO API CALLS MADE"
        )
        print(
            "GUARDED_RECOVERY_PLAN_OK"
        )
        return

    if not eligible:
        print(
            "\nNo eligible runs."
        )
        return

    completed = 0

    for row in eligible[
        :args.max_runs
    ]:

        run_id = row["run_id"]

        attempt_no = (
            next_attempt_number(run_id)
        )

        print(
            "\n----------------------------------------"
        )
        print(
            f"RUN {run_id}"
        )
        print(
            f"Recovery attempt: {attempt_no}"
        )

        attempt_dir = archive_existing(
            row,
            attempt_no,
        )

        tmp_manifest = one_row_manifest(
            row,
            fieldnames,
        )

        try:
            cmd = [
                "python3",
                str(RUNNER),
                "--manifest",
                str(tmp_manifest),
                "--max-runs",
                "1",
                "--resume",
            ]

            proc = subprocess.run(
                cmd,
                text=True,
            )

            if proc.returncode != 0:
                print(
                    "RUNNER PROCESS FAILURE:",
                    proc.returncode,
                )
                archive_post(
                    row,
                    attempt_dir,
                )
                break

        finally:
            try:
                tmp_manifest.unlink()
            except Exception:
                pass

        archive_post(
            row,
            attempt_dir,
        )

        meta = read_meta(row)

        if meta is None:
            print(
                "STOP: metadata missing after run"
            )
            break

        status = meta.get(
            "execution_status"
        )

        valid = meta.get(
            "parse_valid_json"
        )

        error = meta.get(
            "error"
        ) or ""

        print(
            "POST STATUS:",
            status,
            "| JSON:",
            valid,
        )

        if error:
            print(
                "ERROR:",
                error[:300],
            )

        block = provider_block(meta)

        if block:
            print(
                "STOP PROVIDER BATCH:",
                block,
            )
            break

        completed += 1

    print(
        "\n========================================"
    )
    print(
        "GUARDED RECOVERY FINISHED"
    )
    print(
        "Runs processed without provider block:",
        completed,
    )
    print(
        "========================================"
    )


if __name__ == "__main__":
    main()
