#!/usr/bin/env python3

import argparse
import csv
import json
import shutil
import subprocess
import tempfile
from datetime import datetime, timezone
from pathlib import Path


RUNNER = Path(
    "ErBioBench/analysis/run_erbiobench_v2.py"
)

READINESS = Path(
    "ErBioBench/protocol/"
    "REP5_EXECUTION_READINESS_2026-10-18.csv"
)

BATCH_DIR = Path(
    "ErBioBench/protocol/"
    "rep5_execution_batches/by_model"
)

ARCHIVE_ROOT = Path(
    "ErBioBench/results/pilot_1_2/"
    "rep5_recovery_attempts"
)

ATTEMPT_LOG = Path(
    "ErBioBench/results/pilot_1_2/"
    "REP5_EXECUTION_ATTEMPT_LOG_v1.csv"
)


CLOSED_STATUSES = {
    "COMPLETED",
    "REFUSAL",
    "INVALID_JSON",
    "EMPTY_RESPONSE",
}


def read_csv(path):
    with path.open(
        newline="",
        encoding="utf-8-sig",
    ) as f:
        return list(csv.DictReader(f))


def read_json(path):
    try:
        return json.loads(
            Path(path).read_text(
                encoding="utf-8"
            )
        )
    except Exception:
        return None


def classify_metadata(row):
    p = Path(row["metadata_file"])

    if not p.exists():
        return "NEVER_ATTEMPTED"

    m = read_json(p)

    if not m:
        return "METADATA_UNREADABLE"

    status = m.get("execution_status")
    error = (m.get("error") or "").lower()
    finish = (
        m.get("finish_reason") or ""
    ).lower()

    if (
        status == "COMPLETED"
        and m.get("parse_valid_json") is True
    ):
        return "CLOSED_COMPLETED"

    if status == "REFUSAL":
        return "CLOSED_REFUSAL"

    if status == "INVALID_JSON":
        return "CLOSED_INVALID_JSON"

    if status == "EMPTY_RESPONSE":
        return "CLOSED_EMPTY_RESPONSE"

    if (
        "length" in finish
        or "max_tokens" in finish
        or "max_output_tokens" in finish
    ):
        return "CLOSED_OUTPUT_TRUNCATION"

    if status == "QUOTA_EXHAUSTED":
        return "PROVIDER_QUOTA_EXHAUSTED"

    if status == "API_FAILURE":

        if (
            "429" in error
            or "rate limit" in error
        ):
            return "PROVIDER_RATE_LIMIT_429"

        if (
            "403" in error
            or "tier_not_allowed" in error
        ):
            return "PROVIDER_ACCESS_403"

        if "503" in error:
            return "PROVIDER_HTTP_503"

        return "PROVIDER_API_FAILURE_OTHER"

    return f"OTHER_{status}"


def provider_block(state):
    return state in {
        "PROVIDER_QUOTA_EXHAUSTED",
        "PROVIDER_RATE_LIMIT_429",
        "PROVIDER_ACCESS_403",
        "PROVIDER_HTTP_503",
        "PROVIDER_API_FAILURE_OTHER",
    }


def readiness_for(rep, model):
    rows = read_csv(READINESS)

    target = [
        r for r in rows
        if r["repetition"] == f"REP{rep}"
        and r["model_slot"] == model
    ]

    if len(target) != 1:
        raise SystemExit(
            f"ERROR: readiness row not unique "
            f"for REP{rep} {model}"
        )

    return target[0]


def batch_path(rep, model):
    return (
        BATCH_DIR
        / f"REP{rep}_{model}_45runs_v1.csv"
    )


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


def archive_files(row, prefix, dest):
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
            target = dest / (
                prefix + p.name
            )
            shutil.copy2(p, target)
            copied.append(str(target))

    return copied


def make_single_manifest(
    row,
    fieldnames,
):
    f = tempfile.NamedTemporaryFile(
        mode="w",
        newline="",
        encoding="utf-8",
        suffix=".csv",
        prefix="erbiobench_rep5_",
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


def append_log(record):
    ATTEMPT_LOG.parent.mkdir(
        parents=True,
        exist_ok=True,
    )

    exists = ATTEMPT_LOG.exists()

    fields = [
        "timestamp_utc",
        "run_id",
        "repetition",
        "model_slot",
        "attempt_no",
        "pre_state",
        "post_state",
        "runner_returncode",
        "stopped_provider_batch",
        "attempt_dir",
    ]

    with ATTEMPT_LOG.open(
        "a",
        newline="",
        encoding="utf-8",
    ) as f:
        w = csv.DictWriter(
            f,
            fieldnames=fields,
        )

        if not exists:
            w.writeheader()

        w.writerow(record)


def main():
    ap = argparse.ArgumentParser()

    ap.add_argument(
        "--rep",
        type=int,
        required=True,
        choices=[2, 3, 4, 5],
    )

    ap.add_argument(
        "--model-slot",
        required=True,
        choices=[
            "A1", "A2", "A3",
            "G1", "G2", "G3",
            "M1", "M2", "M3",
            "O1", "O2", "O3",
        ],
    )

    ap.add_argument(
        "--max-runs",
        type=int,
        default=1,
    )

    ap.add_argument(
        "--execute",
        action="store_true",
    )

    ap.add_argument(
        "--override-hold",
        action="store_true",
        help=(
            "Allow execution of a batch "
            "currently marked HOLD."
        ),
    )

    args = ap.parse_args()

    ready = readiness_for(
        args.rep,
        args.model_slot,
    )

    state = ready["execution_state"]

    p = batch_path(
        args.rep,
        args.model_slot,
    )

    if not p.exists():
        raise SystemExit(
            f"ERROR: batch not found: {p}"
        )

    rows = read_csv(p)

    if len(rows) != 45:
        raise SystemExit(
            f"ERROR: expected 45 runs, "
            f"found {len(rows)}"
        )

    fieldnames = list(rows[0].keys())

    open_rows = []

    closed_counts = {}

    for r in rows:
        s = classify_metadata(r)

        if s.startswith("CLOSED_"):
            closed_counts[s] = (
                closed_counts.get(s, 0) + 1
            )
        else:
            open_rows.append(r)

    print(
        "========================================"
    )
    print(
        "ERBIOBENCH REP5 GUARDED EXECUTOR"
    )
    print(
        "========================================"
    )
    print("Repetition:", f"R{args.rep:02d}")
    print("Model slot:", args.model_slot)
    print("Readiness:", state)
    print("Batch:", p)
    print("Batch runs:", len(rows))
    print("Open runs:", len(open_rows))
    print("Closed runs:", len(rows) - len(open_rows))
    print("Requested max runs:", args.max_runs)
    print("Execute:", args.execute)

    if closed_counts:
        print("\nCLOSED STATES")
        for k in sorted(closed_counts):
            print(k, closed_counts[k])

    print("\nNEXT RUNS")

    for r in open_rows[
        :args.max_runs
    ]:
        print(
            r["run_id"],
            "|",
            classify_metadata(r),
        )

    if state != "READY":
        print(
            "\nBATCH HOLD:",
            state,
        )

        if not args.override_hold:
            print(
                "Execution blocked by readiness matrix."
            )

            if args.execute:
                raise SystemExit(2)

    if not args.execute:
        print(
            "\nPLAN ONLY — NO API CALLS MADE"
        )
        print(
            "REP5_GUARDED_PLAN_OK"
        )
        return

    if (
        state != "READY"
        and not args.override_hold
    ):
        raise SystemExit(2)

    if not open_rows:
        print(
            "\nNo open runs in batch."
        )
        return

    executed = 0

    for row in open_rows[
        :args.max_runs
    ]:
        run_id = row["run_id"]
        pre_state = classify_metadata(row)

        attempt_no = (
            next_attempt_number(run_id)
        )

        attempt_dir = (
            ARCHIVE_ROOT
            / run_id
            / f"attempt_{attempt_no:02d}"
        )

        attempt_dir.mkdir(
            parents=True,
            exist_ok=False,
        )

        pre_files = archive_files(
            row,
            "pre_",
            attempt_dir,
        )

        manifest_info = {
            "run_id": run_id,
            "repetition": args.rep,
            "model_slot": args.model_slot,
            "attempt_no": attempt_no,
            "archived_at_utc":
                datetime.now(
                    timezone.utc
                ).isoformat(),
            "pre_state": pre_state,
            "pre_files": pre_files,
        }

        (
            attempt_dir
            / "attempt_manifest.json"
        ).write_text(
            json.dumps(
                manifest_info,
                indent=2,
                ensure_ascii=False,
            ),
            encoding="utf-8",
        )

        print(
            "\n----------------------------------------"
        )
        print("RUN:", run_id)
        print("Pre-state:", pre_state)

        tmp = make_single_manifest(
            row,
            fieldnames,
        )

        try:
            cmd = [
                "python3",
                str(RUNNER),
                "--manifest",
                str(tmp),
                "--max-runs",
                "1",
                "--resume",
            ]

            proc = subprocess.run(
                cmd,
                text=True,
            )

        finally:
            try:
                tmp.unlink()
            except Exception:
                pass

        archive_files(
            row,
            "post_",
            attempt_dir,
        )

        post_state = classify_metadata(row)

        stopped = provider_block(
            post_state
        )

        append_log({
            "timestamp_utc":
                datetime.now(
                    timezone.utc
                ).isoformat(),
            "run_id": run_id,
            "repetition": args.rep,
            "model_slot": args.model_slot,
            "attempt_no": attempt_no,
            "pre_state": pre_state,
            "post_state": post_state,
            "runner_returncode":
                proc.returncode,
            "stopped_provider_batch":
                stopped,
            "attempt_dir":
                str(attempt_dir),
        })

        print(
            "Post-state:",
            post_state,
        )

        if proc.returncode != 0:
            print(
                "STOP: runner return code",
                proc.returncode,
            )
            break

        executed += 1

        if stopped:
            print(
                "STOP PROVIDER BATCH:",
                post_state,
            )
            break

    print(
        "\n========================================"
    )
    print(
        "REP5 GUARDED EXECUTION FINISHED"
    )
    print(
        "Runs attempted:",
        executed,
    )
    print(
        "========================================"
    )


if __name__ == "__main__":
    main()
