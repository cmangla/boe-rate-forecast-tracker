#!/usr/bin/env python3
"""Incrementally validate and append UK BoE forecasts & base rate ledgers.

Strict schema assertions fail loud (GitHub Actions ::error:: + exit 1) if
ING or Scotiabank alter their table index, country label, header syntax,
or vintage-date HTML banner.
"""

from __future__ import annotations

import calendar
import csv
import html as html_lib
import io
import re
import sys
from datetime import datetime, timedelta, timezone
from pathlib import Path

import pandas as pd
import requests

REPO_ROOT = Path(__file__).resolve().parent.parent
DATA_DIR = REPO_ROOT / "data"
FORECASTS_PATH = DATA_DIR / "boe_forecasts.csv"
BASE_RATE_PATH = DATA_DIR / "boe_base_rate.csv"

ING_CSV = REPO_ROOT / "ing-forecasts_2.csv"
ING_HTML = REPO_ROOT / "ing-forecasts.html"
SCOTIA_CSV = REPO_ROOT / "scotiabank-forecast-snapshot_1.csv"
SCOTIA_HTML = REPO_ROOT / "scotiabank-forecast-snapshot.html"

HEADERS = {
    "User-Agent": (
        "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 "
        "(KHTML, like Gecko) Chrome/126.0.0.0 Safari/537.36"
    ),
    "Accept-Language": "en-GB,en;q=0.9",
}
IADB_URL = (
    "https://www.bankofengland.co.uk/boeapps/database/"
    "_iadb-fromshowcolumns.asp"
)
HTML_URL = "https://www.bankofengland.co.uk/boeapps/database/Bank-Rate.asp"

FORECAST_COLS = [
    "bank",
    "forecast_date",
    "horizon_label",
    "horizon_type",
    "target_date",
    "forecast_rate",
    "recorded_at",
]


class SchemaError(RuntimeError):
    """Raised when an upstream bank table or HTML banner breaks invariants."""


def emit_schema_error(msg: str) -> None:
    print(
        f"::error file=scripts/update_boe_tables.py,"
        f"title=Upstream Forecast Schema Changed::{msg}",
        file=sys.stderr,
    )


def parse_ing_horizon(label: str) -> tuple[str, str]:
    txt = str(label).strip()
    m_q = re.match(r"^([1-4])Q(\d{2,4})F?$", txt, re.I)
    if m_q:
        q = int(m_q.group(1))
        yr_raw = m_q.group(2)
        yr = int(yr_raw) if len(yr_raw) == 4 else 2000 + int(yr_raw)
        month = q * 3
        day = calendar.monthrange(yr, month)[1]
        return f"{yr:04d}-{month:02d}-{day:02d}", "quarter"
    m_y = re.match(r"^(\d{4})F$", txt, re.I)
    if m_y:
        yr = int(m_y.group(1))
        return f"{yr:04d}-12-31", "annual"
    raise SchemaError(f"ING: unrecognized horizon column header {txt!r}")


def parse_scotia_horizon(label: str) -> tuple[str, str] | None:
    txt = str(label).strip()
    if re.match(r"^\d{4}$", txt):
        return None  # Realized historical year column (e.g. "2025")
    m = re.match(r"^(\d{4})\s*\(([FE])\)$", txt, re.I)
    if m:
        yr = int(m.group(1))
        return f"{yr:04d}-12-31", "annual"
    raise SchemaError(f"Scotiabank: unrecognized horizon column header {txt!r}")


def parse_ing_banner_date(html_text: str, today: datetime) -> str:
    m = re.search(
        r"Policy rates</h2>\s*<p>\s*Last updated:\s*([^<]+)</p>",
        html_text,
        re.I,
    )
    if not m:
        raise SchemaError(
            "ING HTML: missing 'Policy rates</h2><p>Last updated: ...' banner"
        )
    raw = html_lib.unescape(m.group(1)).replace("\xa0", " ").strip()
    raw = re.sub(r"(\d+)(st|nd|rd|th)\b", r"\1", raw, flags=re.I)
    raw = re.sub(r"\s+", " ", raw).strip(" .")

    for fmt in ("%d %B %Y", "%d %b %Y"):
        try:
            return datetime.strptime(raw, fmt).strftime("%Y-%m-%d")
        except ValueError:
            pass
    cdate = today.date()
    for fmt in ("%d %B", "%d %b"):
        try:
            dt = datetime.strptime(f"{raw} {cdate.year}", f"{fmt} %Y").date()
            if (dt - cdate).days > 30:
                dt = dt.replace(year=cdate.year - 1)
            return dt.strftime("%Y-%m-%d")
        except ValueError:
            pass
    raise SchemaError(f"ING HTML: could not parse Last updated date {raw!r}")


def parse_scotia_banner_date(html_text: str) -> str:
    m = re.search(r"Forecasts as of\s+([^<]+)", html_text, re.I)
    if not m:
        raise SchemaError("Scotiabank HTML: missing 'Forecasts as of ...' banner")
    banner = html_lib.unescape(m.group(1)).replace("\xa0", " ").strip()
    m_other = re.search(
        r"and\s+([A-Za-z]+\s+\d{1,2},\s+\d{4})\s+for all other countries",
        banner,
        re.I,
    )
    if m_other:
        date_str = m_other.group(1).strip()
    else:
        m_single = re.match(r"^([A-Za-z]+\s+\d{1,2},\s+\d{4})", banner)
        if not m_single:
            raise SchemaError(
                f"Scotiabank HTML: could not parse vintage date from {banner!r}"
            )
        date_str = m_single.group(1).strip()
    try:
        return datetime.strptime(date_str, "%B %d, %Y").strftime("%Y-%m-%d")
    except ValueError as exc:
        raise SchemaError(
            f"Scotiabank HTML: invalid date string {date_str!r}"
        ) from exc


def extract_ing_current(
    csv_path: Path = ING_CSV,
    html_path: Path = ING_HTML,
    today: datetime | None = None,
) -> list[dict]:
    today = today or datetime.now(timezone.utc)
    if not csv_path.exists():
        raise SchemaError(f"ING CSV missing: {csv_path.name}")
    if not html_path.exists():
        raise SchemaError(f"ING HTML missing: {html_path.name}")

    rows = list(csv.reader(csv_path.open("r", encoding="utf-8")))
    if len(rows) < 5:
        raise SchemaError(f"ING CSV {csv_path.name} has too few rows ({len(rows)})")

    header = rows[0]
    if len(header) < 6 or "g10" not in header[1].lower():
        raise SchemaError(
            f"ING CSV {csv_path.name} col 1 header {header[1]!r} does not look like G10 Policy Rates"
        )

    col1_vals = {r[1].strip() for r in rows[1:] if len(r) > 1}
    if "US (upper bound)" not in col1_vals and "Eurozone (deposit rate)" not in col1_vals:
        raise SchemaError(
            f"ING CSV {csv_path.name} lacks 'US (upper bound)' / 'Eurozone (deposit rate)' "
            "— table index likely shifted again!"
        )

    uk_rows = [r for r in rows[1:] if len(r) > 1 and r[1].strip() == "UK"]
    if len(uk_rows) != 1:
        raise SchemaError(
            f"ING CSV {csv_path.name} expected 1 'UK' row, found {len(uk_rows)}"
        )
    uk_row = uk_rows[0]

    forecast_date = parse_ing_banner_date(
        html_path.read_text(encoding="utf-8", errors="replace"), today
    )
    recorded_at = today.strftime("%Y-%m-%d")

    by_target: dict[str, dict] = {}
    for idx in range(2, len(header)):
        h_label = header[idx].strip()
        if not h_label or h_label.startswith("Unnamed"):
            continue
        target_date, h_type = parse_ing_horizon(h_label)
        if idx >= len(uk_row):
            raise SchemaError(f"ING CSV: UK row missing value for {h_label!r}")
        val_str = uk_row[idx].strip().replace("%", "")
        try:
            rate = float(val_str)
        except ValueError as exc:
            raise SchemaError(
                f"ING CSV: non-numeric UK rate {uk_row[idx]!r} for {h_label!r}"
            ) from exc
        if not (-2.0 <= rate <= 25.0):
            raise SchemaError(
                f"ING CSV: out-of-range UK rate {rate} for {h_label!r}"
            )
        rec = {
            "bank": "ING",
            "forecast_date": forecast_date,
            "horizon_label": h_label,
            "horizon_type": h_type,
            "target_date": target_date,
            "forecast_rate": round(rate, 4),
            "recorded_at": recorded_at,
        }
        if target_date not in by_target or (
            by_target[target_date]["horizon_type"] == "annual"
            and h_type == "quarter"
        ):
            by_target[target_date] = rec

    if len(by_target) < 4:
        raise SchemaError(
            f"ING CSV: expected >= 4 UK forecast horizons, got {len(by_target)}"
        )
    return [by_target[k] for k in sorted(by_target)]


def extract_scotia_current(
    csv_path: Path = SCOTIA_CSV,
    html_path: Path = SCOTIA_HTML,
    today: datetime | None = None,
) -> list[dict]:
    today = today or datetime.now(timezone.utc)
    if not csv_path.exists():
        raise SchemaError(f"Scotiabank CSV missing: {csv_path.name}")
    if not html_path.exists():
        raise SchemaError(f"Scotiabank HTML missing: {html_path.name}")

    rows = list(csv.reader(csv_path.open("r", encoding="utf-8")))
    if len(rows) < 4:
        raise SchemaError(
            f"Scotiabank CSV {csv_path.name} has too few rows ({len(rows)})"
        )

    header = rows[1]
    units_row = rows[2]
    if not any("end of period" in c.lower() for c in units_row):
        raise SchemaError(
            f"Scotiabank CSV {csv_path.name} row 2 missing '(%, end of period)' "
            "— table index likely shifted!"
        )

    uk_rows = [
        r for r in rows[3:] if len(r) > 1 and r[1].strip() == "United Kingdom"
    ]
    if len(uk_rows) != 1:
        raise SchemaError(
            f"Scotiabank CSV {csv_path.name} expected 1 'United Kingdom' row, found {len(uk_rows)}"
        )
    uk_row = uk_rows[0]

    forecast_date = parse_scotia_banner_date(
        html_path.read_text(encoding="utf-8", errors="replace")
    )
    recorded_at = today.strftime("%Y-%m-%d")

    out: list[dict] = []
    for idx in range(2, len(header)):
        h_label = header[idx].strip()
        if not h_label:
            continue
        parsed = parse_scotia_horizon(h_label)
        if parsed is None:
            continue
        target_date, h_type = parsed
        if idx >= len(uk_row):
            raise SchemaError(
                f"Scotiabank CSV: UK row missing value for {h_label!r}"
            )
        val_str = uk_row[idx].strip().replace("%", "")
        try:
            rate = float(val_str)
        except ValueError as exc:
            raise SchemaError(
                f"Scotiabank CSV: non-numeric UK rate {uk_row[idx]!r} for {h_label!r}"
            ) from exc
        if not (-2.0 <= rate <= 25.0):
            raise SchemaError(
                f"Scotiabank CSV: out-of-range UK rate {rate} for {h_label!r}"
            )
        out.append(
            {
                "bank": "Scotiabank",
                "forecast_date": forecast_date,
                "horizon_label": h_label,
                "horizon_type": h_type,
                "target_date": target_date,
                "forecast_rate": round(rate, 4),
                "recorded_at": recorded_at,
            }
        )

    if len(out) < 2:
        raise SchemaError(
            f"Scotiabank CSV: expected >= 2 UK (F) horizons, got {len(out)}"
        )
    out.sort(key=lambda r: r["target_date"])
    return out


def update_forecasts_ledger() -> bool:
    """Validate current bank tables and append new curves to data/boe_forecasts.csv."""
    ing_recs = extract_ing_current()
    scotia_recs = extract_scotia_current()

    if FORECASTS_PATH.exists():
        existing = pd.read_csv(FORECASTS_PATH)
    else:
        existing = pd.DataFrame(columns=FORECAST_COLS)

    appended: list[dict] = []
    for bank, recs in (("ING", ing_recs), ("Scotiabank", scotia_recs)):
        new_sig = tuple(
            (r["target_date"], round(float(r["forecast_rate"]), 4)) for r in recs
        )
        bank_df = existing[existing["bank"] == bank]
        if not bank_df.empty:
            latest_fdate = bank_df["forecast_date"].max()
            latest_sub = bank_df[bank_df["forecast_date"] == latest_fdate].sort_values(
                "target_date"
            )
            old_sig = tuple(
                (str(row["target_date"]), round(float(row["forecast_rate"]), 4))
                for _, row in latest_sub.iterrows()
            )
            if old_sig == new_sig:
                continue
        appended.extend(recs)

    if not appended:
        print("Forecasts unchanged; data/boe_forecasts.csv not modified.")
        return False

    updated = pd.concat([existing, pd.DataFrame(appended)], ignore_index=True)
    updated = updated.drop_duplicates(
        subset=["bank", "forecast_date", "target_date"], keep="last"
    )
    updated = updated[FORECAST_COLS].sort_values(
        ["bank", "forecast_date", "target_date"]
    )
    DATA_DIR.mkdir(parents=True, exist_ok=True)
    updated.to_csv(FORECASTS_PATH, index=False)
    print(f"Appended {len(appended)} forecast rows to {FORECASTS_PATH}")
    return True


def fetch_recent_boe_rates(date_from_dt: datetime) -> tuple[pd.DataFrame, str]:
    date_from_iadb = date_from_dt.strftime("%d/%b/%Y")
    try:
        params = {
            "csv.x": "yes",
            "Datefrom": date_from_iadb,
            "Dateto": "now",
            "SeriesCodes": "IUDBEDR",
            "UsingCodes": "Y",
            "CSVF": "TN",
        }
        resp = requests.get(IADB_URL, params=params, headers=HEADERS, timeout=30)
        resp.raise_for_status()
        text = resp.text.lstrip()
        if text.startswith("<"):
            raise ValueError("BoE IADB returned HTML instead of CSV")
        lines = text.splitlines()
        hdr_idx = next(
            (i for i, ln in enumerate(lines) if ln.upper().startswith("DATE,")),
            None,
        )
        if hdr_idx is None:
            raise ValueError("Header 'DATE,' missing in BoE IADB payload")
        df = pd.read_csv(io.StringIO("\n".join(lines[hdr_idx:])))
        df.columns = [c.strip().upper() for c in df.columns]
        df = df.rename(columns={"DATE": "date", "IUDBEDR": "rate"})
        df["date"] = pd.to_datetime(df["date"], dayfirst=True, errors="coerce")
        df["rate"] = pd.to_numeric(df["rate"], errors="coerce")
        df = df.dropna(subset=["date", "rate"]).sort_values("date")
        if df.empty:
            raise ValueError("BoE IADB returned 0 valid rows")
        return df, "boe_iadb_iudbedr"
    except Exception as exc:
        print(f"BoE IADB fetch failed ({exc}); trying Bank-Rate.asp")
        resp = requests.get(HTML_URL, headers=HEADERS, timeout=30)
        resp.raise_for_status()
        tables = pd.read_html(io.StringIO(resp.text))
        if not tables or "Date Changed" not in tables[0].columns or "Rate" not in tables[0].columns:
            raise SchemaError("BoE Bank-Rate.asp table schema changed") from exc
        df = tables[0].rename(columns={"Date Changed": "date", "Rate": "rate"})
        df["date"] = pd.to_datetime(df["date"], format="%d %b %y", errors="coerce")
        df["rate"] = pd.to_numeric(df["rate"], errors="coerce")
        df = df.dropna(subset=["date", "rate"]).sort_values("date")
        df = df[df["date"] >= pd.Timestamp(date_from_dt.date())]
        if df.empty:
            raise SchemaError("BoE Bank-Rate.asp returned 0 valid rows") from exc
        return df, "boe_bank_rate_asp"


def update_base_rate_ledger(forecasts_changed: bool) -> bool:
    """Append new MPC rate changes; refresh latest_verified only if rate or forecasts changed."""
    if BASE_RATE_PATH.exists():
        existing = pd.read_csv(BASE_RATE_PATH)
        changes_df = existing[existing["event_type"] == "change"].copy()
        last_change_dt = datetime.strptime(
            str(changes_df["date"].max()), "%Y-%m-%d"
        )
        last_change_rate = float(
            changes_df.sort_values("date").iloc[-1]["rate"]
        )
        fetch_from = last_change_dt - timedelta(days=30)
    else:
        existing = pd.DataFrame(columns=["date", "rate", "event_type", "source"])
        changes_df = existing.copy()
        last_change_dt = datetime(2020, 1, 1)
        last_change_rate = -999.0
        fetch_from = last_change_dt

    try:
        raw, source = fetch_recent_boe_rates(fetch_from)
    except SchemaError:
        raise
    except Exception as exc:
        if BASE_RATE_PATH.exists():
            print(f"Warning: BoE endpoints unavailable ({exc}); keeping cached {BASE_RATE_PATH}")
            return False
        raise

    raw = raw.sort_values("date").reset_index(drop=True)
    # Identify any new rate changes strictly after last_change_dt
    new_changes: list[dict] = []
    prev_rate = last_change_rate
    for _, row in raw.iterrows():
        dt_str = row["date"].strftime("%Y-%m-%d")
        rt = round(float(row["rate"]), 4)
        if dt_str > last_change_dt.strftime("%Y-%m-%d") and rt != round(prev_rate, 4):
            new_changes.append(
                {
                    "date": dt_str,
                    "rate": rt,
                    "event_type": "change",
                    "source": source,
                }
            )
            prev_rate = rt

    latest_obs_date = raw.iloc[-1]["date"].strftime("%Y-%m-%d")
    latest_obs_rate = round(float(raw.iloc[-1]["rate"]), 4)

    if not new_changes and not forecasts_changed and BASE_RATE_PATH.exists():
        print("BoE base rate unchanged and no new forecasts; data/boe_base_rate.csv not modified.")
        return False

    if new_changes:
        changes_df = pd.concat(
            [changes_df, pd.DataFrame(new_changes)], ignore_index=True
        )
    latest_row = pd.DataFrame(
        [
            {
                "date": latest_obs_date,
                "rate": latest_obs_rate,
                "event_type": "latest_verified",
                "source": source,
            }
        ]
    )
    out = pd.concat([changes_df, latest_row], ignore_index=True)
    out = out.sort_values(["date", "event_type"])
    DATA_DIR.mkdir(parents=True, exist_ok=True)
    out.to_csv(BASE_RATE_PATH, index=False)
    print(
        f"Updated {BASE_RATE_PATH} ({len(new_changes)} new MPC changes; latest={latest_obs_rate}% on {latest_obs_date})"
    )
    return True


def main() -> int:
    try:
        fc_changed = update_forecasts_ledger()
        update_base_rate_ledger(forecasts_changed=fc_changed)
        return 0
    except SchemaError as exc:
        emit_schema_error(str(exc))
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
