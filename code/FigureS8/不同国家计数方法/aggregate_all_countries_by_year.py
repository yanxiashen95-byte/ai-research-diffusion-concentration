import csv
import re
from collections import Counter
from pathlib import Path

ROOT = Path(r"D:\Python\WOS_AI")
CLASSIFIED = ROOT / "OECD_Natural_Sciences_1990_2025" / "all_records_classified.csv"
MAIN_COUNTS = ROOT / "OECD_Natural_Sciences_1990_2025" / "annual_all_natural_country_publication_counts.csv"
OUTPUT = ROOT / "OECD_Natural_Sciences_1990_2025" / "annual_all_natural_country_publication_counts_full_country.csv"
AUDIT = ROOT / "OECD_Natural_Sciences_1990_2025" / "full_country_counting_audit.csv"


def load_country_aliases():
    canonical = {}
    with MAIN_COUNTS.open("r", encoding="utf-8-sig", newline="") as f:
        for row in csv.DictReader(f):
            country = row["country"].strip()
            if country and country != "Unknown":
                canonical[country.upper()] = country

    aliases = dict(canonical)
    aliases.update({
        "PEOPLES R CHINA": "China",
        "P R CHINA": "China",
        "MAINLAND CHINA": "China",
        "UNITED STATES": "USA",
        "ENGLAND": "United Kingdom",
        "SCOTLAND": "United Kingdom",
        "WALES": "United Kingdom",
        "N IRELAND": "United Kingdom",
        "NORTHERN IRELAND": "United Kingdom",
        "U ARAB EMIRATES": "United Arab Emirates",
        "APO AE": "USA",
        "FPO AE": "USA",
    })
    alternatives = "|".join(re.escape(x) for x in sorted(aliases, key=len, reverse=True))
    # A WoS address ends in a country; the next address begins after a semicolon.
    pattern = re.compile(r"(?<![A-Z])(" + alternatives + r")(?=\s*(?:;|$))", re.I)
    return aliases, pattern


def load_targets():
    targets = {}
    fallback = {}
    with CLASSIFIED.open("r", encoding="utf-8-sig", newline="") as f:
        for row in csv.DictReader(f):
            if row["include_natural_sciences"] != "1":
                continue
            ut = row["UT"].strip()
            if not ut or ut in targets:
                continue
            targets[ut] = (int(row["analysis_year"]), ROOT / Path(row["source_file"]))
            fallback[ut] = row["first_author_country"].strip()
    return targets, fallback


def main():
    aliases, country_pattern = load_country_aliases()
    targets, fallback = load_targets()
    uts_by_file = {}
    for ut, (_, path) in targets.items():
        uts_by_file.setdefault(path, set()).add(ut)

    counts = Counter()
    matched_uts = set()
    fallback_uts = set()
    paper_country_pairs = 0

    files = sorted(uts_by_file)
    for file_i, path in enumerate(files, 1):
        wanted = uts_by_file[path]
        current_ut = None
        c1_parts = []
        reading_c1 = False

        with path.open("r", encoding="utf-8-sig", errors="replace") as f:
            for raw in f:
                line = raw.rstrip("\r\n")
                if line.startswith("C1 "):
                    c1_parts = [line[3:].strip()]
                    reading_c1 = True
                elif reading_c1 and line.startswith("   "):
                    c1_parts.append(line.strip())
                elif line.startswith("UT "):
                    current_ut = line[3:].strip()
                    reading_c1 = False
                elif line == "ER":
                    if current_ut in wanted:
                        text = " ".join(c1_parts).upper()
                        countries = {
                            aliases[m.group(1).upper()]
                            for m in country_pattern.finditer(text)
                        }
                        if not countries:
                            country = fallback.get(current_ut, "")
                            if country and country != "Unknown":
                                countries = {country}
                                fallback_uts.add(current_ut)
                        if countries:
                            year = targets[current_ut][0]
                            matched_uts.add(current_ut)
                            for country in countries:
                                counts[(year, country)] += 1
                                paper_country_pairs += 1
                    current_ut = None
                    c1_parts = []
                    reading_c1 = False
                elif len(line) >= 3 and line[:2].isalnum() and line[2] == " ":
                    reading_c1 = False

        if file_i % 100 == 0 or file_i == len(files):
            print(f"Processed {file_i}/{len(files)} files", flush=True)

    OUTPUT.parent.mkdir(parents=True, exist_ok=True)
    with OUTPUT.open("w", encoding="utf-8-sig", newline="") as f:
        writer = csv.writer(f)
        writer.writerow(["year", "country", "publication_count"])
        for (year, country), n in sorted(counts.items()):
            writer.writerow([year, country, n])

    with AUDIT.open("w", encoding="utf-8-sig", newline="") as f:
        writer = csv.writer(f)
        writer.writerow(["item", "value"])
        writer.writerow(["natural_science_unique_papers", len(targets)])
        writer.writerow(["papers_assigned_at_least_one_country", len(matched_uts)])
        writer.writerow(["papers_using_first_country_fallback", len(fallback_uts)])
        writer.writerow(["distinct_paper_country_assignments", paper_country_pairs])
        writer.writerow(["additional_assignments_due_to_multi_country_counting", paper_country_pairs - len(matched_uts)])

    print(f"Saved: {OUTPUT}")
    print(f"Audit: {AUDIT}")


if __name__ == "__main__":
    main()
