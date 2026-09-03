#!/usr/bin/env python3
"""Classify WoS records into one OECD/FORD field; retain Natural Sciences 1.1-1.6."""

from __future__ import annotations

import argparse, csv, json, re, sqlite3, tempfile
from collections import Counter
from contextlib import ExitStack
from pathlib import Path

TAGS = {"UT","PY","TI","SO","DT","LA","AB","DE","ID","WC","SC","C1","RP","DI"}
FIELDS = {
 "1.1":"Mathematics", "1.2":"Computer and information sciences",
 "1.3":"Physical sciences", "1.4":"Chemical sciences",
 "1.5":"Earth and related environmental sciences", "1.6":"Biological sciences",
 "2":"Engineering and technology", "3":"Medical and health sciences",
 "4":"Agricultural sciences", "5":"Social sciences", "6":"Humanities and the arts",
 "U":"Unresolved / insufficient evidence",
}
NATURAL = {"1.1","1.2","1.3","1.4","1.5","1.6"}
TIE_PRIORITY = ["1.5","1.6","1.4","1.3","1.1","1.2","3","4","2","5","6","U"]

# Author-created transparent crosswalk anchored to OECD/FORD definitions.
WC = {
 "1.1":[r"^Mathematics(?:,|$)",r"^Statistics & Probability$",r"^Logic$"],
 "1.2":[r"^Computer Science(?:,|$)",r"^Information Science & Library Science$",r"^Mathematical & Computational Biology$"],
 "1.3":[r"^Physics(?:,|$)",r"Astronomy & Astrophysics",r"^Optics$",r"^Thermodynamics$",r"Nuclear Science",r"^Acoustics$"],
 "1.4":[r"^Chemistry(?:,|$)",r"^Electrochemistry$",r"^Spectroscopy$",r"^Polymer Science$"],
 "1.5":[r"Geoscience",r"Geochemistry & Geophysics",r"^Geology",r"Geography, Physical",r"Meteorology",r"Atmospheric",r"Oceanography",r"Water Resources",r"Environmental Sciences",r"^Ecology$",r"Remote Sensing",r"Limnology",r"Paleontology",r"Mineralogy"],
 "1.6":[r"Biochemistry",r"Molecular Biology",r"^Biology$",r"Biophysics",r"Biotechnology",r"Cell Biology",r"Genetics",r"Microbiology",r"Mycology",r"Zoology",r"Evolutionary",r"Developmental Biology",r"Reproductive Biology",r"Anatomy",r"Parasitology"],
 "2":[r"^Engineering",r"^Materials Science",r"Metallurgy",r"Nanoscience & Nanotechnology",r"Ceramics",r"Coatings",r"Composites",r"Crystallography",r"Mining & Mineral Processing",r"Automation & Control Systems",r"Robotics",r"Telecommunications",r"Transportation Science & Technology",r"Instruments & Instrumentation",r"Imaging Science",r"Mechanics",r"Operations Research & Management Science",r"Construction & Building Technology",r"Energy & Fuels",r"Ergonomics"],
 "3":[r"Medicine",r"Medical",r"Health",r"Clinical",r"Surgery",r"Oncology",r"Cardiovascular",r"Cardiac",r"Dentistry",r"Anesthesiology",r"Orthopedics",r"Otorhinolaryngology",r"Ophthalmology",r"Pathology",r"Pediatrics",r"Psychiatry",r"Psychology",r"Radiology",r"Rehabilitation",r"Respiratory",r"Rheumatology",r"Urology",r"Nephrology",r"Obstetrics",r"Gynecology",r"Nursing",r"Public, Environmental & Occupational Health",r"Tropical Medicine",r"Virology",r"Infectious",r"Immunology",r"Pharmacology",r"Toxicology",r"Physiology",r"Neurosciences"],
 "4":[r"Agricultur",r"Agronomy",r"Plant Sciences",r"Soil Science",r"Forestry",r"Fisheries",r"Food Science",r"Veterinary",r"Entomology",r"Marine & Freshwater Biology"],
 "5":[r"Social Sciences",r"Education",r"Management",r"Business",r"Economics",r"Anthropology",r"Communication",r"Sociology",r"Urban Studies",r"Regional & Urban Planning"],
 "6":[r"Religion",r"History",r"Humanities",r"Literature",r"^Art$",r"^Arts(?:,|$)",r"Language & Linguistics",r"Linguistics"],
}
TEXT = {
 "1.1":["theorem","mathematical proof","probability theory","numerical analysis","mathematical optimization"],
 "1.2":["computer science","software","database","information system","computer vision","natural language processing","cybersecurity","human-computer interaction","speech recognition"],
 "1.3":["quantum","photon","optics","plasma","particle physics","astrophys","gravitational","thermodynamic"],
 "1.4":["chemical reaction","chemistry","catalyst","catalysis","molecular property","qsar","cheminformatics","chemical synthesis"],
 "1.5":["climate","weather","earth system","geoscience","geophysical","hydrolog","groundwater","streamflow","remote sensing","ocean","atmospher","soil moisture","environmental"],
 "1.6":["genom","proteom","bioinformatics","protein","gene expression","cell biology","biological","evolution","microbi","ecology"],
 "2":["engineering","control system","robot","manufacturing","structural engineering","mechanical","electrical engineering","telecommunication","transportation","energy system","alloy","nanomaterial","composite","coating","metallurgy"],
 "3":["patient","clinical","diagnosis","disease","medical imaging","cancer","tumor","hospital","healthcare","brain imaging"],
 "4":["crop","agricultur","agronom","forestry","fishery","livestock","veterinary","food technology"],
 "5":["social science","education","management","economics","psychology","human behavior","public policy"],
 "6":["humanities","linguistics","history","philosophy","religion"],
}
COMPILED = {k:[re.compile(p,re.I) for p in ps] for k,ps in WC.items()}

COUNTRIES = {
 "USA":"USA","UNITED STATES":"USA","PEOPLES R CHINA":"China","PEOPLE'S R CHINA":"China","CHINA":"China",
 "ENGLAND":"United Kingdom","SCOTLAND":"United Kingdom","WALES":"United Kingdom","NORTH IRELAND":"United Kingdom","UK":"United Kingdom",
 "SOUTH KOREA":"South Korea","REPUBLIC OF KOREA":"South Korea","RUSSIA":"Russia","RUSSIAN FEDERATION":"Russia",
 "GERMANY":"Germany","FRANCE":"France","CANADA":"Canada","AUSTRALIA":"Australia","JAPAN":"Japan","INDIA":"India","ITALY":"Italy","SPAIN":"Spain","BRAZIL":"Brazil","NETHERLANDS":"Netherlands","SWITZERLAND":"Switzerland","SWEDEN":"Sweden","NORWAY":"Norway","DENMARK":"Denmark","FINLAND":"Finland","BELGIUM":"Belgium","AUSTRIA":"Austria","POLAND":"Poland","PORTUGAL":"Portugal","GREECE":"Greece","TURKEY":"Turkey","ISRAEL":"Israel","IRAN":"Iran","TAIWAN":"Taiwan","SINGAPORE":"Singapore","MALAYSIA":"Malaysia","THAILAND":"Thailand","PAKISTAN":"Pakistan","SAUDI ARABIA":"Saudi Arabia","SOUTH AFRICA":"South Africa","MEXICO":"Mexico","ARGENTINA":"Argentina","CHILE":"Chile","NEW ZEALAND":"New Zealand","IRELAND":"Ireland","CZECH REPUBLIC":"Czech Republic","CZECHIA":"Czech Republic","HUNGARY":"Hungary","ROMANIA":"Romania","EGYPT":"Egypt"
}

def records(path):
    cur={}; tag=None
    with path.open("r",encoding="utf-8-sig",errors="replace") as h:
        for raw in h:
            line=raw.rstrip("\r\n")
            if line=="ER":
                if cur: yield cur
                cur={}; tag=None; continue
            m=re.match(r"^([A-Z][A-Z0-9]) (.*)$",line)
            if m:
                tag,val=m.groups()
                if tag in TAGS: cur[tag]=(cur.get(tag,"")+(("; " if cur.get(tag) else "")+val.strip()))
            elif line.startswith("   ") and tag in TAGS and line.strip():
                cur[tag]=cur.get(tag,"")+"; "+line.strip()
    if cur: yield cur

def classify(r):
    scores={k:0 for k in FIELDS if k!="U"}; wc_hits={k:[] for k in scores}
    cats=[x.strip() for x in r.get("WC","").split(";") if x.strip()]
    for cat in cats:
        for code,patterns in COMPILED.items():
            if any(p.search(cat) for p in patterns): scores[code]+=100; wc_hits[code].append(cat)
    tk=" ".join(r.get(x,"") for x in ("TI","DE","ID")).lower(); ab=r.get("AB","").lower()
    for code,terms in TEXT.items():
        scores[code]+=5*sum(t in tk for t in terms)+sum(t in ab for t in terms)
    ranked=sorted(scores,key=lambda c:(-scores[c],TIE_PRIORITY.index(c)))
    best=ranked[0]
    if scores[best]==0: return "U",0,0,0,1,"No mapped WC or domain text evidence",""
    second=scores[ranked[1]]; margin=scores[best]-second
    tied=[c for c in ranked if scores[c]==scores[best]]
    review=int(margin<20 or not wc_hits[best] or len(tied)>1)
    reason=("WC: "+" | ".join(wc_hits[best])) if wc_hits[best] else "Text-only assignment"
    if len(tied)>1: reason+="; tied candidates="+",".join(tied)
    return best,scores[best],second,margin,review,reason,"; ".join(f"{c}:{scores[c]}" for c in ranked if scores[c]>0)

def country(r):
    raw=r.get("C1","") or r.get("RP","")
    first=re.split(r";\s*\[",raw,maxsplit=1)[0]
    first=re.sub(r"^\[[^]]+\]\s*","",first).strip().upper().rstrip(".;")
    for alias in sorted(COUNTRIES,key=len,reverse=True):
        if first==alias or re.search(r"(?:,|\s)"+re.escape(alias)+r"$",first): return COUNTRIES[alias]
    last=first.rsplit(",",1)[-1].strip().title() if first else ""
    return last if last and not re.search(r"\d",last) and len(last)<=40 else "Unknown"

DETAIL=["UT","analysis_year","source_publication_year","title","source_title","doi","document_type","language","wos_categories","research_areas","oecd_code","oecd_field","include_natural_sciences","classification_reason","best_score","second_score","score_margin","needs_manual_review","all_positive_scores","first_author_country","source_file"]

def main():
    ap=argparse.ArgumentParser(); ap.add_argument("--root",type=Path,required=True); ap.add_argument("--output",type=Path,required=True); ap.add_argument("--start-year",type=int,default=1990); ap.add_argument("--end-year",type=int,default=2025); a=ap.parse_args()
    a.output.mkdir(parents=True,exist_ok=True); (a.output/"by_field").mkdir(exist_ok=True)
    db=Path(tempfile.gettempdir())/"wos_oecd_seen.sqlite"; db.unlink(missing_ok=True)
    con=sqlite3.connect(db); con.execute("PRAGMA journal_mode=OFF"); con.execute("PRAGMA synchronous=OFF"); con.execute("CREATE TABLE seen(ut TEXT PRIMARY KEY, yr INT)")
    totals=Counter(); field_year=Counter(); country_year_field=Counter(); wc_year=Counter(); qc=Counter()
    with ExitStack() as stack:
        allh=stack.enter_context((a.output/"all_records_classified.csv").open("w",newline="",encoding="utf-8-sig")); allw=csv.DictWriter(allh,fieldnames=DETAIL); allw.writeheader()
        revh=stack.enter_context((a.output/"manual_review.csv").open("w",newline="",encoding="utf-8-sig")); revw=csv.DictWriter(revh,fieldnames=DETAIL); revw.writeheader()
        writers={}
        for code in sorted(NATURAL):
            h=stack.enter_context((a.output/"by_field"/f"{code}_{FIELDS[code].replace(' ','_')}.csv").open("w",newline="",encoding="utf-8-sig")); w=csv.DictWriter(h,fieldnames=DETAIL); w.writeheader(); writers[code]=w
        for yr in range(a.start_year,a.end_year+1):
            for p in sorted((a.root/str(yr)).glob("*.txt")):
                if "log" in p.name.lower(): continue
                qc[(yr,"files")]+=1
                for r in records(p):
                    qc[(yr,"source")]+=1; ut=r.get("UT","").strip()
                    if not ut: qc[(yr,"missing_ut")]+=1; ut=f"MISSING:{yr}:{p.name}:{qc[(yr,'source')]}"
                    try: con.execute("INSERT INTO seen VALUES (?,?)",(ut,yr))
                    except sqlite3.IntegrityError: qc[(yr,"duplicates")]+=1; continue
                    code,best,second,margin,review,reason,scorelist=classify(r); inc=int(code in NATURAL); c=country(r)
                    row={"UT":ut,"analysis_year":yr,"source_publication_year":r.get("PY",""),"title":r.get("TI",""),"source_title":r.get("SO",""),"doi":r.get("DI",""),"document_type":r.get("DT",""),"language":r.get("LA",""),"wos_categories":r.get("WC",""),"research_areas":r.get("SC",""),"oecd_code":code,"oecd_field":FIELDS[code],"include_natural_sciences":inc,"classification_reason":reason,"best_score":best,"second_score":second,"score_margin":margin,"needs_manual_review":review,"all_positive_scores":scorelist,"first_author_country":c,"source_file":f"{yr}/{p.name}"}
                    allw.writerow(row); totals["unique"]+=1; totals["included"]+=inc; totals["review"]+=review; totals["unknown_country"]+=(c=="Unknown")
                    field_year[(yr,code)]+=1
                    if inc: writers[code].writerow(row); country_year_field[(yr,code,c)]+=1
                    if review: revw.writerow(row)
                    for cat in [x.strip() for x in r.get("WC","").split(";") if x.strip()]: wc_year[(yr,cat)]+=1
            con.commit(); print(f"{yr}: source={qc[(yr,'source')]:,} unique_new={sum(field_year[(yr,c)] for c in FIELDS):,} included={sum(field_year[(yr,c)] for c in NATURAL):,}",flush=True)
    def out(name,fields,rows):
        with (a.output/name).open("w",newline="",encoding="utf-8-sig") as h: w=csv.DictWriter(h,fieldnames=fields); w.writeheader(); w.writerows(rows)
    out("annual_field_counts.csv",["year","oecd_code","oecd_field","publication_count"],({"year":y,"oecd_code":c,"oecd_field":FIELDS[c],"publication_count":n} for (y,c),n in sorted(field_year.items())))
    out("annual_natural_field_country_counts.csv",["year","oecd_code","oecd_field","country","publication_count"],({"year":y,"oecd_code":c,"oecd_field":FIELDS[c],"country":co,"publication_count":n} for (y,c,co),n in sorted(country_year_field.items())))
    out("annual_wos_category_counts.csv",["year","wos_category","publication_count"],({"year":y,"wos_category":wc,"publication_count":n} for (y,wc),n in sorted(wc_year.items())))
    qrows=[]
    for y in range(a.start_year,a.end_year+1):
        unique=sum(field_year[(y,c)] for c in FIELDS); included=sum(field_year[(y,c)] for c in NATURAL); review=sum(1 for _ in ())
        qrows.append({"year":y,"txt_files":qc[(y,"files")],"source_records":qc[(y,"source")],"duplicate_rows_removed":qc[(y,"duplicates")],"unique_records":unique,"included_natural_sciences":included,"excluded_or_unresolved":unique-included,"inclusion_rate":included/unique if unique else 0})
    out("quality_control_by_year.csv",list(qrows[0]),qrows)
    summary={"source_root":str(a.root),"years":[a.start_year,a.end_year],"unique_records":totals["unique"],"included_natural_sciences":totals["included"],"excluded_or_unresolved":totals["unique"]-totals["included"],"needs_manual_review":totals["review"],"unknown_first_author_country":totals["unknown_country"],"method":"One mutually exclusive OECD/FORD assignment per UT. Analysis year is the download folder year; original PY is retained. Natural Sciences 1.1-1.6 are included; other and unresolved records are retained but excluded.","crosswalk_status":"Author-created WoS Category to OECD/FORD crosswalk; sensitivity/manual validation recommended."}
    (a.output/"processing_summary.json").write_text(json.dumps(summary,ensure_ascii=False,indent=2),encoding="utf-8"); print(json.dumps(summary,ensure_ascii=False,indent=2))
    con.close(); db.unlink(missing_ok=True)

if __name__=="__main__": main()
