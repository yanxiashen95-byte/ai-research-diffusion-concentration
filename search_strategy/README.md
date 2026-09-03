# Web of Science Search Strategy

This directory contains the complete Web of Science (WoS) search strategy and detailed instructions for retrieving the bibliographic records used in this study.

## Files

### `WoS_AI_search_query.txt`

This file contains the complete AI-related search query used in the Web of Science Core Collection.

The search was conducted in the **Topic (`TS`) field**, which covers:

- titles;
- abstracts;
- author keywords;
- Keywords Plus.

The search strategy includes general AI terminology, major machine-learning methods, neural-network architectures, physics-informed and domain-guided approaches, foundation models, and generative-AI models.

Search results were restricted to:

- **Database:** Web of Science Core Collection
- **Publication years:** 1990–2025
- **Document types:** Article and Review
- **Language:** English

The exact query can be found here:

[`WoS_AI_search_query.txt`](WoS_AI_search_query.txt)

---

### `WoS_download_workflow.md`

This file provides detailed instructions for reproducing the WoS data-retrieval procedure used in this study.

Because the complete AI-related search returned more than 900,000 records, the results were downloaded in smaller subsets. Records were primarily retrieved **year by year**. When a yearly result set exceeded the effective export range encountered in the WoS web interface, it was further divided into smaller publication-date intervals, such as half-year or quarterly subsets.

The WoS records were exported using the Python browser-automation script provided in the main repository. The script automates batch exports after the user has logged into WoS and prepared the target search-results page.

Detailed instructions are provided here:

[`WoS_download_workflow.md`](WoS_download_workflow.md)

---

## Data retrieval workflow

The overall retrieval procedure was:

1. Run the Python WoS download script.
2. Log in to the Web of Science Core Collection in the browser window opened by the script.
3. Enter the AI-related search query and apply the required filters.
4. Select the target publication year or publication-date interval.
5. Keep only the target WoS search-results page open in the automation browser.
6. Resume the Python script.
7. Export records sequentially in batches of 500.
8. Save records as plain-text files using the **Full Record and Cited References** option.
9. Repeat the procedure for all yearly or date-defined subsets.
10. Merge the exported files and remove duplicate records using the unique Web of Science accession number (`UT`).

The initial search returned approximately **935,160 records**. After merging the downloads and removing duplicate records, **934,495 unique AI-related publications** were retained.

---

## Reproducibility

Web of Science is continuously updated, so researchers repeating the search at a later date may obtain slightly different record counts.

The complete search syntax, download workflow, and processing scripts are provided to enable reproduction of the data-retrieval procedure.

The original WoS plain-text exports are not redistributed in this repository because the underlying bibliographic records are subject to Web of Science/Clarivate licensing conditions.