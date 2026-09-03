1. Why the search results were downloaded by year

The complete AI query returned more than 900,000 records. In our use of the WoS web interface, we encountered an effective export/deep-pagination ceiling at approximately 100,000 records within a single result set. Once this range was exceeded, records beyond the first approximately 100,000 could not be exported reliably.

For this reason, the complete search was divided into smaller, mutually exclusive subsets before downloading.

The primary strategy was to download the records year by year. For example:
1990
1991
1992
...
2024
2025
If the number of records in a single year exceeded approximately 100,000, that year was further divided by publication date.

For example, a year could be divided into two subsets:
January–June
July–December
or, when necessary, into four subsets:
January–March
April–June
July–September
October–December
The subsets should be mutually exclusive and together reproduce the total number of records returned for the original year.
2. Automated WoS export

The WoS records were downloaded using the Python browser-automation script provided in:
code/python/
The script assists with repetitive batch exporting from the WoS web interface but does not perform the WoS search itself.

The download procedure is as follows:
1) Run the WoS download script.
2) The script opens a dedicated browser window.
3) In this browser window, log in to Web of Science using an account or institutional access with permission to use the Web of Science Core Collection.
4) Enter the required search query in WoS and apply the appropriate filters, including:
publication year or publication-date interval;
document type;
language;
Web of Science Core Collection.
5) Confirm that the browser is displaying the final WoS search-results page for the subset that you intend to download.
6) Keep only the target WoS results page open in the automation browser during the export process. Additional WoS tabs or unrelated pages should be closed to reduce the risk that the automation script interacts with the wrong browser page.
7) Return to the Python script and resume execution.
8) The script then exports the records sequentially in batches of 500 records.
9) Records are exported as plain-text files using the Full Record and Cited References option.
10) After one year or date-defined subset is completed, repeat the same procedure for the next subset.

3. Example of splitting a large yearly result set
Suppose the full search for 2025 returns more than 100,000 records.

Instead of attempting to export the entire result set directly, construct smaller date-defined subsets, for example:
2025-01-01 to 2025-06-30
2025-07-01 to 2025-12-31

If either subset remains too large, divide the year further:
2025-01-01 to 2025-03-31
2025-04-01 to 2025-06-30
2025-07-01 to 2025-09-30
2025-10-01 to 2025-12-31
Before downloading, verify that the sum of the subset record counts equals the record count of the complete yearly search.

4. Combining and deduplicating the downloaded records
After all yearly or date-defined subsets had been downloaded, the exported .txt files were combined using the Python processing scripts provided in:
code/python/
Duplicate records generated during the merging process were identified using the unique Web of Science accession number (UT) and removed before subsequent analysis.

The initial WoS search returned approximately 935,160 records. After combining all downloads and removing duplicate records, 934,495 unique AI-related publications were retained.

5. Reproducibility note

The exact number of records returned by Web of Science may change slightly over time because the database is continuously updated. Therefore, users repeating the search at a later date may not obtain exactly the same record count.

The repository provides the complete search syntax and data-processing workflow so that researchers with access to the Web of Science Core Collection can reproduce the retrieval and processing procedure.

6. Raw WoS records

The original WoS plain-text exports are not redistributed in this repository because the underlying bibliographic records are subject to Web of Science/Clarivate licensing conditions.

Instead, the repository provides:

the complete WoS search query;
the browser-automation scripts used for downloading;
Python scripts for merging, deduplication, and classification;
derived analytical datasets;
R scripts used for statistical analyses and figure generation