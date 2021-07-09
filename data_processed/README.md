# Processed files
These are clean, processed tables of data interpreted from the raw
files in [data_raw](https://github.com/CMAP-REPOS/state_disbursements/tree/main/data_raw). 
Interpretation is done by the R scripts in the [scripts folder](https://github.com/CMAP-REPOS/state_disbursements/tree/main/scripts) that begin with `1_loadclean`. Outputs seek, for the most part, to directly 
replicate the raw data. Exceptions to this include:

- PPRT data contains a field `local_gov_type` which is a staff interpretation 
of digits 4-6 of the district number.
- IOC data (coming from local gov AFRs) contains only revenues table info. These are records with 4 digit `Category` 
codes in the 200 series. Category codes that end with anything besides for "t" have been dropped ("t" rows are totals; their subcategories are ignored to avoid double counting). This data also contains a`Catname` column, which is a staff interpretation of the `Category` code. See the ioc_cats.csv file in [resources](https://github.com/CMAP-REPOS/state_disbursements/tree/main/resources). 
