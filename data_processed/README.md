# Processed files
These are clean, processed tables of data interpreted from the raw
files in [data_raw](https://github.com/CMAP-REPOS/state_disbursements/tree/main/data_raw). 
Interpretation is done by the R scripts in the [scripts folder](https://github.com/CMAP-REPOS/state_disbursements/tree/main/scripts) that begin with `1_loadclean`. Outputs seek, for the most part, to directly 
replicate the raw data. Exceptions to this include:

- PPRT data contains a field `local_gov_type` which is a staff interpretation 
of digits 4-6 of the district number.
- IOC data contains only revenues. These are records with 4 digit category 
codes that end with the letter "t"
