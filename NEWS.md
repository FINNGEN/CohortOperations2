# CohortOperations2 3.0.0
- Updated to use DatabaseConnector v7
- Enable editing cohort short names during importing

# CohortOperations2 2.3.0
- Simplified importing from file dialog
- GWAS naming less restrictive now
- Add session token to the logs for tracking
- other minor bug fixes
- 
# CohortOperations2 2.2.1
- Hotfix: fix error when importing cohorts from file

# CohortOperations2 2.2.0
- Improvements in logging display, show logs when error occurs, alerts replace with modals
- Improvements in File import, clearer dialog, parse Kanta data format
- Auto adjust match ratio based on number of cases and controls

# CohortOperations2 2.1.0
- New `Import Cohorts > from Atlas` dialog, import cohorts built in Atlas
- Select database to show info on atlas connection to results db
- Added broadsea connection config file

# CohortOperations2 2.0.0
- New cohorts in `Import Cohorts > from Library`:  Genetic outliers, individuals with Olink data, and more
- New `Import Cohorts > from File` dialog, more clever file type detection
- New menu, `View results`
- Switch from JDBC to BQ-DBI driver, for speed and simplicity
- Other minor fixes and improvements

# CohortOperations2 1.0.2

- Fix 0 subjects when import cohort after matching

# CohortOperations2 1.0.1

- Minor hot fixes

# CohortOperations2 1.0.0
- Major beta release
- Analyses moved to `CO2AnalysisModules`
- removed `renv`
- downgraded features to make it simpler at the moment
  - connect to only one database
  - run analysis on main process instead of `icp`
