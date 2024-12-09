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
