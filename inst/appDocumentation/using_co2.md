# Work with FinnGen cohorts

Cohort Operations (CO2) lets you work with FinnGen participant groups as **cohorts**. Use it to find or create cohorts, prepare case and control groups, compare their clinical features, and review or download the resulting data.

Most work follows the same path: select a data freeze, add the cohorts you need to the workbench, prepare them if necessary, and then choose an analysis.

## Select a data freeze

Start on **Databases connection**. In CO2, a database is a FinnGen data freeze in OMOP format. It contains the participant records, clinical events, standard vocabularies, and cohort tables used during your session.

The available FinnGen data freezes are **DF14**, **DF13v3**, **DF13**, **DF12**, and **DF11**. CO2 normally selects the latest release by default (currently **DF14**). Select an earlier freeze only when your study requires that specific release. Wait until the connection-status table confirms the connection before continuing.

## Find or create cohorts

The **cohort workbench** is your study's working set of cohorts: the groups of people you have gathered to use in CO2. Import cohorts into it when you want to work with them; any cohort created with **Operate Cohorts** or **Match Cohorts** is added there too. Creating a new cohort does not remove its source cohorts. The cohorts in the workbench are the ones you can select when setting up an operation, matching, export, or analysis—for example, as cases, controls, or cohorts to compare.

Open **Import Cohorts** to add cohorts. In most studies, start with one of the prepared sources:

- **from Atlas** imports cohorts whose definitions have been created in Atlas. Create or refine the definition in Atlas first, then select it here.
- **from Endpoint** imports curated FinnGen endpoint cohorts.
- **from Cohort Library** imports reusable, pre-created FinnGen cohorts from curated libraries.
- **from Task Force Cohorts** imports cohorts prepared by FinnGen task forces.
- **from File** imports a local list or cohort table. CO2 accepts `.tsv`, `.csv`, `.txt`, `.xls`, and `.xlsx` files, or pasted data. A one-column list of person identifiers is accepted. For a complete cohort table, provide the columns `cohort_name`, `person_source_value` (or `finngenid`), `cohort_start_date`, and `cohort_end_date`; use tabs or commas to separate columns in text files. If CO2 cannot recognise the columns, use the mapping dialog. It can use each participant's first and last observation dates when dates are not supplied.

Use **Save workbench** to keep the current list for later. Use **Clean workbench** only when you want to remove the current working list and start again.

## Prepare cohorts for a study

Use **Operate Cohorts** to create a cohort from cohorts already in the workbench. Build an expression with the available operations, inspect the intersection plot, and select **Create new cohort**. The source cohorts remain in the workbench.

Use **Match Cohorts** when you need a control group comparable with a case cohort. Select the case and target/control cohorts, choose the matching characteristics, and review the pre-run information. The requested ratio is the maximum number of controls per case: if too few eligible controls are available, CO2 creates the matched-control cohort using the available controls, so the achieved ratio may be lower.

Use **Export Cohorts** to download cohort data when you need it outside CO2.

## Explore cohorts and run analyses

Choose an analysis from the sidebar after the required cohorts are ready. Select the requested cohorts and settings, check the pre-run information, and then select **Run Study**.

- **Cohort Overlaps** shows how many people belong to each selected cohort and to every intersection of those cohorts.
- **Cohort Demographics** shows selected cohort entries by calendar year, approximate 10-year age band, and recorded sex. It produces separate summaries for the year of cohort entry, the year of cohort end, and year of birth. The birth-date summary is therefore mainly a birth-year and recorded-sex distribution.
- **Code WAS** is a binary enrichment analysis. It tests whether a selected clinical feature is recorded in a greater or lower proportion of cases than controls—for example, whether a diagnosis, procedure, measurement, drug, or other feature is more common in one group. Choose the feature groups and one comparison approach:
  - **Fast aggregated tests** use Fisher's exact test for binary features and Welch's t-test for numerical features. They compare case/control counts or averages only, so they do not adjust for factors such as sex or year of birth.
  - **Regressions** use logistic regression for binary features and linear regression for numerical features. They use person-level data and can adjust the comparison for sex and year of birth, but take longer to run.
- **Time Code WAS** compares selected clinical features in your chosen, non-overlapping time windows around each person's cohort start date. Negative breakpoints define windows before cohort start; positive breakpoints define windows after it. Use it to examine how feature enrichment changes across those windows.
- **Phenotype Scoring** is available for **DF14 only**. Use it primarily to refine a phenotype cohort. It compares the case cohort with controls, then lets you group selected concepts, build a score formula, label people, and filter or export individuals by score. This helps identify people who best fit the intended phenotype or who may not fit it. It can also help explore clinically meaningful subtypes or manifestations within a cohort.
- **GWAS** submits selected case and control cohorts to the configured Regenie genetic-association pipeline. Choose additive, dominant, or recessive testing and whether to continue with fine-mapping.
- **CodeWAS (beta)** is available for **DF14 only**. It includes the same binary presence/absence analysis as Code WAS and adds analyses that use more of the available clinical information:
  - **Counts:** whether people with the feature tend to have it recorded more often in cases or in controls.
  - **Age at first event:** whether people with the feature tend to have their first recorded event at a younger or older age in cases than in controls.
  - **Days to first event / days to last event:** whether people with the feature tend to have their first or last recorded event earlier or later relative to cohort start in cases than in controls. The value is the number of days from cohort start: negative values are before cohort entry and positive values are after it.
  - **Categorical values:** whether the distribution of recorded categories differs between cases and controls.
  - **Continuous values:** whether the recorded numeric values, such as laboratory measurements, differ between cases and controls.

  These analyses can reveal associations that binary presence/absence alone does not. The **aggregation method** controls the clinical concept hierarchy used for each result: **Code Counts** uses records mapped to the selected concept only; **Descendant Counts** uses records mapped to that concept and all more-specific concepts below it in the hierarchy; **Code and Descendant Counts** produces both result sets separately. For example, descendant mode can test a broad diagnosis together with its more specific subtypes.

For Code WAS and Time Code WAS, automatic control selection is available when configured. Choose the number of controls per case carefully: larger ratios can take longer to run.

## Review and keep results

When a study finishes, select **Open Viewer** to inspect the results. Use **Study details** to confirm the data freeze, cohorts, and settings used for that run. Use **Download to Sandbox** to save the results database.

If something fails, open **App Logs** and keep the relevant message when asking for support.
