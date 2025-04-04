% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_fct_AppendCohort.R
\name{mod_fct_appendCohort_server}
\alias{mod_fct_appendCohort_server}
\title{Append Cohort Server Function}
\usage{
mod_fct_appendCohort_server(
  id,
  r_databaseConnection,
  r_cohortDefinitionSetToAdd
)
}
\arguments{
\item{id}{A module id.}

\item{r_databaseConnection}{A reactive database connection object.}

\item{r_cohortDefinitionSetToAdd}{A reactive cohort definition set to add.}
}
\value{
A reactive value indicating the number of times cohorts have been appended.
}
\description{
A shiny Module server function for appending cohorts.
}
