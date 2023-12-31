% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/freq_mc.R
\name{tableband_mc}
\alias{tableband_mc}
\title{Create Table for Multiple Choice Answer}
\usage{
tableband_mc(
  df,
  row_vars,
  col_vars,
  count_factor,
  item_labels = T,
  weight = NULL
)
}
\arguments{
\item{df}{Dataframe with variables}

\item{row_vars}{Variables for Breaks}

\item{col_vars}{Multiple Choice Variables to count for}

\item{count_factor}{Factor Level to count for}

\item{item_labels}{Boolean parameter for showing item labels (Default: True) or variable names (False). The item labels are generated from attributes ("label").}

\item{weight}{Weight}
}
\value{
Returns a single dataframe with frequency of multiple choice answers
}
\description{
Create Table for Multiple Choice Answer
}
