% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/imagej_tables.R
\name{collect_csv_files}
\alias{collect_csv_files}
\title{Collect CSVs}
\usage{
collect_csv_files(path = ".", pattern = "*.csv", annotations = NULL)
}
\arguments{
\item{path}{the directory from which files will be read}

\item{pattern}{a regular expression that matches the files of
interest}

\item{annotations}{a dataframe with its first collumn to be matched to filenames}
}
\value{
a list of annotated dataframes.
}
\description{
Collect CSV files
}
\details{
Provide the path to a directory containing csv files that will be
read. Optionally, add a dataframe object containing
annotations. The dataframe containing annotations must, as its
first column, contain a list of strings that will match to the
file names of the imported files.
}
\author{
Jones
}
