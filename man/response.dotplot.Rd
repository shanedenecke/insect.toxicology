% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/response.dotplot.R
\name{response.dotplot}
\alias{response.dotplot}
\title{Creates Dotplots That Reflect How Each Vial Survives an Insecticide Treatment}
\usage{
response.dotplot(adjusted.data, control, write = T, format = "pdf",
  subfolder = "Individual_Vial_Dotplots")
}
\arguments{
\item{adjusted.data}{A data frame or data table. Must include control dose, 0, for each insecticide. This is input for this value is usually an ouput from dmc.correct}

\item{control}{A string of your control genotype. e.g "Line14"}

\item{write}{If TRUE graphs will be produced in the specified format and .csv tables will be generated. If FALSE ouput will be localized to R. Defaults to TRUE}

\item{format}{what format do you want your graph images? Possible values "pdf", "png","tiff","jpeg"}

\item{subfolder}{What do you want the subfolder to be called. This will contain all figures and tables generated in this study}
}
\value{
Creates Abbot's correction Plots and Tables for Toxicology
}
\description{
Creates Dotplots That Reflect How Each Vial Survives an Insecticide Treatment
}
