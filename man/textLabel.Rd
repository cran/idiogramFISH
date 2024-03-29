% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/textLabel.R
\name{textLabel}
\alias{textLabel}
\title{textLabel
This is an internal function that plot labels of cM marks}
\usage{
textLabel(
  xMark,
  yMark,
  listChrSizeText1,
  listOfdfMarkPos,
  specialChrSpacing,
  chrSpacing,
  markLabelSize,
  pattern,
  bannedMarkName,
  isCentromeric = FALSE,
  markNewLine2,
  mylheight2,
  xsize = 0
)
}
\arguments{
\item{xMark}{x coords}

\item{yMark}{y coords}

\item{listOfdfMarkPos}{color}

\item{specialChrSpacing}{vertices number}

\item{chrSpacing}{aspect}

\item{markLabelSize}{size of mark label font}

\item{pattern}{pattern to remove from mark name}

\item{isCentromeric}{use TRUE for cen. marks}
}
\value{
plotted text
}
\description{
It returns a plot
}
\keyword{internal}
