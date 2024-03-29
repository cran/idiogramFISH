% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mimicCenPlotMark.R
\name{mimicCenPlotMark}
\alias{mimicCenPlotMark}
\title{mimicCenPlotMark
This is an internal function that plot marks}
\usage{
mimicCenPlotMark(
  squareness,
  xMark,
  yMark,
  defCenStyleCol,
  listOfdfMarkPosCenStyle,
  chrWidth,
  specialChrWidth,
  yfactor,
  n,
  lwd.mimicCen,
  listOfdfChromSize,
  circularPlot,
  y,
  markLabelSize,
  separFactor,
  labelSpacing,
  circleCenter,
  circleCenterY,
  radius,
  ylistTransChr,
  rotation,
  labelOutwards,
  yMarkLine,
  xMarkRightLine,
  xMarkLeftLine,
  x,
  cenFormat = "triangle",
  pts
)
}
\arguments{
\item{squareness}{squareness of vertices <}

\item{xMark}{x component of polygon}

\item{yMark}{yMark component of polygon}

\item{listOfdfMarkPosCenStyle}{list of df. of mark pos.}

\item{chrWidth}{numeric, width of chr.}

\item{specialChrWidth}{numeric, width of chr.}

\item{yfactor}{yMark distortion based on canvas proportion}

\item{n}{numeric, to define vertices of rounded portions}

\item{listOfdfChromSize}{chr size df list}

\item{circularPlot}{boolean TRUE for circ.}

\item{y}{list, y coords.}

\item{markLabelSize}{numeric, font size}

\item{separFactor}{numeric, sep. kars.}

\item{labelSpacing}{numeric, spacing}

\item{circleCenter}{numeric}

\item{circleCenterY}{numeric}

\item{radius}{numeric}

\item{ylistTransChr}{list, transf. coords.}

\item{rotation}{rotate}

\item{labelOutwards}{srt}
}
\value{
plot
}
\description{
It returns a plot
}
\keyword{internal}
