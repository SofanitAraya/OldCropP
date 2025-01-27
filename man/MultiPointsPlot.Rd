% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CropPhenology.R
\name{MultiPointsPlot}
\alias{MultiPointsPlot}
\title{Time series curves for Multiple points in the Region of Interest}
\usage{
MultiPointsPlot(N, Id1, Id2, Id3, Id4, Id5)
}
\arguments{
\item{N}{- number of intersted points}

\item{Id1}{-  ID number for point 1}

\item{Id2}{-  Id number for point 2}

\item{Id3}{-  ID number for point 3}

\item{Id4}{-  ID number for point 4}

\item{Id5}{-  ID number for point 5}
}
\value{
Multiple time series curves together at the plot pannel
}
\description{
MultiPointsPlot function takes the ID for the pixels within the region of interst and returns, the timeseries curves from these points, ploted together. The Id numbers can be obtained from the txt file (AllPixels.txt) outputs.
}
\details{
This function allows plotting time series curves from multiple points together in a single plot which helps understanding the growth variability across the field.This inforaiton can further analyzed to provide insight on other environemtal factors.

The maximum number of pixeles allowed plotting togther are 5 points.
}
\examples{
MultiPointsPlot(3,11,114,125)




}
\author{
Sofanit Araya
}
\seealso{
PhenoMetrics()
}
\keyword{Curve}
\keyword{curves}
\keyword{from}
\keyword{multiple}
\keyword{points}
\keyword{time-series}

