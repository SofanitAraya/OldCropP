\name{CropPhenology-package}
\alias{CropPhenology-package}
\alias{CropPhenology}
\docType{package}
\title{
\packageTitle{CropPhenology}
}
\description{
This package extracts crop phenological metrics from Moderate Resolution Imaging Spectroradiometer (MODIS) time series vegetation index data. 
}
\details{
The 16 days composite of MODIS vegetation index data provides the overall growth condition of the crop in the growing season with regular capture time. Plotting the vegetation index vakue accross time, provides the time series curve which could reporesent the seaonal growth pattern of the crop. The CropPhenology package extracts metrics from the time seris curve based on the curve nature and shape. These metrics indicate different physiological stages and condition of the crop. i
}
\author{
\packageAuthor{CropPhenology}

Maintainer: \packageMaintainer{CropPhenology}
}
\references{
Araya etal. (2015)
}

\keyword{ Phenology
Time series
}
\seealso{
PhenoMetrics (), TWoPointsPlot ()
}
\examples{
PhenoMetrics(system.file("extdata/data1", package="CropPhenology"), FALSE)
TwoPointsPlot(251,247)
}
