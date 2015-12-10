\name{CropPhenology-package}
\alias{CropPhenology-package}
\alias{CropPhenology}
\docType{package}
\title{
\packageTitle{CropPhenology}
}
\description{
This package extracts crop phenological metrics from time series vegetation index data. 
}
\details{
The CropPhnology package defines the phenologic metrics which are at close correlation with the crop physiological stages based on the description at Zadok crop developmental growth stages.  

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
