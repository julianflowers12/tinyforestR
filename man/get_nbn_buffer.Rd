% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_nbn_data.R
\name{get_nbn_buffer}
\alias{get_nbn_buffer}
\title{Retrieves occurrence data from NBN Atlas within a specified buffer around given longitude and latitude}
\usage{
get_nbn_buffer(lon, lat, radius = 1, n = 10000, tf_id)
}
\arguments{
\item{lon}{numeric Longitude in decimal degrees}

\item{lat}{numeric Latitude in decimal degrees}

\item{radius}{numeric Buffer radius in kilometers, default is 1 km}

\item{n}{numeric Maximum number of occurrences to retrieve, default is 10,000}
}
\value{
data frame containing occurrence information within the buffer
}
\description{
Retrieves occurrence data from NBN Atlas within a specified buffer around given longitude and latitude
}
\examples{
get_nbn_buffer(-1.5, 52.0, 2)

}
