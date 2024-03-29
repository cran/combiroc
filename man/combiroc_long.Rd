% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/combiroc_long.R
\name{combiroc_long}
\alias{combiroc_long}
\title{Reshape combiroc data in long format.}
\usage{
combiroc_long(data)
}
\arguments{
\item{data}{a data.frame returned by load_data().}
}
\value{
a data.frame in long format
}
\description{
A function that simply wraps dyplr::pivot_longer() to reshape data in long format without taking into account NAs.
}
\details{
This function returns the data in long format (with 'Markers' and 'Values' columns)
}
\examples{
\dontrun{
demo_data # combiroc built-in demo data (proteomics data from Zingaretti et al. 2012 - PMC3518104)

# To reshape demo_data in long format

demo_data_long <- combiroc_long(data = demo_data)
}
}
