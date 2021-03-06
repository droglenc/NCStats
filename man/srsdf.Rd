% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc-utils.R
\name{srsdf}
\alias{srsdf}
\title{Extract a simple random sample from a data.frame.}
\usage{
srsdf(df, n, replace = FALSE, vars = NULL, removeRowNames = TRUE)
}
\arguments{
\item{df}{The data frame to be sampled from.}

\item{n}{Number of individuals (i.e., rows) to sample.}

\item{replace}{A logical that indicates whether sampling with replacement should be used or not (default).}

\item{vars}{A vector of strings that indicates which variables to include in the returned data.frame.}

\item{removeRowNames}{A logical that indicates whether the rownames should be removed from the resultant data.frame.}
}
\value{
Returns a data.frame of rows sampled from \code{df}.  Optionally, only includes the columns listed in \code{vars}.
}
\description{
Extract a simple random sample from a data.frame, allowing user to choose particular variables.
}
\examples{
data(ABDLakes)

# sample all variables without replacement
( res1 <- srsdf(ABDLakes,50) )

# sample two variables without replacement
( res2 <- srsdf(ABDLakes,50,vars=c("name","area")) )

# sample two variables with replacement
( res3 <- srsdf(ABDLakes,50,replace=TRUE,vars=c("name","area")) )

}
\seealso{
\code{\link{sample}}.
}
\keyword{manip}
