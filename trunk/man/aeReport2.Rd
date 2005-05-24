\name{aeReport2}
\alias{aeReport2}
\title{Report Summarizing All Adverse Events}
\description{
This report uses a data format in which records exist only for events,
and these events are classified as \code{major} and \code{minor}
events.  Typically \code{major} refers to organ system and \code{minor}
to preferred term.  As only events are signaled in the data, the
denominator (number of subjects at risk and having some likelihood of
having had adverse events collected had they occured) for each treatment
must be specified.  Produces an open meeting table not stratified by
\code{treatment} (\code{"Oae.tex"}) and one stratified by
\code{treatment} (\code{"ae.tex"}) in the \code{"gentex"} directory.
}
\usage{
aeReport2(major, minor, treat, id, denom, bycaption,
size = NULL, longtable = FALSE, lines.page = 50, append = FALSE)
}
\arguments{
  \item{major}{factor or character vector specifying the major
	classification of events, usually organ system}
  \item{minor}{factor or character vector specifying the minor
	classification, usually an individual event given by the preferred term}
  \item{treat}{discrete treatment variable}
  \item{id}{subject ID, used to determine when multiple events arise
	from the same subject}
  \item{denom}{a vector giving the number of subjects on each treatment
	(in order of treatment levels), i.e., number of subjects at risk}
  \item{bycaption}{text string describing how events are coded}
  \item{size}{see \code{\link[Hmisc]{latex.default}}}
  \item{longtable}{see \code{\link[Hmisc]{latex.default}}}}
  \item{lines.page}{see \code{\link[Hmisc]{latex.default}}}
  \item{append}{set to \code{TRUE} to have LaTeX code appended to
	existing files}
}
\section{Side Effets}{creates or appends to files
  \code{"gentex/Oae.tex"} and \code{"gentex/ae.tex"} under the current
  working directory.}
\details{
Values of \code{minor} are not assumed to be unique across values of
\code{major}.
}
\author{Frank E Harrell Jr,
  \cr
  Department of Biostatistics,
  \cr
  Vanderbilt University School of Medicine,
  \cr
\code{f.harrell@vanderbilt.edu}}
\seealso{\code{\link{aeReport}}}
\examples{
major <- c('A', 'A', 'A', 'B', 'B', 'C')
minor <- c('a1','a1','a2','b1','b2','c1')
id    <- c(1,   1,   1,   1,    2,  3)
treat <- c('drug','placebo','drug','placebo','drug','placebo')
aeReport2(major, minor, treat, id, denom=c(10,10))
}
\keyword{interface}
\keyword{category}
\concept{aggregation}

