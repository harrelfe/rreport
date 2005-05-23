\name{aeReport2}
\alias{aeReport2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Report Summarizing All Adverse Events}
\description{
This report uses a data format in which records exist only for events,
and these events are classified as \code{major} and \code{minor}
events.  Typically \code{major} refers to organ system and \code{minor}
to preferred term.  As only events are signaled in the data, the
denominator (number of subjects at risk and having some likelihood of
having had adverse events collected had they occured) for each treatment
must be specified.
}
\usage{
aeReport2(major, minor, treat, id, denom, bycaption,
size = NULL, longtable = FALSE, lines.page = 50, append = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{major}{ ~~Describe \code{major} here~~ }
  \item{minor}{ ~~Describe \code{minor} here~~ }
  \item{treat}{ ~~Describe \code{treat} here~~ }
  \item{id}{ ~~Describe \code{id} here~~ }
  \item{denom}{ ~~Describe \code{denom} here~~ }
  \item{bycaption}{text string describing how events are coded}
  \item{size}{ ~~Describe \code{size} here~~ }
  \item{longtable}{ ~~Describe \code{longtable} here~~ }
  \item{lines.page}{ ~~Describe \code{lines.page} here~~ }
  \item{append}{ ~~Describe \code{append} here~~ }
}
\details{
  ~~ If necessary, more details than the __description__  above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ }

 ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ ~~objects to See Also as \code{\link{~~fun~~}}, ~~~ }
\examples{
major <- c('A', 'A', 'A', 'B', 'B', 'C')
minor <- c('a1','a1','a2','b1','b2','c1')
id    <- c(1,   1,   1,   1,    2,  3)
treat <- c('drug','placebo','drug','placebo','drug','placebo')
aeReport2(major, minor, treat, id, denom=c(10,10))
}
\keyword{ ~kwd1 }% at least one, from doc/KEYWORDS
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
