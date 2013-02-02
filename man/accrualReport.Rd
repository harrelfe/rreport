\name{accrualReport}
\alias{accrualReport}
\title{Accrual Report}
\usage{
  accrualReport(Minor, Major = character(length(Minor)),
    MajorLabel = "", MinorLabel = "", EntryDate1 = NULL,
    EntryDate2 = NULL, EntryDateLabel = "", EntryDate1cap,
    EntryDate2cap, dateRange, dateformat = "y-m-d",
    targetN, targetDate = NULL, panel = "randomized",
    hdotchart = 6, append = TRUE)
}
\arguments{
  \item{Minor}{character vector. Minor categorical variable
  for site.}

  \item{Major}{character vector. Major categorical variable
  for site.}

  \item{MajorLabel}{character.  Label for major site
  variable.}

  \item{MinorLabel}{character.  Label for minor site
  variable.}

  \item{EntryDate1}{character vector. Entry date for each
  subject.}

  \item{EntryDate2}{character vector.  Entry date for each
  subject.}

  \item{EntryDateLabel}{character. X-axis label for entry
  date plot.}

  \item{EntryDate1cap}{character. Caption for first entry
  date.}

  \item{EntryDate2cap}{character. Caption for second entry
  date.}

  \item{dateRange}{a vector of dates. Should contain two
  values specifying a date range.}

  \item{dateformat}{character. Specify date format, see
  \code{format} argument for \code{chron}.}

  \item{targetN}{numeric. Target number of subjects to
  enroll.}

  \item{targetDate}{character. Target date to end
  enrollment period.}

  \item{panel}{character. Name for subjects panel, defaults
  to \sQuote{randomized}.}

  \item{hdotchart}{numeric. Height of plot, passed to
  \code{startPlot}.}

  \item{append}{logical. If \sQuote{TRUE} output will be
  appended instead of overwritten.}
}
\value{
  return something
}
\description{
  Generate reports with patient accrual numbers.
}
\details{
  When \code{EntryDate1} and \code{dateRange} are present,
  generate Ecdf for subject accrual over the enrollment
  period. When \code{Major} is present, generate dotchart
  for number of subjects by Major. Generate dotchart for
  number of subjects by Minor. Generate plot with the count
  of site having a given number of subjects.
}
\examples{
\dontrun{
  accrualReport(sample(15, 200, replace=TRUE), MinorLabel='site',
    EntryDate1=as.Date("2005-01-01")+round(rgamma(200, 2, .01)),
    EntryDate1cap='randomized subjects', dateRange=c('2005-01-01', '2008-12-31'),
    targetN=300, targetDate='2008-12-31')
}
}

