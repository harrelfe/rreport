\name{listTable}
\alias{listTable}
\title{List Table}
\usage{
  listTable(fileName, longtable = TRUE, landscape = FALSE,
    caption = "", fontSize = "small", dataframe,
    zebraPattern = "none", by = names(dataframe)[1],
    orderGroups = FALSE, colNames = names(dataframe),
    vars = names(dataframe), fixedColVars = c(),
    fixedColWdths = c(), markVar = "", markVarVal = "",
    toLatexChar = TRUE, appendix = TRUE, subsection = NULL,
    marker = NULL, append = FALSE)
}
\arguments{
  \item{fileName}{character. A description of the file
  connection.}

  \item{longtable}{logical. Toggle \sQuote{longtable} or
  \sQuote{table} environment. Defaults to \sQuote{TRUE}.}

  \item{landscape}{logical. Use \sQuote{landscape}
  environment. Defaults to \sQuote{FALSE}.}

  \item{caption}{character. Main table caption.}

  \item{fontSize}{character. Define font size from one of
  the following: \sQuote{tiny}, \sQuote{scriptsize},
  \sQuote{footnotesize}, \sQuote{small},
  \sQuote{normalsize}, \sQuote{large}, \sQuote{Large},
  \sQuote{LARGE}, \sQuote{huge}, \sQuote{Huge}.  Default is
  \sQuote{small}.}

  \item{dataframe}{data.frame. Provides content for list
  table.}

  \item{zebraPattern}{character. Defaults to \sQuote{none},
  other options are \sQuote{plain}, \sQuote{group} and
  \sQuote{plaingroup.} \sQuote{plaingroup} is only
  recommended for large groups (more than four objects in a
  group).}

  \item{by}{character. Column used to generate zebra
  pattern, defaulting to the first column in
  \sQuote{dataframe}.}

  \item{orderGroups}{logical. If \sQuote{TRUE} order the
  data by \sQuote{by}.  This is recommened when
  \sQuote{zebraPattern} is set to \sQuote{group} or
  \sQuote{plaingroup}.}

  \item{colNames}{character vector. Define column name
  headers for \sQuote{dataframe}.}

  \item{vars}{character vector. Column names to select from
  \sQuote{dataframe}.}

  \item{fixedColVars}{character vector. Column variables.}

  \item{fixedColWdths}{character vector. Fixed width for
  each column.}

  \item{markVar}{character vector. Marker variable.}

  \item{markVarVal}{character vector. Value for each marker
  variable.}

  \item{toLatexChar}{logical. If \sQuote{TRUE} text will be
  checked and escaped should they contain special LaTeX
  characters.}

  \item{appendix}{logical. If \sQuote{TRUE} the function
  will require the \code{subsection} and \code{marker}
  arguments since it will have to reference the tables in
  the future.}

  \item{subsection}{character. Name of document subsection
  that refers to this table.}

  \item{marker}{character. Marker for document subsection
  that refers to this table.}

  \item{append}{logical. If \sQuote{TRUE} output will be
  appended instead of overwritten.}
}
\description{
  Convert a data.frame object into a LaTeX table.
}
\examples{
listTable(fileName='', caption="\\\\label{table:listtable}Table of groupings", zebraPattern='group',
dataframe=data.frame(code=c('(a)', '(b)', '(c)'), def=c("apple, orange", "dog, cat, horse", "windows, linux, mac")),
appendix=FALSE) # print generated table to standard out
}

