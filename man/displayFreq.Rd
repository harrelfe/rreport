\name{displayFreq}
\alias{displayFreq}
\title{Display Adverse Event Frequencies}
\usage{
  displayFreq(dataframe, subjectVar, minorVar, majorVar,
    occurrenceVar, stratVar, denomSub = NULL,
    fileName = NULL, labelLen = 10, pvalue = 0.05,
    keepPvalue = 0.5, minDisplayNum = 2, majorGrid = NULL,
    minorGrid = NULL, plotGrid = TRUE, gridDig = 0,
    titleOffsetKoef = 10, titleOffsetX = 0,
    minorToMajorKoef = 5, sparseKoef = 1.5, graphWidth = 8,
    graphHeight = 11, gridCex = 0.5, labelCex = 0.5,
    titles = NULL)
}
\arguments{
  \item{dataframe}{data.frame. Data with adverse events.}

  \item{subjectVar}{character. Variable classified to major
  and minor category (subject ID).}

  \item{minorVar}{character. Name of minor category
  variable within dataset. (i.e. specific adverse event)}

  \item{majorVar}{character. Name of major category
  variable within dataset. (i.e. body system adverse event
  belongs to)}

  \item{occurrenceVar}{character. Name of occurrence
  variable within dataset. It indicates the different
  occurrences of a minor category for a given
  \code{subjectVar} (date or order in which event
  happened). This variable is assumed to be unique for
  given subject and given \code{minorVar}.}

  \item{stratVar}{character. Name of stratification
  variable within dataset. (i.e. treatment)}

  \item{denomSub}{numeric vector. Contains the number of
  unique subjects in each \code{stratVar} level. It should
  have the same names and length as
  \code{levels(stratVar)}. Defaults to \sQuote{NULL}, where
  values will be calculated accordingly from the dataset.}

  \item{fileName}{character. Name of output file, defaults
  to \sQuote{NULL}.}

  \item{labelLen}{numeric. Maximum length of grid labels.}

  \item{pvalue}{numeric. If for a given major and minor
  category, the proportion test gives a p-value less than
  \code{pvalue}, then this category will be highlighted.
  Defaults to \sQuote{0.5}.}

  \item{keepPvalue}{numeric. Only categories with a p-value
  (according to the proportion test) less than
  \code{keepPvalue} will be displayed.}

  \item{minDisplayNum}{numeric. Only categories with a
  total frequency more than \code{minDisplayNum} will be
  displayed. Defaults to \sQuote{2}.}

  \item{majorGrid}{numeric vector. Grid of major category.}

  \item{minorGrid}{numeric vector. Grid of minor category.}

  \item{plotGrid}{logical. Set to \sQuote{TRUE} to plot
  grid lines.}

  \item{gridDig}{numeric. Set the number of digits used to
  round the grid digits.}

  \item{titleOffsetKoef}{numeric. The vertical distance the
  title should stay from the graph.}

  \item{titleOffsetX}{numeric. The horizontal distance the
  title should stay from the graph.}

  \item{minorToMajorKoef}{numeric. The distance the bars
  for the minor category should be longer than the bars for
  the major category.}

  \item{sparseKoef}{numeric. How farther away the graphs
  for different \code{stratVar} values should be located on
  the diagram.}

  \item{graphWidth}{numeric. Width of plot, the default is
  \sQuote{8}.}

  \item{graphHeight}{numeric. Height of plot, the default
  is \sQuote{11}.}

  \item{gridCex}{numeric. Relative size of the grid
  digits.}

  \item{labelCex}{numeric. Relative size of the labels.}

  \item{titles}{named character vector. Titles for each
  frequency display. It's names should be levels of
  treatment.}
}
\description{
  summary
}

