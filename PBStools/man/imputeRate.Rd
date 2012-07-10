\name{imputeRate}
\alias{imputeRate}
\title{Impute Rate of Return for an Investment}
\description{
  Impute the rate of return for an investment that experiences
  regular or irregular contributions and/or withdrawals.
}
\usage{
imputeRate(qtName="Ex03_Portfolio", dbName="Examples", 
     AID=1, pathN=2, hnam=NULL) 
}
\arguments{
  \item{qtName}{Name of query or table in a Microsoft ACCESS file (\code{.mdb}).}
  \item{dbName}{Name of the Microsoft ACCESS file (\code{.mdb}).}
  \item{AID}{Numeric specifying account ID.}
  \item{pathN}{Numeric specifying path: \code{1} = current working directory,
    \code{2} = SQL directory \cr \code{.../library/PBStools/sql}.}
  \item{hnam}{Name of a history file.}
}
\details{
  This function creates an interactive GUI that can be used
  to impute the rate of return for an investment account or 
  for a simulated investment. The code adopts the formula for 
  the \dQuote{internal rate of return} used in Microsoft Excel.

  The input data must contain the fields: \cr
  \code{AID.....}Account ID number; \cr
  \code{date....}Date of the account valuation; \cr
  \code{value...}Value of the account as of \code{date}; \cr
  \code{cont....}Total contributions/withdrawals from the previous date 
                 up to and including the current date. \cr

  \bold{The GUI controls:}
  \tabular{ll}{
  \code{Data}         \tab Open the \code{.mdb} database.\cr
  \code{R code}       \tab View the function \code{imputeRate}.\cr
  \code{Window}       \tab View the \emph{window description file}.\cr
  \code{MDB}          \tab Microsoft Access database name (no extesion).\cr
  \code{>}            \tab Displays available \code{.mdb} files on the specified path (below).\cr
  \code{Table}        \tab Table or query that contains account information.\cr
  \code{>}            \tab Displays tables and queries in MDB, choose one.\cr
  \code{GET}          \tab Get the data from the chosen table.\cr
  \code{MDB path}     \tab Choice of \code{cwd} (current working directory) or \code{sql} (package SQL directory).\cr
  \bold{Inputs}       \tab \cr
  \code{Account #}    \tab Account ID number.\cr
  \code{parVec}       \tab Data frame specifying \code{val}, \code{min}, \code{max}, \code{active} for parameter \code{rate}.\cr
  \code{autoD}        \tab If \code{TRUE}, get the date limits of the account automatically.\cr
  \code{Start date}   \tab Starting date of the investment time series to calculate \code{rate}.\cr
  \code{End date}     \tab Ending date of the investment time series to calculate \code{rate}.\cr
  \code{period}       \tab Periods in data source corresponding to the date limits specified.\cr
  \bold{Estimation}   \tab \cr
  \code{Method}       \tab Choose one of various non-linear estimation routines.\cr
  \code{Controls}     \tab Various controls used by \code{calcMin}.\cr
  \code{Reset}        \tab Button resets \code{parVec} and output boxes (below).\cr
  \code{ReInit}       \tab Button sets \code{parVec}'s \code{val} to the last estimated \code{prate} (below).\cr
  \code{RUN}          \tab Runs the rate estimation for the investment interval chosen.\cr
  \bold{Simulate}     \tab \cr
  \code{No Yes Again} \tab Choose to simulate investment data using a random pareto distribution.\cr
  \code{start}        \tab Starting value of the investment.\cr
  \code{rate}         \tab True rate of return per period.\cr
  \code{nper}         \tab Number of periods to run the simulation.\cr
  \code{up}           \tab Proportion of the time that a contribution is made vs. a withdrawal.\cr
  \code{k}            \tab Pareto distribution parameter (volatility decreases as k increases).\cr
  \bold{Outputs}      \tab \cr
  \code{decimal places} \tab Number of decimal places for display output.\cr
  \code{Iters Evals}  \tab Number of iterations and evaluations for the estimation.\cr
  \code{prate arate}  \tab Starting period rate and annualised rate.\cr
  \code{Ctime Etime}  \tab Computer and evaluation times (seconds).\cr
  \code{prate arate}  \tab Estimated period rate and annualised rate.\cr
  \code{Fmin0 Fmin}   \tab Initial function evaluation and the final function value at minimization.\cr
  \code{AIC AICc}     \tab Aikike Information Criterion (model fit) and corrected AIC.\cr
  \code{message}      \tab Message box area reporting results of the minimization.\cr
  }
}
\seealso{
  \code{\link[PBStools]{calcMA}}, \code{\link[PBStools]{glimmer}}, \code{\link[PBStools]{trend}}
}
\keyword{hplot}
\keyword{optimize}
