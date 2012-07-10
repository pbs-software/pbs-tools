\name{showMessage}
\alias{showMessage}
\alias{showError}
\title{Display a Message on the Current Device}
\description{
  Display a message (possibly error) on the current device. If \code{err==TRUE},
  stop code execution.
}
\usage{
showMessage(str, type="", as.is=FALSE, err=FALSE, ...)
}
\arguments{
  \item{str}{string indicating the message or error.}
  \item{type}{code indicating header string before the message.}
  \item{as.is}{logical: if \code{TRUE}, turn off the automatic line breaking. }
  \item{err}{logical: if \code{TRUE}, treat the message as an error message and stop code execution.}
  \item{...}{other graphical parameters to pass to \code{addLabel}.}
}
\details{
  The user-specified message will be displayed on the current device. 
  If \code{err==TRUE}, code execution will cease using \code{stop}.
  
  A wrapper function called \code{showError} is included for backwards compatibility.
}
\value{
  No value is explicitly returned, error message is displayed.
}
\author{
  Rowan Haigh, Pacific Biological Station, Nanaimo BC
}
\seealso{
  \code{\link{getData}}, \code{\link{wrapText}}
}
\keyword{device}
\keyword{utilities}
