\name{getODBC}
\alias{getODBC}
\title{
  Get ODBC Drivers on Windows System
}
\description{
  Get a string vector of ODBC drivers on a user's Windows system.
}
\usage{
getODBC(os=.Platform$OS.type, pattern=NULL, status="Installed")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{os}{operating system (e.g., \code{"windows"}, \code{"unix"}).}
  \item{pattern}{string pattern used to filter the ODBC drivers.}
  \item{status}{string denoting status of driver (e.g., "Installed"); set to \code{NULL} for all drivers.}
}
\details{
  Creates a VB shell script file called \code{getODBC.vbs}
  in R's temporary working directory, then executes the script using the \code{shell} command.

  To quote the shell script author: \cr
  \emph{The registry, as you doubtless know, is the upstairs closet of the operating system: 
  if you're willing to look, you can find almost anything in the registry. 
  And, sure enough, that's where we found the list of installed ODBC drivers, in} \cr 
  \code{HKEY_LOCAL_MACHINE\\SOFTWARE\\ODBC\\ODBCINST.INI\\ODBC Drivers}.
  
  \emph{As soon as we knew we could get this information out of the registry it was easy to 
  write a script that retrieved a list of all the installed ODBC drivers:}

  \preformatted{
Const HKEY_LOCAL_MACHINE = &H80000002
strComputer = "."
Set objRegistry = GetObject("winmgmts:\\\\" & strComputer & "\\root\\default:StdRegProv")
strKeyPath = "SOFTWARE\\ODBC\\ODBCINST.INI\\ODBC Drivers"
objRegistry.EnumValues HKEY_LOCAL_MACHINE,strKeyPath,arrValueNames,arrValueTypes
For i = 0 to UBound(arrValueNames)
   strValueName = arrValueNames(i)
   objRegistry.GetStringValue HKEY_LOCAL_MACHINE,strKeyPath,strValueName,strValue
   Wscript.Echo arrValueNames(i) & " -- " & strValue
Next
}
  These commands will only work on Windows operating platforms, using the system's executable \cr
  \code{\%SystemRoot\%\\system32\\cscript.exe}.
}
\value{
  A character vector of strings that reports the ODBC drivers installed on the user's Windows system.
}
\references{
  \href{https://devblogs.microsoft.com/scripting/how-can-i-get-a-list-of-the-odbc-drivers-that-are-installed-on-a-computer/}{ScriptingGuy1}
  (July 7, 2005), adapted for use in \code{R} by RH.\cr
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Institute of Ocean Sciences (IOS), Sidney BC\cr
  Last modified \code{Rd: 2020-10-13}
}
\seealso{
  \code{\link[PBSmodelling]{clearRcon}}
}
\keyword{programming}
\keyword{IO}

