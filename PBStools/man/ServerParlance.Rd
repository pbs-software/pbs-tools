\name{ServerParlance}
\alias{ServerParlance}
\concept{server}
\concept{SQL}
\concept{Oracle}
\title{Info: Server Parlance -- SQL Server vs. Oracle}
\description{
  \pkg{PBStools} allows a user to easily query SQL Server and Oracle databases
  (although the latter have been avoided like the plague).
  At PBS, both types reside on a single server \code{DFBCV9TWVASP003} (or \code{10.114.52.20}).
  And according to Wizard Faris, the `instance' is named \code{SQL2016STD}.
  Therefore, mortals need to use the combination \code{DFBCV9TWVASP003\\\\SQL2016STD}
  (or \code{10.114.52.20\\\\SQL2016STD}) to enter the magic realm where data are guarded by dragons.
  
  The two database systems (SQL Server & Oracle) are similar but the terminology is potentially confusing. 
  This package attempts to equalize the two database systems for the end user by 
  representing the Oracle hierarchy in terms of the SQL Server system. 
  Specifically, we use the term \code{server} to refer to an Oracle database, 
  and the term \code{database} to refer to an Oracle schema.
}
\details{
\bold{SQL Server Terminology}\cr
In SQL Server, the hierarchy is typically organized as follows:
\enumerate{
  \item \strong{Server} \cr
  For example: \code{DFBCV9TWVASP003}.
  \item \strong{SQL Server Instance(s)} \cr
  A server can host one or more instances.
  \itemize{
    \item The default instance is unnamed.
    \item Additional instances must be named (e.g., \code{SQL2016STD}).
  }
  \item \strong{Databases} \cr
  Each instance contains one or more databases (e.g., \code{GFBioSQL}).
  \item \strong{Schemas} \cr
  Each database contains one or more schemas.
  \itemize{
    \item The default schema is typically \code{dbo}.
  }
  \item \strong{Objects} \cr
  Each schema contains logical objects such as tables, views, and stored procedures.
}
\bold{Oracle Terminology}\cr
In Oracle, the structure is organized as follows:
\enumerate{
  \item \strong{Server} \cr
  For example: \code{DFBCV9TWVASP003}.
  \item \strong{Databases} \cr
  A server hosts one or more Oracle databases (e.g., \code{GFSH}).
  \item \strong{Schemas} \cr
  Each database contains one or more schemas (e.g., \code{GFFOS}).
  \itemize{
    \item In Oracle, a schema is typically associated with a user.
  }
  \item \strong{Objects} \cr
  Each schema contains logical objects such as tables, views, and procedures.
}
\bold{Key Comparisons}\cr
The following mappings provide a rough equivalence between SQL Server and Oracle:
\itemize{
  \item SQL Server \strong{Instance} \eqn{\sim}{~} Oracle \strong{Database}
  \item SQL Server \strong{Database} \eqn{\sim}{~} Oracle \strong{Schema}
}
\bold{Important Distinctions}\cr
\itemize{
  \item In \strong{SQL Server}, schemas are primarily used to organize objects
  and control permissions within a single database.
  \item In \strong{Oracle}, schemas act as distinct logical containers for objects,
  and are more comparable to independent databases in SQL Server.
  \item SQL Server uses a three-level structure (instance \eqn{\rightarrow}{->} database \eqn{\rightarrow}{->} schema),
  whereas Oracle relies more directly on the database \eqn{\rightarrow}{->} schema structure.
}
}
\note{
  The orignal parlance was written a long time ago in a galaxy far, far away.
  I asked my new best friend, Copilot, to reword and reorganize the Details section
  and to verify that the blather was not gibberish.
}
\author{
  \href{https://copilot.microsoft.com}{Microsoft Copilot} -- version 2605\cr
  M365 Copilot, based on the GPT-5 chat model architecture, June 2026

  \href{mailto:norm.olsen@dfo-mpo.gc.ca}{Norm Olsen}, Program Head -- Surveys (retired)\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  Last modified \code{Rd: 2026-06-09}
}
\seealso{
In package \pkg{PBStools}:\cr
  \code{\link[PBStools]{PBStools-package}} for package functions and data.\cr
  \code{\link[PBStools]{SQLcode}} for available SQL queries.\cr
  \code{\link[PBStools]{getData}} to execute the SQL queries.
}
\keyword{interface}
\keyword{connection}
\keyword{database}

