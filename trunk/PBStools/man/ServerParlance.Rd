\name{ServerParlance}
\alias{ServerParlance}
\concept{server}
\concept{SQL}
\concept{Oracle}
\title{Info: Server Parlance -- SQL Server vs. Oracle}
\description{
  \pkg{PBStools} allows a user to easily query SQL Server and Oracle databases.
  At PBS, both types reside on a single server \code{SVBCPBSGFIIS}.
  The two database systems are similar but the terminology is potentially confusing. 
  This package attempts to equalize the two database systems for the end user by 
  representing the Oracle hierarchy in terms of the SQL Server system. 
  Specifically, we use the term \code{server} to refer to an Oracle database, 
  and the term \code{database} to refer to an Oracle schema.
}
\details{
  In SQL Server parlance we have:
  \tabular{rl}{
    \bold{1.} \tab A server (\code{SVBCPBSGFIIS}).\cr
    \bold{2.} \tab One or more SQL Server instances.\cr
              \tab The default instance is unnamed but if you run multiple instances they have to be named.\cr
    \bold{3.} \tab Each instance has one or more databases (e.g. \code{PacHarvest}).\cr
    \bold{4.} \tab Each database contains one or more schemas (the default schema is \code{dbo}).\cr
    \bold{5.} \tab Each schema consists of various logical objects (e.g. tables, views, procedures).
  }
  In Oracle parlance we have:
  \tabular{ll}{
    \bold{1.} \tab A server (\code{SVBCPBSGFIIS}).\cr
    \bold{2.} \tab One or more databases (e.g. \code{GFSH}).\cr
    \bold{3.} \tab Each database contains one or more schemas (e.g. \code{GFFOS}).\cr
    \bold{4.} \tab Each schema consists of various logical objects (e.g. tables, views, procedures).
  }
  In comparison, an SQL Server instance is equivalent to an Oracle database, 
  and an SQL Server database is equivalent to an Oracle schema.  
  
  SQL Server schemas are quite a bit different than Oracle schemas in that 
  they are chiefly a way of organizing objects and controlling permissions within
  a single database, whereas Oracle schemas are basically separate logical 
  structures in the same way that SQL Server databases are.
}
\author{
  Norm Olsen, Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC
}
\seealso{
  \code{\link[PBStools]{PBStools}} for package functions and data.\cr
  \code{\link[PBStools]{SQLcode}} for available SQL queries.\cr
  \code{\link[PBStools]{getData}} to execute the SQL queries.
}
\keyword{interface}
\keyword{connection}
\keyword{database}

