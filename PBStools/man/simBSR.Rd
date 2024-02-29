\name{simBSR}
\alias{simBSR}
\title{
  Simulate Blackspotted Rockfish Biological Data
}
\description{
  Simulate biological data that best characterizes Blackspotted Rockfish
  (BSR, \emph{Sebastes melanostictus}), using parameter estimates from 
  Table 2 in Orr and Hawkins (2008).
}
\usage{
  simBSR(Nfish)
}
\arguments{
  \item{Nfish}{Number of BSR fish to simulate.}
}
\details{
  Exploring 35 morphometric and 9 meristic characters,
  Orr and Hawkins (2008) provide a discriminant function 
  (using only 6 morphometrics \eqn{L} and 2 meristics \eqn{N} ) 
  that claims to correctly classify \emph{Sebastes melanostictus} and 
  \emph{S. aleutianus} 97.8\% of the time (see \code{\link{predictRER}}).

  Table 2 in Orr and Hawkins (2008) provides a range of BSR
  standard lengths (mm) and distributions for morphometrics
  and meristics. This function samples from a random uniform distribution
  for \eqn{S} and from random normal distributions for model inputs of 
  \eqn{\lambda} and \eqn{N} -- table below gives \eqn{(\mu , \sigma)}.

  \tabular{rllc}{
    \eqn{S}  \tab = \tab standard fish length measured from tip of snout \tab 95.5--539 \cr
    \eqn{\lambda_1}{\lambda[1]} \tab = \tab length of dorsal-fin spine 1    \tab (7.8, 0.7) \cr
    \eqn{\lambda_2}{\lambda[2]} \tab = \tab snout length                    \tab (8.0, 0.6) \cr
    \eqn{\lambda_3}{\lambda[3]} \tab = \tab length of gill rakers           \tab (5.6, 0.6) \cr
    \eqn{\lambda_4}{\lambda[4]} \tab = \tab length of pelvic-fin rays       \tab (21.4, 1.2) \cr
    \eqn{\lambda_5}{\lambda[5]} \tab = \tab length of soft-dorsal-fin base  \tab (21.4, 1.5) \cr
    \eqn{\lambda_6}{\lambda[6]} \tab = \tab preanal length                  \tab (70.2, 2.6) \cr
    \eqn{N_1}{N[1]}             \tab = \tab number of gill rakers           \tab (33.0, 1.2) \cr
    \eqn{N_2}{N[2]}             \tab = \tab number of dorsal-fin rays       \tab (13.7, 0.5)
  }
  where, \eqn{\lambda_n = 100 L_n/S}{\lambda[n] = 100 L[n]/S} , i.e., percent Standard Length 
}
\value{
  A numeric matrix with dimensions \code{c(Nfish,9)} where columns are labelled 
  \code{c('S', 'L1', 'L2', 'L3', 'L4', 'L5', 'L6', 'N1', 'N2')}. 
  The values are described above in \bold{Details}, but generally, 
  S = standard length of the fish (mm), L = six diagnostic length measurements (mm),
  and N = numbers of gill rakers and dorsal fin rays.
}
\references{
  Orr, J.W. and Hawkins, S. (2008) Species of the rougheye rockfish complex:
  resurrection of \emph{Sebastes melanostictus} (Matsubara, 1934) and a 
  redescription of \emph{Sebastes aleutianus} (Jordan and Evermann, 1898) 
  (Teleostei: Scorpaeniformes). \emph{Fisheries Bulletin} \bold{106}: 111--134.
}
\author{
  \href{mailto:rowan.haigh@dfo-mpo.gc.ca}{Rowan Haigh}, Program Head -- Offshore Rockfish\cr
  Pacific Biological Station (PBS), Fisheries & Oceans Canada (DFO), Nanaimo BC\cr
  \emph{locus opus}: Remote office, Vancouver BC\cr
  Last modified \code{Rd: 2021-06-15}\cr
}
\seealso{
  \code{\link{simRER}}, \code{\link{predictRER}}
}
\keyword{distribution}
\concept{M02_Biology}

