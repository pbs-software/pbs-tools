\name{predictRER}
\alias{predictRER}
\title{
  Predict Classification of Rougheye Rockfish
}
\description{
  Discriminant function for the accurate classification
  of Rougheye Rockfish (RER, \emph{Sebastes aleutianus}) and 
  Blackspotted Rockfish (BSR, \emph{Sebastes melanostictus}).
}
\usage{
predictRER(S, L, N)
}
\arguments{
  \item{S}{numeric scalar: Standard length (mm) of RER or BSR}
  \item{L}{numeric vector: Six morphometrics (lengths in mm)}
  \item{N}{numeric vector: Two meristics (observed numbers)}
}
\details{
  Exploring 35 morphometric and 9 meristic characters,
  Orr and Hawkins (2008) provide a discriminant function 
  (using only 6 morphometrics \eqn{L} and 2 meristics \eqn{N} ) 
  that claims to correctly classify the two species 97.8\%
  of the time.

  The discriminant function: \cr 
  \deqn{D  = 101.557 \lambda_1 + 52.453 \lambda_2 +  0.294 N_1 +
              51.920 \lambda_3 +  0.564 N_2 -       38.604 \lambda_4 -
              22.601 \lambda_5 - 10.203 \lambda_6 - 10.445 }{%
        D  = 101.557 \lambda1 +  52.453 \lambda2 +   0.294 N1 +
              51.920 \lambda3 +   0.564 N2 -        38.604 \lambda4 -
              22.601 \lambda5 -  10.203 \lambda6 -  10.445 }

  where, \eqn{\lambda_n = 100 L_n/S}{\lambda[n] = 100 L[n] / S} , i.e., percent Standard Length 

  \tabular{rll}{
    \eqn{S}    \tab = \tab standard fish length measured from tip of snout, \cr
    \eqn{L_1}{L[1]} \tab = \tab length of dorsal-fin spine 1, \cr
    \eqn{L_2}{L[2]} \tab = \tab snout length, \cr
    \eqn{L_3}{L[3]} \tab = \tab length of gill rakers, \cr
    \eqn{L_4}{L[4]} \tab = \tab length of pelvic-fin rays, \cr
    \eqn{L_5}{L[5]} \tab = \tab length of soft-dorsal-fin base, \cr
    \eqn{L_6}{L[6]} \tab = \tab preanal length, \cr
    \eqn{N_1}{N[1]} \tab = \tab number of gill rakers, \cr
    \eqn{N_2}{N[2]} \tab = \tab number of dorsal-fin rays.
  }
  When \eqn{D < 0}, the prediction is that the rockfish is \emph{S. aleutianus}. \cr
  When \eqn{D > 0}, the prediction is that the rockfish is \emph{S. melanostictus}.
}
\value{
  Numeric scalar representing the calculated discriminant value.
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
  \emph{locus opus}: Institute of Ocean Sciences (IOS), Sidney BC\cr
  Last modified \code{Rd: 2021-06-15}\cr
}
\seealso{
  \code{\link{simRER}}, \code{\link{simBSR}}
}
\examples{
local(envir=.PBStoolEnv,expr={
pbsfun = function(){
  test = simRER(1000)
  Dval = apply(test,1,function(x){predictRER(x[1],x[2:7],x[8:9])})
  print(paste("RER correctly predicted ",
    round((length(Dval[Dval<0])/length(Dval)*100),1),"\% of the time",sep=""))
  test = simBSR(1000)
  Dval = apply(test,1,function(x){predictRER(x[1],x[2:7],x[8:9])})
  print(paste("BSR correctly predicted ",
    round((length(Dval[Dval>0])/length(Dval)*100),1),"\% of the time",sep=""))
  invisible() }
pbsfun()
})
}
\keyword{multivariate}
\concept{M02_Biology}

