\name{genPa}
\alias{genPa}
\title{
  Generate Proportions-At-Age from Catch Curve Composition
}
\description{
  Generate proportions-at-age using the catch curve composition of \cr
  Schnute and Haigh (2007, Table 2).
}
\usage{
genPa( np = 40, 
   theta=list(Z=0.1,mu=15,sigma=5,bh=c(10,20),rho=c(3,2),tau=1.5),
   sim = TRUE)
}
\arguments{
  \item{np}{number of age classes \eqn{B}.}
  \item{theta}{list of parameters 
    \eqn{\theta=(Z; \; \mu,\sigma; \; b_1,...b_m, \rho_1,...\rho_m, \tau)}{\theta=(Z; \mu,\sigma; b[1],...b[m], \rho[1],...\rho[m], \tau)}: \cr
    \code{Z.......} total mortality (\eqn{Z = F + M}). \cr
    \code{mu......} age \eqn{\mu} of full selectivity. \cr
    \code{sigma...} variance parameter (rendered as the standard deviation \eqn{\sigma}) \cr
    \code{........} for the selectivity curve (left limb of a double normal distribution). \cr
    \code{bh......} ages \eqn{b_h}{b[h]} with anomalous recruitment, \cr
    \code{........} where \eqn{h = 1,...,m} and \eqn{m} = number of recruitment anomalies. \cr
    \code{rho.....} recruitment anomaly parameter \eqn{\rho_h}{\rho[h]} at age \eqn{b_h}{b[h]}. \cr
    \code{tau.....} standard deviation \eqn{\tau} for recruitment anomalies. }
  \item{sim}{logical: if \code{TRUE}, return components of the simulated proportions-at age.}
}
\details{
  Schnute and Haigh (2007) provide a catch curve model in Table 2 (p.219). 
  Their model uses a design vector \eqn{\phi} and a parameter vector \eqn{\theta};
  the latter contains the quantities to be estimated in a negative log likelihood using the complete 
  parameter vector \eqn{(\theta, \sigma)} for a logistic-normal model or 
  \eqn{(\theta, n)} for a Dirichlet model.
  
  Here we use \eqn{\theta} primarily as a parameter vector, but include the design elements
  \eqn{b_1,...b_m}{b[1],...b[m]} for expediency.
  
  Additionally, for the selectivity component, we use the parameterisation of Edwards et al. (2010)
  where \eqn{\mu} represents the age of full selectivity and \eqn{\sigma = \sqrt\nu} represents the 
  variance of the left limb of a double normal (see equation F.7 in the Pacific Ocean Perch assessment).
  This parameretisation replaces (T2.2) in Schnute and Haigh (2007), which uses \eqn{\alpha} and \eqn{\beta_k}{\beta[k]}.
}
\value{
  Proportions-at-age vector of length \code{np}. \cr
  If \code{sim=TRUE}, the components \eqn{S_a}{S[a]}, \eqn{\beta_a}{\beta[a]}, \eqn{R_a}{R[a]}, 
  and \eqn{p_a}{p[a]} are writtten to the global list object \code{PBSfish}.
}
\references{
  Edwards, A.M., Starr, P.J., and Haigh, R. (2010, under revision) 
  Stock assessment for Pacific Ocean Perch (\emph{Sebastes alutus}) in Queen Charlotte Sound, British Columbia. 
  \emph{Canadian Science Advisory Secretariat, Research Document} \bold{2010/xxx}: iii + nnn p.

  Schnute, J.T., and Haigh, R. (2007) 
  Compositional analysis of catch curve data, with an application to \emph{Sebastes maliger}. 
  \emph{ICES Journal of Marine Science} \bold{64}: 218--233.
}
\author{
  Rowan Haigh, Pacific Biological Station, Fisheries and Oceans Canada, Nanaimo BC.
}
\seealso{
  \code{\link[PBStools]{scaleVec}}, \code{\link[PBStools]{sampBG}}
}
\examples{
pbsfun=function(){
  genPa(); unpackList(ttget(PBSfish)); unpackList(theta)
  par0 = par(no.readonly=TRUE); on.exit(par(par0))
  expandGraph(mfrow=c(2,2),mar=c(1,4,1,0.5),oma=c(3,0,0,0),mgp=c(2,.5,0))
  for (i in c("Sa","Ba","Ra","pa")) {
    expr = paste(
      c("plot(1:np,",i,",type=\"l\",lwd=2,col=\"blue\",xlab=\"\",ylab=\"\");",
      "mtext(expression(italic(",substring(i,1,1),
      "[a])),side=2,line=2,cex=1.5,las=1);",
      "points(1:np,",i,",pch=21,bg=\"aliceblue\",cex=1.2)"),collapse="")
    eval(parse(text=expr)) 
    if (i=="Ba") lines(rep(mu,2),c(0.99,par()$usr[3]),lty=2,col="grey30")
    if (i=="Ra") for (j in 1:length(rho))
      lines(rep(bh[j],2),c(rho[j]+0.98,par()$usr[3]),lty=2,col="grey30")
    if (i=="pa") lines(1:np,scaleVec(Sa,pa[np],max(pa)),lty=2,col="grey30") }
  mtext("     Age",outer=TRUE,side=1,line=1.5,cex=1.5)
  invisible() }
pbsfun()
}
\keyword{datagen}

