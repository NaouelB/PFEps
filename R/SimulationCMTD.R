#'Simulation Chaine De Markov a temps Discret
#' @export
#' @param y Vecteur espace des etats
#' @param mu Vecteur de distribution initial
#' @param p Matrice de transition
#' @param n Nombre d'etape
simulationCMTD<-function(y,mu,p,n)
{
  m=length(y)
  x<-c(rep(0,n+1));
  t<-c(seq(0:n));
  x[1]<-rdist(y,mu);
  i<-1;
  for(i in 1:n)
  {
    x[i+1]<-rdist(y,p[x[i],])
  }
  plot(t,x,main="Les états de CMTD",pch=4,xlim=c(0,n),ylim=c(0,length(mu)+1),xlab="Temps",ylab="Etats",col="blue");
  return(x)
}
