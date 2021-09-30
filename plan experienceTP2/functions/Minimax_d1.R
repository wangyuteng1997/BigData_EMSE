Minimax_d1 <- function(X,a,b)
{
  if (min(X)<min(a,b) || max(X)>max(a,b)) {stop('Les bornes sont mal choisies')}
  if (a<b) { X_bis = c(2*a-min(X),sort(X),2*b-max(X))} else {X_bis = c(2*b-min(X),sort(X),2*a-max(X))}
  
  D =diag(as.matrix(dist(X_bis))[2:length(X_bis),1:length(X_bis)])
  return(max(D)/2)
}