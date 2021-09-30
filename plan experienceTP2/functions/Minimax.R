source('Spheres.R')
source('Minimax_d1.R')
source('Minimax_d2.R')

Minimax <- function(XX)
{
  #  Minimax_d( XX )
  # computes minimax distance criterion for points in XX (d-dimensional)
  # included in the hypercube [0,1]^d
  # XX : matrice dont les lignes sont les points x(i), i = 1,..., n
  XX = t(XX)
  epsi=1e-6
  
  d=nrow(XX)

  N0 = ncol(XX)
  
  
  # %%%%%%%%%%%%%%%% Test if dimension = 1 %%%%%%%%%%%%%
  if (d==1) {return(Minimax_d1( XX, 0,1 ))}
  #  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  if (d==2) {return(Minimax_d2( XX ))}
  
  # compute and include symmetric samples % (d-1)-faces 
  
  for (i in 1:N0) {
    Xtst=XX[,i] # ith point in XX
    for (j in 1:d) {
      for (k in seq(0,2,2))
      {
        X=Xtst 
        X[j]=k-X[j] # symmetry wrt one of the limiting hyperplanes of the cube
        XX=cbind(XX,X)
      }
    }
  }
  
  
  # include almost all symmetric points (not all coordinates should be changed)
  #[ XX ] = All_sym_mat_holes( XX0 );
  
  # include all symmetric points !
  #[ XX ] = All_sym_mat( XX0 );
  
  
  # Matlab Delaunay
  XX= unique(t(XX),margin =1) 
  
  TRI=delaunayn(XX)
  
  p = ncol(TRI)
  M = nrow(TRI)
    
  #for (j in (1:M)) {segments(XX[TRI[j,],1],XX[TRI[j,],2],XX[TRI[j,c(2,3,1)],1],XX[TRI[j,c(2,3,1)],2],lty = 3)}
  
  Centres=matrix(0,M,d)
  r2max=0
  for (i in 1:M)
  {
    obj=Spheres(t(XX[TRI[i,],]),25)
    c =obj$c
    Centres[i,]=c
    r2 = obj$beta2
    if (all(c>-epsi) && all(c<1+epsi)  && r2>r2max)
    {
      r2max=r2
      ind = i
    } 
  }
  #% Centres=Centres(:,Centres(1,:)>-epsi & Centres(1,:)<1+epsi & Centres(2,:)>-epsi & Centres(2,:)<1+epsi);
  #                              % plot(Centres(1,:),Centres(2,:),'b.')
  #                              
  dmm=sqrt(r2max)
  #                              %center=xmax
  #                              end
  #                              
  return(dmm)
}

