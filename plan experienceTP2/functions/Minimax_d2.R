
Minimax_d2 <- function(XX)
{
  #  Minimax_d( XX )
  # computes minimax distance criterion for points in XX (d-dimensional)
  # included in the hypercube [0,1]^d
  # XX : matrice dont les lignes sont les points x(i), i = 1,..., n
  
  
  epsi=1e-6
  
  d=nrow(XX)
  N0 = ncol(XX)
  
  
  # %%%%%%%%%%%%%%%% Test if dimension = 1 %%%%%%%%%%%%%
  #if (d==1) {return(Minimax_d1( XX, 0,1 ))}
  #  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
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
  
  plot(XX[,2]~XX[,1],xlim = c(-1,2),ylim = c(-1,2),pch = 21, col =2,bg = 2)
  abline(h = 0,lty = 2)
  abline(h = 1,lty = 2)
  abline(v = 0,lty = 2)
  abline(v = 1,lty = 2)
  
  for (j in (1:M)) {segments(XX[TRI[j,],1],XX[TRI[j,],2],XX[TRI[j,c(2,3,1)],1],XX[TRI[j,c(2,3,1)],2],lty = 3)}
  
  Centres=matrix(0,M,d)
  r2max=0
  for (i in 1:M)
  {    
    obj=Spheres(t(XX[TRI[i,],]),25)
    c =obj$c
    r2 = obj$beta2
    Centres[i,]=c
    # points(c[1],c[2])
    if (all(c>-epsi) && all(c<1+epsi)  && r2>r2max) 
    {
      r2max=r2
      ind = i
      
    } 
    
    # i=i+1
  }
  #% Centres=Centres(:,Centres(1,:)>-epsi & Centres(1,:)<1+epsi & Centres(2,:)>-epsi & Centres(2,:)<1+epsi);
  #                              % plot(Centres(1,:),Centres(2,:),'b.')
  #                              
  dmm=sqrt(r2max)
  segments(XX[TRI[ind,],1],XX[TRI[ind,],2],XX[TRI[ind,c(2,3,1)],1],XX[TRI[ind,c(2,3,1)],2],lty = 1,col =3)
  points(Centres[ind,1],Centres[ind,2],col = 3,pch = 21,bg = 3)
  #                              %center=xmax
  #                              end
  #                              
  return(dmm)
}
