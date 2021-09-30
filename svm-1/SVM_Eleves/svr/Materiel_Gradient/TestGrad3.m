% méthode du gradient à pas optimal sur un cas non quadratique

clear all
close all

FoncCout=@Surface;
FoncCout=@NFoncJ;
ndim=2;
% regler les bornes du dessin
  X=-6:0.4:6;    % pour la Loire
  Y=-6:0.4:10;
  X=-10:0.4:10;    % NFoncJ
  Y=-10:0.4:10;
  
  
  ndesx=length(X);
  ndesy=length(Y)
  for i=1:ndesx
    for j=1:ndesy
    P=[X(i);Y(j)];
    Z(i,j)=FoncCout(P);
    MX(i,j)=X(i);
    MY(i,j)=Y(j);
    end
  end
    figure(2)
     surf(MX,MY,Z)
     figure (1)
     contour(MX,MY,Z,40)
  hold on
  %DessinLoire   % pour la loire
  
  axis equal
% méthode du gradient 
methode='simplex';
epsi=1e-3;
arret=100;
if ndim==2
  Xav=ginput (1);
  Xav=Xav';
  plot(Xav(1),Xav(2),'or','markersize',14)

  hold on
else
    Xav=ones(ndim,1);
end
niter=0;
 while arret>epsi
     niter=niter+1;
     grad=calgrad(FoncCout,Xav,1e-3);
     if or(mod(niter,5)==1,strcmp(methode,'simple'))
        dir=-grad;
     else
         beta=grad'*(grad-gradav)/(gradav'*gradav);
         dir=-grad + beta*dir;
     end

     Xnext=Goldstein(FoncCout,Xav,dir);
     arret=norm(Xav-Xnext);
     %arret=norm(grad);
     Xav=Xnext;
     gradav=grad;
     if ndim==2
       plot(Xnext(1),Xav(2),'o','markersize',14)
       hold on
       pause(0.1)
     end
  
 end
 
 Xsol=Xnext
 Jsol=FoncCout(Xsol)
 niter
    