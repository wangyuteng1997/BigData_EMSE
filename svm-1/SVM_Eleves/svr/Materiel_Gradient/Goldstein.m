function x=Goldstein(fonc,xk,dk)
% Méthode de Goldstein pour recherche linéaire
% foncJ: fonction J à minimiser 
% xk point d'ou part la recherche
% dk: direction de la recherche
% Gradk: gradient de fonc en x0
% phi(t)=J(xk+t*dk)

Jxk=fonc(xk);
Gradk=calgrad(fonc,xk,0.01);

derphi0=dk'*Gradk; % dérivée de phi en 0
k1=0.7;   % coeficiens de "profondeur" de descente
k2=0.3;
slope1=k1*derphi0;
slope2=k2*derphi0;

alphaprev=0;
betaprev=12;
%betaprev=5;
tfound=false;
ncoup=0;
while and(tfound==false,ncoup<10)
 ncoup=ncoup+1;
 t=(alphaprev+betaprev)/2;
 x=xk+t*dk;
 test1=fonc(x)-(Jxk+t*slope1);
 test2=fonc(x)-(Jxk+t*slope2);
 
 if test1>0
     betaprev=t;
 elseif test2<0
     alphaprev=t;
 else
     tfound=true;
 end
    
end