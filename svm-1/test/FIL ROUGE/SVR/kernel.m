function kern=kernel(u,v)

%Renvoie K(u,v), noyau. U et v dans Rn (vecteurs colonnes)

global ikernel
if ikernel==1   %linéaire
    kern=u'*v;
elseif ikernel==2  %Gaussien
    kern=exp(-0.05*norm(u-v)^2);
end


