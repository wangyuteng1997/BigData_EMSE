function Cg=calgrad(nomfonc,x,epsi)
% calcul numérique du gradient 
% nonfonc: pointeur sur fonction de Rn dans R
% x: point de Rn où on veut le gradient
% epsi : parametre du calcul discret des dérivées

ndim=length(x);
val0=nomfonc(x);
for i=1:ndim
    y=x;
    y(i)=y(i)+epsi;
    Cg(i)= (nomfonc(y)-val0)/epsi;
end
Cg=Cg';