function val=FoncJ(x)
% exemple de fonction pour tester l'optimiseur
% 
c1= [-4;-4];
c2= [4;4];
c3= [6;-4];
c3= [-6;5];
c4= [-6;4];

val1= exp(-0.05*norm(x-c1)^2);
val1= sin(0.1*norm(x-c1));
val2=-exp(-0.08*norm(x-c2)^2);
val3=-0.2*exp(-0.05*norm(x-c3)^2);
val4=-1.2*exp(-0.08*norm(x-c4)^2);

val=val1+val2+val3+val4;

