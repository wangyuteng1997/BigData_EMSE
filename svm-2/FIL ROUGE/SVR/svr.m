% S�parateurs � Vaste Marge pour r�gression

clear all
close all
global ikernel  % type de noyau choisi. 1: produit scalaire dans Rp
                                       % 2: noyau gaussien
                                       
% Les parametres des SVR 
 epsilon=0.5;           % demi largeur de la bande
 ikernel=2;             % choix du type de noyau
 C_ecart= 50;           % coef pour l'�cart permis 
 
 
%donn�es X et Y Les points Xi Yi pour la regression
% Xi est  une colonne de Rn ( fonction cherch�e de Rn dans R)
% Les Xi sont stock�s dans X 
% Les Y sont stock�s en ligne dans Y 

load antennes_2d_train  C  S  % Dans le fichier antenne, C contient X' et 
                              % S contient Y'
X=C';
Y=S';
npoints=size(X,2);    % nombre de points d'apprentissage

% dessin des points en 3D ( � modifier si essais avex X dans R   
plot3(X(1,:),X(2,:),Y,'o')
hold on

%
% -----------------------------------------------------------
% Partie que les �l�ves doivent programmer

% M�thode UZAWA gradient � pas constant pour probl�me dual
na=2*npoints; % nombre de multiplicateurs alpha 
u1=epsilon*ones(na,1);  % vecteurs colonne
u2=[Y'; -Y'];
u3=[ones(npoints,1); -ones(npoints,1)];

% assemblage matrice de la forme quadratique du probl�me dual, A

for i=1:npoints
    for j=1:npoints
        B(i,j) = kernel(X(:,i),X(:,j));
    end
end

A = [B,-B;-B,B];


% A faire....

% quelques initialisations pour la boucle de gradient
alpha0=2.5*abs(rand(na,1)); % point de d�part (0, ou autre): r�glable
pasgrad=1e-2;         % pas du gradient : parametre r�glable
crit_arret=1;         % initialisation critere d'arret
npas=0;               % comptage du nombre de pas
npas_max=100000;      % garde-fou convergence
epsi=1e-4;            % seuil convergence


while and(crit_arret>epsi,npas<npas_max)% boucle de gradient
    npas=npas+1;
    % gradient pas constant 
    % projection d' ABORD sur l'hyperplan
    alpha = alpha0+pasgrad*(-A*alpha0-u1+u2);
    % projection sur  alpha > 0
    alpha = alpha - (u3*(alpha'*u3)/((u3'*u3)));
    % projection sur  alpha<C
    alpha = max(0,alpha);
    alpha = min(alpha,C_ecart);

    % stockage dans vit_conv de la distance entre deux alphas successifs par
    % exemple (donne une id�e de la vitesse � laquelle la suite des alpha se
    % tasse (crit�re de Cauchy)
    crit_arret = norm(alpha-alpha0);
    
    vit_conv(npas) = crit_arret;
    
    alpha0 = alpha;
end

fprintf(' nb iterations: %d crit�re: %e ',npas,crit_arret)

Points_support=find(alpha>epsi)';

pause
% on ne se sert pas directement de w, mais de son produit scalaire 
% avec un vecteur, defini par un noyeau : fonction prodw

% Calcul de b (on le calcule pour chaque pt support, puis on moyenne)
% on devrait v�rifier que chaque pt support fournit le m�me b (aux erreurs
% num�riques pr�s, c'est pouquoi on prend la moyenne)
nb_sup=0;
moy_b=[];
for i=Points_support
    if abs(alpha(i))-C_ecart>epsi
        n_sup=nb_sup+1;
        
        if i>npoints
            k=i-npoints;
            proscal=prodwr(X,alpha,X(:,k));
            moy_b(n_sup)=-proscal+Y(k)+epsilon;
        else
            k=i;
            proscal=prodwr(X,alpha,X(:,k));
            moy_b(n_sup)=-proscal+Y(k)-epsilon;
        end
    end
end

b=mean(moy_b);
 %  on moyenne les b des points supports

%--------------------------------------------------------------

save Modele6d X Y alpha b  % On stocke le materiel qui permetra de reconstruire
                         % la fonction apprise � des fins d'optim.
                         % Construire cette fonction
%
% Dessin du "mod�le" (fonction de regression). 
% % On peut aussi l'appeler en dehors de ce programme
% Dessin_Modele
Dessin_Modele



% % dessin des performances de la convergence 
figure(2)  % dessin des performances de convergence
plot(log(vit_conv),'linewidth',2)
xlabel(' nombre it�rations')
ylabel(' log (ecart)')
title('comportement convergence')
grid
fprintf(' nb iterations: %d crit�re: %e \r',npas,crit_arret)
% 
%% --------------------------------------------------------------------------
