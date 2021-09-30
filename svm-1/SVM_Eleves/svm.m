% S�parateurs � Vaste Marge pour la Classification
% Cas lin�aire, non lin�aire, marges "souples"

clear all
close all
global ikernel  % type de noyau choisi.  1: produit scalaire dans Rp (lin�aire)
                                         % 2: noyau gaussien


 load 'data3' X lab     % chargement des donn�es 
 ikernel=2;             % choix du type de noyau
 marge_souple= false;  % choix marge souple ou pas
 C_souple= 10;          % coef souplesse de la marge souple
  
% bornes pour le dessin 2D
xmin=min(X(1,:));
ymin=min(X(2,:));
xmax=max(X(1,:));
ymax=max(X(2,:));

na=length(X); % nombre de points (d'apprentissage)

% dessin des points dans R^2
subplot(1,2,1)
for i=1:na
    if lab(i)==1
        plot(X(1,i),X(2,i),'o','linewidth',2)
        hold on
    else
        plot(X(1,i),X(2,i),'x','linewidth',2,'markersize',12)
        hold on
    end
end
axis([xmin xmax ymin ymax])
grid
axis equal
hold on
pause
% Coeur du Programme - M�thode d'UZAWA
% assemblage matrice de la forme quadratique du probl�me dual



% gradient � pas constant pour probl�me dual
% On vous laisse quelques valeurs pour les paramettres reglables... � vous de voir
alph0=0.5*ones(na,1); % point de d�part (0, ou autre): r�glable
pasgrad=5e-3;         % pas du gradient : parametre r�glable
u=ones(na,1);         % vecteur de 1
crit_arret=1;         % initialisation critere d'arret
npas=0;               % comptage du nombre de pas
npas_max=100000;      % garde-fou convergence : on arrete si le nombre d'iterations est trop grand
epsi=1e-5;            % seuil convergence

% boucle de gradient projet�
% A vous de jouer!

% recherche des points supports
epsi=1e-5;
Points_support=find(alph>epsi)' % voir help find

% on ne se sert plus directement de w, mais de son produit scalaire 
% avec un vecteur, defini par un noyeau : fonction prodw

% Calcul de b (on le calcule pour chaque pt support, puis on moyenne)
% chaque pt support devrait fournit le m�me b (mais aux erreurs
% num�riques pr�s, c'est pouquoi on moyenne)


b=mean(moy_b)   %  on moyenne les b des points supports
grid


% Fin du coeur du programme


%calcul et trac� des isovaleurs
xp=xmin:0.2:xmax;   % cr�ation d'une grille pour les besoins de contour
yp=ymin:0.2:ymax;
npx=length(xp);
npy=length(yp);
for i=1:npx
    for j=1:npy
        ps=prodw(X,lab,alph,[xp(i),yp(j)]'); % calcul de <w,x> + b sur une grille
        V(i,j)=ps + b;   % on n'a pas besoin explicitement de w, mais de son 
                         % produit scalaire avec tout vecteur qu'on
                         % encapsule dans prodw (utilisation noyau si cas non
                         % lin�aire
    end
end
hold on
contour(xp,yp,V',[-1 0 1],'linewidth',2,'color','r')
axis([xmin xmax ymin ymax])
title(' Suport Vector Machine')
grid

subplot(1,2,2)
plot(log(vit_conv),'linewidth',2)
xlabel(' nombre it�rations')
ylabel(' log (ecart)')
title('comportement convergence')
grid


