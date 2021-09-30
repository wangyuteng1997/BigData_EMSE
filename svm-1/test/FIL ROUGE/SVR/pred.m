function s=pred(Xtest)
load Modele X Y alpha b
s=prodwr(X,alpha,Xtest)+b;

end