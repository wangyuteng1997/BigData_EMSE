Spheres <- function(U,nprec)
{
  # function [c,beta2]=Spheres(U,nprec)
  # for U=[u_1...u_(d+1)] formed of d+1 vectors in R^d
  # returns the center of mass c=sum_i (alpha_i u_i) of the u_i's
  # and the squared distance ||c-u_i||^2=beta2 for all i
  # nprec : precision 1e-nprec on a determinant to consider points as aligned
  #
  # Constructed from center_mass.m to replace Sphere_old.m
  
  d = nrow(U)
  dp= ncol(U)
  
  # Version A
  # M=U'*U; un=ones(d+1,1); n=diag(M);
  # AA=[2*M -un;un' 0]; 
# s=pinv(AA)*[n;1]; alpha=s(1:d+1); c=U*alpha; 
# beta2=alpha'*M*alpha-s(d+2);

# Version B
un=rep(1,d+1,1)
AA=cbind(2*t(U), -un)
s=t(pinv(AA)%*%(apply(U^2,2,sum)))
c=s[1:d]
beta2=t(c)%*%c-s[d+1]
          
if (abs(det(AA))<10^(-nprec)) {c=Inf*ones(d,1)}
          
sphere = list(c=c, beta2 = beta2)
return(sphere)
}
