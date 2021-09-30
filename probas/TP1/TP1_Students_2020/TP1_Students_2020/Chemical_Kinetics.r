
#install.packages("gdata")
#install.packages("ggplot2")
#install.packages("ggplot")
#install.packages("scales")



library(datasets) # 
library(ggplot2)
source(file="Chemical_Kinetics_Functions_M1.r")


##################################

#. Part I: Model M1

        
# 2)
test_produce_priors_M1(5E+4,5E+8,100)


####

# 3) 

R0 <- 1200
k <- 1.357e+03
t_mod <- seq(1,1000,1)
R_mod <- Compute_R_profile_M1(t_mod,R0,k)
plot_profile_M1(t_mod,R_mod)

# 4) 

R0 <- 1200
lbound <- 1
ubound <- 3E+3
n_k <- 500
esp <- 0.3
Examine_likelihood_M1(R0,lbound,ubound,n_k,esp)

####

#5) 

R0 <- 1200
lbound <- 5E+5
ubound <- 1.5E+6
eps <- 0.065
lb_plot <- lbound
ub_plot <- ubound
n_k <- 500
Compute_all_posteriors_M2(R0,lbound,ubound,n_k,eps,lb_plot,ub_plot)

# 6) 

lbound <- 2
ubound <- 4.2E+3
n_k <- 500

k <- generate_grid_M1(lbound,ubound,n_k)
# fk
f_k_k <- f_k(k,lbound,ubound)
integrate_density_M1(k,f_k_k,1E+3,1.5E+3)
# fu
f_u_k <- f_u(k,lbound,ubound)
integrate_density_M1(k,f_u_k,1E+3,1.5E+3)
# fw
f_w_k <- f_w(k,lbound,ubound)
integrate_density_M1(k,f_w_k,1E+3,1.5E+3)


# 7) 


R0 <- 1200
R_profile_exp <- read.table("R_Exp.csv", header=FALSE)
lbound <- 1E
ubound <- 1.55E+3
eps <- 0.065
n_k <- 50
k <- generate_grid_M1(lbound,ubound,n_k)
Compute_likelihood_all_M1(R0,lbound,ubound,n_k,eps)
L_all <- read.table("Likelihood_M1.csv",header = FALSE)
colnames(L_all) <- c("k_all","L_all")
L_all <- L_all$L_all



# fk
f_k_k <- f_k(k,lbound,ubound)
post_k <- Compute_posterior_M1(k,f_k_k,L_all)
I_k <- integrate_density_M1(k,post_k,1E+3,1.5E+3)
# fu
f_U_k <- f_u(k,lbound,ubound)
post_U <- Compute_posterior_M1(k,f_U_k,L_all)
I_U <- integrate_density_M1(k,post_U,1E+3,1.5E+3)
# fw
f_W_k <- f_w(k,lbound,ubound)
post_W <- Compute_posterior_M1(k,f_W_k,L_all)
I_W <- integrate_density_M1(k,post_W,1E+3,1.5E+3)

RESULT <- c(I_k,I_U,I_W)
print(RESULT)
# 8) 


R0 <- 1200
lbound <- 2
ubound <- 2E+3
eps <- 0.3
#eps <- 0.065
lb_plot <- lbound
ub_plot <- ubound
n_k <- 500
Compute_all_posteriors_M1(R0,lbound,ubound,n_k,eps,lb_plot,ub_plot)

# 9)
R0 <- 1200
lbound <- 1.2E+3
ubound <- 1.55E+3
eps <- 0.065
lb_plot <- lbound
ub_plot <- ubound
n_k <- 500
Compute_all_posteriors_M1(R0,lbound,ubound,n_k,eps,lb_plot,ub_plot)

# 6) 

lbound <- 2
ubound <- 4.2E+3
n_k <- 500

k <- generate_grid_M1(lbound,ubound,n_k)
# fk
f_k_k <- f_k(k,lbound,ubound)
integrate_density_M1(k,f_k_k,1E+3,1.5E+3)
# fu
f_u_k <- f_u(k,lbound,ubound)
integrate_density_M1(k,f_U4_k,1E+3,1.5E+3)
# fw
f_w_k <- f_w(k,lbound,ubound)
integrate_density_M1(k,f_W_k,1E+3,1.5E+3)


# 7) 


R0 <- 1200
R_profile_exp <- read.table("R_Exp.csv", header=FALSE)
lbound <- 1E
ubound <- 1.55E+3
#eps <- 0.065
eps <- 0.3
n_k <- 50
k <- generate_grid_M1(lbound,ubound,n_k)
Compute_likelihood_all_M1(R0,lbound,ubound,n_k,eps)
L_all <- read.table("Likelihood_M1.csv",header = FALSE)
colnames(L_all) <- c("k_all","L_all")
L_all <- L_all$L_all



# fk
f_k_k <- f_k(k,lbound,ubound)
post_k <- Compute_posterior_M1(k,f_k_k,L_all)
I_k <- integrate_density_M1(k,post_k,1E+3,1.5E+3)
# fu
f_U_k <- f_u(k,lbound,ubound)
post_U <- Compute_posterior_M1(k,f_U_k,L_all)
I_U <- integrate_density_M1(k,post_U,1E+3,1.5E+3)
# fw
f_W_k <- f_w(k,lbound,ubound)
post_W <- Compute_posterior_M1(k,f_W_k,L_all)
I_W <- integrate_density_M1(k,post_W,1E+3,1.5E+3)

RESULT <- c(I_k,I_U,I_W)
print(RESULT)




###################################################################"

# Part II: Model M2


# 2) 


###


# 3) 



###

# 4) 



###

# 5) 



###################################################################"

#  III. Bayes factor


# 1)


#  2)   




###

#  3) 








































