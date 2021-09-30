  
 
"""
Functions for generating and studying Markov chains.
"""
import math
import numpy as np
import sys
import csv
import os
import matplotlib.pyplot as plt
import pickle as pic
import random as rnd 
import numpy.linalg as linalg

from scipy.linalg import eig

import scipy
import scipy.linalg as la

from shutil import copyfile






#################################################################################

def multiply_vector_matrix(v,P):
	# We use lists for vectors and arrays for matrices.
	# Getting Python to treat them as true vectors and matrices can be quite difficult.
	
	# Instead, we'll define the multiplication v*P on our own. 
	
	# BEFORE CALLING THIS FUNCTION, YOU MUST MAKE SURE THAT THE DIMENSIONS OF v and P
	# ARE COHERENT! 

	
	n = len(v); 

	v2 = []; # we first initialise the new vector 
	for i in range(n):	
		v2.append(0); 
		# end initialisation

	for i in range(n):	# for each v2[i]
		for j in range(n):
			v2[i] = v2[i]+v[j]*P[j][i];
			
	# print("v",v,"P",P,"v2",v2); 	

	return v2

#################################################################################


def approx_equal(a,b,tol = 1E-04):
	# Because of numerical innacuracies, real numbers are rarely exactly equal.
	a = a.real; b = b.real; # some numbers (like eigenvalues) can be complex numbers.
	maxi = max(a,b); 
	
	if abs(maxi) > 0:	
		q = (a-b)/maxi;
	else:
		q = 0; # a = b = 0.

	equal = False
	
	if abs(q) < tol:
		equal = True

	return equal   # boolean

#################################################################################

def norm_eigenvector(v):
	# turn the eigenvector into a probability distribution

	n = len(v); 
	summe = 0; 
	for i in range(n):
		v[i] = v[i].real; 
		summe = summe+v[i]; 
	
	v =v/summe;

	v2 = [];
	for i in range(n):  # we only want to have real numbers printed as such.
		v2.append(v[i].real); 
		#print("v[i].real",v[i].real); 

	#print("v2",v2);

	return v2

#################################################################################


def extract_transition_matrix(matrix_file):
	# We extract the transition matrix from the file 


	P = np.loadtxt(matrix_file)
	# print("P",P)


	return P

#################################################################################

def cumulative_transition_matrix(matrix_file):
	# Compute the cumulative transition matrix 


	P = np.loadtxt(matrix_file)

	n_states = len(P); 
	

	
	P2 = np.loadtxt(matrix_file)
	

# We'll initialise the new matrix with 0 everywhere. 
	for i in range(n_states): 	
		for j in range(n_states):	
			P2[i][j] = 0;
	

	for i in range(n_states): # for each row, we compute the cumulative probability distribution
		summe = 0;
		for j in range(n_states): 
			summe = summe+P[i][j];
			P2[i][j] = summe; # print(i,"-",j,"P[i][j]",P[i][j]); 
			
		if P2[i][n_states-1] < (1-1E-10):
			print("Error in cumulative_transition_matrix: P is not a true probability transition matrix!");

	return P2


#################################################################################

def random_index(cumulative_P,i):
	# Generate a random index based on the i-th row of P and cumulative_P.


	# print("We're first in the state indexed by ",i); 
	n_states = len(cumulative_P);
	# print("n_states",n_states); 


	# We generate a random probability value between 0 and 1
	rand_p = rnd.random();  # print("rand_p",rand_p); print("cumulative_P",cumulative_P); 
	
	j = 0; 
	while (rand_p > cumulative_P[i][j] and j < n_states-1):
		j = j+1;
		

	return j

#################################################################################

def find_index(X_value,state_space):
# determine the index corresponding to X_value.
# state_space[3] = -15 means that the third value of the probability distribution 
# corresponds to X = -15. 

	n_states = len(state_space); 

	i = 0; 
	while approx_equal(X_value,state_space[i]) == False and i < n_states:
		i = i+1;
		if i == n_states:
			print("Error: the value ",X_value," can't be found in the state space!"); 

	return i
	
	
	
#################################################################################

def approximate_prob_distributions(X,state_space):
# Based on the realisation of a Markov Chain X, we compute the probability distribution. 
# state_space[3] = -15 means that the third value of the probability distribution 
# corresponds to X = -15. 


	n_states = len(state_space); 
	p_distribution = []; # initialisation of the list
	for i in range(n_states): 
		p_distribution.append(0); 


	n_X = len(X); 
	for i in range(n_X): 
		# we must first find the index of state_space corresponding to X[i].
		local_index = find_index(X[i],state_space)
		p_distribution[local_index]=p_distribution[local_index]+1;

	for k in range(n_states):
		p_distribution[k]=p_distribution[k]/n_X;


	return p_distribution



#################################################################################

def produce_histogram(v,state_space,delta = -5):
	# Produce an histogram based on the vector v.
	# state_space[3] is the value corresponding to v[3]
	
	# delta is the space between two ticks of the x axis. It must be an integer. 
	# delta = -5 means that there is a stick for every value of the state space.	
	
	min_state = min(state_space); max_state = max(state_space);
	
	
	if delta == -5: 
		tick_set = range(len(state_space));
	else:
		tick_set = range(min_state,max_state,delta);
	
	
	
	fig, ax = plt.subplots()
	
	plt.title('Probability/frequency distribution')
	plt.ylabel('Probability')
	plt.xlabel("State")
	plt.bar(state_space, v, align='center', alpha=0.8)
	alpha = 0.8
	ax.set_xticks(tick_set)
	#ax.set_xticklabels(v, rotation='vertical')
	#plt.show()
	plt.savefig('temp.png')

	return 1

#################################################################################



def simulate_finite_mc(matrix_file,n,histogram = False, ini=1E+100,state_space=[]):
	# We simulate a finite Markov chain based on the transition matrix defined in "matrix_file".

# state_space=[3,7,-8,14] means that matrix_file[1][3] = p(14 | 7), which is the probability 
# that we go from state 7 to state 14.
# If state_space=[], the state space consists of the indices of the matrix in Python {0,1,2...}. 

# n is the number of elements of the Markov chain.

# "ini" is the INDEX of the first state of the MC. If ini > 1E+90, it is randomly chosen.

# histogram: we consider all values of the MC to produce an histogram. 



	P = extract_transition_matrix(matrix_file)
# each line of this matrix is a probability distribution. 		
	
	cumulative_P = cumulative_transition_matrix(matrix_file); # 
# each line of this matrix is a cumulative probability distribution. 	
		

	
	n_states = len(P); 

	n_state_space = len(state_space); 

	if n_state_space < 1:  # the states are the indices of the transition matrix
		for i in range(n_states):
			state_space.append(i); # print("i",i); 
		n_state_space = len(state_space)	
		
	if n_state_space != n_states:
		print("ERROR: the length state_space ",n_state_space," doesn't correspond to the dimension of the matrix A ",n_states, "!");		
		sys.exit(0)
		
	

	if ini > 1E+90:
		ini = rnd.randint(0,n_states-1); # random integer 


	index_ini = find_index(ini,state_space)
	X = []; X.append(state_space[index_ini]);  # the realisation of the Markov chain is a LIST
	current_index = index_ini; #  index of X(0). 
	
	
	for i in range(0,n):
		current_index = random_index(cumulative_P,current_index); 
		# the next point is randomly chosen according to the cumulative transition probabilities.
		
		X.append(state_space[current_index]); # next point 

	prob_distributions = approximate_prob_distributions(X,state_space); # we compute the frequency distribution based on the values of X.
	
	if histogram == True:
		produce_histogram(prob_distributions,state_space)
		copyfile("temp.png","average_distribution.png"); 
		os.remove("temp.png")



    # The frequency distribution is saved in this file.
	outfile = open("frequency_dist.csv",'w')	
	for i in range(n_states):
		outfile.write( str(state_space[i]) ); outfile.write(" ");
	outfile.write("\n");		
	for i in range(n_states):
		outfile.write( str(prob_distributions[i]) ); outfile.write(" ");
	outfile.close	



	return X

#################################################################################

def invariant_distribution(matrix_file,histogram = False,state_space=[]):
	# determine the invariant distribution which is an eigenvector of 
	# the transition matrix corresponding to the eigenvalue 1. 

	# This function can only be used for IRREDUCIBLE AND APERIODIC matrices! 


	P = extract_transition_matrix(matrix_file); 

	n_states = len(P);  # print("n_states",n_states); 

	n_state_space = len(state_space); # print("n_state_space",n_state_space); 

	if n_state_space < 1:
		for i in range(n_states):
			state_space.append(i); # print("i",i); 
		n_state_space = len(state_space)	
		
	if n_state_space != n_states:
		print("ERROR: the length state_space ",n_state_space," doesn't correspond to the dimension of the matrix A ",n_states, "!");		
		sys.exit(0)


	results = eig(P, right = False, left=True); # determine the eigenvalues and left eigenvectors. 
	eigenvalues = results[0];  eigenvectors = results[1];  



	n_eigenvalues = len(eigenvalues); 

	one = False; # we seek the index of the eigenvalue 1
	i = 0; 
	while (  ( approx_equal(eigenvalues[i],1)  == False  or eigenvalues[i].imag > 1E-10)  and  i < n_eigenvalues-1 ):
		i = i+1;


	i_1 = i;
	eigenvalue = eigenvalues[i_1]
	
	# print("eigenvector before",eigenvectors[:,i_1]); 
	eigenvector = norm_eigenvector(eigenvectors[:,i_1])

	print("eigenvalue",eigenvalue); print("eigenvector",eigenvector); 


	if histogram == True:
		produce_histogram(eigenvector,state_space)
		copyfile("temp.png","invariant_distribution.png"); 
		os.remove("temp.png")
		
		

	
	# The invariant distribution is saved in this file. 	
	outfile = open("invariant_dist.csv",'w')	
	for i in range(n_states):
		outfile.write( str(state_space[i]) ); outfile.write(" ");
	outfile.write("\n");		
	for i in range(n_states):
		outfile.write( str(eigenvector[i]) ); outfile.write(" ");
	outfile.close	
		
		
		
		
	return eigenvector
	
	
	
#################################################################################


def frequency_distribution_k(matrix_file,k,n,histogram = False, ini=1E+100,state_space=[]):
	# We compute the frequency distribution of X(k) starting in state "ini".
	# We launch n Markov chains and save the k-th value (which is the last value) 
	# each time. We the compute the frequency distribution. 

# "ini" is the INDEX of the first state of the MC. If ini > 1E+90, it is randomly chosen.


	P = extract_transition_matrix(matrix_file); 
# each line of this matrix is a probability distribution. 	

	cumulative_P = cumulative_transition_matrix(matrix_file); # 
# each line of this matrix is a cumulative probability distribution. 	


	n_states = len(P); 

	n_state_space = len(state_space); 


	if ini > 1E+90:
		ini = rnd.randint(0,n_states-1); # random integer 
		# print("ini",ini); 
		


	if n_state_space < 1:  # the states are the indices of the transition matrix
		for i in range(n_states):
			state_space.append(i); # print("i",i); 
		n_state_space = len(state_space)	
		
	if n_state_space != n_states:
		print("ERROR: the length state_space ",n_state_space," doesn't correspond to the dimension of the matrix A ",n_states, "!");		
		sys.exit(0)


	X_k = []; # list 

	histogram2 = False; # we don't produce a histogram for each MC generated.

	for i in range(n):
		X = simulate_finite_mc(matrix_file,k,histogram2, ini,state_space);   # print("X",X,"X[k]",X[k]); 
		X_k.append(X[k]); # last value of the MC.


	prob_distribution = approximate_prob_distributions(X_k,state_space); 
# Computation of the frequency distribution of X(k) based on the n values.

	
	if histogram == True:
		produce_histogram(prob_distribution,state_space)
		copyfile("temp.png","average_distribution_k.png"); 
		os.remove("temp.png")


    # The frequency distribution is saved in this file.
	outfile = open("frequency_dist_k.csv",'w')	
	for i in range(n_states):
		outfile.write( str(state_space[i]) ); outfile.write(" ");
	outfile.write("\n");		
	for i in range(n_states):
		outfile.write( str(prob_distribution[i]) ); outfile.write(" ");
	outfile.close	




	return prob_distribution

#################################################################################

def probability_distribution_k(matrix_file,k,histogram = False,ini=1E+90,state_space=[]):

# determine the probability distribution of X(i).
# "ini" is the INDEX of the first state of the MC. If ini > 1E+90, it is randomly chosen.


	if ini > 1E+90:
		ini = rnd.randint(0,n_states-1); # random integer 
		# print("ini",ini); 
		

	P = extract_transition_matrix(matrix_file); 

	n_states = len(P);  # print("n_states",n_states); 

	n_state_space = len(state_space); # print("n_state_space",n_state_space); 

	if n_state_space < 1:
		for i in range(n_states):
			state_space.append(i); # print("i",i); 
		n_state_space = len(state_space)	
		
	if n_state_space != n_states:
		print("ERROR: the length state_space ",n_state_space," doesn't correspond to the dimension of the matrix A ",n_states, "!");		
		sys.exit(0)

	#print("ini",ini);


	v0 = [];
	for i in range(n_states):	
		v0.append(0); 
	v0[ini] = 1; # initialisation of the initial distribution. 

	v = v0; 
	
	
	if k > 0:
		v = multiply_vector_matrix(v,P)
	
	
	if k > 1:
		for i in range(k-1):
			v = multiply_vector_matrix(v,P); 

# Eventually, v represents the probability distribution of X(k). 


	if histogram == True:
		produce_histogram(v,state_space)
		copyfile("temp.png","prob_distribution_k.png"); 
		os.remove("temp.png")
		
		
    # The frequency distribution is saved in this file.
	outfile = open("prob_distribution_k.csv",'w')	
	for i in range(n_states):
		outfile.write( str(state_space[i]) ); outfile.write(" ");
	outfile.write("\n");		
	for i in range(n_states):
		outfile.write( str(v[i]) ); outfile.write(" ");
	outfile.close			
		
		
		
		
	return v
	
	
	


#################################################################################

def continuous_curves(X,Y1, plot_name= "plot.png", X_name = "X", Y_name = "Y",X2 = [], Y2 = [ ]):
# Produce two continuous curves	(X,Y1) and (X,Y2) or only one if Y2 is empty 


	min_X = np.min(X)-1; min_Y = np.min(Y1); # trick to include all values
	max_X = np.max(X); max_Y = np.max(Y1);

	if len(Y2) > 0:
		min_Y2 = np.min(Y2);  max_Y2 = np.max(Y2);		
		min_Y = min(min_Y,min_Y2); max_Y = max(max_Y,max_Y2);



# 	print("X",X,"Y1",Y1); 

	print("len(X)",len(X),"len(Y1)",len(Y1) ); 
	print("len(X2)",len(X2),"len(Y2)",len(Y2) ); 

	ax = plt.gca()
	plot_file = plot_name;
	ax.plot(X, Y1)
	if len(Y2) > 0:
		ax.plot(X2, Y2)			
	plt.xlabel(X_name)
	plt.ylabel(Y_name)
	locs, labels = plt.xticks()
	locs, labels = plt.yticks()
	#print("locs",locs,"labels",labels)	
	plt.axis([min_X,max_X,min_Y,max_Y])
	#plt.ticklabel_format(axis="y", style="sci", scilimits=(0,0))
	plt.savefig(plot_file)
	plt.close()


	return 1




#################################################################################














