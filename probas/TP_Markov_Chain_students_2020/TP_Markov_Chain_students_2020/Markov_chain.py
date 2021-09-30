# module load anaconda/python3
# ipython  Markov_chain.py
# sbatch Job_3D_Markov_chain.job

import Markov_chain_functions # 

# We first import the functions. 
path = "F:/EMSE/3A/BIG Data/probas/TP_Markov_Chain_students_2020/TP_Markov_Chain_students_2020/transition_matrix_q2.csv"
state_space=[0,1,2,3,4,5,6,7,8,9,10,11]
#extract_transition_matrix = Markov_chain_functions.extract_transition_matrix(path)
#print(extract_transition_matrix)
#simulate_finite_mc = Markov_chain_functions.simulate_finite_mc(path,12,state_space)
#print(simulate_finite_mc)
#invariant_distribution = Markov_chain_functions.invariant_distribution(path,state_space)
#print(invariant_distribution)
#probability_distribution_k = Markov_chain_functions.probability_distribution_k(path,10,state_space)
#print(probability_distribution_k)
frequency_distribution_k = Markov_chain_functions.frequency_distribution_k(path,100000,12,state_space)
print(frequency_distribution_k)





