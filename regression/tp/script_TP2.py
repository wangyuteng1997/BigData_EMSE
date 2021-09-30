# Majeure Science des Données 2020-2021
# TP mini-challenge - cours de Régression 

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

# Importer les données d'apprentissage
data = pd.read_csv('Data_app.txt', delimiter=' ')
data.head()

# Variable à prédire
ventes = data['ventes']

# Moyenne et écart-type des ventes observées
ventes_mean = np.mean(ventes)
print(ventes_mean)
ventes_sd = np.std(ventes)
print(ventes_sd)

# Modèle de Régression Linéaire vide : p = 0 --> pas de prédicteur utilisé
# Ainsi, on prédit par la moyenne des ventes observées
ecarts = ventes - ventes_mean

# Tracé séquentiel des écarts à la moyenne ou résidus
n = len(ecarts)
plt.plot(range(n), ecarts,'o')
plt.xlabel('Numéro observation')
plt.ylabel('Valeur en euros')
plt.plot([0,n-1],[0,0],'red')
plt.grid()
plt.title('Ecart des ventes à la moyenne (résidus)')

# Boxplot
plt.boxplot(ecarts)
plt.ylabel('Valeur en euros')
plt.grid()
plt.title('Boxplot des résidus')

# On prédit par la moyenne des ventes = prédicteur constant
# ne dépend pas des prédicteurs "température" et 'type de jour'
# construction du fichier des prévisions pour les données test

data_test = pd.read_csv('Data_test.txt', delimiter=' ') 
m = len(data_test)
predictions = np.zeros((m,1))

ventes_mean = np.round(ventes_mean,0)
for k in range(m) :
    predictions[k] = ventes_mean

# fichier des prédictions associées aux données "test"
np.savetxt('VotreNOM_pred_py.txt', predictions)
