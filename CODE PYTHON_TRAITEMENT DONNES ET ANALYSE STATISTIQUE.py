
#---------------------------------------------------------------------#
# 1ere partie  -  LES FONCTIONS meite
#---------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------#
import pandas as pd
import numpy as np
import scipy.stats as stats
import statsmodels.api as sm
import matplotlib.pyplot as plt
import seaborn as sns

def traitement_doublons(dataset):
    lignes_doublons = dataset.duplicated()
    nombre_de_doublons = lignes_doublons.sum()
    dataset_sans_doublons = dataset.drop_duplicates()
    print("Nombre de doublons :", nombre_de_doublons)
    return dataset_sans_doublons

#---------------------------------------------------------------------------------------------#
def traitement_donnees_manquantes(dataset, seuil=0.05, k=10):
    pourcentage = 1 - dataset.dropna().shape[0] / dataset.shape[0]
    if pourcentage < seuil:
        dataset = dataset.dropna()
    else:
        from fancyimpute import KNN
        imputer = KNN(k=k)
        dataset = imputer.fit_transform(dataset)
    return dataset

#---------------------------------------------------------------------------------------------#
def afficher_boites_a_moustache(dataframe):
    colonnes_numeriques = dataframe.select_dtypes(include=[np.number]).columns
    if not colonnes_numeriques.empty:
        plt.figure(figsize=(8, 6))
        sns.boxplot(data=dataframe[colonnes_numeriques])
        plt.title("Boîtes à moustache")
        plt.show()
    else:
        print("Aucune colonne numérique à afficher.")

#---------------------------------------------------------------------------------------------#
from scipy.stats.mstats import winsorize

def traitement_donnees_extremes(dataset, trim=0.0):
    # Sélectionner uniquement les colonnes numériques
    colonnes_numeriques = dataset.select_dtypes(include=[np.number])

    # Appliquer la winsorisation à chaque colonne numérique
    for colonne in colonnes_numeriques.columns:
        winsorized_col = winsorize(dataset[colonne], limits=(trim, trim))
        dataset = dataset.assign(**{colonne: winsorized_col})

    return dataset

#---------------------------------------------------------------------------------------------#
def extraire_variables_quantitatives(data):
    return data.select_dtypes(include=[np.number])

#---------------------------------------------------------------------------------------------#
def extraire_variables_qualitatives(data):
    return data.select_dtypes(include=['object', 'category'])


#---------------------------------------------------------------------------------------------#
import pandas as pd
import numpy as np

def transformer_en_tableau_disjonctif_complet(data):
    data_dummies = pd.get_dummies(data.select_dtypes(include=['object', 'category']))

    # Convertir les valeurs booléennes en 0 et 1
    data_dummies = data_dummies.astype(int)

    # Concaténer les colonnes numériques avec les dummies
    result = pd.concat([data.select_dtypes(include=[np.number]), data_dummies], axis=1)

    return result


#---------------------------------------------------------------------#
def meite_qt_tableau(vecteur):
    # Calcul de l'effectif
    T = vecteur.value_counts().sort_index()

    # Cumul croissant et décroissant
    Eff_Cum_crois = T.cumsum()
    Eff_Cum_décrois = T.sum() - T.cumsum() + T

    # Fréquences
    Frequence = T / T.sum()
    Freq_Cum_crois = Frequence.cumsum()
    Freq_Cum_décrois = 1 - Frequence.cumsum() + Frequence

    # Création du tableau
    tab = pd.DataFrame({
        'Effectifs': T,
        'Eff_Cum_crois': Eff_Cum_crois,
        'Eff_Cum_décrois': Eff_Cum_décrois,
        'Frequence': Frequence,
        'Freq_Cum_crois': Freq_Cum_crois,
        'Freq_Cum_décrois': Freq_Cum_décrois
    })

    return tab

#-----------------------------------------------------------------------
# Fonction meite.qt (graphiques de variables quantitatives)
import numpy as np
import matplotlib.pyplot as plt

def meite_qt_graph(vecteur):
    fig, axs = plt.subplots(2, 2, figsize=(14, 10))
    
    # Diagramme en bâton
    axs[0, 0].hist(vecteur, bins=np.arange(vecteur.min(), vecteur.max()+2) - 0.5, rwidth=0.8, align='mid')
    axs[0, 0].set_title('Diagramme en Baton')
    axs[0, 0].set_xlabel('Valeur')
    axs[0, 0].set_ylabel('Effectif')

    # Diagramme en escalier pour les fréquences cumulées
    axs[0, 1].hist(vecteur, bins=np.arange(vecteur.min(), vecteur.max()+2) - 0.5, cumulative=True, histtype='step', rwidth=0.8, align='mid')
    axs[0, 1].set_title('Diagramme en Escalier')
    axs[0, 1].set_xlabel('Valeur')
    axs[0, 1].set_ylabel('Fréquence Cumulée')

    # Histogramme
    axs[1, 0].hist(vecteur, bins=30, color='green')
    axs[1, 0].set_title('Histogramme')
    axs[1, 0].set_xlabel('Valeur')
    axs[1, 0].set_ylabel('Effectif')

    # Boîte à moustache
    axs[1, 1].boxplot(vecteur, vert=False)
    axs[1, 1].set_title('Boîte à moustache')
    axs[1, 1].set_xlabel('Valeur')

    plt.tight_layout()
    plt.show()

#-----------------------------------------------------------------------
import pandas as pd
import numpy as np
from scipy.stats import skew, kurtosis

def meite_qt_resume(vecteur):
    res1 = np.min(vecteur)
    res2 = np.max(vecteur)
    res3 = vecteur.mode().iloc[0] if not vecteur.mode().empty else np.nan
    res4 = np.median(vecteur)
    res5 = np.mean(vecteur)
    res6 = np.percentile(vecteur, [0, 25, 50, 75, 100])
    res7 = np.std(vecteur) / res5 if res5 != 0 else np.nan
    res8 = np.var(vecteur)
    res9 = np.std(vecteur)
    res10 = skew(vecteur, nan_policy='omit')
    interpskew = 'distribution étalée à gauche' if res10 < 0 else 'distribution étalée à droite'
    res11 = kurtosis(vecteur, fisher=False, nan_policy='omit')
    interpkurt = 'distribution platikurtique' if res11 < 3 else 'distribution leptokurtique'
    
    return {
        'le minimum est ': res1,
        'le maximum est ': res2,
        'le mode est ': res3,
        'la mediane est ': res4,
        'la moyenne est ': res5,
        'les quartiles sont': res6,
        'le coefficient_variation est': res7,
        'la variance est': res8,
        'l’ecart_type est': res9,
        'le coefficient_assymetrie ou skewness est': res10,
        'interprétation_skewness': interpskew,
        'le cofficent_applatissement ou Kurtosis est': res11,
        'interprétation_kurtosis': interpkurt
    }

#-----------------------------------------------------------------------
import pandas as pd
def meite_ql_tableau(facteur):
    T = facteur.value_counts()  # Calcul des effectifs pour chaque niveau du facteur
    Tc = T.values
    tab = pd.DataFrame({
        'Modalite': T.index,
        'Effectif': Tc,
        'Frequence': Tc / sum(Tc)
    })
    return tab


#-----------------------------------------------------------------------
import pandas as pd
import matplotlib.pyplot as plt

def meite_ql_graph(facteur):
    # Compute frequency counts
    counts = facteur.value_counts()
    fig, ax = plt.subplots(2, 1, figsize=(10, 10))
    
    # Bar plot
    counts.plot(kind='bar', ax=ax[0], color='steelblue')
    ax[0].set_title("Diagramme en barre")
    ax[0].set_ylabel("Count")
    
    # Pie chart
    counts.plot(kind='pie', ax=ax[1], autopct='%1.1f%%', startangle=90)
    ax[1].set_title("Diagramme en secteur")
    ax[1].set_ylabel("")
    
    plt.tight_layout()
    plt.show()


#-----------------------------------------------------------------------
import pandas as pd

def meite_2ql_tableau(variable1, variable2):
    # Tableau des effectifs
    contingency_table = pd.crosstab(variable1, variable2)
    print("Tableau des effectifs:\n")
    print(contingency_table)
    print("\n" + "-"*50 + "\n")
    
    # Tableau des fréquences
    frequency_table = contingency_table / contingency_table.sum().sum()
    print("Tableau des fréquences:\n")
    print(frequency_table.round(2))
    print("\n" + "-"*50 + "\n")
    
    # Tableau des profils ligne
    row_profiles = contingency_table.div(contingency_table.sum(axis=1), axis=0).round(2)
    print("Tableau des profils ligne:\n")
    print(row_profiles)
    print("\n" + "-"*50 + "\n")
    
    # Tableau des profils colonne
    col_profiles = contingency_table.div(contingency_table.sum(axis=0), axis=1).round(2)
    print("Tableau des profils colonne:\n")
    print(col_profiles)
    print("\n" + "-"*50 + "\n")




#-----------------------------------------------------------------------
import pandas as pd
import numpy as np
import statsmodels.api as sm
from scipy.stats import pearsonr, spearmanr, kendalltau

def meite_2qt_liaison(vecteur1, vecteur2):
    res1 = np.corrcoef(vecteur1, vecteur2)[0, 1]
    res2 = spearmanr(vecteur1, vecteur2)[0]
    res3 = kendalltau(vecteur1, vecteur2)[0]
    res4 = res1**2

    if res4 < 0.10:
        interp1 = 'liaison très faible'
    elif res4 < 0.40:
        interp1 = 'liaison faible'
    elif res4 < 0.60:
        interp1 = 'liaison moyenne'
    elif res4 < 0.80:
        interp1 = 'liaison forte'
    else:
        interp1 = 'liaison très forte'

    X = sm.add_constant(vecteur2)
    model = sm.OLS(vecteur1, X).fit()
    res5 = model.params

    r, p_value = pearsonr(vecteur1, vecteur2)

    if p_value < 0.05:
        interp2 = 'liaison significative'
    else:
        interp2 = 'liaison non significative'

    rem = "Si la liaison n’est pas significative, Ne pas tenir compte de son intensité"

    return {
        'Corrélation_Pearson': res1,
        'Corrélation_Spearman': res2,
        'Corrélation_Kendall': res3,
        'Coefficient_Détermination': res4,
        'Interprétation_Intensité_Liaison': interp1,
        'Coefficients_Droite_Régression': res5,
        'p-value': p_value,
        'Significacité_Liaison': interp2,
        'Remarque': rem
    }


#-----------------------------------------------------------------------
import pandas as pd

def meite_2ql_tableau(variable1, variable2):
    # Tableau des effectifs
    contingency_table = pd.crosstab(variable1, variable2)
    print("Tableau des effectifs:\n")
    print(contingency_table)
    print("\n" + "-"*50 + "\n")
    
    # Tableau des fréquences
    frequency_table = contingency_table / contingency_table.sum().sum()
    print("Tableau des fréquences:\n")
    print(frequency_table.round(2))
    print("\n" + "-"*50 + "\n")
    
    # Tableau des profils ligne
    row_profiles = contingency_table.div(contingency_table.sum(axis=1), axis=0).round(2)
    print("Tableau des profils ligne:\n")
    print(row_profiles)
    print("\n" + "-"*50 + "\n")
    
    # Tableau des profils colonne
    col_profiles = contingency_table.div(contingency_table.sum(axis=0), axis=1).round(2)
    print("Tableau des profils colonne:\n")
    print(col_profiles)
    print("\n" + "-"*50 + "\n")


#-----------------------------------------------------------------------
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

def meite_2ql_graph(facteur1, facteur2):
    data = pd.DataFrame({'facteur1': facteur1, 'facteur2': facteur2})
    
    fig, axes = plt.subplots(nrows=3, ncols=2, figsize=(15, 15))
    
    # Diagramme en barres empilées
    df_grouped = data.groupby(['facteur1', 'facteur2']).size().unstack().reset_index()
    df_grouped.set_index('facteur1').plot(kind='bar', stacked=True, ax=axes[0, 0], legend=False)
    axes[0, 0].set_title("Diagramme en barres empilés")
    
    df_grouped2 = data.groupby(['facteur2', 'facteur1']).size().unstack().reset_index()
    df_grouped2.set_index('facteur2').plot(kind='bar', stacked=True, ax=axes[0, 1], legend=False)
    axes[0, 1].set_title("Diagramme en barres empilés")
    
    # Diagramme en bâtons groupés
    df_grouped.plot(kind='bar', x='facteur1', ax=axes[1, 0], legend=False)
    axes[1, 0].set_title("Diagramme en bâtons groupés")
    
    df_grouped2.plot(kind='bar', x='facteur2', ax=axes[1, 1], legend=False)
    axes[1, 1].set_title("Diagramme en bâtons groupés")
    
    # Profil ligne
    df_pct1 = df_grouped.set_index('facteur1').div(df_grouped.set_index('facteur1').sum(axis=1), axis=0)
    df_pct1.plot(kind='barh', stacked=True, ax=axes[2, 0], legend=False)
    axes[2, 0].set_title("Profil ligne")
    
    # Profil colonne
    df_pct2 = df_grouped2.set_index('facteur2').div(df_grouped2.set_index('facteur2').sum(axis=1), axis=0)
    df_pct2.plot(kind='barh', stacked=True, ax=axes[2, 1], legend=False)
    axes[2, 1].set_title("Profil colonne")
    
    plt.tight_layout()
    plt.show()

#-----------------------------------------------------------------------
import pandas as pd
import numpy as np
from scipy.stats import chi2_contingency

def cramer_v(contingency_table):
    chi2_val, _ = chi2_contingency(contingency_table)[:2]
    n = contingency_table.sum().sum()
    k = min(contingency_table.shape)
    return np.sqrt(chi2_val / (n * k - 1))

def meite_2ql_liaison(vecteur1, vecteur2):
    # Tableau de contingence
    contingency_table = pd.crosstab(vecteur1, vecteur2)

    # Calcul des effectifs théoriques
    _, _, _, expected = chi2_contingency(contingency_table)

    # Test du chi-carré
    chi2_stat, chi2_p_value, _, _ = chi2_contingency(contingency_table)

    # Calcul de V de Cramer
    v_cramer = cramer_v(contingency_table)

    # Interprétations
    interp1 = 'liaison significative, les deux variables sont liées' if chi2_p_value < 0.05 else 'liaison non significative, les deux variables ne sont pas liées'
    if v_cramer < 0.10:
        interp2 = 'liaison très faible'
    elif v_cramer < 0.40:
        interp2 = 'liaison faible'
    elif v_cramer < 0.60:
        interp2 = 'liaison moyenne'
    elif v_cramer < 0.80:
        interp2 = 'liaison forte'
    else:
        interp2 = 'liaison très forte'
    
    rem = "Si la liaison n’est pas significative, Ne pas tenir compte de son intensité"
    
    return {
        'Effectif_Théorique': expected,
        'Résultat_Test_KhiDeux': chi2_stat,
        'Khi_Deux': chi2_stat,
        'V_Cramer': v_cramer,
        'Khi2.P.value': chi2_p_value,
        'Significativité_TestKhi2': interp1,
        'Intensité_liaison': interp2,
        'Remarque': rem
    }

#-----------------------------------------------------------------------
import pandas as pd
import numpy as np
from statsmodels.formula.api import ols
from statsmodels.stats.anova import anova_lm

def meite_qtql_liaison(vecteur, facteur):
    # Create a DataFrame for calculations
    df = pd.DataFrame({'vecteur': vecteur, 'facteur': facteur.astype(str)})
    
    # Calculate ANOVA
    model = ols('vecteur ~ facteur', data=df).fit()
    anova_table = anova_lm(model, typ=2)
    
    # Eta Squared
    eta_squared = anova_table['sum_sq'][0] / (anova_table['sum_sq'][0] + anova_table['sum_sq'][1])
    
    # Interpretations
    if eta_squared < 0.10:
        interp1 = 'liaison très faible'
    elif eta_squared < 0.40:
        interp1 = 'liaison faible'
    elif eta_squared < 0.60:
        interp1 = 'liaison moyenne'
    elif eta_squared < 0.80:
        interp1 = 'liaison forte'
    else:
        interp1 = 'liaison très forte'
        
    p_value = anova_table['PR(>F)'][0]
    interp2 = 'liaison significative, les deux variables sont liées' if p_value < 0.05 else 'liaison non significative, les deux variables ne sont pas liées'
    rem = "Si la liaison n'est pas significative, Ne pas tenir compte de son intensité"
    
    return {
        'Rapport_Correlation': eta_squared,
        'Anova.P.value': p_value,
        'Significativite_TestAnova': interp2,
        'Intensite_liaison': interp1,
        'Remarque': rem
    }

#-----------------------------------------------------------------------

import matplotlib.pyplot as plt
from scipy import stats
import statsmodels.api as sm

def test_normalite_meite(vecteur):
    # Afficher l'histogramme
    plt.figure(figsize=(6, 6))
    plt.subplot(211)
    plt.hist(vecteur, bins='auto', color='green')
    plt.title("Histogramme")

    # Afficher le QQ-plot
    plt.subplot(212)
    sm.qqplot(vecteur, line='s')
    plt.title("QQ-plot")

    plt.tight_layout()
    plt.show()

    # Effectuer les tests de normalité
    shapiro_wilk = stats.shapiro(vecteur)
    interpret_shapiro = "La distribution suit la loi normale" if shapiro_wilk.pvalue >= 0.05 else "La distribution ne suit pas une loi normale"

    jarque_bera = stats.jarque_bera(vecteur)
    interpret_jarque_bera = "La distribution suit la loi normale" if jarque_bera[1] >= 0.05 else "La distribution ne suit pas une loi normale"

    agostino = stats.mstats.normaltest(vecteur)
    interpret_agostino = "La distribution suit la loi normale" if agostino.pvalue >= 0.05 else "La distribution ne suit pas une loi normale"

    kolmogorov_smirnov = stats.kstest(vecteur, 'norm', args=(vecteur.mean(), vecteur.std()))
    interpret_kolmogonov_smirnov = "La distribution suit la loi normale" if kolmogorov_smirnov.pvalue >= 0.05 else "La distribution ne suit pas une loi normale"

    return {
        "test_shapiro_wilk": shapiro_wilk,
        "interpretation_shapiro_wilk": interpret_shapiro,
        "test_jarque_bera": jarque_bera,
        "interpretation_jarque_bera": interpret_jarque_bera,
        "test_agostino": agostino,
        "interpretation_agostino": interpret_agostino,
        "test_kolmogorov_smirnov": kolmogorov_smirnov,
        "interpretation_kolmogorov_smirnov": interpret_kolmogonov_smirnov
    }
#---------------------------------------------------------------------------------------------#
# FONCTIONS DE PRETRAITEMENT DES DONNEES
#---------------------------------------------------------------------------------------------#
# traitement_doublons()                         # traitement des doublons d'un dataframe
# traitement_donnees_manquantes()               # traitement des NA d'un dataframe
# afficher_boites_a_moustache()                 # affiche les boites à moustache d"un dataframe
# traitement_donnees_extremes()                 # traitement des outlyers d'un dataframe
# extraire_variables_quantitatives()            # extraction des variables quanti d"un dataframe
# extraire_variables_qualitatives()             # extraction des variables quali d"un dataframe
# transformer_en_tableau_disjonctif_complet()   # transformation en tableau disjonctif complet d'un dataframe
#---------------------------------------------------------------------------------------------#
# FONCTIONS D'ANALYSE UNIVARIEE EN STATISTIQUE DESCRIPTIVE
#---------------------------------------------------------------------------------------------##---------------------------------------------------------------------------------------------#
# meite_qt_tableau()   # tableau statistique de variable quantitative
# meite_qt_graph()     # graphiques de variable quantitative
# meite_qt_resume()    # resume numerique de variable quantitative
# meite_ql_tableau()   # tableau statistique de variable qualitative
# meite_ql_graph()     # graphique de variable qualitative
#---------------------------------------------------------------------------------------------#
# FONCTIONS D'ANALYSE BIVARIEE EN STATISTIQUE DESCRIPTIVE
#---------------------------------------------------------------------------------------------##---------------------------------------------------------------------------------------------#
# meite_2qt_liaison()  # liaison entre deux variables quantitatives
# meite_2ql_tableau()  # tableaux statistiques de deux variables qualitatives
# meite_2ql_graph()    # graphiques de deux variables qualitatives
# meite_2ql_liaison()  # liaison entre deux variables qualitatives
# meite_qtql_liaison() # liaison entre une variable quantitative et une variable qualitative
#---------------------------------------------------------------------------------------------#
# test_normalite_meite() # Test de normalite
#---------------------------------------------------------------------------------------------##---------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------#

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import statsmodels.api as sm
from scipy.stats.mstats import winsorize
from sklearn.impute import KNNImputer
from scipy.stats import binom_test
from scipy.stats import wilcoxon
import pandas as pd
from scipy.stats import pearsonr,spearmanr, kendalltau
from scipy.stats import shapiro, bartlett, kruskal
import statsmodels.api as sm
import seaborn as sns

from statsmodels.formula.api import ols
import copy