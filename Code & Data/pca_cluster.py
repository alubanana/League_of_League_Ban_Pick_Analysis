"""
Author: Qingyuan
"""
import numpy as np
import pandas as pd
from sklearn.decomposition import PCA
from sklearn.preprocessing import scale
from sklearn.cluster import KMeans
import matplotlib.pyplot as plt

data = pd.read_csv("2020_LoL_esports_match_data_from_OraclesElixir_20201108.csv", index_col=None)
data = data.loc[data["position"] != "team"]
data = data.dropna(axis=1, how="all")
column = ['champion','kills', 'deaths', 'assists',
        'doublekills', 'triplekills', 'quadrakills', 'pentakills',
       'firstblood', 'firstbloodkill', 'firstbloodassist', 'firstbloodvictim',
       'team kpm', 'ckpm', 'damagetochampions',
       'dpm', 'damageshare', 'damagetakenperminute',
       'damagemitigatedperminute', 'wardsplaced', 'wpm', 'wardskilled', 'wcpm',
       'controlwardsbought', 'visionscore', 'vspm', 'totalgold', 'earnedgold',
       'earned gpm', 'earnedgoldshare', 'goldspent', 'total cs', 'minionkills',
       'monsterkills', 'monsterkillsownjungle', 'monsterkillsenemyjungle',
       'cspm', 'goldat10', 'xpat10', 'csat10', 'opp_goldat10', 'opp_xpat10',
       'opp_csat10', 'golddiffat10', 'xpdiffat10', 'csdiffat10', 'goldat15',
       'xpat15', 'csat15', 'opp_goldat15', 'opp_xpat15', 'opp_csat15',
       'golddiffat15', 'xpdiffat15', 'csdiffat15']
nume = data.loc[:, column]
nume = nume.dropna(axis=0)
nume = nume.reset_index(drop=True)

# PCA
def getPC(df):
       """
       :param df: A dataframe with the first col denoting to champion
       :return: principle component
       """
       np.random.seed(100)
       X = scale(df.iloc[:, 1:].values, axis=1)
       pca = PCA(n_components=0.9)
       pca.fit(X)
       print(np.cumsum(pca.explained_variance_ratio_))
       X_new = pca.transform(X)
       loading = pd.DataFrame(pca.components_.T, columns=["F%d" %i for i in range(1, len(X_new[0])+1)])
       df_new = pd.DataFrame(X_new, columns=["F%d" %i for i in range(1,len(X_new[0])+1)])
       df_new = pd.concat([df.iloc[:, 0], df_new], axis=1)
       return df_new, loading
#print(X_new)
pca_result, loading = getPC(nume)
pca_result.to_csv("pca_result.csv")
# Show the groupby result
grby = pca_result.groupby("champion")
#print(list(grby))
#print(grby.agg("mean"))
#print(grby.agg("count")["F1"])
grby.agg("mean").to_csv("grby_pca.csv")
grby_pca = grby.agg("mean")

# Clustering using kmeans with princomp
def cluster(n, df):
       """
       :param n: # of clusters
       :param df: A dataframe with numeric values
       :return: clusters of the index
       """
       np.random.seed(100)
       km = KMeans(n)
       km.fit(df.values)
       result = pd.DataFrame({"champion":df.index, "cluster":km.labels_})
       #print(result.head())
       print(km.inertia_)
       return result
label = cluster(20, grby_pca)
label.to_csv("cluster_pca2.csv")


# Using data with match win/loss
column2 = ["result"] + column
nume2 = data.loc[:, column2]
nume2 = nume2.dropna(axis=0)
nume2 = nume2.reset_index(drop=True)
#print(nume2.head())
win = list(nume2.groupby("result"))[1][1]
loss = list(nume2.groupby("result"))[0][1]
chamwin = win.groupby("champion").agg("mean")
chamloss = loss.groupby("champion").agg("mean")
diff = set(chamloss.index.values)^set(chamwin.index.values)
chamwin = chamwin.loc[list(set(chamwin.index.values)-diff), ]
chamloss = chamloss.loc[list(set(chamloss.index.values)-diff), ]
column2_2 = [i+"_" for i in column2]
column2_2.pop(1)
chamloss.columns = column2_2
champ = pd.concat([chamwin.iloc[:, 1:], chamloss.iloc[:, 1:]], axis=1).reset_index()

# PCA
pca_result2, loading = getPC(champ)
pca_result2.index = pca_result2["champion"]
pca_result2 = pca_result2.iloc[:, 1:]
#print(pca_result2)
pca_result2.to_csv("pca winloss.csv")
loading.index = column2[2:] + column2_2[1:]
loading.to_csv("loading.csv")

# Looking for the best k
sse = np.array([])
for i in range(1, 31):
       X = pca_result2.values
       km_ = KMeans(i)
       km_.fit(X)
       sse = np.append(sse, km_.inertia_)
plt.plot(np.arange(1, 31), sse, "o-")
plt.xlabel("k")
plt.ylabel("sse")
plt.legend()
plt.show()
bestk = 10

# Clustering using kmeans with pc
label2 = cluster(bestk, pca_result2)
total = pd.concat([label2, pd.DataFrame({"champion": list(diff), "cluster": np.repeat(bestk, len(diff))})], axis=0).reset_index(drop=True)
#print(total)
total.to_csv("cluster_winloss10.csv")