### 硬分类 和 软分类
<https://zh.wikipedia.org/wiki/%E6%A8%A1%E7%B3%8A%E8%81%9A%E7%B1%BB>

### k-means对比k-medoids
<https://blog.csdn.net/coder_Gray/article/details/79705032>
PAM算法
<https://blog.csdn.net/panjiao119/article/details/72866009?utm_medium=distribute.pc_relevant.none-task-blog-BlogCommendFromMachineLearnPai2-1.channel_param&depth_1-utm_source=distribute.pc_relevant.none-task-blog-BlogCommendFromMachineLearnPai2-1.channel_param>

### 汉明距离
<https://zh.wikipedia.org/wiki/%E6%B1%89%E6%98%8E%E8%B7%9D%E7%A6%BB>

### Levenshtein距离
<https://zh.wikipedia.org/wiki/%E8%90%8A%E6%96%87%E6%96%AF%E5%9D%A6%E8%B7%9D%E9%9B%A2>

### index of dissimilarity (Indice de dissimilarité)

### jaccard 距离 就是属于dissimilarity距离
用来评估两个样本之间的相似性
<https://fr.wikipedia.org/wiki/Indice_et_distance_de_Jaccard>

Kmeans 

fpc
graogics

pam


### 阶层式集群 (Hierarchical clustering)  (HAC)
hclust函数 中间有method的参数
method表示类的合并方法，有：
single            最短距离法
complete        最长距离法 是指hac中，，cluster1聚类扩展的时候，优先扩展离1距离远的cluster2，然后在扩展离1近的cluster3
median        中间距离法
mcquitty        相似法
average        类平均法
centroid        重心法
ward            离差平方和法

### DBSCAN
DBSCAN函数
<http://blog.fens.me/r-cluster-dbscan/>

### Cross-Validation（交叉检验）

### LVQ算法
lvqtest
olvq1
得到混淆矩阵


lvqtest
lvq1
得到混淆矩阵

### 关联规则学习 (Règle d'association)
<https://zh.wikipedia.org/wiki/%E5%85%B3%E8%81%94%E8%A7%84%E5%88%99%E5%AD%A6%E4%B9%A0>

关联分析中的：
支持度和置信度
<https://blog.csdn.net/sanqima/article/details/42746419>
组合数学 (Combinatoire)
贪婪算法
inspect
quality


apriori规则的方法：
最好的min-support应该根据item数和转移initial来选出
https://www.cnblogs.com/dm-cc/p/5737147.html

apriori(data, parameter = NULL, appearance = NULL, control = NULL)
参数介绍：

data：数据；

parameter：设置参数，默认情况下parameter=list(support=0.1,confidence=0.8,maxlen=10,minlen=1,target=”rules”)

supp：支持度（support）

conf：置信度（confidence）

maxlen，minlen：每个项集所含项数的最大最小值

target：“rules”或“frequent itemsets”（输出关联规则/频繁项集） 如果是frequent那么只看support

max fre：
https://blog.csdn.net/u013007900/article/details/54743395

TP!!!
http://blog.fens.me/r-linear-regression/
https://zhuanlan.zhihu.com/p/49149862
fitted value 是lm函数之后的预测值

学生化残差和标准化残差
https://www.pinggu.com/post/details/5bc9543b35b47a439b35f576
异常点的检测

