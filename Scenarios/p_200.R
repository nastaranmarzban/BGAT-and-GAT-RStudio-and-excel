rm(list=ls())

# Call BDgraph package.
library(BDgraph)

# Creating an empty matrix with 500 rows and 200 columns, 
# since We want to have a multivariate data set with 500 observations and 200 variables. 
df1 = matrix(ncol = 200, nrow = 500)


# Here we have 5 classes and for each we have 40 variables (columns), 
#so to assign to each column a label randomly, we sample from indexes of columns of df1.
set.seed(0)
y = 1:200
a = sample(y, 200)
class_1 = a[1:40]
class_2 =a[41:80]
class_3 = a[81:120]
class_4 = a[121:160]
class_5 = a[161:200]
# Up to here it is obvious among the columns of df1 which of them belong to 
#class_0, class_1, class_2 and class_3, class_4, and class_5.


# Assign to each element of class_0 an element from bdgraph.sim 
# with special quality(random graph and prob = 0.2(shows the sparsity) and type = non-Gaussian). 
set.seed(1)
data_1 = bdgraph.sim(n = 500, p = 40, graph = "random", prob = 0.2,
                     type = "non-Gaussian")
for (i in 1:500){
  for (j in class_1){
    df1[i,j] = data_1$data[i,match(j,class_1)]
  }
}


# Assign to each element of class_1 an element from bdgraph.sim 
# with special quality(random graph and prob = 0.8 and type = Gaussian). 
set.seed(2)
data_2 = bdgraph.sim(n = 500, p = 40, graph = "random", prob = 0.8)
for (i in 1:500){
  for (j in class_2){
    df1[i,j] = data_2$data[i,match(j,class_2)]
  }
}
data_2$data


# Assign to each element of class_2 an element from bdgraph.sim 
# with special quality(hub graph and type = non-Gaussian). 
set.seed(3)
data_3 = bdgraph.sim(n = 500, p = 40, graph = "hub", type = "non-Gaussian")
for (i in 1:500){
  for (j in class_3){
    df1[i,j] = data_3$data[i,match(j,class_3)]
  }
}


# Assign to each element of class_3 an element from bdgraph.sim 
# with special quality(scale-free graph and type = Gaussian).
set.seed(4) 
data_4 = bdgraph.sim(n = 500, p = 40, graph = "scale-free")
for (i in 1:500){
  for (j in class_4){
    df1[i,j] = data_4$data[i,match(j,class_4)]
  }
}


# Assign to each element of class_4 an element from bdgraph.sim 
# with special quality(cluster graph and type = Gaussian).
set.seed(5) 
data_5 = bdgraph.sim(n = 500, p = 40, graph = "cluster")
for (i in 1:500){
  for (j in class_5){
    df1[i,j] = data_5$data[i,match(j,class_5)]
  }
}


# To see if there is duplicate data in df1.
vec = c(df1)
duplicated(vec)
vec[duplicated(vec)]


# Transpose the multivariate data set to use them in GNN 
# (in PyG the nodes(variables) are set in rows and features(observations) are set in columns)
# and then save it to be usable for Gdrive.
df1_t = t(df1)
write.table(df1_t, file = "df1_t_200.csv", col.names = FALSE, row.names = FALSE, sep = ",")


# Use bdgraph to estimate graph, p_link, selected_g and k_hat. 
sample.bdmcmc = bdgraph(data = df1, method = "gcgm",algorithm = "bdmcmc")
output= summary(sample.bdmcmc)
sum(output$selected_g==1)


# Use selected_g as the adjacency matrix, then make it symmetric to be usable in PyG.
sum(output$selected_g==1)
adj_diag_4 = output$selected_g +t(output$selected_g)
write.table(adj_diag_4, file = "adj_diag_200.csv", col.names = FALSE, row.names = FALSE, sep = ",")


# Use p_link as the attention, then make it symmetric and add ones to its diagonal.
output$p_links
p_link = output$p_links +t(output$p_links)+diag(200)
write.table(p_link, file = "p_links_200.csv", col.names = FALSE, row.names = FALSE, sep = ",")


# Create labels for nodes.
labels = c()
for(i in 1:200){
  if (i %in% class_1){labels[i]=0}
  else if(i %in% class_2){labels[i]=1}  
  else if (i %in% class_3){labels[i]=2}
  else if (i %in% class_4){labels[i]=3}
  else {labels[i]=4}
}
print(labels)
write.table(labels, file = "labels_200.csv", col.names = FALSE, row.names = FALSE, sep = ",")
getwd()
