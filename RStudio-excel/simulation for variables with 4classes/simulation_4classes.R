rm(list=ls())



# Call BDgraph package.
library(BDgraph)



# Creating an empty matrix with 500 rows and 40 columns, 
# since We want to have a multivariate data set with 500 observations and 40 variables. 
df1 = matrix(ncol = 40, nrow = 500)
 

 
# Here we have 4 classes and for each we have 10 variables (columns), 
#so to assign to each column a label randomly, we sample from indexes of columns of df1, 4 times.
set.seed(0)
y = 1:40
a = sample(y, 40)
class_1 = a[1:10]
class_2 =a[11:20]
class_3 = a[21:30]
class_4 = a[31:40]
# Up to here it is obvious among the columns of df1 which of them belong to 
#class_1, class_2, class_3 and class_4.



# Assign to each element of class_1 an element from bdgraph.sim 
# with special quality(random graph and prob = 0.2(shows the sparsity)). 
set.seed(1)
data_1 = bdgraph.sim(n = 500, p = 10, graph = "random", prob = 0.2)
for (i in 1:500){
  for (j in class_1){
    df1[i,j] = data_1$data[i,match(j,class_1)]
  }
}



# Assign to each element of class_2 an element from bdgraph.sim 
# with special quality(random graph and prob = 0.8). 
set.seed(2)
data_2 = bdgraph.sim(n = 500, p = 10, graph = "random", prob = 0.8)
for (i in 1:500){
  for (j in class_2){
    df1[i,j] = data_2$data[i,match(j,class_2)]
  }
}



# Assign to each element of class_3 an element from bdgraph.sim 
# with special quality(hub graph). 
set.seed(3)
data_3 = bdgraph.sim(n = 500, p = 10, graph = "hub")
for (i in 1:500){
  for (j in class_3){
    df1[i,j] = data_3$data[i,match(j,class_3)]
  }
}



# Assign to each element of class_4 an element from bdgraph.sim 
# with special quality(scale-free graph).
set.seed(4) 
data_4 = bdgraph.sim(n = 500, p = 10, graph = "scale-free")
for (i in 1:500){
  for (j in class_4){
    df1[i,j] = data_4$data[i,match(j,class_4)]
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
write.table(df1_t, file = "df1_t.csv", col.names = FALSE, row.names = FALSE, sep = ",")



# Use bdgraph to estimate graph, p_link, selected_g and k_hat. 
sample.bdmcmc = bdgraph(data = df1,method = "ggm",algorithm = "bdmcmc")
output= summary(sample.bdmcmc)



# Use selected_g as the adjacency matrix, then make it symmetric to be usable in PyG.
output$selected_g
adj_diag_4 = output$selected_g +t(output$selected_g)
write.table(adj_diag_4, file = "adj_diag_4.csv", col.names = FALSE, row.names = FALSE, sep = ",")



# Use p_link as the attention, then make it symmetric and add ones to its diagonal.
output$p_links
p_link = output$p_links +t(output$p_links)+diag(40)
write.table(p_link, file = "p_links.csv", col.names = FALSE, row.names = FALSE, sep = ",")



# Adjacency matrix of current graph in order to compare with the graph that bdgraph estimates.
adj = matrix(ncol = 40, nrow = 40) 
for (i in class_1){
  for (j in class_1){
    adj[i,j] = data_1$G[match(i,class_1),match(j,class_1)]
  }
}

for (i in class_2){
  for (j in class_2){
    adj[i,j] = data_2$G[match(i,class_2),match(j,class_2)]
  }
}

for (i in class_3){
  for (j in class_3){
    adj[i,j] = data_3$G[match(i,class_3),match(j,class_3)]
  }
}

for (i in class_4){
  for (j in class_4){
    adj[i,j] = data_4$G[match(i,class_4),match(j,class_4)]
  }
}

for (i in 1:40){
  for(j in 1:40){
    if (adj[i,j] %in% NA) {adj[i,j]=0}
  }
}

compare(adj, sample.bdmcmc, main = c( "True", "GGM" ), vis = TRUE)
conf.mat( actual =  adj, pred = sample.bdmcmc )
conf.mat.plot( actual = adj, pred = sample.bdmcmc, color = c( "#4169E1", "#00008B"  ) )



# Create labels for nodes.
labels = c()
for(i in 1:40){
  if (i %in% class_1){labels[i]=0}
  else if(i %in% class_2){labels[i]=1}  
  else if (i %in% class_3){labels[i]=2}
  else if (i %in% class_4){labels[i]=3}
}
print(labels)
write.table(labels, file = "labels.csv", col.names = FALSE, row.names = FALSE, sep = ",")