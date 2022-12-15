rm(list=ls())



# Call BDgraph package.
library("BDgraph")



# Read breast cancer data set from its excel file which is stored in Dropbox. 
#cancer = read.csv("C:\\Users\\ARA-SOFT\\Dropbox\\My Paper\\BGAT\\Code\\Breast cancer dataset\\cancer.csv", header = FALSE)



# Change rows' name in dataset.
row.names(cancer) = c("feture.1","feture.2", "feture.3", "feture.4", "feture.5", "feture.6", "feture.7", "feture.8", "feture.9", "feture.10",
                      "feture.11","feture.12", "feture.13", "feture.14", "feture.15", "feture.16", "feture.17", "feture.18", "feture.19", "feture.20",
                      "feture.21","feture.22", "feture.23", "feture.24", "feture.25", "feture.26", "feture.27", "feture.28", "feture.29", "feture.30") 



# Create histograms of the first 6 variables for all observations. 
par(mfrow = c(2, 3))
for(i in 1 : 6)
  hist(as.numeric(unlist(cancer[i,])), xlab = rownames(cancer)[i], main = NULL, cex.lab=1.8, cex.axis=1.6)



# Use bdgraph to estimate graph, p_link, selected_g and k_hat.
sample.bdmcmc <- bdgraph(data = cancer, method = "gcgm", g.prior = 0.1, 
                         iter = 10000, burnin = 7000, cores = 1)
output = summary(sample.bdmcmc)



# We save transpose gene data set to be usable in GNN.
cancer_t = t(cancer)
write.table(cancer_t, file = "cancer_t.csv", col.names = FALSE, row.names = FALSE, sep = ",")



# Use selected_g as the adjacency matrix, then make it symmetric to be usable in PyG.
output$selected_g
adj_cancer = output$selected_g + t(output$selected_g)
write.table(adj_cancer, file = "adj_cancer.csv", col.names = FALSE, row.names = FALSE, sep = ",")



# Use p_link as the attention, then make it symmetric and add ones to its diagonal.
output$p_links
p_link_cancer = output$p_links + t(output$p_links)+ diag(80)
write.table(p_link_cancer, file = "p_links_cancer.csv", col.names = FALSE, row.names = FALSE, sep = ",")



# Create labels for nodes.
labels = c()
labels[1:40] = 0
labels[41:80] = 1
write.table(labels, file = "labels_cancer.csv", col.names = FALSE, row.names = FALSE, sep = ",")  