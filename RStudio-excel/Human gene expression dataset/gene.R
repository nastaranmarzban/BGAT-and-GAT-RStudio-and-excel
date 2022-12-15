# rm(list = ls())
# library("BDgraph")
# gene = data("geneExpression",package="BDgraph")
# (col = colnames(geneExpression))
# 
#
#
####################Change the column's name in geneExpression.
# gi = strsplit(col, "_")
# GI = sapply(gi , `[`, 1)
#
# h = strsplit(GI, "s")
# H = sapply(h , `[`, 1)  
# 
# hm = strsplit(H, "m3")
# hm = sapply(hm , `[`, 1)  
# 
# hm = strsplit(hm, "m1")
# hm = sapply(hm , `[`, 1) 
# 
# hm = strsplit(hm, "m9")
# hm = sapply(hm , `[`, 1) 
# colnames(geneExpression) = c(hm)
# 
# 
# 
####################Diagnosing which column has "h" and "hmm".
# h = c()
# for (i in 1:100){
#   if ("H" %in% colnames(geneExpression)[i] ) {h[i] = i}
# }
# h = na.omit(h)
# 
# hmm = c()
# for (i in 1:100){
#   if ("hm" %in% colnames(geneExpression)[i] ) {hmm[i] = i}
# }
# hmm = na.omit(hmm)
# 
# 
#################### Making the data which is balanced.  
# sample_GI = c()
# for (i in 1:100){
#   if ("GI" %in% colnames(geneExpression)[i] ) {sample_GI[i] = i}
# }
# sample_GI = na.omit(sample_GI)
# 
# sample = sample(sample_GI, 22)
# GI =  geneExpression[, sample] 
# hmm =  geneExpression[, hmm]
# H =  geneExpression[, h]
##### We have just made a dataset 60*39.
#
#
#
#####Simulate from uniform to increase variables with name of H and hmm.
# df1 = data.frame()
# df2 = data.frame()
# 
# 
# for(i in 1:60){
#   for(j in 1:15){
#     df1[i,j] = 5+10*runif(1)
#     
#   }
# }
# 
# 
# 
# for(i in 1:60){
#   for(j in 1:11){
#     df2[i,j] = 6+10*runif(1)
#     
#   }
# }
# 

##### Our complete data, gene.
# gene = data.frame(GI, hmm, H, df1, df2)
# write.table(gene, file = "gene.csv", col.names = FALSE, row.names = FALSE, sep = ",")


######### By doing all above we have data set 60*65. we save it in Dropbox to use it later on.########## 



#gene = read.csv("C:\\Users\\ARA-SOFT\\Dropbox\\My Paper\\BGAT\\Code\\Human gene expression dataset\\gene.csv")



# We save transpose gene data set to be usable in GNN.
gene_t = t(gene)
write.table(gene_t, file = "gene_t.csv", col.names = FALSE, row.names = FALSE, sep = ",")



# Make name for each column.
colnames(gene)[1:22] = c('GI1','GI2','GI3','GI4','GI5','GI6','GI7','GI8','GI9','GI10','GI11','GI12','GI13','GI14','GI15', 'GI16','GI17','GI18','GI19','GI20','GI21', 'GI22')

colnames(gene)[23:28] = c('hm1','hm2','hm3','hm4','hm5','hm6')
colnames(gene)[40:54] = c('hm7','hm8','hm9','hm10','hm11','hm12', 'hm13','hm14','hm15','hm16','hm17','hm18','hm19','hm20','hm21') 


colnames(gene)[29:39] = c('hs1','hs2','hs3','hs4','hs5','hs6','hs7','hs8','hs9','hs10','hs11') 
colnames(gene)[55:65] = c ('hs12','hs13','hs14','hs15','hs16','hs17','hs18','hs19','hs20','hs21', 'hs22') 



# Create histograms of the first 6 variables. 
par(mfrow = c(2, 3))
for(i in 1 : 6)
  hist(gene[ , i ], xlab = colnames(gene)[ i ], main = NULL, cex.lab=1.8, cex.axis=1.6)



# Use bdgraph to estimate graph, p_link, selected_g and k_hat.
sample.bdmcmc = bdgraph(data = gene, method = "gcgm", g.prior = 0.3)
output = summary(sample.bdmcmc)



# Use selected_g as the adjacency matrix, then make it symmetric to be usable in PyG. 
output$selected_g
adj_gene = output$selected_g + t(output$selected_g)
write.table(adj_gene, file = "adj_gene.csv", col.names = FALSE, row.names = FALSE, sep = ",")



# Use p_link as the attention, then make it symmetric and add ones to its diagonal.
output$p_links
p_link_gene = output$p_links + t(output$p_links) + diag(65)
write.table(p_link_gene, file = "p_links_gene.csv", col.names = FALSE, row.names = FALSE, sep = ",")



# Create labels for nodes.
labels = c()
labels[1:22] = 0
labels[23:28] = 1
labels[40:54] = 1
labels[29:39] = 2
labels[55:65] = 2
write.table(labels, file = "labels_gene.csv", col.names = FALSE, row.names = FALSE, sep = ",")  