rm(list = ls())
library(BDgraph)
setwd("C:\\Users\\lotus\\Dropbox\\My file\\My Paper\\BGAT_code")



gene = data("geneExpression",package="BDgraph")
col = colnames(geneExpression)
gi = strsplit(col, "_")
GI = sapply(gi , `[`, 1)
sum(GI == "GI")
h = strsplit(GI, "s")
H = sapply(h , `[`, 1)  
sum(H =='H') 
hm = strsplit(H, "m3")
hm = sapply(hm , `[`, 1)  
# 
hm = strsplit(hm, "m1")
hm = sapply(hm , `[`, 1)  
hm = strsplit(hm , "m9")
hm = sapply(hm , `[`, 1) 
colnames(geneExpression) = c(hm)

set.seed(00)
GI_g_sam = sample(which(colnames(geneExpression)=="GI"), 83)
GI_g_org = geneExpression[,GI_g_sam]

set.seed(11)
H_g_sam = sample(which(colnames(geneExpression)=="H"), 11)
H_g_org = geneExpression[,H_g_sam]


set.seed(22)
hm_g_sam = sample(which(colnames(geneExpression)=="hm"), 6)
hm_g_org = geneExpression[,hm_g_sam]


matrix_GI_aug = matrix(0, nrow = 60, ncol = 415)
matrix_GI = as.matrix(GI_g_org)
set.seed(10)                      
for(i in 1:60){
  for(j in 1:83){
    matrix_GI_aug[i, c(1:415)] = c(runif(415, matrix_GI[i,j] - 0.01, matrix_GI[i,j] + 0.01))
    }
}


set.seed(20)
matrix_H_aug = matrix(0, nrow = 60, ncol = 55)
matrix_H = as.matrix(H_g_org)
for(i in 1:60){
  for(j in 1:11){
    matrix_H_aug[i, c(1:55)] = c(runif(55, matrix_H[i,j] - 0.01, matrix_H[i,j] + 0.01)) 
    
  }
}


set.seed(30)
matrix_hm_aug = matrix(0, nrow = 60, ncol = 30)
matrix_hm = as.matrix(hm_g_org)
for(i in 1:60){
  for(j in 1:6){
    matrix_hm_aug[i, c(1:30)] = c(runif(30, matrix_hm[i,j] - 0.01, matrix_hm[i,j] + 0.01)) 
    
  }
}


matrix_gene_total = cbind(GI_g_org, matrix_GI_aug, H_g_org, matrix_H_aug,  hm_g_org, matrix_hm_aug)
dim(matrix_gene_total)
colnames(matrix_gene_total)=c(rep("GI", 498), rep("H", 66), rep("hm", 36))
rownames(matrix_gene_total) = NULL


matrix_gen_wr = matrix_gene_total[, c(1:30, 499:528, 571:600)]
dim(matrix_gen_wr)
colnames(matrix_gen_wr)



write.csv(t(matrix_gen_wr), "matrix_gen_wr.csv", row.names = F)

output = bdgraph(data = matrix_gen_wr, method = "gcgm",algorithm = "bdmcmc", g.prior = 0.3)
output = summary(output)

output$selected_g
adj_gene = output$selected_g + t(output$selected_g)
write.table(adj_gene, file = "adj_gene.csv", col.names = FALSE, row.names = FALSE, sep = ",")


output$p_links
p_link_gene = output$p_links + t(output$p_links) + diag(90)
write.table(p_link_gene, file = "p_links_gene.csv", col.names = FALSE, row.names = FALSE, sep = ",")

labels = c()
labels[1:30] = 0
labels[31:60] = 1
labels[61:90] = 2
write.table(labels, file = "labels_gene.csv", col.names = FALSE, row.names = FALSE, sep = ",") 


# Shapiro-Wilk test for normality
for (i in 1:90){
  shapiro_test_result <- shapiro.test(matrix_gen_wr[,i])  
  print(shapiro_test_result)
  print("***")
}
