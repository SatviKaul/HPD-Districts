
library(tidyverse)
library(clValid)
library(factoextra)


Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

soce <- read.csv("Book1.csv")

head(soce)

soce_1 <- select(soce, -("District"))

soce_1agg <- aggregate(. ~ State, data = soce_1 , FUN = mean)

head(soce_1agg)

pl <- ggplot(soce_1agg, aes(x = State, y =  IMR)) + geom_bar(stat = "identity",aes(fill = factor(State))) + theme(axis.text.x = element_text(angle = 90)) + geom_hline(yintercept = 34, color = "Black", size = 1)
pl

pl1 <- ggplot(soce_1agg, aes(x = State, y =  DR)) + geom_bar(stat = "identity",aes(fill = factor(State))) + theme(axis.text.x = element_text(angle = 90)) + geom_hline(yintercept = 54, color = "Black", size = 1)
pl1

pl2 <- ggplot(soce_1agg, aes(x = State, y =  U5MR)) + geom_bar(stat = "identity",aes(fill = factor(State))) + theme(axis.text.x = element_text(angle = 90)) + geom_hline(yintercept = 43, color = "Black", size = 1)
pl2

pl3 <- ggplot(soce_1agg, aes(x = State, y =  SRB)) + geom_bar(stat = "identity",aes(fill = factor(State))) + theme(axis.text.x = element_text(angle = 90)) + geom_hline(yintercept = 900, color = "Black", size = 1)
pl3

pl4 <- ggplot(soce_1agg, aes(x = State, y =  NNMR)) + geom_bar(stat = "identity",aes(fill = factor(State))) + theme(axis.text.x = element_text(angle = 90)) + geom_hline(yintercept = 28, color = "Black", size = 1)
pl4

pl5 <- ggplot(soce_1agg, aes(x = State, y =  TFR)) + geom_bar(stat = "identity",aes(fill = factor(State))) + theme(axis.text.x = element_text(angle = 90)) + geom_hline(yintercept = 2.33, color = "Black", size = 1)
pl5

pl6 <- ggplot(soce_1agg, aes(x = State, y =  CL)) + geom_bar(stat = "identity",aes(fill = factor(State))) + theme(axis.text.x = element_text(angle = 90)) + geom_hline(yintercept = 2.3, color = "Black", size = 1)
pl6

soce1 <- select(soce, -c("State","District"))

head(soce1)

soce1_sd <- scale(soce1)

head(soce1_sd)

fviz_nbclust(soce1, hcut, method = "wss")

#r_sq <- rnorm(20)
#for(number in 1:20){
#    clus <- kmeans(soce1, centers = number, nstart = 50)
#    r_sq[number] <- clus$betweenss/clus$totss
#}
#plot(r_sq, ylim = c(1,0))

fviz_nbclust(soce1, hcut, method = "silhouette")

#soce_dist <- dist(soce1_sd)

#avg_sil <- function(k){
 #  km.res <- kmeans(soce1_sd, centers = k, nstart = 25 )
  #  ss <- silhouette(km.res$cluster, dist(soce1_sd))
  #  mean(ss[, 3])
#}

#k.values <- 2:15
#avg_sil_values <- map_dbl(k.values, avg_sil)
#plot(k.values, avg_sil_values, type = "b", pch = 19, frame = FALSE, xlab = "Number of clusters K", ylab = "Average Silhouettes")


intern <- clValid(soce1,2:10, maxitems = nrow(soce1), clMethods =  c("kmeans"),validation=c("internal","stability"))
summary(intern)

intern <- clValid(soce1,2:10, maxitems = nrow(soce1), clMethods =  c("hierarchical"),validation=c("internal","stability"))
summary(intern)

set.seed(101)

clus2 <- kmeans(soce1_sd, centers = 2, iter.max = 50, nstart = 20)

clus2$size

clus4 <- kmeans(soce1_sd, centers = 4, iter.max = 50, nstart = 50)

clus4$size

clus5 <- kmeans(soce1_sd, centers = 5, iter.max = 50, nstart = 50)

clus5$size

clus6 <- kmeans(soce1_sd, centers = 6, iter.max = 50, nstart = 50)

clus6$size

soce_2km <- cbind(soce, clus2$cluster)


colnames(soce_2km)[14] <- "ClusterID"

soce_2km

write.csv(soce_2km, file = "Soce2.csv")

soce_4km <- cbind(soce, clus4$cluster)

colnames(soce_4km)[14] <- "ClusterID"

write.csv(soce_4km, file = "Soce4.csv")

soce_5km <- cbind(soce, clus5$cluster)

colnames(soce_5km)[14] <- "ClusterID"

write.csv(soce_5km, file = "Soce5.csv")

soce_6km <- cbind(soce, clus6$cluster)

colnames(soce_6km)[14] <- "ClusterID"

write.csv(soce_6km, file = "Soce6.csv")

head(soce_2km)
head(soce_4km)
head(soce_5km)
head(soce_6km)

Clus1_km <- subset(soce_2km, ClusterID == 1)
head(Clus1_km)

Clus2_km <- subset(soce_2km, ClusterID == 2)
head(Clus2_km)

Clus1_km4 <- subset(soce_4km, ClusterID == 1)

Clus2_km4 <- subset(soce_4km, ClusterID == 2)

Clus3_km4 <- subset(soce_4km, ClusterID == 3)

Clus4_km4 <- subset(soce_4km, ClusterID == 4)

Clus1_km5 <- subset(soce_5km, ClusterID == 1)

Clus2_km5 <- subset(soce_5km, ClusterID == 2)

Clus3_km5 <- subset(soce_5km, ClusterID == 3)

Clus4_km5 <- subset(soce_5km, ClusterID == 4)

Clus5_km5 <- subset(soce_5km, ClusterID == 5)

Clus1_km6 <- subset(soce_6km, ClusterID == 1)

Clus2_km6 <- subset(soce_6km, ClusterID == 2)

Clus3_km6 <- subset(soce_6km, ClusterID == 3)

Clus4_km6 <- subset(soce_6km, ClusterID == 4)

Clus5_km6 <- subset(soce_6km, ClusterID == 5)

Clus6_km6 <- subset(soce_6km, ClusterID == 6)

l1 <- list(table(Clus1_km$State),table(Clus2_km$State))

Assam <- as.vector(sapply(l1, function(x) x[1]))
Bihar <- as.vector(sapply(l1, function(x) x[2]))
Chhattisgarh <- as.vector(sapply(l1, function(x) x[3]))
Jharkhand <- as.vector(sapply(l1, function(x) x[4]))
Madhya_Pradesh <- as.vector(sapply(l1, function(x) x[5]))
Odisha <- as.vector(sapply(l1, function(x) x[6]))
Rajasthan <- as.vector(sapply(l1, function(x) x[7]))
Uttar_Pradesh <- as.vector(sapply(l1, function(x) x[8]))
Uttarakhand <- as.vector(sapply(l1, function(x) x[9]))

km1 <- group_by(soce_2km, ClusterID)

tab <- summarise(km1, Mean_DR = mean(DR), Mean_NNMR = mean(NNMR), Mean_CSP = mean(CSP), Mean_CL = mean(CL), Mean_CFI = mean(CFI), Mean_IMR = mean(IMR), Mean_SRB = mean(SRB), Mean_U5MR = mean(U5MR), Mean_LR = mean(LR), Mean_TFR = mean(TFR), Mean_SD = mean(SD))

tab <- cbind(tab, Assam, Bihar, Chhattisgarh, Jharkhand, Madhya_Pradesh, Odisha, Rajasthan, Uttar_Pradesh, Uttarakhand)

tab

km1 <- group_by(soce_2km, ClusterID)
tab1 <- summarise(km1, Mean_DR = mean(DR), Mode_DR = Mode(DR), Median_DR = median(DR), Range_DR = max(DR)-min(DR))

tab1 <- cbind(tab1, Assam, Bihar, Chhattisgarh, Jharkhand, Madhya_Pradesh, Odisha, Rajasthan, Uttar_Pradesh, Uttarakhand)

tab1

km2 <- group_by(soce_2km, ClusterID)
tab2 <- summarise(km2, Mean_LR = mean(LR), Mode_LR = Mode(LR), Median_LR = median(LR), Range_LR = max(LR)-min(LR))

tab2 <- cbind(tab2, Assam, Bihar, Chhattisgarh, Jharkhand, Madhya_Pradesh, Odisha, Rajasthan, Uttar_Pradesh, Uttarakhand)

tab2

km3 <- group_by(soce_2km, ClusterID)
tab3 <- summarise(km3, Mean_CSP = mean(CSP), Mode_CSP = Mode(CSP), Median_CSP = median(CSP), Range_CSP = max(CSP)-min(CSP))

tab3 <- cbind(tab3, Assam, Bihar, Chhattisgarh, Jharkhand, Madhya_Pradesh, Odisha, Rajasthan, Uttar_Pradesh, Uttarakhand)

tab3

km4 <- group_by(soce_2km, ClusterID)
tab4 <- summarise(km4, Mean_CL = mean(CL), Mode_CL = Mode(CL), Median_CL = median(CL), Range_CL = max(CL)-min(CL))

tab4 <- cbind(tab4, Assam, Bihar, Chhattisgarh, Jharkhand, Madhya_Pradesh, Odisha, Rajasthan, Uttar_Pradesh, Uttarakhand)

tab4

km5 <- group_by(soce_2km, ClusterID)
tab5 <- summarise(km5, Mean_NNMR = mean(NNMR), Mode_NNMR = Mode(NNMR), Median_NNMR = median(NNMR), Range_NNMR = max(NNMR)-min(NNMR))

tab5 <- cbind(tab5, Assam, Bihar, Chhattisgarh, Jharkhand, Madhya_Pradesh, Odisha, Rajasthan, Uttar_Pradesh, Uttarakhand)

tab5

km6 <- group_by(soce_2km, ClusterID)
tab6 <- summarise(km6, Mean_IMR = mean(IMR), Mode_IMR = Mode(IMR), Median_IMR = median(IMR), Range_IMR = max(IMR)-min(IMR))

tab6 <- cbind(tab6, Assam, Bihar, Chhattisgarh, Jharkhand, Madhya_Pradesh, Odisha, Rajasthan, Uttar_Pradesh, Uttarakhand)

tab6

km7 <- group_by(soce_2km, ClusterID)
tab7 <- summarise(km7, Mean_SRB = mean(SRB), Mode_SRB = Mode(SRB), Median_SRB = median(SRB), Range_SRB = max(SRB)-min(SRB))

tab7 <- cbind(tab7, Assam, Bihar, Chhattisgarh, Jharkhand, Madhya_Pradesh, Odisha, Rajasthan, Uttar_Pradesh, Uttarakhand)

tab7

km8 <- group_by(soce_2km, ClusterID)
tab8 <- summarise(km8, Mean_U5MR = mean(U5MR), Mode_U5MR = Mode(U5MR), Median_U5MR = median(U5MR), Range_U5MR = max(U5MR)-min(U5MR))

tab8 <- cbind(tab8, Assam, Bihar, Chhattisgarh, Jharkhand, Madhya_Pradesh, Odisha, Rajasthan, Uttar_Pradesh, Uttarakhand)

tab8

km9 <- group_by(soce_2km, ClusterID)
tab9 <- summarise(km9, Mean_TFR = mean(TFR), Mode_TFR = Mode(TFR), Median_TFR = median(TFR), Range_TFR = max(TFR)-min(TFR))

tab9 <- cbind(tab9,Assam, Bihar, Chhattisgarh, Jharkhand, Madhya_Pradesh, Odisha, Rajasthan, Uttar_Pradesh, Uttarakhand)

tab9

km10 <- group_by(soce_2km, ClusterID)
tab10 <- summarise(km10, Mean_SD = mean(SD), Mode_SD = Mode(SD), Median_SD = median(SD), Range_SD = max(SD)-min(SD))

tab10 <- cbind(tab10, Assam, Bihar, Chhattisgarh, Jharkhand, Madhya_Pradesh, Odisha, Rajasthan, Uttar_Pradesh, Uttarakhand)

tab10

km11 <- group_by(soce_2km, ClusterID)
tab11 <- summarise(km11, Mean_CFI = mean(CFI), Median_CFI = median(CFI), Mode_CFI = Mode(CFI), Range_CFI = max(CFI)-min(CFI))

tab11 <- cbind(tab11, Assam, Bihar, Chhattisgarh, Jharkhand, Madhya_Pradesh, Odisha, Rajasthan, Uttar_Pradesh, Uttarakhand)

tab11

hpd <- read.csv("highprioritydistricts.csv")

head(hpd,10)

dist_hpd <- as.vector(hpd$District)
length(dist_hpd)

dist_km <- as.vector(Clus2_km$District)

dist_km1 <- as.vector(Clus2_km4$District)

dist_km2 <- as.vector(Clus3_km4$District)

m <- match(dist_hpd,dist_km)
m


length(m)

(1-(16/89))*100

l2 = list(table(Clus1_km4$State),table(Clus2_km4$State),table(Clus3_km4$State),table(Clus4_km4$State))

Assam <- as.vector(sapply(l2, function(x) x[1]))
Bihar <- as.vector(sapply(l2, function(x) x[2]))
Chhattisgarh <- as.vector(sapply(l2, function(x) x[3]))
Jharkhand <- as.vector(sapply(l2, function(x) x[4]))
Madhya_Pradesh <- as.vector(sapply(l2, function(x) x[5]))
Odisha <- as.vector(sapply(l2, function(x) x[6]))
Rajasthan <- as.vector(sapply(l2, function(x) x[7]))
Uttar_Pradesh <- as.vector(sapply(l2, function(x) x[8]))
Uttarakhand <- as.vector(sapply(l2, function(x) x[9]))

km_1 <- group_by(soce_4km, ClusterID)

tab_1 <- summarise(km_1, Mean_DR = mean(DR), Mean_NNMR = mean(NNMR), Mean_CSP = mean(CSP), Mean_CL = mean(CL), Mean_CFI = mean(CFI), Mean_IMR = mean(IMR), Mean_SRB = mean(SRB), Mean_U5MR = mean(U5MR), Mean_LR = mean(LR), Mean_TFR = mean(TFR), Mean_SD = mean(SD))

tab_1 <- cbind(tab_1, Assam, Bihar, Chhattisgarh, Jharkhand, Madhya_Pradesh, Odisha, Rajasthan, Uttar_Pradesh, Uttarakhand)

tab_1

l3 = list(table(Clus1_km5$State),table(Clus2_km5$State),table(Clus3_km5$State),table(Clus4_km5$State), table(Clus5_km5$State))


Assam <- as.vector(sapply(l3, function(x) x[1]))
Bihar <- as.vector(sapply(l3, function(x) x[2]))
Chhattisgarh <- as.vector(sapply(l3, function(x) x[3]))
Jharkhand <- as.vector(sapply(l3, function(x) x[4]))
Madhya_Pradesh <- as.vector(sapply(l3, function(x) x[5]))
Odisha <- as.vector(sapply(l3, function(x) x[6]))
Rajasthan <- as.vector(sapply(l3, function(x) x[7]))
Uttar_Pradesh <- as.vector(sapply(l3, function(x) x[8]))
Uttarakhand <- as.vector(sapply(l3, function(x) x[9]))

km_2 <- group_by(soce_5km, ClusterID)

tab_2 <- summarise(km_2, Mean_DR = mean(DR), Mean_NNMR = mean(NNMR), Mean_CSP = mean(CSP), Mean_CL = mean(CL), Mean_CFI = mean(CFI), Mean_IMR = mean(IMR), Mean_SRB = mean(SRB), Mean_U5MR = mean(U5MR), Mean_LR = mean(LR), Mean_TFR = mean(TFR), Mean_SD = mean(SD))

tab_2 <- cbind(tab_2, Assam, Bihar, Chhattisgarh, Jharkhand, Madhya_Pradesh, Odisha, Rajasthan, Uttar_Pradesh, Uttarakhand)

tab_2

l4 = list(table(Clus1_km6$State),table(Clus2_km6$State),table(Clus3_km6$State),table(Clus4_km6$State), table(Clus5_km6$State), table(Clus6_km6$State))

Assam <- as.vector(sapply(l4, function(x) x[1]))
Bihar <- as.vector(sapply(l4, function(x) x[2]))
Chhattisgarh <- as.vector(sapply(l4, function(x) x[3]))
Jharkhand <- as.vector(sapply(l4, function(x) x[4]))
Madhya_Pradesh <- as.vector(sapply(l4, function(x) x[5]))
Odisha <- as.vector(sapply(l4, function(x) x[6]))
Rajasthan <- as.vector(sapply(l4, function(x) x[7]))
Uttar_Pradesh <- as.vector(sapply(l4, function(x) x[8]))
Uttarakhand <- as.vector(sapply(l4, function(x) x[9]))

km_3 <- group_by(soce_6km, ClusterID)

tab_3 <- summarise(km_3, Mean_DR = mean(DR), Mean_NNMR = mean(NNMR), Mean_CSP = mean(CSP), Mean_CL = mean(CL), Mean_CFI = mean(CFI), Mean_IMR = mean(IMR), Mean_SRB = mean(SRB), Mean_U5MR = mean(U5MR), Mean_LR = mean(LR), Mean_TFR = mean(TFR), Mean_SD = mean(SD))

tab_3 <- cbind(tab_3, Assam, Bihar, Chhattisgarh, Jharkhand, Madhya_Pradesh, Odisha, Rajasthan, Uttar_Pradesh, Uttarakhand)

tab_3


