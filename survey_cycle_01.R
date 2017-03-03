
#interactive graphs installed and load
library(plot3D)

#survey dataset you sent me//names it data, for the sake of 
data <- read.csv('file') 

names(data)

# Label columns that have quantitative data
# Note: for gender: 0 = Female, 1 = Male
quantCols <- c(6, 7, 9, 11:30, 32:48, 50, 52:56, 58:64, 66:76, 78:95, 97:104, 106:120, 
               122:132, 135, 138)

head(data[, quantCols])

qDat <- data[, quantCols]
dim(qDat) # need to lose 10 cols--drop ones with least info:

for (i in 1:ncol(qDat)) {
  print(names(qDat)[i])
  print(table(qDat[, i]))
}

# Merge:
# r7International 1, r8MustDo1, r13Pro1, 
# jAdmin 2, jAnalyst2, jFinance2, jSales2, jStrategy2, 
# Remove:
# appCityMapper2, appGarmin2,

# Merge some data to create few cols:
qDat$r17Other1 = 1 * (((!is.na(qDat$r7International) & qDat$r7International == 1) | 
                         (!is.na(qDat$r8MustDo) & qDat$r8MustDo == 1) | 
                         (!is.na(qDat$r13Pro) & qDat$r13Pro == 1)) |
                        qDat$r17Other1)
qDat$jOther = 1 * (((!is.na(qDat$jAdmin) & qDat$jAdmin == 1) |
                      (!is.na(qDat$jAnalyst) & qDat$jAnalyst == 1) |
                      (!is.na(qDat$jFinance) & qDat$jFinance == 1) |
                      (!is.na(qDat$jSales) & qDat$jSales == 1) |
                      (!is.na(qDat$jStrategy) & qDat$jStrategy == 1)) |
                     qDat$jOther)

names(qDat)[c(5, 7, 10, 14, 15, 43, 44, 72, 73, 78)]
qDat <- qDat[, -c(5, 7, 10, 14, 15, 43, 44, 72, 73, 78)]	

# PCA
# fill NAs with col means
for (f in 1:ncol(qDat)) {
  qDat[, f][is.na(qDat[, f])] <- mean(qDat[, f], na.rm = T)
}

# Scale data
for (f in 1:ncol(qDat)) {
  qDat[, f] <- scale(qDat[, f])
}

pca <- princomp(qDat, cor = T)
plot(pca, type = 'l') # First 4 PCs get most of the variance

# Plot PC projections
plot(pca$scores[, 1], pca$score[, 2], pch = 16, xlab = 'PC1', ylab = 'PC2', main = '')
abline(h = 0, col = 'grey', lty = 4); abline(v = 0, col = 'grey', lty = 4)

plot(pca$scores[, 3], pca$score[, 4], pch = 16, xlab = 'PC3', ylab = 'PC4', main = '')
abline(h = 0, col = 'grey', lty = 4); abline(v = 0, col = 'grey', lty = 4)

#plot(pca$scores[, 1], pca$score[, 3], pch = 16, xlab = 'PC1', ylab = 'PC3', main = '')
#abline(h = 0, col = 'grey', lty = 4); abline(v = 0, col = 'grey', lty = 4)

#plot(pca$scores[, 1], pca$score[, 4], pch = 16, xlab = 'PC1', ylab = 'PC4', main = '')
#abline(h = 0, col = 'grey', lty = 4); abline(v = 0, col = 'grey', lty = 4)

# PCs 1-3
scatter3D(pca$scores[, 1], 
          pca$scores[, 2], 
          pca$scores[, 3], 
          pch = 16, 
          xlab = 'PC1', 
          ylab = 'PC2', 
          zlab = 'PC3')

loadings(pca)




# Use only data that indicate the "type" of cyclist they might be
names(qDat)
importantVars <- c(11:34, 46:72)
redDat <- qDat[, importantVars]

pca2 <- princomp(redDat, cor = T)
plot(pca2, type = 'l') # First 8 PCs get most of the variance

# Plot PC projections
plot(
  pca2$scores[, 1], pca2$score[, 2], pch = 16, xlab = 'PC1', ylab = 'PC2', main = '')
abline(h = 0, col = 'grey', lty = 4); abline(v = 0, col = 'grey', lty = 4)

plot(
  pca2$scores[, 3], pca2$score[, 4], pch = 16, xlab = 'PC3', ylab = 'PC4', main = '')
abline(h = 0, col = 'grey', lty = 4); abline(v = 0, col = 'grey', lty = 4)

#plot(pca2$scores[, 1], pca2$score[, 3], pch = 16, xlab = 'PC1', ylab = 'PC3', main = '')
#abline(h = 0, col = 'grey', lty = 4); abline(v = 0, col = 'grey', lty = 4)

#plot(pca2$scores[, 1], pca2$score[, 4], pch = 16, xlab = 'PC1', ylab = 'PC4', main = '')
#abline(h = 0, col = 'grey', lty = 4); abline(v = 0, col = 'grey', lty = 4)

# PCs 1-3
scatter3D(pca2$scores[, 1], 
          pca2$scores[, 2], 
          pca2$scores[, 3], 
          pch = 16, 
          xlab = 'PC1', 
          ylab = 'PC2', 
          zlab = 'PC3')

loadings(pca2)


# Reduced data set
k2 <- kmeans(redDat, centers = 2L, iter.max = 100L, nstart = 100L)
(eta2 <- k2$betweenss / k2$totss) # 0.1470
plot(pca2$scores[, 1], 
     pca2$score[, 2], 
     pch = 16, 
     xlab = 'PC1', 
     ylab = 'PC2', 
     main = '', 
     col = k2$cluster)
abline(h = 0, col = 'grey', lty = 4); abline(v = 0, col = 'grey', lty = 4)

k3 <- kmeans(redDat, centers = 3L, iter.max = 100L, nstart = 100L)
(eta3 <- k3$betweenss / k3$totss) # 0.2254
plot(pca2$scores[, 1], 
     pca2$score[, 2], 
     pch = 16, 
     xlab = 'PC1', 
     ylab = 'PC2', 
     main = '', 
     col = k3$cluster)
abline(h = 0, col = 'grey', lty = 4); abline(v = 0, col = 'grey', lty = 4)

k4 <- kmeans(redDat, centers = 4L, iter.max = 100L, nstart = 100L)
(eta4 <- k4$betweenss / k4$totss) # 0.2699
par(mfrow = c(2, 2))
plot(pca2$scores[, 1], 
     pca2$score[, 2], 
     pch = 16, 
     xlab = 'PC1', 
     ylab = 'PC2', 
     main = '', 
     col = k4$cluster)
abline(h = 0, col = 'grey', lty = 4); abline(v = 0, col = 'grey', lty = 4)
plot(pca2$scores[, 4], 
     pca2$score[, 2], 
     pch = 16, 
     xlab = 'PC4', 
     ylab = 'PC2', 
     main = '', 
     col = k4$cluster)
abline(h = 0, col = 'grey', lty = 4); abline(v = 0, col = 'grey', lty = 4)
plot(pca2$scores[, 1], 
     pca2$score[, 3], 
     pch = 16, 
     xlab = 'PC1', 
     ylab = 'PC3', 
     main = '', 
     col = k4$cluster)
abline(h = 0, col = 'grey', lty = 4); abline(v = 0, col = 'grey', lty = 4)
plot(pca2$scores[, 4], 
     pca2$score[, 3], 
     pch = 16, 
     xlab = 'PC4', 
     ylab = 'PC3', 
     main = '', 
     col = k4$cluster)
abline(h = 0, col = 'grey', lty = 4); abline(v = 0, col = 'grey', lty = 4)


k5 <- kmeans(redDat, centers = 5L, iter.max = 100L, nstart = 100L)
(eta5 <- k5$betweenss / k5$totss) # 0.3220
par(mfrow = c(1, 1))
plot(pca2$scores[, 1], 
     pca2$score[, 2], 
     pch = 16, 
     xlab = 'PC1', 
     ylab = 'PC2', 
     main = '', 
     col = k5$cluster)
abline(h = 0, col = 'grey', lty = 4); abline(v = 0, col = 'grey', lty = 4)

k6 <- kmeans(redDat, centers = 6L, iter.max = 100L, nstart = 100L)
(eta6 <- k6$betweenss / k6$totss) # 0.35333
par(mfrow = c(1, 1))
plot(pca2$scores[, 1], 
     pca2$score[, 2], 
     pch = 16, 
     xlab = 'PC1', 
     ylab = 'PC2', 
     main = '', 
     col = k6$cluster)
abline(h = 0, col = 'grey', lty = 4); abline(v = 0, col = 'grey', lty = 4)

par(mfrow = c(2, 2))
plot(pca2$scores[, 1], 
     pca2$score[, 2], 
     pch = 16, 
     xlab = 'PC1', 
     ylab = 'PC2', 
     main = '', 
     col = k3$cluster)
abline(h = 0, col = 'grey', lty = 4); abline(v = 0, col = 'grey', lty = 4)
plot(pca2$scores[, 1], 
     pca2$score[, 2], 
     pch = 16, 
     xlab = 'PC1', 
     ylab = 'PC2', 
     main = '', 
     col = k4$cluster)
abline(h = 0, col = 'grey', lty = 4); abline(v = 0, col = 'grey', lty = 4)
plot(pca2$scores[, 1], 
     pca2$score[, 2], 
     pch = 16, 
     xlab = 'PC1', 
     ylab = 'PC2', 
     main = '', 
     col = k5$cluster)
abline(h = 0, col = 'grey', lty = 4); abline(v = 0, col = 'grey', lty = 4)
plot(pca2$scores[, 1], 
     pca2$score[, 2], 
     pch = 16, 
     xlab = 'PC1', 
     ylab = 'PC2', 
     main = '', 
     col = k6$cluster)
abline(h = 0, col = 'grey', lty = 4); abline(v = 0, col = 'grey', lty = 4)


k4.t <- table(k4$cluster)
(k4.t / sum(k4.t))
k4$centers


data$k4clust <- k4$cluster
data$k5clust <- k5$cluster
data$k6clust <- k6$cluster

c1.means <- apply(subset(qDat, k4$cluster == 1), 2, mean, na.rm = T)
c2.means <- apply(subset(qDat, k4$cluster == 2), 2, mean, na.rm = T)
c3.means <- apply(subset(qDat, k4$cluster == 3), 2, mean, na.rm = T)
c4.means <- apply(subset(qDat, k4$cluster == 4), 2, mean, na.rm = T)
names(qDat)
demo <- 1:10
media <- 35:45
spendBrands <- 73:95
favOwn <- 96:108


# BY CLUSTER

# Check cluster names and %s
(k4.t / sum(k4.t))
load('~/Desktop/Drew/code/kmeans.RData')


colnames(k4$centers)[1:8] <- c(
  'Cold', 'Snow - effort', 'Snow - slippery', 'Rain', 'Steep route', 'Heavy load',
  'Heat/\nhumidity', 'Other')
colnames(k4$centers)[9:24] <- c(
  'Departure time', 'Multiple trips', 'Fastest', 'Predictable time', 'Few cars', 
  'Slow traffic', 'Few parked cars', 'Continous route', 'Path w/ barrier', 
  'Bike signage', 'School/work encrg.', 'Colleagues cycle', 
  'Identify with culture', 'Fun', 'Way to reach dest.', 'Sports/Rec.')
colnames(k4$centers)[25:36] <- c(
  'Cycling club - y/n', 'Get away', 'Relax', 'Exciting', 'Family/friends', 
  'Meet new people', 'Social', 'Well-organized', 'Enjoy cycling', 'Lifestyle', 'Team', 
  'Other')
colnames(k4$centers)[37:51] <- c(
  'Race - y/n', 'Personal goal', 'Pers. challenge', 'Peer respect', 
  'Mind/body control', 'Health', 'Challenge', 'Annual event', 'Train/Qualify', 'Team',
  'Club', 'Self pride', 'Share identity', 'Addiction', 'Other')

names(c1.means)[1:10] <- names(c2.means)[4:10] <- names(c3.means)[4:10] <- names(c4.means)[4:10] <- c(
  'Gender', 'Age', 'Income', 'Other', 'Advert./Market.', 'Art', 'Consulting', 'HR', 
  'IT', 'Management')
names(c1.means)[media] <- names(c2.means)[media] <- names(c3.means)[media] <- names(c4.means)[media] <- c(
  'Blog - y/n', 'Apps - y/n', 'CitiBike', 'MapMyRide', 'Strava', 'Don\'t connect', 
  'Email', 'Facebook', 'Phone', 'Text', 'What\'sApp')
names(c1.means)[spendBrands] <- names(c2.means)[spendBrands] <- names(c3.means)[spendBrands] <- names(c4.means)[spendBrands] <- c(
  'Cycling Costs', 'Soft goods', 'Cycling store', 'Online Cy str', 
  'Sports retailer', 'Amazon', 'Walmart', 'Other', 'Avanti', 'Castelli', 'DeFeet',
  'DHB', 'Endura', 'Giro', 'Gore Bike Wthr', 'Grib Grab', 'Maloja', 'Morvelo', 'POC', 
  'Rapha', 'Sportful', 'Tifosi', 'None')
names(c1.means)[favOwn] <- names(c2.means)[favOwn] <- names(c3.means)[favOwn] <- names(c4.means)[favOwn] <- c(
  'Cadence - fav', 'Castelli - fav', 'Chrome - fav', 'DHB - fav', 
  'Prl Izumi - fav', 'Giro - fav', 'Rapha - fav', 'Spcialized - fav', 
  'Outlier - fav', 'None - fav', 'Castelli - own', 'Giro - own', 'Rapha - own')

# C1: Occasional/Fair-Weather Cyclists
par(mfrow = c(2, 4))
barplot(k4$centers[1, 1:8], 
        las = 2, 
        main = 'Cluster 1: Occasional/Fair-Weather Riders\n(45% of riders):\n"I don\'t cycle when..."',
        ylab = 'Standard deviations',
        ylim = c(-0.5, 1.2), 
        cex.name = 0.7,
        col = 1:8)
abline(h = 0, lwd = 3)
barplot(k4$centers[1, 9:24], 
        las = 2, 
        main = 'Cluster 1:\nImportant Factors', 
        ylim = c(-2, 1), 
        cex.name = 0.7,
        col = 9:24)
abline(h = 0, lwd = 3)
barplot(k4$centers[1, 25:36],
        las = 2,
        main = 'Cluster1:\nCyling Clubs',
        ylim = c(-1, 5),
        cex.name = 0.7,
        col = 25:36)
abline(h = 0, lwd = 3)
barplot(k4$centers[1, 37:51],
        las = 2,
        main = 'Cluster 1:\nRacing',
        ylim = c(-0.5, 2),
        cex.name = 0.7,
        col = 37:51)
abline(h = 0, lwd = 3)
barplot(c1.means[demo],
        las = 2,
        main = 'Cluster 1:\nDemographics',
        ylab = 'Standard deviations',
        ylim = c(-0.6, 2),
        cex.name = 0.7,
        col = 1:length(demo))
abline(h = 0, lwd = 3)
barplot(c1.means[media],
        las = 2,
        main = 'Cluster 1:\nMedia',
        ylim = c(-0.6, 2),
        cex.name = 0.7,
        col = 1:length(media))
abline(h = 0, lwd = 3)
barplot(c1.means[spendBrands],
        las = 2,
        main = 'Cluster 1:\nSpending & Recognized Brands',
        ylim = c(-0.5, 3),
        cex.name = 0.7,
        col = 1:length(spendBrands))
abline(h = 0, lwd = 3)
barplot(c1.means[favOwn],
        las = 2,
        main = 'Cluster 1:\nFavorites & Owned Brands',
        ylim = c(-0.8, 2.5),
        cex.name = 0.7,
        col = 1:length(favOwn))
abline(h = 0, lwd = 3)

# C2: The Hard-Core Cyclists
par(mfrow = c(2, 4))
barplot(k4$centers[2, 1:8], 
        las = 2, 
        main = 'Cluster 2: Lone-Wolf, Conscientious\n(41% of riders):\n"I don\'t cycle when..."', 
        ylab = 'Standard deviations',
        ylim = c(-0.5, 1.2), 
        cex.name = 0.7,
        col = 1:8)
abline(h = 0, lwd = 3)
barplot(k4$centers[2, 9:24], 
        las = 2, 
        main = 'Cluster 2:\nImportant Factors', 
        ylim = c(-2, 1), 
        cex.name = 0.7,
        col = 9:24)
abline(h = 0, lwd = 3)
barplot(k4$centers[2, 25:36],
        las = 2,
        main = 'Cluster 2:\nCyling Clubs',
        ylim = c(-1, 5),
        cex.name = 0.7,
        col = 25:36)
abline(h = 0, lwd = 3)
barplot(k4$centers[2, 37:51],
        las = 2,
        main = 'Cluster 2:\nRacing',
        ylim = c(-0.5, 2),
        cex.name = 0.7,
        col = 37:51)
abline(h = 0, lwd = 3)
barplot(c2.means[demo],
        las = 2,
        ylab = 'Standard deviations',
        main = 'Cluster 2:\nDemographics',
        ylim = c(-0.6, 2),
        cex.name = 0.7,
        col = 1:length(demo))
abline(h = 0, lwd = 3)
barplot(c2.means[media],
        las = 2,
        main = 'Cluster 2:\nMedia',
        ylim = c(-0.6, 2),
        cex.name = 0.7,
        col = 1:length(media))
abline(h = 0, lwd = 3)
barplot(c2.means[spendBrands],
        las = 2,
        main = 'Cluster 2:\nSpending & Recognized Brands',
        ylim = c(-0.5, 3),
        cex.name = 0.7,
        col = 1:length(spendBrands))
abline(h = 0, lwd = 3)
barplot(c2.means[favOwn],
        las = 2,
        main = 'Cluster 2:\nFavorites & Owned Brands',
        ylim = c(-0.8, 2.5),
        cex.name = 0.7,
        col = 1:length(favOwn))
abline(h = 0, lwd = 3)


# C3: Loner/Conscientious Cylists
par(mfrow = c(2, 4))
barplot(k4$centers[3, 1:8], 
        las = 2, 
        main = 'Cluster 3: Hard-Core Cyclists\n(11% of riders):\n"I don\'t cycle when..."', 
        ylab = 'Standard deviations',
        ylim = c(-0.5, 1.2), 
        cex.name = 0.7,
        col = 1:8)
abline(h = 0, lwd = 3)
barplot(k4$centers[3, 9:24], 
        las = 2, 
        main = 'Cluster 3:\nImportant Factors', 
        ylim = c(-2, 1), 
        cex.name = 0.7,
        col = 9:24)
abline(h = 0, lwd = 3)
barplot(k4$centers[3, 25:36],
        las = 2,
        main = 'Cluster 3:\nCyling Clubs',
        ylim = c(-1, 5),
        cex.name = 0.7,
        col = 25:36)
abline(h = 0, lwd = 3)
barplot(k4$centers[3, 37:51],
        las = 2,
        main = 'Cluster 3:\nRacing',
        ylim = c(-0.5, 2),
        cex.name = 0.7,
        col = 37:51)
abline(h = 0, lwd = 3)
barplot(c3.means[demo],
        las = 2,
        main = 'Cluster 3:\nDemographics',
        ylab = 'Standard deviations',
        ylim = c(-0.6, 2),
        cex.name = 0.7,
        col = 1:length(demo))
abline(h = 0, lwd = 3)
barplot(c3.means[media],
        las = 2,
        main = 'Cluster 3:\nMedia',
        ylim = c(-0.6, 2),
        cex.name = 0.7,
        col = 1:length(media))
abline(h = 0, lwd = 3)
barplot(c3.means[spendBrands],
        las = 2,
        main = 'Cluster 3:\nSpending & Recognized Brands',
        ylim = c(-0.5, 3),
        cex.name = 0.7,
        col = 1:length(spendBrands))
abline(h = 0, lwd = 3)
barplot(c3.means[favOwn],
        las = 2,
        main = 'Cluster 3:\nFavorites & Owned Brands',
        ylim = c(-0.8, 2.5),
        cex.name = 0.7,
        col = 1:length(favOwn))
abline(h = 0, lwd = 3)

# C4 Outliers--The Social Cyclists
par(mfrow = c(2, 4))
barplot(k4$centers[4, 1:8], 
        las = 2, 
        main = 'Cluster 4: Social Cyclists\n(3% of riders):\n"I don\'t cycle when..."', 
        ylab = 'Standard deviations',
        ylim = c(-0.5, 1.2), 
        cex.name = 0.7,
        col = 1:8)
abline(h = 0, lwd = 3)
barplot(k4$centers[4, 9:24], 
        las = 2, 
        main = 'Cluster 4:\nImportant Factors', 
        ylim = c(-2, 1), 
        cex.name = 0.7,
        col = 9:24)
abline(h = 0, lwd = 3)
barplot(k4$centers[4, 25:36],
        las = 2,
        main = 'Cluster 4:\nCyling Clubs',
        ylim = c(-1, 5),
        cex.name = 0.7,
        col = 25:36)
abline(h = 0, lwd = 3)
barplot(k4$centers[4, 37:51],
        las = 2,
        main = 'Cluster 4:\nRacing',
        ylim = c(-0.5, 2),
        cex.name = 0.7,
        col = 37:51)
abline(h = 0, lwd = 3)
barplot(c4.means[demo],
        las = 2,
        ylab = 'Standard deviations',
        main = 'Cluster 4:\nDemographics',
        ylim = c(-0.6, 2),
        cex.name = 0.7,
        col = 1:length(demo))
abline(h = 0, lwd = 3)
barplot(c4.means[media],
        las = 2,
        main = 'Cluster 4:\nMedia',
        ylim = c(-0.6, 2),
        cex.name = 0.7,
        col = 1:length(media))
abline(h = 0, lwd = 3)
barplot(c4.means[spendBrands],
        las = 2,
        main = 'Cluster 4:\nSpending & Recognized Brands',
        ylim = c(-0.5, 3),
        cex.name = 0.7,
        col = 1:length(spendBrands))
abline(h = 0, lwd = 3)
barplot(c4.means[favOwn],
        las = 2,
        main = 'Cluster 4:\nFavorites & Owned Brands',
        ylim = c(-0.8, 2.5),
        cex.name = 0.7,
        col = 1:length(favOwn))
abline(h = 0, lwd = 3)

data$cluster <- k4$cluster

#write.csv

