######## Multivariate Statistical Analysisby Petrella Riccardo
rm(list = rm())
require(readxl)   
require(psych)
require(rgl)
require(ks)
require(HSAUR)
require(pls)
require(car)
require(plotly)
require(lmtest)
require(outliers)
dati <- read_xlsx("highways_data.xlsx")

str(dati)
summary(dati)
#boxplots 
for (i in 2:7){boxplot(dati[,i], show.names= T)}

#dataset with only quantitative variables
dati.quant <- dati[,2:7]
dati.quant <- data.frame(dati.quant)



#we standardize each variable on the number of kilometers
dati.std <- dati.quant/dati.quant[,1]
View(dati.std)
var(dati.std)
### skewness and curtosis 
skew(dati.std)
kurtosi(dati.std)

database <- cbind(dati.std, dati[,8:9])  #we now ad the binary variables to the standardized df
str(database)


####Univariate explorative analysis for qualitative variables


table(database$gallerie)  #absolute frequencies of the tunnels (gallerie = tunnels in italian)

barplot(table(database$gallerie)) #barplot of the tunnels

table(database$percorso)   # percorso = precense or absence of curves

barplot(table(database$percorso))

#histograms and density kernel estimation with Epanechnicov. (for quantitat. variables)

for (j in 2:6){
  hist(dati.std[,j], freq = F)
  lines(density(dati.std[,j], kernel = "epanechnikov"), col = 3, lwd = 2 )
}


#####multivariate graphical representation
# for simplicity we exclude "km" from the standardized dataset:
dati.std <- dati.std[,2:6]
dati.std

# 2d scatterplot

plot(dati.std)
plot(density(dati.std$media.veicoli.giornalieri.medi) )  


pairs.panels(dati.std, stars = T, ci = T)

# boxplot with conditional distributions of accidents (incidenti) given the 2 qualitative variables

boxplot(dati.std$incidenti ~ database$percorso)
boxplot(dati.std$incidenti ~ database$gallerie)



# joint density estimation + level curves of some pairs of predictors
plot(kde(dati.std[,c(1,2) ] ), col = 2, lwd = 2)
points(dati.std[,c(1,2) ] )

plot(kde(dati.std[,c(2,3) ] ), col = 2, lwd = 2)
points(dati.std[,c(2,3) ] )

plot(kde(dati.std[,4:5] ), col = 2, lwd = 2)
points(dati.std[,4:5] )

# covariance matrix
matr.cov <- cov(dati.std)


#### PCA 


pairs.panels(dati.std, stars = T, ci = T)
# very high correlation indeces.

analisi.pca <- prcomp(dati.std, scale = T) # good practice to scale the data with pca

analisi.pca
names(analisi.pca)
analisi.pca$sdev# roots of lambda = std dev of the principal components
analisi.pca$x  # new variables 

# how to interpret Yi?
summary(analisi.pca)

# barplot for each Yi
for (i in (1:5)){
  #barplot((ris.pca$rotation[,i]),main = paste(c("weight of the PC-", i), collapse = "") )
  
  #use abs to compare all the observations
  barplot(abs(analisi.pca$rotation[,i]),
          main = paste(c("Pesi per |", "CP-", i, "|"), collapse = ""), las = 2 )
}  


Y <- analisi.pca$x # new data matrix
head(Y)
pairs.panels(Y) # pc are incorrelated by definition

# How many pc we consider?
screeplot(analisi.pca, type = "lines") #Y1 and Y2 are enough

var.spieg <- analisi.pca$sdev^2 / sum(analisi.pca$sdev^2)

var.spieg.cum <- cumsum(var.spieg)
plot(var.spieg.cum, type ="b", main = "FEV") 
abline(h=0.95, col=2)
# the first 2 PC explain 95% of the total variance
# let us create a df with just (Y1, Y2)
datiPC <- data.frame(Y[,1:2])

pairs.panels(datiPC)

################# Linear Regression
ris.lm1 <- lm(dati.std$n..interventi.polizia.str..In.1.anno ~ ., data = dati.std)

summary(ris.lm1)
# "num.veic. medi" not significative for the model
# too much collinearity with "num. veic. leggeri"
plot(ris.lm1)
influencePlot(ris.lm1)
ncvTest(ris.lm1)
bptest(formula(ris.lm1))
ks.test(scale(residuals(ris.lm1)), y="pnorm" ) #residuals must be Gaussian



####### we build the new df without "num.veic.leggeri": 
nuovi.dati <- cbind(dati.std$incidenti, dati.std$n..interventi.polizia.str..In.1.anno, 
                    dati.std$media.veicoli.giornalieri.pesanti,
                    dati.std$media.veicoli.giornalieri.medi)

nuovi.dati <- data.frame(nuovi.dati)
colnames(nuovi.dati) <- c("num.incidenti","num.interv.poliz.","num.veic.pesan."
                          , "num.veic.medi") 
View(nuovi.dati)

pairs.panels(nuovi.dati, stars= T, ci = F)

##### Linear regression with two predictors
ris.lm2 <- lm(nuovi.dati$num.interv.poliz. ~ nuovi.dati$num.incidenti + 
                nuovi.dati$num.veic.pesan.+ nuovi.dati$num.veic.medi, data= nuovi.dati)
summary(ris.lm2) #"num.veic. medi" not significative

# diagnosis
plot(ris.lm2)
influencePlot(ris.lm2)
ncvTest(ris.lm2)
bptest(formula(ris.lm2))

# test of normality with Kolmogorov-smirnoff (H0 = errors normally distributed):
ks.test(scale(residuals(ris.lm2)), y="pnorm" ) #use scale to set residus between 0 and 1
# residuals are Gaussian


#features selection with VIF
vif(ris.lm2)
#we delete "num.veic.giorn.pesanti" and  "num.veic.giorn.medi"

# log transformation
ris.lm2 <- lm(log(nuovi.dati$num.interv.poliz.) ~ log(nuovi.dati$num.incidenti) )

summary(ris.lm2) 
plot(ris.lm2)
influencePlot(ris.lm2)
# diagnosis
ncvTest(ris.lm2)
bptest(formula(ris.lm2))

ks.test(scale(residuals(ris.lm2)), y= "pnorm")
shapiro.test(residuals(ris.lm2))


# let us try another log transform
ris.lm3 <- lm(log(nuovi.dati$num.interv.poliz.) ~ log(nuovi.dati$num.incidenti)+
                log(nuovi.dati$num.veic.pesan.))
summary(ris.lm3)
plot(ris.lm3)
influencePlot(ris.lm3)  

#Model diagnosis
ncvTest(ris.lm3) #omoschedasticity confirmed
bptest(formula(ris.lm3))

ks.test(scale(residuals(ris.lm3)), y= "pnorm")# Gaussianity of the residuals confirmed
shapiro.test(residuals(ris.lm3))
#outliers
outlierTest(ris.lm3)

#Clustering

Matrice.dist <- dist(nuovi.dati, method = "euclidean")

min(dist(nuovi.dati))

#Hierarchical Clustering:
#hc <- hclust(Matrice.dist, method = "centroid")
#hc <- hclust(Matrice.dist, method = "complete")  
hc <- hclust(Matrice.dist, method = "single")
#hc <- hclust(Matrice.dist, method = "average")
plot(hc)   #dendrogram
rect.hclust(hc, k = 2)  #shows the number of desired groups on the dendrogram 
# wwe associate "1" e "2" to the 30 observations
groups <- cutree( hc, k = 2)
groups

# visualize the clusters with joint densities of the pairs 
pairs(nuovi.dati, col = groups) 
by(nuovi.dati, groups, colMeans)
by(nuovi.dati, groups, var)

####### Clustering with k-means

k <- 2 #number of desired clusters
ris.kmeans <- kmeans(nuovi.dati, k)
pairs(nuovi.dati, col = ris.kmeans$cluster)

# the algorith converges and it is stable
table(ris.kmeans$cluster)


