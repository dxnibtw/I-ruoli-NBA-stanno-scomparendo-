#REPORT STATISTICA COMPUTAZIONALE - NBA PLAYERS STATS - BALL BROTHERS

set.seed(123)
stats<-Dummer_Pozzi_Tresoldi_3_2025


#Escludiamo variabili senza significato statistico
stats<-stats[,-c(1,31,32)]

#Escludiamo i giocatori meno utilizzati e i giocatori con valori mancanti
stats<-subset(stats, G>20)
stats<-subset(stats, MP>=500)
stats<-subset(stats, Team !="2TM")
anyNA(stats)
stats<-na.omit(stats)

#Trasformiamo la variabile Pos in factor e ricodifichiamo i suoi livelli in 3 fattori: 
#guardie (G), ali (F) e centri (C). Facendo così avremo un'interpretazione più chiara e diretta

str(stats)
stats$Pos<-as.factor(stats$Pos)

levels(stats$Pos)
stats$Pos <- dplyr::recode(
  stats$Pos,
  "PG" = "G",
  "C" = "C",
  "SF" = "F",
  "PF" = "F",
  "SG"  = "G"
)

#Creiamo nuove variabili che utilizzeremo nel report

stats$AST_TOV<-stats$AST/stats$TOV
stats$X3PA_FGA<-stats$`3PA`/stats$FGA
stats$AST_TRB<-stats$AST/stats$TRB  

#Selezioniamo le variabili oggetto di studio

var<-c(10,23,26,30,31,32)
names(stats[,var])

#facciamo una piccola analisi esplorativa

summary(stats[,var])
apply(stats[,var],2,var)


#Guardiamo la struttura di correlazione

library(ggcorrplot)

cor<-cor(stats[,var])
ggcorrplot(cor, hc.order = TRUE, lab = T)

#Notiamo una struttura a blocchi: il blocco "interno" e il blocco "perimetrale",
#con correlazioni moderate (<0.8)


#CLUSTERING

library(mclust)

#Utilizziamo la libreria mclust per effetturare un clustering,
#basandoci sulle variabili precedentemente selezionate

clust_prova<-Mclust(stats[,var])     
summary(clust_prova)
plot(clust_prova, what = "BIC")

#Ci propone un modello VVE con 5 cluster, troppi per la nostra analisi. Proviamo un clustering
#basato sull'ICL

clustICL<-mclustICL(stats[,var])
summary(clustICL)
plot(clustICL)

#Secondo il criterio ICL il secondo miglior modello (per differenza minima) è un modello EVE
#con 3 cluster. Proviamo a forzare il numero di cluster

clust<-Mclust(stats[,var], G=3)
summary(clust)

#Otteniamo proprio un modello EVE con 3 cluster. Visualizziamo questo modello

plot(clust, what = "classification")
plot(clust, what = "uncertainty")

clust_density<-densityMclust(stats[,var], model ="EVE", G=3)
plot(clust_density, what = "density", data = stats[,var])

#Inizialmente sembrano esserci dei cluster, ma non troppo distinti

#Aggiungiamo la variabile che identifica il cluster di ogni osservazione

stats$cluster<-as.factor(clust$classification)

#Visulizziamo la distribuzione delle variabili distinte per gruppo

library(ggplot2)
library(dplyr)
library(tidyr)

STATS <- stats %>%
  select(cluster, `FG%`, X3PA_FGA, TRB, AST_TRB, BLK, AST_TOV) %>%
  pivot_longer(-cluster, names_to = "variable", values_to = "value")

ggplot(STATS, aes(cluster, value, fill = cluster))+
  geom_boxplot(outlier.alpha = 0.3)+
  facet_wrap(~ variable, scales = "free")+
  theme_minimal()+
  labs(title = "Distribuzione delle variabili per cluster")

  cluster_profiles <- stats %>%
  group_by(cluster) %>%
  summarise(across(all_of(var), mean, na.rm = TRUE))
cluster_profiles

#I risultati grafici e numerici sembrano essere coerenti con i vari ruoli:
#   -il primo cluster ha AST, AST_TOV e X3PA_FGA alti e le altre variabili basse, tipico delle guardie
#   -il secondo cluster ha statistiche intermedie, più equilibrate, caratteristica delle ali
#   -il terzo gruppo domina su BLK, FG. e TRB, identikit dei centri


#Visualizziamo le distribuzioni e gli scatter plot di ogni coppia di variabile, colorati per ruolo. 
#Questo grafico ci fa vedere come le variabili separano i cluster: notiamo, tendenzialmente, una 
#separazione poco netta, coerente con i ruoli odierni nell'NBA.

library(GGally)
ggpairs(stats[,var])
pairs(stats[,var], col=as.factor(stats$Pos))


#BONTA' DEL CLUSTERING 

#Siccome le etichette sono note e, di conseguenza, conosciamo la vera struttura dei cluster,
#possiamo calcolare vari indicatori di bontà del modello

#CER

CER<-classError(clust$classification, stats$Pos) #0.32, coerente con i grafici e il contesto
CER

#ARI

ARI<-adjustedRandIndex(clust$classification, stats$Pos) # 0.28 non ottimo ma buono, coerente
ARI

#Distanza Kullback-Liebler

mu <- clust$parameters$mean       
Sigma <- clust$parameters$variance$sigma

KL_gaussian <- function(mu1, Sigma1, mu2, Sigma2) {
  p <- length(mu1)
  term1 <- log(det(Sigma2) / det(Sigma1))
  term2 <- sum(diag(solve(Sigma2) %*% Sigma1))
  term3 <- t(mu2 - mu1) %*% solve(Sigma2) %*% (mu2 - mu1)
  0.5 * (term1 - p + term2 + term3)
}

KL_sym <- function(mu1, Sigma1, mu2, Sigma2) {
  KL_gaussian(mu1, Sigma1, mu2, Sigma2) +
    KL_gaussian(mu2, Sigma2, mu1, Sigma1)
}

G <- ncol(mu)
KL_matrix <- matrix(0, G, G)

for(i in 1:G){
  for(j in 1:G){
    KL_matrix[i,j] <- KL_sym(
      mu[,i], Sigma[,,i],
      mu[,j], Sigma[,,j]
    )
  }
}

colnames(KL_matrix) <- rownames(KL_matrix) <- paste0(c("G","F", "C"))
KL_matrix

#La matrice delle distanze di Kullback–Leibler mostra una netta separazione tra i cluster estremi
#con una distanza molto elevata tra Guardie e Centri, a indicare profili chiaramente distinti
#riconducibili ai poli perimetrale e interno. Al contrario, le distanze più contenute che coinvolgono la classe delle ali
#suggeriscono una posizione intermedia, coerente con un ruolo di ponte tra i due estremi


#Confusion matrix
#Innanzitutto dobbiamo ricodificare la variabile cluster in modo che le etichette coincidano

stats$cluster <- dplyr::recode(
  stats$cluster,
  "1"="G",
  "2"="F",
  "3"="C")

#ora possiamo calcolare la matrice di confusione

library(caret)
conf_matrix<-confusionMatrix(stats$cluster, stats$Pos)
conf_matrix 

#Notiamo come la maggior parte degli errori sono tra F e G. Effettivamente nell'NBA odierna questi
#due ruoli sono simili, soprattutto ali piccole e guardie tiratrici. Il modello è prudente
#nell'assegnare un'osservazione al cluster G, lo capiamo dalla bassa sensitivity e dall'alta specificity



#PCA
pca <- princomp(stats[,var], cor=T)
summary(pca)

#Seleziono le prima due componenti principali (varianza spiegata > 60%)

sum((pca$sdev[1:2])^2)/6
pca$loadings[,1:2]

#Creo un dataframe con gli scores sulle prima due componenti principali di ogni giocatore,
#per rappresentare i cluster sulla loro dimensione 

pca_df <- data.frame(
  Player=stats$Player,
  PC1 = pca$scores[,1],
  PC2 = pca$scores[,2],
  cluster = factor(stats$cluster)
)

#Posso scegliere i giocatori, i cui nomi appariranno nel grafico, di cui voglio conoscere il profilo di gioco



players_to_label <- c("Nikola Jokić",
                      "Victor Wembanyama",
                      "LeBron James",
                      "Stephen Curry",
                      "Tyrese Haliburton",
                      "Shai Gilgeous-Alexander")  
label_df <- pca_df %>% filter(Player %in% players_to_label)

ggplot(pca_df, aes(PC1, PC2, color = cluster)) +
  geom_point(alpha = 0.5, size =2) +
  geom_hline(yintercept = 0, linetype = 2,linewidth = 0.35, color = "black") +
  geom_vline(xintercept = 0,linetype = 2 ,linewidth = 0.35, color = "black") +
  geom_text(data= label_df,aes(x = PC1, y = PC2, label = Player), size = 3, vjust = -0.8, color = "black")+
  labs(color = "Ruolo", title = "Proiezione PCA (2025) con model based clustering", x = "PC1",y = "PC2") +
  stat_ellipse(type = "norm", linetype = 2, linewidth = 0.6) +
  annotate("text",x = min(pca_df$PC1),y = min(pca_df$PC2) - 0.6,label = "Tiro perimetrale - Perimetro",hjust = 0,size = 4, fontface = "bold") +
  annotate("text",x = max(pca_df$PC1),y = min(pca_df$PC2) - 0.6,label = "Tiro perimetrale - Interno",hjust = 1,size = 4, fontface = "bold")+
  annotate("text",x = max(pca_df$PC1),y = max(pca_df$PC2),label = "Playmaking - Interno",hjust = 1,size = 4, fontface = "bold")+
  annotate("text",x = min(pca_df$PC1)+3.4,y = max(pca_df$PC2),label = "Playmaking - Perimetro",hjust = 1,size = 4, fontface = "bold")+
  theme_classic()+
  theme(legend.title = element_text(face = "bold"), legend.text = element_text(face = "bold"),axis.title.x = element_text(face = "bold", size = 13),axis.title.y = element_text(face = "bold", size = 13))

#Il grafico mostra abbastanza chiaramente i 3 cluster, con il gruppo delle Ali (F) che fa
#da ponte tra guardie e centri.

#Possiamo rappresentare i profili medi dei cluster tramite un radar plot

X<-scale(stats[,var])
radar_df<-as.data.frame(X)
radar_df$cluster<-factor(stats$cluster)

vars6 <- c("FG%", "TRB", "BLK", "X3PA_FGA", "AST_TOV", "AST_TRB")
centroids <- radar_df %>%
  group_by(cluster) %>%
  summarise(across(all_of(vars6), mean, na.rm = TRUE)) %>%
  ungroup() #creo i miei centroidi, le medie di ogni cluster


radar_df2 <- rbind(
  rep(2, length(vars6)),
  rep(-2, length(vars6)),
  centroids[, vars6]
) #creo delle finte osservazioni che fungono da limite per il radar plot
colnames(radar_df2)<-vars6

fmsb::radarchart(radar_df2,axistype = 0,pcol = c("red", "green", "blue"),pfcol = scales::alpha(c("red", "green", "blue"), 0.2),
  plwd = 2,cglcol = "grey",cglty = 1,axislabcol = "grey30"
)
legend("topright",legend = levels(stats$cluster),col = c("red", "green", "blue"),lwd = 2,bty = "n"
)
title(main = "Profili medi dei cluster 2025 (radar plot)",font.main = 2)

#Il grafico mette bene in mostra i profili dei cluster: 
#i centri sono molto spostati su TRB, BLK e FG., profilo da interno
#le guardie si concentrano su AST_TRB, AST_TOV e X3PA_FGA, a confermare la loro presenza perimetrale
#le ali, come sempre, hanno un profilo ibrido, ponte tra gli altri due cluster



# K-MEANS

set.seed(909085)
X<-scale(stats[, var])
kmeans <- kmeans(X, centers = 3, nstart = 50)

stats$cluster_km <-factor(kmeans$cluster)
table(stats$cluster_km)
table(stats$cluster)
#L'algoritmo ha trovato 3 cluster simili nella numerosità a quelli trovati da Mclust

#CER
classError(kmeans$cluster, stats$Pos)#CER leggermente più alto rispetto al model based clustering (0.32). 

#ARI
adjustedRandIndex(kmeans$cluster, stats$Pos)#L'ARI si abbassa, conferma che k-means sembra essere
#meno preciso

#Silhouette

library(cluster)
library(factoextra)

d<-dist(X)#Calcolo le distanze euclidee sui dati standardizzati, poichè in scale diverse
cl<-as.integer(stats$cluster_km)

#La silhouette ha un valore medio di 0.28, buono ma non eccellente. Ce lo aspettavamo, coerente
#con la natura continua dei cluster

sil<-silhouette(cl, d)
sil
mean(sil[, 3])

#Possiamo visualizzare il silhouette plot: il cluster numero 2 sembra quello più solido,
#il numero 1 il meno solido, ha una caduta a picco e valori negativi.

fviz_silhouette(sil) +
  ggplot2::labs(title = "Silhouette plot – k-means")


#Visualizziamo anche questo clustering, nella dimensione delle prime due componenti principali

pca_km <- princomp(stats[,var], cor = TRUE)
summary(pca_km)
pca_km$loadings

stats$cluster_km <- dplyr::recode(
  stats$cluster_km,
  "1"="F",
  "2"="C",
  "3"="G")

pca_df_km <- data.frame(
  Player=stats$Player,
  PC1_km = pca_km$scores[,1],
  PC2_km = pca_km$scores[,2],
  cluster = stats$cluster_km
)

#Impostiamo i nomi dei giocatori da visualizzare nei cluster

players_to_label_km<- c("Nikola Jokić",
                      "Anthony Davis",
                      "LeBron James",
                      "Darius Garland",
                      "Luke Kornet",
                      "Tyrese Haliburton",
                      "Shai Gilgeous-Alexander")  
label_km_df <- pca_df_km %>% filter(Player %in% players_to_label_km)

ggplot(pca_df_km, aes(PC1_km, PC2_km, color = cluster)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_hline(yintercept = 0, linetype = 2,linewidth = 0.35, color = "black") +
  geom_vline(xintercept = 0,linetype = 2 ,linewidth = 0.35, color = "black") +
  geom_text(data= label_km_df,aes(x = PC1_km, y = PC2_km, label = Player), size = 3, vjust = -0.8, color = "black")+
  labs(color= "Ruolo", title = "Proiezione PCA (2025) con k-means clustering",x = "PC1",y = "PC2") +
  annotate("text",x = min(pca_df_km$PC1_km),y = min(pca_df_km$PC2_km) - 0.4,label = "Tiro perimetrale - Perimetro",hjust = 0,size = 4, fontface = "bold") +
  annotate("text",x = max(pca_df_km$PC1_km),y = min(pca_df_km$PC2_km) - 0.4,label = "Tiro perimetrale - Interno",hjust = 1,size = 4, fontface = "bold")+
  annotate("text",x = max(pca_df_km$PC1_km),y = max(pca_df_km$PC2_km),label = "Playmaking - Interno",hjust = 1,size = 4, fontface = "bold")+
  annotate("text",x = min(pca_df_km$PC1_km)+3.4,y = max(pca_df_km$PC2_km),label = "Playmaking - Perimetro",hjust = 1,size = 4, fontface = "bold")+
  stat_ellipse(type = "norm", linetype = 2, linewidth = 0.6) +
  theme_classic()+
  theme(legend.title = element_text(face = "bold"), legend.text = element_text(face = "bold"),axis.title.x = element_text(face = "bold", size = 13),axis.title.y = element_text(face = "bold", size = 13))+
  coord_cartesian(ylim = c(-3, 4.5))


#Notiamo la classica struttura con le ali a fare da ponte. Il modello sembra andare in difficoltà
#nel distinguere ali e guardie, con quest'ultime molto penalizzate. Ben staccata la classe dei centri

#-------------------------------------------------------------------------


#CLASSIFICAZIONE
set.seed(123)
basket.data <- stats[,var]
basket.class <- stats$Pos

library(Rmixmod)

# Rmixmod: EDDA (classificazione) con tutti i 14 modelli gaussiani

allModels <-  mixmodLearn(basket.data, basket.class,
                          models = mixmodGaussianModel(family='all', equal.proportions=FALSE),
                          criterion = c("CV","BIC"), seed = 123)
allModels@bestResult #modello migliore: Gaussian_pk_L_C con 3 cluster, coerente
#con la struttura dei dati. Il BIC e la CV indicano un buon compromesso tra
#adattamento e complessità del modello

allModels@models@listModels
#estrazione di CV e BIC per tutti i modelli

allModels@results[[1]]@criterionValue [1]    #CV
allModels@results[[1]]@criterionValue [2]    #BIC
# coincidono con il modello migliore



BIC = CV = rep(NA ,length(allModels@models@listModels) )

allModels@models@listModels

for (i in 1: length(allModels@models@listModels)){
  ind = which(allModels@results [[i]] @model == allModels@models@listModels)
  CV[ind] = allModels@results [[i]] @criterionValue [1]
  BIC[ind] = allModels@results [[i]] @criterionValue [2]
}

# Stampo BIC e trovo il migliore secondo min()
round(BIC,1)
min(BIC)
which.min(BIC)



# Stampo CV e trovo il migliore, errore CV più basso è il migliore
round(CV,3)
min(CV) # combacia con il modello risultato migliore
which.min(CV)


#grafici CV e BIC per modello
par(mfrow=c(2,1))
plot(BIC ,type='b',xlab='',xaxt='n',col =2); axis(1,at=1: length(
  allModels@results),labels=substr(allModels@models@listModels ,10 ,30),cex.axis =0.8
  ,las =2)
abline(v=which.min(BIC), col=1, lty =2)
# non combacia con la scelta del modello migliore


plot(CV ,type='b',xlab='',xaxt='n',col =3); axis(1,at=1: length(
  allModels@results),labels=substr(allModels@models@listModels,10 ,30),cex.axis =0.8
  ,las =2)
abline(v=which.min(CV), col=1, lty =2)
#dato l'obbiettivo predittivo il CV risulta il criterio piu appropriato.
#Il modello migliore è pk_L_C


# HOLD-OUT: split 80/20 e valutazione su test (Rmixmod)
# utilizzo un ciclo for per evitare risultati fortuiti dati da un singolo split
(n<-nrow(basket.data))
B <- 100
acc_vec <- numeric(B)
for (b in 1:B) {
  test.set.labels <- sample(1:n, round(0.2*n))
  
  # divido in train e test
  train.data  <- basket.data[-test.set.labels, ]
  test.data   <- basket.data[test.set.labels, ]
  train.class <- basket.class[-test.set.labels]
  test.class  <- basket.class[test.set.labels]
  
  # seleziono il best result utilizzando il training set
  
  CLASSIF <- mixmodLearn(train.data, train.class,
                         models = mixmodGaussianModel(family='all', equal.proportions=FALSE),
                         criterion = c("CV","BIC"))
  CLASSIF@bestResult 
  
  # Predizione sul test usando la regola di classificazione migliore trovata sul training
  
  PRED <- mixmodPredict(data = test.data,
                        classificationRule = CLASSIF["bestResult"])
  acc_vec[b] <- mean(as.integer(test.class) == PRED@partition)
  
  
  
}


#Accuracy sul test 
(mean(acc_vec))
(sd(acc_vec))
#Accuracy media su 100 prove casuali ≈ 76%, discreta ma coerente con tutti gli
#altri indicatori e soprattutto con la natura dei dati


#per produrre una confusion matrix e analisi qualitative fisso uno split specifico  
set.seed(123)
test.set.labels <- sample(1:n, round(0.2*n))

# divido in train e test
train.data  <- basket.data[-test.set.labels, ]
test.data   <- basket.data[test.set.labels, ]
train.class <- basket.class[-test.set.labels]
test.class  <- basket.class[test.set.labels]

# alleno il modello sul training

CLASSIF <- mixmodLearn(train.data, train.class,
                       models = mixmodGaussianModel(family='all', equal.proportions=FALSE),
                       criterion = c("CV","BIC"))
CLASSIF@bestResult 

# Predizione sul test usando la regola di classificazione migliore trovata sul training

PRED <- mixmodPredict(data = test.data,
                      classificationRule = CLASSIF["bestResult"])

str(PRED)

PRED

# Confronto tra classi vere e classi predette
basket.class[test.set.labels]
PRED@partition



# Conversione corretta: trasformo partition in factor con le etichette delle classi
pred_factor <- factor(PRED@partition,
                      levels = 1:length(levels(train.class)),
                      labels = levels(train.class))

true_factor <- factor(test.class, levels = levels(train.class))

#MAtrice di confusione più metriche
table(Predicted = pred_factor, True = true_factor) #confusione soprattutto tra F e G e tra C e F
confusionMatrix(data = pred_factor, reference = true_factor)
#La matrice mostra una buona capacità di classificazione. La classe F
#si conferma la più problematica. Ottimo riconoscimento dei centri


# VISUALIZZAZIONE 2D DELLE REGIONI DI CLASSIFICAZIONE (Rmixmod) SU 2 VARIABILI



# Seleziono due variabili per plot 2D 
par(mfrow=c(1,3))
Z <- basket.data[, c("TRB", "X3PA_FGA")]   
Z <- as.data.frame(Z)

#Gaussian_pk_L_C
pkLC <- mixmodLearn(Z, basket.class,
                    models = mixmodGaussianModel(listModels = "Gaussian_pk_L_C"))

prec <- 150
# Creo una griglia di punti nello spazio delle due variabili



x1 <- seq(min(Z[[1]]), max(Z[[1]]), length.out = prec)
x2 <- seq(min(Z[[2]]), max(Z[[2]]), length.out = prec)

s <- expand.grid(x1, x2)
s <- as.data.frame(s)
colnames(s) <- colnames(Z)

# Calcolo le probabilità di classe sulla griglia

P <- mixmodPredict(s, pkLC@bestResult)@proba

# Disegno lo sfondo colorato in base alla classe più probabile, poi sovrappongo i punti reali

pastel <- .7

plot(Z[[1]], Z[[2]], type = "n",
     xlab = colnames(Z)[1],
     ylab = colnames(Z)[2])

points(s[[1]], s[[2]], type = "p", pch = 16,
       col = c(rgb(1, pastel, pastel),
               rgb(pastel, 1, pastel),
               rgb(pastel, pastel, 1))[max.col(P)])

points(Z[[1]], Z[[2]], col = as.numeric(basket.class) + 1, pch = 19)

title(main = "EDDA with model 'pk_L_C'")
box()



#Gaussian_pk_Lk_C

pkLkC <- mixmodLearn(Z, basket.class, models =
                       mixmodGaussianModel(listModels="Gaussian_pk_Lk_C"))

prec <- 150

x1 <- seq(min(Z[[1]]), max(Z[[1]]), length.out = prec)
x2 <- seq(min(Z[[2]]), max(Z[[2]]), length.out = prec)

s <- expand.grid(x1, x2); s <- as.data.frame(s)
colnames(s) <- colnames(Z)

P <- mixmodPredict(s, pkLkC@bestResult)@proba

plot(Z[[1]], Z[[2]], type='n',
     xlab = colnames(Z)[1], ylab = colnames(Z)[2])

pastel <- .7
points(s[[1]], s[[2]], type='p', pch=16,
       col=c(rgb(1,pastel,pastel),
             rgb(pastel,1,pastel),
             rgb(pastel,pastel,1))[max.col(P)])

points(Z[[1]], Z[[2]], col=as.numeric(basket.class)+1, pch=19)

title(main="EDDA with model 'pk_Lk_C'")
box()


#Gaussian_pk_Lk_Ck (QDA)

pkLkCk <- mixmodLearn(Z, basket.class, models =
                        mixmodGaussianModel(listModels="Gaussian_pk_Lk_Ck"))

prec <- 150

x1 <- seq(min(Z[[1]]), max(Z[[1]]), length.out = prec)
x2 <- seq(min(Z[[2]]), max(Z[[2]]), length.out = prec)

s <- expand.grid(x1, x2); s <- as.data.frame(s)
colnames(s) <- colnames(Z)

P <- mixmodPredict(s, pkLkCk@bestResult)@proba

plot(Z[[1]], Z[[2]], type='n',
     xlab = colnames(Z)[1], ylab = colnames(Z)[2])

pastel <- .7
points(s[[1]], s[[2]], type='p', pch=16,
       col=c(rgb(1,pastel,pastel),
             rgb(pastel,1,pastel),
             rgb(pastel,pastel,1))[max.col(P)])

points(Z[[1]], Z[[2]], col=as.numeric(basket.class)+1, pch=19)

title(main="EDDA with model 'pk_Lk_Ck'")
box()
par(mfrow=c(1,1))
#I modelli mostrati consentono invece un confronto diretto e interpretabile delle regioni di classificazione.
#Abbiamo rappresentato una progressione di 3 modelli: il primo un modello baseline (risulta il migliore secondo CV),
#fino ad arrivare ad un QDA. Vediamo come l'aumento di flessibilità sembra rendere il modello
#più aderente alla realtà. 

# MCLUSTDA: split 80/20 e confronto di modelli
set.seed(123)
basket.class <- factor(basket.class)
n <- nrow(basket.data)


perm <- sample(n)
# Split 80/20 prendendo gli ultimi indici di perm come test (20%)
test.size <- round(0.2 * n)
test.set  <- perm[(n - test.size + 1):n]

test.data    <- basket.data[test.set, ]
test.labels  <- basket.class[test.set]

training.data   <- basket.data[-test.set, ]
training.labels <- basket.class[-test.set]

# Modello fissato: G=3 e covarianza VVV
mod <- MclustDA(training.data, training.labels, G = 3, modelNames = "VVV")
summary(mod)

#Modello senza vincoli su covarianza
mod1 = MclustDA(training.data, training.labels,G=3)
summary(mod1)

#Modello senza vincoli su covarianza e G non fissato 
mod2 = MclustDA(training.data, training.labels)
summary(mod2)


# Predizione: senza dati, predice sul training
predict(mod)                    
# Predizione sul test
predict(mod, test.data)$class   
predict(mod1, test.data)$class 
predict(mod2, test.data)$class 

# Errori sul test
sum(predict(mod, test.data)$class != test.labels)            
mean(predict(mod, test.data)$class != test.labels)           

sum(predict(mod1, test.data)$class != test.labels)            
mean(predict(mod1, test.data)$class != test.labels)  

sum(predict(mod2, test.data)$class != test.labels)            
mean(predict(mod2, test.data)$class != test.labels)  


#Alla luce di questi risultati il modello migliore risulta essere nettamente il modello con G=3 
#e struttura di covarianza VVV: esso ha il BIC e l'errore più basso.
#La presenza di tre componenti per ciascun ruolo evidenzia una marcata eterogeneità interna alle classi,
#coerente con la varietà di profili di gioco osservabili, soprattutto tra guardie e ali.


# CROSS-VALIDATION 20-FOLD PER SCEGLIERE G (MclustDA)

G <- 10
V <- 20
n <- nrow(basket.data)

# assegno a ogni riga un fold 1..20 (bilanciato e random)

fold_id <- sample(rep(1:V, length.out = n))

# err[g,v] = error rate con G=g sul fold v usato come test

err <- matrix(NA_real_, nrow = G, ncol = V)

for (g in 1:G){
  for (v in 1:V){
    
    # Alleno su tutti i dati tranne fold v
    
    mod <- MclustDA(basket.data[fold_id != v, , drop=FALSE],
                    basket.class[fold_id != v],
                    G=g, modelNames="VVV")
    
    # Predico sul fold v
    
    pred <- predict(mod, basket.data[fold_id == v, , drop=FALSE])$class
    
    # Salvo l'errore di classificazione
    
    err[g, v] <- mean(pred != basket.class[fold_id == v])
  }
}
err

# Errore medio sui 20 fold per ogni g (1..10)

round(rowMeans(err, na.rm=TRUE), 4)
(minimo <- which.min(round(rowMeans(err, na.rm=TRUE), 4)))
#L’errore medio minimo si ottiene con G=2 (0.2760)

# Grafico errore medio

par(mfrow=c(1,1))
plot (1:G,rowMeans(err),type='b',ylab='Classification error ',xlab='G',ylim=c(0,0.4))
abline(v=minimo, col=1, lty =2)
#La selezione di G = 2 tramite cross-validation e la scelta di G = 3 nel modello MclustDA non sono in contraddizione.
#La cross-validation privilegia la capacità di generalizzazione, indicando che due sottoprofili per ruolo sono sufficienti ai fini predittivi. 
#Il modello con G = 3, selezionato secondo BIC, evidenzia invece una struttura latente più articolata, utile per una descrizione più fine dell’eterogeneità interna alle classi.




######################################################################################################




#Proviamo ora ad analizzare le stesse variabili, ma catturate 10 anni fa, durante la stagione 2014/15.
#Vogliamo capire se il basket si sta evolvendo in un gioco "positionless"

stats_2015<-Dummer_Pozzi_Tresoldi_3_2015

#Escludiamo le variabili senza significato statistico

stats_2015<-stats_2015[,-c(1,31,32)]


#Escludiamo i giocatori meno utilizzati e le osservazioni con NA

stats_2015<-subset(stats_2015, G>20)
stats_2015<-subset(stats_2015, MP>=500)
stats_2015<-subset(stats_2015, Team !="2TM")
anyNA(stats_2015)
stats_2015<-na.omit(stats_2015)

#Ricodifico la variabili Pos e creo le variabili utili all'analisi

stats_2015$Pos<-as.factor(stats_2015$Pos)
stats_2015$Pos <- dplyr::recode(
  stats_2015$Pos,
  "PG" = "G",
  "C" = "C",
  "SF" = "F",
  "PF" = "F",
  "SG"  = "G"
)

stats_2015$AST_TOV<-stats_2015$AST/stats_2015$TOV
stats_2015$X3PA_FGA<-stats_2015$`3PA`/stats_2015$FGA
stats_2015$AST_TRB<-stats_2015$AST/stats_2015$TRB 

#Facciamo una piccola analisi esplorativa

summary(stats_2015[,var])
apply(stats_2015[,var],2,var)


#Correlazioni

cor_2015<-cor(stats_2015[,var])
ggcorrplot(cor_2015, hc.order = TRUE,lab = T )
#Anche qui emerge la struttra a blocchi

#Iniziamo col model based clustering, forziamo i 3 cluster (5 naturalmente)
library(mclust)
clust_2015<-Mclust(stats_2015[,var], G=3)#EVV 3
summary(clust_2015)

#Secondo il criterio ICL il nostro è il terzo miglior modello

clustICL_2015<-mclustICL(stats_2015[,var])
summary(clustICL_2015)

#Visualizziamo il nostro modello

plot(clust_2015, what = "classification")
plot(clust_2015, what = "uncertainty")

clust_2015_density<-densityMclust(stats_2015[,var], model ="EVV", G=3)
plot(clust_2015_density, what = "density", data = stats_2015[,var])

#Creiamo la variabile che identifica il cluster

stats_2015$cluster<-as.factor(clust_2015$classification)

#Visualizziamo la distribuzione delle variabili divise per cluster

library(dplyr)
library(tidyr)

STATS_2015 <- stats_2015 %>%
  select(cluster, `FG%`, X3PA_FGA, TRB, AST_TRB, BLK, AST_TOV) %>%
  pivot_longer(-cluster, names_to = "variable", values_to = "value")

ggplot(STATS_2015, aes(cluster, value, fill = cluster))+
  geom_boxplot(outlier.alpha = 0.3)+
  facet_wrap(~ variable, scales = "free")+
  theme_minimal()+
  labs(title = "Distribuzione delle variabili per cluster")
library(ggplot2)
cluster_profiles_2015 <- stats_2015 %>%
  group_by(cluster) %>%
  summarise(across(all_of(var), mean, na.rm = TRUE))
cluster_profiles
cluster_profiles_2015

#Dai grafici possiamo già notare una differenza più marcata dei cluster in alcune statistiche.
#I tentativi da 3 punti dei centri calano drasticamente. Effetto "Steph Curry". Questo sostiene la nostra toeria

#Visualizziamo le distribuzioni e gli scatterplot delle variabili a coppie: per alcune notiamo
#una divisione dei cluster più netta rispetto a prima 

library(GGally)
ggpairs(stats_2015[,var])
pairs(stats_2015[,var], col=as.factor(stats_2015$Pos))


#Calcoliamo ora gli indicatori di bontà per avere la conferma della nostra teoria

#CER
CER_2015<-classError(clust_2015$classification, stats_2015$Pos)
CER_2015 #Si abbassa notevolmente, arrivando al 23% circa, bene

#ARI

ARI_2015<-adjustedRandIndex(clust_2015$classification, stats_2015$Pos)
ARI_2015 #Anche l'ARI si alza notevolmente e arriva a 0.44, che è buono

#Distanza KL

mu_2015 <- clust_2015$parameters$mean       
Sigma_2015 <- clust_2015$parameters$variance$sigma

KL_gaussian <- function(mu1, Sigma1, mu2, Sigma2) {
  p <- length(mu1)
  term1 <- log(det(Sigma2) / det(Sigma1))
  term2 <- sum(diag(solve(Sigma2) %*% Sigma1))
  term3 <- t(mu2 - mu1) %*% solve(Sigma2) %*% (mu2 - mu1)
  0.5 * (term1 - p + term2 + term3)
}

KL_sym <- function(mu1, Sigma1, mu2, Sigma2) {
  KL_gaussian(mu1, Sigma1, mu2, Sigma2) +
    KL_gaussian(mu2, Sigma2, mu1, Sigma1)
}

G_2015 <- ncol(mu_2015)
KL_matrix_2015 <- matrix(0, G_2015, G_2015)

for(i in 1:G_2015){
  for(j in 1:G_2015){
    KL_matrix_2015[i,j] <- KL_sym(
      mu_2015[,i], Sigma_2015[,,i],
      mu_2015[,j], Sigma_2015[,,j]
    )
  }
}

colnames(KL_matrix_2015) <- rownames(KL_matrix_2015) <- paste0(c("G","F", "C"))
KL_matrix_2015

#Nel 2015 le distanze di KL risultano complessivamente più elevate e più bilanciate
#rispetto al 2025, soprattutto tra guardie e ali, la cui distanza è ora nettamente maggiore.
#Questo indica una maggiore distinzione strutturale dei ruoli rispetto all’analisi del 2025,
#dove le ali tendevano a collocarsi più chiaramente come ponte tra perimetro e gioco interno.
#Rimane invece molto elevata, in entrambi gli anni, la distanza tra guardie e centri (C), 
#confermando che la contrapposizione tra blocco perimetrale e blocco interno è stabile nel tempo,
#mentre è la zona intermedia a essersi progressivamente “compressa” nel basket più recente.



#Confusion matrix

library(caret)

stats_2015$cluster <- dplyr::recode(
  stats_2015$cluster,
  "1"="G",
  "2"="F",
  "3"="C"
)

conf_matrix_2015<-confusionMatrix(stats_2015$cluster, stats_2015$Pos)
conf_matrix_2015
#Anche in questo caso le ali sono quelle che mettono più in difficoltà il nostro modello:
#spesso vengono confuse con centri e guardie. Quest'ultima è la classe più forte e meglio
#riconosciuta. Meno solidi rispetto a prima i centri


#Visualizziamo anche questo clustering, nella dimensione delle prime due componenti principali
pca_2015<- princomp(stats_2015[,var], cor = TRUE)
summary(pca_2015)
pca_2015$loadings

pca_df_2015 <- data.frame(
  Player=stats_2015$Player,
  PC1_2015 = pca_2015$scores[,1],
  PC2_2015 = pca_2015$scores[,2],
  cluster = stats_2015$cluster
)

#Impostiamo i nomi dei giocatori da visualizzare nei cluster

players_to_label_2015<- c("LeBron James",
                        "Stephen Curry", 
                        "Anthony Davis",
                        "Chris Paul", 
                        "Kevin Durant",
                        "DeAndre Jordan")  
label_2015_df <- pca_df_2015 %>% filter(Player %in% players_to_label_2015)


#La struttura dei cluster rimane la stessa, la classe delle ali funge da ponte tra i centri e le guardie,
#ben divise. La divisione sembra più marcata in generale. LeBron James viene classificato come guardia,
#contrariamente a prima.

ggplot(pca_df_2015, aes(PC1_2015, PC2_2015, color = cluster)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_hline(yintercept = 0, linetype = 2,linewidth = 0.35, color = "black") +
  geom_vline(xintercept = 0,linetype = 2 ,linewidth = 0.35, color = "black") +
  geom_text(data= label_2015_df,aes(x = PC1_2015, y = PC2_2015, label = Player), size = 3, fontface="bold", vjust = -0.8, color= "black")+
  labs(color = "Ruolo", x = "PC1",y = "PC2", title = "Proiezione PCA (2015) con model based clustering") +
  annotate("text",x = min(pca_df_2015$PC1_2015),y = min(pca_df_2015$PC2_2015) - 0.6,label = "Tiro perimetrale - Perimetro",hjust = 0,size = 4, fontface = "bold") +
  annotate("text",x = max(pca_df_2015$PC1_2015),y = min(pca_df_2015$PC2_2015) - 0.6,label = "Tiro perimetrale - Interno",hjust = 1,size = 4, fontface = "bold")+
  annotate("text",x = max(pca_df_2015$PC1_2015),y = max(pca_df_2015$PC2_2015),label = "Playmaking - Interno",vjust = -1,size = 4, fontface = "bold")+
  annotate("text",x = min(pca_df_2015$PC1_2015)+.85,y = max(pca_df_2015$PC2_2015),label = "Playmaking - Perimetro",vjust = -1,size = 4, fontface = "bold")+
  stat_ellipse(type = "norm", linetype = 2, linewidth = 0.6) +
  theme_classic()+
  theme(legend.title = element_text(face = "bold"), legend.text = element_text(face = "bold"),axis.title.x = element_text(face = "bold", size = 13),axis.title.y = element_text(face = "bold", size = 13))+
  coord_cartesian(xlim = c(-4, 7.5))


#Creiamo il radar plot dei profili medi dei cluster

library(fmsb)

X_2015<-scale(stats_2015[,var])
radar_df_2015<-as.data.frame(X_2015)
radar_df_2015$cluster<-factor(stats_2015$cluster)

vars6 <- c("FG%", "TRB", "BLK", "X3PA_FGA", "AST_TOV", "AST_TRB")
centroids <- radar_df_2015 %>%
  group_by(cluster) %>%
  summarise(across(all_of(vars6), mean, na.rm = TRUE)) %>%
  ungroup() #creo i miei centroidi, le medie di ogni cluster

radar_df2_2015 <- rbind(
  rep(2, length(vars6)),
  rep(-2, length(vars6)),
  centroids[, vars6]
) #creo delle finte osservazioni che fungono da limite per il radar plot
colnames(radar_df2_2015)<-vars6

fmsb::radarchart(radar_df2_2015,axistype = 0,pcol = c("red", "green", "blue"),pfcol = scales::alpha(c("red", "green", "blue"), 0.2),
                 plwd = 2,cglcol = "grey",cglty = 1,axislabcol = "grey30"
)
legend("topright",legend = levels(stats_2015$cluster),col = c("red", "green", "blue"),lwd = 2,bty = "n"
)
title(main = "Profili medi dei cluster 2015(radar plot)",font.main = 2)

#Anche qui sono chiare le tendenze di ogni ruolo.


# K-MEANS
#Proviamo a cluserizzare con l'algoritmo delle k-medie

set.seed(123)

kmeans_2015 <- kmeans(X_2015, centers = 3, nstart = 30)
summary(kmeans_2015)
stats_2015$cluster_km <-factor(kmeans_2015$cluster)
table(stats_2015$cluster_km)

#Cluster abbastanza equilibrati nella numerosità

#Vediamo gli indici di bontà e confrontiamoli con quelli ottenuti in precedenza

#CER
classError(kmeans_2015$cluster, stats_2015$Pos)#0.35, abbastanza alto rispetto agli altri risultati ottenuti

#ARI
adjustedRandIndex(kmeans_2015$cluster, stats_2015$Pos)#0.28, si abbassa notevolmente rispetto
#model based clustering. Il k-means sembra funzionare peggio con questi dati

#Silhouette

library(cluster)
library(factoextra)

d_2015<-dist(X_2015)#Calcolo le distanze euclidee sui dati standardizzati, poichè in scale diverse
cl_2015<-as.integer(stats_2015$cluster_km)

#La silhouette ha un valore medio di 0.29, buono, coerente, leggermente superiore ai dati del 2025

sil_2015<-silhouette(cl_2015, d_2015)
sil
mean(sil[, 3])

#Il silhouette plot sembra essere migliore di quello del 2025: i 3 gruppi sembrano essere più solidi
#e bilanciati, con pochissimi valori negativi 

fviz_silhouette(sil) +
  ggplot2::labs(title = "Silhouette plot – k-means")



stats_2015$cluster_km <- dplyr::recode(
  stats_2015$cluster_km,
  "1"="G",
  "2"="F",
  "3"="C"
)

#Visulizziamoli nella dimensione delle prime due componenti principali

pca_km_2015 <- princomp(stats_2015[,var], cor = TRUE)
pca_df_km_2015 <- data.frame(
  Player = stats_2015$Player,
  PC1_km_2015 = pca_km_2015$scores[,1],
  PC2_km_2015 = pca_km_2015$scores[,2],
  cluster_2015 = stats_2015$cluster_km
)

#Scegliamo i giocatori da visualizzare

players_to_label_km_2015<- c("LeBron James",
                             "Stephen Curry", 
                             "Anthony Davis",
                             "Chris Paul", 
                             "Kevin Durant",
                             "DeAndre Jordan")  
label_km_df_2015 <- pca_df_km_2015%>% filter(Player %in% players_to_label_km_2015)

ggplot(pca_df_km_2015, aes(PC1_km_2015, PC2_km_2015, color = cluster_2015)) +
  geom_point(alpha = 0.5,size = 2) +
  geom_hline(yintercept = 0, linetype = 2,linewidth = 0.35, color = "black") +
  geom_vline(xintercept = 0,linetype = 2 ,linewidth = 0.35, color = "black") +
  geom_text(data= label_km_df_2015,aes(x = PC1_km_2015, y = PC2_km_2015, label = Player), size = 3,fontface="bold", vjust = -0.8, color = "black")+
  labs(color = "Ruolo", x = "PC1",y = "PC2", title = "Proiezione PCA (2015) con k-means clustering") +
  annotate("text",x = min(pca_df_km$PC1_km),y = min(pca_df_km$PC2_km) - 0.4,label = "Tiro perimetrale - Perimetro",hjust = 0,size = 4, fontface = "bold") +
  annotate("text",x = max(pca_df_km$PC1_km),y = min(pca_df_km$PC2_km) - 0.4,label = "Tiro perimetrale - Interno",hjust = 1,size = 4, fontface = "bold")+
  annotate("text",x = max(pca_df_km$PC1_km),y = max(pca_df_km$PC2_km),label = "Playmaking - Interno",hjust = 1,size = 4, fontface = "bold")+
  annotate("text",x = min(pca_df_km$PC1_km)+3.4,y = max(pca_df_km$PC2_km),label = "Playmaking - Perimetro",hjust = 1,size = 4, fontface = "bold")+
  stat_ellipse(type = "norm", linetype = 2, linewidth = 0.6) +
  theme_classic()+
  theme(legend.title = element_text(face = "bold"), legend.text = element_text(face = "bold"),axis.title.x = element_text(face = "bold", size = 10),axis.title.y = element_text(face = "bold", size = 10), plot.title = element_text(size = 14, face = "bold"))+
  coord_cartesian(xlim = c(-4, 7.5))

#----------------------------------------------------------

#CLASSIFICAZIONE

library(dplyr)
library(caret)
set.seed(123)

var<-c(10,23,26,30,31,32)
basket.data_2015 <- stats_2015[,var]
basket.class_2015 <- stats_2015$Pos

library(Rmixmod)

# Rmixmod: EDDA (classificazione) con tutti i 14 modelli gaussiani

allModels_2015 <-  mixmodLearn(basket.data_2015, basket.class_2015,
                               models = mixmodGaussianModel(family='all', equal.proportions=FALSE),
                               criterion = c("CV","BIC"), seed=123)
allModels_2015@bestResult #modello migliore: Gaussian_pk_L_C.
#Per il 2015, il modello EDDA selezionato (Gaussian_pk_L_C) risulta più semplice rispetto a quello individuato nel 2025 (Gaussian_pk_Lk_Bk).
#Questo indica che nel 2015 la struttura dei ruoli è descrivibile con minore eterogeneità interna,
#coerentemente con una definizione più rigida delle posizioni tradizionali.

allModels_2015@models@listModels
#estrazione di CV e BIC per tutti i modelli

allModels_2015@results[[1]]@criterionValue [1]    #CV
allModels_2015@results[[1]]@criterionValue [2]    #BIC
# coincidono con il modello migliore



BIC_2015 = CV_2015 = rep(NA ,length(allModels_2015@models@listModels) )

allModels_2015@models@listModels

for (i in 1: length(allModels_2015@models@listModels)){
  ind = which(allModels_2015@results [[i]] @model == allModels_2015@models@listModels)
  CV_2015[ind] = allModels_2015@results [[i]] @criterionValue [1]
  BIC_2015[ind] = allModels_2015@results [[i]] @criterionValue [2]
}

# Stampo BIC e trovo il migliore secondo min()
round(BIC_2015,1)
min(BIC_2015)
which.min(BIC_2015)


# Stampo CV e trovo il migliore, errore CV più basso è il migliore
round(CV_2015,3)
min(CV_2015) # combacia con il modello risultato migliore
which.min(CV_2015)


#grafici CV e BIC per modello

par(mfrow=c(2,1))
plot(BIC_2015 ,type='b',xlab='',xaxt='n',col =2); axis(1,at=1: length(
  allModels_2015@results),labels=substr(allModels_2015@models@listModels ,10 ,30),cex.axis =0.8
  ,las =2)
abline(v=which.min(BIC_2015), col=1, lty =2)
# non combacia con la scelta del modello migliore


plot(CV_2015 ,type='b',xlab='',xaxt='n',col =3); axis(1,at=1: length(
  allModels_2015@results),labels=substr(allModels_2015@models@listModels,10 ,30),cex.axis =0.8
  ,las =2)
abline(v=which.min(CV_2015), col=1, lty =2)
#corrisponde al modello migliore
par(mfrow=c(1,1))

# HOLD-OUT: split 80/20 e valutazione su test (Rmixmod)
# utilizzo un ciclo for per evitare risultati fortuiti dati da un singolo split
(n_2015<-nrow(basket.data_2015))
B <- 100
acc_vec_2015 <- numeric(B)
for (b in 1:B) {
  test.set.labels_2015 <- sample(1:n_2015, round(0.2*n_2015))
  
  # divido in train e test
  train_2015.data  <- basket.data_2015[-test.set.labels_2015, ]
  test_2015.data   <- basket.data_2015[test.set.labels_2015, ]
  train_2015.class <- basket.class_2015[-test.set.labels_2015]
  test_2015.class  <- basket.class_2015[test.set.labels_2015]
  
  # seleziono il best result utilizzando il training set
  
  CLASSIF_2015 <- mixmodLearn(train_2015.data, train_2015.class,
                              models = mixmodGaussianModel(family='all', equal.proportions=FALSE),
                              criterion = c("CV","BIC"))
  CLASSIF_2015@bestResult 
  
  # Predizione sul test usando la regola di classificazione migliore trovata sul training
  
  PRED_2015 <- mixmodPredict(data = test_2015.data,
                             classificationRule = CLASSIF_2015["bestResult"])
  acc_vec_2015[b] <- mean(as.integer(test_2015.class) == PRED_2015@partition)
  
  
  
}


#Accuracy sul test 
(mean(acc_vec_2015))
(sd(acc_vec_2015))
#Accuracy media su 100 prove casuali ≈ 79%. Leggermente superiore al 2025, coerente con la nostra tesi.



#per produrre una confusion matrix e analisi qualitative fisso uno split specifico  
set.seed(123)
test.set.labels_2015 <- sample(1:n_2015, round(0.2*n_2015))

# divido in train e test
train_2015.data  <- basket.data_2015[-test.set.labels_2015, ]
test_2015.data   <- basket.data_2015[test.set.labels_2015, ]
train_2015.class <- basket.class_2015[-test.set.labels_2015]
test_2015.class  <- basket.class_2015[test.set.labels_2015]

# alleno il modello sul training

CLASSIF_2015 <- mixmodLearn(train_2015.data, train_2015.class,
                            models = mixmodGaussianModel(family='all', equal.proportions=FALSE),
                            criterion = c("CV","BIC"))
CLASSIF_2015@bestResult 

# Predizione sul test usando la regola di classificazione migliore trovata sul training

PRED_2015 <- mixmodPredict(data = test_2015.data,
                           classificationRule = CLASSIF_2015["bestResult"])

str(PRED_2015)

PRED_2015

# Confronto tra classi vere e classi predette
basket.class_2015[test.set.labels_2015]
PRED_2015@partition



# Conversione corretta: trasformo partition in factor con le etichette delle classi
pred_factor_2015 <- factor(PRED_2015@partition,
                           levels = 1:length(levels(train_2015.class)),
                           labels = levels(train_2015.class))

true_factor_2015 <- factor(test_2015.class, levels = levels(train_2015.class))

#MAtrice di confusione più metriche
table(Predicted = pred_factor_2015, True = true_factor_2015) #confusione soprattutto tra F e G e tra C e F
confusionMatrix(data = pred_factor_2015, reference = true_factor_2015)

#La matrice di confusione del 2015 mostra una migliore separazione complessiva tra le classi.
#La confusione tra ruoli è piu contenuta rispetto al 2025


# VISUALIZZAZIONE 2D DELLE REGIONI DI CLASSIFICAZIONE (Rmixmod) SU 2 VARIABILI



# Seleziono due variabili per plot 2D 
par(mfrow=c(1,3))
Z_2015 <- basket.data_2015[, c("TRB", "X3PA_FGA")]   
Z_2015 <- as.data.frame(Z_2015)

#Gaussian_pk_L_C
pkLC_2015 <- mixmodLearn(Z_2015, basket.class_2015,
                         models = mixmodGaussianModel(listModels = "Gaussian_pk_L_C"))

prec_2015 <- 150
# Creo una griglia di punti nello spazio delle due variabili



x1_2015 <- seq(min(Z_2015[[1]]), max(Z_2015[[1]]), length.out = prec_2015)
x2_2015 <- seq(min(Z_2015[[2]]), max(Z_2015[[2]]), length.out = prec_2015)

s_2015 <- expand.grid(x1_2015, x2_2015)
s_2015 <- as.data.frame(s_2015)
colnames(s_2015) <- colnames(Z_2015)

# Calcolo le probabilità di classe sulla griglia

P_2015 <- mixmodPredict(s_2015, pkLC_2015@bestResult)@proba

# Disegno lo sfondo colorato in base alla classe più probabile, poi sovrappongo i punti reali

pastel_2015 <- .7

plot(Z_2015[[1]], Z_2015[[2]], type = "n",
     xlab = colnames(Z_2015)[1],
     ylab = colnames(Z_2015)[2])

points(s_2015[[1]], s_2015[[2]], type = "p", pch = 16,
       col = c(rgb(1, pastel_2015, pastel_2015),
               rgb(pastel_2015, 1, pastel_2015),
               rgb(pastel_2015, pastel_2015, 1))[max.col(P_2015)])

points(Z_2015[[1]], Z_2015[[2]], col = as.numeric(basket.class_2015) + 1, pch = 19)

title(main = "EDDA with model 'pk_L_C'")
box()



#Gaussian_pk_Lk_C

pklKC_2015 <- mixmodLearn(Z_2015, basket.class_2015, models =
                            mixmodGaussianModel(listModels="Gaussian_pk_Lk_C"))

prec_2015 <- 150

x1_2015 <- seq(min(Z_2015[[1]]), max(Z_2015[[1]]), length.out = prec_2015)
x2_2015 <- seq(min(Z_2015[[2]]), max(Z_2015[[2]]), length.out = prec_2015)

s_2015 <- expand.grid(x1_2015, x2_2015); s_2015 <- as.data.frame(s_2015)
colnames(s_2015) <- colnames(Z_2015)

P_2015 <- mixmodPredict(s_2015, pklKC_2015@bestResult)@proba

plot(Z_2015[[1]], Z_2015[[2]], type='n',
     xlab = colnames(Z_2015)[1], ylab = colnames(Z_2015)[2])

pastel_2015 <- .7
points(s_2015[[1]], s_2015[[2]], type='p', pch=16,
       col=c(rgb(1,pastel_2015,pastel_2015),
             rgb(pastel_2015,1,pastel_2015),
             rgb(pastel_2015,pastel_2015,1))[max.col(P_2015)])

points(Z_2015[[1]], Z_2015[[2]], col=as.numeric(basket.class_2015)+1, pch=19)

title(main="EDDA with model 'pk_Lk_C'")
box()


#Gaussian_pk_Lk_Ck (QDA)

pkLkCk_2015 <- mixmodLearn(Z_2015, basket.class_2015, models =
                             mixmodGaussianModel(listModels="Gaussian_pk_Lk_Ck"))

prec_2015 <- 150

x1_2015 <- seq(min(Z_2015[[1]]), max(Z_2015[[1]]), length.out = prec_2015)
x2_2015 <- seq(min(Z_2015[[2]]), max(Z_2015[[2]]), length.out = prec_2015)

s_2015 <- expand.grid(x1_2015, x2_2015); s_2015 <- as.data.frame(s_2015)
colnames(s_2015) <- colnames(Z_2015)

P_2015 <- mixmodPredict(s_2015, pkLkCk_2015@bestResult)@proba

plot(Z_2015[[1]], Z_2015[[2]], type='n',
     xlab = colnames(Z_2015)[1], ylab = colnames(Z_2015)[2])

pastel_2015 <- .7
points(s_2015[[1]], s_2015[[2]], type='p', pch=16,
       col=c(rgb(1,pastel_2015,pastel_2015),
             rgb(pastel_2015,1,pastel_2015),
             rgb(pastel_2015,pastel_2015,1))[max.col(P_2015)])

points(Z_2015[[1]], Z_2015[[2]], col=as.numeric(basket.class_2015)+1, pch=19)

title(main="EDDA with model 'pk_Lk_Ck'")
box()
par(mfrow=c(1,1))
#La rappresentazione sembra essere più leggibile rispetto al 2025. Già dal modello
#più semplice emergono confini relativamente netti. L'aumento di complessità produce miglioramenti marginali 
#nella separazioni delle classi.

# MCLUSTDA: split 80/20 e confronto di modelli
set.seed(123)
basket.class_2015 <- factor(basket.class_2015)
n_2015 <- nrow(basket.data_2015)


perm_2015 <- sample(n_2015)
# Split 80/20 prendendo gli ultimi indici di perm come test (20%)
test.size <- round(0.2 * n_2015)
test.set  <- perm_2015[(n_2015 - test.size + 1):n_2015]

test_2015.data    <- basket.data_2015[test.set, ]
test_2015.labels  <- basket.class_2015[test.set]

training_2015.data   <- basket.data_2015[-test.set, ]
trainin_2015.labels <- basket.class_2015[-test.set]

# Modello fissato: G=3 e covarianza VVV
mod_2015 <- MclustDA(training_2015.data, trainin_2015.labels, G = 3, modelNames = "VVV")
summary(mod_2015)

#Modello senza vincoli su covarianza
mod1_2015 = MclustDA(training_2015.data, trainin_2015.labels,G=3)
summary(mod1_2015)

#Modello senza vincoli su covarianza e G non fissato 
mod2_2015 = MclustDA(training_2015.data, trainin_2015.labels)
summary(mod2_2015)

#Il modello migliore è mod_2015 per BIC e errore di classificazione, ha una maggiore capacità
#di cogliere l'eterogeneità delle classi. Inoltre l'errore di classificazione e considerevolmente più basso rispetto al 2025

# Predizione: senza dati, predice sul training
predict(mod_2015)                    
# Predizione sul test
predict(mod_2015, test_2015.data)$class   
predict(mod1_2015, test_2015.data)$class 
predict(mod2_2015, test_2015.data)$class 

# Errori sul test
sum(predict(mod_2015, test_2015.data)$class != test_2015.labels)            
mean(predict(mod_2015, test_2015.data)$class != test_2015.labels)           

sum(predict(mod1_2015, test_2015.data)$class != test_2015.labels)            
mean(predict(mod1_2015, test_2015.data)$class != test_2015.labels)  

sum(predict(mod2_2015, test_2015.data)$class != test_2015.labels)            
mean(predict(mod2_2015, test_2015.data)$class != test_2015.labels)  


#Sul test set, di piccole dimensioni, i 3 modelli hanno prestazioni equivalenti:
#questo ci suggerisce che gli errori sono commessi su osservazioni strutturalmente ambigue


# CROSS-VALIDATION 20-FOLD PER SCEGLIERE G (MclustDA)

G <- 10
V <- 20
n_2015 <- nrow(basket.data_2015)

# assegno a ogni riga un fold 1..20 (bilanciato e random)

fold_id_2015 <- sample(rep(1:V, length.out = n_2015))

# err[g,v] = error rate con G=g sul fold v usato come test

err_2015 <- matrix(NA_real_, nrow = G, ncol = V)

for (g in 1:G){
  for (v in 1:V){
    
    # Alleno su tutti i dati tranne fold v
    
    mod_2015 <- MclustDA(basket.data_2015[fold_id_2015 != v, , drop=FALSE],
                         basket.class_2015[fold_id_2015 != v],
                         G=g, modelNames="VVV")
    
    # Predico sul fold v
    
    pred_2015 <- predict(mod_2015, basket.data_2015[fold_id_2015 == v, , drop=FALSE])$class
    
    # Salvo l'errore di classificazione
    
    err_2015[g, v] <- mean(pred_2015 != basket.class_2015[fold_id_2015 == v])
  }
}
err_2015

# Errore medio sui 20 fold per ogni g (1..10)

round(rowMeans(err_2015, na.rm=TRUE), 4)
(minimo_2015 <- which.min(round(rowMeans(err_2015, na.rm=TRUE), 4)))
#L’errore medio minimo si ottiene con G=2 (0.2031)

# Grafico errore medio e G e linea sul G migliore

par(mfrow=c(1,1))
plot (1:G,rowMeans(err_2015),type='b',ylab='Classification error ',xlab='G',ylim=c(0,0.4))
abline(v=minimo_2015, col=1, lty =2)
#La CV individua anche per il 2015 G=2 come valore ottimale, ma con un errore più basso rispetto 
#al 2025 (0.2760). Questo conferma la nostra tesi, nel 2015 erano presenti dei sottoprofili
#molto più facili da distinguere e interpretare.
