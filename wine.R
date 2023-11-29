require(plotly)
require(tidyverse)
require(dplyr)
require(magrittr)
require(mclust)
require(NbClust)
require(cluster)
require(clustvarsel)
require(vscc)
require(glasso)
require(Rmixmod)
require(flexmix)
require(highcharter)


#write.csv(wine, "C:/Users/Stefano/Desktop/LUMSA/DataMining/ProgettoMistureFinite/wine.csv", row.names = FALSE)

wine <- read_csv("wine.csv")

wine <- na.omit(wine)
summary(wine)

#colnames(wine)[c("Tartaric Acid", "Uronic Acid", "Malic Acid")] <- c("Tartaric", "UronicAcid", "MalicAcid")


cor(x=wine$pH, y=wine$Alcohol) #Non c'è correlazione tra il pH e la quantità di alcol nel vino

cor(wine[, c("Flavanoids","Proanthocyanins","Type")]) #Correlazione tra Flavonoidi, Proantocianidine e Tipo di vino


# GRAFICO ACIDI CATTIVI

BC <- plot_ly(wine, x = ~`Tartaric Acid`, y = ~`Uronic Acids`, z = ~`Malic Acid`, text = ~Type, marker = list(color = ~Type, colorscale = c('#FFE1A1', '#683531'), showscale = TRUE))

BC <- BC %>% add_markers()

BC <- BC %>% layout(scene = list(xaxis = list(title = 'Tartaric'), yaxis = list(title = 'Uronics'), zaxis = list(title = 'Malic')), annotations = list(
  x = 1.02,
  y = 1.0,
  text = 'Type',
  xref = 'paper',
  yref = 'paper',
  font = list(family="Arial", size=20),
  showarrow = FALSE),
  title = list(
    text = "Presenza di Acido Tartatico | Acidi Urici | Acido Malico",
    font = list(family = "Arial", size = 20), y=0.98))

BC


# GRAFICO COMPONENTI SALUTARI (TRANNE ALCOL)

ColorScale <- c("1" = "#e77d57", "2" = "#427D9D", "3" = "#A9AF7E")

CentroidsPoints <- wine %>%
  group_by(Type) %>%
  summarise(FL = mean(Flavanoids), ALC = mean(Alcohol), PR = mean(Proanthocyanins)) %>%
  mutate(Type = factor(Type, levels = c("1", "2", "3")))

CentroidsPoints$ColoreMedie <- c("#cc0000", "#164863", "#557153")

GC <- plot_ly(wine, x = ~Flavanoids, y = ~Alcohol, z = ~Proanthocyanins, color = ~as.factor(Type), colors = ColorScale) %>% add_markers(marker = list(size = 5))

GC <- GC %>% add_trace(
  data = CentroidsPoints, x = ~FL, y = ~ALC, z = ~PR,
  type = "scatter3d", mode = "markers",
  marker = list(size = 10, color = ~ColoreMedie, symbol = 4))


GC <- GC %>%
  layout(scene = list(
    xaxis = list(title = 'Flavanoids'),
    yaxis = list(title = 'Alcohol'),
    zaxis = list(title = 'Proanthocyanins')),
    
    legend = list(title = ''),
    title = list(
      text = "Salute e Vino",
      font = list(family = "Arial", size = 20), y=0.98),
    annotations = list(
      list(y = 0.99,
           text = "Presenza di Flavonoidi | Alcol | Proantocianidine",
           showarrow = FALSE)))
GC



wine$GWC <- wine$Flavanoids + wine$Proanthocyanins + wine$Potassium + wine$Magnesium #Good Wine Components (Potassium is good too :))
wine$BWC <- wine$Calcium + wine$Methanol + wine$Chloride + wine$`Total Nitrogen` #Bad Wine Components
wine$CS <- wine$BWC/wine$GWC #Components Score

summary(wine$CS)



# COMPONENTS SCORES BOXPLOT

GCS <- plot_ly(wine, x = ~Type, y = ~CS, text = ~Type, type = "box", color = ~as.factor(Type), colors = c("#6B240C", "#994D1C", "#E48F45"))


GCS <- GCS %>% layout(title = list(
    text = "Wine Components Scores Boxplot",
    font = list(family = "Arial", size = 20), y=0.98),
    xaxis = list(
      tickvals = c(1, 2, 3),
      ticktext = c("Barolo", "Grignolino", "Barbera")))

GCS


# TASTE SCORE GRAPH

wine$TasteScore <- wine$Alcohol/wine$Glycerol #Presenza d'alcol rapportata alla morbidezza e dolcezza data dal glicerolo

TSP <- plot_ly(wine, x = ~Type, y = ~TasteScore, text = ~Type, type = "box", color = ~as.factor(Type), colors = c("#2a4d69", "#4b86b4", "#adcbe3"))

TSP <- TSP %>% layout(title = list(
  text = "Taste Score Boxplot",
  font = list(family = "Arial", size = 20), y=0.98),
  xaxis = list(
    tickvals = c(1, 2, 3),
    ticktext = c("Barolo", "Grignolino", "Barbera")))

TSP


#Ash and Acalinity of Ash Plot

corrispondenza <- c("Barolo", "Grignolino", "Barbera")
wine$Type <- corrispondenza[wine$Type]

AAPlot <- wine %>% 
  hchart('scatter', hcaes(x = Ash, y = `Alcalinity of Ash`, group = Type)) %>%
  hc_colors(c("#00bfff", "#36D636", "#d70a53",  "#588c7e", "#D526BB", "#34AACB")) %>%
  hc_xAxis(title = list(text="Ash")) %>%
  hc_yAxis(title = list(text="Alcalinity of Ash"))%>%
  hc_title(text = "Ash and Alcalinity of Ash Plot") %>%
  hc_add_theme(hc_theme_smpl())

AAPlot



remove(wine)
wine <- read_csv("wine.csv")



# FINE ANALISI DESCRITTIVA -----------------------------------------------------------



#GRAFICO SILHOUETTE

{silhouette_scores <- sapply(2:10, function(k) {
  clusters <- kmeans(wine, centers = k)$cluster
  silhouette_score <- silhouette(clusters, dist(wine))
  mean(silhouette_score[, "sil_width"])
})
  
  plot(2:10, silhouette_scores, type = "b", pch = 19, col = "blue", frame = FALSE, 
       xlab = "Number of Clusters", ylab = "Silhouette Score",
       main = "Silhouette Analysis for Clustering",
       sub = "Choose the number of clusters that maximizes the silhouette score")
  
  max_score <- max(silhouette_scores)
  abline(h = max_score, col = "red", lty = 2) # Linea di marcatura per il picco
  
  max_index <- which.max(silhouette_scores)
  
  text(max_index + 1, max_score, 
       labels = paste("K =", max_index + 1), pos = 3, col = "red")}



# -------------- VARIABLE SELECTION and MIXTURE MODELS --------------

MMD <- wine[,-1]

set.seed(1111)
VSDataCVS <- clustvarsel(MMD, G=2, direction="forward") #Forward è impostato di default (si parte da un modello vuoto e si esamina una variabile alla volta)


# VSCC

VSDataVSCC <- vscc(MMD, G=2, automate = "mclust", initial = NULL, train = NULL, forcereduction = FALSE)
summary(VSDataVSCC)


# Clustvarsel

#Controllo del numero di cluster
set.seed(1111)
NbClust(MMD, distance="euclidean", method="kmeans") 
NbClust(MMD, distance="manhattan", method="kmeans")
NbClust(MMD, distance="minkowski", method="kmeans")


SelectedVariables <- colnames(MMD)[VSDataCVS$subset]
print(SelectedVariables)

MixtureModel1 <- Mclust(MMD[, VSDataCVS$subset], G=2)

summary(MixtureModel1)
MixtureModel1$parameters$mean
MixtureModel1$parameters$pro


plot(VSDataCVS$model)



# FLEXMIX - Misture di Regressione

FlexMixModel <- FLXMRglm(family = "gaussian")
fittedModel <- stepFlexmix(pH ~ Alcohol, model = FlexMixModel, nrep=5, k = 2, data = MMD) #Calcolo della regressione per ogni cluster e delle probabilità a posteriori di appartenenza ad un cluster
summary(fittedModel)
summary(refit(fittedModel))




















