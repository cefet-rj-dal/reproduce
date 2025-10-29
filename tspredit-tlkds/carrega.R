options(scipen=999)

library(dplyr)
library(stringr)

carrega <- function() {
  landUse <- read.csv("input/Inputs_LandUse_E_All_Data_NOFLAG.csv", header=TRUE, sep=',')
  landUse <- landUse |> filter(Area.Code < 5000) |>  filter(Item.Code == 6620)
  landUse$Area.Code..M49. <- NULL
  landUse$Item.Code <- NULL
  landUse$Element.Code <- NULL
  
  fertilizers <- read.csv("input/Environment_Fertilizers_E_All_Data_NOFLAG.csv", header=TRUE, sep=',')
  fertilizers <- fertilizers |> filter(Element.Code == 5159) |> filter(Area.Code != 41) |> filter(Area.Code != 185)
  fertilizers$Area.Code..M49. <- NULL
  fertilizers$Item.Code <- NULL
  fertilizers$Element.Code <- NULL
  
  data <- merge(fertilizers, landUse, by.x = "Area.Code", by.y = "Area.Code")
  
  sF <- which(colnames(data) == "Y1961.x")
  eF <- which(colnames(data) == "Y2020.x")
  
  sL <- which(colnames(data) == "Y1961.y")
  
  for (i in sF:eF) {
    data[,i] <- data[,i] * data[, sL + i - sF] / 1000
  }
  
  data <- data[,-((ncol(fertilizers)+1):ncol(data))]
  colnames(data) <- colnames(fertilizers)
  
  consumption <- data |> group_by(Area.Code, Area) |> summarise(Y2020 = sum(Y2020))
  i <- order(consumption$Y2020, decreasing = TRUE)
  consumption <- consumption[i, ] 
  consumption <- consumption[consumption$Area == "Brazil", ]
  consumption <- head(consumption, 10)
  consumption$rank <- 1:nrow(consumption)
  
  rank <- consumption |> select(Area.Code, rank)
  
  fertilizers <- merge(fertilizers, rank, by="Area.Code")
  fertilizers <- fertilizers[order(fertilizers$rank, fertilizers$Item), ]
  fertilizers$Item[fertilizers$Item == "Nutrient nitrogen N (total)"] <- "N"
  fertilizers$Item[fertilizers$Item == "Nutrient phosphate P2O5 (total)"] <- "P2O5"
  fertilizers$Item[fertilizers$Item == "Nutrient potash K2O (total)"] <- "K2O"
  fertilizers$rank <- NULL
  
  dataset <- list()
  for (i in 1:nrow(fertilizers)) {
    x <- as.vector(t(fertilizers[i, 6:ncol(fertilizers)]))
    names(x) <- str_replace(colnames(fertilizers)[6:ncol(fertilizers)], "Y", "")
    dataset[[i]] <- x
  }
  names(dataset) <- tolower(sprintf("%s_%s", str_replace_all(fertilizers$Area, " ", "_"), fertilizers$Item)) 
  return(dataset)
}

fertilizers <- carrega()
rank <- data.frame(name = names(fertilizers), pos = 1:length(fertilizers))

save(fertilizers, file="data/fertilizers.RData")
