library(ggplot2)


setwd("/Users/anttonalberdi/github/HoloFood")

chickentable <- read.table("data/holofood_chicken_master2.csv",sep=",",header=TRUE,row.names=1)

#Curate
chickentable[chickentable == "-"] <- NA

######
#
# RAW PAIRWISE CORRELATIONS
#
######

variables <- colnames(chickentable)
variables <- variables[! variables %in% c("Pen","System","Trial","Sampling_time","Sampling_date","TBARS","Salmonella","Campylobacter","Clostridium","Observations","Comments","Treatment","Sex","Replicate","Breed","FPD")]

pairwise <- combn(variables, 2)

cortable <- c()
for (i in c(1:ncol(pairwise))){
pair <- pairwise[,i]
subset <- chickentable[,pair]
subset <- subset[complete.cases(subset),]
correlation <- cor(as.numeric(subset[,1]),as.numeric(subset[,2]))
row <- c(pair,correlation)
cortable <- rbind(cortable,row)
}

cortable <- as.data.frame(cortable)
cortable <- cortable[complete.cases(cortable),]
cortable[,3] <- as.numeric(as.character(cortable[,3]))

write.table(cortable[cortable[,3] > 0.80,],"results/correlations_raw_positive.csv",sep=",",row.names=FALSE,col.names=FALSE)
write.table(cortable[cortable[,3] < -0.80,],"results/correlations_raw_negative.csv",sep=",",row.names=FALSE,col.names=FALSE)

body_weight_cor <- cortable[cortable[,1]== "Chicken_body_weight",]
body_weight_cor <- body_weight_cor[order(body_weight_cor[,3]),]

body_weight_corplot <- ggplot(data=body_weight_cor, aes(x=reorder(V2, V3), y=V3)) +
  geom_bar(stat="identity") +
  ylim(-1, 1) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  ggsave("plots/body_weight_corplot.png",body_weight_corplot,width=10,height=4)


#Test
pair_subset <- chickentable[,c("Chicken_body_weight","MUC2_caecum")]
pair_subset <- pair_subset[complete.cases(pair_subset),]
cor.test(as.numeric(pair_subset[,1]),as.numeric(pair_subset[,2]))





######
#
# TIME-SPECIFIC PAIRWISE CORRELATIONS
#
######

#Day 7
chickentable_sub <- chickentable[chickentable$Sampling_time == "Day 7",]
cortable <- c()
for (i in c(1:ncol(pairwise))){
pair <- pairwise[,i]
subset <- chickentable_sub[,pair]
subset <- subset[complete.cases(subset),]
correlation <- cor(as.numeric(subset[,1]),as.numeric(subset[,2]))
row <- c(pair,correlation)
cortable <- rbind(cortable,row)
}

cortable <- as.data.frame(cortable)
cortable <- cortable[complete.cases(cortable),]
cortable[,3] <- as.numeric(as.character(cortable[,3]))

write.table(cortable[cortable[,3] > 0.80,],"results/correlations_d7_positive.csv",sep=",",row.names=FALSE,col.names=FALSE)
write.table(cortable[cortable[,3] < -0.80,],"results/correlations_d7_negative.csv",sep=",",row.names=FALSE,col.names=FALSE)

body_weight_cor <- cortable[cortable[,1]== "Chicken_body_weight",]
body_weight_cor <- body_weight_cor[order(body_weight_cor[,3]),]


body_weight_corplot <- ggplot(data=body_weight_cor, aes(x=reorder(V2, V3), y=V3)) +
  geom_bar(stat="identity") +
  ylim(-1, 1) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  ggsave("plots/body_weight_corplot_d7.png",body_weight_corplot,width=10,height=4)


#Day 21
chickentable_sub <- chickentable[chickentable$Sampling_time == "Day 21",]
cortable <- c()
for (i in c(1:ncol(pairwise))){
pair <- pairwise[,i]
subset <- chickentable_sub[,pair]
subset <- subset[complete.cases(subset),]
correlation <- cor(as.numeric(subset[,1]),as.numeric(subset[,2]))
row <- c(pair,correlation)
cortable <- rbind(cortable,row)
}

cortable <- as.data.frame(cortable)
cortable <- cortable[complete.cases(cortable),]
cortable[,3] <- as.numeric(as.character(cortable[,3]))

write.table(cortable[cortable[,3] > 0.80,],"results/correlations_d21_positive.csv",sep=",",row.names=FALSE,col.names=FALSE)
write.table(cortable[cortable[,3] < -0.80,],"results/correlations_d21_negative.csv",sep=",",row.names=FALSE,col.names=FALSE)

body_weight_cor <- cortable[cortable[,1]== "Chicken_body_weight",]
body_weight_cor <- body_weight_cor[order(body_weight_cor[,3]),]

body_weight_corplot <- ggplot(data=body_weight_cor, aes(x=reorder(V2, V3), y=V3)) +
  geom_bar(stat="identity") +
  ylim(-1, 1) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  ggsave("plots/body_weight_corplot_d21.png",body_weight_corplot,width=10,height=4)

#Day 35
chickentable_sub <- chickentable[chickentable$Sampling_time == "Day 35",]
cortable <- c()
for (i in c(1:ncol(pairwise))){
pair <- pairwise[,i]
subset <- chickentable_sub[,pair]
subset <- subset[complete.cases(subset),]
correlation <- cor(as.numeric(subset[,1]),as.numeric(subset[,2]))
row <- c(pair,correlation)
cortable <- rbind(cortable,row)
}

cortable <- as.data.frame(cortable)
cortable <- cortable[complete.cases(cortable),]
cortable[,3] <- as.numeric(as.character(cortable[,3]))

write.table(cortable[cortable[,3] > 0.80,],"results/correlations_d35_positive.csv",sep=",",row.names=FALSE,col.names=FALSE)
write.table(cortable[cortable[,3] < -0.80,],"results/correlations_d35_negative.csv",sep=",",row.names=FALSE,col.names=FALSE)

body_weight_cor <- cortable[cortable[,1]== "Chicken_body_weight",]
body_weight_cor <- body_weight_cor[order(body_weight_cor[,3]),]

#Plot
body_weight_corplot <- ggplot(data=body_weight_cor, aes(x=reorder(V2, V3), y=V3)) +
  geom_bar(stat="identity") +
  ylim(-1, 1) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  ggsave("plots/body_weight_corplot_d35.png",body_weight_corplot,width=10,height=4)

#Model
model <- lm(Chicken_body_weight ~ MUC2_ileum + Acidic_goblet_cell_number_ileum + Cortisol + IL6_caecum + Crypt_depth_ileum + IFNg_ileum + Propionic.totalFA_ratio_ileum, data = chickentable_sub)
summary(model)
sigma(model)/mean(chickentable_sub$Chicken_body_weight)

ind="CA05.17"
predicted <- 2691.806 + -104.153*chickentable_sub[ind,"MUC2_ileum"] + 1.056*chickentable_sub[ind,"Acidic_goblet_cell_number_ileum"] + -2.847*chickentable_sub[ind,"Cortisol"] + 159.006*chickentable_sub[ind,"IL6_caecum"] + -2.064*chickentable_sub[ind,"Crypt_depth_ileum"] + -185.763*chickentable_sub[ind,"IFNg_ileum"] + 1851.851*chickentable_sub[ind,"Propionic.totalFA_ratio_ileum"]
observed <- chickentable_sub[ind,"Chicken_body_weight"]
predicted / observed


#i.Valeric_acid_caecum CA01.03 - change to NA

######
#
# VISUALISATIONS
#
######

bw_ibutiricratio <- ggplot(chickentable, aes(x = Chicken_body_weight, y = IL1_ileum, colour = Sampling_time, shape = Breed)) +
  geom_point(size = 2) +
  theme_classic()
ggsave("plots/weight_ibutiricratio.pdf",bw_ibutiricratio,width=8,height=6)





bw_cortisol <- ggplot(chickentable, aes(x = Chicken_body_weight, y = Cortisol, colour = Sampling_time)) +
  geom_point(size = 2) +
  theme_classic()
ggsave("plots/weight_cortisol.pdf",bw_cortisol,width=8,height=6)


bw_Haptoglobin <- ggplot(chickentable, aes(x = Chicken_body_weight, y = Haptoglobin, colour = Sampling_time)) +
  geom_point(size = 1) +
  theme_classic()
ggsave("plots/weight_Haptoglobin.png",bw_Haptoglobin,units="cm",width=20,height=8)

bw_LPS <- ggplot(chickentable, aes(x = Chicken_body_weight, y = LPS, colour = Sampling_time)) +
  geom_point(size = 1) +
  theme_classic()
ggsave("plots/weight_LPS.png",bw_LPS,units="cm",width=20,height=8)


bw_Crypt_depth_caecum <- ggplot(chickentable, aes(x = Chicken_body_weight, y = Crypt_depth_caecum, colour = Sampling_time)) +
  geom_point(size = 1) +
  theme_classic()
ggsave("plots/weight_Crypt_depth_caecum.png",bw_Crypt_depth_caecum,units="cm",width=20,height=8)
