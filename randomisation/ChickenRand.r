#Chicken randomisation function
#Author: Antton Alberdi
#Date: 2019/07/02
#Version: 1.0

# FUNCTIONALITY
# 1) Randomly select N number of animals from a pool of animals.
# 2) Randomly distribute the selected animals into N number of plates. This step can be done in parallel for multiple batches, e.g. one per sample type.

# ARGUMENTS
# animalvector: vector of animal codes to be included in the randomisation
# animalexcludevector: vector of animal codes to be excluded in the randomisation (e.g. samples randomised in a previous batch)
# blocksize: number of animals in the lowest block size (pen). In the case of chicken: 6
# blocksampled: number of animals in the lowest block size (pen) to be included in the randomisation. e.g. If 2, two chickens out of the six will be randomly selected per pen
# batchnames: vector of batch names. e.g. c("F1","D1","B1") or c("IleumMucosa","CaecumMucosa","CaecumContent"). Samples will be randomly distributed into plates in each batch. 
# positive: number of positive samples per plate. Default 90
# negext: number of extraction controls per plate. Default 2
# neglib: number of library controls per plate. Default 2
# negpcr: number of indexing controls per plate. Default 2
# fit: whether positive samples + controls will be fited to avoid semi-empty plates or not. Note that if TRUE, some of the selected samples will be removed from the plates.


#Example: ChickenRand(animalvector=c("CB02.17","CA23.05","CB15.16","CB09.04","CB05.16","CA13.11"),animalexcludevector=c("CB02.17","CA23.05"),blocksize=6,blocksampled=2,batchnames=c('C1','F1','B1','D2'),positive=90,negext=2,neglib=2,negpcr=2,fit=TRUE,seed=112)



ChickenRand <- function(animalvector,animalexcludevector,blocksize=6,blocksampled=2,batchnames,positive=90,negext=2,neglib=2,negpcr=2,fit=TRUE,seed=9999){

#Exclude samples if argument animalexcludevector provided
if(!missing(animalexcludevector)){
animals <- setdiff(animalvector,animalexcludevector)
}else{
animals <- animalvector
}

batchsize <- positive + negext + neglib + negpcr

library(stringr)
split.table <- as.data.frame(cbind(substring(animals, 1,2),substring(animals, 3,4),substring(animals, 6,7)))
colnames(split.table) <- c("trial","pen","animal")

split.table$time <- str_replace_all(split.table$animal, "^0[1-6]$", "1")
split.table$time <- str_replace_all(split.table$time, "^0[7-9]$", "2")
split.table$time <- str_replace_all(split.table$time, "^1[0-2]$", "2")
split.table$time <- str_replace_all(split.table$time, "^1[3-8]$", "3")

#Sort by trial,pen,time
split.table <- split.table[with(split.table, order(trial, pen, time)),]
rownames(split.table) <- c(1:nrow(split.table))

#Randomly select animales per pen
d <- c(1:nrow(split.table))
d.split <- split(d, ceiling(seq_along(d)/blocksize))
if(!missing(seed)){set.seed(seed)}
d.split.subset <- lapply(d.split, function(x) sample(x,blocksampled))
selected <- unlist(d.split.subset, use.names = FALSE)

#Perform the subsetting
split.table.subset <- split.table[selected,]
selected.animals <- paste(split.table.subset$trial, split.table.subset$pen,".",split.table.subset$animal, sep="")

if(fit == TRUE){
batchn = floor(length(selected.animals)/positive)
samplen = batchn * positive
if(!missing(seed)){set.seed(seed)}
selected.animals <- selected.animals[sample(1:length(selected.animals),samplen,replace = FALSE)]
}

#Iterate according to number of randomisation batches selected
rand.batches <- list(samples=selected.animals)
for (r in c(2:(length(batchnames)+1))){
#Generate batches
if(!missing(seed)){set.seed(seed)}
batches <- split(sample(selected.animals), ceiling(seq_along(selected.animals)/positive))
names(batches) <- paste("Plate",c(1:length(batches)),sep="")

#Add controls and randomise batches
cont.ext <- paste("CE",c(1:negext),sep="")
cont.lib <- paste("CL",c(1:negext),sep="")
cont.pcr <- paste("CP",c(1:negext),sep="")
batches.controls <- lapply(batches, function(x) {x <- c(x,cont.ext,cont.lib,cont.pcr)})
if(!missing(seed)){set.seed(seed)}
batches.controls.shuffled <- lapply(batches.controls, function(x) {x <- sample(x)})
if(fit == FALSE){
filled <- length(batches.controls.shuffled[[length(batches.controls.shuffled)]])
NAs <- rep("NA",batchsize - filled)
batches.controls.shuffled[[length(batches.controls.shuffled)]] <- c(batches.controls.shuffled[[length(batches.controls.shuffled)]],NAs)
}
batches.controls.shuffled.matrix <- lapply(batches.controls.shuffled, function(x) {x <- matrix(x,nrow = 8,ncol = 12, byrow = TRUE, dimnames = list(c("A", "B","C","D","E","F","G","H"),c(1:12)))})
rand.batches[[r]] <- batches.controls.shuffled.matrix
names(rand.batches)[r] <- batchnames[(r-1)]
}

#Return
return(rand.batches)
}
