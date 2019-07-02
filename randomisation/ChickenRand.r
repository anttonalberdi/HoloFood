#Chicken randomisation function
ChickenRand <- function(animalvector,animalexcludevector,totalgroupsize=6,sampledgroupsize=2,randomisations=4,batchpos=90,batchext=2,batchlib=2,batchpcr=2,fit=TRUE,seed=9999){

if(!missing(animalexcludevector)){
animals <- setdiff(animalvector,animalexcludevector)
}else{
animals <- animalvector
}

batchsize <- batchpos + batchext + batchlib + batchpcr

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
d.split <- split(d, ceiling(seq_along(d)/totalgroupsize))
if(!missing(seed)){set.seed(seed)}
d.split.subset <- lapply(d.split, function(x) sample(x,sampledgroupsize))
selected <- unlist(d.split.subset, use.names = FALSE)

#Perform the subsetting
split.table.subset <- split.table[selected,]
selected.animals <- paste(split.table.subset$trial, split.table.subset$pen,".",split.table.subset$animal, sep="")

if(fit == TRUE){
batchn = floor(length(selected.animals)/batchpos)
samplen = batchn * batchpos
if(!missing(seed)){set.seed(seed)}
selected.animals <- selected.animals[sample(1:length(selected.animals),samplen,replace = FALSE)]
}

#Iterate according to number of randomisations selected
rand.batches <- list(samples=selected.animals)
for (r in c(2:(randomisations+1))){
#Generate batches
if(!missing(seed)){set.seed(seed)}
batches <- split(sample(selected.animals), ceiling(seq_along(selected.animals)/batchpos))
names(batches) <- paste("Plate",c(1:length(batches)),sep="")

#Add controls and randomise batches
cont.ext <- paste("CE",c(1:batchext),sep="")
cont.lib <- paste("CL",c(1:batchext),sep="")
cont.pcr <- paste("CP",c(1:batchext),sep="")
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
names(rand.batches)[r] <- paste("Rand",(r-1),sep="")
}

#Return
return(rand.batches)
}
