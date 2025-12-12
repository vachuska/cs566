setwd("V:/vachuskaproject/csvision/")

df1 <- read.csv("clip_embeddings_vitb32_val2014.csv")
df2 <- read.csv("clip_embeddings_vitb32_val2014(1).csv")
df3 <- read.csv("clip_embeddings_vitb32_val2014(2).csv")
t1 <- read.csv("clip_text_embeddings_vitb32_val2014.csv")

inst <- read.csv("instances_2014all.csv")

library(jsonlite)
capt <- fromJSON("captions_train2014.json")
capv <- fromJSON("captions_val2014.json")

attr <- read.table("list_attr_celeba.txt", header = TRUE, skip = 1, stringsAsFactors = FALSE)
idz <- read.table("identity_celeba.txt", header = FALSE, stringsAsFactors = FALSE)

trbox <- read.csv("oidv6-train-annotations-bbox.csv")
vbox <- read.csv("validation-annotations-bbox.csv")
tebox <- read.csv("test-annotations-bbox.csv")

classes <- read.csv("oidv7-class-descriptions-boxable.csv")

trbox2 <- read.csv("open_images_extended_miap_boxes_train.csv")
vbox2 <- read.csv("open_images_extended_miap_boxes_val.csv")
tebox2 <- read.csv("open_images_extended_miap_boxes_test.csv")

box2 <- rbind(trbox2, vbox2, tebox2)
length(which(is.na(match(box2$ImageID, c(trbox$ImageID, vbox$ImageID,tebox$ImageID)))))

box3 <- box2[which(is.na(match(box2$ImageID, c(trbox$ImageID, vbox$ImageID,tebox$ImageID)))==FALSE),]

newbox <- data.frame(cbind(unique(box3$ImageID), matrix(0, nrow=length(unique(box3$ImageID)), ncol=7)))
colnames(newbox) <- c("filename", "male", "female", "sum1", "young", "middle", "old", "sum2")
newbox$male[which(is.na(match(newbox$filename,box3$ImageID[which(box3$GenderPresentation=="Predominantly Masculine")]))==FALSE)] <- 1
newbox$female[which(is.na(match(newbox$filename,box3$ImageID[which(box3$GenderPresentation=="Predominantly Feminine")]))==FALSE)] <- 1
newbox$young[which(is.na(match(newbox$filename,box3$ImageID[which(box3$AgePresentation=="Young")]))==FALSE)] <- 1
newbox$middle[which(is.na(match(newbox$filename,box3$ImageID[which(box3$AgePresentation=="Middle")]))==FALSE)] <- 1
newbox$old[which(is.na(match(newbox$filename,box3$ImageID[which(box3$AgePresentation=="Older")]))==FALSE)] <- 1
newbox$sum1 <- as.numeric(newbox$male)+as.numeric(newbox$female)
newbox$sum2 <- as.numeric(newbox$young)+as.numeric(newbox$middle)+as.numeric(newbox$old)

genbox <- newbox[which(newbox$sum1==1), c("filename", "male")]
agebox <- newbox[which(newbox$sum2==1), c("filename", "young", "middle", "old")]

allbox <- rbind(trbox[,c("ImageID", "LabelName")], vbox[,c("ImageID", "LabelName")], tebox[,c("ImageID", "LabelName")])
allbox <- allbox[which(is.na(match(allbox$ImageID, c(genbox$filename, agebox$filename)))==FALSE),]

gw <- read.csv("13428_2018_1099_MOESM2_ESM.csv")

rw <- read.csv("Trait_Ratings_Averages.csv")
sim <- read.csv("clip_neighbors_top10.csv")

cos_sim <- function(x, y) { sum(x * y) / (sqrt(sum(x^2)) * sqrt(sum(y^2))) }

samplesize <- 20
fulldf <- data.frame(matrix(NA, nrow=0, ncol=25))
for(i in c(1:20, 22:39)) {
  vechere <- c(i)
  for(ii in c(21,40)) {
  outcome <- attr[,i]
  outcome[which(outcome==-1)] <- 0
  idzhere <- idz$V2[match(rownames(attr), idz$V1)]
  Xhere <- df2[match(rownames(attr), df2$filename),c(2:ncol(df2))]
  grouphere <- attr[,ii]
  grouphere[which(grouphere==-1)] <- 0
  for(j in 0:1) {
    for(k in 0:1) {
    uniqidzhere <- unique(idzhere[which(outcome==j & grouphere==k)])
    if(length(uniqidzhere)>1) {
      simzhere <- c()
      set.seed(19970514)
      probhere <- 1
      uniqidzhere2 <- sample(uniqidzhere, min(samplesize, length(uniqidzhere)), replace=FALSE)
      for(jk in 1:length(uniqidzhere2)) {
        for(kj in 1:length(uniqidzhere2)) {
          if(jk<kj) {
            simzhere <- c(simzhere, cos_sim(Xhere[sample(which(idzhere==uniqidzhere2[jk]),1),], Xhere[sample(which(idzhere==uniqidzhere2[kj]),1),]))
          }
        }
      }
      vechere <- c(vechere, median(simzhere))
    }
    if(length(uniqidzhere)<2) {
      vechere <- c(vechere, NA)
    }
    vechere <- c(vechere, length(uniqidzhere), length(which(outcome==j & grouphere==k)))
    }
  }
  }
  fulldf <- rbind(fulldf, vechere)
  print(i)
}
colnames(fulldf) <- c("feature", paste0("female_nothave", c("_cossim", "_n", "_N")), paste0("male_nothave", c("_cossim", "_n", "_N")),paste0("female_have", c("_cossim", "_n", "_N")), paste0("male_have", c("_cossim", "_n", "_N")), paste0("old_nothave", c("_cossim", "_n", "_N")), paste0("young_nothave", c("_cossim", "_n", "_N")),paste0("old_have", c("_cossim", "_n", "_N")), paste0("young_have", c("_cossim", "_n", "_N")))
save(fulldf, file="cossim_celeba.RData")

fulldf$cossimdiff <- fulldf$male_have_cossim-fulldf$female_have_cossim
fulldf$genderedness <- ((fulldf$male_have_N)/(fulldf$male_have_N+fulldf$female_have_N))-((fulldf$male_have_N+fulldf$male_nothave_N)/(fulldf$male_have_N+fulldf$female_have_N+fulldf$male_nothave_N+fulldf$female_nothave_N))

summary(lm(cossimdiff~genderedness, data=fulldf))