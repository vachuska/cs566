load("text_word_sims_df1.RData")

text_word_sims$Typicality <- text_word_sims$malescore


m1 <- summary(lm(cosdiff~Typicality, data=text_word_sims))$coefficients
write.csv(m1, file="text_word_sims_reg1.csv")
m1 <- summary(lm(cosdiff~Typicality, data=text_word_sims[which(text_word_sims$malescore>5 | text_word_sims$malescore<3),]))$coefficients
write.csv(m1, file="text_word_sims_reg2.csv")

library(vtable)
st <- sumtable(text_word_sims[,c("male_n_images", "female_n_images", "male_median_cosine_txt", "female_median_cosine_txt", "cosdiff", "Typicality")], out = "return")
st$Variable[1:5] <- c("N Male", "N Female", "Male Med. Cos. Sim.", "Female Med. Cos. Sim.", "Med. Cos. Sim. Diff.")
write.csv(st, "summary_stats_textwordsims.csv", row.names = FALSE)


load("sharp_gender_wide_df2.RData")

library(ggplot2)
ggplot(sharp_gender_wide, aes(x = genderedness, y = cosdiff)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    x = "Male ??? female attribute association",
    y = "Male ??? female cosine similarity",
    title = NULL
  ) +
  theme_minimal()

load("sharp_age_wide_df2.RData")

ggplot(sharp_age_wide, aes(x = agedness, y = cosdiff)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    x = "Young - old attribute association",
    y = "Young ??? old cosine similarity",
    title = NULL
  ) +
  theme_minimal()

sharp_gender_wide$Typicality <- sharp_gender_wide$genderedness
sharp_age_wide$Typicality <- sharp_age_wide$agedness
m1 <- summary(lm(cosdiff ~ genderedness, data = sharp_gender_wide))$coefficients
write.csv(m1, file="df2_gender_reg1.csv")
m1 <- summary(lm(cosdiff ~ agedness,    data = sharp_age_wide))$coefficients
write.csv(m1, file="df2_age_reg1.csv")

library(vtable)
st <- sumtable(sharp_gender_wide[,c("male_n_img", "female_n_img", "cosdiff", "Typicality")], out = "return")
st$Variable[1:3] <- c("N Male", "N Female", "Med. Cos. Sim. Diff.")
write.csv(st, "summary_stats_df2_gender.csv", row.names = FALSE)
st <- sumtable(sharp_age_wide[,c("young_n_img", "old_n_img", "cosdiff", "Typicality")], out = "return")
st$Variable[1:3] <- c("N Young", "N Old", "Med. Cos. Sim. Diff.")
write.csv(st, "summary_stats_df2_age.csv", row.names = FALSE)


load("sharp_gender_one_row_df3.RData")
sharp_gender_one_row$Typicality <- sharp_gender_one_row$genderedness 
m1 <- summary(lm(cosdiff~Typicality, data=sharp_gender_one_row))$coefficients
write.csv(m1, file="df3_gender_reg1.csv")

library(vtable)
st <- sumtable(sharp_gender_one_row[,c("n_img_male", "n_img_female", "mean_cos_male", "mean_cos_female", "cosdiff", "Typicality")], out = "return")
st$Variable[1:5] <- c("N Male", "N Female", "Male Med. Cos. Sim.", "Female Med. Cos. Sim.", "Med. Cos. Sim. Diff.")
write.csv(st, "summary_stats_df3_gender.csv", row.names = FALSE)

load("sharp_age_one_row_3cat_df3.RData")

sharp_age_one_row_3cat$Typicality <- sharp_age_one_row_3cat$agedness
m1 <- summary(lm(cosdiff~Typicality, data=sharp_age_one_row_3cat))$coefficients
write.csv(m1, file="df3_age_reg1.csv")

library(vtable)
st <- sumtable(sharp_age_one_row_3cat[,c("n_img_young", "n_img_middle", "n_img_old", "mean_cos_young", "mean_cos_middle", "mean_cos_old", "cosdiff", "Typicality")], out = "return")
st$Variable[1:7] <- c("N Young", "N Middle", "N Old", "Young Med. Cos. Sim.", "Middle Med. Cos. Sim.", "Old Med. Cos. Sim.", "Med. Cos. Sim. Diff.")
write.csv(st, "summary_stats_df3_age.csv", row.names = FALSE)


load("int_resultdf_gender_df2.RData")

resultdf$cossim01 <- as.numeric(resultdf$cossim01)
resultdf$cossim11 <- as.numeric(resultdf$cossim11)
resultdf$cossim02 <- as.numeric(resultdf$cossim02)
resultdf$cossim12 <- as.numeric(resultdf$cossim12)
resultdf$inequality1 <- as.numeric(resultdf$cossim11)-as.numeric(resultdf$cossim01)
resultdf$inequality2 <- as.numeric(resultdf$cossim12)-as.numeric(resultdf$cossim02)
resultdf$Typicality <- resultdf$genderedness

plot(resultdf$Typicality, resultdf$cossim12-resultdf$cossim11, xlab = "Typicality", ylab = "Change After Intervention")

m1<- summary(lm(cosdiff~Typicality, data=resultdf))$coefficients
rownames(m1)[2] <- "Cos. Sim. Diff."
write.csv(m1, file="df2_gender_bk_reg1.csv")
m2 <- summary(lm(inequality1~Typicality+cosdiff, data=resultdf))$coefficients
rownames(m2)[3] <- "Cos. Sim. Diff."
write.csv(m2, file="df2_gender_bk_reg2.csv")
m3 <- summary(lm(inequality2~Typicality+cosdiff, data=resultdf))$coefficients
rownames(m3)[3] <- "Cos. Sim. Diff."
write.csv(m3, file="df2_gender_bk_reg3.csv")
m4 <- summary(lm(c(resultdf$inequality1, resultdf$inequality2) ~ c(resultdf$genderedness,resultdf$genderedness)*c(rep(0, nrow(resultdf)), rep(1, nrow(resultdf)))+c(resultdf$cosdiff, resultdf$cosdiff)*c(rep(0, nrow(resultdf)), rep(1, nrow(resultdf)))))$coefficients
rownames(m4)[2:6] <- c("Typicality", "Equitable Training", "Cos. Sim. Diff.", "Typicality X Equitable Training", "Cos. Sim. Diff. X Equitable Training")
write.csv(m4, file="df2_gender_bk_reg4.csv")

library(vtable)
st <- sumtable(resultdf[,c("cossim01", "cossim11", "cossim02", "cossim12", "cosdiff", "inequality1", "inequality2", "Typicality")], out = "return")
st$Variable[1:7] <- c("Cos. Sim. Vec. #1 - Females", "Cos. Sim. Vec. #1 - Males", "Cos. Sim. Vec. #2 - Females", "Cos. Sim. Vec. #2 - Males", "Cos. Sim. Diff.", "Cos. Sim. Vec. #1 Diff.", "Cos. Sim. Vec. #2 Diff.")
write.csv(st, "summary_stats_df2_bk_gender.csv", row.names = FALSE)


c1 <- c()
c2 <- c()
c3 <- c()
c4 <- c()
c5 <- c()
c6 <- c()
c7 <- c()
set.seed(19970514)
for(i in 1:1000) {
  samphere <- sample(c(1:nrow(resultdf)), nrow(resultdf), replace=TRUE)
  m1<- summary(lm(cosdiff~Typicality, data=resultdf[samphere,]))$coefficients
  c1 <- c(c1, m1[2,1])
  m2 <- summary(lm(inequality1~Typicality+cosdiff, data=resultdf[samphere,]))$coefficients
  c2 <- c(c2, m2[2,1])
  c3 <- c(c3, m2[3,1])
  m3 <- summary(lm(inequality2~Typicality+cosdiff, data=resultdf[samphere,]))$coefficients
  c4 <- c(c4, m3[2,1])
  c5 <- c(c5, m3[3,1])
  m4 <- summary(lm(c(resultdf$inequality1[samphere], resultdf$inequality2[samphere]) ~ c(resultdf$genderedness[samphere],resultdf$genderedness[samphere])*c(rep(0, nrow(resultdf)), rep(1, nrow(resultdf)))+c(resultdf$cosdiff[samphere], resultdf$cosdiff[samphere])*c(rep(0, nrow(resultdf)), rep(1, nrow(resultdf)))))$coefficients
  c6 <- c(c6, m4[4,1])
  c7 <- c(c7, m4[6,1])
  print(i)
}
dir1 <- c(median(c2))
indir1 <- c(median(c1*c3))
dir2 <- c(median(c4))
indir2 <- c(median(c1*c5))
tot <- c(median(c6))
prop <- c(median(c7))

dir1l <- c(quantile(c2,.025))
indir1l <- c(quantile(c1*c3,.025))
dir2l <- c(quantile(c4,.025))
indir2l <- c(quantile(c1*c5,.025))
totl <- c(quantile(c6,.025))
propl <- c(quantile(c7,.025))

dir1u <- c(quantile(c2,.975))
indir1u <- c(quantile(c1*c3,.975))
dir2u <- c(quantile(c4,.975))
indir2u <- c(quantile(c1*c5,.975))
totu <- c(quantile(c6,.975))
propu <- c(quantile(c7,.975))

prop1 <- c(median((c1*c3)/(c2+(c1*c3))))
prop2 <- c(median((c1*c5)/(c4+(c1*c5))))
prop1l <- c(quantile((c1*c3)/(c2+(c1*c3)),.025))
prop2l <- c(quantile((c1*c5)/(c4+(c1*c5)),.025))
prop1u <- c(quantile((c1*c3)/(c2+(c1*c3)),.975))
prop2u <- c(quantile((c1*c5)/(c4+(c1*c5)), .975))

prop3 <- c(median(c4/c2))
prop4 <- c(median((c1*c5)/(c1*c3)))
prop3l <- c(quantile(c4/c2, .025))
prop4l <- c(quantile((c1*c5)/(c1*c3), .025))
prop3u <- c(quantile(c4/c2, .975))
prop4u <- c(quantile((c1*c5)/(c1*c3), .975))

load("int_resultdf_age_df2.RData")


resultdf$cossim01 <- as.numeric(resultdf$cossim01)
resultdf$cossim11 <- as.numeric(resultdf$cossim11)
resultdf$cossim02 <- as.numeric(resultdf$cossim02)
resultdf$cossim12 <- as.numeric(resultdf$cossim12)
resultdf$inequality1 <- as.numeric(resultdf$cossim11)-as.numeric(resultdf$cossim01)
resultdf$inequality2 <- as.numeric(resultdf$cossim12)-as.numeric(resultdf$cossim02)
resultdf$Typicality <- resultdf$agedness

plot(resultdf$Typicality, resultdf$cossim12-resultdf$cossim11, xlab = "Typicality", ylab = "Change After Intervention")

m1<- summary(lm(cosdiff~Typicality, data=resultdf))$coefficients
rownames(m1)[2] <- "Cos. Sim. Diff."
write.csv(m1, file="df2_age_bk_reg1.csv")
m2 <- summary(lm(inequality1~Typicality+cosdiff, data=resultdf))$coefficients
rownames(m2)[3] <- "Cos. Sim. Diff."
write.csv(m2, file="df2_age_bk_reg2.csv")
m3 <- summary(lm(inequality2~Typicality+cosdiff, data=resultdf))$coefficients
rownames(m3)[3] <- "Cos. Sim. Diff."
write.csv(m3, file="df2_age_bk_reg3.csv")
m4 <- summary(lm(c(resultdf$inequality1, resultdf$inequality2) ~ c(resultdf$Typicality,resultdf$Typicality)*c(rep(0, nrow(resultdf)), rep(1, nrow(resultdf)))+c(resultdf$cosdiff, resultdf$cosdiff)*c(rep(0, nrow(resultdf)), rep(1, nrow(resultdf)))))$coefficients
rownames(m4)[2:6] <- c("Typicality", "Equitable Training", "Cos. Sim. Diff.", "Typicality X Equitable Training", "Cos. Sim. Diff. X Equitable Training")
write.csv(m4, file="df2_age_bk_reg4.csv")

library(vtable)
st <- sumtable(resultdf[,c("cossim01", "cossim11", "cossim02", "cossim12", "cosdiff", "inequality1", "inequality2", "Typicality")], out = "return")
st$Variable[1:7] <- c("Cos. Sim. Vec. #1 - Old", "Cos. Sim. Vec. #1 - Young", "Cos. Sim. Vec. #2 - Old", "Cos. Sim. Vec. #2 - Young", "Cos. Sim. Diff.", "Cos. Sim. Vec. #1 Diff.", "Cos. Sim. Vec. #2 Diff.")
write.csv(st, "summary_stats_df2_bk_age.csv", row.names = FALSE)


c1 <- c()
c2 <- c()
c3 <- c()
c4 <- c()
c5 <- c()
c6 <- c()
c7 <- c()
set.seed(19970514)
for(i in 1:1000) {
  samphere <- sample(c(1:nrow(resultdf)), nrow(resultdf), replace=TRUE)
  m1<- summary(lm(cosdiff~Typicality, data=resultdf[samphere,]))$coefficients
  c1 <- c(c1, m1[2,1])
  m2 <- summary(lm(inequality1~Typicality+cosdiff, data=resultdf[samphere,]))$coefficients
  c2 <- c(c2, m2[2,1])
  c3 <- c(c3, m2[3,1])
  m3 <- summary(lm(inequality2~Typicality+cosdiff, data=resultdf[samphere,]))$coefficients
  c4 <- c(c4, m3[2,1])
  c5 <- c(c5, m3[3,1])
  m4 <- summary(lm(c(resultdf$inequality1[samphere], resultdf$inequality2[samphere]) ~ c(resultdf$Typicality[samphere],resultdf$Typicality[samphere])*c(rep(0, nrow(resultdf)), rep(1, nrow(resultdf)))+c(resultdf$cosdiff[samphere], resultdf$cosdiff[samphere])*c(rep(0, nrow(resultdf)), rep(1, nrow(resultdf)))))$coefficients
  c6 <- c(c6, m4[4,1])
  c7 <- c(c7, m4[6,1])
  print(i)
}
dir1 <- c(dir1, median(c2))
indir1 <- c(indir1, median(c1*c3))
dir2 <- c(dir2, median(c4))
indir2 <- c(indir2, median(c1*c5))
tot <- c(tot,median(c6))
prop <- c(prop,median(c7))

dir1l <- c(dir1l,quantile(c2,.025))
indir1l <- c(indir1l,quantile(c1*c3,.025))
dir2l <- c(dir2l,quantile(c4,.025))
indir2l <- c(indir2l,quantile(c1*c5,.025))
totl <- c(totl,quantile(c6,.025))
propl <- c(propl,quantile(c7,.025))

dir1u <- c(dir1u,quantile(c2,.975))
indir1u <- c(indir1u,quantile(c1*c3,.975))
dir2u <- c(dir2u,quantile(c4,.975))
indir2u <- c(indir2u,quantile(c1*c5,.975))
totu <- c(totu,quantile(c6,.975))
propu <- c(propu,quantile(c7,.975))


prop1 <- c(prop1, median((c1*c3)/(c2+(c1*c3))))
prop2 <- c(prop2, median((c1*c5)/(c4+(c1*c5))))
prop1l <- c(prop1l, quantile((c1*c3)/(c2+(c1*c3)),.025))
prop2l <- c(prop2l, quantile((c1*c5)/(c4+(c1*c5)),.025))
prop1u <- c(prop1u, quantile((c1*c3)/(c2+(c1*c3)),.975))
prop2u <- c(prop2u, quantile((c1*c5)/(c4+(c1*c5)), .975))

prop3 <- c(prop3, median(c4/c2))
prop4 <- c(prop4, median((c1*c5)/(c1*c3)))
prop3l <- c(prop3l, quantile(c4/c2, .025))
prop4l <- c(prop4l, quantile((c1*c5)/(c1*c3), .025))
prop3u <- c(prop3u, quantile(c4/c2, .975))
prop4u <- c(prop4u, quantile((c1*c5)/(c1*c3), .975))

load("int_resultdf_gender_df3.RData")



resultdf$cossim01 <- as.numeric(resultdf$cossim01)
resultdf$cossim11 <- as.numeric(resultdf$cossim11)
resultdf$cossim02 <- as.numeric(resultdf$cossim02)
resultdf$cossim12 <- as.numeric(resultdf$cossim12)
resultdf$inequality1 <- as.numeric(resultdf$cossim11)-as.numeric(resultdf$cossim01)
resultdf$inequality2 <- as.numeric(resultdf$cossim12)-as.numeric(resultdf$cossim02)
resultdf$Typicality <- resultdf$genderedness

m1<- summary(lm(cosdiff~Typicality, data=resultdf))$coefficients
rownames(m1)[2] <- "Cos. Sim. Diff."
write.csv(m1, file="df3_gender_bk_reg1.csv")
m2 <- summary(lm(inequality1~Typicality+cosdiff, data=resultdf))$coefficients
rownames(m2)[3] <- "Cos. Sim. Diff."
write.csv(m2, file="df3_gender_bk_reg2.csv")
m3 <- summary(lm(inequality2~Typicality+cosdiff, data=resultdf))$coefficients
rownames(m3)[3] <- "Cos. Sim. Diff."
write.csv(m3, file="df3_gender_bk_reg3.csv")
m4 <- summary(lm(c(resultdf$inequality1, resultdf$inequality2) ~ c(resultdf$Typicality,resultdf$Typicality)*c(rep(0, nrow(resultdf)), rep(1, nrow(resultdf)))+c(resultdf$cosdiff, resultdf$cosdiff)*c(rep(0, nrow(resultdf)), rep(1, nrow(resultdf)))))$coefficients
rownames(m4)[2:6] <- c("Typicality", "Equitable Training", "Cos. Sim. Diff.", "Typicality X Equitable Training", "Cos. Sim. Diff. X Equitable Training")
write.csv(m4, file="df3_gender_bk_reg4.csv")

library(vtable)
st <- sumtable(resultdf[,c("cossim01", "cossim11", "cossim02", "cossim12", "cosdiff", "inequality1", "inequality2", "Typicality")], out = "return")
st$Variable[1:7] <- c("Cos. Sim. Vec. #1 - Female", "Cos. Sim. Vec. #1 - Male", "Cos. Sim. Vec. #2 - Female", "Cos. Sim. Vec. #2 - Male", "Cos. Sim. Diff.", "Cos. Sim. Vec. #1 Diff.", "Cos. Sim. Vec. #2 Diff.")
write.csv(st, "summary_stats_df3_bk_gender.csv", row.names = FALSE)


c1 <- c()
c2 <- c()
c3 <- c()
c4 <- c()
c5 <- c()
c6 <- c()
c7 <- c()
set.seed(19970514)
for(i in 1:1000) {
  samphere <- sample(c(1:nrow(resultdf)), nrow(resultdf), replace=TRUE)
  m1<- summary(lm(cosdiff~Typicality, data=resultdf[samphere,]))$coefficients
  c1 <- c(c1, m1[2,1])
  m2 <- summary(lm(inequality1~Typicality+cosdiff, data=resultdf[samphere,]))$coefficients
  c2 <- c(c2, m2[2,1])
  c3 <- c(c3, m2[3,1])
  m3 <- summary(lm(inequality2~Typicality+cosdiff, data=resultdf[samphere,]))$coefficients
  c4 <- c(c4, m3[2,1])
  c5 <- c(c5, m3[3,1])
  m4 <- summary(lm(c(resultdf$inequality1[samphere], resultdf$inequality2[samphere]) ~ c(resultdf$genderedness[samphere],resultdf$genderedness[samphere])*c(rep(0, nrow(resultdf)), rep(1, nrow(resultdf)))+c(resultdf$cosdiff[samphere], resultdf$cosdiff[samphere])*c(rep(0, nrow(resultdf)), rep(1, nrow(resultdf)))))$coefficients
  c6 <- c(c6, m4[4,1])
  c7 <- c(c7, m4[6,1])
  print(i)
}
dir1 <- c(dir1, median(c2))
indir1 <- c(indir1, median(c1*c3))
dir2 <- c(dir2, median(c4))
indir2 <- c(indir2, median(c1*c5))
tot <- c(tot,median(c6))
prop <- c(prop,median(c7))

dir1l <- c(dir1l,quantile(c2,.025))
indir1l <- c(indir1l,quantile(c1*c3,.025))
dir2l <- c(dir2l,quantile(c4,.025))
indir2l <- c(indir2l,quantile(c1*c5,.025))
totl <- c(totl,quantile(c6,.025))
propl <- c(propl,quantile(c7,.025))

dir1u <- c(dir1u,quantile(c2,.975))
indir1u <- c(indir1u,quantile(c1*c3,.975))
dir2u <- c(dir2u,quantile(c4,.975))
indir2u <- c(indir2u,quantile(c1*c5,.975))
totu <- c(totu,quantile(c6,.975))
propu <- c(propu,quantile(c7,.975))

prop1 <- c(prop1, median((c1*c3)/(c2+(c1*c3))))
prop2 <- c(prop2, median((c1*c5)/(c4+(c1*c5))))
prop1l <- c(prop1l, quantile((c1*c3)/(c2+(c1*c3)),.025))
prop2l <- c(prop2l, quantile((c1*c5)/(c4+(c1*c5)),.025))
prop1u <- c(prop1u, quantile((c1*c3)/(c2+(c1*c3)),.975))
prop2u <- c(prop2u, quantile((c1*c5)/(c4+(c1*c5)), .975))

prop3 <- c(prop3, median(c4/c2))
prop4 <- c(prop4, median((c1*c5)/(c1*c3)))
prop3l <- c(prop3l, quantile(c4/c2, .025))
prop4l <- c(prop4l, quantile((c1*c5)/(c1*c3), .025))
prop3u <- c(prop3u, quantile(c4/c2, .975))
prop4u <- c(prop4u, quantile((c1*c5)/(c1*c3), .975))

load("int_resultdf_age_df3.RData")



resultdf$cossim01 <- as.numeric(resultdf$cossim01)
resultdf$cossim11 <- as.numeric(resultdf$cossim11)
resultdf$cossim21 <- as.numeric(resultdf$cossim21)
resultdf$cossim02 <- as.numeric(resultdf$cossim02)
resultdf$cossim12 <- as.numeric(resultdf$cossim12)
resultdf$cossim22 <- as.numeric(resultdf$cossim22)
resultdf$inequality1 <- as.numeric(resultdf$cossim11)-as.numeric(resultdf$cossim01)
resultdf$inequality2 <- as.numeric(resultdf$cossim12)-as.numeric(resultdf$cossim02)
resultdf$Typicality <- resultdf$agedness

m1<- summary(lm(cosdiff~Typicality, data=resultdf))$coefficients
rownames(m1)[2] <- "Cos. Sim. Diff."
write.csv(m1, file="df3_age_bk_reg1.csv")
m2 <- summary(lm(inequality1~Typicality+cosdiff, data=resultdf))$coefficients
rownames(m2)[3] <- "Cos. Sim. Diff."
write.csv(m2, file="df3_age_bk_reg2.csv")
m3 <- summary(lm(inequality2~Typicality+cosdiff, data=resultdf))$coefficients
rownames(m3)[3] <- "Cos. Sim. Diff."
write.csv(m3, file="df3_age_bk_reg3.csv")
m4 <- summary(lm(c(resultdf$inequality1, resultdf$inequality2) ~ c(resultdf$Typicality,resultdf$Typicality)*c(rep(0, nrow(resultdf)), rep(1, nrow(resultdf)))+c(resultdf$cosdiff, resultdf$cosdiff)*c(rep(0, nrow(resultdf)), rep(1, nrow(resultdf)))))$coefficients
rownames(m4)[2:6] <- c("Typicality", "Equitable Training", "Cos. Sim. Diff.", "Typicality X Equitable Training", "Cos. Sim. Diff. X Equitable Training")
write.csv(m4, file="df3_age_bk_reg4.csv")

library(vtable)
st <- sumtable(resultdf[,c("cossim01", "cossim11", "cossim21", "cossim02", "cossim12","cossim22", "cosdiff", "inequality1", "inequality2", "Typicality")], out = "return")
st$Variable[1:9] <- c("Cos. Sim. Vec. #1 - Young", "Cos. Sim. Vec. #1 - Middle", "Cos. Sim. Vec. #1 - Old", "Cos. Sim. Vec. #2 - Young", "Cos. Sim. Vec. #2 - Middle", "Cos. Sim. Vec. #2 - Old", "Cos. Sim. Diff.", "Cos. Sim. Vec. #1 Diff.", "Cos. Sim. Vec. #2 Diff.")
write.csv(st, "summary_stats_df3_bk_age.csv", row.names = FALSE)


c1 <- c()
c2 <- c()
c3 <- c()
c4 <- c()
c5 <- c()
c6 <- c()
c7 <- c()
set.seed(19970514)
for(i in 1:1000) {
  samphere <- sample(c(1:nrow(resultdf)), nrow(resultdf), replace=TRUE)
  m1<- summary(lm(cosdiff~Typicality, data=resultdf[samphere,]))$coefficients
  c1 <- c(c1, m1[2,1])
  m2 <- summary(lm(inequality1~Typicality+cosdiff, data=resultdf[samphere,]))$coefficients
  c2 <- c(c2, m2[2,1])
  c3 <- c(c3, m2[3,1])
  m3 <- summary(lm(inequality2~Typicality+cosdiff, data=resultdf[samphere,]))$coefficients
  c4 <- c(c4, m3[2,1])
  c5 <- c(c5, m3[3,1])
  m4 <- summary(lm(c(resultdf$inequality1[samphere], resultdf$inequality2[samphere]) ~ c(resultdf$agedness[samphere],resultdf$agedness[samphere])*c(rep(0, nrow(resultdf)), rep(1, nrow(resultdf)))+c(resultdf$cosdiff[samphere], resultdf$cosdiff[samphere])*c(rep(0, nrow(resultdf)), rep(1, nrow(resultdf)))))$coefficients
  c6 <- c(c6, m4[4,1])
  c7 <- c(c7, m4[6,1])
  print(i)
}
dir1 <- c(dir1, median(c2))
indir1 <- c(indir1, median(c1*c3))
dir2 <- c(dir2, median(c4))
indir2 <- c(indir2, median(c1*c5))
tot <- c(tot,median(c6))
prop <- c(prop,median(c7))

dir1l <- c(dir1l,quantile(c2,.025))
indir1l <- c(indir1l,quantile(c1*c3,.025))
dir2l <- c(dir2l,quantile(c4,.025))
indir2l <- c(indir2l,quantile(c1*c5,.025))
totl <- c(totl,quantile(c6,.025))
propl <- c(propl,quantile(c7,.025))

dir1u <- c(dir1u,quantile(c2,.975))
indir1u <- c(indir1u,quantile(c1*c3,.975))
dir2u <- c(dir2u,quantile(c4,.975))
indir2u <- c(indir2u,quantile(c1*c5,.975))
totu <- c(totu,quantile(c6,.975))
propu <- c(propu,quantile(c7,.975))

prop1 <- c(prop1, median((c1*c3)/(c2+(c1*c3))))
prop2 <- c(prop2, median((c1*c5)/(c4+(c1*c5))))
prop1l <- c(prop1l, quantile((c1*c3)/(c2+(c1*c3)),.025))
prop2l <- c(prop2l, quantile((c1*c5)/(c4+(c1*c5)),.025))
prop1u <- c(prop1u, quantile((c1*c3)/(c2+(c1*c3)),.975))
prop2u <- c(prop2u, quantile((c1*c5)/(c4+(c1*c5)), .975))

prop3 <- c(prop3, median(c4/c2))
prop4 <- c(prop4, median((c1*c5)/(c1*c3)))
prop3l <- c(prop3l, quantile(c4/c2, .025))
prop4l <- c(prop4l, quantile((c1*c5)/(c1*c3), .025))
prop3u <- c(prop3u, quantile(c4/c2, .975))
prop4u <- c(prop4u, quantile((c1*c5)/(c1*c3), .975))

