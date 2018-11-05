
#Note: train set and test set need to be ranked separately.
#train <- read.csv ('C:/Users/z5023853/OneDrive - UNSW/Jupyter/Feature_selected/train_cat_ac.csv', header = TRUE)
#test <- read.csv ('C:/Users/z5023853/OneDrive - UNSW/Jupyter/Feature_selected/test_cat_ac.csv', header = TRUE)

df_train <- read.csv ('C:/Users/z5023853/OneDrive - UNSW/Jupyter/Feature_selected/train_cat_ac.csv', header = TRUE)
df_test <- read.csv ('C:/Users/z5023853/OneDrive - UNSW/Jupyter/Feature_selected/test_cat_ac.csv', header = TRUE)

library ('dplyr')
library ('onehot')
###########
#mytable <- xtabs (~ivis+iev+ilc+iqt+icost+icd+ipro, df)

df_en <- df_train [c('CS_cat', 'BT_cat', 'per_cat', 'PT_cat', 'yr_cat', 'contract', 'Location')]
df_en$Location <- as.factor (df_en$Location)
df_en_test <- df_test [c('CS_cat', 'BT_cat', 'per_cat', 'PT_cat', 'yr_cat', 'contract', 'Location')]
df_en_test$Location <- as.factor (df_en_test$Location)

encoder = onehot (df_en)
x_end <- predict(encoder, df_en)
x_test_end <- predict (encoder, df_en_test)

train_enc <- data.frame (x_end)
test_enc <- data.frame (x_test_end)

df_train$mid <- rep(1:323)
train_enc$mid <- rep (1:323)
df_train <- merge (df_train, train_enc, by = "mid")

df_test$mid <- rep (1:82)
test_enc$mid <- rep (1:82)
df_test <- merge (df_test, test_enc, by = "mid")
###########
df_train$sub <- "train"
df_test$sub <- "test"
df <- rbind (df_train, df_test)

df$qid [df$iothers=='1'] <- '8'
df$qid [df$ilc=='1'] <- '7'
df$qid [df$ipro=='1'] <- '6'
df$qid [df$icd=='1'] <- '5'
df$qid [df$icost=='1'] <- '4'
df$qid [df$iev=='1'] <- '3'
df$qid [df$iqt=='1'] <- '2'
df$qid [df$ivis=='1'] <- '1'
df$qid [is.na (df$qid)] <- '8'
df$qid <- as.factor (df$qid)
summary (df$qid)

#create subrank by summarising row-wise
subrank<- df %>%
  rowwise %>%
  mutate (subrank = 10*sum (c(ivis, iev, iqt, icost, icd, ipro, ilc, iothers))) %>%
  select (subrank)

subrank$nid <- rep (1:405)
df$nid <- rep (1:405)
df <- merge (df, subrank, by = "nid")

#order by frequency
rank <- df %>%
  rowwise %>%
  mutate (rank = sum (subrank, qid)) %>%
  select (rank)

rank$nid <- rep (1:405)
df <- merge (df, rank, by = "nid")
#############
for (x in c("ivis", 'iev', 'ilc', 'iqt', 'icost', 'icd', 'ipro', 'iothers')){
  print (count (df, vars = x))
}

#############
df_final <- df [,3:101]

############
df_final$rank <- as.factor (df_final$rank)
df_final$rank_cat[df_final$rank =="8"] <- "1"
df_final$rank_cat[df_final$rank =="11"] <- "2"
df_final$rank_cat[df_final$rank =="12"] <- "3"
df_final$rank_cat[df_final$rank =="13"] <- "4"
df_final$rank_cat[df_final$rank =="14"] <- "5"
df_final$rank_cat[df_final$rank =="15"] <- "6"
df_final$rank_cat[df_final$rank =="16"] <- "7"
df_final$rank_cat[df_final$rank =="17"] <- "8"
df_final$rank_cat[df_final$rank =="18"] <- "9"
df_final$rank_cat[df_final$rank =="21"] <- "10"
df_final$rank_cat[df_final$rank =="22"] <- "11"
df_final$rank_cat[df_final$rank =="23"] <- "12"
df_final$rank_cat[df_final$rank =="24"] <- "13"
df_final$rank_cat[df_final$rank =="31"] <- "14"
df_final$rank_cat[df_final$rank =="32"] <- "15"
df_final$rank_cat[df_final$rank =="33"] <- "16"
df_final$rank_cat[df_final$rank =="41"] <- "17"
df_final$rank_cat[df_final$rank =="42"] <- "18"
df_final$rank_cat[df_final$rank =="43"] <- "19"
df_final$rank_cat[df_final$rank =="51"] <- "20"
df_final$rank_cat[df_final$rank =="52"] <- "21"
df_final$rank_cat[df_final$rank =="61"] <- "22"
df_final$rank_cat[df_final$rank =="71"] <- "23"
#############
df_train <- df_final [1:323,]
df_test <- df_final [324:405,]

# Train Costs (IM1, IM2, IM3, OS1)
IM1 <- as.matrix(df_test["IM1"])
IM2 <- as.matrix(df_test["IM2"])
IM3 <- as.matrix(df_test["IM3"])
OS1 <- as.matrix(df_test["OS1"])
rank <- as.matrix(df_test["rank_cat"])
qid <- as.matrix(df_test["qid"])
Lrg <- as.matrix(df_test ["CS_cat.Large"])
Med <- as.matrix (df_test ["CS_cat.Medium"])
Sml <- as.matrix (df_test ["CS_cat.Small"])
Arc <- as.matrix (df_test ["BT_cat.Architect"])
Csl <- as.matrix (df_test ["BT_cat.Consultant"])
Ctr <- as.matrix (df_test ["BT_cat.Contractor"])
Eng <- as.matrix (df_test ["BT_cat.Engineering"])
p10 <- as.matrix (df_test ["per_cat..10."])
p50 <- as.matrix (df_test ["per_cat..50."])
p00 <- as.matrix (df_test ["per_cat.0."])
p13 <- as.matrix (df_test ["per_cat.10._30."])
p35 <- as.matrix (df_test ["per_cat.30._50."])
Com <- as.matrix (df_test ["PT_cat.Commercial"])
Ids <- as.matrix (df_test ["PT_cat.Industrial"])
Ifa <- as.matrix (df_test ["PT_cat.Infrastructure"])
Res <- as.matrix (df_test ["PT_cat.Residential"])
m06 <- as.matrix (df_test ["yr_cat..6.months"])
yr5 <- as.matrix (df_test ["yr_cat...5.years"])
yr0 <- as.matrix (df_test ["yr_cat.0"])
yr12 <- as.matrix (df_test ["yr_cat.1.2.years"])
yr25 <- as.matrix (df_test ["yr_cat.2.5.years"])
m12 <- as.matrix (df_test ["yr_cat.6.12.months"])
DB <- as.matrix (df_test ["contract.DB"])
DBB <- as.matrix (df_test ["contract.DBB"])
EPC <- as.matrix (df_test ["contract.EPC"])
IPD <- as.matrix (df_test ["contract.IPD"])
Oth <- as.matrix (df_test ["contract.Others"])
L1 <- as.matrix (df_test ["Location.1"])
L2 <- as.matrix (df_test ["Location.2"])

trainC <- data.frame (rank, 
                      qid = paste ("qid:", rep (qid), sep =""),
                      Lrg = paste ("1:", rep(Lrg), sep = ""),
                      Med = paste ("2:", rep(Med), sep = ""),
                      Sml = paste ("3:", rep(Sml), sep = ""),
                      Arc = paste ("4:", rep(Arc), sep = ""),
                      Csl = paste ("5:", rep(Csl), sep = ""),
                      Ctr = paste ("6:", rep(Ctr), sep = ""),
                      Eng = paste ("7:", rep(Eng), sep = ""),
                      p10 = paste ("8:", rep(p10), sep = ""),
                      p50 = paste ("9:", rep(p50), sep = ""),
                      p00 = paste ("10:", rep(p00), sep = ""),
                      p13 = paste ("11:", rep(p13), sep = ""),
                      p35 = paste ("12:", rep(p35), sep = ""),
                      Com = paste ("13:", rep(Com), sep = ""),
                      Ids = paste ("14:", rep(Ids), sep = ""),
                      Ifa = paste ("15:", rep(Ifa), sep = ""),
                      Res = paste ("16:", rep(Res), sep = ""),
                      m06 = paste ("17:", rep(m06), sep = ""),
                      yr5 = paste ("18:", rep(yr5), sep = ""),
                      yr0 = paste ("19:", rep(yr0), sep = ""),
                      yr12= paste ("20:", rep(yr12), sep = ""),
                      yr25= paste ("21:", rep(yr25), sep = ""),
                      m12 = paste ("22:", rep(m12), sep = ""),
                      DB = paste ("23:", rep(DB), sep = ""),
                      DBB = paste ("24:", rep(DBB), sep = ""),
                      EPC = paste ("25:", rep(EPC), sep = ""),
                      IPD = paste ("26:", rep(IPD), sep = ""),
                      Oth = paste ("27:", rep(Oth), sep = ""),
                      L1 = paste ("28:", rep(L1), sep = ""),
                      L2 = paste ("29:", rep(L2), sep = ""),
                      IM1 = paste ("30:", rep(IM1), sep =""),
                      IM2 = paste ("31:", rep(IM2), sep =""),
                      IM3 = paste ("32:", rep(IM3), sep =""),
                      OS1 = paste ("33:", rep(OS1), sep =""))
trainC <- trainC [order (qid),]

library ("xlsx")
write.table(trainC, "trainC_test.txt", sep=" ", col.names = F, row.names = F) 
