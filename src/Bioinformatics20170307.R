###########################################
# Bioinformatics Statistic
# Editor: Hantaegune
# Date: 2017-03-07
###########################################
# Data Prep
library("imputeMissings") 
library("openxlsx")
dfAll <- read.xlsx("D:/Rworkspace/ItemResult1.xlsx", sheet = 1, startRow = 2, colNames = TRUE)
# head(dfAll)
# dim(dfAll)
############################################
# Delete Sex is NULL
############################################

dtSex <- dfAll[!is.na(dfAll[,"Sex"]),]
dim(dtSex)
head(dtSex)

Names<-paste(as.character(c(1:nrow(dtSex))),as.character(dtSex[,3]),sep = "")
length(unique(Names)) # check unique
head(dtSex)
dtSetName<-dtSex[,-1:-3]
head(dtSetName)
rownames(dtSetName) <- Names

head(dtSetName)
dtSex.imp <- impute(dtSetName)
head(dtSex.imp)
dim(dtSex.imp)

########################################

library(dplyr)
library(ggplot2)

dtSex_df <- tbl_df(dtSex.imp)
dtSex_df
summary(dtSex_df)
# 함수 filter()를 이용한 데이터 추출
dtSex_df<-filter(dtSex_df, Age < 100)
dim(dtSex_df)
summary(dtSex_df)
head(dtSex_df)

# character data frame change to numeric data frame    (특정 열)
dtSex_df[, 3:ncol(dtSex_df)] <- sapply(dtSex_df[, 3:ncol(dtSex_df)], as.numeric)

sum<-summary(dtSex_df)
write.csv(sum,"D:/Rworkspace/summaryDtSexImp1031.csv")
str(dtSex_df)

# 함수 group_by()를 이용한 그룹화
groupSex <- group_by(dtSex_df, Sex)
delay <- summarise(groupSex, count = n(), Protein = mean(Protein.total, na.rm = TRUE), 
                   Albumin = mean(Albumin, na.rm = TRUE))

library(ggplot2)
ggplot(delay, aes(Protein, Albumin)) + geom_point(aes(size = count), alpha = 1/2) + 
  geom_smooth() + scale_size_area()

boxplot(as.numeric(Sex)~Albumin, data = dtSex_df)


# character data frame change to numeric data frame    (특정 열)
dtSex_test <- sapply(dtSex_df[, 1], as.numeric)
