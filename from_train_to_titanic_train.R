##setwd("./kaggle/titanic")
library(caret)
titanic <- read.csv("train.csv", na="")
test <- read.csv("test.csv", na="")
test$Survived <- 0
combi <- rbind(titanic, test)
combi$Survived <- as.factor(combi$Survived)

## family size because size matters
combi$fam.size <- combi$SibSp + combi$Parch


## family id if fam.size is larger and equal than 3
combi$fam.id <- 0
large.fam <- which(combi$fam.size >= 3)
combi[large.fam,]$fam.id <- paste0(combi[large.fam, "last.name"], 
                                   "_", 
                                   combi[large.fam,"fam.size"])
combi$fam.id <- as.factor(combi$fam.id)


## missing fare fix
ticket <- data.frame(combi)
ticket$Ticket <- as.integer(as.character(combi$Ticket))
tix.subset <- subset(ticket, Ticket > 2800 & Ticket < 4200)
tix.subset[order(tix.subset$Ticket),c("Ticket", "Fare", "Pclass")]
combi$Fare[which(is.na(combi$Fare))] <- 7.8875

## parse last names
library(stringr)
combi$last.name <- as.character(combi$Name)

getLast.Name <- function(x) {
    Last.Name <- str_trim(strsplit(x, ",")[[1]][1])
    Last.Name
}

combi$last.name <- sapply(combi$last.name, getLast.Name)


## feature design: if someone is traveling alone or as a group
combi$group <- 0

for (i in 1:nrow(combi)) {
    
    if (is.na(combi[i, "Cabin"]) == TRUE) {
        next
    }
    if(combi[i, "Cabin"] %in% combi[-i, "Cabin"] == TRUE
       & nchar(as.character(combi[i, "Cabin"])) >=2 ) {
        combi[i, "group"] <- 1
    }
    
    
    #  print(nchar(as.character(combi[i, "Cabin"])))
    
}


## cabin letter isolation
combi$cabin.letter <- substr(combi$Cabin, 1, 1)
combi$Ticket <- as.factor(combi$Ticket)

## cabin letter prediction based on Pclass, last.name, SibSp, Parch,  Fare
## embarked, , group, and ticket

cabin.letter.pred.full <- data.frame()
for (i in 1:nrow(combi)) {
    
    if(is.na(combi[i, "cabin.letter"]) == FALSE) {
        next
    }
    if(combi[i, "last.name"] %in% combi[-i, "last.name"] == TRUE) {
        same.name <- subset(combi[-i, ], last.name == combi[i, "last.name"])
        for (j in 1:nrow(same.name)) {
            if (same.name[j, "Pclass"] == combi[i, "Pclass"]
                & same.name[j, "Fare"] == combi[i, "Fare"]
                & same.name[j, "Ticket"] == combi[i, "Ticket"]
                & is.na(same.name[j, "Cabin"]) == FALSE) {
                #                 cabin.letter.pred <- data.frame()
                #                 cabin.letter.pred$PassengerId <- same.name[j, "PassengerId"]
                #                 cabin.letter.pred$cabin.letter <- combi[i, "cabin.letter"]
                #                 cabin.letter.pred$Cabin <- combi[i, "Cabin"]
                #                 cabin.letter.pred.full <- rbind(cabin.letter.pred.full, cabin.letter.pred)
                #                 
                print(same.name[j, "PassengerId"])
                print(same.name[j, "Cabin"])
            }
        }
    }
}


## cabin letter prediction

cabin.letter.fit.rf <- train(as.factor(cabin.letter) ~ Pclass + Fare, 
                             data=subset(combi, is.na(Cabin)==FALSE), 
                             method="rf", trControl=trainControl(method="cv", number=10))

cabin.letter.pred <- predict(cabin.letter.fit.rf, subset(combi, is.na(Cabin)))

combi[is.na(combi$Cabin), "cabin.letter"] <- as.character(cabin.letter.pred)

combi$cabin.letter <- as.factor(combi$cabin.letter)

## isolate title
library(stringr)
combi$Name <- as.character(combi$Name)

cleanTitle <- function(x) {
    nameSplit <- str_trim(strsplit(x, ",")[[1]][2])
    nameSplitClean <- strsplit(nameSplit, "\\.")[[1]][1]
    nameSplitClean
}
combi$title <- sapply(combi$Name, cleanTitle)
combi$title <- as.factor(combi$title)


## fill in Age
# age.fit.rf <- train(Age ~ Pclass + Sex + Fare + cabin.letter + group + title, 
#                    data=combi,method="rf")
# age.fit.lm <- train(Age ~ Pclass + Sex + Fare + cabin.letter + group, 
#                     data=combi,method="lm")
age.fit.gbm <- train(Age ~ Pclass + Sex + Fare + group + title, 
                     data=combi,method="gbm", verbose=F)

age.na <- combi[is.na(combi$Age), c("Pclass", "Sex", "Fare",
                                    "group", "title")]
age.na.pred <- predict(age.fit.gbm, age.na)
combi[is.na(combi$Age), "Age"] <- age.na.pred


# ## seprating Par from Parch
# combi$par <- 0
# combi$ch <- 0
# parch <- subset(combi, Parch != 0)
# 
# for (i in 1:nrow(parch)) {
#     if (parch[i, "last.name"] %in% parch[-i, "last.name"]) {
#         parch.ln <- subset(parch[-i,], last.name == parch[i, "last.name"])
#         for (j in 1:nrow(parch.ln)) {
#             if ((parch.ln[j, "Age"] - parch[i, "Age"]) > 13) {
#                 parch[i, "par"] <- 1
#             }
#             else if ((parch.ln[j, "Age"] - parch[i, "Age"]) < -13) {
#                 parch[i, "ch"] <- 1
#             }
#         }
#     }
#     
# }
# 
# combi[which(combi$Parch != 0),]$par <- parch$par
# combi[which(combi$Parch != 0),]$ch <- parch$ch
# 


##separateing training and testing
training <- combi[1:891,]
testing <- combi[892:nrow(combi),]

## creating titanic_train.csv
write.csv(result_fresh_vote, "result_fresh_vote.csv", row.names=F)




