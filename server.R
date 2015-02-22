library(shiny)

## the training data has already been cleaned and added missing age, Fare, cabin data
## also added fami.id and fam.size based on family name and SibSp, and Parch
training <- read.csv("titanic_train.csv")

## creating a matrix, turning factors into dummy variables
training.mod.mat <- model.matrix(Survived ~ Pclass + Sex + Age + Fare  + group +
                                     title + cabin.letter + SibSp + Parch +
                                     fam.size + fam.id, data=training)


library(glmnet)

## use cross-validated lasso to find the best lambda value that has the best AUC
lasso.cv.fit <- cv.glmnet(x=training.mod.mat,
                          y=as.factor(training$Survived),
                          family="binomial",
                          type.measure="auc")



# Define server logic for random distribution application
shinyServer(function(input, output) {
    ## use the first line of training data as placeholder, and stepwise replace each field based on user inputs
    ph <- training[1,]
    
    ## directly display output result 
    output$summary <- renderPrint({
        ph$Pclass <- as.numeric(input$pclass)
        
        ph$Sex <- as.factor(input$sex)
        levels(ph$Sex) <- c('male', 'female')
        
        
        ## normalize age
        if(is.na(input$age)) {
            ph$Age <- 36
        }
        else if(input$age > 99) {
            ph$Age <- 99
        }
        else if (input$age <= 1) {
            ph$Age <- 1
        }
        else ph$Age <- input$age
        
        
        ## nomralize SibSp
        if(is.na(input$sibsp)) {
            ph$SibSp <- 1
        }
        else if (input$sibsp <= 0) {
            ph$SibSp <- 0
        }
        else if (input$sibsp >= 10) {
            ph$SibSp <- 10
        }
        else ph$SibSp <- input$sibsp
        
        
        ## nomralize Parch
        if(is.na(input$parch)) {
            ph$Parch <- 2
        }
        else if (input$parch <= 0) {
            ph$Parch <- 0
        }
        else if (input$parch >= 10) {
            ph$Parch <- 10
        }
        else ph$Parch <- input$parch
        
        
        
        ## normalize Fare
        if(is.na(input$fare)) {
            ph$Fare <- 7.25
        }
        else if(input$fare > 870) {
            ph$Fare <- 500
        }
        
        
        ph$group <- as.numeric(input$group)
        
        ph$fam.size <- ph$Parch + ph$SibSp
        
        
        ## turn the user input data into a matrix that has the same fields as the training dataset matrix
        ph.test <- model.matrix(Survived ~ Pclass + Sex + Age + Fare  + group +
                                    title + cabin.letter + SibSp + Parch + 
                                    fam.size + fam.id, data=ph)
        ph.pred <- predict(lasso.cv.fit, newx=ph.test, s="lambda.min", type="response")
    
        paste0(round(ph.pred[1,1], 2) * 100,"%"," likely to survive.")


    })
    

})