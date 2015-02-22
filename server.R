library(shiny)
training <- read.csv("titanic_train.csv")


training.mod.mat <- model.matrix(Survived ~ Pclass + Sex + Age + Fare  + group +
                                     title + cabin.letter + SibSp + Parch +
                                     fam.size + fam.id, data=training)


library(glmnet)


lasso.cv.fit <- cv.glmnet(x=training.mod.mat,
                          y=as.factor(training$Survived),
                          family="binomial",
                          type.measure="auc")



# Define server logic for random distribution application
shinyServer(function(input, output) {
    ph <- training[1,]
    # Reactive expression to generate the requested distribution. This is 
    # called whenever the inputs change. The output renderers defined 
    # below then all used the value computed from this expression
#      data <- reactive({  
# # #         dist <- switch(input$dist,
# # #                        norm = rnorm,
# # #                        unif = runif,
# # #                        lnorm = rlnorm,
# # #                        exp = rexp,
# # #                        rnorm)
# #       
#     sex <- input$sex
#     age <- input$age
#     group <- input$group
#     t.string <- c(sex, age, group)
# #         input$n
#     })
    
    # Generate a plot of the data. Also uses the inputs to build the 
    # plot label. Note that the dependencies on both the inputs and
    # the data reactive expression are both tracked, and all expressions 
    # are called in the sequence implied by the dependency graph
#     output$plot <- renderPlot({
#         dist <- input$dist
#         n <- input$n
#         
#         hist(data(), 
#              main=paste('r', dist, '(', n, ')', sep=''))
#     })
    

#     data <- reactive({  
# 
# 
#     })
    
    
    
    
    # Generate a summary of the data
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
        
        
        
        
        #         ph.pred <- predict(lasso.cv.fit, newx=ph.mod, s="lambda.min", type="class")
        ph.test <- model.matrix(Survived ~ Pclass + Sex + Age + Fare  + group +
                                    title + cabin.letter + SibSp + Parch + 
                                    fam.size + fam.id, data=ph)
        ph.pred <- predict(lasso.cv.fit, newx=ph.test, s="lambda.min", type="response")
    
        paste0(round(ph.pred[1,1], 2) * 100,"%"," likely to survive.")


#         data <- c(input$sex, input$age, input$group, 
#           input$fare, input$pclass, input$title,
#           input$sibsp, input$parch)
#         data <- c(data, 1509)
        
    
        
    })
    
    # Generate an HTML table view of the data
#     output$table <- renderTable({
#         data.frame(x=data())
#     })
})