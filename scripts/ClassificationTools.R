###############################################################
#### Machine learning tools
# - plot2Dclass()
# - plotClassPerformance()
# - genmat()
# - SensAnalysisMLP()
#
# Contributors: 
# - Jose Portela
# - Guillermo Mestre
# - Jaime Pizarroso



plot2Dclass <- function(X, Y, model, var1=NULL, var2=NULL, selclass = NULL, np_grid = 200){
  ###############################################################################################
  ## Custom function for plotting the results of a classification model with two input variables 
  ## and one output variable.
  ## 
  ## Function arguments:
  ##    X -> data frame with two input variables
  ##    Y -> the real output variable "Y"
  ##    model -> fitted model using caret
  ##    var1 -> name of the x axis variable 
  ##    var2 -> name of the y axis variable
  ##    class to be analyzed (probabilities)
  ##    np_grid -> number of discretization points in each dimension
  
  #Load directlabels package for assigning labels to a contourplot
  existe <- require(directlabels)
  if (existe == FALSE){
    install.packages("directlabels",dep=TRUE)
    library(directlabels)
  }
  #Load gridExtra package for multiple plots
  existe <- require(gridExtra)
  if (existe == FALSE){
    install.packages("gridExtra",dep=TRUE)
    library(gridExtra)
  }
  
  #Check that X has two variables
  if ( ncol(X) < 2 ){
    stop("There have to be at least 2 input variables")
  }
  #Check input variable names
  if (is.null(var1) | is.null(var2)){
    stop("Please, introduce the variable names for each axis (var1 and var2)")
  }
  
  #Change input variable colnames to X1 and X2 and output to Y
  c.names <- colnames(X)
  #Check input variable names
  if (sum(c(var1,var2) %in% c.names) < 2){
    stop(paste("Variable ",var1," or ",var2," do not exist in the data"))
  }
  fdata <- X[,c(var1,var2)]
  fdata$Y <- Y
  colnames(fdata) <- c("X1","X2","Y")
  

  
  
  #If there are only two input variables, plot partition of input space
  if ( ncol(X) == 2 ){
    #Grid for evaluating the model 
    np.X1 <- seq(from = min(fdata$X1), to = max(fdata$X1), length.out = np_grid)
    np.X2 <- seq(from = min(fdata$X2), to = max(fdata$X2), length.out = np_grid)
    grid_X1_X2 <- expand.grid(X1 = np.X1, X2 = np.X2)
    colnames(grid_X1_X2) <- c.names
    
    #Predict each point of the grid
    grid_X1_X2$pred <- predict(model, type="raw" , newdata = grid_X1_X2) #predict class
    
    #Obtain probabilites of the model in the grid and add to the grid data frame
    for(i in seq_len(length(levels(fdata$Y)))){
      grid_X1_X2[,ncol(grid_X1_X2)+1] <- predict(model, type="prob" , newdata = grid_X1_X2)[,i]
      names(grid_X1_X2)[ncol(grid_X1_X2)] <- paste("prob",levels(fdata$Y)[i],sep = "_")
    }
  }
  
  #Predict input data
  pred <- predict(model, type="raw" , newdata = X) #predict class
  fdata = cbind(fdata, pred)
  
  #List to plot all the plots together
  plotlist <- list()
  #Select variable to plot
  if (is.null(selclass)){
    response <- select.list(levels(fdata$Y),
                            multiple = FALSE,
                            graphics = TRUE,
                            title = "Choose variable to plot") 
  }else{
    response <- selclass
  }
  
  if(response != ""){
    
    #If there are only two input variables, plot partition of input space
    if ( ncol(X) == 2 ){
        prob <- paste("prob",response, sep = "_")
        #Add variable to grid which is the variable to plot
        grid_X1_X2$prob <- grid_X1_X2[,grep(prob, names(grid_X1_X2))]
        #Classification of input space
        plotlist[[1]] <- ggplot()+
          geom_point(data = grid_X1_X2, mapping = aes(x = X1, y = X2, colour = pred))+
          labs(title="Classification of input space", x=var1, y=var2) + 
          theme(legend.position = "right")
        
        #Probabilities estimated for input space
        plotlist[[2]] <-ggplot()+
          geom_point(data = grid_X1_X2, mapping = aes(x = X1, y = X2, colour = prob))+
          labs(title= paste("Probabilities estimated for input space, class:",response), x=var1, y=var2) +
          theme(legend.position = "right")
        
        
        #Classification resutls
        plotlist[[3]] <- ggplot()+
          stat_contour(data = grid_X1_X2, mapping = aes(x = X1, y = X2, z = prob), colour = "navyblue", breaks = 0.5)+
          geom_point(data = cbind(fdata,Y_Yest <- interaction(fdata$Y,pred, sep = "_")) , aes(x=X1,y=X2, color = Y_Yest))
        plotlist[[3]] <- plotlist[[3]] + labs(title=paste("Classification results, class:", response), x=var1, y=var2) +
          theme(legend.position = "right")
        
        #Plot of probabilities
         plotlist[[4]] <- ggplot()+
           stat_contour(data = grid_X1_X2, mapping = aes(x = X1, y = X2, z = prob, colour = ..level..), binwidth = 0.1)+
           geom_point(data = fdata, aes(x= X1, y = X2, fill = Y),shape = 21,color = "NA")+
           labs(title=paste("Estimated classes and probability contour lines for class:",response), x=var1, y=var2)
        #plotlist[[4]] <- direct.label(plotlist[[4]], method="bottom.pieces") #label of contour lines
        
        
        #Plot the list of plots created before
        grid.arrange(grobs = plotlist,
                     nrow = floor(sqrt(length(plotlist))),
                     ncols = ceiling(sqrt(length(plotlist))))
        
    }else{
      prob <- paste("prob",response, sep = "_")

      #Classification resutls
      plotlist[[1]] <- ggplot()+
        geom_point(data = cbind(fdata,Y_Yest <- interaction(fdata$Y,pred, sep = "_")) , aes(x=X1,y=X2, color = Y_Yest))
      plotlist[[1]] <- plotlist[[1]] + labs(title=paste("Classification results, class:", response), x=var1, y=var2) +
        theme(legend.position = "right")
      
      #Estimated classes
      plotlist[[2]] <- ggplot()+
        geom_point(data = fdata, aes(x= X1, y = X2, color = Y))+
        labs(title=paste("Estimated classes:",response), x=var1, y=var2)
      
      
      #Plot the list of plots created before
      grid.arrange(grobs = plotlist,
                   nrow = 1,
                   ncols = 2)
    }
    
    # #Wait for the plot to finish
    # Sys.sleep(1)
    # 
    # # Ask if another variable should be plotted
    # response2 <- select.list(c("YES", "NO"),
    #                          multiple = FALSE,
    #                          graphics = TRUE,
    #                          title = "Do you want to plot other?") 
    # if(response2 == "YES"){
    #   #Select variable to plot
    #   response <- select.list(levels(fdata$Y),
    #                           multiple = FALSE,
    #                           graphics = TRUE,
    #                           title = "Choose variable to plot") 
    # }else{
    #   #Exit the program
    #   response <- ""
    # }
  }
}



plotClassPerformance <- function(Y, prob.est, selClass = NULL){
  
  #Load ROCR package for assigning labels to a contourplot
  existe <- require(ROCR)
  if (existe == FALSE){
    install.packages("ROCR",dep=TRUE)
    library(ROCR)
  }
  
  
  #Check selclass
  if (is.null(selClass)){
      stop("The class to be analyzed should be given")
  }
  
  #selected class
  #check probabilities
  if (ncol(prob.est) != 1){
    prob.est <- prob.est[,selClass]
  }
  

  #calibration plot
  #Use cuts for setting the number of probability splits
  df <- data.frame(Y,prob.est)
  calPlotData <- calibration(Y ~ prob.est, data = df, class = selClass ,cuts = 6) 
  P1 <- xyplot(calPlotData, auto.key = list(columns = 2),
         main="Plot 1/3: Calibration plot")
  print(P1)
    
  #Histogram comparison
  P2 <- ggplot(df) +
    geom_histogram(aes(x = prob.est, fill = Y), bins = 15) +
    facet_wrap(~Y) +
    labs(x=paste("Probability of Class ",selClass), title = "Plot 2/3: Probability histograms")
  print(P2)
  

  #create a 2 by 1 matrix of plots
  par(mfrow=c(1,2),oma=c(0,0,2,0)) 
  pred <- prediction(prob.est, Y,
                  label.ordering = levels(fTR_eval$Y)[order(levels(fTR_eval$Y) %in% selClass, decreasing = FALSE)])
  perf <- performance(pred, "tpr", "fpr")
  plot(perf, avg= "threshold", colorize=T, lwd= 3,
       coloraxis.at=seq(0,1,by=0.2),
       main= "ROC curve")
  grid()
  #plot(perf, col="gray78", add=T)
  #plot(perf, avg= "threshold", colorize=T, colorkey=F,lwd= 3,
  #     main= "ROC curve",add=T)
  
  perf <- performance(pred, "acc")
  plot(perf, lwd=3,col='blue',
       main= "Accuracy across the range of possible cutoffs")
  grid()
  # plot(performance(pred, "cal", window.size= 10),
  #      avg="vertical",
  #      main= "How well are the probability predictions calibrated?")
  
  # plot(0,0,type="n", xlim= c(0,1), ylim=c(0,7),
  #      xlab="Cutoff", ylab="Density",
  #      main="How well do the predictions separate the classes?")
  # clases <- levels(Y)
  # lines(density(pred@predictions[[1]][pred@labels[[1]]==clases[1]]), col= "blue",lwd= 2)
  # lines(density(pred@predictions[[1]][pred@labels[[1]]==clases[2]]), col="red", lwd= 2)
  # legend("topright",legend = clases, lwd =2, col = c("red","blue"))
  # grid()
  title("Plot 3/3: ROC curves", outer=TRUE)
  
  #restore par
  par(mfrow=c(1,1)) 
  
  
  #Print auc
  print(paste("Area under the ROC curve (auc): ",performance(pred, "auc")@y.values[[1]]))
}


genmat <- function(nc,npt,sigma){
  # Function that creates a dataset of points associated to different classes
  # 
  # Input arguments:
  #   nc --> 2 #number of classes to generate
  #   npt -->  number of points to generate per click 
  #   sigma --> Standard deviation of the gaussians used for randomly select points in each click
  #
  # Output argument:
  #   Dataframe with x1 and x2 values of axis and Y variable with the class.
  
  
  #Check parameter inputs
  if (is.null(nc)) 
    stop("Please, input the number of classes (nc)")
  if (is.null(npt)) 
    stop("Please, input the number of points per click (npt)")
  if (is.null(sigma)) 
    stop("Please, input the standard deviation for the gaussian distributions (sigma)")

  
  existe <- require(mvtnorm)
  if (existe == FALSE){
    install.packages("mvtnorm",dep=TRUE)
    library(mvtnorm)
  }
  
  #ranges
  rx = seq(from = -10,to = 10, length.out = 100 )
  ry = seq(from = -10,to = 10, length.out = 100 )
  xy = expand.grid(X1 = rx, X2 = ry)
  plot(xy,pch = ".", xlim = range(xy$X1), ylim = range(xy$X2))
  simdata = data.frame(X1 = double(), X2 = double(),Y = integer())
  clase = 1
  message(c("Click on the plot to create points of category ",clase))
  message("Press Escape to move on to the next category")
  while(1){
    sel = identify(xy, n=1, plot = FALSE)
    if(!length(sel)){
      if(clase == nc)
        break
      else
        clase = clase+1
      message("---------------------------------------------------------")
      message(c("Click on the plot to create points of category ",clase))
      message("Press Escape to move on to the next category")
    }else{ 
      aux = rmvnorm(npt, mean = as.numeric(xy[sel,]), sigma = sigma*diag(2))
      aux = data.frame(X1 = aux[,1], X2= aux[,2], Y= rep(clase,length(aux[,1]),1))
      simdata = rbind(simdata,aux)
    }
    plot(x = simdata$X1, y = simdata$X2, col = simdata$Y , pch = 20, axes = FALSE, xlab = "", ylab = "", xlim = range(xy$X2), ylim = range(xy$X1))
    par(new=T)
    plot(xy,pch = ".", xlim = range(xy$X1), ylim = range(xy$X2))
  }
  
  #Classes start from 0
  simdata$Y = simdata$Y-1
  #Randomize data
  simdata <- simdata[sample.int(length(simdata$Y)),]
  return(simdata)
}


SensAnalysisMLP <-function(MLP.fit){
  ############################################################################
  #### Function for evaluating the sensitivities of the inputs variables
  #### Argument:
  ####    -MLP.fit: fitted model from caret package using nnet method
  #### Output:
  ####    -sens: dataframe with the sensitivities obtained for each variable
  ############################################################################
  
  existe <- require(tidyr)
  if (existe == FALSE){
    install.packages("tidyr",dep=TRUE)
    library(tidyr)
  }
  #Load gridExtra package for multiple plots
  existe <- require(gridExtra)
  if (existe == FALSE){
    install.packages("gridExtra",dep=TRUE)
    library(gridExtra)
  }
  
  library(NeuralNetTools)
  #Multiplot function will be sourced at the same time
  #source("Multiplot.R")
  
  
  #Supress warnings
  options(warn=-1)
  
  
  #Obtain structure of fitted model
  mlpstr = MLP.fit$finalModel$n
  #Obtain weights
  nwts = neuralweights(MLP.fit$finalModel)
  wts = nwts$wts
  #VariableNames
  varnames <- MLP.fit$finalModel$coefnames
  #TestData
  dummies <- dummyVars(.outcome ~ ., data = MLP.fit$trainingData, fullRank=TRUE, sep = NULL)
  TestData <- data.frame(predict(dummies, newdata = MLP.fit$trainingData))
  TestData = TestData[,varnames]
  if(!is.null(MLP.fit$preProcess)){
    TestData = predict(MLP.fit$preProcess, TestData[,varnames])
  }
  
  sigfun <- function(x){
    return(1 / (1 + exp(-x)))
    #return(tanh(x))
  }
  dersigfun <- function(x){
    if (x>100){
      return(0)
    }else{
      return(exp(x) / (exp(x)+1)^2)
    }
    #return((1-tanh(x)^2))
  }
  
  der = matrix(nrow = dim(TestData)[1], ncol = dim(TestData)[2])
  out = matrix(nrow = dim(TestData)[1], ncol = mlpstr[3])
  #For each row in the TestData
  for(irow in 1:nrow(TestData)){
    #For each output
    for(io in 1:mlpstr[3]){
      #For each input
      outnh = rep(0,mlpstr[2])
      for(ih in 1:mlpstr[2]){
        outnh[ih] = wts[[ih]][1] + sum(wts[[ih]][-1]*TestData[irow,])
      }
      for(ii in 1:mlpstr[1]){
        #Calculate derivatives of output io as a function of the input ii
        #hiden units
        der[irow,ii] = 0
        for(ih in 1:mlpstr[2]){
          der[irow,ii] = der[irow,ii] + wts[[mlpstr[2]+1]][ih+1]*dersigfun(outnh[ih])*wts[[ih]][ii+1]
        }
        outno = wts[[mlpstr[2]+1]][1] + sum(wts[[mlpstr[2]+1]][-1]*sigfun(outnh))
        if(MLP.fit$modelType == "Classification"){
          der[irow,ii] = der[irow,ii] * dersigfun(outno)
          out[irow] = sigfun(outno)         
        }else{
          der[irow,ii] = der[irow,ii]
          out[irow] = outno       
        }
      }
    }
  }
  
  #Remove n
  #der <- na.omit(der)
  
  sens = data.frame(varNames = varnames, mean = colMeans(der,na.rm = TRUE), std = apply(der, 2, sd, na.rm=TRUE), meanSensSQ =colMeans(der^2,na.rm = TRUE))
  
  
  
  #List to plot all the plots together
  plotlist <- list()
  
  
  #
  plotlist[[1]] <- ggplot(sens)+geom_point(aes(x=mean, y = std))+
    geom_label(aes(x=mean, y = std,label=varnames),position = "nudge")+
    geom_point(aes(x=0,y=0),size = 5,color="blue")+
    geom_hline(aes(yintercept = 0),color="blue") + geom_vline(aes(xintercept = 0),color="blue")+
    #coord_cartesian(xlim = c(min(sens$mean,0)-0.1*abs(min(sens$mean,0)), max(sens$mean)+0.1*abs(max(sens$mean))), ylim = c(0, max(sens$std)*1.1))+
    labs(x= "mean(Sens)",y="std(Sens)")
  
  
  plotlist[[2]] <- ggplot()+geom_col(aes(x=MLP.fit$finalModel$coefnames, y=colMeans(der^2,na.rm = TRUE), fill = colMeans(der^2,na.rm = TRUE)))+
    labs(x= "Input variables", y="mean(Sens^2)")+guides(fill = "none")
  
  der2 = as.data.frame(der)
  colnames(der2) = varnames
  dataplot = gather(der2,varnames)
  #bwidth <- sd(dataplot$value)/(1.34*(dim(dataplot)[1]/length(varnames)))
  plotlist[[3]] <- ggplot(dataplot)+geom_density(aes(x=value, fill=varnames),alpha = 0.4,bw="bcv")+
    labs(x= "Sens", y="density(Sens)")+xlim(-2*max(sens$std,na.rm = TRUE),2*max(sens$std,na.rm = TRUE))

  #Plot the list of plots created before
  grid.arrange(grobs = plotlist,
               nrow = 3,
               ncols = 1)
  
  #restore warnings
  options(warn=0)
  
  return(sens)
}



##########################################################################
#### Author: Jaime Pizarroso
#Function for plotting multiple plots between the output of a data frame
#and the predictors of this output.
#   -If the output is continuous:
#       -It plots a histogram of the output.
#       -If a variable is a character (with factor behaviour) or a factor, it makes
#       histograms of the output divided by the different levels of the factor.
#       -If a variable is numeric:
#             -If it is continuous, it makes a geom_point plot of the variable (x-axis)
#              and the output (y-axis). The geom_point plot is approximated by a spline,
#              with a default degree of 3.
#             -If it is discrete, if it has different values less than a customizable level 
#              (default value is 7), the variable is treated as a factor. If not, it is treated
#              as a continuous variable.
#           
#   -If the output is discrete:
#       -It plots a bar plot of the output.
#       -If a variable is numeric, it makes a histogram of the variable divided by
#       the different levels of the output
#
#Arguments:
#   -fdata:                 data.frame, must include the output data
#   -output name:           character, name of the output component
#   -color (optional):      factor with the same length as the fdata components. It
#                           contains the factors which determine the color of the 
#                           points in the geom_point plots. By default, color is NULL
#   -spline_deg (optional): integer, degree of the approximation curve of the geom_point plot.
#                           By default, spline_deg is 3
#   -factor_lev (optional): integer, number of different values that a discrete numeric variable
#                           must have to be treated as continuous. By default, factor_level
#                           is 7.
#
#Required libraries:
#   -ggplot2
#   -gridExtra
#
#################### USAGE EXAMPLE
# #Source plotModelDiagnosis function, a custom function useful for diagnosis the adjustment of a model.
# source("plotModelDiagnosis.R")
# ## LoadData 
# fdata <- read.table("Insurance.csv",
#                     sep = ";",
#                     header = TRUE,
#                     stringsAsFactors = FALSE)
#Calculate as "factor" the output variable to color the ggpairs
#color <-as.factor(as.integer(fdata$charges/4300))
# #Age and charges displays several straight lines, as charges and sex
# plot_dataframe(fdata, "charges", color, 5 , 7)

plot_dataframe <- function(fdata = NULL, output_name = NULL, color = NULL,
                           spline_deg = 3, factor_lev = 7){
  
  #Check for the ggplot library
  existe <- require(ggplot2)
  if (existe == FALSE){
    install.packages("ggplot2",dep=TRUE)
    library(ggplot2)
  }
  #Check for the gridExtra library
  existe <- require(gridExtra)
  if (existe == FALSE){
    install.packages("gridExtra",dep=TRUE)
    library(gridExtra)
  }
  
  #Check that the data provided is correct
  if(is.null(fdata)||!is.data.frame(fdata)){
    print("Please, provide a valid data frame")
    
  } else if(is.null(output_name) || !is.character(output_name)){
    print("Please, especify the name of the output of the data")
    
  } else {
    plotlist <- list() #Empty list where the plots would be stored
    nplots <- length(variable.names(fdata)) #number of plots
    out_num <- which(variable.names(fdata) == output_name) #number of column of the output in the data frame
    for(i in 1:nplots){
      print(i)
      #Check if the variable must be treated as a factor
      var_factor <- is.factor(fdata[,i])||is.character(fdata[,i])||(nlevels(as.factor(fdata[,i]))<factor_lev)
      if(i == out_num){
        #If the variable is the output and is discrete, make a bar plot
        if(is.factor(fdata[,out_num])){
          local({
            i <- i
            ploti <- ggplot(fdata, aes(x=fdata[ ,out_num],
                                       y =..count..,
                                       fill=factor(fdata[,out_num]))) +
              geom_bar() + labs(title = paste("Levels of ", colnames(fdata)[out_num]),
                                x = colnames(fdata)[out_num], 
                                fill = colnames(fdata[out_num]))
            plotlist[[i]] <<- ploti
          })
        }
        
        else{#If the variable is the output and is continuous, make a histogram
          local({
            i <- i
            ploti <- ggplot(fdata, aes(x = fdata[,out_num],
                                       fill = fdata[,out_num])) +
              geom_histogram(alpha = 0.65, aes(y=..count..)) +
              geom_density(alpha = 0.8, aes(y=..count..)) +
              labs(title = paste("Histogram of ",colnames(fdata)[out_num]),
                   x = colnames(fdata)[out_num])
            plotlist[[i]] <<- ploti
          })
        }
      }
      
      else if(is.factor(fdata[,out_num])){
        #If the output is a factor (classification problem) print a histogram for each predictor
        #based on the output
        local({
          i <- i
          ploti <- ggplot(fdata, aes_string(colnames(fdata)[i],
                                            color = colnames(fdata)[out_num],
                                            fill = colnames(fdata)[out_num])) +
            geom_histogram(aes(y = ..count..), alpha = 0.2) +
            geom_density(alpha = 0.8, aes(y = ..count..)) +
            facet_grid(fdata[,out_num] ~ .) +
            labs(title = paste(colnames(fdata)[i]," vs ",colnames(fdata)[out_num]),
                 x = colnames(fdata)[i])
          plotlist[[i]] <<- ploti
        })
      }
      
      else if(var_factor){
        #If the variable is a factor, make a histogram divided in the factors
        #If it is a char, make it a factor
        if(is.character(fdata[,i])||(nlevels(as.factor(fdata[,i])))){
          fdata[,i] <- as.factor(fdata[,i])
        }
        local({
          i <- i
          ploti <- ggplot(fdata, aes_string(x = colnames(fdata)[out_num],
                                            color = colnames(fdata)[i],
                                            fill = colnames(fdata)[i])) +
            geom_histogram(aes(y = ..count..), alpha = 0.2) +
            geom_density(alpha = 0.8, aes(y=..count..)) +
            facet_grid(fdata[,i] ~ .) +
            labs(title = paste(colnames(fdata)[out_num]," vs ",colnames(fdata)[i]),
                 x = colnames(fdata)[out_num])
          plotlist[[i]] <<- ploti
        })
      }
      
      else{
        #If the variable is not a factor, make a point plot
        if(!is.null(color)){ #Check if there are color specified
          fdata$color <- color
          local({
            i <- i
            ploti <- ggplot(fdata, aes(x = fdata[ ,i],
                                       y = fdata[ ,out_num],
                                       color = color)) +
              geom_point() +
              geom_smooth(method = "lm", formula = y ~ splines::bs(x, spline_deg),
                          se = FALSE, color = 'blue') + 
              #geom_smooth(method = "loess")+
              labs(title = paste(colnames(fdata)[out_num]," vs ",colnames(fdata)[i]),
                   x = colnames(fdata)[i], y = colnames(fdata)[out_num])
            plotlist[[i]] <<- ploti
          })
        }
        
        else{
          local({ #If there is no color specified, print it with no colour
            i <- i
            ploti <- ggplot(fdata, aes_string(x = colnames(fdata)[i], y = colnames(fdata)[out_num])) +
              geom_point() +
              geom_smooth(method = "lm", formula = y ~ splines::bs(x, spline_deg),
                          se = FALSE, color = 'blue') + 
              #geom_smooth(method = "loess")+
              labs(title = paste(colnames(fdata)[out_num]," vs ",colnames(fdata)[i]),
                   x = colnames(fdata)[i], y = colnames(fdata)[out_num])
            plotlist[[i]] <<- ploti
          })
        }
      }
    }
    #Plot the list of plots created before
    grid.arrange(grobs = plotlist,
                 nrow = floor(sqrt(length(plotlist))),
                 ncols = ceiling(sqrt(length(plotlist))))
  }
}
