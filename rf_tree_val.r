#functions for training rf-tree using data.tree package

#setup on R version 3.4.4

library(data.tree)
library(randomForest)
library(caret)
library(rsvg)
library(DiagrammeR)

#funtions to compute purity
IsPure <- function(data) {
  length(unique(data[,ncol(data)])) == 1
}

IsAlmostPure <- function(data) {
  length(unique(data[,ncol(data)])) <= 2
}

rf_node<-function(data, val=0.50, num.vars=2){

  #tuning parameter
  #val=1.00

  #get labs
  labs = data[,ncol(data)]
  #remove labs from data
  data = data[,-ncol(data)]

  #getting training values
  t_f<-data[,1]
  train<-which(t_f==TRUE)
  #remove TRUE/FALSE from data
  #keep this to ease creation of results later
  data2 = data
  data = data[,-1]

  #initializing some needed vectors
  errors_train<-c()
  errors_valid<-c()

  size_1<-c()
  size_0<-c()

  size_1_counts<-c()
  size_0_counts<-c()
  diff_counts<-c()

  #for picking variables
  dist1_e_feat<-c()
  dist2_e_feat<-c()

  #for overall
  dist1_e<-c()
  dist2_e<-c()


  #finding all possible combinations of groups of classes
  numb = length(table(labs))
  outcomes = c(0,1)
  l = rep(list(outcomes), numb)
  combos = expand.grid( l )

  #remove last and first which are all one class
  combos = combos[-1,]
  last = dim(combos)[1]
  last2 = last-1
  combos = combos[1:last2, ]

  if(numb==2){

    m=1

  }else{

    m = dim(combos)[1]/2

  }

  best<-c()
  relab<-c(1:10)
  errors_train= 0
  errors_valid= 0

  errors_train2= 0
  errors_valid2= 0
  error_vars_best = matrix(nrow=m, ncol=num.vars, data=0)

  while( (( (length(best)>0) )==FALSE ) & (sum(table(relab))>2 ) ){

    for(i in 1:m){
        print(i)
        #get first possible combination
        relab = as.character(combos[i,])
        #create loopkup table
        lookup <- data.frame(cbind(
                  names(table(labs)),
                  relab
                  ))
        #recode the labels to new combination
        labs_new = lookup$relab[match(labs, lookup$V1)]

        labs_rf<-as.numeric(labs_new)

        test<-as.data.frame(cbind(labs_rf[train], data[train,]) )
        colnames(test)[1]<-"labs_rf"

        valid<-as.data.frame(cbind(labs_rf[-train], data[-train,]) )
        colnames(valid)[1]<-"labs_rf"

        #getting counts
        lasso_labs = test[,1]
        change = which(lasso_labs==2)
        lasso_labs[change] = 0
        lasso_labs = test[,1]
        #getting class sizes
        size_1_counts[i]<-length(table(labs[train[which(lasso_labs==1)] ] ))
        size_0_counts[i]<-length(table(labs[train[which(lasso_labs==2)] ] ))
        diff_counts[i]<- abs(size_1_counts[i] - size_0_counts[i])

        size_1[i]<-mean(table(labs[train[which(lasso_labs==1)] ] ))
        size_0[i]<-mean(table(labs[train[which(lasso_labs==2)] ] ))

        #change NA's to 0's if a single class
        if(is.na(size_1[i]) ){

          size_1[i]=0

        }

        if(is.na(size_0[i]) ){

          size_0[i]=0

        }

        #doing all possible combinations of variables
        v <- dim(test)[2]-1
        var_combos<-t(combn(v, num.vars))+1
        ones<-which(test[,1]==1)
        twos<-which(test[,1]==2)

        for(j in 1:dim(var_combos)[1]){
          keep<-c(1,var_combos[j,])

          #keeping variables here
          test2<-test[,keep]
          valid2<-valid[,keep]

          tc <- trainControl(method='oob',
                            search='grid')

          rfGrid <-  expand.grid(#n.trees = (1:30)*50,
                        mtry = c(1:5))

          tune.out<-train(as.factor(labs_rf) ~.,
                    data=test2,
                    method='rf',
                    trControl = tc,
                    tuneGrid = rfGrid)

          rf.fit<-tune.out$finalModel

          ypred=predict(rf.fit ,test2)
          #table(predict=ypred, truth=test$labs_rf)
          errors_train2[j] = mean(ypred== test2$labs_rf )

          ypred=predict(rf.fit ,valid2)
          #table(predict=ypred, truth=test$labs_rf)
          errors_valid2[j] = mean(ypred== valid2$labs_rf )

          temp_mat<-test2
          temp_mat[,1]<- (temp_mat[,1]-mean(temp_mat[,1]))/sd(temp_mat[,1])
          temp_mat[,2]<- (temp_mat[,2]-mean(temp_mat[,2]))/sd(temp_mat[,2])

          dist1_e_feat[j]<-mean(dist(temp_mat[ones,-1 ], method="euclidean"))
          dist2_e_feat[j]<-mean(dist(test2[twos,-1 ], method="euclidean"))


        }

        c_b<-which(errors_train2==max(errors_train2))
        abs_diff_dist<-abs(dist1_e_feat-dist2_e_feat)
        c_b_choice<-which(abs_diff_dist[c_b]==min(abs_diff_dist[c_b]))

        errors_train[i]<-errors_train2[c_b[c_b_choice[1]]]
        errors_valid[i]<-errors_valid2[c_b[c_b_choice[1]]]
        error_vars_best[i,]<-var_combos[c_b[c_b_choice[1]],]

        ones<-which(test[,1]==1)
        twos<-which(test[,1]==2)
        #euclidean
        temp_mat<-test[,error_vars_best[i,]]
        temp_mat[,1]<- (temp_mat[,1]-mean(temp_mat[,1]))/sd(temp_mat[,1])
        temp_mat[,2]<- (temp_mat[,2]-mean(temp_mat[,2]))/sd(temp_mat[,2])

        dist1_e[i]<-mean(dist(temp_mat[ones,], method="euclidean" ))
        dist2_e[i]<-mean(dist(temp_mat[twos,], method="euclidean" ))


        }

    errors<-cbind(errors_train, errors_valid)
    colnames(errors)<-c("Error_train", "Error_valid")

    #finding which errors for training and valid have perect results
    best<- which( (errors_train==1) )


  }

  if(length(best)>0){

    errors_train_nodes = 0
    errors_valid_nodes = 0

      #getting the best of the best
      diff<-abs(size_1[best]-size_0[best])
      best_of_best<-which(diff==max(diff))

      #
      d_e<-((dist1_e+dist2_e+diff_counts*val)/2)[best]
      d_e_best<-which(d_e==min( d_e ))
      if(length(d_e_best==1)){

        use = best[ d_e_best [1] ]
        keep=c(1,error_vars_best[use,])
        variables<- colnames(data[,error_vars_best[use,]-1])
        relab = as.character(combos[use,])


      }else{
        d_m3<-((dist1_e+dist2_e+diff_counts*val)/2)[best[ d_e_best] ]
        d_m3_best<-which(d_m3==min(d_m3) )

        use = best[ d_e_best[ d_m3_best ] ]
        keep=c(1,error_vars_best[use,])
        variables<- colnames(data[,error_vars_best[use,]-1])
        relab = as.character(combos[use,])

      }
      #create loopkup table
      lookup <- data.frame(cbind(
                names(table(labs)),
                relab
                ))

      #recode the labels to new combination
      labs_new = lookup$relab[match(labs, lookup$V1)]

      labs_rf<-as.numeric(labs_new)

      test<-as.data.frame(cbind(labs_rf[train], data[train,]) )
      colnames(test)[1]<-"labs_rf"

      valid<-as.data.frame(cbind(labs_rf[-train], data[-train,]) )
      colnames(valid)[1]<-"labs_rf"

      #keeping variables here
      test2<-test[,keep]
      valid2<-valid[,keep]

      tune.out<-train(as.factor(labs_rf) ~.,
                data=test2,
                method='rf',
                trControl = tc,
                tuneGrid = rfGrid)

      rf.fit<-tune.out$finalModel
      ypred=predict(rf.fit ,test2)
      tab1 = table(predict=ypred, truth=test2$labs_rf)
      errors_train_nodes = mean(ypred== test2$labs_rf )

      #validation

      ypred=predict(rf.fit ,valid2)
      tab2 = table(predict=ypred, truth=valid2$labs_rf)
      errors_valid_nodes = mean(ypred== valid2$labs_rf )
      print(errors_train_nodes)

    results =list()

    #error on training data
    results[[1]] = errors_train_nodes
    #error on validation
    results[[2]] = errors_valid_nodes
    #best model
    results[[3]] = best[1]
    #new labels
    results[[4]] = relab
    #original labels
    results[[5]] = names(table(labs))
    ####################
    #left node labels
    ####################
    results[[6]] = labs[which(labs_new==1)]
    #left node  data
    new_data = cbind(data2[which(labs_new==1),], results[[6]])
    colnames(new_data)[dim(new_data)[2]] <- "labs"
    results[[7]] = new_data
    #all nodes training observation
    results[[8]] = train
    ####################
    #right node labels
    ####################
    results[[9]] =labs[which(labs_new==0)]
    #right node  data
    new_data = cbind(data2[which(labs_new==0),], results[[9]])
    colnames(new_data)[dim(new_data)[2]] <- "labs"
    results[[10]] =new_data
    #all nodes training observations
    results[[11]] = train
    #returning the vairables results from lasso
    results[[12]] = (variables)
    ####################
    ## labs_rf Model Params
    ####################
    #mtry
    results[[13]] = rf.fit$mtry
    #ntree
    results[[14]] = rf.fit$ntree
    #importance
    results[[15]] = paste(rf.fit$importance, ",")
    #########################
    ## Error Rates per Class
    #########################
    #class 1 accuracy test
    results[[16]] = tab1[1,1]/colSums(tab1)[1]
    #class 1 accuracy valid
    results[[17]] = tab2[1,1]/colSums(tab2)[1]
    #class 2 accuracy test
    results[[18]] = tab1[2,2]/colSums(tab1)[2]
    #class 2 accuracy valid
    results[[19]] = tab2[2,2]/colSums(tab2)[2]


    return(results)
    #print("Class 1:")
    #print(as.character( names(table( labs))[which(relab=='1')] ))
    #print("Class 2:")
    #print(as.character( names(table( labs))[which(relab=='0')] ))


  }else{

    best<- which( (errors_train==max(errors_train)) )

    #getting the best of the best
    diff<-abs(size_1[best]-size_0[best])
    best_of_best<-which(diff==max(diff))
    #
    d_e<-((dist1_e+dist2_e+diff_counts*val)/2)[best]
    d_e_best<-which(d_e==min(d_e ))
    if(length(d_e_best==1)){

      use = best[ d_e_best [1] ]
      keep=c(1,error_vars_best[use,])
      variables<- colnames(data[,error_vars_best[use,]-1])
      relab = as.character(combos[use,])


    }else{
      d_m3<-((dist1_e+dist2_e)/2)[best[ d_e_best] ]
      d_m3_best<-which(d_m3==min(d_m3) )

      use = best[ d_e_best[ d_m3_best ] ]
      keep=c(1,error_vars_best[use,])
      variables<- colnames(data[,error_vars_best[use,]-1])
      relab = as.character(combos[use,])

    }
    #create loopkup table
    lookup <- data.frame(cbind(
              names(table(labs)),
              relab
              ))

    #recode the labels to new combination
    labs_new = lookup$relab[match(labs, lookup$V1)]

    labs_rf<-as.numeric(labs_new)

    test<-as.data.frame(cbind(labs_rf[train], data[train,]) )
    colnames(test)[1]<-"labs_rf"

    valid<-as.data.frame(cbind(labs_rf[-train], data[-train,]) )
    colnames(valid)[1]<-"labs_rf"

    #keeping variables here
    test2<-test[,keep]
    valid2<-valid[,keep]

    tune.out<-train(as.factor(labs_rf) ~.,
              data=test2,
              method='rf',
              trControl = tc,
              tuneGrid = rfGrid)

    rf.fit<-tune.out$finalModel
    ypred=predict(rf.fit ,test2)
    tab1 = table(predict=ypred, truth=test2$labs_rf)
    errors_train_nodes = mean(ypred== test2$labs_rf )

    #validation

    ypred=predict(rf.fit ,valid2)
    tab2 = table(predict=ypred, truth=valid2$labs_rf)
    errors_valid_nodes = mean(ypred== valid2$labs_rf )

    results =list()

    #error on training data
    results[[1]] = errors_train_nodes
    #error on validation
    results[[2]] = errors_valid_nodes
    #best model
    results[[3]] = best[1]
    #new labels
    results[[4]] = relab
    #original labels
    results[[5]] = names(table(labs))
    ####################
    #left node labels
    ####################
    results[[6]] = labs[which(labs_new==1)]
    #left node  data
    new_data = cbind(data2[which(labs_new==1),], results[[6]])
    colnames(new_data)[dim(new_data)[2]] <- "labs"
    results[[7]] = new_data
    #all nodes training observation
    results[[8]] = train
    ####################
    #right node labels
    ####################
    results[[9]] =labs[which(labs_new==0)]
    #right node  data
    new_data = cbind(data2[which(labs_new==0),], results[[9]])
    colnames(new_data)[dim(new_data)[2]] <- "labs"
    results[[10]] =new_data
    #all nodes training observations
    results[[11]] = train
    #returning the vairables results from lasso
    results[[12]] = (variables)
    ####################
    ## RF Model Params
    ####################
    #mtry
    results[[13]] = rf.fit$mtry
    #ntree
    results[[14]] = rf.fit$ntree
    #importance
    results[[15]] = paste(rf.fit$importance, ",")
    #########################
    ## Error Rates per Class
    #########################
    #class 1 accuracy test
    results[[16]] = tab1[1,1]/colSums(tab1)[1]
    #class 1 accuracy valid
    results[[17]] = tab2[1,1]/colSums(tab2)[1]
    #class 2 accuracy test
    results[[18]] = tab1[2,2]/colSums(tab1)[2]
    #class 2 accuracy valid
    results[[19]] = tab2[2,2]/colSums(tab2)[2]

    return(results)
    #print("Class 1:")
    #print(as.character( names(table( labs))[which(relab=='1')] ))
    #print("Class 2:")
    #print(as.character( names(table( labs))[which(relab=='0')] ))

    }

}



#train the tree
TrainDMG <- function(node, data, val, num.vars) {

  node$TrainCount <- dim(data)[1]

  #if the data-set is almost pure (e.g. all capsule or ovals), then
  if (IsAlmostPure(data)) {
    #construct a leaf having the name of the pure feature (e.g. 'capsule')
    #create rf node
    node_new = rf_node(data, val, num.vars)
    print(paste(as.character(unique(node_new[[6]])),collapse = ','))
    print(paste(as.character(unique(node_new[[9]])),collapse = ','))
    #add children
    child1 <- node$AddChild(paste(as.character(unique(node_new[[6]])),collapse = ','))
    child0 <- node$AddChild(paste(as.character(unique(node_new[[9]])),collapse = ','))
    #add classes
    node$class1 <- paste(as.character(unique(node_new[[6]])),collapse = ',')
    node$class0 <- paste(as.character(unique(node_new[[9]])),collapse = ',')
    #add overal error rates
    #add child error rates
    child1$trainError <- node_new[[18]]
    child0$trainError <- node_new[[16]]

    child1$validError <- node_new[[19]]
    child0$validError <- node_new[[17]]
    #add variables
    child1$variables  <- paste(as.character(unique(node_new[[12]])),collapse = ',')
    child0$variables  <- paste(as.character(unique(node_new[[12]])),collapse = ',')
    #add parameters
    child1$mtry = node_new[[13]]
    child1$ntree = node_new[[14]]
    child1$import = node_new[[15]]
    child0$mtry = node_new[[13]]
    child0$ntree = node_new[[14]]
    child0$import = node_new[[15]]

  } else {

    #create rf node
    node_new = rf_node(data, val, num.vars)
    print(paste(as.character(unique(node_new[[6]])),collapse = ','))
    print(paste(as.character(unique(node_new[[9]])),collapse = ','))

    #construct a child having the name of the remaining classes
    child1 <- node$AddChild( paste(as.character(unique(node_new[[6]])),collapse = ',') )
    child1$trainError <- node_new[[18]]
    child1$validError <- node_new[[19]]
    child1$variables  <- paste(as.character(unique(node_new[[12]])),collapse = ',')
    child0 <- node$AddChild( paste(as.character(unique(node_new[[9]])),collapse = ',') )
    child0$variables  <- paste(as.character(unique(node_new[[12]])),collapse = ',')
    child0$trainError <- node_new[[16]]
    child0$validError <- node_new[[17]]
    #add parameters
    child1$mtry = node_new[[13]]
    child1$ntree = node_new[[14]]
    child1$import = node_new[[15]]
    child0$mtry = node_new[[13]]
    child0$ntree = node_new[[14]]
    child0$import = node_new[[15]]

    #call the algorithm recursively on the child and the new training data
    #first need to check if the new node is pure
    if(IsPure(node_new[[7]])){
      #construct a leaf having the name of the pure feature (e.g. 'capsule')
      child1$TrainCount <- dim(node_new[[7]])[1]
      child1$class1 <- unique(node_new[[6]])
      child1$class0 <- ''
      child1$trainError <- node_new[[18]]
      child1$validError <- node_new[[19]]
      child1$variables  <- paste(as.character(unique(node_new[[12]])),collapse = ',')
      #add parameters
      child1$mtry = node_new[[13]]
      child1$ntree = node_new[[14]]
      child1$import = node_new[[15]]

    }else{

      TrainDMG(child1, node_new[[7]], val, num.vars )

    }
    #first need to check if node is pure
    if(IsPure(node_new[[10]])){
      #construct a leaf having the name of the pure feature (e.g. 'capsule')
      child0$TrainCount <- dim(node_new[[10]])[1]
      child0$class1 <- ''
      child0$class0 <- unique(node_new[[9]])
      child0$trainError <- node_new[[16]]
      child0$validError <- node_new[[17]]
      child0$variables  <- paste(as.character(unique(node_new[[12]])),collapse = ',')
      #add parameters
      child0$mtry = node_new[[13]]
      child0$ntree = node_new[[14]]
      child0$import = node_new[[15]]

    }else{

      TrainDMG(child0, node_new[[10]], val, num.vars)

    }

  }



}


#
