#example from data.tree website
#https://cran.r-project.org/web/packages/data.tree/vignettes/applications.html

rm(list=ls())


  val = 0.41

  print('Value of')
  print(val)

  set.seed(1382)

  source('svm_tree_val.r')

  ##################
  #loading data
  ##################

  #cleaning data for analysis

  #round or circle
  circ1 <-read.table("data_txt/round1.txt", sep=",", header=TRUE)
  circ2 <-read.table('data_txt/round2.txt', sep=",", header=TRUE)
  circ3 <-read.table("data_txt/round3.txt", sep=",", header=TRUE)
  circ4 <-read.table("data_txt/round4.txt", sep=",", header=TRUE)
  circ5 <-read.table("data_txt/round5.txt", sep=",", header=TRUE)

  circ<-rbind(circ1, circ2, circ3, circ4, circ5)

  circ1_shapes <-read.table("data_txt/round1_SHAPES.txt", sep=",", header=TRUE)
  circ2_shapes <-read.table('data_txt/round2_SHAPES.txt', sep=",", header=TRUE)
  circ3_shapes <-read.table("data_txt/round3_SHAPES.txt", sep=",", header=TRUE)
  circ4_shapes <-read.table("data_txt/round4_SHAPES.txt", sep=",", header=TRUE)
  circ5_shapes <-read.table("data_txt/round5_SHAPES.txt", sep=",", header=TRUE)

  circ_shapes<-rbind(circ1_shapes, circ2_shapes, circ3_shapes,
                     circ4_shapes, circ5_shapes)

  #capsule
  caps <-read.table("data_txt/caps.txt", sep=",", header=TRUE)

  caps_shapes <-read.table("data_txt/caps_SHAPES.txt", sep=",", header=TRUE)

  #diamond
  diam <-read.table("data_txt/diam.txt", sep=",", header=TRUE)

  diam_shapes <-read.table("data_txt/diam_SHAPES.txt", sep=",", header=TRUE)

  #hexagon
  hex1 <-read.table("data_txt/hex.txt", sep=",", header=TRUE)
  hex2 <-read.table("data_txt/reg_hex.txt", sep=",", header=TRUE)

  hex<-rbind(hex1, hex2)

  hex_shapes1 <-read.table("data_txt/hex_SHAPES.txt", sep=",", header=TRUE)
  hex_shapes2 <-read.table("data_txt/reg_hex_SHAPES.txt", sep=",", header=TRUE)

  hex_shapes<-rbind(hex_shapes1, hex_shapes2)


  #oval
  oval1 <-read.table("data_txt/oval1.txt", sep=",", header=TRUE)
  oval2 <-read.table("data_txt/oval2.txt", sep=",", header=TRUE)
  oval3 <-read.table("data_txt/oval3.txt", sep=",", header=TRUE)

  oval<-rbind(oval1, oval2, oval3)

  oval1_SHAPES <-read.table("data_txt/oval1_SHAPES.txt", sep=",", header=TRUE)
  oval2_SHAPES <-read.table("data_txt/oval2_SHAPES.txt", sep=",", header=TRUE)
  oval3_SHAPES <-read.table("data_txt/oval3_SHAPES.txt", sep=",", header=TRUE)

  oval_shapes<-rbind(oval1_SHAPES, oval2_SHAPES, oval3_SHAPES)

  #pentagon
  pent1 <-read.table("data_txt/pent.txt", sep=",", header=TRUE)
  pent2 <-read.table("data_txt/reg_pent.txt", sep=",", header=TRUE)

  pent<-rbind(pent1, pent2)

  pent_shapes1 <-read.table("data_txt/pent_SHAPES.txt", sep=",", header=TRUE)
  pent_shapes2 <-read.table("data_txt/reg_pent_SHAPES.txt", sep=",", header=TRUE)

  pent_shapes<-rbind(pent_shapes1, pent_shapes2)

  #rectangle
  rect <-read.table("data_txt/rect.txt", sep=",", header=TRUE)

  rect_shapes <-read.table("data_txt/rect_SHAPES.txt", sep=",", header=TRUE)

  #semi-circle
  sc <-read.table("data_txt/sc.txt", sep=",", header=TRUE)

  sc_shapes <-read.table("data_txt/sc_SHAPES.txt", sep=",", header=TRUE)

  #square
  squ <-read.table("data_txt/squ.txt", sep=",", header=TRUE)

  squ_shapes <-read.table("data_txt/squ_SHAPES.txt", sep=",", header=TRUE)

  #tear
  tear <-read.table("data_txt/tear.txt", sep=",", header=TRUE)

  tear_shapes <-read.table("data_txt/tear_SHAPES.txt", sep=",", header=TRUE)

  #trapezoid
  trap <-read.table("data_txt/trap.txt", sep=",", header=TRUE)

  trap_shapes <-read.table("data_txt/trap_SHAPES.txt", sep=",", header=TRUE)

  #triangle
  tri <-read.table("data_txt/tri.txt", sep=",", header=TRUE)

  tri_shapes <-read.table("data_txt/tri_SHAPES.txt", sep=",", header=TRUE)


  #cleaning data for ggplot2 and analysis
  labs<-as.factor(c(rep(1, dim(caps)[1]),
                    rep(2, dim(diam)[1]),
                    rep(3, dim(hex)[1]),
                    rep(4, dim(oval)[1]),
                    rep(5, dim(pent)[1]), rep(6, dim(rect)[1]),
                    rep(7, dim(circ)[1]),
                    rep(8, dim(sc)[1]),
                    rep(9, dim(squ)[1]), rep(10, dim(tear)[1]),
                    rep(11, dim(trap)[1]),rep(12, dim(tri)[1])    ) )

  labs2<-as.factor(c(rep("Capsule", dim(caps)[1]),
                    rep("Diamond", dim(diam)[1]),
                    rep("Hexagon", dim(hex)[1]),
                    rep("Oval", dim(oval)[1]),
                    rep("Pentagon", dim(pent)[1]),
                    rep("Rectangle", dim(rect)[1]), rep("Round", dim(circ)[1]),
                    rep("Semi-Circle", dim(sc)[1]), rep("Square", dim(squ)[1]),
                    rep("Tear", dim(tear)[1]), rep("Trapezoid", dim(trap)[1]),
                    rep("Triangle", dim(tri)[1])    ) )

  #complete data set

  temp<-rbind(caps, diam, hex, oval,
              pent, rect, circ, sc,
              squ, tear, trap, tri
              )

  temp_shapes<-rbind(caps_shapes, diam_shapes, hex_shapes, oval_shapes,
              pent_shapes, rect_shapes, circ_shapes, sc_shapes,
              squ_shapes, tear_shapes, trap_shapes, tri_shapes
              )

  sp<-temp$white/(temp$white+temp$black)

  #fix mistake
  svns<-which(labs==7)

  #boxplot(sp[svns])
  #table(round(sp[svns], 2))
  #pi/4

  look_round<-which(sp[svns]<0.70)

  labs[svns[look_round]]=4
  labs2[svns[look_round]]="Oval"

  print('##################')
  print('# first run')
  print('##################')

  mydata<-cbind(labs, sp, temp, temp_shapes)

  cols_keep<-c(1:6, 10, 11)

  mydata<-mydata[,cols_keep]
  colnames(mydata)<-c("labs_svm", "SP",
                      "White", "Black", "Circularity",
                      "Eccentricity", "White_Box", "Black_Box"
                      )


  #sample from round, oval, caps, and rectangle
  keep_rect<-which( ( labs== 6) )
  keep_circ<-which( ( labs== 7) )
  keep_cap<-which( ( labs== 1) )
  keep_oval<-which( ( labs== 4) )

  ref_tab_counts<-table(labs)

  cap_rand<-sample(keep_cap, ref_tab_counts[1])
  train_01<-cap_rand[1:(ref_tab_counts[1]/2)]
  oval_rand<-sample(keep_oval, ref_tab_counts[4])
  train_4<-oval_rand[1:(ref_tab_counts[4]/2)]

  rect_rand<-sample(keep_rect, 6)
  train_6<-rect_rand[1:3]
  circ_rand<-sample(keep_circ, ref_tab_counts[7])
  train_7<-circ_rand[1:(ref_tab_counts[7]/2)]

  #sample from other
  keep_diam<-which( ( labs== 2) )
  keep_hex<-which( ( labs== 3) )
  keep_pent<-which( ( labs== 5) )
  keep_sc<-which( ( labs== 8) )
  keep_squ<-which( ( labs== 9) )
  keep_tear<-which( ( labs== 10) )
  keep_trap<-which( ( labs== 11) )
  keep_tri<-which( ( labs== 12) )

  diam_rand<-sample(keep_diam, 12)
  train_2<-diam_rand[1:6]
  hex_rand_reg<-sample(keep_hex[1:6], 6)
  hex_rand_other<-sample(keep_hex[7:8], 2)
  hex_rand<-c(hex_rand_reg[1:3], hex_rand_other[1],
              hex_rand_reg[4:6], hex_rand_other[2])
  train_3<-hex_rand[1:4]

  pent_rand_reg<-sample(keep_pent[3:12], 10)
  pent_rand_other<-sample(keep_pent[1:2], 2)
  pent_rand<-c(pent_rand_reg[1:5], pent_rand_other[1],
              pent_rand_reg[6:10], pent_rand_other[2])
  train_5<-pent_rand[1:6]
  sc_rand<-sample(keep_sc, 4)
  train_8<-sc_rand[1:2]

  squ_rand<-sample(keep_squ, 8)
  train_9<-squ_rand[1:4]
  tear_rand<-sample(keep_tear, 10)
  train_10<-tear_rand[1:5]

  trap_rand<-sample(keep_trap, 4)
  train_11<-trap_rand[1:2]
  tri_rand<-sample(keep_tri, 12)
  train_12<-tri_rand[1:6]


  train_vals<-c(train_01, train_2, train_3, train_4,
                train_5, train_6, train_7, train_8,
                train_9, train_10, train_11, train_12)

  train_vec<-rep(FALSE, length(labs))
  train_vec[train_vals]<-TRUE

  #cleanign and setup of data
  data<- mydata
  train<-train_vec

  labs<-data[,1]
  data<-data[,-1]
  #classes to categorize must be the last column
  data<-cbind(data, labs)
  #putting training values as first column
  data<-cbind(train, data)

  ############################
  #doing initial meta-class
  ############################

  data<-data[,-9]

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

  dist1_mink_3<-c()
  dist2_mink_3<-c()

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
  error_vars_best = matrix(nrow=m, ncol=2, data=0)

  i=1
  m=1

  #get first possible combination
  relab = as.character(combos[i,])
  relab[4] = "1"
  relab[6] = "1"
  relab[7] = "1"
  #create loopkup table
  lookup <- data.frame(cbind(
            names(table(labs)),
            relab
            ))
  #recode the labels to new combination
  labs_new = lookup$relab[match(labs, lookup$V1)]

  labs_svm<-as.numeric(labs_new)

  test<-as.data.frame(cbind(labs_svm[train], data[train,]) )
  colnames(test)[1]<-"labs_svm"

  valid<-as.data.frame(cbind(labs_svm[-train], data[-train,]) )
  colnames(valid)[1]<-"labs_svm"

  #getting counts
  lasso_labs = test[,1]
  change = which(lasso_labs==2)
  lasso_labs[change] = 0
  lasso_labs = test[,1]
  #getting class sizes
  size_1[i]<-var(table(labs[train[which(lasso_labs==1)] ] ))
  size_0[i]<-var(table(labs[train[which(lasso_labs==0)] ] ))

  #change NA's to 0's if a single class
  if(is.na(size_1[i]) ){

    size_1[i]=0

  }

  if(is.na(size_0[i]) ){

    size_0[i]=0

  }

  #doing all possible combinations of variables
  v <- dim(test)[2]-1
  var_combos<-t(combn(v, 2))+1
  ones<-which(test[,1]==1)
  twos<-which(test[,1]==2)

  for(j in 1:dim(var_combos)[1]){
    keep<-c(1,var_combos[j,])

    #keeping variables here
    test2<-test[,keep]
    valid2<-valid[,keep]

    tune.out<-tune(svm, as.factor(labs_svm) ~.,
              data=test2,
              kernel='polynomial',
              ranges=list(cost=c(1), coef0=c(1:3, 50), degree=c(1:3, 5)),
              validation.x=valid2[,-1],
              validation.y=as.factor(valid2[,1]),
              tunecontrol = tune.control(sampling = "fix"))

    ypred=predict(tune.out$best.model ,test2)
    #table(predict=ypred, truth=test$labs_svm)
    errors_train2[j] = mean(ypred== test2$labs_svm )

    ypred=predict(tune.out$best.model ,valid2)
    #table(predict=ypred, truth=test$labs_svm)
    errors_valid2[j] = mean(ypred== valid2$labs_svm )

    temp_mat<-test2
    temp_mat[,1]<- (temp_mat[,1]-mean(temp_mat[,1]))/sd(temp_mat[,1])
    temp_mat[,2]<- (temp_mat[,2]-mean(temp_mat[,2]))/sd(temp_mat[,2])

    dist1_e_feat[j]<-mean(dist(temp_mat[ones,-1 ], method="euclidean"))
    dist2_e_feat[j]<-mean(dist(test2[twos,-1 ], method="euclidean"))

  }

  c_b<-which(errors_train2==max(errors_train2))
  abs_diff_dist<-abs(dist1_e_feat-dist2_e_feat)
  c_b_choice<-which(abs_diff_dist[c_b]==min(abs_diff_dist[c_b]))

  errors_train[i]<-errors_train2[c_b[c_b_choice]]
  errors_valid[i]<-errors_valid2[c_b[c_b_choice]]
  error_vars_best[i,]<-var_combos[c_b[c_b_choice],]

  ones<-which(test[,1]==1)
  twos<-which(test[,1]==2)
  #euclidean
  dist1_e[i]<-mean(dist(test[ones,error_vars_best[i,]], method="euclidean" ))
  dist2_e[i]<-mean(dist(test[twos,error_vars_best[i,]], method="euclidean" ))

  dist1_mink_3[i]<-mean(dist(test[ones,error_vars_best[i,]],
                          method="minkowski", p=3))
  dist2_mink_3[i]<-mean(dist(test[twos,error_vars_best[i,]],
                          method="minkowski", p=3 ))

  errors<-cbind(errors_train, errors_valid)
  colnames(errors)<-c("Error_train", "Error_valid")

  #finding which errors for training and valid have perfect results
  best<- which( (errors_train==1) )
  print(best)
  print(errors)

  variables<- colnames(data[,error_vars_best[best,]-1])
  variables


  #create first meta group of
  #round, capsule, oval and rectangle
  #vs.
  #else

  #cleanign and setup of data

  mydata<-cbind(labs, sp, temp, temp_shapes)

  cols_keep<-c(1:6, 10, 11)

  mydata<-mydata[,cols_keep]
  colnames(mydata)<-c("labs_svm", "SP",
                      "White", "Black", "Circularity",
                      "Eccentricity", "White_Box", "Black_Box"
                      )


  train_vals<-c(train_01, train_2, train_3, train_4,
                train_5, train_6, train_7, train_8,
                train_9, train_10, train_11, train_12)

  train_vec<-rep(FALSE, length(labs))
  train_vec[train_vals]<-TRUE

  #cleanign and setup of data
  data<- mydata
  train<-train_vec


  labs<-as.numeric(data[,1])
  data<-data[,-1]
  #classes to categorize must be the last column
  data<-cbind(data, labs)
  #putting training values as first column
  data<-cbind(train, data)


  #########

  keep1<-which(labs==1)
  keep2<-which(labs==2)
  keep3<-which(labs==3)

  keep4<-which(labs==4)
  keep5<-which(labs==5)

  keep6<-which(labs==6)
  keep7<-which(labs==7)
  keep8<-which(labs==8)

  keep10<-which(labs==10)
  keep11<-which(labs==11)
  keep12<-which(labs==12)

  keep<-c(keep1, #capsule
          keep4, #oval
          keep6, #rect
          keep7 #round
          )


  data1<-data[keep,]

  tree1 <- Node$new("Capsule, Round, Oval, and Rectangle")
  TrainDMG(tree1, data1, val )
  print(tree1, "TrainCount", "class1",
              "class0", "trainError",
              "validError", "variables",
              "Cost", "Degree",  "coef0")
  #export_graph(ToDiagrammeRGraph(tree1), "tree1_11_20_num1_0.41.pdf")

  #########

  data2<-data[-keep,]

  tree2 <- Node$new("Else")
  TrainDMG(tree2, data2, val )
  print(tree2, "TrainCount", "class1",
              "class0", "trainError",
              "validError", "variables",
              "Cost", "Degree",  "coef0")
  #export_graph(ToDiagrammeRGraph(tree2), "tree2_11_20_num1_0.41.pdf")

  print('##################')
  print('# 2nd run')
  print('##################')

  mydata<-cbind(labs, sp, temp, temp_shapes)

  cols_keep<-c(1:6, 10, 11)

  mydata<-mydata[,cols_keep]
  colnames(mydata)<-c("labs_svm", "SP",
                      "White", "Black", "Circularity",
                      "Eccentricity", "White_Box", "Black_Box"
                      )


  #sample from round, oval, caps, and rectangle
  keep_rect<-which( ( labs== 6) )
  keep_circ<-which( ( labs== 7) )
  keep_cap<-which( ( labs== 1) )
  keep_oval<-which( ( labs== 4) )

  cap_rand<-sample(keep_cap, ref_tab_counts[1])
  train_01<-cap_rand[1:(ref_tab_counts[1]/2)]
  oval_rand<-sample(keep_oval, ref_tab_counts[4])
  train_4<-oval_rand[1:(ref_tab_counts[4]/2)]

  rect_rand<-sample(keep_rect, 6)
  train_6<-rect_rand[1:3]
  circ_rand<-sample(keep_circ, ref_tab_counts[7])
  train_7<-circ_rand[1:(ref_tab_counts[7]/2)]

  #sample from other
  keep_diam<-which( ( labs== 2) )
  keep_hex<-which( ( labs== 3) )
  keep_pent<-which( ( labs== 5) )
  keep_sc<-which( ( labs== 8) )
  keep_squ<-which( ( labs== 9) )
  keep_tear<-which( ( labs== 10) )
  keep_trap<-which( ( labs== 11) )
  keep_tri<-which( ( labs== 12) )

  diam_rand<-sample(keep_diam, 12)
  train_2<-diam_rand[1:6]
  hex_rand_reg<-sample(keep_hex[1:6], 6)
  hex_rand_other<-sample(keep_hex[7:8], 2)
  hex_rand<-c(hex_rand_reg[1:3], hex_rand_other[1],
              hex_rand_reg[4:6], hex_rand_other[2])
  train_3<-hex_rand[1:4]

  pent_rand_reg<-sample(keep_pent[3:12], 10)
  pent_rand_other<-sample(keep_pent[1:2], 2)
  pent_rand<-c(pent_rand_reg[1:5], pent_rand_other[1],
              pent_rand_reg[6:10], pent_rand_other[2])
  train_5<-pent_rand[1:6]
  sc_rand<-sample(keep_sc, 4)
  train_8<-sc_rand[1:2]

  squ_rand<-sample(keep_squ, 8)
  train_9<-squ_rand[1:4]
  tear_rand<-sample(keep_tear, 10)
  train_10<-tear_rand[1:5]

  trap_rand<-sample(keep_trap, 4)
  train_11<-trap_rand[1:2]
  tri_rand<-sample(keep_tri, 12)
  train_12<-tri_rand[1:6]


  train_vals<-c(train_01, train_2, train_3, train_4,
                train_5, train_6, train_7, train_8,
                train_9, train_10, train_11, train_12)

  train_vec<-rep(FALSE, length(labs))
  train_vec[train_vals]<-TRUE

  #cleanign and setup of data
  data<- mydata
  train<-train_vec

  labs<-data[,1]
  data<-data[,-1]
  #classes to categorize must be the last column
  data<-cbind(data, labs)
  #putting training values as first column
  data<-cbind(train, data)

  ############################
  #doing initial meta-class
  ############################

  data<-data[,-9]

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

  dist1_mink_3<-c()
  dist2_mink_3<-c()

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
  error_vars_best = matrix(nrow=m, ncol=2, data=0)

  i=1
  m=1

  #get first possible combination
  relab = as.character(combos[i,])
  relab[4] = "1"
  relab[6] = "1"
  relab[7] = "1"
  #create loopkup table
  lookup <- data.frame(cbind(
            names(table(labs)),
            relab
            ))
  #recode the labels to new combination
  labs_new = lookup$relab[match(labs, lookup$V1)]

  labs_svm<-as.numeric(labs_new)

  test<-as.data.frame(cbind(labs_svm[train], data[train,]) )
  colnames(test)[1]<-"labs_svm"

  valid<-as.data.frame(cbind(labs_svm[-train], data[-train,]) )
  colnames(valid)[1]<-"labs_svm"

  #getting counts
  lasso_labs = test[,1]
  change = which(lasso_labs==2)
  lasso_labs[change] = 0
  lasso_labs = test[,1]
  #getting class sizes
  size_1[i]<-var(table(labs[train[which(lasso_labs==1)] ] ))
  size_0[i]<-var(table(labs[train[which(lasso_labs==0)] ] ))

  #change NA's to 0's if a single class
  if(is.na(size_1[i]) ){

    size_1[i]=0

  }

  if(is.na(size_0[i]) ){

    size_0[i]=0

  }

  #doing all possible combinations of variables
  v <- dim(test)[2]-1
  var_combos<-t(combn(v, 2))+1
  ones<-which(test[,1]==1)
  twos<-which(test[,1]==2)

  for(j in 1:dim(var_combos)[1]){
    keep<-c(1,var_combos[j,])

    #keeping variables here
    test2<-test[,keep]
    valid2<-valid[,keep]

    tune.out<-tune(svm, as.factor(labs_svm) ~.,
              data=test2,
              kernel='polynomial',
              ranges=list(cost=c(1), coef0=c(1:3, 50), degree=c(1:3, 5)),
              validation.x=valid2[,-1],
              validation.y=as.factor(valid2[,1]),
              tunecontrol = tune.control(sampling = "fix"))

    ypred=predict(tune.out$best.model ,test2)
    #table(predict=ypred, truth=test$labs_svm)
    errors_train2[j] = mean(ypred== test2$labs_svm )

    ypred=predict(tune.out$best.model ,valid2)
    #table(predict=ypred, truth=test$labs_svm)
    errors_valid2[j] = mean(ypred== valid2$labs_svm )

    temp_mat<-test2
    temp_mat[,1]<- (temp_mat[,1]-mean(temp_mat[,1]))/sd(temp_mat[,1])
    temp_mat[,2]<- (temp_mat[,2]-mean(temp_mat[,2]))/sd(temp_mat[,2])

    dist1_e_feat[j]<-mean(dist(temp_mat[ones,-1 ], method="euclidean"))
    dist2_e_feat[j]<-mean(dist(test2[twos,-1 ], method="euclidean"))

  }

  c_b<-which(errors_train2==max(errors_train2))
  abs_diff_dist<-abs(dist1_e_feat-dist2_e_feat)
  c_b_choice<-which(abs_diff_dist[c_b]==min(abs_diff_dist[c_b]))

  errors_train[i]<-errors_train2[c_b[c_b_choice]]
  errors_valid[i]<-errors_valid2[c_b[c_b_choice]]
  error_vars_best[i,]<-var_combos[c_b[c_b_choice],]

  ones<-which(test[,1]==1)
  twos<-which(test[,1]==2)
  #euclidean
  dist1_e[i]<-mean(dist(test[ones,error_vars_best[i,]], method="euclidean" ))
  dist2_e[i]<-mean(dist(test[twos,error_vars_best[i,]], method="euclidean" ))

  dist1_mink_3[i]<-mean(dist(test[ones,error_vars_best[i,]],
                          method="minkowski", p=3))
  dist2_mink_3[i]<-mean(dist(test[twos,error_vars_best[i,]],
                          method="minkowski", p=3 ))

  errors<-cbind(errors_train, errors_valid)
  colnames(errors)<-c("Error_train", "Error_valid")

  #finding which errors for training and valid have perfect results
  best<- which( (errors_train==1) )
  print(best)
  print(errors)

  variables<- colnames(data[,error_vars_best[best,]-1])
  variables


  #create first meta group of
  #round, capsule, oval and rectangle
  #vs.
  #else

  #cleanign and setup of data
  data<- mydata
  train<-train_vec

  labs<-data[,1]
  data<-data[,-1]
  #classes to categorize must be the last column
  data<-cbind(data, labs)
  #putting training values as first column
  data<-cbind(train, data)


  #########

  keep1<-which(labs==1)
  keep2<-which(labs==2)
  keep3<-which(labs==3)

  keep4<-which(labs==4)
  keep5<-which(labs==5)

  keep6<-which(labs==6)
  keep7<-which(labs==7)
  keep8<-which(labs==8)

  keep10<-which(labs==10)
  keep11<-which(labs==11)
  keep12<-which(labs==12)

  keep<-c(keep1, #capsule
          keep4, #oval
          keep6, #rect
          keep7 #round
          )


  data1<-data[keep,]

  tree1 <- Node$new("Capsule, Round, Oval, and Rectangle")
  TrainDMG(tree1, data1, val )
  print(tree1, "TrainCount", "class1",
              "class0", "trainError",
              "validError", "variables",
              "Cost", "Degree",  "coef0")
  #export_graph(ToDiagrammeRGraph(tree1), "tree1_11_20_num2_0.41.pdf")

  #########

  data2<-data[-keep,]

  tree2 <- Node$new("Else")
  TrainDMG(tree2, data2, val )
  print(tree2, "TrainCount", "class1",
              "class0", "trainError",
              "validError", "variables",
              "Cost", "Degree",  "coef0")
  #export_graph(ToDiagrammeRGraph(tree2), "tree2_11_20_num2_0.41.pdf")

  print('##################')
  print('# 3rd run')
  print('##################')

  mydata<-cbind(labs, sp, temp, temp_shapes)

  cols_keep<-c(1:6, 10, 11)

  mydata<-mydata[,cols_keep]
  colnames(mydata)<-c("labs_svm", "SP",
                      "White", "Black", "Circularity",
                      "Eccentricity", "White_Box", "Black_Box"
                      )


  #sample from round, oval, caps, and rectangle
  keep_rect<-which( ( labs== 6) )
  keep_circ<-which( ( labs== 7) )
  keep_cap<-which( ( labs== 1) )
  keep_oval<-which( ( labs== 4) )

  cap_rand<-sample(keep_cap, ref_tab_counts[1])
  train_01<-cap_rand[1:(ref_tab_counts[1]/2)]
  oval_rand<-sample(keep_oval, ref_tab_counts[4])
  train_4<-oval_rand[1:(ref_tab_counts[4]/2)]

  rect_rand<-sample(keep_rect, 6)
  train_6<-rect_rand[1:3]
  circ_rand<-sample(keep_circ, ref_tab_counts[7])
  train_7<-circ_rand[1:(ref_tab_counts[7]/2)]

  #sample from other
  keep_diam<-which( ( labs== 2) )
  keep_hex<-which( ( labs== 3) )
  keep_pent<-which( ( labs== 5) )
  keep_sc<-which( ( labs== 8) )
  keep_squ<-which( ( labs== 9) )
  keep_tear<-which( ( labs== 10) )
  keep_trap<-which( ( labs== 11) )
  keep_tri<-which( ( labs== 12) )

  diam_rand<-sample(keep_diam, 12)
  train_2<-diam_rand[1:6]
  hex_rand_reg<-sample(keep_hex[1:6], 6)
  hex_rand_other<-sample(keep_hex[7:8], 2)
  hex_rand<-c(hex_rand_reg[1:3], hex_rand_other[1],
              hex_rand_reg[4:6], hex_rand_other[2])
  train_3<-hex_rand[1:4]

  pent_rand_reg<-sample(keep_pent[3:12], 10)
  pent_rand_other<-sample(keep_pent[1:2], 2)
  pent_rand<-c(pent_rand_reg[1:5], pent_rand_other[1],
              pent_rand_reg[6:10], pent_rand_other[2])
  train_5<-pent_rand[1:6]
  sc_rand<-sample(keep_sc, 4)
  train_8<-sc_rand[1:2]

  squ_rand<-sample(keep_squ, 8)
  train_9<-squ_rand[1:4]
  tear_rand<-sample(keep_tear, 10)
  train_10<-tear_rand[1:5]

  trap_rand<-sample(keep_trap, 4)
  train_11<-trap_rand[1:2]
  tri_rand<-sample(keep_tri, 12)
  train_12<-tri_rand[1:6]


  train_vals<-c(train_01, train_2, train_3, train_4,
                train_5, train_6, train_7, train_8,
                train_9, train_10, train_11, train_12)

  train_vec<-rep(FALSE, length(labs))
  train_vec[train_vals]<-TRUE

  #cleanign and setup of data
  data<- mydata
  train<-train_vec

  labs<-data[,1]
  data<-data[,-1]
  #classes to categorize must be the last column
  data<-cbind(data, labs)
  #putting training values as first column
  data<-cbind(train, data)

  ############################
  #doing initial meta-class
  ############################

  data<-data[,-9]

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

  dist1_mink_3<-c()
  dist2_mink_3<-c()

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
  error_vars_best = matrix(nrow=m, ncol=2, data=0)

  i=1
  m=1

  #get first possible combination
  relab = as.character(combos[i,])
  relab[4] = "1"
  relab[6] = "1"
  relab[7] = "1"
  #create loopkup table
  lookup <- data.frame(cbind(
            names(table(labs)),
            relab
            ))
  #recode the labels to new combination
  labs_new = lookup$relab[match(labs, lookup$V1)]

  labs_svm<-as.numeric(labs_new)

  test<-as.data.frame(cbind(labs_svm[train], data[train,]) )
  colnames(test)[1]<-"labs_svm"

  valid<-as.data.frame(cbind(labs_svm[-train], data[-train,]) )
  colnames(valid)[1]<-"labs_svm"

  #getting counts
  lasso_labs = test[,1]
  change = which(lasso_labs==2)
  lasso_labs[change] = 0
  lasso_labs = test[,1]
  #getting class sizes
  size_1[i]<-var(table(labs[train[which(lasso_labs==1)] ] ))
  size_0[i]<-var(table(labs[train[which(lasso_labs==0)] ] ))

  #change NA's to 0's if a single class
  if(is.na(size_1[i]) ){

    size_1[i]=0

  }

  if(is.na(size_0[i]) ){

    size_0[i]=0

  }

  #doing all possible combinations of variables
  v <- dim(test)[2]-1
  var_combos<-t(combn(v, 2))+1
  ones<-which(test[,1]==1)
  twos<-which(test[,1]==2)

  for(j in 1:dim(var_combos)[1]){
    keep<-c(1,var_combos[j,])

    #keeping variables here
    test2<-test[,keep]
    valid2<-valid[,keep]

    tune.out<-tune(svm, as.factor(labs_svm) ~.,
              data=test2,
              kernel='polynomial',
              ranges=list(cost=c(1), coef0=c(1:3, 50), degree=c(1:3, 5)),
              validation.x=valid2[,-1],
              validation.y=as.factor(valid2[,1]),
              tunecontrol = tune.control(sampling = "fix"))

    ypred=predict(tune.out$best.model ,test2)
    #table(predict=ypred, truth=test$labs_svm)
    errors_train2[j] = mean(ypred== test2$labs_svm )

    ypred=predict(tune.out$best.model ,valid2)
    #table(predict=ypred, truth=test$labs_svm)
    errors_valid2[j] = mean(ypred== valid2$labs_svm )

    temp_mat<-test2
    temp_mat[,1]<- (temp_mat[,1]-mean(temp_mat[,1]))/sd(temp_mat[,1])
    temp_mat[,2]<- (temp_mat[,2]-mean(temp_mat[,2]))/sd(temp_mat[,2])

    dist1_e_feat[j]<-mean(dist(temp_mat[ones,-1 ], method="euclidean"))
    dist2_e_feat[j]<-mean(dist(test2[twos,-1 ], method="euclidean"))

  }

  c_b<-which(errors_train2==max(errors_train2))
  abs_diff_dist<-abs(dist1_e_feat-dist2_e_feat)
  c_b_choice<-which(abs_diff_dist[c_b]==min(abs_diff_dist[c_b]))

  errors_train[i]<-errors_train2[c_b[c_b_choice]]
  errors_valid[i]<-errors_valid2[c_b[c_b_choice]]
  error_vars_best[i,]<-var_combos[c_b[c_b_choice],]

  ones<-which(test[,1]==1)
  twos<-which(test[,1]==2)
  #euclidean
  dist1_e[i]<-mean(dist(test[ones,error_vars_best[i,]], method="euclidean" ))
  dist2_e[i]<-mean(dist(test[twos,error_vars_best[i,]], method="euclidean" ))

  dist1_mink_3[i]<-mean(dist(test[ones,error_vars_best[i,]],
                          method="minkowski", p=3))
  dist2_mink_3[i]<-mean(dist(test[twos,error_vars_best[i,]],
                          method="minkowski", p=3 ))

  errors<-cbind(errors_train, errors_valid)
  colnames(errors)<-c("Error_train", "Error_valid")

  #finding which errors for training and valid have perfect results
  best<- which( (errors_train==1) )
  print(best)
  print(errors)

  variables<- colnames(data[,error_vars_best[best,]-1])
  variables


  #create first meta group of
  #round, capsule, oval and rectangle
  #vs.
  #else

  #cleanign and setup of data
  data<- mydata
  train<-train_vec

  labs<-data[,1]
  data<-data[,-1]
  #classes to categorize must be the last column
  data<-cbind(data, labs)
  #putting training values as first column
  data<-cbind(train, data)


  #########

  keep1<-which(labs==1)
  keep2<-which(labs==2)
  keep3<-which(labs==3)

  keep4<-which(labs==4)
  keep5<-which(labs==5)

  keep6<-which(labs==6)
  keep7<-which(labs==7)
  keep8<-which(labs==8)

  keep10<-which(labs==10)
  keep11<-which(labs==11)
  keep12<-which(labs==12)

  keep<-c(keep1, #capsule
          keep4, #oval
          keep6, #rect
          keep7 #round
          )


  data1<-data[keep,]

  tree1 <- Node$new("Capsule, Round, Oval, and Rectangle")
  TrainDMG(tree1, data1, val )
  print(tree1, "TrainCount", "class1",
              "class0", "trainError",
              "validError", "variables",
              "Cost", "Degree",  "coef0")
  #export_graph(ToDiagrammeRGraph(tree1), "tree1_11_20_num3_0.41.pdf")

  #########

  data2<-data[-keep,]

  tree2 <- Node$new("Else")
  TrainDMG(tree2, data2, val )
  print(tree2, "TrainCount", "class1",
              "class0", "trainError",
              "validError", "variables",
              "Cost", "Degree",  "coef0")
  #export_graph(ToDiagrammeRGraph(tree2), "tree2_11_20_num3_0.41.pdf")

  print('##################')
  print('# 4th run')
  print('##################')

  mydata<-cbind(labs, sp, temp, temp_shapes)

  cols_keep<-c(1:6, 10, 11)

  mydata<-mydata[,cols_keep]
  colnames(mydata)<-c("labs_svm", "SP",
                      "White", "Black", "Circularity",
                      "Eccentricity", "White_Box", "Black_Box"
                      )


  #sample from round, oval, caps, and rectangle
  keep_rect<-which( ( labs== 6) )
  keep_circ<-which( ( labs== 7) )
  keep_cap<-which( ( labs== 1) )
  keep_oval<-which( ( labs== 4) )

  cap_rand<-sample(keep_cap, ref_tab_counts[1])
  train_01<-cap_rand[1:(ref_tab_counts[1]/2)]
  oval_rand<-sample(keep_oval, ref_tab_counts[4])
  train_4<-oval_rand[1:(ref_tab_counts[4]/2)]

  rect_rand<-sample(keep_rect, 6)
  train_6<-rect_rand[1:3]
  circ_rand<-sample(keep_circ, ref_tab_counts[7])
  train_7<-circ_rand[1:(ref_tab_counts[7]/2)]

  #sample from other
  keep_diam<-which( ( labs== 2) )
  keep_hex<-which( ( labs== 3) )
  keep_pent<-which( ( labs== 5) )
  keep_sc<-which( ( labs== 8) )
  keep_squ<-which( ( labs== 9) )
  keep_tear<-which( ( labs== 10) )
  keep_trap<-which( ( labs== 11) )
  keep_tri<-which( ( labs== 12) )

  diam_rand<-sample(keep_diam, 12)
  train_2<-diam_rand[1:6]
  hex_rand_reg<-sample(keep_hex[1:6], 6)
  hex_rand_other<-sample(keep_hex[7:8], 2)
  hex_rand<-c(hex_rand_reg[1:3], hex_rand_other[1],
              hex_rand_reg[4:6], hex_rand_other[2])
  train_3<-hex_rand[1:4]

  pent_rand_reg<-sample(keep_pent[3:12], 10)
  pent_rand_other<-sample(keep_pent[1:2], 2)
  pent_rand<-c(pent_rand_reg[1:5], pent_rand_other[1],
              pent_rand_reg[6:10], pent_rand_other[2])
  train_5<-pent_rand[1:6]
  sc_rand<-sample(keep_sc, 4)
  train_8<-sc_rand[1:2]

  squ_rand<-sample(keep_squ, 8)
  train_9<-squ_rand[1:4]
  tear_rand<-sample(keep_tear, 10)
  train_10<-tear_rand[1:5]

  trap_rand<-sample(keep_trap, 4)
  train_11<-trap_rand[1:2]
  tri_rand<-sample(keep_tri, 12)
  train_12<-tri_rand[1:6]


  train_vals<-c(train_01, train_2, train_3, train_4,
                train_5, train_6, train_7, train_8,
                train_9, train_10, train_11, train_12)

  train_vec<-rep(FALSE, length(labs))
  train_vec[train_vals]<-TRUE

  #cleanign and setup of data
  data<- mydata
  train<-train_vec

  labs<-data[,1]
  data<-data[,-1]
  #classes to categorize must be the last column
  data<-cbind(data, labs)
  #putting training values as first column
  data<-cbind(train, data)

  ############################
  #doing initial meta-class
  ############################

  data<-data[,-9]

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

  dist1_mink_3<-c()
  dist2_mink_3<-c()

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
  error_vars_best = matrix(nrow=m, ncol=2, data=0)

  i=1
  m=1

  #get first possible combination
  relab = as.character(combos[i,])
  relab[4] = "1"
  relab[6] = "1"
  relab[7] = "1"
  #create loopkup table
  lookup <- data.frame(cbind(
            names(table(labs)),
            relab
            ))
  #recode the labels to new combination
  labs_new = lookup$relab[match(labs, lookup$V1)]

  labs_svm<-as.numeric(labs_new)

  test<-as.data.frame(cbind(labs_svm[train], data[train,]) )
  colnames(test)[1]<-"labs_svm"

  valid<-as.data.frame(cbind(labs_svm[-train], data[-train,]) )
  colnames(valid)[1]<-"labs_svm"

  #getting counts
  lasso_labs = test[,1]
  change = which(lasso_labs==2)
  lasso_labs[change] = 0
  lasso_labs = test[,1]
  #getting class sizes
  size_1[i]<-var(table(labs[train[which(lasso_labs==1)] ] ))
  size_0[i]<-var(table(labs[train[which(lasso_labs==0)] ] ))

  #change NA's to 0's if a single class
  if(is.na(size_1[i]) ){

    size_1[i]=0

  }

  if(is.na(size_0[i]) ){

    size_0[i]=0

  }

  #doing all possible combinations of variables
  v <- dim(test)[2]-1
  var_combos<-t(combn(v, 2))+1
  ones<-which(test[,1]==1)
  twos<-which(test[,1]==2)

  for(j in 1:dim(var_combos)[1]){
    keep<-c(1,var_combos[j,])

    #keeping variables here
    test2<-test[,keep]
    valid2<-valid[,keep]

    tune.out<-tune(svm, as.factor(labs_svm) ~.,
              data=test2,
              kernel='polynomial',
              ranges=list(cost=c(1), coef0=c(1:3, 50), degree=c(1:3, 5)),
              validation.x=valid2[,-1],
              validation.y=as.factor(valid2[,1]),
              tunecontrol = tune.control(sampling = "fix"))

    ypred=predict(tune.out$best.model ,test2)
    #table(predict=ypred, truth=test$labs_svm)
    errors_train2[j] = mean(ypred== test2$labs_svm )

    ypred=predict(tune.out$best.model ,valid2)
    #table(predict=ypred, truth=test$labs_svm)
    errors_valid2[j] = mean(ypred== valid2$labs_svm )

    temp_mat<-test2
    temp_mat[,1]<- (temp_mat[,1]-mean(temp_mat[,1]))/sd(temp_mat[,1])
    temp_mat[,2]<- (temp_mat[,2]-mean(temp_mat[,2]))/sd(temp_mat[,2])

    dist1_e_feat[j]<-mean(dist(temp_mat[ones,-1 ], method="euclidean"))
    dist2_e_feat[j]<-mean(dist(test2[twos,-1 ], method="euclidean"))

  }

  c_b<-which(errors_train2==max(errors_train2))
  abs_diff_dist<-abs(dist1_e_feat-dist2_e_feat)
  c_b_choice<-which(abs_diff_dist[c_b]==min(abs_diff_dist[c_b]))

  errors_train[i]<-errors_train2[c_b[c_b_choice]]
  errors_valid[i]<-errors_valid2[c_b[c_b_choice]]
  error_vars_best[i,]<-var_combos[c_b[c_b_choice],]

  ones<-which(test[,1]==1)
  twos<-which(test[,1]==2)
  #euclidean
  dist1_e[i]<-mean(dist(test[ones,error_vars_best[i,]], method="euclidean" ))
  dist2_e[i]<-mean(dist(test[twos,error_vars_best[i,]], method="euclidean" ))

  dist1_mink_3[i]<-mean(dist(test[ones,error_vars_best[i,]],
                          method="minkowski", p=3))
  dist2_mink_3[i]<-mean(dist(test[twos,error_vars_best[i,]],
                          method="minkowski", p=3 ))

  errors<-cbind(errors_train, errors_valid)
  colnames(errors)<-c("Error_train", "Error_valid")

  #finding which errors for training and valid have perfect results
  best<- which( (errors_train==1) )
  print(best)
  print(errors)

  variables<- colnames(data[,error_vars_best[best,]-1])
  variables


  #create first meta group of
  #round, capsule, oval and rectangle
  #vs.
  #else

  #cleanign and setup of data
  data<- mydata
  train<-train_vec

  labs<-data[,1]
  data<-data[,-1]
  #classes to categorize must be the last column
  data<-cbind(data, labs)
  #putting training values as first column
  data<-cbind(train, data)


  #########

  keep1<-which(labs==1)
  keep2<-which(labs==2)
  keep3<-which(labs==3)

  keep4<-which(labs==4)
  keep5<-which(labs==5)

  keep6<-which(labs==6)
  keep7<-which(labs==7)
  keep8<-which(labs==8)

  keep10<-which(labs==10)
  keep11<-which(labs==11)
  keep12<-which(labs==12)

  keep<-c(keep1, #capsule
          keep4, #oval
          keep6, #rect
          keep7 #round
          )


  data1<-data[keep,]

  tree1 <- Node$new("Capsule, Round, Oval, and Rectangle")
  TrainDMG(tree1, data1, val )
  print(tree1, "TrainCount", "class1",
              "class0", "trainError",
              "validError", "variables",
              "Cost", "Degree",  "coef0")
  #export_graph(ToDiagrammeRGraph(tree1), "tree1_11_20_num4_0.41.pdf")

  #########

  data2<-data[-keep,]

  tree2 <- Node$new("Else")
  TrainDMG(tree2, data2, val )
  print(tree2, "TrainCount", "class1",
              "class0", "trainError",
              "validError", "variables",
              "Cost", "Degree",  "coef0")
  #export_graph(ToDiagrammeRGraph(tree2), "tree2_11_20_num4_0.41.pdf")

  print('##################')
  print('# 5th run')
  print('##################')

  mydata<-cbind(labs, sp, temp, temp_shapes)

  cols_keep<-c(1:6, 10, 11)

  mydata<-mydata[,cols_keep]
  colnames(mydata)<-c("labs_svm", "SP",
                      "White", "Black", "Circularity",
                      "Eccentricity", "White_Box", "Black_Box"
                      )


  #sample from round, oval, caps, and rectangle
  keep_rect<-which( ( labs== 6) )
  keep_circ<-which( ( labs== 7) )
  keep_cap<-which( ( labs== 1) )
  keep_oval<-which( ( labs== 4) )

  cap_rand<-sample(keep_cap, ref_tab_counts[1])
  train_01<-cap_rand[1:(ref_tab_counts[1]/2)]
  oval_rand<-sample(keep_oval, ref_tab_counts[4])
  train_4<-oval_rand[1:(ref_tab_counts[4]/2)]

  rect_rand<-sample(keep_rect, 6)
  train_6<-rect_rand[1:3]
  circ_rand<-sample(keep_circ, ref_tab_counts[7])
  train_7<-circ_rand[1:(ref_tab_counts[7]/2)]

  #sample from other
  keep_diam<-which( ( labs== 2) )
  keep_hex<-which( ( labs== 3) )
  keep_pent<-which( ( labs== 5) )
  keep_sc<-which( ( labs== 8) )
  keep_squ<-which( ( labs== 9) )
  keep_tear<-which( ( labs== 10) )
  keep_trap<-which( ( labs== 11) )
  keep_tri<-which( ( labs== 12) )

  diam_rand<-sample(keep_diam, 12)
  train_2<-diam_rand[1:6]
  hex_rand_reg<-sample(keep_hex[1:6], 6)
  hex_rand_other<-sample(keep_hex[7:8], 2)
  hex_rand<-c(hex_rand_reg[1:3], hex_rand_other[1],
              hex_rand_reg[4:6], hex_rand_other[2])
  train_3<-hex_rand[1:4]

  pent_rand_reg<-sample(keep_pent[3:12], 10)
  pent_rand_other<-sample(keep_pent[1:2], 2)
  pent_rand<-c(pent_rand_reg[1:5], pent_rand_other[1],
              pent_rand_reg[6:10], pent_rand_other[2])
  train_5<-pent_rand[1:6]
  sc_rand<-sample(keep_sc, 4)
  train_8<-sc_rand[1:2]

  squ_rand<-sample(keep_squ, 8)
  train_9<-squ_rand[1:4]
  tear_rand<-sample(keep_tear, 10)
  train_10<-tear_rand[1:5]

  trap_rand<-sample(keep_trap, 4)
  train_11<-trap_rand[1:2]
  tri_rand<-sample(keep_tri, 12)
  train_12<-tri_rand[1:6]


  train_vals<-c(train_01, train_2, train_3, train_4,
                train_5, train_6, train_7, train_8,
                train_9, train_10, train_11, train_12)

  train_vec<-rep(FALSE, length(labs))
  train_vec[train_vals]<-TRUE

  #cleanign and setup of data
  data<- mydata
  train<-train_vec

  labs<-data[,1]
  data<-data[,-1]
  #classes to categorize must be the last column
  data<-cbind(data, labs)
  #putting training values as first column
  data<-cbind(train, data)

  ############################
  #doing initial meta-class
  ############################

  data<-data[,-9]

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

  dist1_mink_3<-c()
  dist2_mink_3<-c()

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
  error_vars_best = matrix(nrow=m, ncol=2, data=0)

  i=1
  m=1

  #get first possible combination
  relab = as.character(combos[i,])
  relab[4] = "1"
  relab[6] = "1"
  relab[7] = "1"
  #create loopkup table
  lookup <- data.frame(cbind(
            names(table(labs)),
            relab
            ))
  #recode the labels to new combination
  labs_new = lookup$relab[match(labs, lookup$V1)]

  labs_svm<-as.numeric(labs_new)

  test<-as.data.frame(cbind(labs_svm[train], data[train,]) )
  colnames(test)[1]<-"labs_svm"

  valid<-as.data.frame(cbind(labs_svm[-train], data[-train,]) )
  colnames(valid)[1]<-"labs_svm"

  #getting counts
  lasso_labs = test[,1]
  change = which(lasso_labs==2)
  lasso_labs[change] = 0
  lasso_labs = test[,1]
  #getting class sizes
  size_1[i]<-var(table(labs[train[which(lasso_labs==1)] ] ))
  size_0[i]<-var(table(labs[train[which(lasso_labs==0)] ] ))

  #change NA's to 0's if a single class
  if(is.na(size_1[i]) ){

    size_1[i]=0

  }

  if(is.na(size_0[i]) ){

    size_0[i]=0

  }

  #doing all possible combinations of variables
  v <- dim(test)[2]-1
  var_combos<-t(combn(v, 2))+1
  ones<-which(test[,1]==1)
  twos<-which(test[,1]==2)

  for(j in 1:dim(var_combos)[1]){
    keep<-c(1,var_combos[j,])

    #keeping variables here
    test2<-test[,keep]
    valid2<-valid[,keep]

    tune.out<-tune(svm, as.factor(labs_svm) ~.,
              data=test2,
              kernel='polynomial',
              ranges=list(cost=c(1), coef0=c(1:3, 50), degree=c(1:3, 5)),
              validation.x=valid2[,-1],
              validation.y=as.factor(valid2[,1]),
              tunecontrol = tune.control(sampling = "fix"))

    ypred=predict(tune.out$best.model ,test2)
    #table(predict=ypred, truth=test$labs_svm)
    errors_train2[j] = mean(ypred== test2$labs_svm )

    ypred=predict(tune.out$best.model ,valid2)
    #table(predict=ypred, truth=test$labs_svm)
    errors_valid2[j] = mean(ypred== valid2$labs_svm )

    temp_mat<-test2
    temp_mat[,1]<- (temp_mat[,1]-mean(temp_mat[,1]))/sd(temp_mat[,1])
    temp_mat[,2]<- (temp_mat[,2]-mean(temp_mat[,2]))/sd(temp_mat[,2])

    dist1_e_feat[j]<-mean(dist(temp_mat[ones,-1 ], method="euclidean"))
    dist2_e_feat[j]<-mean(dist(test2[twos,-1 ], method="euclidean"))

  }

  c_b<-which(errors_train2==max(errors_train2))
  abs_diff_dist<-abs(dist1_e_feat-dist2_e_feat)
  c_b_choice<-which(abs_diff_dist[c_b]==min(abs_diff_dist[c_b]))

  errors_train[i]<-errors_train2[c_b[c_b_choice]]
  errors_valid[i]<-errors_valid2[c_b[c_b_choice]]
  error_vars_best[i,]<-var_combos[c_b[c_b_choice],]

  ones<-which(test[,1]==1)
  twos<-which(test[,1]==2)
  #euclidean
  dist1_e[i]<-mean(dist(test[ones,error_vars_best[i,]], method="euclidean" ))
  dist2_e[i]<-mean(dist(test[twos,error_vars_best[i,]], method="euclidean" ))

  dist1_mink_3[i]<-mean(dist(test[ones,error_vars_best[i,]],
                          method="minkowski", p=3))
  dist2_mink_3[i]<-mean(dist(test[twos,error_vars_best[i,]],
                          method="minkowski", p=3 ))

  errors<-cbind(errors_train, errors_valid)
  colnames(errors)<-c("Error_train", "Error_valid")

  #finding which errors for training and valid have perfect results
  best<- which( (errors_train==1) )
  print(best)
  print(errors)

  variables<- colnames(data[,error_vars_best[best,]-1])
  variables


  #create first meta group of
  #round, capsule, oval and rectangle
  #vs.
  #else

  #cleanign and setup of data
  data<- mydata
  train<-train_vec

  labs<-data[,1]
  data<-data[,-1]
  #classes to categorize must be the last column
  data<-cbind(data, labs)
  #putting training values as first column
  data<-cbind(train, data)


  #########

  keep1<-which(labs==1)
  keep2<-which(labs==2)
  keep3<-which(labs==3)

  keep4<-which(labs==4)
  keep5<-which(labs==5)

  keep6<-which(labs==6)
  keep7<-which(labs==7)
  keep8<-which(labs==8)

  keep10<-which(labs==10)
  keep11<-which(labs==11)
  keep12<-which(labs==12)

  keep<-c(keep1, #capsule
          keep4, #oval
          keep6, #rect
          keep7 #round
          )


  data1<-data[keep,]

  tree1 <- Node$new("Capsule, Round, Oval, and Rectangle")
  TrainDMG(tree1, data1, val )
  print(tree1, "TrainCount", "class1",
              "class0", "trainError",
              "validError", "variables",
              "Cost", "Degree",  "coef0")
  #export_graph(ToDiagrammeRGraph(tree1), "tree1_11_20_num5_0.41.pdf")

  #########

  data2<-data[-keep,]

  tree2 <- Node$new("Else")
  TrainDMG(tree2, data2, val )
  print(tree2, "TrainCount", "class1",
              "class0", "trainError",
              "validError", "variables",
              "Cost", "Degree",  "coef0")
  #export_graph(ToDiagrammeRGraph(tree2), "tree2_11_20_num5_0.41.pdf")






#
