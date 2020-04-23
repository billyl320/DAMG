#example from data.tree website
#https://cran.r-project.org/web/packages/data.tree/vignettes/applications.html

#removes the plotting function, but keeps everything else the same from 2_1_2020 version

rm(list=ls())

#to allow R CMD BATCH to continue if encountering an error
library(R.utils)

#number of variables
number_vars=3

  val = 0.41

  print('Value of')
  print(val)

  set.seed(45846)

  source('rf_tree_val.r')

  ##################
  #loading data
  ##################

  #cleaning data for analysis

  #bird 1
  bir11 <-read.table("bir1_SHAPES.txt", sep=",", header=TRUE)
  bir14 <-read.table("bir1_EI.txt", sep=",", header=TRUE)

  bir1<-cbind(bir11, bir14)

  #bird 2
  bir21 <-read.table("bir2_SHAPES.txt", sep=",", header=TRUE)
  bir24 <-read.table("bir2_EI.txt", sep=",", header=TRUE)

  bir2<-cbind(bir21, bir24)

  #bird 3
  bir31 <-read.table("bir3_SHAPES.txt", sep=",", header=TRUE)
  bir34 <-read.table("bir3_EI.txt", sep=",", header=TRUE)

  bir3<-cbind(bir31, bir34)

  #bird 4
  bir41 <-read.table("bir4_SHAPES.txt", sep=",", header=TRUE)
  bir44 <-read.table("bir4_EI.txt", sep=",", header=TRUE)

  bir4<-cbind(bir41, bir44)


  bird<-rbind(bir1, bir2, bir3, bir4)

  #bone
  bone1 <-read.table("bone_SHAPES.txt", sep=",", header=TRUE)
  bone4 <-read.table("bone_EI.txt", sep=",", header=TRUE)

  bone<-cbind(bone1, bone4)

  #brik
  brik1 <-read.table("brik_SHAPES.txt", sep=",", header=TRUE)
  brik4 <-read.table("brik_EI.txt", sep=",", header=TRUE)

  brik<-cbind(brik1, brik4)

  #cams
  cam11 <-read.table("cams_SHAPES.txt", sep=",", header=TRUE)
  cam14 <-read.table("cams_EI.txt", sep=",", header=TRUE)

  cams<-cbind(cam11, cam14)

  #cups
  cup1 <-read.table("cups_SHAPES.txt", sep=",", header=TRUE)
  cup2 <-read.table("cups_EI.txt", sep=",", header=TRUE)

  cups<-cbind(cup1, cup2)

  #cleaning data for ggplot2 and analysis
  labs<-as.factor(c(rep(1, dim(bird)[1]),
                    rep(2, dim(bone)[1]),
                    rep(3, dim(brik)[1]),
                    rep(4, dim(cams)[1]),
                    rep(5, dim(cups)[1])    ) )

  labs2<-as.factor(c(rep("Bird", dim(bird)[1]),
                    rep("Bone", dim(bone)[1]),
                    rep("Brick", dim(brik)[1]),
                    rep("Camel", dim(cams)[1]),
                    rep("Cups", dim(cups)[1])    ) )

  #complete data set

  temp<-rbind(bird, bone, brik, cams, cups
              )

  sp<-temp$white/(temp$white+temp$black)


  print('##################')
  print('# first run')
  print('##################')

  mydata<-cbind(labs, sp, temp)

  colnames(mydata)[c(1:9, 17,18)]<-c("labs_rf", "SP",
                      "Circ", "Ecc", "E1", "E2",
                      "Corners", "WhiteBB", "BlackBB",
                      "White", "Black"
                      )
    mydata<-mydata[,c(-7,-10:-16)]


  #sample from classes
  keep_bird<-which( ( labs== 1) )
  bird_rand1<-sample(keep_bird, 80)
  train_1<-c(bird_rand1[1:56])

  keep_bone<-which( ( labs== 2) )
  bone_rand<-sample(keep_bone, 80)
  train_2<-bone_rand[1:56]

  keep_brick<-which( ( labs== 3) )
  brick_rand<-sample(keep_brick, 80)
  train_3<-brick_rand[1:56]

  keep_cams<-which( ( labs== 4) )
  cams_rand1<-sample(keep_cams, 80)
  train_4<-c(cams_rand1[1:56])

  keep_cups<-which( ( labs== 5) )
  cups_rand<-sample(keep_cups, 80)
  train_5<-c(cups_rand[1:56])

  ref_tab_counts<-table(labs)


  train_vals<-c(train_1, train_2, train_3, train_4,
                train_5)

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


  tree1 <- Node$new("Bird, Bone, Brick, Camels, and cups")
  rslt=withTimeout(expr=TrainDMG(tree1, data, val, number_vars ),
                 timeout=60*60,
                onTimeout=c("silent"))

if(is.null(rslt)==TRUE){

  next

}
  print(tree1, "TrainCount", "class1",
              "class0", "trainError",
              "validError", "variables",
              "mtry", "ntree", "Importance")



  print('##################')
  print('# 2nd run')
  print('##################')

  mydata<-cbind(labs, sp, temp)

  colnames(mydata)[c(1:9, 17,18)]<-c("labs_rf", "SP",
                      "Circ", "Ecc", "E1", "E2",
                      "Corners", "WhiteBB", "BlackBB",
                      "White", "Black"
                      )
    mydata<-mydata[,c(-7,-10:-16)]

  #sample from classes
  keep_bird<-which( ( labs== 1) )
  bird_rand1<-sample(keep_bird, 80)
  train_1<-c(bird_rand1[1:56])

  keep_bone<-which( ( labs== 2) )
  bone_rand<-sample(keep_bone, 80)
  train_2<-bone_rand[1:56]

  keep_brick<-which( ( labs== 3) )
  brick_rand<-sample(keep_brick, 80)
  train_3<-brick_rand[1:56]

  keep_cams<-which( ( labs== 4) )
  cams_rand1<-sample(keep_cams, 80)
  train_4<-c(cams_rand1[1:56])

  keep_cups<-which( ( labs== 5) )
  cups_rand<-sample(keep_cups, 80)
  train_5<-c(cups_rand[1:56])

  ref_tab_counts<-table(labs)


  train_vals<-c(train_1, train_2, train_3, train_4,
                train_5)

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


  tree1 <- Node$new("Bird, Bone, Brick, Camels, and cups")
  rslt=withTimeout(expr=TrainDMG(tree1, data, val, number_vars ),
                 timeout=60*60,
                onTimeout=c("silent"))

if(is.null(rslt)==TRUE){

  next

}
  print(tree1, "TrainCount", "class1",
              "class0", "trainError",
              "validError", "variables",
              "mtry", "ntree", "importance")


print('##################')
print('# 3rd run')
print('##################')

mydata<-cbind(labs, sp, temp)

colnames(mydata)[c(1:9, 17,18)]<-c("labs_rf", "SP",
                    "Circ", "Ecc", "E1", "E2",
                    "Corners", "WhiteBB", "BlackBB",
                    "White", "Black"
                    )
  mydata<-mydata[,c(-7,-10:-16)]

#sample from classes
keep_bird<-which( ( labs== 1) )
bird_rand1<-sample(keep_bird, 80)
train_1<-c(bird_rand1[1:56])

keep_bone<-which( ( labs== 2) )
bone_rand<-sample(keep_bone, 80)
train_2<-bone_rand[1:56]

keep_brick<-which( ( labs== 3) )
brick_rand<-sample(keep_brick, 80)
train_3<-brick_rand[1:56]

keep_cams<-which( ( labs== 4) )
cams_rand1<-sample(keep_cams, 80)
train_4<-c(cams_rand1[1:56])

keep_cups<-which( ( labs== 5) )
cups_rand<-sample(keep_cups, 80)
train_5<-c(cups_rand[1:56])

ref_tab_counts<-table(labs)


train_vals<-c(train_1, train_2, train_3, train_4,
              train_5)

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


tree1 <- Node$new("Bird, Bone, Brick, Camels, and cups")
rslt=withTimeout(expr=TrainDMG(tree1, data, val, number_vars ),
                 timeout=60*60,
                onTimeout=c("silent"))

if(is.null(rslt)==TRUE){

  next

}
print(tree1, "TrainCount", "class1",
            "class0", "trainError",
            "validError", "variables",
            "mtry", "ntree", "Importance")

print('##################')
print('# 4th run')
print('##################')

mydata<-cbind(labs, sp, temp)

colnames(mydata)[c(1:9, 17,18)]<-c("labs_rf", "SP",
                    "Circ", "Ecc", "E1", "E2",
                    "Corners", "WhiteBB", "BlackBB",
                    "White", "Black"
                    )
  mydata<-mydata[,c(-7,-10:-16)]

#sample from classes
keep_bird<-which( ( labs== 1) )
bird_rand1<-sample(keep_bird, 80)
train_1<-c(bird_rand1[1:56])

keep_bone<-which( ( labs== 2) )
bone_rand<-sample(keep_bone, 80)
train_2<-bone_rand[1:56]

keep_brick<-which( ( labs== 3) )
brick_rand<-sample(keep_brick, 80)
train_3<-brick_rand[1:56]

keep_cams<-which( ( labs== 4) )
cams_rand1<-sample(keep_cams, 80)
train_4<-c(cams_rand1[1:56])

keep_cups<-which( ( labs== 5) )
cups_rand<-sample(keep_cups, 80)
train_5<-c(cups_rand[1:56])

ref_tab_counts<-table(labs)


train_vals<-c(train_1, train_2, train_3, train_4,
              train_5)

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


tree1 <- Node$new("Bird, Bone, Brick, Camels, and cups")
rslt=withTimeout(expr=TrainDMG(tree1, data, val, number_vars ),
                 timeout=60*60,
                onTimeout=c("silent"))

if(is.null(rslt)==TRUE){

  next

}
print(tree1, "TrainCount", "class1",
            "class0", "trainError",
            "validError", "variables",
            "mtry", "ntree", "Importance")

print('##################')
print('# 5th run')
print('##################')

mydata<-cbind(labs, sp, temp)

colnames(mydata)[c(1:9, 17,18)]<-c("labs_rf", "SP",
                    "Circ", "Ecc", "E1", "E2",
                    "Corners", "WhiteBB", "BlackBB",
                    "White", "Black"
                    )
  mydata<-mydata[,c(-7,-10:-16)]

#sample from classes
keep_bird<-which( ( labs== 1) )
bird_rand1<-sample(keep_bird, 80)
train_1<-c(bird_rand1[1:56])

keep_bone<-which( ( labs== 2) )
bone_rand<-sample(keep_bone, 80)
train_2<-bone_rand[1:56]

keep_brick<-which( ( labs== 3) )
brick_rand<-sample(keep_brick, 80)
train_3<-brick_rand[1:56]

keep_cams<-which( ( labs== 4) )
cams_rand1<-sample(keep_cams, 80)
train_4<-c(cams_rand1[1:56])

keep_cups<-which( ( labs== 5) )
cups_rand<-sample(keep_cups, 80)
train_5<-c(cups_rand[1:56])

ref_tab_counts<-table(labs)


train_vals<-c(train_1, train_2, train_3, train_4,
              train_5)

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


tree1 <- Node$new("Bird, Bone, Brick, Camels, and cups")
rslt=withTimeout(expr=TrainDMG(tree1, data, val, number_vars ),
                 timeout=60*60,
                onTimeout=c("silent"))

if(is.null(rslt)==TRUE){

  next

}
print(tree1, "TrainCount", "class1",
            "class0", "trainError",
            "validError", "variables",
            "mtry", "ntree", "Importance")


#
