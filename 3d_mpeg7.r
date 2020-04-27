#3d scatterplot for the mpeg7 data meta-class
#brick and bone vs. bird

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
  library(scatterplot3d)
  source('rf_tree_val.r')

  ##################
  #loading data
  ##################

  #cleaning data for analysis

  #bird 1
  bir11 <-read.table("mpeg_txt_files/bird_SHAPES.txt", sep=",", header=TRUE)
  bir14 <-read.table("mpeg_txt_files/bird_EI.txt", sep=",", header=TRUE)

  bird<-cbind(bir11, bir14)

  #bone
  bone1 <-read.table("mpeg_txt_files/bone_SHAPES.txt", sep=",", header=TRUE)
  bone4 <-read.table("mpeg_txt_files/bone_EI.txt", sep=",", header=TRUE)

  bone<-cbind(bone1, bone4)

  #brik
  brik1 <-read.table("mpeg_txt_files/brik_SHAPES.txt", sep=",", header=TRUE)
  brik4 <-read.table("mpeg_txt_files/brik_EI.txt", sep=",", header=TRUE)

  brik<-cbind(brik1, brik4)

  #cams
  cam11 <-read.table("mpeg_txt_files/cams_SHAPES.txt", sep=",", header=TRUE)
  cam14 <-read.table("mpeg_txt_files/cams_EI.txt", sep=",", header=TRUE)

  cams<-cbind(cam11, cam14)

  #cups
  cup1 <-read.table("mpeg_txt_files/cups_SHAPES.txt", sep=",", header=TRUE)
  cup2 <-read.table("mpeg_txt_files/cups_EI.txt", sep=",", header=TRUE)

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


  colors <- c("#999999", "#E69F00", "#56B4E9")
  colors <- colors[as.numeric(mydata$labs_rf[c(keep_bird, keep_bone, keep_brick)])]
  table(colors)

    s3d<-scatterplot3d(mydata[c(keep_bird, keep_bone, keep_brick),2:4],
                      pch=16,
                      color=colors,
                      angle=60,
                      ylab="Circularity",
                      zlab="Eccentricity")

    legend(s3d$xyz.convert(0.25, 5, 175), legend = c("Bird", "Bone", "Brick"),
      col =  c("#999999", "#E69F00", "#56B4E9"), pch = 16)


#
