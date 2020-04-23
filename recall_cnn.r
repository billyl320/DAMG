#recall calculation for CNN

#training

bird_train<-c(11, 17, 11, 56, 12 )
mean(bird_train/56)

bone_train<-c(14, 17, 56, 13, 56 )
mean(bone_train/56)

brik_train<-c(56, 56, 16, 14, 13 )
mean(brik_train/56)

cams_train<-c( 9, 13, 12, 12, 14 )
mean(cams_train/56)

cups_train<-c(13, 13, 13, 14, 14 )
mean(cups_train/56)

mean( c(bird_train, bone_train, brik_train, cams_train, cups_train)/56 )

#testing
bird_test<-c(7, 3, 4, 22, 5 )
mean(bird_test/24)

bone_test<-c(6, 3, 24, 7, 24 )
mean(bone_test/24)

brik_test<-c(24, 24, 4, 6, 7 )
mean(brik_test/24)

cams_test<-c( 9, 6, 8, 7, 6 )
mean(cams_test/24)

cups_test<-c(6, 7, 6, 6, 6 )
mean(cups_test/24)

mean( c(bird_test, bone_test, brik_test, cams_test, cups_test)/24 )


##
