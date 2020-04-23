#recall calculation for Maddala models

#LR

diag<-c(231, 4, 0 , 767, 0, 1, 1050, 0, 9, 2, 8)
rowsums<-c(243, 8, 3, 790, 8, 4, 1054, 7, 9, 3, 10)

mean(diag/rowsums)

#Adaptable Rings

diag<-c(230, 8, 3, 779, 8, 4, 1051, 7, 9, 3, 10)
rowsums<-c(243, 8, 3, 790, 8, 4, 1054, 7, 9, 3, 10)

mean(diag/rowsums)

#for comparison Lamberti - HMH

diag<-c(24+307, 24+593)
rowsums<-c(25+377, 25+593)

mean(c(diag/rowsums, rep(1, 9)))

#lamberti - DAMG

#mean average recall
mean(c(0.9892, 1, 0.90, 0.9854, 0.933, 1, 1, 1, 0.84, 0.60, 1, 1))

#adjusted mean average precision


#
