tp_fp_l<-c(402, 12, 8,
		  618, 12, 6,
		  904, 8, 10,
		  4, 12)

fp_l<-c( 71, 0, 0,
	   	1, 0, 0,
	    0, 0, 0,
	    0, 0 )

total<-c(332, 12, 8,
				 686, 12, 6,
			 	 906, 8,
			 	 10, 4, 12)

tp<-tp_fp_l-fp_l

recall<-tp/total
mean(recall)
