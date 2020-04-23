#converting images for analysis in R
#importing custom module for analysis
import convert2 as cvt

#desired directories
#note that each class should be separated into different directories.
#however, for the fucntion to work, multiple directories should be specified.
#thus, an empty folder is utilized for this task
#the empty folder is called "none"

#----------------------------------------------------------
#bird class

bird = ["/home/billy/Documents/Research/data/mpeg7/bird", "none"]


#name of .txt file
name = 'bird'

#converting images
cvt.BinaryHistTXT(name, bird)

#----------------------------------------------------------
#bone class

bone = ["/home/billy/Documents/Research/data/mpeg7/bone", "none"]


#name of .txt file
name = 'bone'

#converting images
cvt.BinaryHistTXT(name, bone)


#----------------------------------------------------------
#brick class

brik = ["/home/billy/Documents/Research/data/mpeg7/brik", "none"]


#name of .txt file
name = 'brik'

#converting images
cvt.BinaryHistTXT(name, brik)

#----------------------------------------------------------
#cam class

cams = ["/home/billy/Documents/Research/data/mpeg7/cams", "none"]


#name of .txt file
name = 'cams'

#converting images
cvt.BinaryHistTXT(name, cams)


#----------------------------------------------------------
#cups class

cups = ["/home/billy/Documents/Research/data/mpeg7/cups", "none"]


#name of .txt file
name = 'cups'

#converting images
cvt.BinaryHistTXT(name, cups)


#
