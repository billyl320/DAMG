import scipy.misc as sm
import scipy.ndimage as nd
import numpy as np
import os
import math


#getting all .pgm's and .png's
def GetPicNames( indir ):
    a = os.listdir( indir )
    a = [ x for x in a if "._" not in x ]
    pgmnames= []
    for t in a:
        if '.pgm' in t:
            pgmnames.append( indir + '/' + t )
        if '.png' in t:
            pgmnames.append( indir + '/' + t )
        if '.jpg' in t:
            pgmnames.append( indir + '/' + t )
        if '.gif' in t:
            pgmnames.append( indir + '/' + t )
    return pgmnames

#getting all images as is (for CNN)
def GetAllImagesCNN( dirs ):
    mgs = []
    for d in dirs:
        mgnames = GetPicNames( d )
        k = 0
        for j in mgnames:
            adata = sm.imread(j, flatten=True)
            new = (adata>128) + 0
            new = sm.imresize(new, (120,120) )
            mgs.append( new )
            #data augementation via rotations
            deg=90.0
            turns = (360.0)/(deg+0.0)
    #return mgs
            for i in range(1, int(turns)):
                    temp = nd.rotate(new, deg*i, reshape=True, cval=0.0)
                    temp = (temp >0.5) +0
                    mgs.append( temp )
                    sm.imsave("/home/billy/Documents/Research/2020/DMG/4_20_2020/temp/"+ str(k)+ "_" +str(i) +".png",temp)
            k += 1
    return mgs
