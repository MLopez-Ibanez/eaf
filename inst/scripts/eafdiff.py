#!/usr/bin/python3 
from rpy2.robjects.packages import importr
import numpy as np
from rpy2.robjects import r as R
from rpy2.robjects import numpy2ri
numpy2ri.activate()

eaf = importr("eaf")
path = R('system.file(package="eaf")')[0] + "/extdata/"
alg1 = eaf.read_data_sets_(path + "wrots_l100w10_dat")
#alg1 = np.array(alg1).transpose()
alg2 = eaf.read_data_sets_(path + "wrots_l10w100_dat")
x = eaf.eafdiff(alg1, alg2)
print(x)
