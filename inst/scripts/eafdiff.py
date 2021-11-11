#!/usr/bin/python3 
from rpy2.robjects.packages import importr
import numpy as np
from rpy2.robjects import r as R
from rpy2.robjects import numpy2ri
numpy2ri.activate()

eaf = importr("eaf")
path = R('system.file(package="eaf")')[0] + "/extdata/"
alg1 = eaf.read_datasets(path + "wrots_l100w10_dat")
#alg1 = np.array(alg1).transpose()
alg2 = eaf.read_data_sets_(path + "wrots_l10w100_dat")
x = eaf.eafdiff(alg1, alg2)
# numpy.asarray constructs a view.
x = np.asarray(x)

import pandas as pd
data = eaf.read_datasets(path + "ran.10pts.3d.10")
data = np.asarray(data)
df = pd.DataFrame(data, columns = ['obj1','obj2','obj3','run'])
eaf50 = eaf.eafs(df.iloc[:,:3].to_numpy(), sets=df.run.to_numpy(),
                 # It must be type float.
                 percentiles=np.array([50.0]))
# Remove useless last column
eaf50 = np.asarray(eaf50)[:, :3]

def select_side(rectangles, side):
        if (side == "left"):
                rectangles = rectangles[rectangles[:, 4] >= 1, :]
        elif (side == "right"):
                rectangles = rectangles[rectangles[:, 4] <= -1, :]
                rectangles[:, 4] = -rectangles[:,4]
        else:
                raise("Unknown side {}\n".format(side))
        return rectangles


