<img src="man/figures/ALG_1_dat-ALG_2_dat.png" width="455" height="242" alt= "EAF
   differences between two bi-objective optimizers" title= "EAF differences
   between two bi-objective optimizers" style= "border:0; align:right; float:right;"/>
<img src="man/figures/eafdiff-color.png" width="480"
   height="240" alt="EAF differences between two variants of W-RoTS (color)"
   title="EAF differences between two variants of W-RoTS (color)"
   style="border:0; align:right; float:right; clear:right"/>
   
**eaf**: Empirical Attainment Function (EAF) Tools
================================================================

[![CRAN Status](https://www.r-pkg.org/badges/version-last-release/eaf)](https://cran.r-project.org/package=eaf) [![CRAN Downloads](https://cranlogs.r-pkg.org/badges/grand-total/eaf)](https://CRAN.R-project.org/package=eaf) [![Travis Build
Status](https://travis-ci.org/MLopez-Ibanez/eaf.svg?branch=master)](https://travis-ci.org/MLopez-Ibanez/eaf)

[ [**Homepage**](http://lopez-ibanez.eu/eaftools) ]  [ [**GitHub**](https://github.com/MLopez-Ibanez/eaf) ]

**Maintainer:** [Manuel López-Ibáñez](http://lopez-ibanez.eu)

**Contributors:**
    [Manuel López-Ibáñez](http://lopez-ibanez.eu),
    [Marco Chiarandini](http://www.imada.sdu.dk/~marco),
    [Carlos M. Fonseca](http://eden.dei.uc.pt/~cmfonsec),
    [Luís Paquete](http://eden.dei.uc.pt/~paquete),
    [Thomas Stützle](http://iridia.ulb.ac.be/~stuetzle),
    and Mickaël Binois.
    
---------------------------------------

Introduction
============

The empirical attainment function (EAF) describes the probabilistic
distribution of the outcomes obtained by a stochastic algorithm in the
objective space. This [R](https://www.R-project.org/) package implements plots
of summary attainment surfaces and differences between the first-order
EAFs. These plots may be used for exploring the performance of stochastic local
search algorithms for biobjective optimization problems and help in identifying
certain algorithmic behaviors in a graphical way.

The corresponding [book chapter](#LopPaqStu09emaa) [1] explains the use of
these visualization tools and illustrates them with examples arising from
practice.

In addition, the package provides functions for computing several quality metrics, such as hypervolume, IGD, IGD+, and epsilon.

**Keywords**: empirical attainment function, summary attainment surfaces, EAF
differences, multi-objective optimization, bi-objective optimization,
performance measures, performance assessment, graphical analysis,
visualization.

**Relevant literature:**

 1. <a name="LopPaqStu09emaa"/>Manuel López-Ibáñez, Luís Paquete, and Thomas Stützle. [Exploratory Analysis of Stochastic Local Search Algorithms in Biobjective Optimization](https://doi.org/10.1007/978-3-642-02538-9_9). In T. Bartz-Beielstein, M. Chiarandini, L. Paquete, and M. Preuss, editors, *Experimental Methods for the Analysis of Optimization Algorithms*, pages 209–222. Springer, Berlin, Germany, 2010.<br>
    (This chapter is also available in a slightly extended form as [Technical Report TR/IRIDIA/2009-015](http://iridia.ulb.ac.be/IridiaTrSeries/IridiaTr2009-015r001.pdf)).<br>
    [ [bibtex](http://lopez-ibanez.eu/LopezIbanez_bib.html#LopPaqStu09emaa) |
    doi: [10.1007/978-3-642-02538-9_9](https://doi.org/10.1007/978-3-642-02538-9_9)
    | [Presentation](http://lopez-ibanez.eu/doc/gecco2010moworkshop.pdf) ]


Download and installation
-------------------------

The software is implemented as an R package, but the code for only computing
the EAF is available as a C program, and it does not require installing R or
any R packages. Just [download the package source
code](https://cran.r-project.org/package=eaf), uncompress it, and look for the
directory `src/eaf`. This code can be used to implement your own visualizations
instead of the visualizations provided by the **eaf** package. Compiled binaries
can be found under `system.file(package="eaf", "bin")`. Other useful
binaries can be found there.

The visualization of the EAFs requires installing the **eaf** package. Therefore,
a basic knowledge of R is recommended to make use of all features. However, the
`eaf` package contains two Perl scripts that allow to generate standard plots
without any R knowledge. See `inst/scripts/eafplot/` and
`inst/scripts/eafdiff/` in the package source code. The scripts use the **eaf** package internally to generate the plots, and, hence, the **eaf** package must be installed and working.

The first step before installing the **eaf** package is to [install R](https://cran.r-project.org/). Once R is installed in the system, there are two methods for installing the **eaf** package:

 1. Install within R (automatic download, internet connection required). Invoke
    R, then
    
    ```r
        install.packages(c("modeltools", "eaf"))
    ```
    
 2. [Download the **eaf** package from CRAN](https://cran.r-project.org/package=eaf) (you may also need to download and install
    first the [package modeltools](https://cran.r-project.org/package=modeltools)), and invoke at the command-line:
    
    ```bash
        R CMD INSTALL <package>
    ```
    where `<package>` is one of the three versions available: `.tar.gz` (Unix/BSD/GNU/Linux), `.tgz` (MacOS X), or `.zip` (Windows).

Search the [R documentation](https://cran.r-project.org/faqs.html) if you need more help to install an R package on your system.

If you wish to be notified of bugfixes and new versions, please subscribe to the [low-volume emo-list](https://lists.dei.uc.pt/mailman/listinfo/emo-list), where announcements will be made.

[ [Download **eaf** package from CRAN](https://cran.r-project.org/package=eaf) ]  [ [Documentation](http://lopez-ibanez.eu/eaftools) ]  [ [Development version (GitHub)](https://github.com/MLopez-Ibanez/eaf) ]


Usage
-----

Once the **eaf** package is installed, the following R commands will give more information:
```r
    library(eaf)
    ?eaf
    ?eafplot
    ?eafdiffplot
    ?read.data.sets
    example(eafplot)
    example(eafdiffplot) # This one takes some time
```

Apart from the main R package, the source code contains the following extras in
the directory `inst/` (after installation, these files can be found at the
directory printed by the R command `system.file(package="eaf")`):

 * `scripts/eafplot` : Perl script to plot summary attainment surfaces.
 * `scripts/eafdiff` : Perl script to plot the differences between the EAFs of two input sets.
 * `extdata/` : Examples of utilization of the above programs. These are discussed in the corresponding [book chapter](#LopPaqStu09emaa) [1].

In addition, the source code contains the following under `src/`:
 * `src/eaf` : This C program computes the empirical attainment function in 2 dimensions. It is NOT required by the other programs, but it is provided as a useful command-line utility. This version is based on the original code written by Carlos M. Fonseca available at http://www.tik.ee.ethz.ch/pisa/. Another version of the code that handles three dimensions is available at [Prof. Fonseca's website](http://eden.dei.uc.pt/~cmfonsec/software.html#aft).
 * `src/mo-tools` : Several tools for working with multi-objective data.
 
For more information, consult the `README` files at each subdirectory.


Python
------

Thanks to [rpy2](https://rpy2.github.io/doc/latest/html/index.html), you can
use the `eaf` package from Python. A complete example would be:

```python
import os
import subprocess
import sys
os.environ['RPY2_CFFI_MODE'] = "API" # bug in cffi 1.13.0 https://bitbucket.org/rpy2/rpy2/issues/591/runtimeerror-found-a-situation-in-which-we

# Tested with rpy2 2.9.2-1
import numpy as np
from rpy2.robjects.packages import importr
from rpy2.robjects import r as R
from rpy2.robjects import numpy2ri
numpy2ri.activate()
from rpy2.interactive import process_revents
process_revents.start()

eaf = importr("eaf")
path = R('system.file(package="eaf", "extdata")')[0] + "/"
alg1 = eaf.read_data_sets_(path + "ALG_1_dat")
alg1 = np.asarray(alg1)
alg2 = np.asarray(eaf.read_data_sets_(path + "ALG_2_dat"))

eaf.eafplot(alg1[:, 0:2], sets=alg1[:,2])

input("Press ENTER to see next plot: ")

eaf.eafdiffplot(alg1, alg2, title_left="A", title_right="B")
```

License
--------

This software is Copyright (C) 2011-2019 Carlos M. Fonseca, Luís Paquete, Thomas
Stützle, Manuel López-Ibáñez and Marco Chiarandini.

This program is free software (software libre); you can redistribute it and/or
modify it under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your option)
any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE. See the [GNU General Public License](http://www.gnu.org/licenses/gpl.html) for more details.

**IMPORTANT NOTE**: Please be aware that the fact that this program is released
as Free Software does not excuse you from scientific propriety, which obligates
you to give appropriate credit! If you write a scientific paper describing
research that made substantive use of this program, it is your obligation as a
scientist to (a) mention the fashion in which this software was used in the
Methods section; (b) mention the algorithm in the References section. The
appropriate citation is:

 * Manuel López-Ibáñez, Luís Paquete, and Thomas Stützle. **Exploratory Analysis of Stochastic Local Search Algorithms in Biobjective Optimization.** In T. Bartz-Beielstein, M. Chiarandini, L. Paquete, and M. Preuss, editors, *Experimental Methods for the Analysis of Optimization Algorithms*, pages 209–222. Springer, Berlin, Germany, 2010.  doi: 10.1007/978-3-642-02538-9_9

Moreover, as a personal note, I would appreciate it if you would email
`manuel.lopez-ibanez@manchester.ac.uk` with citations of papers referencing
this work so I can mention them to my funding agent and tenure committee.
