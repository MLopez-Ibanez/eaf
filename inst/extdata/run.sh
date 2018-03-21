#!/bin/sh
# Reproduce the examples described in
#
#   Manuel Lopez-Ibanez, Luis Paquete, and Thomas Stutzle. Exploratory
#   analysis of stochastic local search algorithms in biobjective
#   optimization. In EMAA, pages X-Y, publisher, 2009.
#
# The directory containing the programs eaf, eafplot.pl and eafdiff.pl
# must be in your PATH. For example, to link the programs to ~/bin 
#
##  mkdir -p ~/bin
##  ln -s eaftools/eaf/eaf ~/bin/eaf
##  ln -s eaftools/eafplot/eafplot.pl ~/bin/eafplot.pl
##  ln -s eaftools/eafplot/eafdiff.pl ~/bin/eafdiff.pl
#
# and uncomment the following line:
#
#PATH=$PATH:~/bin/
#
# You may add the above line to your startup scripts (~/.profile,
# ~/.bashrc, ~/.shrc or similar) to make its effect permanent.
#
error ()
{
    echo "$0: example \`$@\' FAILED!"
    exit 1
}

verbose ()
{
    echo "==== EXAMPLE: $@"
    eval $@ || error $@
    echo "====================================================================="
}

verbose 'eafplot.pl example1_dat'

verbose 'eafplot.pl --best --median --worst --percentiles=25,75 r15_dat r50_dat r200_dat'

verbose 'eafdiff.pl --full --left="Algorithm 1" ALG_1_dat --right="Algorithm 2" ALG_2_dat'

verbose 'eafdiff.pl --left="Algorithm 1" ALG_1_dat --right="Algorithm 2" ALG_2_dat'

verbose eafplot.pl n75_dat

verbose eafplot.pl p75_dat

verbose 'eafdiff.pl --left="WRoTS, l=100, w=10" --right="WRoTS, l=10, w=100" wrots_l100w10_dat wrots_l10w100_dat'

verbose 'eafdiff.pl --left="TPLS" --right="Restart" tpls rest'

# Cleanup 
rm -f Rplots.ps *.R *.med *.eaf *.best *.diff *.worst att_*.p??
