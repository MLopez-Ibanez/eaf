#!/usr/bin/perl -w
#---------------------------------------------------------------------
#
# eafdiff.pl $Revision: 252 $
#
#---------------------------------------------------------------------
#
# Copyright (c) 2007-2014
# Manuel Lopez-Ibanez <manuel.lopez-ibanez@manchester.ac.uk>
# LaTeX: Manuel L{\'o}pez-Ib{\'a}{\~n}ez
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You can obtain a copy of the GNU General Public License at
# http://www.gnu.org/licenses/gpl.html or writing to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
# USA
#
#---------------------------------------------------------------------
#
# IMPORTANT NOTE: Please be aware that the fact that this program is
# released as Free Software does not excuse you from scientific
# propriety, which obligates you to give appropriate credit!  If you
# write a scientific paper describing research that made substantive
# use of this program, it is your obligation as a scientist to (a)
# mention the fashion in which this software was used in the Methods
# section; (b) mention the algorithm in the References section. The
# appropriate citation is: 
#
#    Manuel López-Ibáñez, Luís Paquete, and Thomas Stützle.
#    Exploratory Analysis of Stochastic Local Search Algorithms in
#    Biobjective Optimization. In T. Bartz-Beielstein, M. Chiarandini, 
#    L. Paquete, and M. Preuss, editors, Experimental Methods for the
#    Analysis of Optimization Algorithms, pages 209–222. Springer, 
#    Berlin, Germany, 2010.
#    doi: 10.1007/978-3-642-02538-9_9
#
# Moreover, as a personal note, I would appreciate it if you would
# email <manuel.lopez-ibanez@manchester.ac.uk> with citations of papers
# referencing this work so I can mention them to my funding agent and
# tenure committee.
#
#---------------------------------------------------------------------
#
# Literature:
#
# [1] Manuel Lopez-Ibanez, Luis Paquete, and Thomas Stutzle. Hybrid
#     population-based algorithms for the bi-objective quadratic
#     assignment problem.  Journal of Mathematical Modelling and
#     Algorithms, 5(1):111-137, April 2006.
#
# [2] Manuel Lopez-Ibanez, Luis Paquete, and Thomas Stutzle. Hybrid
#     population-based algorithms for the bi-objective quadratic
#     assignment problem.  Journal of Mathematical Modelling and
#     Algorithms, 5(1):111-137, April 2006.
# 
#---------------------------------------------------------------------
#
# TODO: 
#
# * Fail if any subcommand fails.
#
#---------------------------------------------------------------------
use strict;
use warnings FATAL => 'all';
use diagnostics;
use File::Basename;
use Getopt::Long qw(:config pass_through);
use File::Temp qw/ tempfile /;

my $copyright = '
Copyright (C) 2007-2017  Manuel Lopez-Ibanez <manuel.lopez-ibanez@manchester.ac.uk>
This is free software.  You may redistribute copies of it under the terms of
the GNU General Public License <http://www.gnu.org/licenses/gpl.html>.
There is NO WARRANTY, to the extent permitted by law.
';

my $progname = basename($0);
my $version = ' $Revision: 252 $ ';

my $Rexe = "R"; 

my $ps2epsfilter = undef;
$ps2epsfilter = "ps2eps"; ## Comment this out if you do not want ps2eps.
my $eps2png = "convert"; # this is only required if --png is given
my $pdf2png = "convert";

requiredprog($Rexe);

# Old default
#my $colors = '"#FFFFFF", "#BFBFBF","#808080","#404040","#000000"';
my $colors = '"#808080","#000000"';
my $intervals = 5;
my $compress_flag = 0;
my $flag_xmaximise = 0;
my $flag_ymaximise = 0;
my $flag_fulleaf = 0;
my $flag_area;
my $flag_eps = 0;
my $flag_crop = (which_program("pdfcrop") ne "pdfcrop");
my $jpeg_flag = 0;
my $flag_png  = 0;
my $flag_verbose = 0;
my $flag_attsurfs = 1;
my $filter = "";
my $legendpos;
my $xlim = "NULL";
my $ylim = "NULL";
my $label_left;
my $label_right;
my $label_obj1 = "objective 1";
my $label_obj2 = "objective 2";
my $output_file;
my $output_dir = "";
my $overwrite = 1;
my $scale = 1;
&usage(1) if (@ARGV == 0);

## Format commandline.
my $commandline = &format_commandline ();

GetOptions ('eps' => \$flag_eps,
            'png' => \$flag_png,
            'crop' => \$flag_crop,
            'attsurfs!' => \$flag_attsurfs,
            'output|o=s'   => \$output_file,
            'output-dir=s' => \$output_dir,
            'obj1:s'   => \$label_obj1,
            'obj2:s'   => \$label_obj2,
            'scale=f' => \$scale,
            'points' => sub { $flag_area = 0; },
            'colors=s' => \$colors,
            'intervals=i' => \$intervals,
            'full'   => sub { $flag_fulleaf = 1;
                              $flag_area = 1 unless (defined($flag_area)); },
            'verbose|v!'    => \$flag_verbose,
## Silently accept these options for backwards compatibility.
            'area' => \$flag_area,,
            'save-temps' => \$flag_verbose);

requiredprog($ps2epsfilter) if ($flag_eps);
requiredprog($eps2png) if ($flag_png and $flag_eps);
requiredprog($pdf2png) if ($flag_png and not $flag_eps);
requiredprog("pdfcrop") if ($flag_crop and not $flag_eps);

sub usage {
    my $exitcode = shift;
    print<<"EOF";
$progname: version $version
$copyright
Usage: $progname FILE1 FILE2
Create a plot of the differences between the EAFs of FILE1 and FILE2.

 -h, --help          print this summary and exit
     --version       print version number and exit
 -v, --verbose       increase verbosity and keep intermediate files

     --full          plot the full EAF instead of the differences
     --points        plot points instead of areas

     --left=STRING   label for left plot
     --right=STRING  label for right plot
     --obj1=STRING   label for objective 1 (x-axis)
     --obj2=STRING   label for objective 2 (y-axis)
                     (labels can be R expressions,
                      e.g., --obj1="expression(pi)")

     --legendpos=none|{top,bottom}{left,right} no legend or position of the legend

     --xlim=REAL,REAL  limits of x-axis
     --ylim=REAL,REAL  limits of y-axis

     --maximise      handle a maximisation problem
     --xmaximise     maximise first objective
     --ymaximise     maximise second objective

     --[no]attsurfs  do not add attainment surfaces to the plot
     --scale REAL    scale the plot so fonts become larger

     --colors=STRING,STRING  colors for 50\% and 100\% difference (default: '"#808080","#000000"')
     --intervals=INT number of intervals (default: 5)

 -o  --output=FILE   output file
 -z  --gzip          compress the output file (adds gz extension)
     --png           generate also png file from the eps output
                     (requires ImageMagick)
     --eps           generate EPS file (default is PDF)
EOF
    exit $exitcode;
}

my @files = ();
## Handle parameters
while (@ARGV) {
    my $argv = shift @ARGV;

    if ($argv =~ /^--left=/
           or $argv =~ /^--left$/) {
        $label_left = &get_arg ($argv);

    } elsif ($argv =~ /^--right=/
           or $argv =~ /^--right$/) {
        $label_right =  &get_arg ($argv);

    } elsif ($argv =~ /^--legendpos=/
           or $argv =~ /^--legendpos$/) {
        my $arg = &get_arg ($argv);
        $legendpos  = "$arg" if ($arg);
    }
    elsif ($argv =~ /^--xlim=/
           or $argv =~ /^--xlim$/) {
        my $arg = &get_arg ($argv);
        $xlim  = "c($arg)" if ($arg);
    }
    elsif ($argv =~ /^--ylim=/
           or $argv =~ /^--ylim$/) {
        my $arg = &get_arg ($argv);
        $ylim  = "c($arg)" if ($arg);
    }
    elsif ($argv =~ /--maxim/) {
        $flag_xmaximise = 1;
        $flag_ymaximise = 1;
    }
    elsif ($argv =~ /--xmax/) {
        $flag_xmaximise = 1;
    }
    elsif ($argv =~ /--ymax/) {
        $flag_ymaximise = 1;
    }
    elsif ($argv =~ /^-z$/ or $argv =~ /^--gzip$/) {
        requiredprog ("gzip");
        $compress_flag = 1;
    }
    ## The remainder options are standard.
    elsif ($argv =~ /^--help/ or $argv =~ /^-h/) {
        &usage(0);
    }
    elsif ($argv =~ /^--version/) {
        print "$progname: version $version\n";
        print $copyright;
        exit (0);
    }
    elsif ($argv =~ /^--/ or $argv =~ /^-/) {
        print "$progname: unknown option $argv\n";
        &usage(1);
    } else {
        push (@files, $argv);
    }
}

unless (@files == 2) {
    print "$progname: you must specify two input files.\n";
    print "$progname: try '$progname --help' for more information.\n";
    exit 1;
}

my $file1 = $files[0];
die "$progname: error: cannot read $file1\n" unless (-r $file1);
my $file2 = $files[1];
die "$progname: error: cannot read $file2\n" unless (-r $file2);
$output_dir .= "/" if ($output_dir and not($output_dir =~ m|/$|));
my $outsuffix = ($flag_eps) ? ".eps" : ".pdf";
unless (defined($output_file) and $output_file) {
    $output_file  = $output_dir . basename($file1)."-".basename($file2);
    $output_file = (($flag_fulleaf) 
                    ? "${output_file}_full$outsuffix" : "${output_file}$outsuffix");
}
if ($output_file !~ m/$outsuffix$/) {
    $output_file =~ s/(\.pdf|\.eps|\.ps|\.png)?$/$outsuffix/;
}

die "$progname: error: $output_file already exists.\n" if (-e $output_file
                                                           and not $overwrite);

$filter = "|$ps2epsfilter -s b0 -q -l -B -O -P > " if ($flag_eps
                                                       and defined($ps2epsfilter)
                                                       and -x $ps2epsfilter);
unless (defined($legendpos)) {
    $legendpos = ($flag_fulleaf) ? "bottomleft" : "topright";
}

$label_left = basename($file1) unless (defined($label_left) and $label_left);
$label_right= basename($file2) unless (defined($label_right) and $label_right);
$flag_area = 1 unless (defined($flag_area));


$label_left = parse_expression ($label_left);
$label_right = parse_expression ($label_right);
$label_obj1 = parse_expression ($label_obj1);
$label_obj2 = parse_expression ($label_obj2);

#print "$progname: generating plot $output_file ...\n";

my $Rfile = "$$.R";
open(R, ">$Rfile") or die "$progname: error: can't open $Rfile: $!\n";
print R <<"EOFR";
#!/usr/bin/r --vanilla
# 
# R script to plot the differences
#
# This script is executable if you have littler installed. [*]
# [*] http://code.google.com/p/littler/
#
# Input: 
#        label1    = label for first plot
#        label2    = label for second plot
#        output_file = filename for output plot
#        legendpos = location of the legend or "" for no legend.
#
# Created by $commandline
#
# $version
#

library(eaf)

col <- c("#FFFFFF", $colors)
intervals <- $intervals
filter <- "$filter"
file.left <- "$file1"
file.right <- "$file2"
title.left  <- $label_left
title.right <- $label_right
output.file  <- "$output_file"
legend.pos <- "$legendpos"
maximise <- c(${flag_xmaximise}, ${flag_ymaximise})
xlab <- $label_obj1
ylab <- $label_obj2
Xlim <- $xlim
Ylim <- $ylim
full.eaf <- as.logical(${flag_fulleaf})
eaf.type <- ifelse(${flag_area}, "area", "point")
flag.eps <- as.logical(${flag_eps})
flag.attsurfs <- as.logical(${flag_attsurfs})
scale <- $scale
EOFR

print R <<'EOFR';


data.left <- read.data.sets (file.left)
data.right <- read.data.sets (file.right)
xlim <- range(data.left[,1], data.right[,1])
ylim <- range(data.left[,2], data.right[,2])

cat(sprintf("xlim = c(%s, %s)\n", xlim[1], xlim[2]))
cat(sprintf("ylim = c(%s, %s)\n", ylim[1], ylim[2]))
if (!is.null(Xlim)) xlim <- Xlim
if (!is.null(Ylim)) ylim <- Ylim

output.title <- output.file
output.file <- paste(filter, output.file, sep="")
if (flag.eps) {
  postscript(output.file, title = output.title,
             paper = "special", horizontal=F, onefile=F,
             width = 9 / scale, height = 6 / scale, family = "Helvetica")
} else {
  pdf(output.file, title = output.title,
      width = 9 / scale, height = 6 / scale, family = "Helvetica")
}
eafdiffplot (data.left, data.right, col = col, intervals = intervals,
             full.eaf = full.eaf, type = eaf.type, legend.pos = legend.pos,
             title.left = title.left, title.right = title.right,
             cex = 1.0, cex.lab = 1.1, cex.axis = 1.0,
             xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab,
             maximise = maximise,
             percentiles = if(flag.attsurfs) 50 else NA,
             grand.lines = flag.attsurfs)
dev.off()
cat (paste("eafdiffplot:", output.file, "\n"))

EOFR

close R;

my @args = ("$Rexe --quiet --vanilla --slave < $$.R");
system(@args) == 0
or die "$progname: error: R failed to create the plots (@args)\n";

if ($flag_crop and not $flag_eps) {
    my ($tmpfh, $tmpfilename) = tempfile();
    execute ("pdfcrop $output_file $tmpfilename");
    execute ("mv $tmpfilename $output_file");
}
# FIXME: EPS files tend to be huge, so sometimes pays off to convert
# them to PNG or JPEG and then back to EPS. How to fix this:
#
# Option 1: Find the proper settings of 'convert' or an alternative
# tool to do the best possible conversion. Multi-platform tools are
# better. Choose either PNG or JPEG. PNG seems the best option.
#
# Option 2: Create the output of R directly in PNG format. Then,
# conversion to EPS seems easier and pdfLaTeX handles PNG. Still, the
# proper settings for PNG would need to be investigated.
#
# Option 3: create smaller EPS files, so no conversion is
# needed. Currently, points overlapped by other points are still
# present in the EPS file and take space (and time to render). Avoid
# anything that is not shown in final plot. If the points are at
# exactly the same position, we should be able to detect that and only
# plot the point that is visible. However, a point can also be
# completely overlapped by other points in different positions (since
# points in a plot have a nonzero area). No idea how to avoid plotting
# those points. Perhaps there is some way to detec this in R itself.
#
#
if ($flag_png) {
    my $output_png = $output_file;
    $output_png =~ s/${outsuffix}$/.png/;
    #$output_png =~ s[/eps/][/png/];
    if ($flag_eps) {
        `$eps2png -render +antialias -density 300 -background white -flatten $output_file $output_png`;
    } else {
        my $tmpfile = "/tmp/". basename($output_file);
        `pdfcrop $output_file $tmpfile`;
        `$pdf2png -render +antialias -density 300 -background white -flatten $tmpfile $output_png`;
    }
    #my $output_png_eps = $output_png;
    #my $output_png_eps =~ s/\.png$/_png.eps/;
    #`convert $output_png $output_png_eps`;
    #`rm -f  $output_png`;
    #`gzip --force -v $output_png_eps`;
}
#                 if($jpeg_flag) {
#                     $output_jpg = $output_file;
#                     $output_jpg =~ s/\.eps$/.jpg/;
#                     $output_jpg =~ s[/eps/][/jpg/];
#                     $output_jpg_eps = $output_jpg;
#                     $output_jpg_eps =~ s/\.jpg$/_jpg.eps/;

#                     `convert -render +antialias -density 300 $output_eps $output_jpg`;
#                     `convert $output_jpg -compress jpeg eps2:${output_jpg_eps}`;
#                     `rm -f  $output_jpg`;
#                     `gzip --force -v $output_jpg_eps`;
#                 }

## FIXME: Do this on the fly within the R script by using pipes.
if ($compress_flag) {
    print "$progname: compressing: ";
    `gzip --force -v $output_file`;
}

unless ($flag_verbose) {
    unlink("$$.R", "$$.Rout");
} else {
    print "$progname: generated R script: $$.R\n";
}



###################################
# helper sub-routines
###################################

sub parse_expression {
    my $label = shift;
    $label = "\"$label\"" if ($label !~ /^expression\(/);
    return $label;
}

sub get_arg {
    my ($option, $arg) = split (/=/, $_[0], 2);
    $arg = shift @ARGV if (not $arg);
    return $arg;
}

sub format_commandline {
    my $cmd = $0 . " ";
    for (my $i=0, my $j=25; $i < @ARGV; $i++) {
        if ($i == $j) {
            $j += 25;
            $cmd .= "\\\n# ";
        }
        if ($ARGV[$i] =~ /\s/) {
            $cmd .= " \"" . $ARGV[$i] . "\"";
        } else {
            $cmd .= " " . $ARGV[$i];
        }
    }
    return $cmd;
}

use Env '@PATH';
use Cwd;
# FIXME: This should return the empty string if not found.
sub which_program {
    my $program = shift;
    my $cwd = cwd();
    unless ($program =~ m|^.?.?/|) {
        # If no path was given
        foreach my $path (@PATH) {
            if (-e "$path/$program") {
                $program = "$path/$program";
                last;
            }
        }
        # Try also the current directory:
        $program = "$cwd/$program" if (-e "$cwd/$program");
    }
    return $program;
}

sub requiredprog {
    my $cwd = cwd();

    foreach my $program (@_)
    {
        next if not defined $program;
        my $pathtoprogram = which_program($program);
        die "$progname: cannot find required program `$program',"
        ." either in your PATH=\"". join(':',@PATH)
        ."\" or in the current directory`$cwd'\n"
        unless ($pathtoprogram =~ m|^.?.?/|);
        
        die "$progname: required program `$program' is not executable\n"
        unless (-x $pathtoprogram);
    }
}

sub execute {
    my $command = shift;

    if ($flag_verbose) {
        print $command ."\n";
        print `$command`;
    } else {
        `$command`;
    }
    my $exit = $? >> 8;
    die "$progname: command `$command' failed with value $exit\n" if ($exit);
}
