#!/usr/bin/perl -w
#---------------------------------------------------------------------
#
# eafplot.pl  ($Revision: 251 $)
#
#---------------------------------------------------------------------
#
# Copyright (c) 2005-2017
# Manuel Lopez-Ibanez  <manuel.lopez-ibanez@manchester.ac.uk>
# TeX: \copyright 2005-2017  Manuel L{\'o}pez-Ib{\'a}{\~n}ez
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
# * Fail if any subcommand fails. For example, do not even run R if
#   the call to the eaf program failed. (See execute in eafdiff.pl).
#
#---------------------------------------------------------------------
use strict;
use warnings FATAL => 'all';
use diagnostics;
use Carp;
use File::Basename;
use Getopt::Long qw(:config pass_through);;
use File::Temp qw/ tempfile /;

my $copyright ='
Copyright (C) 2005-2017  Manuel Lopez-Ibanez <manuel.lopez-ibanez@manchester.ac.uk>
This is free software.  You may redistribute copies of it under the terms of
the GNU General Public License <http://www.gnu.org/licenses/gpl.html>.
There is NO WARRANTY, to the extent permitted by law.
';

my $progname = basename($0);
my $version = ' $Revision: 251 $ ';

my $Rexe = "R";
requiredprog($Rexe);


&usage(1) if (@ARGV == 0);

my $colors = "NULL";
my $percentiles;
# FIXME: single/multiple plots are a mess. This is how it should work:
# --single file1 file2 ... --output plotfile
#      produces a single plot 'plotfile' from all input files. Labels are input filenames
#
# --no-single file --output plotfile
#      produces a single plot 'plotfile' from input file. Labels are percentiles.
#
# --no-single file1 file2  --output plotfile
#      error: use --suffix and/or --output-dir.
my $flag_single_plot = 0;
my $legend;
my $output_file;
my @output_files;
my $flag_verbose = 0;
my $flag_eps = 0;
my $flag_crop = (which_program("pdfcrop") ne "pdfcrop");
my $label_obj1 = "objective 1";
my $label_obj2 = "objective 2";
my $flag_area = 0;
my $flag_ymaximise = 0;
my $flag_xmaximise = 0;
my $flag_maximise = 0;
my $flag_best = 0;
my $flag_median = 0;
my $flag_worst = 0;

GetOptions ('area' => \$flag_area,
            'eps' => \$flag_eps,
            'crop' => \$flag_crop,
            'single' => \$flag_single_plot,
            'best|b' => \$flag_best,
            'median|m' => \$flag_median,
            'worst|w' => \$flag_worst,
            'maximise' => \$flag_maximise,
            'xmaximise' => \$flag_xmaximise,
            'ymaximise' => \$flag_ymaximise,
            'output|o=s'   => \$output_file,
            'legend=s' => \$legend,
            'colors=s' => \$colors,
            'obj1:s'   => \$label_obj1,
            'obj2:s'   => \$label_obj2,
            'verbose|v' => \$flag_verbose);


requiredprog("pdfcrop") if ($flag_crop and not $flag_eps);

if ($flag_best) {
    $percentiles .= ", " if (defined($percentiles));
    $percentiles .= "0";
}
if ($flag_median) {
    $percentiles .= ", " if (defined($percentiles));
    $percentiles .= "50";
}
if ($flag_worst) {
    $percentiles .= ", " if (defined($percentiles));
    $percentiles .= "100";
}

sub version {
    print "$progname: version $version\n";
    print $copyright;
    exit (0);
}

sub usage {
    my $exitcode = shift;
    print <<"EOF";
$progname version $version
$copyright
Usage: $progname [OPTIONS] FILES

  Plot the best/median/worst attainment surfaces for a set of input
  points.

 -h, --help          print this summary and exit
     --version       print version number and exit

 -v, --verbose      increase verbosity and keep intermediate files

 -I, --iqr, --IQR     plot IQR (25%-75% percentile) instead of best and
                      worst
 -b, --best           plot best attainment surface
 -m, --median         plot median attainment surface
 -w, --worst          plot worst attainment surface
 -p, --percentiles=INT[,INT..] 
                      plot the given percentile(s) of the EAF

     --extra=FILE   add extra points to the plot

     --extra-label=STRING  label in the legend for the extra points

     --obj1=STRING   label for objective 1 (x-axis)
     --obj2=STRING   label for objective 2 (y-axis)
                     (labels can be R expressions, 
                      e.g., --obj1="expression(pi)")

     --legend=STRING A legend like: '"Algorithm A", "Algorithm B"'

     --legendpos={top,bottom}{left,right}  position of the legend

     --xlim=REAL,REAL  limits of x-axis
     --ylim=REAL,REAL  limits of y-axis

     --area         plot the area dominated by the attainment surfaces instead
                    of lines.
                    
     --colors=STRING,STRING  bounds for color range in area mode (default '"gray","black"')

     --maximise      handle a maximisation problem
     --xmaximise     maximise first objective
     --ymaximise     maximise second objective

 -o, --output=FILE  output file.

     --[no]single   generate a single plot.

     --eps          generate EPS file (default is PDF).

    --no-eaf        just plot the points directly.
EOF
    exit $exitcode;
}

## Format commandline.
my $commandline = &format_commandline ();

my @files = ();
my $flag_axislog = "";
my $extra_points = "NULL";
my $extra_labels = "NULL";
my $IQR_flag = 0;
my $legendpos = "topright";
my $xlim = "NULL";
my $ylim = "NULL";
my $do_eaf = 1;

#FIXME: Move everything to GetOpt above.
## Handle parameters
while (@ARGV) {
    my $argv = shift @ARGV;

    if ($argv =~ /^--help/ or $argv =~ /^-h/) {
        &usage(0);
    } elsif ($argv =~ /^--version/) {
        &version();
    } elsif (# For backwards compatibility
             $argv =~ /--stoptime/ or $argv =~ /-stoptime/) {
        print "$progname: maximising y-axis\n";
        $flag_ymaximise = 1;
    }
    elsif ($argv =~ /^-I/ or $argv =~ /^--iqr/i) {
        $IQR_flag = 1;
        $percentiles .= ", " if (defined($percentiles));
        $percentiles .= "25, 50, 75";
    }
    elsif ($argv =~ /^-p$/ or $argv =~ /^--percentiles=/
           or $argv =~ /^--percentiles$/) {
        $percentiles .= ", " if (defined($percentiles));
        $percentiles .= &get_arg($argv);
    }
    elsif ($argv =~ /--no-eaf/i) {
        $do_eaf = 0;
    }
    elsif ($argv =~ /^--extra=/
           or $argv =~ /^--extra$/) {
        my $arg = &get_arg ($argv);
        if ($arg and -r $arg) {
            $extra_points  = ($extra_points eq "NULL") 
            ?  "\"$arg\"" : $extra_points . ", \"$arg\"";
        } else {
            die "cannot read file $arg.\n";
        }
    }
    elsif ($argv =~ /^--extra-label=/
           or $argv =~ /^--extra-label$/) {
        my $arg = &get_arg ($argv);
        if ($arg) {
            $extra_labels  = ($extra_labels eq "NULL") 
            ?  "\"$arg\"" : $extra_labels . ", \"$arg\"";
        }
    } elsif ($argv =~ /^--legendpos=/
           or $argv =~ /^--legendpos$/) {
        my $arg = &get_arg ($argv);
        $legendpos  = "$arg" if ($arg);
    }
    elsif ($argv =~ /^--ylog/) {
        $flag_axislog = "y";
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
    } elsif ($argv =~ /^--/ or $argv =~ /^-/) {
        print "$progname: unknown option $argv\n";
        &usage(1);
    } else {
        push (@files, $argv);
    }
}

@files = &unique(@files);
die "$progname: error: no input files given\n" unless (@files);

if (defined($percentiles) and not $do_eaf) {
    die "$progname: cannot specify --no-eaf and any EAF option" .
    " (--best, --median, --worst, --iqr, --percentiles)";
}


my $ps2epsfilter = undef;
$ps2epsfilter = "ps2eps";
requiredprog ($ps2epsfilter) if ($flag_eps);
my $filter = "";
$filter = "|$ps2epsfilter -s b0 -q -l -B -O -P > " if ($flag_eps
                                                       and defined($ps2epsfilter)
                                                       and -x $ps2epsfilter);

if ($extra_labels eq "NULL") {
    $extra_labels = $extra_points;
}

$percentiles = "NULL" unless ($do_eaf);
# Default is best, median, worst.
unless(defined($percentiles)) {
    $percentiles = ($flag_single_plot) ? "50" : "0, 50, 100";
}

$label_obj1 = parse_expression ($label_obj1);
$label_obj2 = parse_expression ($label_obj2);


my $num = 0;
my $eaffiles = "";
my @titles;
my @eaffiles;
foreach my $inpfile (@files) {
    unless (-r $inpfile) {
	die "$progname: warning: $inpfile: cannot read file.\n";
        #next;
    } elsif (!(-s $inpfile)
             or `grep -v -e "#\\|^\$" $inpfile | wc --bytes` =~ /\s+0$/o) {
	die "$progname: warning: $inpfile: empty file.\n";
        #next;
    }

    $num++;
    my $basefile;
    my $inpdir;
    my $file_eps;
    chomp($basefile = `basename $inpfile .dat`);
    chomp($inpdir = `dirname $inpfile`);

    my $filedata  = $inpfile;
    if ($num < 10) { print "# f$num: $inpfile\n"; }
    else           { print "#f$num: $inpfile\n"; }

    if ($do_eaf) {
        if ($IQR_flag) {
            $file_eps = "$inpdir/att_iqr_${basefile}";
        } elsif ($flag_area) {
            $file_eps = "$inpdir/att_area_${basefile}";
        } else {
            $file_eps = "$inpdir/${basefile}_att";
        }
    } else {
        $file_eps = "$inpdir/${basefile}";
    }
    $eaffiles = "\"${filedata}\"";
    push @output_files, $file_eps;
    push @titles, $inpfile;
    push @eaffiles, $eaffiles;
}

# FIXME: We can override only one output file.
if (defined($output_file) and $output_file) {
    # Remove suffix, the R code will add the correct one.
    $output_file =~ s/(\.pdf|\.eps|\.ps|\.png)$//;
    $output_files[0] = $output_file;
}



my $str_outfiles = asRlist(@output_files);
my $str_titles = asRlist(@titles);
my $str_eaffiles = join(', ', @eaffiles);
$legend = $str_titles unless(defined($legend));

if ($flag_maximise) {
    $flag_xmaximise = 1;
    $flag_ymaximise = 1;
}

print "$progname: maximising x-axis\n" if ($flag_xmaximise);
print "$progname: maximising y-axis\n" if ($flag_ymaximise);


my $Rfile = "$$.R";
open(R, ">$Rfile") or die "$progname: error: can't open $Rfile: $!\n";
print R <<"EOFR";
#!/usr/bin/r --vanilla
# 
# R script to plot attainment surfaces
#
# This script is executable if you have littler installed. [*]
# [*] http://code.google.com/p/littler/
#
# Created by $commandline
#
# $version
#

library(eaf)

filter <- "$filter"

file.extra <- list(${extra_points})
extra.legend <- c(${extra_labels})
extra.pch <- c(20, 1:25)
extra.col <- c("black")
maximise <- as.logical(c($flag_xmaximise, $flag_ymaximise))
legend.txt <- c($legend)
legend.pos <- "$legendpos"
log <- "$flag_axislog"
Xlim <- $xlim
Ylim <- $ylim
eaf.type <- ifelse(${flag_area}, "area", "point")
xlab <- $label_obj1
ylab <- $label_obj2
colors <- c($colors)
percentiles <- c($percentiles)
percentiles <- if (is.null(percentiles)) percentiles else sort(percentiles)
do.eaf <- as.logical($do_eaf)
single.plot <- as.logical($flag_single_plot)
flag.eps <- as.logical(${flag_eps})
output.files <- c(${str_outfiles})
titles   <- c(${str_titles})
eaffiles <- list(${str_eaffiles})

EOFR

print R <<'EOFR';
if (eaf.type == "area") {
  col <- if (!is.null(colors)) colors else c('grey', 'black')
  # These two are unused for "area"
  lty <- NULL
  pch <- NA
} else {
  col <- c("black", "darkgrey", "black", "grey40", "darkgrey")
  lty <- c("dashed", "solid", "solid", "solid", "dashed")
  pch <- NA
  #lty <- c("blank")
  #pch <- c(20,21,22,23,4,5)
}

data <- NULL
attsurfs <- NULL
xlim <- NULL
ylim <- NULL

if (do.eaf) {
  data <- lapply (eaffiles, read.data.sets)
  xlim <- range(xlim, sapply(data, function(x) x[, 1]))
  ylim <- range(ylim, sapply(data, function(x) x[, 2]))
} else {
  attsurfs <- lapply(eaffiles, function(x) {
                     z <- read.data.sets(x)
                     z <- split.data.frame(z, z$set)
                     z <- lapply(z, function(y) {y$set <- 1; return(y)})
                     return(z)})
  xlim <- range(xlim, sapply(attsurfs, function(x) do.call("rbind", x)[,1]))
  ylim <- range(ylim, sapply(attsurfs, function(x) do.call("rbind", x)[,2]))
  lty <- c("blank")
  pch <- c(20,21,22,23,4,5)
  col <- c("black", "darkgrey", "black", "grey40", "darkgrey")
}

if (is.null (file.extra[[1]])) {
      extra.points <-  NULL
} else {
  extra.points <- list()
  for (i in 1:length(file.extra)) {
     tmp <- read.table(file.extra[[i]], na.strings="NA")[,c(1,2)]
     extra.points[[i]] <- tmp
     xlim <- range(xlim, tmp[,1], na.rm=T)
     ylim <- range(ylim, tmp[,2], na.rm=T)
  }
}

cat(sprintf("xlim = c(%s, %s)\n", xlim[1], xlim[2]))
cat(sprintf("ylim = c(%s, %s)\n", ylim[1], ylim[2]))
if (!is.null(Xlim)) xlim <- Xlim
if (!is.null(Ylim)) ylim <- Ylim

for (k in seq_along(output.files)) {
  if (single.plot) {
    output.title <- output.files
  } else {
    output.title <- titles[k]
  }
  output.file <- output.files[k]
  if (flag.eps) {
      output.file <- paste(filter, output.file, ".eps", sep='')
      postscript(output.file, title = output.title,
                 paper = "special", horizontal=F, onefile=F,
                 width=4.5, height=4.5)
  } else {
      output.file <- paste(filter, output.file, ".pdf", sep='')
      pdf(output.file, title = output.title, width=4.5, height=4.5)
  }
  if (single.plot) {
    eafplot (data, attsurfs = attsurfs, percentiles = percentiles,
             xlab = xlab, ylab = ylab, las = 0, log = log,
             type = eaf.type, lty = lty, col = col, pch=pch, cex.pch=0.75,
             extra.points = extra.points, extra.pch = extra.pch, extra.col = extra.col,
             xlim = xlim, ylim = ylim,
             legend.pos = legend.pos, extra.legend = extra.legend,
             legend.txt = legend.txt, maximise = maximise)
    } else {
      eafplot.default (data[[k]][,1:2], sets = data[[k]][,3],
                       attsurfs = attsurfs[[k]], percentiles = percentiles,
                       xlab = xlab, ylab = ylab, las = 0, log = log,
                       type = eaf.type, lty = lty, col = col, pch=pch, cex.pch=0.75,
                       extra.points = extra.points, extra.pch = extra.pch, extra.col = extra.col,
                       xlim=xlim, ylim=ylim,
                       legend.pos = legend.pos, extra.legend = extra.legend,
                       maximise = maximise)
    }
  dev.null <- dev.off()
  cat ("Plot: ", output.file, "\n", sep='')
}

EOFR
close R;
&execute_verbose ("$Rexe --quiet --vanilla --slave <$Rfile");

($flag_verbose) 
? print "$progname: generated R script: $Rfile\n" 
: unlink($Rfile);

if ($flag_crop and not $flag_eps) {
    my ($tmpfh, $tmpfilename) = tempfile();
    foreach my $outfile (@output_files) {
        execute ("pdfcrop ${outfile}.pdf $tmpfilename");
        execute ("mv $tmpfilename ${outfile}.pdf");
    }
}

exit 0;

###################################
# helper sub-routines
###################################
sub parse_expression {
    my $label = shift;
    $label = "\"$label\"" if ($label !~ /^expression\(/);
    return $label;
}

sub unique {
    my %seen =() ;
    @_ = grep { ! $seen{$_}++ } @_ ;
}
sub round {
    my($number) = shift;
    return int($number + .5 * ($number <=> 0));
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

## See
## http://svn.collab.net/repos/svn/trunk/contrib/hook-scripts/svn-keyword-check.pl
## for a better way to read from a command output

sub runcmd {
    my $command = shift;
    system ($command);

    if ($? == -1) {
        die "error: failed to execute $command: $!\n";
    }
    elsif ($? & 127) {
        die "child died with signal ". ($? & 127) 
        . ($? & 128) ? "with core dump.\n" : ".\n";
    }
    else {
        return $? >> 8;
    }
}

sub execute {
    my $command = shift;

    if ($flag_verbose) {
        &execute_verbose ("$command");
    } else {
        `$command`;
    }
}

sub execute_verbose {
    unless (@_) {
        croak "$progname: execute_verbose passed no arguments.\n";
    }
    print "\n@_\n" if ($flag_verbose);
    my $fh = _pipe(@_);
    my @output;
    while (<$fh>) {
        print;
        chomp;
        push(@output, $_);
    }
    close($fh);
    my $result = $?;
    my $exit   = $result >> 8;
    my $signal = $result & 127;
    my $cd     = $result & 128 ? "with core dump" : "";
    if ($signal or $cd) {
        warn "$progname: pipe from `@_' failed $cd: exit=$exit signal=$signal\n";
    }
    if (wantarray) {
        return ($result, @output);
    } else {
        return $result;
    }
}

# Return the filehandle as a glob so we can loop over it elsewhere.
sub _pipe {
    local *SAFE_READ;
    my $pid = open(SAFE_READ, '-|');
    unless (defined $pid) {
        die "$progname: cannot fork: $!\n";
    }
    unless ($pid) {
        open(STDERR, ">&STDOUT") or die "$progname: cannot dup STDOUT: $!\n";
        exec(@_) or die "$progname: cannot exec `@_': $!\n";
    }
    return *SAFE_READ;
}

sub asRlist {
    return "\"". join("\", \"", @_) . "\"";
}
