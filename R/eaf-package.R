#'Plots of the Empirical Attainment Function
#'
#'  The empirical attainment function (EAF) describes the probabilistic
#'  distribution of the outcomes obtained by a stochastic algorithm in the
#'  objective space. This package implements plots of summary
#'  attainment surfaces and differences between the first-order
#'  EAFs. These plots may be used for exploring the performance of
#'  stochastic local search algorithms for biobjective optimization
#'  problems and help in identifying certain algorithmic behaviors in a
#'  graphical way. 
#'
#' Functions:
#'
#'\tabular{rl}{
#'\code{\link{eafdiffplot}} \tab  Empirical attainment function differences\cr
#'\code{\link{eafplot}} \tab  Plot the Empirical Attainment Function for two
#'objectives\cr
#'\code{\link{read.data.sets}}\tab  Read several data.frame sets\cr
#'}
#'
#'Data:
#'
#'\describe{
#'\item{\code{\link{gcp2x2}}}{  Metaheuristics for solving the Graph Vertex Coloring Problem}
#'\item{\code{\link{HybridGA}}}{  Results of Hybrid GA on vanzyl and Richmond
#'water networks}
#'\item{\code{\link{SPEA2minstoptimeRichmond}}}{ Results of SPEA2 when minimising electrical cost and maximising the
#'minimum idle time of pumps on Richmond water network}
#'}
#' 
#' Extras are available at \code{file.path(system.file(package="eaf"))}:
#'
#'\tabular{ll}{
#'\code{extdata}        \tab  External data sets (see \code{\link{read.data.sets}})\cr
#'\code{scripts/eaf}    \tab  EAF command-line program\cr
#'\code{scripts/eafplot}\tab  Perl script to generate plots of attainment surfaces\cr
#'\code{scripts/eafdiff}\tab  Perl script to generate plots of EAF differences\cr
#'}
#'
#' @import graphics grDevices stats
#'
#' @useDynLib eaf, .registration = TRUE
#'
#' @aliases eaf
#' 
#' @author
#' Maintainer:  Manuel \enc{López-Ibáñez}{Lopez-Ibanez}
#' \email{manuel.lopez-ibanez@ulb.ac.be}
#'
#'  Contributors: Carlos Fonseca, Luis Paquete, Thomas \enc{Stützle}{Stuetzle}, 
#'  Manuel \enc{López-Ibáñez}{Lopez-Ibanez} and Marco Chiarandini.
#'
#' @references
#'  V. Grunert da Fonseca, C. M. Fonseca, and A. O. Hall, \emph{Inferential
#'  performance assessment of stochastic optimisers and the attainment
#'  function}, in Evolutionary Multi-Criterion Optimization. First
#'  International Conference, EMO 2001 (E. Zitzler, K. Deb, L. Thiele,
#'  C. A. Coello Coello, and D. Corne, eds.), vol. 1993 of Lecture Notes
#'  in Computer Science, pp. 213-225, Berlin: Springer, 2001.
#'
#'  V. Grunert da Fonseca and C. M. Fonseca, \emph{The attainment-function
#'  approach to stochastic multiobjective optimizer assessment and
#'  comparison}. In T. Bartz-Beielstein, M. Chiarandini, L. Paquete, and
#'  M. Preuss, editors, Experimental Methods for the Analysis of
#'  Optimization Algorithms, pages 103-130, Springer, Berlin, Germany,
#'  2010.
#'  
#'  M. \enc{López-Ibáñez}{Lopez-Ibanez}, L. Paquete, and T. \enc{Stützle}{Stuetzle}. \emph{Exploratory
#'  Analysis of Stochastic Local Search Algorithms in Biobjective
#'  Optimization}. In T. Bartz-Beielstein, M. Chiarandini, L. Paquete, and
#'  M. Preuss, editors, Experimental Methods for the Analysis of
#'  Optimization Algorithms, pages 209–222. Springer, Berlin, Germany,
#'  2010. doi: 10.1007/978-3-642-02538-9_9
#'  
#'@keywords package graphs
#'@concept multivariate
#'@concept optimize
#'@concept time-quality algorithm profile
#'@concept empirical attainment function
#'
#'@examples
#'data(gcp2x2)
#'tabucol<-subset(gcp2x2, alg!="TSinN1")
#'tabucol$alg<-tabucol$alg[drop=TRUE]
#'eafplot(time+best~run,data=tabucol,subset=tabucol$inst=="DSJC500.5")
#'
#'eafplot(time+best~run|inst,groups=alg,data=gcp2x2)
#'eafplot(time+best~run|inst,groups=alg,data=gcp2x2,
#'	percentiles=c(0,50,100),include.extremes=TRUE,
#'	cex=1.4, lty=c(2,1,2),lwd=c(2,2,2),
#'        col=c("black","blue","grey50"))
#'
#'A1<-read.data.sets(file.path(system.file(package="eaf"),"extdata","ALG_1_dat"))
#'A2<-read.data.sets(file.path(system.file(package="eaf"),"extdata","ALG_2_dat"))
#'eafplot(A1,A2, percentiles=c(50))
#'eafplot(list(A1=A1, A2=A2), percentiles=c(50))
#'eafdiffplot(A1, A2)
#'## Save to a PDF file
#'# dev.copy2pdf(file="eaf.pdf", onefile=TRUE, width=5, height=4)
#'
"_PACKAGE"
#> [1] "_PACKAGE"


#' Results of Hybrid GA on vanzyl and Richmond water networks
#'
#' The data has the only goal of providing an example of use of eafplot.
#'
#'@format
#'  A list with two data frames, each of them with three columns, as
#'  produced by \code{\link{read.data.sets}}.
#'  \describe{
#'    \item{\code{$vanzyl}}{data frame of results on vanzyl network}
#'    \item{\code{$richmond}}{data frame of results on Richmond
#'      network. The second column is filled with \code{NA}}
#'  }
#' 
#'@source Manuel \enc{López-Ibáñez}{Lopez-Ibanez}. Operational Optimisation of Water Distribution
#'  Networks. PhD thesis, School of Engineering and the Built Environment,
#'  Edinburgh Napier University, UK, 2009.
#'
#' @examples
#'data(HybridGA)
#'print(HybridGA$vanzyl)
#'print(HybridGA$richmond)
#'
"HybridGA"

#'Results of SPEA2 when minimising electrical cost and maximising the
#'minimum idle time of pumps on Richmond water network.
#'
#'The data has the only goal of providing an example of use of eafplot.
#'
#'@format 
#'  A data frame as produced by \code{\link{read.data.sets}}. The second
#'  column measures time in seconds and corresponds to a maximisation problem.
#'
#' @source
#'  Manuel \enc{López-Ibáñez}{Lopez-Ibanez}. Operational Optimisation of Water Distribution
#'  Networks. PhD thesis, School of Engineering and the Built Environment,
#'  Edinburgh Napier University, UK, 2009.
#'
#'@examples 
#'data(HybridGA)
#'data(SPEA2minstoptimeRichmond)
#'SPEA2minstoptimeRichmond[,2] <- SPEA2minstoptimeRichmond[,2] / 60
#'eafplot (SPEA2minstoptimeRichmond, xlab = expression(C[E]),
#'         ylab = "Minimum idle time (minutes)",
#'         las = 1, log = "y", maximise = c(FALSE, TRUE))
#'
"SPEA2minstoptimeRichmond"

#' Results of SPEA2 with relative time-controlled triggers on Richmond water
#' network.
#'
#' The data has the only goal of providing an example of use of eafplot.
#'
#'@format 
#'  A data frame as produced by \code{\link{read.data.sets}}.
#'
#' @source
#'  Manuel \enc{López-Ibáñez}{Lopez-Ibanez}. Operational Optimisation of Water Distribution
#'  Networks. PhD thesis, School of Engineering and the Built Environment,
#'  Edinburgh Napier University, UK, 2009.
#'
#'@examples 
#'data(HybridGA)
#'data(SPEA2relativeRichmond)
#'eafplot (SPEA2relativeRichmond, percentiles = c(25, 50, 75),
#'         xlab = expression(C[E]), ylab = "Total switches",
#'         xlim = c(90, 140), ylim = c(0, 25),
#'         extra.points = HybridGA$richmond, extra.lty = "dashed",
#'         extra.legend = "Hybrid GA")
#'
"SPEA2relativeRichmond"

#'Results of SPEA2 with relative time-controlled triggers on Vanzyl's
#'water network.
#'
#'The data has the only goal of providing an example of use of eafplot.
#'
#'@format 
#'  A data frame as produced by \code{\link{read.data.sets}}.
#'
#'@source
#'  Manuel \enc{López-Ibáñez}{Lopez-Ibanez}. Operational Optimisation of Water Distribution
#'  Networks. PhD thesis, School of Engineering and the Built Environment,
#'  Edinburgh Napier University, UK, 2009.
#'
#'@examples 
#'data(HybridGA)
#'data(SPEA2relativeVanzyl)
#'eafplot(SPEA2relativeVanzyl, percentiles = c(25, 50, 75), 
#'        xlab = expression(C[E]), ylab = "Total switches", xlim = c(320, 400),
#'        extra.points = HybridGA$vanzyl, extra.legend = "Hybrid GA")
#'
"SPEA2relativeVanzyl"

#' Metaheuristics for solving the Graph Vertex Coloring Problem
#'
#'  Two metaheuristic algorithms, TabuCol (Hertz et al., 1987) and
#'  simulated annealing (Johnson et al., 1991), to find a good
#'  approximation of the chromatic number of two random graphs. The data
#'  here has the only goal of providing an example of use of eafplot for
#'  comparing algorithm performance with respect to both time and quality
#'  when modelled as two objectives in trade off.
#'
#' @format 
#'  A data frame with 3133 observations on the following 6 variables.
#'  \describe{
#'    \item{\code{alg}}{a factor with levels \code{SAKempeFI} and \code{TSinN1}}
#'    \item{\code{inst}}{a factor with levels \code{DSJC500.5} and
#'      \code{DSJC500.9}. Instances are taken from the DIMACS repository.}
#'    \item{\code{run}}{a numeric vector indicating the run to
#'  which the observation belong. }
#'    \item{\code{best}}{a numeric vector indicating the best solution in
#'  number of colors found in the corresponding run up to that time.}
#'    \item{\code{time}}{a numeric vector indicating the time since the
#'  beginning of the run for each observation. A rescaling is applied.}
#'    \item{\code{titer}}{a numeric vector indicating iteration number
#'  corresponding to the observations.}
#'  }
#'
#'@details
#'  Each algorithm was run 10 times per graph registering the time and
#'  iteration number at which a new best solution was found. A time limit
#'  corresponding to 500*10^5 total iterations of TabuCol was imposed. The
#'  time was then normalized on a scale from 0 to 1 to make it instance
#'  independent.
#'
#'@source M. Chiarandini (2005). Stochastic local search methods for highly
#'   constrained combinatorial optimisation problems. Ph.D. thesis, Computer
#'   Science Department, Darmstadt University of Technology, Darmstadt,
#'   Germany. page 138.
#'
#'@references 
#'  A. Hertz and D. de Werra. Using Tabu Search Techniques for Graph
#'  Coloring. Computing, 1987, 39(4), 345-351.
#'
#'  D.S. Johnson, C.R. Aragon, L.A. McGeoch and C. Schevon. Optimization
#'  by Simulated Annealing: An Experimental Evaluation; Part II, Graph
#'  Coloring and Number Partitioning. Operations Research, 1991, 39(3),
#'  378-406
#'
#'@examples 
#' data(gcp2x2)
#'
"gcp2x2"
