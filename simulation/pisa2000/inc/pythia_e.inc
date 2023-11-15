c
c	Pythia event Global variables
c
c     Original author: Andrew Rose
c     Creation date: May 31, 1998
c
c	These variables are read from the pythia event file, and 
c	stored in the event header record.
c
      common       /pythia_e/
     1  intr_part_id, intr_part_p,
     2  lpythia, pyth_proc_id, pyth_bjork, pyth_partstu,
     3  pyth_qsqr, pyth_ptrans


      real  	intr_part_p(4,4)
      integer	intr_part_id(4)
      logical   lpythia

      integer	pyth_proc_id		!Process ID
      real	pyth_bjork(2)		!Bjorken's x
      real	pyth_partstu(3)		!partonic s,t,u 
      real	pyth_qsqr		!Q squared
      real	pyth_ptrans		!transverse momentum


