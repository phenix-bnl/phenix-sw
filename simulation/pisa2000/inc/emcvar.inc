c
c
c	file emcvar.inc
c
c	Arrays used in all (or most) emc analysis routines
c
c     This requires now a huge amount of space (to store cumulated energy
c     in all possible cells)
c
      integer max_pbglz,max_pbgly,max_shkbz,max_shkby
      parameter (max_pbglz = 96)
      parameter (max_pbgly = 48)
      parameter (max_shkbz = 72)
      parameter (max_shkby = 36)
c
      integer i_detmax    ! set in emc_gini.f routine
	parameter (i_detmax = 8)
c
c     We store their DELE and TOF data in a (*,*,12) array, dimensioned
c     such that it can accomodate the largest subdetector.  This wastes
c     some memory, but makes things very simple and transparent to code.
c
c
      integer max_chanz,max_chany,max_subdet,max_overlap
      parameter (max_chanz = 96)
      parameter (max_chany = 48)
      parameter (max_subdet = 8)
	parameter (max_overlap = 4)
      integer emc_trkid(max_overlap,max_chanz,max_chany,max_subdet)
c
      COMMON/EMC_VARS/ emc_trkid
c
c
