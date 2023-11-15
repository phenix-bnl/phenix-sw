*-- Author :
C
C ZEBRA Store Numbers for the Cave B Analysis Programs
C RELIC FROM THE FOPI PROGRAM BEINGS SAVED FOR NOW
C
C The 'facilities' are denoted by the following letters:
C     U  UTIL  utility functions
C     C  COMM  Common for experiment, e.g. beam and trigger stuff
C     P  PHYS  physics analysis
C
      common   /stosto/ ixstor
C
      integer*4   ixstor(16)
C
      integer*4   ixstor_u
      integer*4   ixstor_c
      integer*4   ixstor_p
      integer*4   ixstor_f
C
      equivalence (ixstor(1), ixstor_u)
      equivalence (ixstor(2), ixstor_c)
      equivalence (ixstor(3), ixstor_p)
      equivalence (ixstor(4), ixstor_f)
