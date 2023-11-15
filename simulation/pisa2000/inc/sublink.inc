c
c     maximum number of subevents for the link pointers
c
c   Revision History
c  C.F. Maguire      January 26, 1998   Change from 100 to 200
c                                       More albedo generation
c
c  C.F. Maguire      February 9, 2000   Change from 200 to 6000 to accommodate ZDC
c                                       NOTE: these link pointers have become
c                                       obsolete for ROOT-in-PISA version.  There
c                                       would have to be a major clean-up of the
c                                       user code to get rid of them, however.
c
 
      integer maxsub
      parameter (maxsub = 6000)
