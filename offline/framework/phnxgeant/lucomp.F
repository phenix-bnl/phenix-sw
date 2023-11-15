C
C  this is a dummy routine to make our linker happy. The original subroutine 
C  lives in jetset (in /cern/pro/src/mclibs/fritiof/jetset and
C  /cern/pro/src/mclibs/jetset/jetset, the codes are different - aarghs)
C  If we use the original, then we will have to add either libjetset or 
C  libfritiof to our cern library lists and I really do not want that
C
 
      FUNCTION LUCOMP(KF)
      print *,'You are not supposed to call LUCOMP'
      STOP
      RETURN
      END
 
