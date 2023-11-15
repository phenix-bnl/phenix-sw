      SUBROUTINE ZEBRA_INIT(NZEB, QUIET)

C.    ******************************************************************
C.    *                                                                *
C.    *       Routine to initialise ZEBRA store (//)                   *
C.    *                                                                *
C.    *       Reimplemented from MZEBRA()                                                         *
C.    *                                                                *
C.    ******************************************************************

#include "geant321/gcbank.inc"
#include "geant321/gcunit.inc"
#include "geant321/gclink.inc"

      write( *,* ) 'ZEBRA_INIT'
      IF ( QUIET .EQ. 0 ) THEN

C.    ------------------------------------------------------------------
C.normal (verbose) mode.
C.    ------------------------------------------------------------------

        CALL MZEBRA(-1)
      ELSE

C.    ------------------------------------------------------------------
C.     quiet mode
C.     we 'brute force' redirect the LOUT logical unit to /dev/null 
C.     to remove the (anoying) geant initialization output from the 
C.    stdout.
C.    ------------------------------------------------------------------
      LOUT=66
        OPEN (UNIT=LOUT, FILE='/dev/null', STATUS='OLD')
        CALL MZEBRA(-3)

      ENDIF
      write( *,* ) 'ZEBRA_INIT - mzebra done'

      NZEBRA=NZEB
      CALL MZSTOR(IXSTOR,'/GCBANK/',' ',FENDQ,LQ,LR1,WS,LQ(KWWORK+100)
     +  ,LQ(NZEBRA-30))
       
      write(  *,* ) 'ZEBRA_INIT - mzstor done'
      
      RETURN
      END
