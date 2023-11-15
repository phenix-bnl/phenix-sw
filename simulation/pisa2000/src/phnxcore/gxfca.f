**
      SUBROUTINE GXFCA(CHFUNC,NCH,JAD,IER)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *        To analize character string CHFUNC of length NCH        *
C.    *        CHFUNC may be the name of a COMIS function              *
C.    *        or a file name                                          *
C.    *                                                                *
C.    ******************************************************************
      COMMON/QUEST/IQUEST(100)
      CHARACTER*(*) CHFUNC
      CHARACTER*32 CHFILE
      INTEGER CSADDR
CTON SRTonse removed all this choice, hardwired instead.
C      CHARACTER*(*) BSLASH
C+SELF,IF=BSLASH.
C      PARAMETER (BSLASH='\\')
C+SELF,IF=-BSLASH.
C      PARAMETER (BSLASH='\\')
C+SELF.
CTON
      CHARACTER*(*) BSLASH
      PARAMETER (BSLASH='\\')
C.
C.    ------------------------------------------------------------------
C.
      JADF=0
      IQUEST(1)=0
  50  JAD=JADF
*
  99  END
