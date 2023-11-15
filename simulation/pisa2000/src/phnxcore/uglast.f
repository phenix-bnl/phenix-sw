*CMZ :  2.04/00 09/04/93  14.31.01  by  S.R.Tonse
*-- Author :
*-- Author :
c     dummy entry points for detector sub-systems  April 26, 1992
c     revised for Release 2
C     Revised for Release 2.1: S.R.Tonse 31st March 1993.
C     An indication is given when one of these dummy routines is entered
C     but only for the first call of the routine. output to unit 6.
c
      SUBROUTINE uglast
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      IF(FIRST)THEN
        WRITE(6,*)' UGLAST: <I> Dummy Routine Entered '
        FIRST = .FALSE.
      END IF
      RETURN
      END
