      LOGICAL FUNCTION KILCON()
      implicit none

c     Function will check that there is no end-of-run request

#include "gcflag.inc"

c     Check if there is an IEORUN or IEOTRI set (error condition GUEVGEN)

      if(ieorun.eq.1.or.ieotri.eq.1)then
           write(6,1)ievent-1,ieorun,ieotri
1       format(/,3x,'KILCON <I>: after event',i8,' found end-of-run ',
     1  'flag at',i2,' and end-of-trigger flag at',i2,/)
           KILCON = .true.
           return
      endif

c     Check for   endit.dat  file for quick kill of the run;
c     if non-existent, continue processing the triggers.

      open(unit=16,file='endit.dat',status='old',form='formatted',
     1          err=5)
      close(unit=16)
      write(6,2)ievent-1
2     format(/,3x,'KILCON <I>: after event',i8,' found  endit.dat ',
     1' file.',/,3x,'Please delete before the next run of PISA',/)
      KILCON  = .true.
      return
5     continue
      KILCON = .false.
      return
      end
