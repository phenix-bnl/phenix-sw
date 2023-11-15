*CMZ :  2.04/00 22/12/94  09.35.46  by  H. Kehayias
*CMZ :  2.03/00 22/06/93  14.02.21  by  Surender Saini
*-- Author :    Surender Saini   16/04/93
 
      subroutine uvolattr(chvnam, icolvol)
c
c    *************************************************************
c    *                                                           *
c    *  UVOLATTR (vsn 1.00)  :: Sets volume attributes           *
c    *                                                           *
c    *  Called by ==> ::  <USER> , Geometry routines             *
c    *  IN   :: chvnam, icolvol                                  *
c    *  OUT  :: none                                             *
c    *                                                           *
c    *  written  by ::  Surender Saini, 16/04/93 13.58.09        *
c    *  modified by ::                                           *
c    *                                                           *
c    *************************************************************
c
      integer       icolvol
      character*4   chvnam
 
c
 
      if(icolvol .gt. 0)then
        call gsatt( chvnam, 'SEEN', 1)
        call gsatt( chvnam, 'COLO', icolvol)
      else
        call gsatt( chvnam, 'SEEN', 0)
      end if
 
      return
      end
