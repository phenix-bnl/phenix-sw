*CMZ :  2.04/00 22/06/93  14.08.32  by  Surender Saini
*-- Author :    Surender Saini   23/04/93
 
      subroutine ugtrcut(itrmed,tr_cuts)
c
c    *************************************************************
c    *                                                           *
c    *  UGTRCUT (vsn 1.00) change tracking_medium cuts           *
c    *                                                           *
c    *  Called by ==> ::  < user > , < mumater >                 *
c    *  IN   :: itrmed, tr_cuts                                  *
c    *  OUT  :: none                                             *
c    *                                                           *
c    *  written  by ::  Surender Saini, 23/04/93 20.06.26        *
c    *  modified by ::                                           *
c    *                                                           *
c    *************************************************************
c
c tr_cuts(9) => cutgam,cutele,cutneu,cuthad,cutmu,
c             cutebrm,cutmbr,cutedr,cutmdr
 
      real*4 itrmed, tr_cuts(*)
 
      character*6 chcutn(9)
      data chcutn/'CUTGAM','CUTELE','CUTNEU','CUTHAD','CUTMUO',
     +  'BCUTE','BCUTM','DCUTE','DCUTM'/
 
 
      do j = 1 , 9
       if( tr_cuts(j) .ne. 0.0)then
          call gstpar(itrmed,chcutn(j),tr_cuts(j))
       end if
      end do
 
      return
      end
