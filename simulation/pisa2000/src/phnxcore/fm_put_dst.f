c     $Id: fm_put_dst.f,v 1.6 2009/02/24 17:34:15 hpereira Exp $

      subroutine fm_put_dst

c    *************************************************************
c    *                                                           *
c    *  FMPUTDST (vsn 1.00)                                      *
c    *                                                           *
c    *  Called by ==> ::                                         *
c    *  IN   ::                                                  *
c    *  OUT  ::                                                  *
c    *                                                           *
c    *  written  by ::  Surender Saini, 18/04/93 12.31.00        *
c    *  modified by ::                                           *
c    *                                                           *
c    *************************************************************

      implicit none
 
c -------------------
#include "udst.inc"
#include "fstore.inc"
#include "guphnx.inc"
#include "sublink.inc"
#include "subevt.inc"
#include "fpmlink.inc"
#include "guevgen.inc"
c --------------------

      integer lk
      integer dcode /11/
            
      if ( cudst_otag_typ .eq. 'PARA') then
              
        call u_put_ds(
     +    ixdiv_fr, lfm_para, 'PISA', 'MUM ',
     +    'PARA', ' ' )
              
        call u_put_ds( 
     +    ixdiv_fr, lfm_paru, 'PISA', 'MUM ',
     +    'PARU', ' ' )
              
        lk=lfm_para
              
        if(root_output.eq.1)then
          call parrootout(dcode, iqf(lk), qf(lk))
        endif
              
      else if (cudst_otag_typ .eq. 'EVNT') then
              
        call u_put_ds(
     +    ixdiv_fe, lfm_cal(1),  'PISA', 'MUM ',
     +    'MCAL',' ')
          
        lk=lfm_cal(1)
        if(
     +    root_output.eq.1 .and. 
     +    lk.gt.0 .and. 
     +    iqf(lk+1).gt.0) then
              
          if(nStageStack.eq.0) then
                
            call dstrootout(
     +        dcode, 0, iqf(lk+1),
     +        iqf(lk+2), qf(lk+2))
                
          endif  ! normal call, not using intra-event staged simulation
                
          if(nStageStack.gt.0.and.nStoreAncestor.gt.0) then
                  
            call dstrootoutspecial(
     +        dcode, nStoreAncestor,
     +        iqf(lk+1), iqf(lk+2), qf(lk+2), storeAncestor(1))
                  
          endif  ! special call,  using intra-event staged simulation
                  
        endif !  check on last subevent or hits > 0  
              
      endif
      return
      end
            
