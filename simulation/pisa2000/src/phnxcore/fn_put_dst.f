c     $Id: fn_put_dst.f,v 1.5 2009/02/24 17:34:16 hpereira Exp $

      subroutine fn_put_dst

c    *************************************************************
c    *                                                           *
c    *  FNPUTDST (vsn 1.00)                                      *
c    *                                                           *
c    *  Called by ==> ::                                         *
c    *  IN   ::                                                  *
c    *  OUT  ::                                                  *
c    *                                                           *
c    *  written  by ::  Surender Saini, 18/04/93 12.58.32        *
c    *  modified by ::                                           *
c    *                                                           *
c    *************************************************************
C   Created 25-Sep-1992  by GAP.
C   To write out the PISA Moun identifier output zebra
C   banks to a Zebra FZ file
 
      implicit none
c --------------------
#include "udst.inc"
#include "fstore.inc"
#include "guphnx.inc"
#include "subevt.inc"
#include "sublink.inc"
#include "fpnlink.inc"
#include "guevgen.inc"

c --------------------

      integer lk
      integer dcode /12/
       
      if (cudst_otag_typ .eq. 'PARA') then
        
        call u_put_ds(ixdiv_fr,lfn_para,'PISA','MUN ','PARA',' ')
        lk=lfn_para
        
        if(root_output.eq.1)then
          call parrootout(dcode, iqf(lk+2), qf(lk+2))
        endif

      else if (cudst_otag_typ .eq. 'EVNT') then
        call u_put_ds(ixdiv_fe,lfn_cal(1),'PISA','MUN ','NCAL',' ')
        
        lk=lfn_cal(1)
        if(
     +    root_output.eq.1 .and. 
     +    lk.gt.0 .and.   
     +    iqf(lk+1).gt.0)then
               
          if(nStageStack.eq.0) then 
            call dstrootout(
     +        dcode, 0, iqf(lk+1),
     +        iqf(lk+2), qf(lk+2))
          endif ! normal call, not using intra-event staged simulations
                
          if(nStageStack.gt.0.and.nStoreAncestor.gt.0) then
                  
            call dstrootoutspecial(
     +        dcode, nStoreAncestor,
     +        iqf(lk+1), iqf(lk+2), qf(lk+2), storeAncestor(1))
                  
          endif          ! special call,  using intra-event staged simulation
        endif !  check on last subevent or hits > 0  
      endif
 
      return
      end
