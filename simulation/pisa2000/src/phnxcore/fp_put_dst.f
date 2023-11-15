c     $Id: fp_put_dst.f,v 1.4 2009/02/24 17:34:16 hpereira Exp $

      subroutine fp_put_dst
      implicit none

c     pc2/pc3 zebra data structure output
c     original author: charles f. maguire,  march 29, 1996
 
#include "udst.inc"
#include "fstore.inc"
#include "guphnx.inc"
#include "sublink.inc"
#include "subevt.inc"
#include "fpplink.inc"

      integer dcode /7/        ! wbs order, after kin and pri
      integer isector

c detector parameters:
      integer nmchmb            ! pc chambers
      parameter (nmchmb = 8)    ! fix at 8 (cfm 3/30/94 for ucdr)
 
 

c  ...and names.. (these should be declared only in one place, not declared
c   locally in n places)

      integer ic, lk
      character*4 bnam
      character*4  nmpads(8),  nmpadt(8)    ! ucdr version
      data nmpads /'PDS1', 'PDS2', 'PDS3', 'PDS4', 'PDS5', 'PDS6',
     1  'PDS7', 'PDS8'/
      data nmpadt /'PDT1', 'PDT2', 'PDT3', 'PDT4', 'PDT5', 'PDT6',
     1  'PDT7', 'PDT8'/
        
      ! at beginning of run write out parameters associated with PAD set
      if (cudst_otag_typ .eq. 'PARA') then
              
        call u_put_ds(ixdiv_fr,lfp_para,'PISA','PAD ','PARA',' ')
        call u_put_ds(ixdiv_fr,lfp_paru,'PISA','PAD ','PARU',' ')
        
      elseif (cudst_otag_typ .eq. 'EVNT') then
        
        ! Pad2 event data
        do ic = 1,nmchmb
          bnam = nmpads(ic)
          lk = lfp_pd2(ic,1)
          
          call u_put_ds(ixdiv_fe,LK,'PISA','PAD ',BNAM,' ')
          
          if(
     +      root_output.eq.1 .and. 
     +      lk.gt.0 .and.   
     +      iqf(lk+1).gt.0) then
                
            isector = ic + 100
            call dstrootout(
     +        dcode, isector, iqf(lk+1),
     +        iqf(lk+2), qf(lk+2))
                  
          endif  ! check if ROOT output requested
        end do

        ! Pad 3 event data
        do ic = 1,nmchmb
          bnam = nmpadt(ic)
          lk = lfp_pd3(ic,1)
          call u_put_ds(ixdiv_fe,LK,'PISA','PAD ',BNAM,' ')
                
          if(
     +      root_output.eq.1 .and. 
     +      lk.gt.0 .and.   
     +      iqf(lk+1).gt.0) then
                 
            isector = ic + 200
            call dstrootout(
     +        dcode, isector, iqf(lk+1),
     +        iqf(lk+2), qf(lk+2))
                  
          endif  ! check if sector has hits or last subevent
        end do
      endif
            
      return
      end
