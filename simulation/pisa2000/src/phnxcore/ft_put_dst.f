c     $Id: ft_put_dst.f,v 1.4 2009/02/24 17:34:17 hpereira Exp $

      subroutine ft_put_dst

c  to write out the pisa trd tracker data (pads only for now)
c  banks to a zebra fz file srtonse 22-sep-1992

      implicit none
 
#include "udst.inc"
#include "fstore.inc"
#include "sublink.inc"
#include "subevt.inc"
#include "fptlink.inc"
#include "guphnx.inc"
 
C detector parameters:
 
      integer nmplane
      parameter (nmplane = 6)   ! 6 possible TEC planes
      integer ip
      integer lk
      character*4 bnam

      Integer dcode /9/


c     leave 6 position for the names for possible upgrade

      character*4 xe(6) /'XEN1','XEN2','XEN3','XEN4','XEN5','XEN6'/
 

C  at beginning of run write out parameters associated with TRD set

      if (cudst_otag_typ .eq. 'PARA') then

        call u_put_ds(ixdiv_fr,lft_para,'PISA','TRD ','PARA',' ')
        call u_put_ds(ixdiv_fr,lft_paru,'PISA','TRD ','PARU',' ')

      elseif (cudst_otag_typ .eq. 'EVNT') then

c  TEC event data (organized by planes not by PHI sectors)
c  Active Planes are set in the TECGEO routine with Ltec(6) array
c  The Ltec array is saved in the parameter bank array

        do ip = 1,nmplane
          if(iqf(lft_para + 4 + ip) .gt. 0)then
            bnam = xe(ip)
            lk = lft_tec(ip,1)
            
            call u_put_ds(ixdiv_fe,LK,'PISA','TRD ',BNAM,' ')
            
            if(
     +        root_output.eq.1 .and. 
     +        lk.gt.0 .and.   
     +        iqf(lk+1).gt.0)then
      
              call dstrootout(
     +          dcode, ip, iqf(lk+1),
     +          iqf(lk+2), qf(lk+2))
              
            endif  ! check on last subevent or hits > 0          
          endif   ! check on active plane
        end do
      endif
      return
      end
