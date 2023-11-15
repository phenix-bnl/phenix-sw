c     $Id: fw_put_dst.f,v 1.7 2009/02/24 17:34:17 hpereira Exp $

      subroutine fw_put_dst

c    *************************************************************
c    *                                                           *
c    *  fw_put_dst  For TFW hits output                          *
c    *                                                           *
c    *                                                           *
c    *  original  by ::  C.F. Maguire, August 16, 2006           *
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
#include "fpwlink.inc"
c --------------------
      integer lk
      integer dcode /26/  ! used as switch in the dstrootout function in the rootPISA.cc file

      INTEGER N_SUBVOLUMES,ISUB
      PARAMETER (N_SUBVOLUMES=8)
      
      logical PRINT /.false./

      if(cvolu_opt(2,12).eq.'P_ID' .and. cvolu_opt(4,12).eq.'ELEM')then

C  at beginning of run write out parameters associated with TFW 
        if (cudst_otag_typ .eq. 'PARA') then

          if(print)then
            write(6,1)
 1          format(/,' fw_put_dst: call for para and paru output')
          endif ! check print switch

          call u_put_ds(ixdiv_fr,lfw_para,'PISA','TFWR','PARA',' ')
          call u_put_ds(ixdiv_fr,lfw_paru,'PISA','TFWR','PARU',' ')
        
        elseif (cudst_otag_typ .eq. 'EVNT') then

c event data written to DST

          call u_put_ds(ixdiv_fe,lFW_TFW(1),
     +      'PISA','TFWR','TFW1',' ')
          
          if(root_output.eq.1)then

            do isub = 1, n_subvolumes
              lk = lFW_TFW(isub)
              if(
     +          lk.gt.0 .and.   
     +          iqf(lk+1).gt.0)then

                call dstrootout(
     +            dcode, 0, 
     +            iqf(lk+1), iqf(lk+2), qf(lk+2))
                    
              endif  ! safety check on ZEBRA link value
            enddo ! loop over subvolumes of TFW
          endif !  check on root output           

        endif ! check on check on para or evnt call
      endif  ! check on elem output
 
      return
      end
