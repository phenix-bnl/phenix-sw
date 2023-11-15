c     $Id: fa_put_dst.f,v 1.6 2009/02/24 17:34:14 hpereira Exp $
      subroutine fa_put_dst

c     this is the aer which is new for run2 (2001)

      implicit none
#include "udst.inc"
#include "fstore.inc"
#include "guphnx.inc"
#include "sublink.inc"
#include "subevt.inc"
#include "fpalink.inc"

      integer lk
      integer dcode /20/
      integer isub  ! sensitive volume index

      integer n_subvolumes
      parameter (n_subvolumes=160)  ! 80 -> 160 (run5, mk)
      character*4 name_subvolumes(n_subvolumes)

      integer n_volume
      integer volume_n
      character*50 name_volu
      
      volume_n = 1000
      do  n_volume=1,n_subvolumes
        volume_n = volume_n + 1
        write(name_volu,*)volume_n
        name_subvolumes(n_volume) = 'A'//name_volu(3:5)
      enddo



      if(cvolu_opt(2,14).eq.'P_ID' .and. cvolu_opt(4,14).eq.'ELEM')then

        ! at beginning of run write out parameters associated with aer 
        if( cudst_otag_typ .eq. 'PARA') then
                
          call u_put_ds(ixdiv_fr,lfa_para,'PISA','AERR','PARA',' ')
                
        elseif (cudst_otag_typ .eq. 'EVNT') then

          ! event data written to DST
          ! 2 sensitive volumes: AER1, AER2
          do isub = 1,n_subvolumes      
               
            lk = lFA_AER(isub,1)
            call u_put_ds(
     +        ixdiv_fe,lk,
     +        'PISA','AERR', 
     +        name_subvolumes(isub),' ')

c           write( *,*) 'fa_put_dst - isub: ', isub
c           write( *,*) 'fa_put_dst - hits: ', iqf(lk+1)
c           write( *,*) 'fa_put_dst - bank: ', lFA_AER(isub,1)
                    
            if(
     +        root_output.eq.1 .and. 
     +        lk.gt.0 .and. 
     +        iqf(lk+1).gt.0 ) then
                  
              call dstrootout(
     +          dcode, 0, 
     +          iqf(lk+1), iqf(lk+2), qf(lk+2))
                    
            endif  ! check on last subevent or hits > 0, first layer
                  
          enddo  ! loop over sensitive volumes
                
        endif ! check on check on para or evnt call
      endif  ! check on elem output

      RETURN
      END
