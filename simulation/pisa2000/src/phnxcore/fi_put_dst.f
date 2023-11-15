c     $Id: fi_put_dst.f,v 1.4 2009/02/24 17:34:15 hpereira Exp $

      subroutine fi_put_dst

C  to write out the PISA Itermediate tracker output zebra
C banks to a Zebra FZ file.

      implicit none

#include "udst.inc"
#include "fstore.inc"
#include "guphnx.inc"
#include "sublink.inc"
#include "fpilink.inc"
#include "subevt.inc"


c     local variables


      integer igroup,icell,iplane,jplane,ikey

c     This routine outputs both the DC and PC1

      integer dcodeDCH /5/   ! WBS order, after KIN and PRI
      integer iarm
      integer dcodePC  /7/   ! WBS order, after KIN and PRI

C detector parameters:

      integer nmchmb
      integer ic,lk
      character*4 bnam

C  ...and names..

c 1996 PC1 geometry names
      character*4 nmpix96(16),nmgas96(16)
      data nmpix96 /'P101', 'P102', 'P103', 'P104', 'P105', 'P106',
     +'P107', 'P108', 'P109', 'P110', 'P111', 'P112', 'P113', 'P114',
     +'P115', 'P116' /

      data nmgas96 /'ZZ01', 'ZZ02', 'ZZ03', 'ZZ04', 'ZZ05', 'ZZ06',
     +'ZZ07', 'ZZ08', 'ZZ09', 'ZZ10', 'ZZ11', 'ZZ12', 'ZZ13', 'ZZ14',
     +'ZZ15', 'ZZ16' /

 
c EXECUTABLE STATEMENTS

      if (cvolu_opt(4,4).eq.'TRKS') then

C     at beginning of run write out parameters associated with ITR
        if (cudst_otag_typ.eq.'PARA') then

          call u_put_ds(ixdiv_fr,lfi_para,'PISA',
     +      'ITR ','PARA',' ')
          call u_put_ds(ixdiv_fr,lfi_dcgp,'PISA',
     +      'ITR ','DCGP',' ')

          if(root_output.eq.1)then
            lk = lfi_para  ! contains last change dates of DC and PC1
            call parrootout(-dcodeDCH, iqf(lk+2), qf(lk+2))
            lk = lfi_dcgp 
            call parrootout(dcodeDCH, iqf(lk+2), qf(lk+2))
          endif
            
        elseif (cudst_otag_typ.eq.'EVNT') then

c     write event data to DST

          if (cvolu_opt(1,4).eq.'IT96') then   ! 1996 DC/PC1 geometry
          
          lk = lfi_dcaw(1)
          call u_put_ds(ixdiv_fe,lk,'PISA','ITR ','DCAW',' ')
          
          lk = lfi_dcae(1)
          call u_put_ds(ixdiv_fe,lk,'PISA','ITR ','DCAE',' ')
          
          if(root_output.eq.1)then
            lk = lfi_dcaw(1)   !  West Arm of Dch, arm=0?
            iarm = 0
            if(lk.gt.0 .and. iqf(lk+1).gt.0)then
                    
              call dstrootout(
     +          dcodeDCH, iarm, iqf(lk+1),
     +          iqf(lk+2), qf(lk+2))
                    
              endif !  check if last subevent or hits > 0
            endif  ! check if ROOT output requested

            if(root_output.eq.1)then
              lk = lfi_dcae(1)   !  East Arm of Dch, arm=1?
              iarm = 1
              if(lk.gt.0 .and. iqf(lk+1).gt.0) then
                      
                call dstrootout(
     +            dcodeDCH, iarm, iqf(lk+1),
     +            iqf(lk+2), qf(lk+2))
                      
              endif !  check if last subevent or hits > 0
                    
            endif  ! check if ROOT output requested

C  PC1 pixel
            do ic=1,16
              bnam = nmpix96(ic)
              lk = lfi_pad(ic,1)
              call u_put_ds(ixdiv_fe,lk,'PISA','ITR ',bnam,' ')
              if( 
     +          root_output.eq.1 .and. 
     +          lk.gt.0 .and. 
     +          iqf(lk+1).gt.0) then
                    
                call dstrootout(dcodePC, ic, iqf(lk+1),
     +            iqf(lk+2), qf(lk+2))
              endif ! check if last subevent or hits > 0
            enddo

          elseif (cvolu_opt(1,4).eq.'PC1N') then ! no DC, new PC

            do ic = 1,16
              bnam = nmpix96(ic)
              lk = lfi_pad(ic,1)
              call u_put_ds(ixdiv_fe,lk,'PISA','ITR ',bnam,' ')
            
              if(
     +          root_output.eq.1 .and. 
     +          lk.gt.0 .and. 
     +          iqf(lk+1).gt.0)then
                  
                call dstrootout(
     +            dcodePC, ic, iqf(lk+1),
     +            iqf(lk+2), qf(lk+2))
                      
              endif ! check if last subevent of hits > 0
            enddo
          endif               ! cvolu_opt
        endif                  ! cuds_otag_typ
      endif                     ! cvolu_opt

      return
      end
