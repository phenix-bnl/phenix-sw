c $Id: beam_pipe.f,v 1.4 2008/05/21 08:22:04 hpereira Exp $

      subroutine BEAM_PIPE(type)

c C.F. Maguire 21-April-1992
c Modified :: S. Saini  16-JUN-1993

c Beam Pipe plus enclosed vacuum
c main geometry parameter file (phnx.par) segment

c>>Added S. Saini :: 16-JUN-1993
c There are three distinct sections of beam pipe which span the following Z_rang
c Z =-60 cm to +60 cm  ::Beryllium (OD=8 cm, Thickness=0.3% of a radiation lengt
c Z =-200cm to -60 cm &
c Z =+60 cm to +200cm  ::Stainless Steel (OD=8 cm, Thickness = 2mm)
c Z =-800cm to -200cm &
c Z =+200cm to +800cm  ::Stainless Steel (OD=12.70 cm, Thickness = 2.1 cm)
c CP ::: Collision Point , CM :: Central Magnet

c 4 Apr 08 Hubert van Hecke: Added argument TYPE that allows switching between old
c          and new beam pipe. TYPE='PIPE' reads namelist pip_par, TYPE='PIPN' reads
c          namelist pipn_par. Argument comes from file pisa.kumac. 
c---------.---------.---------.---------.---------.---------.---------.--

      implicit none
      character*4 type
C  JPSullivan added this to send "happy" message to LOUT instead of 6
#include "gcunit.inc"
 
      integer nmed_pipe(3)/33,5,5/        !Berylium magnetized,Stainless Steel

*     Corrected by A.Ster December 1999 as part of the ZDC installation

*     real zleng_pipe(3)/60.,70.,300./    !CP to CM ; CM to Piston, Piston to Pu
*     real radius_pipe(3)/4.00,6.35,6.35/ !Outer radius in cm
*     real radius_max/7.00/               !Max_Outer_radius (in cm) of the Beryl
*     real thick_pipe(3)/0.10,2.0,2.1/    !Thickness in cm
*     real zpos_pipe(3)/0.0,60.0,200.0/   !Z_position of the pipe

*!!!!!!! ASK R. Ruggiero for precise description !!!!!!

      real zleng_pipe(3)/70.,60.,278.1/   !CP to CM ; CM to Piston, Piston to Pu
      real radius_pipe(3)/3.81,3.81,6.35/ !Outer radius in cm
      real radius_max/7.00/               !Max_Outer_radius (in cm) of the Beryl
      real thick_pipe(3)/0.10,0.10,0.27/    !Thickness in cm
      real zpos_pipe(3)/0.0,70.0,190.0/   !Z_position of the pipe


      integer nmed_vacu /32/              !Medium for vacuum magnetized
      integer color_pipe /4/              !Color (visible/invisible)
      namelist /pip_par/  nmed_pipe, zleng_pipe, thick_pipe,
     1            radius_pipe, nmed_vacu, color_pipe,zpos_pipe
      namelist /pipn_par/ nmed_pipe, zleng_pipe, thick_pipe,
     1            radius_pipe, nmed_vacu, color_pipe,zpos_pipe
      real tube_par(3)     ! parameter array for the tube call
      real rad_default(3),zlen_default(3),th_default(3),zpos_default(3)
      integer nmed_default(3)

      character*4 v_m_name,v_i_name

      integer  npar,nmed,ivolu, icall
      integer jk,nobpfl
      real    zpos
 
      data icall/0/
       
c---------------------------------------------------------------------
c     geometry description logical unit       
      integer itf_lun
      common /interface/itf_lun

c ------------------------------------------------------------------------------

      if(icall.eq.0) then
         icall = 1
      else
         write (LOUT,*)' BEAM_PIPE was already called before - return'
         return         ! beam_pipe called twice ?
      endif
 
      do jk = 1, 3
       rad_default(jk) = radius_pipe(jk)
       zlen_default(jk)= zleng_pipe(jk)
         th_default(jk)  = thick_pipe(jK)
         zpos_default(jk)= zpos_pipe(jK)
      end do

c     Read the geometery file segment

      if (type.eq.'PIPE') then     
        write(LOUT,* ) 
     &  'beam_pipe - reading old PIPE parameters from common interface'
        rewind(itf_lun)
        read( itf_lun, nml = pip_par, err = 999 )
      elseif (type.eq.'PIPN') then     
        write(LOUT,* ) 
     &  'beam_pipe - reading new PIPN parameters from common interface'
        rewind(itf_lun)
        read( itf_lun, nml = pipn_par, err = 998 )
      else
        go to 997
      endif

c     do some checking  (should add medium checking?)

      if(radius_pipe(1) .gt. radius_max) then
       write(LOUT,6)radius_pipe(1),radius_max
6      format(//,'beam_pipe - Beryllium beam pipe radius ',g10.3,
     1      ' exceeds',g6.3,' cm maximum ?',//)
       stop ' PISA stopped because of error detected in BEAM_PIPE'
      endif
 
      nobpfl = 0
      do jk = 1 , 3
       if(zleng_pipe(jk).eq.0.0.or.radius_pipe(jk).eq.0.0.or.
     1    thick_pipe(jk).eq.0.0) then
         nobpfl = 1
       end if
      end do
 
      if(nobpfl .eq. 1) then
       write(LOUT,11)
11     format(//,'beam_pipe - One of the beam pipe parameter',
     1      ' dimensions is zero,'/,3x,'so no beam pipe is installed',
     2      //)

c     need lengths for vacuum installation

       do jk = 1, 3
        radius_pipe(jk) = rad_default(jk)
        zleng_pipe(jk)  = zlen_default(jk)
        thick_pipe(jk)  = th_default(jk)
        zpos_pipe(jk)   = zpos_default(jk)
       end do
       go to 50
      end if

c now put in pipe geometry into proper mother volume

      v_m_name = 'HALL'    ! use HALL as immediate mother
      npar = 3       ! parameters for Tube

c now fill up the TUBE parameters for the pipe

      do jk = 1 , 3
       if(jk .eq. 1) then
        v_i_name ='PIPE'
        zpos     = zpos_pipe(jk)
       else
        write(v_i_name ,'(a3,i1)')'PIP',jk
        zpos     = zpos_pipe(jk) + zleng_pipe(jk)
       end if
 
       nmed = nmed_pipe(jk)
       tube_par(1) = radius_pipe(jk) - thick_pipe(jK)
       tube_par(2) = radius_pipe(jk)
       tube_par(3) = zleng_pipe(jk)
       call gsvolu(v_i_name,'TUBE',nmed,tube_par,npar,ivolu)
       if(color_pipe.gt.0) then
         CALL GSATT(v_i_name,'SEEN',1)
         CALL GSATT(v_i_name,'COLO',color_pipe)
       else

c invisible

         CALL GSATT(v_i_name,'SEEN',0)
       endif

c position the volumes

       call gspos(v_i_name,1,v_m_name,0.0,0.0,zpos,1,'ONLY')

c Now Position PIPE_2 and PIPE_3 on the -Z side

       if(jk .gt. 1) then
        call gspos(v_i_name,2,v_m_name,0.0,0.0,-zpos,1,'ONLY')
       end if
      end do
 
      write(LOUT,*) 'beam_pipe - Beam pipe installed'
50    continue

c now put in vacuum geometry into proper mother volume

      v_m_name = 'HALL'    ! use HALL as immediate mother
      nmed = nmed_vacu           ! should be magnetic vacuum
      npar = 3       ! parameters for Tube

c now fill up the TUBE parameters for the pipe

      do jk = 1 , 3
 
        if(jk .eq. 1) then
         v_i_name = 'PVAC'
         zpos     = zpos_pipe(jk)
        else
         write(v_i_name ,'(a3,i1)')'PVA',jk
         zpos     = zpos_pipe(jk) + zleng_pipe(jk)
        end if
       tube_par(1) = 0.0
       tube_par(2) = radius_pipe(jk) - thick_pipe(jK)
       tube_par(3) = zleng_pipe(jk)
       call gsvolu(v_i_name,'TUBE',nmed,tube_par,npar,ivolu)
       if(color_pipe.gt.0) then
         CALL GSATT(v_i_name,'SEEN',1)
         CALL GSATT(v_i_name,'COLO',color_pipe)
       else

c     invisible

         CALL GSATT(v_i_name,'SEEN',0)
       endif

c position the volumes

       call gspos(v_i_name,1,v_m_name,0.0,0.0,zpos,1,'ONLY')

c Now Position PIPE_2 and PIPE_3 on the -Z side

       if(jk .gt. 1) then
        call gspos(v_i_name,2,v_m_name,0.0,0.0,-zpos,1,'ONLY')
       end if
      end do

c End of pipe and vacuum geometry set up

      write(LOUT,*) 'beam_pipe - Vacuum installed'
      return

 999  STOP 'beam_pipe - Read error in pip_par segment'
 998  STOP 'beam_pipe - Read error in pipn_par segment'
 997  STOP 'beam_pipe - Unrecognized input argument'
      end
