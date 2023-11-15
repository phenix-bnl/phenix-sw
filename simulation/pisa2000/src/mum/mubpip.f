c $Id: mubpip.f,v 1.4 2012/10/19 06:46:45 bbannier Exp $

      subroutine mubpip
c
c    *************************************************************
c    *                                                           *
c    *  MUBPIP (vsn 1.00) reinstall the beam section in the MUID *
c    *                                                           *
c    *  Called by ==> :: < MUM >                                 *
c    *  IN   :: none                                             *
c    *  OUT  :: none                                             *
c    *                                                           *
c    *  written  by ::  Surender Saini, 22/06/93 19.14.43        *
c    *  modified by ::                                           *
c    *                                                           *
c    *************************************************************
c Reinstall the beamsection in the MUID because the original beampipe
c was replaced by the muon identifier planes.
c
c Z =+680cm to +930cm  ::Stainless Steel (OD=12.70 cm, Thickness = 2.1 cm)

      implicit none

      integer nmed_pipe(3)/33,5,5/        !Berylium magnetized,Stainless Steel,S
      real zleng_pipe(3)/60.,70.,120./    !CP to CM ; CM to Piston, Piston to Pu
      real radius_pipe(3)/4.00,6.35,6.35/ !Outer radius in cm
      real radius_max/4.00/               !Max_Outer_radius (in cm) of the Beryl
      real thick_pipe(3)/0.10,2.0,2.1/    !Thickness in cm
      real zpos_pipe(3)/0.0,60.0,680.0/   !Z_position of the pipe
      integer nmed_vacu /32/              !Medium for vacuum magnetized
      integer color_pipe /4/              !Color (visible/invisible)
      namelist /pip_par/ nmed_pipe, zleng_pipe, thick_pipe,
     1            radius_pipe, nmed_vacu, color_pipe,zpos_pipe
      real tube_par(3)     ! parameter array for the tube call
      real rad_default(3),zlen_default(3),th_default(3),zpos_default(3)
      integer nmed_default(3)
c
      character*4 v_m_name,v_i_name
c
      integer  npar,nmed,ivolu, icall
      integer jk,nobpfl
      real    zpos

c---------------------------------------------------------------------
c     geometry description logical unit
      integer itf_lun
      common /interface/itf_lun

c -------------------------------------------------------------------------
c

      do jk = 1, 3
       rad_default(jk) = radius_pipe(jk)
       zlen_default(jk)= zleng_pipe(jk)
       th_default(jk)  = thick_pipe(jK)
       zpos_default(jk)= zpos_pipe(jK)
      end do
c
c     Read the geometery file segment
c
      write( *,* ) 'mubpip - reading parameter from common interface'
      rewind(itf_lun)
      read( itf_lun, nml = pip_par, err = 999 )

c
c now put in MUBB geometry into proper mother volume
c
      v_m_name = 'HALL'    ! use HALL as immediate mother
      npar = 3             ! parameters for Tube
c
c now fill up the TUBE parameters for MUBB
c
      do jk = 1 , 3
       if(jk .eq. 3)then
        v_i_name ='MUBB'
        zpos     = zpos_default(jk) + zlen_default(jk)

        nmed = nmed_pipe(jk)
        tube_par(1) = radius_pipe(jk) - thick_pipe(jK)
        tube_par(2) = radius_pipe(jk)
        tube_par(3) = zlen_default(jk)
        call gsvolu(v_i_name,'TUBE',nmed,tube_par,npar,ivolu)
        if(color_pipe.gt.0)then
         CALL GSATT(v_i_name,'SEEN',1)
         CALL GSATT(v_i_name,'COLO',color_pipe)
        else
c
c invisible
c
         CALL GSATT(v_i_name,'SEEN',0)
        endif
c
c position MUBB inside the mother_volume
c
        call gspos(v_i_name,1,v_m_name,0.0,0.0,zpos,1,'ONLY')
       end if
      end do


      write(6,*) 'mubpip - muon beam pipe reinstalled'
50    continue
c
c now put in vacuum geometry into proper mother volume
c
      v_m_name = 'HALL'    ! use HALL as immediate mother
      nmed = nmed_vacu           ! should be magnetic vacuum
      npar = 3       ! parameters for Tube
c
c now fill up the TUBE parameters for the Vacuum
c
      do jk = 1 , 3
       if(jk .eq. 3)then
        v_i_name = 'MUVC'
        zpos     = zpos_default(jk) + zlen_default(jk)

        tube_par(1) = 0.0
        tube_par(2) = radius_pipe(jk) - thick_pipe(jK)
        tube_par(3) = zlen_default(jk)
        call gsvolu(v_i_name,'TUBE',nmed,tube_par,npar,ivolu)
        if(color_pipe.gt.0)then
         CALL GSATT(v_i_name,'SEEN',1)
         CALL GSATT(v_i_name,'COLO',color_pipe)
        else
c
c     invisible
c
         CALL GSATT(v_i_name,'SEEN',0)
        endif
c
c position the volumes
c
        call gspos(v_i_name,1,v_m_name,0.0,0.0,zpos,1,'ONLY')
       end if
      end do

      write(6,*) 'mubpip - Vacuum reinstalled'
      return

999   write(6,*) 'mubpip - Read error in pip_par segment of phnx.par.',
     +  ' The PHNX.PAR file will be re-read to pinpoint the erroneous',
     +  ' line'

      rewind( itf_lun )
      read( itf_lun, nml = pip_par )
      stop 'mubpip - PISA stop because of PHNX.PAR file error.'
      end
