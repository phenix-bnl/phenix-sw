c $Id: mutrst.f,v 1.10 2015/09/01 16:57:34 hubert Exp $
*-- Author :    Soren P. Sorensen 28/09/92

      subroutine mutrst( full, nh)

C    *************************************************************
C    *                                                           *
C    *  MUTRST (vsn 1.00) muon_arm tracking stations geometry    *
C    *                                                           *
C    *  Called by ==> ::  < MUM >                                *
C    *  IN   :: full, nh                                         *
C    *  OUT  :: none                                             *
C    *                                                           *
C    *  written  by ::  Soren P. Sorensen, 28/09/92              *
C    *  modified by ::  Surender Saini, 12/04/93                 *
C    *                  JPSullivan, Oct 5, 1993                  *
C    *                    (some output to LOUT)                  *
C    *                  Mike Leitch, Aug 5, 1994                 *
C    *                  Melynda Brooks, Aug. 19 1996             *
C    *                    extensive re-write to put in real      *
C    *                    chamber geometries                     *
C    *  modified by ::  Douglas Fields, Jan 19, 2001             *
C    *                  Put in realistic station one geometry    *
C    * Mar 2010 Hubert van Hecke: numerous small modifications 
C                       to eliminate volume conflicts, mostly 
C                       extrusions. See 
C      http://p25ext.lanl.gov/people/hubert/phenix/silicon/simulations/mar10/mutr.html
C      All conflicts >10um are resolved. 
C*****************************************************************

c This routine creates the muon tracking geometry for one or both
c muon endcaps. It proceeds as follows:
c   1) read the namelist geometrical parameters and store them in a zebra bank
c   2) loop over endcaps, create muon mother cone volume
c   3) loop over stations, create each station volume (octagonal covering 
c      all phi)
c   4) loop over planes, create planes within each station
c   5) create inner and outer frames (in radial direction)
c   6) divide the hexagonal shape into 8 octants
c   7) add frames along the radial divisions between octants
c   8) create the active volume in each octant

C Tracking for the Muon Detector in PHENIX
C  S. P. Sorensen  28-sep-1992

      implicit none

C  Include files.

#include "guphnx.inc"
#include "gugeom.inc"
#include "fstore.inc"
#include "sublink.inc"
#include "fpmlink.inc"
#include "gcunit.inc"
#include "gconst.inc"

C  Begin of declarations.

      integer         nh, ns, ip, ifoil, ispace, is, ia, 
     &                  nbitsv( 5 ), iod, i, iaa, ivolu,
     &                  idtype, nwpa, nwsa, iset, idet, j, k

C  Volume names.

      character*4     ArmName
      character*4     StationName,StationNamef,StationNameb
      character*4     OctantName,OctantNameb,OctantNamef
      character*4     MylarName,Mylar2Name
      character*4     ShieldFoilName,ShieldFoil2Name
      character*4     FrameName,Frame2Name,Frame3Name,Frame4Name
      character*4     HoneyName
      character*4     BarNamef,BarNameb
      character*4     QuadrantName
      character*4     PanelName
      character*4     SkinName
      character*4     AnodeName,Anode2Name,Anode3Name,Anode4Name
      character*4     Rib45Name,Rib22Name
      character*4     ReadoutName,Readout2Name
      character*4     GasName,Gas2Name
      character*4     MountName,Mount2Name
      character*4     FeeName,Fee2Name,Fee3Name,Fee4Name
      character*4     VmeName,Vme2Name,Vme3Name
      character*4     SpaceNamef,Space2Namef
      character*4     SpaceNameb,Space2Nameb
      character*4     FoilNamef,Foil2Namef,Foil3Namef
      character*4     FoilNameb,Foil2Nameb,Foil3Nameb

C  Utility variables.

      real            xpos,ypos
      real            zpos,zpos2,zpos3
      real            xoff,yoff,zoff
      real            StationOneThickness
      real            StationTwoThickness
      real            StationTwoGap
      real            StationThreeThickness
      real            FeeThickness
      real            phi1, phi2, dphi, radius

C  Shapes input data.

      real            par_pgon(10) /10 *0.0/
      real            par_para(6) /6 * 0.0/
      real            par_tube(3)/3 * 0.0/
      real            par_trap(11)/11*0./
      real            par_box(3) /3*0.0/
      real            par_tubs(5) /5*0.0/
      real            par_pcon(15) /15*0.0/

C  Various rotation matrix names for positioning parts.

      integer irot22,irot45,irot67,irot90,irot112,irot45f
      integer irot202,irot292,irot67f,irot90f,irot180f,irot225f

C  Paramters used to define the sensitive volumes and hit storage
C  (see also namesh in GUGEOM)
C  muon tracking station resolution, 0.001 cm ( 10 microns )
C  nbitmu is bits stored for mu:
C          POSX,POSY,POSZ: positions in 10 microns
C          DELE: energy in 0.1 KeV
C          TOFL: tof in 0.01 nsec
C          P_ID: pid
C          P_PX,P_PY,P_PZ: momentum in units of MeV/c
C          PLEN: 100 micron path length
C          ETOT: total track energy
      character*4     full, set_id, namesv( 5 )
      data idtype/7/
      real origmu(11)/3*1000.0,3*0.0,       3*100., 2*0.      / !offset
      real factmu(11)/3*1000.0,1.e7,100.,1.,3*1000.,100.,2000./ !gain
      integer nbitmu(11)/3*24,2*20,8,5*20/ !number of bits

c---------------------------------------------------------------------
c     geometry description logical unit
      integer itf_lun
      common /interface/itf_lun
      
C ========================================================================
C  End of declarations.

C  Read geometry file.

      write( *,* ) 'mutrst - reading parameter from common interface'
      rewind(itf_lun)
      read( itf_lun, nml = mum_par, err = 999 )


C  If mumtrflg = 1 then using old geometry of pre-March 2001.  Return error
C  and recommend new geometry which is chosen by mumtrflg=2

      if( mumtrflg .eq. 1 ) then

        print *, ' You have chosen the old Muon Tracking geometry with
     +             mumtrflg=1'
        print *, ' Please update your PISA code and phnx.par file and
     +             try again'
        return

      else if( mumtrflg .eq. 5) then

        mum_arms        = min( mum_arms, mum_arms_max )
        mum_stations    = min( mum_stations, mum_stations_max )

C  Write mum_par parameters into the zebra bank MUMA.

        call    mzform( 'PARA', '-F', iod )
        call    mzbook( ixdiv_fr,
     &                  lfm_para,
     &                  lfm_para,
     &                  1,
     &                  'PARA',
     &                  0,
     &                  0,
     &                  mum_par_words,
     &                  iod,
     &                  0 )

C  Fill MUMA bank.

        i = 0
        i = i+1
        qf( lfm_para + i ) = real( mumtrflg )
        i = i+1
        qf( lfm_para + i ) = real( mum_arms )
        i = i+1
        qf( lfm_para + i ) = real( mum_stations )
        i = i+1
        qf( lfm_para + i ) = real( mum_channels )
        i = i+1
        qf( lfm_para + i ) = real( mum_color )
        i = i+1
        qf(lfm_para+i) = real(ArmMedium)
        do ia=1,2
          do j=1,3
            i = i+1
            qf(lfm_para+i)=real(PlanesPerStation(j,ia))
          enddo
        enddo
        i = i+1
        qf(lfm_para+i) = real(StationMedium)
        i = i+1
        qf(lfm_para+i) = real(HoneyMedium)
        i = i+1
        qf(lfm_para+i) = real(FEEFlag)
        do j = 1, 8
          i = i+1
          qf(lfm_para+i) = StationOneFrame(j)
        enddo
        do j = 1, 8
          i = i+1
          qf(lfm_para+i) = StationOnePanel(j)
        enddo
        do j = 1, 4
          i = i+1
          qf(lfm_para+i) = StationOneAnode(j)
        enddo
        do j = 1, 8
          i = i+1
          qf(lfm_para+i) = StationOneRead(j)
        enddo
        do j = 1, 7
          i = i+1
          qf(lfm_para+i) = StationOneRib(j)
        enddo
        do j = 1, 8
          i = i+1
          qf(lfm_para+i) = StationOneGas(j)
        enddo
        do j = 1, 7
          i = i+1
          qf(lfm_para+i) = StationOneMount(j)
        enddo
        do j = 1, 10
          i = i+1
          qf(lfm_para+i) = StationOneFee(j)
        enddo
        do ia=1,2
          do j=1,4
            do k = 1, 6
              i = i+1
              qf(lfm_para+i)=StationOneAngles(k,j,ia)
            enddo
          enddo
        enddo
        do ia=1,2
          do j=1,4
            do k = 1, 3
              i = i+1
              qf(lfm_para+i)=StationOneOffsets(k,j,ia)
            enddo
          enddo
        enddo
        do ia=1,2
          do j=1,7
            i = i+1
            qf(lfm_para+i)=StationTwoFFrame(j,ia)
          enddo
        enddo
        do ia=1,2
          do j=1,2
            i = i+1
            qf(lfm_para+i)=StationTwoBFrame(j,ia)
          enddo
        enddo
        do ia=1,2
          do j=1,2
            i = i+1
            qf(lfm_para+i)=StationTwoFBar(j,ia)
          enddo
        enddo
        do ia=1,2
          do j=1,2
            i = i+1
            qf(lfm_para+i)=StationTwoBBar(j,ia)
          enddo
        enddo
        do ia=1,2
          i = i+1
          qf(lfm_para+i) = StationTwoAnode(ia)
        enddo
        i = i+1
        qf(lfm_para+i) = StationTwoFoilThickness
        i = i+1
        qf(lfm_para+i) = StationTwoAlFoilThickness
        do ia=1,2
          do j=1,4
            i = i+1
            qf(lfm_para+i)=StationTwoFRib(j,ia)
          enddo
        enddo
        do ia=1,2
          do j=1,4
            i = i+1
            qf(lfm_para+i)=StationTwoBRib(j,ia)
          enddo
        enddo
        do ia=1,2
          do j=1,6
            i = i+1
            qf(lfm_para+i)=StationTwoFGas(j,ia)
          enddo
        enddo
        do ia=1,2
          i = i+1
          qf(lfm_para+i)=StationTwoBGas(ia)
        enddo
        do ia=1,2
          do j=1,8
            do k = 1, 6
              i = i+1
              qf(lfm_para+i)=StationTwoAngles(k,j,ia)
            enddo
          enddo
        enddo
        do ia=1,2
          do j=1,8
            do k = 1, 3
              i = i+1
              qf(lfm_para+i)=StationTwoOffsets(k,j,ia)
            enddo
          enddo
        enddo
        do ia=1,2
          do j=1,9
            i = i+1
            qf(lfm_para+i)=StationThreeFrame(j,ia)
          enddo
        enddo
        do ia=1,2
          do j=1,7
            i = i+1
            qf(lfm_para+i)=StationThreePanel(j,ia)
          enddo
        enddo
        do ia=1,2
          do j=1,5
            i = i+1
            qf(lfm_para+i) = StationThreeAnode(j,ia)
          enddo
        enddo
        do ia=1,2
          do j=1,4
            i = i+1
            qf(lfm_para+i)=StationThreeRib(j,ia)
          enddo
        enddo
        do ia=1,2
          do j=1,6
            i = i+1
            qf(lfm_para+i)=StationThreeGas(j,ia)
          enddo
        enddo
        do ia=1,2
          do j=1,8
            do k = 1, 6
              i = i+1
              qf(lfm_para+i)=StationThreeAngles(k,j,ia)
            enddo
          enddo
        enddo
        do ia=1,2
          do j=1,8
            do k = 1, 3
              i = i+1
              qf(lfm_para+i)=StationThreeOffsets(k,j,ia)
            enddo
          enddo
        enddo
        do ia=1,2
          do j=1,3
            i = i+1
            qf(lfm_para+i)=real(FrameMedium(j,ia))
          enddo
        enddo
        do ia=1,2
          do j=1,3+1
            i = i+1
            qf(lfm_para+i)=StationNominalZpos(j,ia)
          enddo
        enddo
        i = i+1
        qf(lfm_para+i) = real(SpokeMedium)

        if ( i .ne. mum_par_words ) print *, ' error #1 in mum. i = ', i

C If input parameters do not require detector, return.

        if (    cvolu_opt( 1, 10 ) .ne. 'FULL'  .and.
     &          cvolu_opt( 1, 10 ) .ne. 'VOLS' ) then
          print *, ' MUM not requested '
          return
        end if

C Create rotation matrices needed to position components in detectors.

        irot=irot+1
        irot22=irot
        call gsrotm(irot22,90.,22.5,90.,112.5,0.0,0.0)
        irot = irot + 1
        irot45 = irot
        call gsrotm(irot45, 90., 45., 90., 135., 0.0, 0.)
        irot=irot+1
        irot67=irot
        call gsrotm(irot67,90.,67.5,90.,157.5,0.0,0.0)
        irot = irot + 1
        irot90 = irot
        call gsrotm(irot90, 90.0, 90.0, 90.0, 180.0, 0.0, 0.0)
        irot=irot+1
        irot112=irot
        call gsrotm(irot112,90.,112.5,90.,202.5,0.0,0.0)
        irot=irot+1
        irot202=irot
        call gsrotm(irot202,90.,202.5,90.,292.5,0.0,0.0)
        irot=irot+1
        irot292=irot
        call gsrotm(irot292,90.,292.5,90.,22.5,0.0,0.0)
        irot = irot + 1
        irot45f = irot
        call gsrotm(irot45f, 90.0, 45.0, 90.0, 315.0, 180.0, 0.0)
        irot = irot + 1
        irot67f = irot
        call gsrotm(irot67f, 90.0, 67.5, 90.0, 337.50, 180.0, 0.0)
        irot = irot + 1
        irot90f = irot
        call gsrotm(irot90f, 90.0, 90.0, 90.0, 0.0, 180.0, 0.0)
        irot = irot + 1
        irot180f = irot
        call gsrotm(irot180f, 90., 180.00, 90., 90.0, 180., 0.)
        irot = irot + 1
        irot225f = irot
        call gsrotm(irot225f, 90., 225.00, 90., 135.0, 180., 0.)

C  Define tracking station geometry. Loop over arms, 
C  positive and negative z.

        do ia = 1, mum_arms

C  Asymmetric muon arms.

          iaa = 2 * mod( ia, 2 ) - 1
          is = 1
          zpos = iaa * StationNominalZpos( is,ia )

C  The arm volumes are now created in magnet.f so that the piston,
C  lampshade and other magnet hardware can be put into it.  The
C  arm volume uses the lampshade z dimensions.  Calculate here
C  what the center of the arm volume is so that we can properly
C  place the station volumes into it.

          write(ArmName,'(a3,i1)')'MUA',ia

C  Build Station One chambers.

          ns = mum_stations * ( ia - 1 ) + is

C  Station volume (octagon).

          StationOneThickness = 2*StationOneFrame(5)+
     &                          4*StationOnePanel(5)+
     &                          3*StationOneAnode(3)+
     &                          1*StationOneMount(1)+
     &                          1*StationOneMount(2)
          par_tube(1)=StationOneMount(3)
          par_tube(2)=StationOneMount(4)
     &                +1.491  ! overlap studies HvH
          par_tube(3)=StationOneThickness/2.
     &                +0.1    ! overlap studies
          write(StationName,'(a2,2i1)')'MS',ia,is
          call gsvolu(StationName,'TUBE',
     &        ArmMedium,par_tube,3,ivolu)
          if ( mum_color .gt. 0 ) then
            call gsatt( StationName, 'SEEN', 1 )
            call gsatt( StationName, 'COLO', mum_color)
          else
            call gsatt( StationName, 'SEEN', 0 )
          endif

C  Position Station One in Muon Arm. zpos is the z position of the
C  first plane (outside skin) of station one.

          zpos2=zpos+iaa*(StationOneThickness/2.)
          if(ia.eq.1) then
            call gspos( StationName, 1, ArmName,
     &          0.0, 0.0, zpos2, irot180f, 'ONLY' )
          elseif(ia.eq.2) then
            call gspos( StationName, 1, ArmName,
     &          0.0, 0.0, zpos2, 0, 'ONLY' )
          endif

C  Make quadrant volume, out of air, then fill this with frames,
C  honeycomb panels, read-out cards, sensitive gas volumes, etc.

          par_tubs(1)=StationOneMount(3)
          par_tubs(2)=StationOneMount(4)
     &               +1.491   ! overlap studies HvH
          par_tubs(3)=StationOneThickness/2.
          par_tubs(4)=0.0
          par_tubs(5)=90.0
          write(QuadrantName,'(a2,2i1)')'MQ',ia,is
          call gsvolu(QuadrantName, 'TUBS',
     &           ArmMedium, par_tubs, 5, ivolu)
          if ( mum_color .gt. 0 ) then
            call gsatt( QuadrantName, 'SEEN', 1 )
            call gsatt( QuadrantName, 'COLO', mum_color)
          else
            call gsatt( QuadrantName, 'SEEN', 0 )
          endif

C  Make split ring volumes out of aluminum, with fill volumes 
C  for the open sections out of air.

          par_tubs(1)=StationOneMount(3)
          par_tubs(2)=StationOneMount(4)
          par_tubs(3)=StationOneMount(1)/2.
          par_tubs(4)=0.
          par_tubs(5)=90.
          write(MountName,'(a2,2i1)')'MA',ia,is
          call gsvolu(MountName, 'TUBS',
     &           26, par_tubs, 5, ivolu)
          if ( mum_color .gt. 0 ) then
            call gsatt( MountName, 'SEEN', 1 )
            call gsatt( MountName, 'COLO', mum_color)
          else
            call gsatt( MountName, 'SEEN', 0 )
          endif

C  Fill volumes for open spaces.

          par_tubs(1)=StationOneMount(5)
          par_tubs(2)=StationOneMount(6)
          par_tubs(3)=StationOneMount(1)/2.
          par_tubs(4)=0.
          par_tubs(5)=90.
          write(Mount2Name,'(a2,2i1)')'MG',ia,is
          call gsvolu(Mount2Name, 'TUBS',
     &           ArmMedium, par_tubs, 5, ivolu)
          if ( mum_color .gt. 0 ) then
            call gsatt( Mount2Name, 'SEEN', 1 )
            call gsatt( Mount2Name, 'COLO', mum_color)
          else
            call gsatt( Mount2Name, 'SEEN', 0 )
          endif

C  Position holes in split ring.

          xpos = StationOneMount(7)
          ypos = StationOneMount(7)
          call gspos( Mount2Name, 1, MountName,
     &          xpos, ypos, 0.0, 0, 'ONLY' )

C  Make frame volumes out of aluminum, with fill volumes 
C  for the open sections out of air.

          par_pgon(1)=0.
          par_pgon(2)=90.
          par_pgon(3)=4.
          par_pgon(4)=2.
          par_pgon(5)=-StationOneFrame(5)/2.
          par_pgon(6)=StationOneFrame(1)
          par_pgon(7)=StationOneFrame(2)
          par_pgon(8)=par_pgon(5)+StationOneFrame(5)
          par_pgon(9)=par_pgon(6)
          par_pgon(10)=par_pgon(7)
          write(FrameName,'(a2,2i1)')'MF',ia,is
          call gsvolu(FrameName, 'PGON',
     &           26, par_pgon, 10, ivolu)

          if ( mum_color .gt. 0 ) then
            call gsatt( FrameName, 'SEEN', 1 )
            call gsatt( FrameName, 'COLO', mum_color)
          else
            call gsatt( FrameName, 'SEEN', 0 )
          endif

C  Fill volumes for open spaces.

          par_pgon(1)=0.
          par_pgon(2)=45.
          par_pgon(3)=2.
          par_pgon(4)=2.
          par_pgon(5)=-StationOneFrame(5)/2.
          par_pgon(6)=StationOneFrame(3)
          par_pgon(7)=StationOneFrame(4)
          par_pgon(8)=par_pgon(5)+StationOneFrame(5)
          par_pgon(9)=par_pgon(6)
          par_pgon(10)=par_pgon(7)
          write(Frame2Name,'(a2,2i1)')'MI',ia,is
          call gsvolu(Frame2Name, 'PGON',
     &           ArmMedium, par_pgon, 10, ivolu)

          if ( mum_color .gt. 0 ) then
            call gsatt( Frame2Name, 'SEEN', 1 )
            call gsatt( Frame2Name, 'COLO', mum_color)
          else
            call gsatt( Frame2Name, 'SEEN', 0 )
          endif

C  Position holes in frame.

          xpos = StationOneFrame(6)
          ypos = StationOneFrame(7)
          call gspos( Frame2Name, 1, FrameName,
     &          xpos, ypos, 0.0, 0, 'ONLY' )
          xpos = StationOneFrame(7)
          ypos = StationOneFrame(6)
          call gspos( Frame2Name, 2, FrameName,
     &          xpos, ypos, 0.0, irot90f, 'ONLY' )

C  Make honeycomb panels out of FR4 with entire thickness (including
C  cathode skins, glue, etc).

          par_pgon(1)=0.
          par_pgon(2)=90.
          par_pgon(3)=4.
          par_pgon(4)=2.
          par_pgon(5)=-StationOnePanel(5)/2.
          par_pgon(6)=StationOnePanel(1)
          par_pgon(7)=StationOnePanel(2)
          par_pgon(8)=par_pgon(5)+StationOnePanel(5)
          par_pgon(9)=par_pgon(6)
          par_pgon(10)=par_pgon(7)
          write(PanelName,'(a2,2i1)')'MP',ia,is
          call gsvolu(PanelName, 'PGON',
     &           FrameMedium(is,ia), par_pgon, 10, ivolu)
          if ( mum_color .gt. 0 ) then
            call gsatt( PanelName, 'SEEN', 1 )
            call gsatt( PanelName, 'COLO', mum_color)
          else
            call gsatt( PanelName, 'SEEN', 0 )
          endif

C  Now honeycomb fill.

          par_pgon(1)=0.
          par_pgon(2)=90.
          par_pgon(3)=4.
          par_pgon(4)=2.
          par_pgon(5)=-StationOnePanel(6)/2.
          par_pgon(6)=StationOnePanel(3)
          par_pgon(7)=StationOnePanel(4)
          par_pgon(8)=par_pgon(5)+StationOnePanel(6)
          par_pgon(9)=par_pgon(6)
          par_pgon(10)=par_pgon(7)
          write(HoneyName,'(a2,2i1)')'MH',ia,is
          call gsvolu(HoneyName, 'PGON',
     &           HoneyMedium, par_pgon, 10, ivolu)
          if ( mum_color .gt. 0 ) then
            call gsatt( HoneyName, 'SEEN', 1 )
            call gsatt( HoneyName, 'COLO', mum_color)
          else
            call gsatt( HoneyName, 'SEEN', 0 )
          endif

C  Position honeycomb inside of panels (0.5" edge fill).

          xpos = 1.27
          ypos = 1.27
          call gspos( HoneyName, 1, PanelName,
     &          xpos, ypos, 0.0, 0, 'ONLY' )

C  Make skins for panels out of copper. 

          par_pgon(1)=0.
          par_pgon(2)=90.
          par_pgon(3)=4.
          par_pgon(4)=2.
          par_pgon(5)=-StationOnePanel(7)/2.
          par_pgon(6)=StationOnePanel(1)
          par_pgon(7)=StationOnePanel(2)
          par_pgon(8)=par_pgon(5)+StationOnePanel(7)
          par_pgon(9)=par_pgon(6)
          par_pgon(10)=par_pgon(7)
          write(SkinName,'(a2,2i1)')'MK',ia,is
          call gsvolu(SkinName, 'PGON',
     &           17, par_pgon, 10, ivolu)
          if ( mum_color .gt. 0 ) then
            call gsatt( SkinName, 'SEEN', 1 )
            call gsatt( SkinName, 'COLO', mum_color)
          else
            call gsatt( SkinName, 'SEEN', 0 )
          endif

C  Position skins inside of panel.

          zpos2 = -StationOnePanel(5)/2.+StationOnePanel(7)/2.
          call gspos( SkinName, 1, PanelName,
     &          0.0, 0.0, zpos2, 0, 'ONLY' )
          zpos2 = StationOnePanel(5)/2.-StationOnePanel(7)/2.
          call gspos( SkinName, 2, PanelName,
     &          0.0, 0.0, zpos2, 0, 'ONLY' )

C  Make Anode readout extensions out of FR4.

          par_box(1) = StationOneRead(2)/2.0
          par_box(2) = StationOneRead(1)/2.0
          par_box(3) = StationOneAnode(3)/4.0
          write(ReadoutName,'(a2,2i1)')'MD',ia,is
          call gsvolu(ReadoutName ,'BOX ',
     &         FrameMedium(is,ia), par_box, 3, ivolu)
          if ( mum_color .gt. 0 ) then
            call gsatt( ReadoutName, 'SEEN', 1 )
            call gsatt( ReadoutName, 'COLO', mum_color)
          else
            call gsatt( ReadoutName, 'SEEN', 0 )
          endif

C  Make Cathode readout extensions out of FR4.

          par_pgon(1)=0.
          par_pgon(2)=90.
          par_pgon(3)=4.
          par_pgon(4)=2.
          par_pgon(5)=-StationOneRead(3)/2.
          par_pgon(6)=StationOneRead(6)
     &           +0.003   ! overlap studies
          par_pgon(7)=StationOneRead(7)
     &           +0.003   ! overlap studies
          par_pgon(8)=par_pgon(5)+StationOneRead(3)/2.
          par_pgon(9)=par_pgon(6)
          par_pgon(10)=par_pgon(7)
          write(Readout2Name,'(a2,2i1)')'MC',ia,is
          call gsvolu(Readout2Name, 'PGON',
     &           17, par_pgon, 10, ivolu)
          if ( mum_color .gt. 0 ) then
            call gsatt( Readout2Name, 'SEEN', 1 )
            call gsatt( Readout2Name, 'COLO', mum_color)
          else
            call gsatt( Readout2Name, 'SEEN', 0 )
          endif

C  Make 22.5 degree rib out of FR4.

          par_box(1) = StationOneRib(2)/2.0
          par_box(2) = StationOneRib(1)/2.0
          par_box(3) = StationOneAnode(3)/4.0
          write(Rib22Name,'(a2,2i1)')'MB',ia,is
          call gsvolu(Rib22Name ,'BOX ',
     &         FrameMedium(is,ia), par_box, 3, ivolu)
          if ( mum_color .gt. 0 ) then
            call gsatt( Rib22Name, 'SEEN', 1 )
            call gsatt( Rib22Name, 'COLO', mum_color)
          else
            call gsatt( Rib22Name, 'SEEN', 0 )
          endif

C  Make 45 degree rib out of FR4.

          par_box(1) = StationOneRib(6)/2.0
          par_box(2) = StationOneRib(5)/2.0
          par_box(3) = StationOneAnode(3)/4.0
          write(Rib45Name,'(a2,2i1)')'ME',ia,is
          call gsvolu(Rib45Name ,'BOX ',
     &         FrameMedium(is,ia), par_box, 3, ivolu)
          if ( mum_color .gt. 0 ) then
            call gsatt( Rib45Name, 'SEEN', 1 )
            call gsatt( Rib45Name, 'COLO', mum_color)
          else
            call gsatt( Rib45Name, 'SEEN', 0 )
          endif

C  Position all parts in the quadrant.  Start with first frame and
C  panel, then loop over three chambers.

          xpos = StationOneFrame(8)
          ypos = StationOneFrame(8)
          zpos2 = StationOneThickness/2.-StationOneFrame(5)/2.
          zpos3 = zpos2-StationOneFrame(5)/2.
          call gspos( FrameName, 1, QuadrantName,
     &               xpos, ypos, zpos2, 0, 'ONLY' )
          xpos = StationOnePanel(8)
          ypos = StationOnePanel(8)
          zpos2 = zpos3-StationOnePanel(5)/2.
          zpos3 = zpos2-StationOnePanel(5)/2.
          call gspos( PanelName, 1, QuadrantName,
     &               xpos, ypos, zpos2, 0, 'ONLY' )

C  Loop over the three chambers in station one.

          do ip = 1, PlanesPerStation( is,ia )

C  MT plane name, e.g. MT42 is 2nd arm, 1st station, 2nd chamber
C  Make active gas volumes. Smaller outer one first.

            par_pgon(1)=0.
            par_pgon(2)=22.5
            par_pgon(3)=1.
            par_pgon(4)=2.
            par_pgon(5)=-StationOneAnode(3)/2.
            par_pgon(6)=StationOneGas(1)
            par_pgon(7)=StationOneGas(2)
            par_pgon(8)=par_pgon(5)+StationOneAnode(3)
            par_pgon(9)=par_pgon(6)
            par_pgon(10)=par_pgon(7)
            write(GasName,'(a2,2i1)')'MT', ns, ip
            call gsvolu(GasName, 'PGON',
     &             StationMedium, par_pgon, 10, ivolu)
            if ( mum_color .gt. 0 ) then
              call gsatt( GasName, 'SEEN', 1 )
              call gsatt( GasName, 'COLO', mum_color)
            else
              call gsatt( GasName, 'SEEN', 0 )
            endif

C  Make active gas volumes. Larger inner one now.

            par_pgon(1)=0.
            par_pgon(2)=22.5
            par_pgon(3)=1.
            par_pgon(4)=2.
            par_pgon(5)=-StationOneAnode(3)/2.
            par_pgon(6)=StationOneGas(5)
            par_pgon(7)=StationOneGas(6)
            par_pgon(8)=par_pgon(5)+StationOneAnode(3)
            par_pgon(9)=par_pgon(6)
            par_pgon(10)=par_pgon(7)
            write(Gas2Name,'(a2,2i1)')'MU', ns, ip
            call gsvolu(Gas2Name, 'PGON',
     &             StationMedium, par_pgon, 10, ivolu)
            if ( mum_color .gt. 0 ) then
              call gsatt( Gas2Name, 'SEEN', 1 )
              call gsatt( Gas2Name, 'COLO', mum_color)
            else
              call gsatt( Gas2Name, 'SEEN', 0 )
            endif

C  Make anode plane out of FR4.

            par_pgon(1)=0.
            par_pgon(2)=90.
            par_pgon(3)=4.
            par_pgon(4)=2.
            par_pgon(5)=-StationOneAnode(3)/2.
            par_pgon(6)=StationOnePanel(1)
            par_pgon(7)=StationOnePanel(2)
            par_pgon(8)=par_pgon(5)+StationOneAnode(3)
            par_pgon(9)=par_pgon(6)
            par_pgon(10)=par_pgon(7)
            write(AnodeName,'(a2,2i1)')'MN',ns,ip
            call gsvolu(AnodeName, 'PGON',
     &             FrameMedium(is,ia), par_pgon, 10, ivolu)
            if ( mum_color .gt. 0 ) then
              call gsatt( AnodeName, 'SEEN', 1 )
              call gsatt( AnodeName, 'COLO', mum_color)
            else
              call gsatt( AnodeName, 'SEEN', 0 )
            endif

C  Now make hole in anode plane out of air.

            par_pgon(1)=0.
            par_pgon(2)=90.
            par_pgon(3)=4.
            par_pgon(4)=2.
            par_pgon(5)=-StationOneAnode(3)/2.
            par_pgon(6)=StationOneAnode(1)
            par_pgon(7)=StationOneAnode(2)
            par_pgon(8)=par_pgon(5)+StationOneAnode(3)
            par_pgon(9)=par_pgon(6)
            par_pgon(10)=par_pgon(7)
            write(Anode2Name,'(a2,2i1)')'MO',ns,ip
            call gsvolu(Anode2Name, 'PGON',
     &             ArmMedium, par_pgon, 10, ivolu)
            if ( mum_color .gt. 0 ) then
              call gsatt( Anode2Name, 'SEEN', 1 )
              call gsatt( Anode2Name, 'COLO', mum_color)
            else
              call gsatt( Anode2Name, 'SEEN', 0 )
            endif

C  Position hole in anode frame.

            xpos = StationOneAnode(4)
            ypos = StationOneAnode(4)
            call gspos( Anode2Name, 1, AnodeName,
     &            xpos, ypos, 0.0, 0, 'ONLY' )

C  Position gas and ribs inside of hole.

C  Now position 45 degree rib.

            xpos = StationOneRib(7)-
     &              StationOnePanel(8)-StationOneAnode(4)
            ypos = StationOneRib(7)-
     &              StationOnePanel(8)-StationOneAnode(4)
            zpos2 = -StationOneAnode(3)/4.
            call gspos( Rib45Name, 1, Anode2Name,
     &                 xpos, ypos, zpos2, irot45, 'ONLY' )

C  Now position 22.5 degree ribs.

            xpos = StationOneRib(3)-
     &              StationOnePanel(8)-StationOneAnode(4)
            ypos = StationOneRib(4)-
     &              StationOnePanel(8)-StationOneAnode(4)
            zpos2 = -StationOneAnode(3)/4.
            call gspos( Rib22Name, 1, Anode2Name,
     &                 xpos, ypos, zpos2, irot22, 'ONLY' )
            xpos = StationOneRib(4)-
     &              StationOnePanel(8)-StationOneAnode(4)
            ypos = StationOneRib(3)-
     &              StationOnePanel(8)-StationOneAnode(4)
            zpos2 = -StationOneAnode(3)/4.
            call gspos( Rib22Name, 2, Anode2Name,
     &                 xpos, ypos, zpos2, irot67, 'ONLY' )

C  Now position smaller outer gas panels.

            xpos = StationOneGas(3)-
     &              StationOnePanel(8)-StationOneAnode(4)
            ypos = StationOneGas(4)-
     &              StationOnePanel(8)-StationOneAnode(4)
            zpos2 = 0.0
            call gspos( GasName, 1, Anode2Name,
     &                 xpos, ypos, zpos2, 0, 'ONLY' )
            xpos = StationOneGas(4)-
     &              StationOnePanel(8)-StationOneAnode(4)
            ypos = StationOneGas(3)-
     &              StationOnePanel(8)-StationOneAnode(4)
            zpos2 = 0.0
            call gspos( GasName, 2, Anode2Name,
     &                 xpos, ypos, zpos2, irot90f, 'ONLY' )

C  Now position larger inner gas panels.

            xpos = StationOneGas(7)-
     &              StationOnePanel(8)-StationOneAnode(4)
            ypos = StationOneGas(8)-
     &              StationOnePanel(8)-StationOneAnode(4)
            zpos2 = 0.0
            call gspos( Gas2Name, 1, Anode2Name,
     &                 xpos, ypos, zpos2, irot22, 'ONLY' )
            xpos = StationOneGas(8)-
     &              StationOnePanel(8)-StationOneAnode(4)
            ypos = StationOneGas(7)-
     &              StationOnePanel(8)-StationOneAnode(4)
            zpos2 = 0.0
            call gspos( Gas2Name, 2, Anode2Name,
     &                 xpos, ypos, zpos2, irot67f, 'ONLY' )

C  Now position anode frame.

            xpos = StationOnePanel(8)
            ypos = StationOnePanel(8)
            zpos2 = zpos3-StationOneAnode(3)/2.
            zpos3 = zpos2-StationOneAnode(3)/2.
            call gspos( AnodeName, 1, QuadrantName,
     &                 xpos, ypos, zpos2, 0, 'ONLY' )

C  Now position anode readout cards.

            xpos = StationOneRead(4)
            ypos = StationOneRead(5)
            zpos2 = zpos3+StationOneAnode(3)/4.
            zpos3 = zpos2-StationOneAnode(3)/4.
            call gspos( ReadoutName, (2*ip-1), QuadrantName,
     &                 xpos, ypos, zpos2, 0, 'ONLY' )
            xpos = StationOneRead(5)
            ypos = StationOneRead(4)
            zpos2 = zpos3+StationOneAnode(3)/4.
            zpos3 = zpos2-StationOneAnode(3)/4.
            call gspos( ReadoutName, 2*ip, QuadrantName,
     &                 xpos, ypos, zpos2, irot90, 'ONLY' )

C  Now position cathode readout cards.

            xpos = StationOneRead(8)
            ypos = StationOneRead(8)
            zpos2 = zpos3+StationOneRead(3)/2.
            zpos3 = zpos2-StationOneRead(3)/2.
            call gspos( Readout2Name, ip, QuadrantName,
     &                 xpos, ypos, zpos2, 0, 'ONLY' )
C  Now put in next panel.

            xpos = StationOnePanel(8)
            ypos = StationOnePanel(8)
            zpos2 = zpos3-StationOnePanel(5)/2.
            zpos3 = zpos2-StationOnePanel(5)/2.
            call gspos( PanelName, ip+1, QuadrantName,
     &               xpos, ypos, zpos2, 0, 'ONLY' )

C  Put volume elements together into sensitive set if requested.

            if ( cvolu_opt( 1, 10 ) .eq. 'FULL' ) then
              set_id          = 'MUM '        ! put it in a SET
              namesv( 1 )     = QuadrantName
              nbitsv( 1 )     = 4
              nwpa            = 200           ! for now
              nwsa            = 200           ! for now
              call gsdet( set_id, GasName, 1, namesv,
     &          nbitsv, idtype, nwpa, nwsa, iset, idet )
              call gsdeth( set_id, GasName, nh, namesh,
     &          nbitmu, origmu, factmu)
              call gsdet( set_id, Gas2Name, 1, namesv,
     &          nbitsv, idtype, nwpa, nwsa, iset, idet )
              call gsdeth( set_id, Gas2Name, nh, namesh,
     &          nbitmu, origmu, factmu)
            end if
          end do                  ! loop over planes

C  Now position last frame.

          xpos = StationOneFrame(8)
          ypos = StationOneFrame(8)
          zpos2 = zpos3-StationOneFrame(5)/2.
          zpos3 = zpos2-StationOneFrame(5)/2.
          call gspos( FrameName, 2, QuadrantName,
     &               xpos, ypos, zpos2, 0, 'ONLY' )

C  Now position split ring.

          zpos2 = zpos3-StationOneMount(2)
     &            -StationOneMount(1)/2.
          zpos3 = zpos2-StationOneMount(1)/2.
          call gspos( MountName, 2, QuadrantName,
     &               0.0, 0.0, zpos2, 0, 'ONLY' )

C  Create rotation matrices for positioning quadrants 
C  in the station volume using real survey information.

C  Place 4 quadrants into the station

          do i = 1, 4
            irot = irot + 1
            xoff = StationOneOffsets(1,i,ia)
            yoff = StationOneOffsets(2,i,ia)
            zoff = StationOneOffsets(3,i,ia)
            call gsrotm(irot, StationOneAngles(1,i,ia),
     &                        StationOneAngles(2,i,ia),
     &                        StationOneAngles(3,i,ia),
     &                        StationOneAngles(4,i,ia),
     &                        StationOneAngles(5,i,ia),
     &                        StationOneAngles(6,i,ia))
            call gspos(QuadrantName, i, StationName,
     &                 xoff, yoff, zoff, irot, 'ONLY' )
          end do

C  Put in Station one FEE plate and VME crates, since they are
C  in the acceptance.  First make mother volume MFEE.  Only do
C  this once.

          if (FEEFlag .EQ. 1) then

          if(FeeName.ne.'MFEE') then
            write(FeeName,'(a4)')'MFEE'

            FeeThickness=StationOneFee(1)+
     &                   StationOneFee(10)
                                   ! Changed MFEE from a TUBS to a PCON
            par_pcon(1) = 0.       ! This is a pcon that wraps tightly
            par_pcon(2) = 360.     ! around the support plate MFPL and
            par_pcon(3) = 4        ! the FEE crates.
                                   ! Feb 2010 HvH
            par_pcon(4) = -FeeThickness/2.0
            par_pcon(5) = StationOneFee(2)
            par_pcon(6) = StationOneFee(3)

            par_pcon(7) = -FeeThickness/2.0 + StationOneFee(1)
            par_pcon(8) = StationOneFee(2)   ! step down at the support
            par_pcon(9) = StationOneFee(3)   ! plate thickness

            par_pcon(10) = -FeeThickness/2.0 + StationOneFee(1)
            par_pcon(11) = 122.      ! r just inside the fee crates
            par_pcon(12) = 149.      ! r just outside the fee crates

            par_pcon(13) =  FeeThickness/2.0
            par_pcon(14) = 122.     ! out to crate thickness in z
            par_pcon(15) = 149.

            call gsvolu(FeeName,'PCON',ArmMedium,par_pcon,15,ivolu)

            if ( mum_color .gt. 0 ) then
              call gsatt( FeeName, 'SEEN', 1 )
              call gsatt( FeeName, 'COLO', mum_color)
            else
              call gsatt( FeeName, 'SEEN', 0 )
            endif

C  Now make FEE plate.

            par_tubs(1)=StationOneFee(2)
            par_tubs(2)=StationOneFee(3)
            par_tubs(3)=StationOneFee(1)/2.
            par_tubs(4)=0.
            par_tubs(5)=90.
            write(Fee2Name,'(a4)')'MFPL'
            call gsvolu(Fee2Name, 'TUBS',
     &             5, par_tubs, 5, ivolu)
            if ( mum_color .gt. 0 ) then
              call gsatt( Fee2Name, 'SEEN', 1 )
              call gsatt( Fee2Name, 'COLO', mum_color)
            else
              call gsatt( Fee2Name, 'SEEN', 0 )
            endif

C  Now make hole in plate.

            par_box(1)=StationOneFee(5)/2.
            par_box(2)=StationOneFee(6)/2.
            par_box(3)=StationOneFee(1)/2.
            write(Fee3Name,'(a4)')'MFPH'
            call gsvolu(Fee3Name, 'BOX ',
     &             ArmMedium, par_box, 3, ivolu)
            if ( mum_color .gt. 0 ) then
              call gsatt( Fee3Name, 'SEEN', 1 )
              call gsatt( Fee3Name, 'COLO', mum_color)
            else
              call gsatt( Fee3Name, 'SEEN', 0 )
            endif

C  Position holes in plate.

            xpos = StationOneFee(7)
            ypos = StationOneFee(6)/2.
            call gspos( Fee3Name, 1, Fee2Name,
     &            xpos, ypos, 0.0, 0, 'ONLY' )
            xpos = StationOneFee(6)/2.
            ypos = StationOneFee(7)
            call gspos( Fee3Name, 2, Fee2Name,
     &            xpos, ypos, 0.0, irot90, 'ONLY' )

C  Now make annulus.

            par_tubs(1)=StationOneFee(2)
            par_tubs(2)=StationOneFee(4)
            par_tubs(3)=StationOneFee(1)/2.
            par_tubs(4)=0.
            par_tubs(5)=90.
            write(Fee4Name,'(a4)')'MFAN'
            call gsvolu(Fee4Name, 'TUBS',
     &             5, par_tubs, 5, ivolu)
            if ( mum_color .gt. 0 ) then
              call gsatt( Fee4Name, 'SEEN', 1 )
              call gsatt( Fee4Name, 'COLO', mum_color)
            else
              call gsatt( Fee4Name, 'SEEN', 0 )
            endif

C  Now make FEE chassis out of aluminum.

            par_box(1) = StationOneFee(8)/2.0
            par_box(2) = StationOneFee(9)/2.0
            par_box(3) = StationOneFee(10)/2.0
            write(VmeName,'(a4)')'MVME'
            call gsvolu(VmeName ,'BOX ',
     &           26, par_box, 3, ivolu)
            if ( mum_color .gt. 0 ) then
              call gsatt( VmeName, 'SEEN', 1 )
              call gsatt( VmeName, 'COLO', mum_color)
            else
              call gsatt( VmeName, 'SEEN', 0 )
            endif

C  Now put air hole in solid aluminum FEE chassis.

            par_box(1) = (StationOneFee(8)-1.0)/2.0
            par_box(2) = (StationOneFee(9)-4.0)/2.0
            par_box(3) = (StationOneFee(10)-2.0)/2.0
            write(Vme2Name,'(a4)')'MVMH'
            call gsvolu(Vme2Name ,'BOX ',
     &           ArmMedium, par_box, 3, ivolu)
            if ( mum_color .gt. 0 ) then
              call gsatt( Vme2Name, 'SEEN', 1 )
              call gsatt( Vme2Name, 'COLO', mum_color)
            else
              call gsatt( Vme2Name, 'SEEN', 0 )
            endif

C  Position hole in VME chassis.

            call gspos( Vme2Name, 1, VmeName,
     &            0.0, 0.0, 0.0, 0, 'ONLY' )

C  Now make FEE card out of silicon.

            par_box(1) = (StationOneFee(8)-1.0)/2.0
            par_box(2) = (StationOneFee(9)-4.0)/2.0
            par_box(3) = 0.2/2.0
            write(Vme3Name,'(a4)')'MVMC'
            call gsvolu(Vme3Name ,'BOX ',
     &           26, par_box, 3, ivolu)
            if ( mum_color .gt. 0 ) then
              call gsatt( Vme3Name, 'SEEN', 1 )
              call gsatt( Vme3Name, 'COLO', mum_color)
            else
              call gsatt( Vme3Name, 'SEEN', 0 )
            endif

C  Now put 6 cards in chassis.

            do i=1,6
              zpos2=(StationOneFee(10)-2.0)/2.0-
     &              i*(StationOneFee(10)-3.0)/6.0
              call gspos( Vme3Name, i, VmeName,
     &              0.0, 0.0, zpos2, 0, 'ONLY' )
            enddo

C  Position four quads of FEE plate in mother volume.

            zpos2=StationOneFee(1)/2.0-FeeThickness/2.0
            do i = 1, 4
              if (i .eq. 1) then
                call gspos(Fee2Name, i, FeeName,
     &          0.0, 0.0, zpos2, irot22, 'ONLY' )
              else if (i .eq. 2) then
                call gspos(Fee2Name, i, FeeName,
     &          0.0, 0.0, zpos2, irot112, 'ONLY' )
              else if (i .eq. 3) then
                call gspos(Fee2Name, i, FeeName,
     &          0.0, 0.0, zpos2, irot202, 'ONLY' )
              else if (i .eq. 4) then
                call gspos(Fee2Name, i, FeeName,
     &          0.0, 0.0, zpos2, irot292, 'ONLY' )
              end if
            end do

C  Put VME chassis in FEE mother volume.

            dphi = 18.0  ! increment of phi in degrees
            radius = 135.0 ! in cm
            do i = 1,20
              irot = irot + 1
              if (i.eq.1) then
                phi1 = 22.5 + 9.0 ! the 9.0 degrees offsets the position
                phi2 = 112.5 + 9.0 ! of the first module
              else
                phi1 = phi1 + dphi
                if (phi1.gt.360.0) phi1 = phi1 - 360.0
                phi2 =  phi2 + dphi
                if (phi2.gt.360.0) phi2 = phi2 - 360.0
              endif
              xpos = radius * cos((phi2-90.0)*3.14159/180.0)
              ypos = radius * sin((phi2-90.0)*3.14159/180.0)
              zpos2 = StationOneFee(1)-FeeThickness/2.0+
     &               StationOneFee(10)/2.0
              call gsrotm(irot, 90.0, phi1, 90.0, phi2, 0.0, 0.0)
              call gspos('MVME',i,'MFEE',xpos,ypos,zpos2,irot,'ONLY')
            enddo
          endif  ! if fee not yet created

C  Position FEE mother volume in the arm.  FEE offset by 1cm
C  from front of Station One.

          zpos2=zpos-iaa*(StationOneFrame(5)+1.0+
     &          FeeThickness/2.)
          if(ia.eq.1) then
            call gspos( FeeName, ia, ArmName,
     &          0.0, 0.0, zpos2, irot180f, 'ONLY' )
          elseif(ia.eq.2) then
            call gspos( FeeName, ia, ArmName,
     &          0.0, 0.0, zpos2, 0, 'ONLY' )
          endif

          end if  ! if FEEFlag=1 then create fee plate

C  If station 2, then the chambers should be octants which
C  overlap each other, and the front and back octants are slightly
C  different dimensions.  For stations 1 and 3, the octants are all
C  in the same z locations.

C  Build Station Two chambers.

          is = 2
          ns = mum_stations * ( ia - 1 ) + is
          zpos = iaa * StationNominalZpos( is,ia )

C  Station volumes (octagon).

          StationTwoThickness = 2*StationTwoFFrame(5,ia)+
     &                          26*StationTwoAnode(ia)+
     &                          StationTwoAlFoilThickness
          StationTwoGap = StationNominalZpos( is+1,ia )-
     &                   StationNominalZpos( is,ia )-
     &                   StationTwoThickness+
     &                   StationTwoFoilThickness
          par_pgon(1)=22.5
          par_pgon(2)=360.
          par_pgon(3)=8.
          par_pgon(4)=2.
          par_pgon(5)=-StationTwoThickness/2.
     &               -0.1   ! overlap studies
          par_pgon(6)=StationTwoFFrame(1,ia)-
     &            sqrt(StationTwoFFrame(6,ia)**2.+
     &                 StationTwoFFrame(7,ia)**2.)
          par_pgon(7)=StationTwoFFrame(2,ia)-
     &            sqrt(StationTwoFFrame(6,ia)**2.+
     &                 StationTwoFFrame(7,ia)**2.) +3.0
          par_pgon(8)=par_pgon(5)+StationTwoThickness
     &             +0.2   ! overlap studies
          par_pgon(9)=par_pgon(6)
          par_pgon(10)=par_pgon(7)
          write(StationNamef,'(a2,2i1)')'MS',ia,is
          call gsvolu(StationNamef,'PGON',
     &        ArmMedium,par_pgon,10,ivolu)
          if ( mum_color .gt. 0 ) then
            call gsatt( StationNamef, 'SEEN', 1 )
            call gsatt( StationNamef, 'COLO', mum_color)
          else
            call gsatt( StationNamef, 'SEEN', 0 )
          endif
          par_pgon(1)=22.5
          par_pgon(2)=360.
          par_pgon(3)=8.
          par_pgon(4)=2.
          par_pgon(5)=-StationTwoThickness/2.
     &                -0.5     ! overlap studies HvH
          par_pgon(6)=StationTwoFFrame(1,ia)-
     &            sqrt(StationTwoFFrame(6,ia)**2.+
     &                 StationTwoFFrame(7,ia)**2.)
          par_pgon(7)=StationTwoBFrame(1,ia)-
     &            sqrt(StationTwoFFrame(6,ia)**2.+
     &                 StationTwoFFrame(7,ia)**2.) +3.0
          par_pgon(8)=par_pgon(5)+StationTwoThickness 
     &               +1.0     ! overlap studies HvH
          par_pgon(9)=par_pgon(6)
          par_pgon(10)=par_pgon(7)
          write(StationNameb,'(a2,2i1)')'MZ',ia,is
          call gsvolu(StationNameb,'PGON',
     &        ArmMedium,par_pgon,10,ivolu)
          if ( mum_color .gt. 0 ) then
            call gsatt( StationNameb, 'SEEN', 1 )
            call gsatt( StationNameb, 'COLO', mum_color)
          else
            call gsatt( StationNameb, 'SEEN', 0 )
          endif

C  For Station 2, First make an aluminum trapezoidal volume that will
C  eventually be the chamber frame.  Then fill the center of this volume
C  with a TUBS shaped air volume.  Make two different size chambers
C  to represent the "front" chambers and the "back" chambers at station 2.

C  Front octant volume:

          par_pgon(1)=0.
          par_pgon(2)=45.0
          par_pgon(3)=1.
          par_pgon(4)=2.
          par_pgon(5)=-StationTwoThickness/2.
          par_pgon(6)=StationTwoFFrame(1,ia)
cyy     &           -sqrt(StationTwoFFrame(6,ia)**2.+
cyy     &                 StationTwoFFrame(7,ia)**2.)
          par_pgon(7)=StationTwoFFrame(2,ia)
cyy     &           -sqrt(StationTwoFFrame(6,ia)**2.+
cyy     &                 StationTwoFFrame(7,ia)**2.)
          par_pgon(8)=par_pgon(5)+StationTwoThickness
          par_pgon(9)=par_pgon(6)
          par_pgon(10)=par_pgon(7)
          write(OctantNamef,'(a2,2i1)')'MP',ia,is
          call gsvolu(OctantNamef, 'PGON',
     &         ArmMedium, par_pgon, 10, ivolu)
          if ( mum_color .gt. 0 ) then
            call gsatt( OctantNamef, 'SEEN', 1 )
            call gsatt( OctantNamef, 'COLO', mum_color)
          else
            call gsatt( OctantNamef, 'SEEN', 0 )
          endif

C  Back octant volume:

          par_pgon(1)=0.
          par_pgon(2)=45.0
          par_pgon(3)=1.
          par_pgon(4)=2.
          par_pgon(5)=-StationTwoThickness/2.
          par_pgon(6)=StationTwoFFrame(1,ia)
          par_pgon(7)=StationTwoBFrame(1,ia)
          par_pgon(8)=par_pgon(5)+StationTwoThickness
          par_pgon(9)=par_pgon(6)
          par_pgon(10)=par_pgon(7)
          write(OctantNameb,'(a2,2i1)')'MQ',ia,is
          call gsvolu(OctantNameb, 'PGON',
     &         ArmMedium, par_pgon, 10, ivolu)
          if ( mum_color .gt. 0 ) then
            call gsatt( OctantNameb, 'SEEN', 1 )
            call gsatt( OctantNameb, 'COLO', mum_color)
          else
            call gsatt( OctantNameb, 'SEEN', 0 )
          endif

C  Now make front frame out of aluminum.

          par_pgon(1)=0.0
          par_pgon(2)=45.
          par_pgon(3)=1.
          par_pgon(4)=2.
          par_pgon(5)=-StationTwoFFrame(5,ia)/2.
          par_pgon(6)=StationTwoFFrame(1,ia)
          par_pgon(7)=StationTwoFFrame(2,ia)
          par_pgon(8)=par_pgon(5)+StationTwoFFrame(5,ia)
          par_pgon(9)=par_pgon(6)
          par_pgon(10)=par_pgon(7)
          write(FrameName,'(a2,2i1)')'MF',ia,is
          call gsvolu(FrameName, 'PGON',
     &         FrameMedium(is,ia), par_pgon, 10, ivolu)
          if ( mum_color .gt. 0 ) then
            call gsatt( FrameName, 'SEEN', 1 )
            call gsatt( FrameName, 'COLO', mum_color)
          else
            call gsatt( FrameName, 'SEEN', 0 )
          endif

C  Now make front frame hole out of air.

          par_tubs(1)=StationTwoFFrame(3,ia)
          par_tubs(2)=StationTwoFFrame(4,ia)
          par_tubs(3)=StationTwoFFrame(5,ia)/2.
          par_tubs(4)=0.0
          par_tubs(5)=45.
          write(Frame2Name,'(a2,2i1)')'MI',ia,is
          call gsvolu(Frame2Name, 'TUBS',
     &         ArmMedium, par_tubs, 5, ivolu)
          if ( mum_color .gt. 0 ) then
            call gsatt( Frame2Name, 'SEEN', 1 )
            call gsatt( Frame2Name, 'COLO', mum_color)
          else
            call gsatt( Frame2Name, 'SEEN', 0 )
          endif

C  Position hole in front frame.

          xpos=-2.*StationTwoFFrame(6,ia)
          ypos=-2.*StationTwoFFrame(7,ia)
          call gspos(Frame2Name, 1, FrameName,
     &          xpos, ypos, 0.0, 0, 'ONLY')

C  Now do the same for the back frame.

          par_pgon(1)=0.0
          par_pgon(2)=45.
          par_pgon(3)=1.
          par_pgon(4)=2.
          par_pgon(5)=-StationTwoFFrame(5,ia)/2.
          par_pgon(6)=StationTwoFFrame(1,ia)
          par_pgon(7)=StationTwoBFrame(1,ia)
          par_pgon(8)=par_pgon(5)+StationTwoFFrame(5,ia)
          par_pgon(9)=par_pgon(6)
          par_pgon(10)=par_pgon(7)
          write(Frame3Name,'(a2,2i1)')'MA',ia,is
          call gsvolu(Frame3Name, 'PGON',
     &         FrameMedium(is,ia), par_pgon, 10, ivolu)
          if ( mum_color .gt. 0 ) then
            call gsatt( Frame3Name, 'SEEN', 1 )
            call gsatt( Frame3Name, 'COLO', mum_color)
          else
            call gsatt( Frame3Name, 'SEEN', 0 )
          endif

C  Now make back frame hole out of air.

          par_tubs(1)=StationTwoFFrame(3,ia)
          par_tubs(2)=StationTwoBFrame(2,ia)
          par_tubs(3)=StationTwoFFrame(5,ia)/2.
          par_tubs(4)=0.0
          par_tubs(5)=45.
          write(Frame4Name,'(a2,2i1)')'ML',ia,is
          call gsvolu(Frame4Name, 'TUBS',
     &         ArmMedium, par_tubs, 5, ivolu)
          if ( mum_color .gt. 0 ) then
            call gsatt( Frame4Name, 'SEEN', 1 )
            call gsatt( Frame4Name, 'COLO', mum_color)
          else
            call gsatt( Frame4Name, 'SEEN', 0 )
          endif

C  Position hole in back frame.

          xpos=-2.*StationTwoFFrame(6,ia)
          ypos=-2.*StationTwoFFrame(7,ia)
          call gspos(Frame4Name, 1, Frame3Name,
     &          xpos, ypos, 0.0, 0, 'ONLY')

C  Now make front bar.

          par_pgon(1)=0.0
          par_pgon(2)=45.
          par_pgon(3)=1.
          par_pgon(4)=2.
          par_pgon(5)=-StationTwoFFrame(5,ia)/2.
          par_pgon(6)=StationTwoFBar(1,ia)
          par_pgon(7)=StationTwoFBar(2,ia)
          par_pgon(8)=par_pgon(5)+StationTwoFFrame(5,ia)
          par_pgon(9)=par_pgon(6)
          par_pgon(10)=par_pgon(7)
          write(BarNamef,'(a3,i1)')'MBF',ia
          call gsvolu(BarNamef, 'PGON',
     &         FrameMedium(is,ia), par_pgon, 10, ivolu)
          if ( mum_color .gt. 0 ) then
            call gsatt( BarNamef, 'SEEN', 1 )
            call gsatt( BarNamef, 'COLO', mum_color)
          else
            call gsatt( BarNamef, 'SEEN', 0 )
          endif

C  Put bar in front frame hole.

          xpos=0.0
          ypos=0.0
          call gspos(BarNamef, 1, Frame2Name,
     &          xpos, ypos, 0.0, 0, 'ONLY')

C  Now make back bar.

          par_pgon(1)=0.0
          par_pgon(2)=45.
          par_pgon(3)=1.
          par_pgon(4)=2.
          par_pgon(5)=-StationTwoFFrame(5,ia)/2.
          par_pgon(6)=StationTwoBBar(1,ia)
          par_pgon(7)=StationTwoBBar(2,ia)
          par_pgon(8)=par_pgon(5)+StationTwoFFrame(5,ia)
          par_pgon(9)=par_pgon(6)
          par_pgon(10)=par_pgon(7)
          write(BarNameb,'(a3,i1)')'MBB',ia
          call gsvolu(BarNameb, 'PGON',
     &         FrameMedium(is,ia), par_pgon, 10, ivolu)
          if ( mum_color .gt. 0 ) then
            call gsatt( BarNameb, 'SEEN', 1 )
            call gsatt( BarNameb, 'COLO', mum_color)
          else
            call gsatt( BarNameb, 'SEEN', 0 )
          endif

C  Put bar in back frame hole.

          xpos=0.0
          ypos=0.0
          call gspos(BarNameb, 1, Frame4Name,
     &          xpos, ypos, 0.0, 0, 'ONLY')

C  Now make the front spacer planes.

          par_pgon(1)=0.0
          par_pgon(2)=45.
          par_pgon(3)=1.
          par_pgon(4)=2.
          par_pgon(5)=-2.*StationTwoAnode(ia)/2.
          par_pgon(6)=StationTwoFFrame(1,ia)
          par_pgon(7)=StationTwoFFrame(2,ia)
          par_pgon(8)=par_pgon(5)+2.*StationTwoAnode(ia)
          par_pgon(9)=par_pgon(6)
          par_pgon(10)=par_pgon(7)
          write(SpaceNamef,'(a2,2i1)')'MJ',ia,is
          call gsvolu(SpaceNamef, 'PGON',
     &         FrameMedium(is,ia), par_pgon, 10, ivolu)
          if ( mum_color .gt. 0 ) then
            call gsatt( SpaceNamef, 'SEEN', 1 )
            call gsatt( SpaceNamef, 'COLO', mum_color)
          else
            call gsatt( SpaceNamef, 'SEEN', 0 )
          endif

C  Now make front spacer hole out of station medium (gas).

          par_tubs(1)=StationTwoFFrame(3,ia)
          par_tubs(2)=StationTwoFFrame(4,ia)
          par_tubs(3)=2.*StationTwoAnode(ia)/2.
          par_tubs(4)=0.0
          par_tubs(5)=45.
          write(Space2Namef,'(a2,2i1)')'MK',ia,is
          call gsvolu(Space2Namef, 'TUBS',
     &         ArmMedium, par_tubs, 5, ivolu)
          if ( mum_color .gt. 0 ) then
            call gsatt( Space2Namef, 'SEEN', 1 )
            call gsatt( Space2Namef, 'COLO', mum_color)
          else
            call gsatt( Space2Namef, 'SEEN', 0 )
          endif

C  Position hole in front spacer.

          xpos=-2.*StationTwoFFrame(6,ia)
          ypos=-2.*StationTwoFFrame(7,ia)
          call gspos(Space2Namef, 1, SpaceNamef,
     &          xpos, ypos, 0.0, 0, 'ONLY')

C  Now make the back spacer planes.

          par_pgon(1)=0.0
          par_pgon(2)=45.
          par_pgon(3)=1.
          par_pgon(4)=2.
          par_pgon(5)=-2*StationTwoAnode(ia)/2.
          par_pgon(6)=StationTwoFFrame(1,ia)
          par_pgon(7)=StationTwoBFrame(1,ia)
          par_pgon(8)=par_pgon(5)+2*StationTwoAnode(ia)
          par_pgon(9)=par_pgon(6)
          par_pgon(10)=par_pgon(7)
          write(SpaceNameb,'(a2,2i1)')'MC',ia,is
          call gsvolu(SpaceNameb, 'PGON',
     &         FrameMedium(is,ia), par_pgon, 10, ivolu)
          if ( mum_color .gt. 0 ) then
            call gsatt( SpaceNameb, 'SEEN', 1 )
            call gsatt( SpaceNameb, 'COLO', mum_color)
          else
            call gsatt( SpaceNameb, 'SEEN', 0 )
          endif

C  Now make back spacer hole out of station medium (gas).

          par_tubs(1)=StationTwoFFrame(3,ia)
          par_tubs(2)=StationTwoBFrame(2,ia)
          par_tubs(3)=2*StationTwoAnode(ia)/2.
          par_tubs(4)=0.0
          par_tubs(5)=45.
          write(Space2Nameb,'(a2,2i1)')'MD',ia,is
          call gsvolu(Space2Nameb, 'TUBS',
     &         ArmMedium, par_tubs, 5, ivolu)
          if ( mum_color .gt. 0 ) then
            call gsatt( Space2Nameb, 'SEEN', 1 )
            call gsatt( Space2Nameb, 'COLO', mum_color)
          else
            call gsatt( Space2Nameb, 'SEEN', 0 )
          endif

C  Position hole in back spacer.

          xpos=-2.*StationTwoFFrame(6,ia)
          ypos=-2.*StationTwoFFrame(7,ia)
          call gspos(Space2Nameb, 1, SpaceNameb,
     &          xpos, ypos, 0.0, 0, 'ONLY')

C  Now make mylar foils and shield for front octant.

          par_tubs(1)=StationTwoFFrame(3,ia)
          par_tubs(2)=StationTwoFFrame(4,ia)
          par_tubs(3)=StationTwoFoilThickness/2.
          par_tubs(4)=0.0
          par_tubs(5)=45.
          write(MylarName,'(a2,2i1)')'AS',ia,is
          call gsvolu(MylarName, 'TUBS',
     &         25, par_tubs, 5, ivolu)
          if ( mum_color .gt. 0 ) then
            call gsatt( MylarName, 'SEEN', 1 )
            call gsatt( MylarName, 'COLO', mum_color)
          else
            call gsatt( MylarName, 'SEEN', 0 )
          endif

          par_tubs(1)=StationTwoFFrame(3,ia)
          par_tubs(2)=StationTwoFFrame(4,ia)
          par_tubs(3)=StationTwoAlFoilThickness/2.
          par_tubs(4)=0.0
          par_tubs(5)=45.
          !write(ShieldFoilName,'(a2,2i1)')'AS',ia,is
          write(ShieldFoilName,'(a2,2i1)')'SFS',ia,is
          call gsvolu(ShieldFoilName, 'TUBS',
     &         26, par_tubs, 5, ivolu)
          if ( mum_color .gt. 0 ) then
            call gsatt( ShieldFoilName, 'SEEN', 1 )
            call gsatt( ShieldFoilName, 'COLO', mum_color)
          else
            call gsatt( ShieldFoilName, 'SEEN', 0 )
          endif

C  Now make mylar foils and shield for back octant.

          par_tubs(1)=StationTwoFFrame(3,ia)
          par_tubs(2)=StationTwoBFrame(2,ia)
          par_tubs(3)=StationTwoFoilThickness/2.
          par_tubs(4)=0.0
          par_tubs(5)=45.
          write(Mylar2Name,'(a2,2i1)')'MH',ia,is
          call gsvolu(Mylar2Name, 'TUBS',
     &         25, par_tubs, 5, ivolu)
          if ( mum_color .gt. 0 ) then
            call gsatt( Mylar2Name, 'SEEN', 1 )
            call gsatt( Mylar2Name, 'COLO', mum_color)
          else
            call gsatt( Mylar2Name, 'SEEN', 0 )
          endif

          par_tubs(1)=StationTwoFFrame(3,ia)
          par_tubs(2)=StationTwoBFrame(2,ia)
          par_tubs(3)=StationTwoAlFoilThickness/2.
          par_tubs(4)=0.0
          par_tubs(5)=45.
          write(ShieldFoil2Name,'(a2,2i1)')'AT',ia,is
          call gsvolu(ShieldFoil2Name, 'TUBS',
     &         26, par_tubs, 5, ivolu)
          if ( mum_color .gt. 0 ) then
            call gsatt( ShieldFoil2Name, 'SEEN', 1 )
            call gsatt( ShieldFoil2Name, 'COLO', mum_color)
          else
            call gsatt( ShieldFoil2Name, 'SEEN', 0 )
          endif

C  Now make the front foil planes.

          par_pgon(1)=0.0
          par_pgon(2)=45.
          par_pgon(3)=1.
          par_pgon(4)=2.
          par_pgon(5)=-StationTwoAnode(ia)/2.
          par_pgon(6)=StationTwoFFrame(1,ia)
          par_pgon(7)=StationTwoFFrame(2,ia)
          par_pgon(8)=par_pgon(5)+StationTwoAnode(ia)
          par_pgon(9)=par_pgon(6)
          par_pgon(10)=par_pgon(7)
          write(FoilNamef,'(a2,2i1)')'MV',ia,is
          call gsvolu(FoilNamef, 'PGON',
     &         FrameMedium(is,ia), par_pgon, 10, ivolu)
          if ( mum_color .gt. 0 ) then
            call gsatt( FoilNamef, 'SEEN', 1 )
            call gsatt( FoilNamef, 'COLO', mum_color)
          else
            call gsatt( FoilNamef, 'SEEN', 0 )
          endif

C  Now make front foil hole out of station medium (gas).

          par_tubs(1)=StationTwoFFrame(3,ia)
          par_tubs(2)=StationTwoFFrame(4,ia)
          par_tubs(3)=StationTwoAnode(ia)/2.
          par_tubs(4)=0.0
          par_tubs(5)=45.
          write(Foil2Namef,'(a2,2i1)')'MW',ia,is
          call gsvolu(Foil2Namef, 'TUBS',
     &         ArmMedium, par_tubs, 5, ivolu)
          if ( mum_color .gt. 0 ) then
            call gsatt( Foil2Namef, 'SEEN', 1 )
            call gsatt( Foil2Namef, 'COLO', mum_color)
          else
            call gsatt( Foil2Namef, 'SEEN', 0 )
          endif

C  Position hole in front foil.

          xpos=-2.*StationTwoFFrame(6,ia)
          ypos=-2.*StationTwoFFrame(7,ia)
          call gspos(Foil2Namef, 1, FoilNamef,
     &          xpos, ypos, 0.0, 0, 'ONLY')

C  Position mylar foil in front foil hole.

          xpos=0.0
          ypos=0.0
          zpos2=-StationTwoAnode(ia)/2.+StationTwoFoilThickness/2.
          call gspos(MylarName, 1, Foil2Namef,
     &          xpos, ypos, zpos2, 0, 'ONLY')

C  Now make the back foil planes.

          par_pgon(1)=0.0
          par_pgon(2)=45.
          par_pgon(3)=1.
          par_pgon(4)=2.
          par_pgon(5)=-StationTwoAnode(ia)/2.
          par_pgon(6)=StationTwoFFrame(1,ia)
          par_pgon(7)=StationTwoBFrame(1,ia)
          par_pgon(8)=par_pgon(5)+StationTwoAnode(ia)
          par_pgon(9)=par_pgon(6)
          par_pgon(10)=par_pgon(7)
          write(FoilNameb,'(a2,2i1)')'MG',ia,is
          call gsvolu(FoilNameb, 'PGON',
     &         FrameMedium(is,ia), par_pgon, 10, ivolu)
          if ( mum_color .gt. 0 ) then
            call gsatt( FoilNameb, 'SEEN', 1 )
            call gsatt( FoilNameb, 'COLO', mum_color)
          else
            call gsatt( FoilNameb, 'SEEN', 0 )
          endif

C  Now make back foil hole out of station medium (gas).

          par_tubs(1)=StationTwoFFrame(3,ia)
          par_tubs(2)=StationTwoBFrame(2,ia)
          par_tubs(3)=StationTwoAnode(ia)/2.
          par_tubs(4)=0.0
          par_tubs(5)=45.
          write(Foil2Nameb,'(a2,2i1)')'MR',ia,is
          call gsvolu(Foil2Nameb, 'TUBS',
     &         ArmMedium, par_tubs, 5, ivolu)
          if ( mum_color .gt. 0 ) then
            call gsatt( Foil2Nameb, 'SEEN', 1 )
            call gsatt( Foil2Nameb, 'COLO', mum_color)
          else
            call gsatt( Foil2Nameb, 'SEEN', 0 )
          endif

C  Position hole in back foil.

          xpos=-2.*StationTwoFFrame(6,ia)
          ypos=-2.*StationTwoFFrame(7,ia)
          call gspos(Foil2Nameb, 1, FoilNameb,
     &          xpos, ypos, 0.0, 0, 'ONLY')

C  Position mylar foil in back foil hole.

          xpos=0.0
          ypos=0.0
          zpos2=-StationTwoAnode(ia)/2.+StationTwoFoilThickness/2.
          call gspos(Mylar2Name, 1, Foil2Nameb,
     &          xpos, ypos, zpos2, 0, 'ONLY')

C  Now make front rib.

          par_box(1)=StationTwoFRib(2,ia)/2.
     &               -1.706      ! overlap studies HvH
          par_box(2)=StationTwoFRib(1,ia)/2.
          par_box(3)=2.*StationTwoAnode(ia)
          write(Rib22Name,'(a2,2i1)')'MB',ia,is
          call gsvolu(Rib22Name ,'BOX ',
     &         FrameMedium(is,ia), par_box, 3, ivolu)
          if ( mum_color .gt. 0 ) then
            call gsatt( Rib22Name, 'SEEN', 1 )
            call gsatt( Rib22Name, 'COLO', mum_color)
          else
            call gsatt( Rib22Name, 'SEEN', 0 )
          endif

C  Now make back rib.

          par_box(1)=StationTwoBRib(2,ia)/2.
     &            -1.706      ! overlap studies HvH
          par_box(2)=StationTwoBRib(1,ia)/2.
          par_box(3)=2.*StationTwoAnode(ia)
          write(Rib45Name,'(a2,2i1)')'ME',ia,is
          call gsvolu(Rib45Name ,'BOX ',
     &         FrameMedium(is,ia), par_box, 3, ivolu)
          if ( mum_color .gt. 0 ) then
            call gsatt( Rib45Name, 'SEEN', 1 )
            call gsatt( Rib45Name, 'COLO', mum_color)
          else
            call gsatt( Rib45Name, 'SEEN', 0 )
          endif

C  Now put all elements together in the octants. Start with frame,
C  a foil plane, and a spacer plane.

          ifoil = 0
          ispace = 0
          xpos = 0    ! overlap studies HvH
          ypos = 0
          zpos2 = StationTwoThickness/2.-StationTwoFFrame(5,ia)/2.
          zpos3 = zpos2-StationTwoFFrame(5,ia)/2.
          call gspos( FrameName, 1, OctantNamef,
     &               xpos, ypos, zpos2, 0, 'ONLY' )
          call gspos( Frame3Name, 1, OctantNameb,
     &               xpos, ypos, zpos2, 0, 'ONLY' )

          ifoil = ifoil+1
          zpos2 = zpos3-StationTwoAnode(ia)/2.
          zpos3 = zpos2-StationTwoAnode(ia)/2.
          call gspos( FoilNamef, ifoil, OctantNamef,
     &               xpos, ypos, zpos2, irot45f, 'ONLY' )
          call gspos( FoilNameb, ifoil, OctantNameb,
     &               0.  , 0.  , zpos2, irot45f, 'ONLY' )
chvh &              xpos, ypos, zpos2, irot45f, 'ONLY' )

          ifoil = ifoil+1
          zpos2 = zpos3-StationTwoAnode(ia)/2.
          zpos3 = zpos2-StationTwoAnode(ia)/2.
          call gspos( FoilNamef, ifoil, OctantNamef,
     &               xpos, ypos, zpos2, 0, 'ONLY' )
          call gspos( FoilNameb, ifoil, OctantNameb,
     &               0.  , 0.  , zpos2, 0, 'ONLY' )
chvh &               xpos, ypos, zpos2, 0, 'ONLY' )

C  Now loop over planes in station 2.

          do ip = 1, PlanesPerStation( is,ia )

C  MT plane name, e.g. MT42 is 2nd arm, 1st station, 2nd chamber

            par_pgon(1)=0.0
            par_pgon(2)=22.5
            par_pgon(3)=1.
            par_pgon(4)=2.
            par_pgon(5)=-2*StationTwoAnode(ia)
            par_pgon(6)=StationTwoFGas(1,ia)
            par_pgon(7)=StationTwoFGas(2,ia)
            par_pgon(8)=par_pgon(5)+4*StationTwoAnode(ia)
            par_pgon(9)=par_pgon(6)
            par_pgon(10)=par_pgon(7)
            write(GasName,'(a2,2i1)')'MT',ns,ip
            call gsvolu(GasName, 'PGON',
     &           StationMedium, par_pgon, 10, ivolu)
            if ( mum_color .gt. 0 ) then
              call gsatt( GasName, 'SEEN', 1 )
              call gsatt( GasName, 'COLO', mum_color)
            else
              call gsatt( GasName, 'SEEN', 0 )
            endif            

C  Now back gas.

            par_pgon(1)=0.0
            par_pgon(2)=22.5
            par_pgon(3)=1.
            par_pgon(4)=2.
            par_pgon(5)=-2*StationTwoAnode(ia)
            par_pgon(6)=StationTwoFGas(1,ia)
            par_pgon(7)=StationTwoBGas(ia)
            par_pgon(8)=par_pgon(5)+4*StationTwoAnode(ia)
            par_pgon(9)=par_pgon(6)
            par_pgon(10)=par_pgon(7)
            write(Gas2Name,'(a2,2i1)')'MU',ns,ip
            call gsvolu(Gas2Name, 'PGON',
     &           StationMedium, par_pgon, 10, ivolu)
            if ( mum_color .gt. 0 ) then
              call gsatt( Gas2Name, 'SEEN', 1 )
              call gsatt( Gas2Name, 'COLO', mum_color)
            else
              call gsatt( Gas2Name, 'SEEN', 0 )
            endif

C  Put volume elements together into sensitive set if requested.

            if ( cvolu_opt( 1, 10 ) .eq. 'FULL' ) then
              set_id          = 'MUM '        ! put it in a SET
              namesv( 1 )     = ArmName
              namesv( 2 )     = StationNamef
              namesv( 3 )     = OctantNamef
              namesv( 4 )     = AnodeName  
              namesv( 5 )     = Anode2Name  
              nbitsv( 1 )     = 4
              nwpa            = 200           ! for now
              nwsa            = 200           ! for now
              call gsdet( set_id, GasName, 5, namesv,
     &          nbitsv, idtype, nwpa, nwsa, iset, idet )
              call gsdeth( set_id, GasName, nh, namesh,
     &          nbitmu, origmu, factmu)
              set_id          = 'MUM '        ! put it in a SET
              namesv( 1 )     = ArmName
              namesv( 2 )     = StationNameb
              namesv( 3 )     = OctantNameb 
              namesv( 4 )     = Anode3Name  
              namesv( 5 )     = Anode4Name  
              nbitsv( 1 )     = 4
              nwpa            = 200           ! for now
              nwsa            = 200           ! for now
              call gsdet( set_id, Gas2Name, 5, namesv,
     &          nbitsv, idtype, nwpa, nwsa, iset, idet )
              call gsdeth( set_id, Gas2Name, nh, namesh,
     &          nbitmu, origmu, factmu)
            end if

C  Now make the front anode planes.

            par_pgon(1)=0.0
            par_pgon(2)=45.
            par_pgon(3)=1.
            par_pgon(4)=2.
            par_pgon(5)=-2.*StationTwoAnode(ia)
            par_pgon(6)=StationTwoFFrame(1,ia)
            par_pgon(7)=StationTwoFFrame(2,ia)
            par_pgon(8)=par_pgon(5)+4.*StationTwoAnode(ia)
            par_pgon(9)=par_pgon(6)
            par_pgon(10)=par_pgon(7)
            write(AnodeName,'(a2,2i1)')'MN',ns,ip
            call gsvolu(AnodeName, 'PGON',
     &           FrameMedium(is,ia), par_pgon, 10, ivolu)
            if ( mum_color .gt. 0 ) then
              call gsatt( AnodeName, 'SEEN', 1 )
              call gsatt( AnodeName, 'COLO', mum_color)
            else
              call gsatt( AnodeName, 'SEEN', 0 )
            endif

C  Now make front anode hole out of station medium (gas).

            par_tubs(1)=StationTwoFFrame(3,ia)
            par_tubs(2)=StationTwoFFrame(4,ia)
            par_tubs(3)=2.*StationTwoAnode(ia)
            par_tubs(4)=0.0
            par_tubs(5)=45.
            write(Anode2Name,'(a2,2i1)')'MO',ns,ip
            call gsvolu(Anode2Name, 'TUBS',
     &           ArmMedium, par_tubs, 5, ivolu)
            if ( mum_color .gt. 0 ) then
              call gsatt( Anode2Name, 'SEEN', 1 )
              call gsatt( Anode2Name, 'COLO', mum_color)
            else
              call gsatt( Anode2Name, 'SEEN', 0 )
            endif

C  Position hole in front anode.

            xpos=-2.*StationTwoFFrame(6,ia)
            ypos=-2.*StationTwoFFrame(7,ia)
            call gspos(Anode2Name, 1, AnodeName,
     &            xpos, ypos, 0.0, 0, 'ONLY')

C  Now make the back anode planes.

            par_pgon(1)=0.0
            par_pgon(2)=45.
            par_pgon(3)=1.
            par_pgon(4)=2.
            par_pgon(5)=-2.*StationTwoAnode(ia)
            par_pgon(6)=StationTwoFFrame(1,ia)
            par_pgon(7)=StationTwoBFrame(1,ia)
            par_pgon(8)=par_pgon(5)+4.*StationTwoAnode(ia)
            par_pgon(9)=par_pgon(6)
            par_pgon(10)=par_pgon(7)
            write(Anode3Name,'(a2,2i1)')'MX',ns,ip
            call gsvolu(Anode3Name, 'PGON',
     &           FrameMedium(is,ia), par_pgon, 10, ivolu)
            if ( mum_color .gt. 0 ) then
              call gsatt( Anode3Name, 'SEEN', 1 )
              call gsatt( Anode3Name, 'COLO', mum_color)
            else
              call gsatt( Anode3Name, 'SEEN', 0 )
            endif

C  Now make back anode hole out of station medium (gas).

            par_tubs(1)=StationTwoFFrame(3,ia)
            par_tubs(2)=StationTwoBFrame(2,ia)
            par_tubs(3)=2.*StationTwoAnode(ia)
            par_tubs(4)=0.0
            par_tubs(5)=45.
            write(Anode4Name,'(a2,2i1)')'MY',ns,ip
            call gsvolu(Anode4Name, 'TUBS',
     &           ArmMedium, par_tubs, 5, ivolu)
            if ( mum_color .gt. 0 ) then
              call gsatt( Anode4Name, 'SEEN', 1 )
              call gsatt( Anode4Name, 'COLO', mum_color)
            else
              call gsatt( Anode4Name, 'SEEN', 0 )
            endif

C  Position hole in back anode.

            xpos=-2.*StationTwoFFrame(6,ia)
            ypos=-2.*StationTwoFFrame(7,ia)
            call gspos(Anode4Name, 1, Anode3Name,
     &            xpos, ypos, 0.0, 0, 'ONLY')

C  Now put in ribs in anode holes.

            xpos = StationTwoFRib(3,ia)
            ypos = StationTwoFRib(4,ia)
            zpos2 = 0.0
            call gspos( Rib22Name, 1, Anode2Name,
     &                 xpos, ypos, zpos2, irot22, 'ONLY' )
            xpos = StationTwoBRib(3,ia)
            ypos = StationTwoBRib(4,ia)
            zpos2 = 0.0
            call gspos( Rib45Name, 1, Anode4Name,
     &                 xpos, ypos, zpos2, irot22, 'ONLY' )

C  Now put in gas plane in anode holes.

            xpos = StationTwoFGas(3,ia)+StationTwoFFrame(6,ia)
            ypos = StationTwoFGas(4,ia)+StationTwoFFrame(7,ia)
            zpos2 = 0.0
            call gspos( GasName, 1, Anode2Name, 
     &                 xpos, ypos, zpos2, 0, 'ONLY' )
            xpos = StationTwoFGas(5,ia)+StationTwoFFrame(6,ia)
            ypos = StationTwoFGas(6,ia)+StationTwoFFrame(7,ia)
            zpos2 = 0.0
            call gspos( GasName, 2, Anode2Name, 
     &                 xpos, ypos, zpos2, irot22, 'ONLY' )
            xpos = StationTwoFGas(3,ia)+StationTwoFFrame(6,ia)
            ypos = StationTwoFGas(4,ia)+StationTwoFFrame(7,ia)
            zpos2 = 0.0
            call gspos( Gas2Name, 1, Anode4Name, 
     &                 xpos, ypos, zpos2, 0, 'ONLY' )
            xpos = StationTwoFGas(5,ia)+StationTwoFFrame(6,ia)
            ypos = StationTwoFGas(6,ia)+StationTwoFFrame(7,ia)
            zpos2 = 0.0
            call gspos( Gas2Name, 2, Anode4Name, 
     &                 xpos, ypos, zpos2, irot22, 'ONLY' )

C  Now put in an anode plane.

chvh        xpos = StationTwoFFrame(6,ia)
chvh        ypos = StationTwoFFrame(7,ia)
            xpos = 0
            ypos = 0
            zpos2 = zpos3-4.*StationTwoAnode(ia)/2.
            zpos3 = zpos2-4.*StationTwoAnode(ia)/2.
            call gspos( AnodeName, 1, OctantNamef,
     &                 xpos, ypos, zpos2, 0, 'ONLY' )
            call gspos( Anode3Name, 1, OctantNameb,
     &                 xpos, ypos, zpos2, 0, 'ONLY' )

C  Now put in two foil planes.

            ifoil = ifoil+1
            zpos2 = zpos3-StationTwoAnode(ia)/2.
            zpos3 = zpos2-StationTwoAnode(ia)/2.
            call gspos( FoilNamef, ifoil, OctantNamef,
     &                 xpos, ypos, zpos2, irot45f, 'ONLY' )
            call gspos( FoilNameb, ifoil, OctantNameb,
chvh &                 xpos, ypos, zpos2, irot45f, 'ONLY' )   
     &                 0   , 0   , zpos2, irot45f, 'ONLY' )
            ifoil = ifoil+1
            zpos2 = zpos3-StationTwoAnode(ia)/2.
            zpos3 = zpos2-StationTwoAnode(ia)/2.
            call gspos( FoilNamef, ifoil, OctantNamef,
     &                 xpos, ypos, zpos2, 0, 'ONLY' )
            call gspos( FoilNameb, ifoil, OctantNameb,
chvh &                 xpos, ypos, zpos2, 0, 'ONLY' )
     &                 0   , 0   , zpos2, 0, 'ONLY' )

C  Now put in a spacer and foil if not last gap.

            if(ip.ne.PlanesPerStation( is,ia ))then
              ispace = ispace+1
chvh          xpos = StationTwoFFrame(6,ia)
chvh          ypos = StationTwoFFrame(7,ia)
              xpos = 0
              xpos = 0
              zpos2 = zpos3-2.*StationTwoAnode(ia)/2.
              zpos3 = zpos2-2.*StationTwoAnode(ia)/2.
              call gspos( SpaceNamef, ispace, OctantNamef,
     &                   xpos, ypos, zpos2, 0, 'ONLY' )
              call gspos( SpaceNameb, ispace, OctantNameb,
     &                   xpos, ypos, zpos2, 0, 'ONLY' )
              ifoil = ifoil+1
              zpos2 = zpos3-StationTwoAnode(ia)/2.
              zpos3 = zpos2-StationTwoAnode(ia)/2.
              call gspos( FoilNamef, ifoil, OctantNamef,
     &                   xpos, ypos, zpos2, 0, 'ONLY' )
              call gspos( FoilNameb, ifoil, OctantNameb,
chvh &                   xpos, ypos, zpos2, 0, 'ONLY' )
     &                   0   , 0   , zpos2, 0, 'ONLY' )
            endif

          end do                  ! loop over planes

C  Put in last frame.

chvh      xpos = StationTwoFFrame(6,ia)
chvh      ypos = StationTwoFFrame(7,ia)
          xpos = 0
          ypos = 0
          zpos2 = zpos3-StationTwoFFrame(5,ia)/2.
          zpos3 = zpos2-StationTwoFFrame(5,ia)/2.
          call gspos( FrameName, 2, OctantNamef,
     &               xpos, ypos, zpos2, 0, 'ONLY' )
          call gspos( Frame3Name, 2, OctantNameb,
     &               xpos, ypos, zpos2, 0, 'ONLY' )

C  Put in shield foil.

          xpos = -2.0*StationTwoFFrame(6,ia)
          ypos = -2.0*StationTwoFFrame(7,ia)
chvh      xpos = 0
chvh      ypos = 0
          zpos2 = zpos3-StationTwoAlFoilThickness/2.
          zpos3 = zpos2-StationTwoAlFoilThickness/2.
          call gspos( ShieldFoilName, 1, OctantNamef,
     &               xpos, ypos, zpos2, 0, 'ONLY' )
          call gspos( ShieldFoil2Name, 1, OctantNameb,
     &               xpos, ypos, zpos2, 0, 'ONLY' )

C  Position front and back octants in station volume

C  Create rotation matrices for positioning quadrants
C  in the station volume using real survey information.

          do i = 1, 7, 2           ! octants 1 3 5 7
            irot = irot + 1
            xoff = 9.916*( mod(i,5)/2-1 -2*((mod(i,5)+4)/5-1) ) ! -1  0 +1  0
            yoff = 9.916*( (i/7)*2 - mod(i,5)/2 )               !  0 +1  0 -1
            xoff  = xoff + StationTwoOffsets(1,i,ia)        ! Added the offsets Sep 2015 HvH
            yoff  = yoff + StationTwoOffsets(2,i,ia)        ! for st2 x and y
            zoff = StationTwoOffsets(3,i,ia)
            call gsrotm(irot, StationTwoAngles(1,i,ia),
     &                        StationTwoAngles(2,i,ia),
     &                        StationTwoAngles(3,i,ia),
     &                        StationTwoAngles(4,i,ia),
     &                        StationTwoAngles(5,i,ia),
     &                        StationTwoAngles(6,i,ia))
            call gspos(OctantNamef, i, StationNamef,
     &                 xoff, yoff, zoff, irot, 'ONLY' )
          end do
          do i = 2, 8, 2        ! octants 2 4 6 8 
            irot = irot + 1     ! 7.012 = .5*sqrt(2)*9.916 from above
            yoff = 7.012*( (i/5)*2-1 )         ! - - + +
            xoff = 7.012*( (mod(i,8)/4)*2-1 )  ! - + + -
            zoff = StationTwoOffsets(3,i,ia)
            call gsrotm(irot, StationTwoAngles(1,i,ia),
     &                        StationTwoAngles(2,i,ia),
     &                        StationTwoAngles(3,i,ia),
     &                        StationTwoAngles(4,i,ia),
     &                        StationTwoAngles(5,i,ia),
     &                        StationTwoAngles(6,i,ia))
            call gspos(OctantNameb, i, StationNameb,
     &                 xoff, yoff, zoff, irot, 'ONLY' )
          end do

C  Position Station Two in Muon Arm. zpos is the z position of the
C  front of the frame of the front station two.

          if(ia.eq.1) then
            zpos2 = iaa *(StationNominalZpos( is,ia )
     &                      +StationTwoThickness/2.)
            call gspos( StationNamef, 1, ArmName,
     &          0.0, 0.0, zpos2, irot180f, 'ONLY' )
            zpos2 = iaa *(StationNominalZpos( is+1,ia )
     &                      +StationTwoThickness/2.)
            call gspos( StationNameb, 1, ArmName,
     &          0.0, 0.0, zpos2, irot180f, 'ONLY' )
          elseif(ia.eq.2) then
            zpos2 = iaa *(StationNominalZpos( is,ia )
     &                      +StationTwoThickness/2.)
            call gspos( StationNamef, 1, ArmName,
     &          0.0, 0.0, zpos2, 0, 'ONLY' )
            zpos2 = iaa *(StationNominalZpos( is+1,ia )
     &                      +StationTwoThickness/2.)
            call gspos( StationNameb, 1, ArmName,
     &          0.0, 0.0, zpos2, 0, 'ONLY' )
          endif

C  Build Station Three chambers.

          is = 3
          ns = mum_stations * ( ia - 1 ) + is
          zpos = iaa * StationNominalZpos( is+1,ia )

C  Muon station volume (octagon).

          StationThreeThickness = 2*StationThreeFrame(5,ia)+
     &                            (PlanesPerStation(is,ia)+1)*
     &                               StationThreePanel(3,ia)+
     &                            PlanesPerStation(is,ia)*
     &                               StationThreeAnode(3,ia)
          par_pgon(1)=22.5
          par_pgon(2)=360.
          par_pgon(3)=16.
          par_pgon(4)=2.         ! make the mother volume thicket to allow for z offsets:
          par_pgon(5)=-StationThreeThickness/2. 
     &               -1.0     ! overlap studies HvH
          par_pgon(6)=StationThreeFrame(1,ia)
          par_pgon(7)=StationThreeFrame(2,ia)+     ! add these bits Mar '10 HvH:
     &                (StationThreeFrame(6,ia)-StationThreeFrame(8,ia))+
     &                (StationThreeFrame(1,ia)-StationThreeFrame(3,ia))
     &                +2.599                       ! from overlap studies HvH
          par_pgon(8)=par_pgon(5)+StationThreeThickness
     &               +2.0
          par_pgon(9)=par_pgon(6)
          par_pgon(10)=par_pgon(7)
          write(StationName,'(a2,2i1)')'MS',ia,is
          call gsvolu(StationName,'PGON',
     &      ArmMedium,par_pgon,10,ivolu)
          if ( mum_color .gt. 0 ) then
            call gsatt( StationName, 'SEEN', 1 )
            call gsatt( StationName, 'COLO', mum_color)
          else
            call gsatt( StationName, 'SEEN', 0 )
          endif
          zpos2=zpos+iaa*(StationThreeThickness/2.)
          if(ia.eq.1) then
            call gspos( StationName, 1, ArmName,
     &          0.0, 0.0, zpos2, irot225f, 'ONLY' )
          elseif(ia.eq.2) then
            call gspos( StationName, 1, ArmName,
     &          0.0, 0.0, zpos2, 0, 'ONLY' )
          endif

C  Make octant volume out of air, then fill this with
C  honeycomb planes, sensitive gas volumes, spokes, etc.

          par_pgon(1)=0.0
          par_pgon(2)=45.
          par_pgon(3)=2.
          par_pgon(4)=2.
          par_pgon(5)=-StationThreeThickness/2.
          par_pgon(6)=StationThreeFrame(1,ia)
          par_pgon(7)=StationThreeFrame(2,ia)+
     &                (StationThreeFrame(6,ia)-StationThreeFrame(8,ia))+
     &                (StationThreeFrame(1,ia)-StationThreeFrame(3,ia))
     &               +2.599      ! from overlap studies HvH
          par_pgon(8)=par_pgon(5)+StationThreeThickness
          par_pgon(9)=par_pgon(6)
          par_pgon(10)=par_pgon(7)
          write(OctantName,'(a2,2i1)')'MQ',ia,is
          call gsvolu(OctantName, 'PGON',
     &         ArmMedium, par_pgon, 10, ivolu)
          if ( mum_color .gt. 0 ) then
            call gsatt( OctantName, 'SEEN', 1 )
            call gsatt( OctantName, 'COLO', mum_color)
          else
            call gsatt( OctantName, 'SEEN', 0 )
          endif

C  Make Copper frame.

          par_pgon(1)=0.0
          par_pgon(2)=45.
          par_pgon(3)=2.
          par_pgon(4)=2.
          par_pgon(5)=-StationThreeFrame(5,ia)/2.
          par_pgon(6)=StationThreeFrame(1,ia)
          par_pgon(7)=StationThreeFrame(2,ia)
          par_pgon(8)=par_pgon(5)+StationThreeFrame(5,ia)
          par_pgon(9)=par_pgon(6)
          par_pgon(10)=par_pgon(7)
          write(FrameName,'(a2,2i1)')'MF',ia,is
          call gsvolu(FrameName, 'PGON',
     &         17, par_pgon, 10, ivolu)
          if ( mum_color .gt. 0 ) then
            call gsatt( FrameName, 'SEEN', 1 )
            call gsatt( FrameName, 'COLO', mum_color)
          else
            call gsatt( FrameName, 'SEEN', 0 )
          endif

C  Make hole in frame.

          par_pgon(1)=0.0
          par_pgon(2)=45.
          par_pgon(3)=2.
          par_pgon(4)=2.
          par_pgon(5)=-StationThreeFrame(5,ia)/2.
          par_pgon(6)=StationThreeFrame(3,ia)
          par_pgon(7)=StationThreeFrame(4,ia)
          par_pgon(8)=par_pgon(5)+StationThreeFrame(5,ia)
          par_pgon(9)=par_pgon(6)
          par_pgon(10)=par_pgon(7)
          write(Frame2Name,'(a2,2i1)')'MI',ia,is
          call gsvolu(Frame2Name, 'PGON',
     &         ArmMedium, par_pgon, 10, ivolu)
          if ( mum_color .gt. 0 ) then
            call gsatt( Frame2Name, 'SEEN', 1 )
            call gsatt( Frame2Name, 'COLO', mum_color)
          else
            call gsatt( Frame2Name, 'SEEN', 0 )
          endif

C  Position holes in frame.

          xpos = StationThreeFrame(6,ia)
          ypos = StationThreeFrame(7,ia)
          call gspos( Frame2Name, 1, FrameName,
     &          xpos, ypos, 0.0, 0, 'ONLY' )


C  Make honeycomb panels out of FR4 with entire thickness (including
C  cathode skins, glue, etc).

          par_pgon(1)=0.
          par_pgon(2)=45.
          par_pgon(3)=2.
          par_pgon(4)=2.
          par_pgon(5)=-StationThreePanel(3,ia)/2.
          par_pgon(6)=StationThreeFrame(1,ia)
          par_pgon(7)=StationThreeFrame(2,ia)
          par_pgon(8)=par_pgon(5)+StationThreePanel(3,ia)
          par_pgon(9)=par_pgon(6)
          par_pgon(10)=par_pgon(7)
          write(PanelName,'(a2,2i1)')'MP',ia,is
          call gsvolu(PanelName, 'PGON',
     &           FrameMedium(is,ia), par_pgon, 10, ivolu)
          if ( mum_color .gt. 0 ) then
            call gsatt( PanelName, 'SEEN', 1 )
            call gsatt( PanelName, 'COLO', mum_color)
          else
            call gsatt( PanelName, 'SEEN', 0 )
          endif

C  Now honeycomb fill.

          par_pgon(1)=0.
          par_pgon(2)=45.
          par_pgon(3)=2.
          par_pgon(4)=2.
          par_pgon(5)=-StationThreePanel(4,ia)/2.
          par_pgon(6)=StationThreePanel(1,ia)
          par_pgon(7)=StationThreePanel(2,ia)
          par_pgon(8)=par_pgon(5)+StationThreePanel(4,ia)
          par_pgon(9)=par_pgon(6)
          par_pgon(10)=par_pgon(7)
          write(HoneyName,'(a2,2i1)')'MH',ia,is
          call gsvolu(HoneyName, 'PGON',
     &           HoneyMedium, par_pgon, 10, ivolu)
          if ( mum_color .gt. 0 ) then
            call gsatt( HoneyName, 'SEEN', 1 )
            call gsatt( HoneyName, 'COLO', mum_color)
          else
            call gsatt( HoneyName, 'SEEN', 0 )
          endif

C  Position honeycomb inside of panels (0.5" edge fill).

          xpos = StationThreePanel(6,ia)
          ypos = StationThreePanel(7,ia)

          call gspos( HoneyName, 1, PanelName,
     &          xpos, ypos, 0.0, 0, 'ONLY' )

C  Make skins for panels out of copper.

          par_pgon(1)=0.
          par_pgon(2)=45.
          par_pgon(3)=2.
          par_pgon(4)=2.
          par_pgon(5)=-StationThreePanel(5,ia)/2.
          par_pgon(6)=StationThreeFrame(1,ia)
          par_pgon(7)=StationThreeFrame(2,ia)
          par_pgon(8)=par_pgon(5)+StationThreePanel(5,ia)
          par_pgon(9)=par_pgon(6)
          par_pgon(10)=par_pgon(7)
          write(SkinName,'(a2,2i1)')'MK',ia,is
          call gsvolu(SkinName, 'PGON',
     &           17, par_pgon, 10, ivolu)
          if ( mum_color .gt. 0 ) then
            call gsatt( SkinName, 'SEEN', 1 )
            call gsatt( SkinName, 'COLO', mum_color)
          else
            call gsatt( SkinName, 'SEEN', 0 )
          endif

C  Position skins inside of panel.

          zpos2 = -StationThreePanel(3,ia)/
     &             2.+StationThreePanel(5,ia)/2.
          call gspos( SkinName, 1, PanelName,
     &          0.0, 0.0, zpos2, 0, 'ONLY' )
          zpos2 = StationThreePanel(3,ia)/
     &             2.-StationThreePanel(5,ia)/2.
          call gspos( SkinName, 2, PanelName,
     &          0.0, 0.0, zpos2, 0, 'ONLY' )

C  Make rib out of FR4.

          par_box(1) = StationThreeRib(2,ia)/2.0
     &                 -1.1     ! overlap studies HvH
          par_box(2) = StationThreeRib(1,ia)/2.0
     &                 -0.001   ! overlap studies HvH
          par_box(3) = StationThreeAnode(3,ia)/4.0
          write(Rib45Name,'(a2,2i1)')'ME',ia,is
          call gsvolu(Rib45Name ,'BOX ',
     &         FrameMedium(is,ia), par_box, 3, ivolu)
          if ( mum_color .gt. 0 ) then
            call gsatt( Rib45Name, 'SEEN', 1 )
            call gsatt( Rib45Name, 'COLO', mum_color)
          else
            call gsatt( Rib45Name, 'SEEN', 0 )
          endif

C  Position all parts in the Octant.  Start with first frame and
C  panel, then loop over chambers.

          xpos = StationThreeFrame(8,ia)
          ypos = StationThreeFrame(9,ia)
          zpos2 = StationThreeThickness/
     &             2.-StationThreeFrame(5,ia)/2.
          zpos3 = zpos2-StationThreeFrame(5,ia)/2.
          call gspos( FrameName, 1, OctantName,
     &               xpos, ypos, zpos2, 0, 'ONLY' )
          zpos2 = zpos3-StationThreePanel(3,ia)/2.
          zpos3 = zpos2-StationThreePanel(3,ia)/2.
          call gspos( PanelName, 1, OctantName,
     &               xpos, ypos, zpos2, 0, 'ONLY' )

C  Loop over the chambers in station three.

          do ip = 1, PlanesPerStation( is,ia )

C  MT plane name, e.g. MT42 is 2nd arm, 1st station, 2nd chamber
C  Make active gas volumes.

            par_pgon(1)=0.
            par_pgon(2)=22.5
            par_pgon(3)=1.
            par_pgon(4)=2.
            par_pgon(5)=-StationThreeAnode(3,ia)/2.
            par_pgon(6)=StationThreeGas(1,ia)
            par_pgon(7)=StationThreeGas(2,ia)
            par_pgon(8)=par_pgon(5)+StationThreeAnode(3,ia)
            par_pgon(9)=par_pgon(6)
            par_pgon(10)=par_pgon(7)
            write(GasName,'(a2,2i1)')'MT', ns, ip
            call gsvolu(GasName, 'PGON',
     &             StationMedium, par_pgon, 10, ivolu)
            if ( mum_color .gt. 0 ) then
              call gsatt( GasName, 'SEEN', 1 )
              call gsatt( GasName, 'COLO', mum_color)
            else
              call gsatt( GasName, 'SEEN', 0 )
            endif

C  Make anode plane out of FR4.

            par_pgon(1)=0.
            par_pgon(2)=45.
            par_pgon(3)=2.
            par_pgon(4)=2.
            par_pgon(5)=-StationThreeAnode(3,ia)/2.
            par_pgon(6)=StationThreeFrame(1,ia)
     &            -0.456    ! overlap studies HvH
            par_pgon(7)=StationThreeFrame(2,ia)
            par_pgon(8)=par_pgon(5)+StationThreeAnode(3,ia)
            par_pgon(9)=par_pgon(6)
            par_pgon(10)=par_pgon(7)
            write(AnodeName,'(a2,2i1)')'MN',ns,ip
            call gsvolu(AnodeName, 'PGON',
     &             FrameMedium(is,ia), par_pgon, 10, ivolu)
            if ( mum_color .gt. 0 ) then
              call gsatt( AnodeName, 'SEEN', 1 )
              call gsatt( AnodeName, 'COLO', mum_color)
            else
              call gsatt( AnodeName, 'SEEN', 0 )
            endif

C  Now make hole in anode plane out of air (same as frame hole).

            par_pgon(1)=0.
            par_pgon(2)=45.
            par_pgon(3)=2.
            par_pgon(4)=2.
            par_pgon(5)=-StationThreeAnode(3,ia)/2.
            par_pgon(6)=StationThreeAnode(1,ia)
            par_pgon(7)=StationThreeAnode(2,ia)
            par_pgon(8)=par_pgon(5)+StationThreeAnode(3,ia)
            par_pgon(9)=par_pgon(6)
            par_pgon(10)=par_pgon(7)
            write(Anode2Name,'(a2,2i1)')'MO',ns,ip
            call gsvolu(Anode2Name, 'PGON',
     &             ArmMedium, par_pgon, 10, ivolu)
            if ( mum_color .gt. 0 ) then
              call gsatt( Anode2Name, 'SEEN', 1 )
              call gsatt( Anode2Name, 'COLO', mum_color)
            else
              call gsatt( Anode2Name, 'SEEN', 0 )
            endif

C  Position rib and gas planes in anode hole.

C  Now position rib.

            xpos = StationThreeRib(3,ia)
            ypos = StationThreeRib(4,ia)
            zpos2 = -StationThreeAnode(3,ia)/4.
            call gspos( Rib45Name, 1, Anode2Name,
     &                 xpos, ypos, zpos2, irot22, 'ONLY' )

C  Now position gas panels.

            xpos = StationThreeGas(3,ia)
            ypos = StationThreeGas(4,ia)
            zpos2 = 0.0
            call gspos( GasName, 1, Anode2Name,
     &                 xpos, ypos, zpos2, 0, 'ONLY' )
            xpos = StationThreeGas(5,ia)
            ypos = StationThreeGas(6,ia)
            call gspos( GasName, 2, Anode2Name,
     &                 xpos, ypos, zpos2, irot22, 'ONLY' )

C  Position hole in anode frame.

            xpos = StationThreeAnode(4,ia)
            ypos = StationThreeAnode(5,ia)
            call gspos( Anode2Name, 1, AnodeName,
     &            xpos, ypos, 0.0, 0, 'ONLY' )

C  Now position anode frame.

            xpos = StationThreeFrame(8,ia)
            ypos = StationThreeFrame(9,ia)
            zpos2 = zpos3-StationThreeAnode(3,ia)/2.
            zpos3 = zpos2-StationThreeAnode(3,ia)/2.
            call gspos( AnodeName, 1, OctantName,
     &                 xpos, ypos, zpos2, 0, 'ONLY' )

C  Now put in next panel.

            xpos = StationThreeFrame(8,ia)
            ypos = StationThreeFrame(9,ia)
            zpos2 = zpos3-StationThreePanel(3,ia)/2.
            zpos3 = zpos2-StationThreePanel(3,ia)/2.
            call gspos( PanelName, ip+1, OctantName,
     &                 xpos, ypos, zpos2, 0, 'ONLY' )

C  Put volume elements together into sensitive set if requested.

            if ( cvolu_opt( 1, 10 ) .eq. 'FULL' ) then
              set_id          = 'MUM '        ! put it in a SET
              namesv( 1 )     = OctantName
              nbitsv( 1 )     = 4
              nwpa            = 200           ! for now
              nwsa            = 200           ! for now
              call gsdet( set_id, GasName, 1, namesv,
     &          nbitsv, idtype, nwpa, nwsa, iset, idet )
              call gsdeth( set_id, GasName, nh, namesh,
     &          nbitmu, origmu, factmu)
            end if
          end do                  ! loop over planes

C  Now position last frame.

          xpos = StationThreeFrame(8,ia)
          ypos = StationThreeFrame(9,ia)
          zpos2 = zpos3-StationThreeFrame(5,ia)/2.
          zpos3 = zpos2-StationThreeFrame(5,ia)/2.
          call gspos( FrameName, 2, OctantName,
     &               xpos, ypos, zpos2, 0, 'ONLY' )

C  Place 8 octants into the station

C  Create rotation matrices for positioning quadrants
C  in the station volume using real survey information.

          do i = 1, 8
            irot = irot + 1
            xoff = StationThreeOffsets(1,i,ia)
            yoff = StationThreeOffsets(2,i,ia)
            zoff = StationThreeOffsets(3,i,ia)
            call gsrotm(irot, StationThreeAngles(1,i,ia),
     &                        StationThreeAngles(2,i,ia),
     &                        StationThreeAngles(3,i,ia),
     &                        StationThreeAngles(4,i,ia),
     &                        StationThreeAngles(5,i,ia),
     &                        StationThreeAngles(6,i,ia))
            call gspos(OctantName, i, StationName,
     &                 xoff, yoff, zoff, irot, 'ONLY' )
          end do

C  End of making tracking stations for this arm.

        end do                          ! loop on positive or negative Z

        write(LOUT,'(/2x,
     &        ''mutrst - Muon tracking stations installed'')')

      end if

      return


c---------------------------------------------------------------------
  999 continue
      close(unit=15)
      stop 'mutrst - PISA stop, geometry error.'
      end
