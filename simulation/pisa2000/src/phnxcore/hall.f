c $Id: hall.f,v 1.8 2008/05/21 08:22:10 hpereira Exp $
      subroutine hall
      IMPLICIT NONE
c=============================================================================


c     Authors: S.R. Tonse and C.F. Maguire (from old FOPI code)

c     Principle purpose is to establish the HALL mother volume
c     Subsidiary purpose is to establish the subsystem mother volumes in HALL
c     Only central arm systems have these special mother (virtual) volumes
c     Muon Arms have HALL as their mother volume
c     The tracking media are also set in all the volumes

c     Calling map: called by GUGEOM



c     Initial Radial Allocations  (March 1992: Historical interest only)

c Detector Sub-System      Radial Space         Simulation Contact-Person(s)

c Vertex barrel (VER)         0.05 ---> 0.2 m   John Sullivan
c Vertex endcap (PAD)         0.05 ---> 0.2 m   Marzia Rosati
c Dalitz tracker (INR)         0.2 ---> 1.0 m   Tom Hemmick
c Intermediate tracker (ITR)   1.0 ---> 2.5 m   Nikolai Smirnov + S. Tonse
c Cerenkov (CRK)               2.5 ---> 4.1 m   Tom Hemmick
c Full tracker (TRD)           4.1 ---> 4.9 m   Nikolai Smirnov + S. Tonse
c Time-of-flight (TOF)         4.9 ---> 5.1 m   T. Nayak
c Calorimeter (EMC)            5.1 ---> 7.0 m   Charlie Maguire
c Photon (PHO)                 5.1 ---> 7.0 m   Terry Awes + Gabor David
c Muon (MUO)        ENDCAP: 5.0 ---> 30 degrees CM + Gus Petitt

c NOTE: The above spatial allocations and contact persons have been revised
c       significantly since 1992


c  Revision History

C  S.R.Tonse 4/13/93     Code now checks whether the detector has been
C                        switched on before booking the virtual volume.

c  C.F. Maguire 2/2/96   Added comments and expanded header file
c                        Re-arranged code lines more logically
c                        Set DC & RICH mother volume media to be same as EMCal

c  C.F. Maguire 2/26/96  Improved description of central arm return yoke
c                        volumes, required removal of CERK mother volume

c  C.F. Maguire 7/29/97  Removed INNR volume entirely; future Dalitz rejector
c                        will have to become compatible with new Helium bag
c                        Take magnetic air as standard tracking medium in HALL

c  C.F. Maguire 8/11/97  Put in new Air medium (lower tracking thresholds) in
c                        EMCal mother volume (for PC2, TEC, PC3, TOF, and EMC)

c  C.F. Maguire 5/11/2000 Put in TUBS for EAST and WEST submaster volumes
c                         (These will have to be changed to PCONs in order to
c                          exclude the Muon Arm regions)

c                         Put shift and rotate postions in gugeom.inc common block

c  C.F. Maguire 5/14/2000 Change EAST and WEST to PCONs so as not to overlap
c                         the magnet steel and the Muon Arm regions.  Also,
c                         use no rotation (0) instead of null rotation (1) for
c                         submaster volumes in HALL.

c                         NOTE: the carriage arms themselves ('CARR' in GEOP) are
c                         not normally included in the simulations.  These volumes
c                         have to be made compatible with EAST and WEST by Andrew Rose

c=============================================================================

c     Global Variables

#include "gclist.inc"
#include "gugeom.inc"
#include "guphnx.inc"

c     Local variables

      real nul_rot(6) /90.0,0.0,90.0,90.0,0.0,0.0/  ! null rotation
      INTEGER NMED,NPAR,IVOLU
      CHARACTER*4 V_M_NAME,V_M_SHAPE

      integer eastNPoly
      parameter (eastNPoly = 15)
      real eastPoly(eastNPoly)
     +     / 110.0, 117.5, 4.0,     !  PHI1, DELPHI, # ZPLANES
     +       -587., 700., 700.,     !  south Z limit at end of EMCal
     +       -134.86, 180.0, 700.,  !  south Z limit before Dch
     +       +134.86, 180.0, 700.,  !  north Z limit before Dch
     +       +587., 700., 700. /    !  north Z limit at end of EMCal


      integer westNPoly
      parameter (westNPoly = 15)
      real westPoly(westNPoly)
     +     / -47.5, 117.5, 4.0,     !  PHI1, DELPHI, # ZPLANES  (-47.5 to +70.0)
     +       -587., 700., 700.,     !  south Z limit at end of EMCal
     +       -134.86, 180.0, 700.,  !  south Z limit before Dch
     +       +134.86, 180.0, 700.,  !  north Z limit before Dch
     +       +587., 700., 700. /    !  north Z limit at end of EMCal

      integer iRotateEast /1/
      integer iRotateWest /1/

      integer arm_shift /0/   ! 0 means no shift, 1 means shift
      real east_shift(3) /0.0,0.0,0.0/   ! (x,y,z) shifts in cm to the East
      real west_shift(3) /0.0,0.0,0.0/   ! (x,y,z) shifts in cm to the West
      real east_rotate(3) /0.0,0.0,0.0/  ! azimuthal rotation of East Arm
      real west_rotate(3) /0.0,0.0,0.0/  ! azimuthal rotation of West Arm

      namelist /hall_par/ arm_shift, east_shift, west_shift,
     +                    east_rotate, west_rotate

      integer iLoop

c---------------------------------------------------------------------
c     geometry description logical unit
      integer itf_lun
      common /interface/itf_lun


c     Begin execution



c     Read the geometery file segment

      write( *,* ) 'hall - reading parameter from common interface'
      rewind(itf_lun)
      read( itf_lun, nml = hall_par, err = 999 )

      if(arm_shift.eq.1)then
         iEastWest = 1
         eDCH = 'EAST'
         wDCH = 'WEST'
         ePC1 = 'EAST'
         wPC1 = 'WEST'
         eCRK = 'EAST'
         wCRK = 'WEST'
         ePC2 = 'EAST'
         wPC2 = 'WEST'
         eTEC = 'EAST'
         wTEC = 'WEST'
         ePC3 = 'EAST'
         wPC3 = 'WEST'
         eTOF = 'EAST'
         wTOF = 'WEST'
         eEMC = 'EAST'
         wEMC = 'WEST'
         do iLoop = 1,3
            shiftEast(iLoop) = east_shift(iLoop)
            shiftWest(iLoop) = west_shift(iLoop)
            rotateEast(iLoop) = east_rotate(iLoop)
            rotateWest(iLoop) = west_rotate(iLoop)
         enddo
      else
         iEastWest = 0
         eDCH = 'INTR'
         wDCH = 'INTR'
         ePC1 = 'INTR'
         wPC1 = 'INTR'
         eCRK = 'HALL'
         wCRK = 'HALL'
         ePC2 = 'EMCL'
         wPC2 = 'EMCL'
         eTEC = 'EMCL'
         wTEC = 'EMCL'
         ePC3 = 'EMCL'
         wPC3 = 'EMCL'
         eTOF = 'EMCL'
         wTOF = 'EMCL'
         eEMC = 'EMCL'
         wEMC = 'EMCL'
      endif  ! check if horizontal shift of East or West Arms


c     Define the first rotation matrix to be the NULL rotation
c     This goes in to the gugeompar common block in gugeom.inc

      irot = 1
      call gsrotm(irot,nul_rot(1),nul_rot(2),nul_rot(3),
     1      nul_rot(4),nul_rot(5),nul_rot(6))
      irotnull = irot      ! IROT and IROTNULL both in gugeompar common block

c   Define master volume

      v_m_name = 'HALL'
      v_m_shape ='BOX '

c    Set the HALL tracking medium according to if LGEOM(5) set in pisa.kumac

      if(lgeom(5).ne.0)then
         nmed=lgeom(5)
      else
         nmed = 19      !   air (magnetic) high field
      endif
      npar = 3
      write(6,*) 'Hall is filled with tracking volume medium=',nmed

c    Check if either the Beam Gas geometry or the ZDC switch is set

      if(lgeom(2).eq.0)then

c     Standard HALL size at +/- 1100 cm

         call gsvolu(v_m_name,v_m_shape,nmed,par_m,npar,ivolu)
      endif  ! check for normal geometry

      if(lgeom(2).eq.1)then

c     Expanded HALL size at +/- 10100 cm longitudinal (Beam gas)

         call gsvolu(v_m_name,v_m_shape,nmed,par_m_bgas,npar,ivolu)
      endif  ! check for Beam gas geometry


c     Expanded HALL size at +/- 2020 cm longitudinal (ZDC)

      if(lgeom(2).eq.2)then
         call gsvolu(v_m_name,v_m_shape,nmed,par_m_zdc,npar,ivolu)
         write(6,1)par_m_zdc(3)
 1       format(/,' HALL <I>: Using ZDC version of the hall volume ',
     +            'with longitudinal extent +/-',f6.0,' cm',/)
      endif  ! check for ZDC geometry

      if(lgeom(2).ne.0.and.lgeom(2).ne.1.and.lgeom(2).ne.2)then
         write(6,*)' '
         write(6,*)'  Fatal input error'
         write(6,*)' hall.f <E>:  LGEOM(2) value is not acceptable'
         stop ' PISA is stopping'
      endif

      CALL GSATT(V_M_NAME,'SEEN',0)


c     Insertions for East or West Arm carriage movement

      if(arm_shift.eq.1)then

c     Put in Polycones

         call gsvolu('EAST','PCON',nmed,eastPoly,eastNPoly,ivolu)
         call gsvolu('WEST','PCON',nmed,westPoly,westNPoly,ivolu)

c     Now position the EAST box

         call gspos('EAST',1, 'HALL', east_shift(1),
     +              east_shift(2), east_shift(3), 0, 'ONLY')
         write(6,12)east_shift
 12      format(/,'  hall.f <I>: East Arm shifts (x,y,z) = ',
     +          3(f6.1,',  '),' cm',/)

c     Now position the WEST box

         call gspos('WEST',1, 'HALL', west_shift(1),
     +              west_shift(2), west_shift(3), 0, 'ONLY')
         write(6,13)west_shift
 13      format(/,'  hall.f <I>: West Arm shifts (x,y,z) = ',
     +          3(f6.1,',  '),' cm',/)
      endif  ! check if East or West Arm shift


c     Put in the mother volumes for the different central detector subgroups

c     First the MVD

       npar = 3
       v_m_shape ='TUBE'
       IF(IVOLU_OPT(1,1) .NE. 0)THEN
         v_m_name = 'VERT'   ! Central vertex tube VER
         call gsvolu(v_m_name,v_m_shape,nmed,dim_vert,npar,ivolu)
         CALL GSATT(V_M_NAME,'SEEN',0)
         CALL GSATT(V_M_NAME,'COLO',3)
         call gspos(v_m_name,1,'HALL',pos_vert(1),
     +      pos_vert(2),pos_vert(3),0,'ONLY')
       END IF

c     Now the Dalitz rejector (does not exist as of 2/2/1996)

       IF(IVOLU_OPT(1,3) .NE. 0)THEN
          write(6,987)
 987      format(//, 'Special inner tracking volume for upgrade',//)
       END IF

c      Now the inner tracking detectors (DC and PC1)

       IF(IVOLU_OPT(1,4) .NE. 0 .and. ARM_SHIFT .EQ. 0)THEN
         v_m_name = 'INTR'   ! Intermediate tracker ITR
c        call gsvolu(v_m_name,'PCON',nmed,intr_poly,intr_npoly,ivolu)
	 call gsvolu(v_m_name,'TUBE',nmed,intr_tube,intr_ntube,ivolu)
         CALL GSATT(V_M_NAME,'SEEN',1)
         CALL GSATT(V_M_NAME,'COLO',4)
         call gspos(v_m_name,1,'HALL',0.0, 0.0, 0.0, 0, 'ONLY')
	 write(6,*)' Central arm region up to DC filled with medium ',
     +             nmed
       END IF

c     Set the tracking medium for PC2 and beyond

      if(lgeom(3).eq.0) then
       nmed = 18    ! default = Air + low field
      else
       nmed = lgeom(3)
      endif

c    RICH former mother volume (CERK) removed because of conflict with
c         central arm return yoke volumes


      if(arm_shift.eq.0)then

c   Outer pseudo-volume (region beyond the RICH)
c   This is the mother volume for the PC2, TRD, PC3, TOF, and EMCL

         v_m_name = 'EMCL'      ! Electromagnetic Calorimeter
         call gsvolu(v_m_name,'PCON',nmed,emcl_poly,emcl_npoly,ivolu)
         CALL GSATT(V_M_NAME,'SEEN',0)
         CALL GSATT(V_M_NAME,'COLO',3)
         call gspos(v_m_name,1,'HALL',0.0, 0.0, 0.0, 0,'ONLY')
         write(6,*)' Central arm region beyond PC2 filled with medium ',
     +        nmed
      endif  ! check if no shift (old version)

c     Muon and BBC use HALL as their immediate mother volume

      RETURN

999   continue
      write(6,1000)
1000  format(/,3x,'Read error in hall_par segment'/,3x,
     1   '  Namelist mis-match in hall_par segment ?',//,3x,
     2   'Geometry will be re-read to pinpoint the erroneous',
     3  ' line',/,3x,'****This will cause the program to crash.****',//)

      rewind( itf_lun )
      read( itf_lun, nml = hall_par )
      stop ' HALL.F <I>  PISA stopped because of geometry error.'

      END
