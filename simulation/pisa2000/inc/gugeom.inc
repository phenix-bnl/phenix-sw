c
c     General use geometry definition variables
c
c     Author: S.R. Tonse and C.F. Maguire (from old FOPI code)
c
c     Contains the parameters of the central arm subsystem mother volumes
c
c     Revision History
c	1992-95 Adjustements to central and endcap volumes as
c               PHENIX detector becomes better defined
c     Mike Leitch 1995 Enlarge mother volume to contain
c                      central muon id outside the hall (Muon Wall system)
c     CFM: July 7, 1995  Add par_m_bgas for Beam Gas simulations
c                        Paul Kirk LSU request
c
c     CFM: Feb 7, 1996  Major overhaul for updated geometries
c 	                New header and comments
c                       Removed Silicon Pad volume (McGill intermittency)
c			Put in 5 cm radial expansion, 20 to 25 cm, of VERT
c                       Corresponding 5 cm adjustment for INNR volume
c                       Revise CERK volume as per Akiba's request
c
c
c     CFM: Feb 26, 1996 Had to remove CERK volume because of conflict with
c                       central arm return yoke volume
c     
c     CFM: Dec 8, 1996  Change the INTR mother volume from a polycone to a
c                       simple TUBE compatible with new DC/PC1 and new RICH
c
c     CFM: Remove the INNR volume (no detectors there, and helium bag conflict)
c          July 30, 1997
c
c     CFM: July 31, 1997  Change inner radius of INTR from 199.0 to 201.5 cm
c                         with approval of Jeff Mitchell.  This is needed for
c                         the Helium bag and DC Shield volumes
c
c     CFM: Oct 24, 1997 Change to symmetric nosecone configuration, as per
c                       September Detector Council decision
c
c     JPS: Jan 9, 1998  another 5 cm radial expansion, 25 to 30 cm, of VERT
c  
c     CFM: Feb 9, 2000  Add par_m_zdc(3) array to accommodate ZDC installation
c
c     Global variables for geometry rotations
c
      integer irot, irotnull           ! IROT floating rotation integer variable
                                       ! IROTNULL null (non-) rotation value = 1

      character*4 eDCH, wDCH
      character*4 ePC1, wPC1
      character*4 eCRK, wCRK
      character*4 ePC2, wPC2
      character*4 ePC3, wPC3
      character*4 eTEC, wTEC
      character*4 eTOF, wTOF
      character*4 eEMC, wEMC
      integer iEastWest
      real shiftEast(3), shiftWest(3)
      real rotateEast(3), rotateWest(3)
      common/gugeompar/irot, irotnull, iEastWest, shiftEast,
     +                 shiftWest, rotateEast, rotateWest,
     +                 eDCH, wDCH, 
     +                 ePC1, wPC1,
     +                 eCRK, wCRK,
     +                 ePC2, wPC2,
     +                 eTEC, wTEC,
     +                 eTOF, wTOF,
     +                 ePC3, wPC3,
     +                 eEMC, wEMC


c
c     Local variables for subsystem definitions
c
c
c     The following are used in GSDETH
c     Hit parameters will be position, energy, and energy loss in the detector
c     and path length in detector
c
      character*4 namesh(11) /'POSX','POSY','POSZ','DELE','TOFL','P_ID',
     1                        'P_PX','P_PY','P_PZ','PLEN','ETOT'/
      integer*4 nbitsh(11) /11*32/
c
c     default setting of offsets and gains
c
      REAL ORIG(11) /1000.0,1000.0,1000.0,3*0.,3*50.,2*0./  !offset
      REAL FACT(11) /3*100.,1.E7,100.,1.,3*1000.,100.,2000./   !gain
c
c
c     The above gains give
c              - 0.1 keV energy deposition resolution
c              - 10  picosec resolution on time of flight
c              - 0.1 mm position resolution
c              - 1.0 MeV/c momentum resolution
c              - 0.1 mm path length resolution
c              - 0.5 MeV energy resolution
c
c
c     logical reference volumes  (mother volumes for detector sub-systems)
c     flaring of volumes is accomplished with the use of POLYCONES
c
c mjl enlarge mother volume to contain central muon id outside the hall
c
c mjl real par_m(3) /750.,750.,1000./     ! Mother volume (exp. hall)
      real par_m(3) /1000.,1100.,1100./   ! Mother volume (exp. hall)

      real par_m_zdc(3) /1000.,1100.,2020./ ! Mother volume with ZDC (exp. hall)

c
c CFM: July 7, 1995  Add par_m_bgas for Beam Gas simulations (Paul Kirk LSU)
c
      real par_m_bgas(3) /1000., 1100., 10100./ ! Special Mother volume
c
c     MVD mother volume VERT
c
CJPS  real dim_vert(3) /5.0,20.0,31.9/    ! VERT volume shrunk allow nosecone
C     real dim_vert(3) /4.5,20.0,40.0/    ! VERT volume enlargement (1995)
C     real dim_vert(3) /4.5,25.0,40.0/    ! VERT volume enlargement (2/96)
      real dim_vert(3) /4.5,30.0,40.0/    ! VERT volume enlargement (1/98)

      real pos_vert(3) /0.0,0.0,0.0/      ! symmetric nose cone; official
c
c     The following commented line was for the obsolete asymmetric nosecones
c
c     real pos_vert(3) /0.0,0.0,-8.0/     ! VERT position fix conflict (asymmetric)


c
c     INTR polycone: from 100 to 250 cm for DC and PC1
c
c     integer intr_npoly
c     parameter (intr_npoly = 15)
c     real intr_poly (intr_npoly)
c    A         / 0.0, 360.0, 4.0,   ! PHI1, PHI2, # ZPLANES
c    1         -120., 250., 250.,   ! first Z plane (low Z half)
c    2         -65.3, 100., 250.,   ! second Z plane (low Z half)
c    3         +65.3, 100., 250.,   ! third Z plane (high Z half)
c    4         +120., 250., 250./   ! fourth Z plane (high Z half)
c
c
c     INTR tube from R = 201.5 to 255 cm for DC and PC1, Z = -129 to +129
c
      integer intr_ntube
      parameter (intr_ntube = 3)
      real intr_tube (intr_ntube) / 201.5, 255., 129. /

c     EMCL contains all outer volumes (PC2, TEC/TRD, PC3, TOF, and EMCal)
c
c     EMCL polycone: from 410 to 700 cm   cfm: June 10, 1993
c
      integer emcl_npoly
      parameter (emcl_npoly = 15)
      real emcl_poly (emcl_npoly)
     A         / 0.0, 360.0, 4.0,   ! PHI1, DELPHI, # ZPLANES
     1         -587., 700., 700.,   ! first Z plane (low Z half, 50 deg)
     2         -344., 410., 700.,   ! second Z plane (low Z half, 50 deg)
     3         +344., 410., 700.,   ! third Z plane (high Z half, 50 deg)
     4         +587., 700., 700./   ! fourth Z plane (high Z half, 50 deg)
c
c    End of gugeom.inc
c
 

