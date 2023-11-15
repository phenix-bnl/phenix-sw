c $Id: svx_ifvtx.f,v 1.4 2009/04/22 04:29:05 youzy Exp $
C     File name: svx_ifvtx.f      ( was previously part of svx.f)
C     ------xxx----------

C     Original author: Hubert van Hecke, Michael Malik
C     Creation date: March 2008

C     Purpose: Set up the IFVTX (LDRD pixel planes)

C     Revision History: This code was lifted out of svx.f

*=====================================================================================

        SUBROUTINE svx_ifvtx
        implicit none

#include "gugeom.inc"
#include "gconst.inc"

        character*4  sil_name
        character*4  set_id    /'SVX '/             ! Detector/hit set ID
        Integer      nbitsv(7) /7*8/                ! Bits to pack vol. copy #
        Integer      idtype    /2001/               ! User def. detector type
        Integer      nwpa      /500/                ! Init. size of HITS banks
        Integer      nwsa      /500/                ! Init. size of DIGI banks
        integer      iset, idet
        INTEGER  LNAM(5)
        integer  LNUM(5)       /1,1,1,1,1/
        Character*4 namesw(7) /'HALL','SIEN','SICG',
     &                         'SIxx','SIPx','SISx','SSSx'/
        integer sili_med_silicon /10/     ! Sensitive silicon nmed (nmat=50,Si)
        integer sili_med_silipass/11/     ! Non-sensitive silicon nmed (nmat=50,Si)
        Integer sili_med_coldair /121/    ! Gas inside the SICG (cold air)
        integer sili_med_carbon  /123/    ! carbon-carbon composite

        real par_sisb(3)      ! x,y,z of the total Si volume (sensitive + non-sensitive Si)
        real par_sisi(3)      ! x,y,z of the sensitive Si volume
        real par_chmr(3)      ! x,y,z of the FPIX readout chips volume (all 8, includes gaps)
        real par_hdib(3)      ! x,y,z of the HDI volume (Kapton, Cu, and glue)
        real par_sicb(3)      ! x,y,z of the TPG (carbon) volume big
        real par_sics(3)      ! x,y,z of the TPG (carbon) volume small
        real par_sipb(3)      ! x,y,z of one "module" mother vol, contains sisb, sisi, chmr, hdib.
        real par_si09(9)      ! small station volume PCON pars
        real par_si10(9)      ! big stations PCON parameters
        real par_pxlb(9)      ! big PC board
        real par_pxls(9)      ! small PC boards
        real par_holb(4)      ! hole in the big boards
        real par_hols(4)      ! hole in the small boards
        real par_ptch(3)      ! patch PC board

        real gap_y,           ! Adjust such that module-to-module spacing is 1.24mm
     &       yhdi_start,      ! adjust so that the bottom hdi start 4.033 from the beamline
     &       hdib_gap_y, chmr_gap_y,
     &       tpg_thick, ystrip, spacing, ifvtx_zstation(8),
     &       sicb_y_pos, hdib_y_pos, chmr_y_pos, sisb_y_pos,
     &       sicb_z_pos, hdib_z_pos, chmr_z_pos, sisb_z_pos

        integer sisb_medium, sisi_medium, chmr_medium, hdib_medium,
     &       sicb_medium, sipb_medium, pxlb_medium, pxls_medium,
     &       nmodb, nmods, i, isen, ierr, ivol1, 
     &       istation, iflip, irstation, debug
                                                       ! Hit component names
        character*4 inrNMSH(21) /'POSX','POSY','POSZ'  ! Global positions
     &     ,'DELE','TOFL'                              ! Energy loss & TOF
     &     ,'P_ID','MOMX', 'MOMY', 'MOMZ'              ! Particle ID & Entry mom.
     &     ,'XILC','YILC','ZILC','XOLC','YOLC','ZOLC'  ! Local entry & exit
     &     ,'XIGL','YIGL','ZIGL','XOGL','YOGL','ZOGL'/ ! global entry & exit
        character*4 namesv(5) /'HALL','SIEN','SICG'
     *     ,'SIxx','SISI'/                             ! Volume names 

        Integer     nhh         /21/                   ! Number of hit components
        integer*4 inrNBITSH(21) /21*32/                ! Bits for packing the hits

c       Default setting of offsets and gains
        REAL inrORIG(21) /3*1000.,3*0.,3*1000.,6*1000.,6*1000./       ! offsets
        REAL inrFACT(21) /3*100000.,1.E7,1.e12,1.0,3*100000.
     &                   ,6*100000.,6*100000./         !   These gains give:
c              - 0.1 keV energy deposition resolution
c              - 0.0001 mm position resolution
c              - 0.01 MeV/c momentum resolution
  
        integer           itf_lun                    ! phnx(Sili).par logical unit
        common /interface/itf_lun                    ! in pisa core
        namelist / sili_ifvtx_par / ifvtx_zstation, par_sisb, par_sisi,
     &             par_chmr, par_hdib, par_sicb, par_pxlb, par_pxls, 
     &             gap_y, par_holb, par_hols, tpg_thick, par_ptch,
     &             yhdi_start, nmodb, nmods, spacing,
     &             debug

*=========================================================================================================================

        read( itf_lun, nml = sili_ifvtx_par, err = 996 )
        HDIB_GAP_Y = 0.05                           ! This is how far the Si volume (SISB) sticks out past the HDI in the y-direction.
        CHMR_GAP_Y = 0.02                           ! This is how far the Si volume (SISB) sticks out past the readout chips in the 
        write (6,*)' SVX_IFVTX.F: installing pixel planes'

        do i=1,3                                    ! convert box sizes to half-sizes
           par_sisb(i) = par_sisb(i)/2
           par_sisi(i) = par_sisi(i)/2
           par_chmr(i) = par_chmr(i)/2
           par_hdib(i) = par_hdib(i)/2
           par_sicb(i) = par_sicb(i)/2
           par_holb(i) = par_holb(i)/2
           par_hols(i) = par_hols(i)/2
           par_sicb(i) = par_holb(i) - 0.1          ! make carbon 1mm smaller than hole, all around
           par_sics(i) = par_hols(i) - 0.1
        enddo

        irot = irot+1                               ! modules flipped around y
        call gsrotm(irot,90.,180.,90.,90.,180.,0.)  ! pc boards also
        iflip = irot                                ! save
        irot = irot+1                               ! Tip the whole device to the West
        call gsrotm(irot,90.,-90.,90., 0.,  0.,0.)  ! (used to be up)
        irstation = irot                            ! save

        sisb_medium = sili_med_silipass             ! This is the medium type; should be 11 for pass si.        
        sisi_medium = sili_med_silicon              ! This is the medium type; should be 10 for active si.
        chmr_medium = sili_med_silipass             ! This is the medium type; should be 10 for pass si.
        hdib_medium = sili_med_carbon               ! hdi   xxx carbon is not best
        sicb_medium = sili_med_carbon               ! This is the medium type; should be 123 for carbon.
        sipb_medium = sili_med_coldair              ! dummy volume -  should be 121 for cold air.
        pxlb_medium = sili_med_carbon               ! xxx needs to be g10 pcb
        pxls_medium = sili_med_carbon               ! xxx needs to be G10 PCB
        
        par_SIPB(1) = par_HDIB(1)                   ! half x-length of one "module" mother volume (contains sisb, sisi, chmr, 
        par_SIPB(2) = par_HDIB(2) + hdib_gap_y/2    ! half y-length of one "module" mother volume (contains sisb, sisi, chmr, 
        par_SIPB(3) = par_SISI(3) + par_CHMR(3) + par_HDIB(3)    ! half z

        do i=1,9                                    ! station master volume; start with pc board:
           par_si09(i) = par_pxls(i)
           par_si10(i) = par_pxlb(i)
        enddo
        par_si09(4) = -spacing/2 - 2*par_pxls(7)    ! increase thickness 
        par_si09(7) =  spacing/2 + 2*par_pxls(7)    !    ''
        par_si09(6) =  par_si09(6) + 0.6            ! increase outer radius + 6 mm 
        par_si09(9) =  par_si09(9) + 0.6            !     ''

        par_si10(4) = -spacing/2 - 2*par_pxlb(7)    ! same for big half-cylinders 
        par_si10(7) =  spacing/2 + 2*par_pxlb(7)
        par_si10(6) =  par_si10(6) + 0.6            ! increase outer radius + 6 mm 
        par_si10(9) =  par_si10(9) + 0.6
        
        sisb_y_pos = -(par_hdib(2) + hdib_gap_y -
     &                 par_sisb(2)) +  hdib_gap_y/2 ! This is the y-position of the SISB volume inside SIPB. 

        chmr_y_pos = -(par_hdib(2) - par_chmr(2) +    ! This is the y-position of the readout chips volume (CHMR) inside SIPB.
     &                 hdib_gap_y - CHMR_GAP_Y) + hdib_gap_y/2

        hdib_y_pos = hdib_gap_y/2

        sisb_z_pos = -par_sipb(3) +   par_sisb(3)                 ! silicon
        chmr_z_pos = -par_sipb(3) + 2*par_sisb(3) + par_chmr(3)   ! chip                    !
        hdib_z_pos =  par_sipb(3) - par_hdib(3)    ! hdi

        CALL GSVOLU('SIPB','BOX ',SIPB_MEDIUM,PAR_SIPB, 3,IVOL1)   ! module: silicon (sensitive + non), HDI, glue, readout chips
        CALL GSVOLU('SIPS','BOX ',SIPB_MEDIUM,PAR_SIPB, 3,IVOL1)   ! copy for layer 9
        CALL GSVOLU('SISB','BOX ',SISB_MEDIUM,PAR_SISB, 3,IVOL1)   ! one Si volume (sensitive + non).  This SISB goes inside SIPB.
        CALL GSVOLU('SISS','BOX ',SISB_MEDIUM,PAR_SISB, 3,IVOL1)   ! copy for layer 9
        CALL GSVOLU('SISI','BOX ',SISI_MEDIUM,par_SISI, 3,IVOL1)   ! sensitive Si volume.  This SISI goes inside SISB.
        CALL GSVOLU('CHMR','BOX ',CHMR_MEDIUM,PAR_CHMR, 3,IVOL1)   ! read-out chip volume.  This CHMR goes inside SIPB.
        CALL GSVOLU('HDIB','BOX ',HDIB_MEDIUM,par_HDIB, 3,IVOL1)   ! HDI volume (includes Kapton, Cu, and glue). HDIB goes in SIPB.
        CALL GSVOLU('PXLB','PCON',PXLB_MEDIUM,PAR_PXLB, 9,IVOL1)   ! This is one of the large pixel planes (at stations 10-12).
        CALL GSVOLU('PXLS','PCON',PXLS_MEDIUM,PAR_PXLS, 9,IVOL1)   ! This is one of the small pixel planes (at station 9).

        par_sicb(3) = tpg_thick/2
        par_sics(3) = tpg_thick/2
        CALL GSVOLU('SICB','BOX ',SICB_MEDIUM,PAR_SICB, 3,IVOL1)   ! This is one big TPG (carbon) volume. Goes inside PXLB pcboard
        CALL GSVOLU('SICS','BOX ',SICB_MEDIUM,PAR_SICS, 3,IVOL1)   ! This is one smallTPG (carbon) volume.  Goes inside PXLS pcboard

        call gsvolu('HOLB','BOX ',sili_med_coldair, par_holb, 3, ivol1)       ! hole in the big picture frame board
        call gspos ('HOLB',1,'PXLB', 0.0, par_holb(2) + par_holb(4),
     &              0.0, irotnull,'ONLY')
        call gspos ('SICB',1, 'HOLB', 0.0, 0.0, par_holb(3)-par_sicb(3), 
     &             irotnull, 'ONLY')                                ! big carbon in the hole
        call gsatt ('PXLB','COLO',7)                                ! 7 = light blue
        call gsatt ('HOLB','COLO',2)
        call gsatt ('SICB','COLO',6)

        call gsvolu('HOLS','BOX ',sili_med_coldair, par_hols, 3, ivol1)  ! hole in the small picture frame board
        call gspos ('HOLS',1,'PXLS', 0.0, par_hols(2)+par_hols(4), 0.0,
     &             irotnull,'ONLY')
        call gspos ('SICS',1, 'HOLS', 0.0, 0.0, par_hols(3)-par_sics(3), 
     &              irotnull, 'ONLY')                               ! small carbon in the hole
        call gsatt ('PXLS','COLO',7)                                ! 7 = light blue
        call gsatt ('HOLS','COLO',2)
        call gsatt ('SICS','COLO',6)

        call gsvolu('PATC','BOX ', PXLB_MEDIUM, par_ptch, 3, ivol1)
        call gspos ('PATC', 1, 'SICG',-4.0, 0.0, 30.0, irstation,'ONLY')

        call gsatt ('PATC', 'COLO', 2)    ! red

        CALL GSPOS('SISB', 1, 'SIPB', 0.0, SISB_Y_POS, sisb_z_pos,
     &           IROTNULL, 'ONLY')                                     !
        CALL GSPOS('SISI', 1, 'SISB', 0.0, 0.0, 0.0, IROTNULL, 'ONLY') ! Place the sensitive Si inside the Si mother volume SISB.
        CALL GSPOS('CHMR', 1, 'SIPB', 0.0, CHMR_Y_POS, chmr_z_pos,     ! Place the readout chips volume inside SIPB.
     &     IROTNULL, 'ONLY')                                           !

        CALL GSPOS('HDIB', 1, 'SIPB', 0.0, hdib_y_pos, hdib_z_pos,
     &            IROTNULL,                                            ! Position one HDI volume inside the SIPB mother volume.
     &     'ONLY')                                                     !
                                                                       !
        CALL GSPOS('SISS', 1, 'SIPS', 0.0, SISB_Y_POS, sisb_z_pos,     ! same for sips (in ifvtx, this is a copy of sipb)
     &           IROTNULL, 'ONLY')                                           !
        CALL GSPOS('SISI', 1, 'SISS', 0.0, 0.0, 0.0, IROTNULL, 'ONLY') ! Place the sensitive Si inside the Si mother volume SISB.
        CALL GSPOS('CHMR', 1, 'SIPS', 0.0, CHMR_Y_POS, chmr_z_pos,     ! Place the readout chips volume inside SIPB.
     &     IROTNULL, 'ONLY')                                           !

        CALL GSPOS('HDIB', 1, 'SIPS', 0.0, hdib_y_pos, hdib_z_pos,
     &            IROTNULL,                                            ! Position one HDI volume inside the SIPB mother volume.
     &     'ONLY')                                                     !
                                                                       !
        DO ISTATION=9,12                                               ! Place only on the North side, so go from 9 -> 12 (skip 5-8).
           IF (ISTATION.EQ.9) THEN                                     ! station SI09 is a short panel with 6 sensors per plane
              WRITE (SIL_NAME, '(''SI'',I2.2)') ISTATION               ! compose SI10, SI11, SI12 names

              CALL GSVOLU(SIL_NAME,'PCON',SILI_MED_COLDAIR,            ! 
     &             PAR_SI09,9,IVOL1)                                   ! 
              CALL GSATT (SIL_NAME, 'COLO',7)                          ! This is to set the color of the volume SI09. Light blue
              CALL GSPOS (SIL_NAME,1,'SICG',                           ! Place the station (SI09) into SICG, which sits in SIEN.
     &             0.0, 0.0, IFVTX_ZSTATION(ISTATION-8), irstation,    ! Changed from irot to irotnull to keep z along the beam pipe.
     &             'ONLY')                                             !
                                                                       !
              if (debug.eq.0.or.debug.eq.1) then                       ! 'debug' allows turn off of front/back modules
                call gspos('PXLS', 1,'SI09', 0.0, 0.0,                 ! Small picture frame PC board #1
     &                     +spacing/2 + par_pxls(7) , iflip,'ONLY')    ! 
                DO I=1,nmods                                           ! place the Si volumes into the -z plane in the 
                   YSTRIP = (I-1)*(2*par_SIPB(2) + GAP_Y) +            !      station volume 
     &                      yhdi_start + PAR_SIPB(2)
                   CALL GSPOS('SIPS', I, SIL_NAME, 0.0, ystrip,        ! 
     &                  +spacing/2 - par_sipb(3), IROTNULL,'ONLY')     ! 
                ENDDO                                                  !
              endif
              if (debug.eq.0.or.debug.eq.2) then                       ! 
                call gspos('PXLS', 2,'SI09', 0.0, 0.585,               ! Small PC board #2, flipped
     &                     -spacing/2 - par_pxls(7) ,irotnull,'ONLY')  ! 
                DO I=1,nmods                                           !
                   YSTRIP = (I-1)*(2*par_SIPB(2) + GAP_Y) + yhdi_start +
     &                  PAR_SIPB(2) + 0.58                             ! the ones in front are 5.8 higher, for overlap of
                   CALL GSPOS('SIPS', I+nmods, SIL_NAME, 0.0, YSTRIP,  !        0.6mm sens Si
     &                  -spacing/2 + par_sipb(3), iflip, 'ONLY')       ! Rotated to make Si face each other
                ENDDO                                                  ! Loop over all stations
              endif                                                    !
                                                                       !
           ELSE                                                        ! Now for the bigger stations (10, 11, and 12).
              WRITE (SIL_NAME, '(''SI'',I2.2)') ISTATION               ! Compose names SI10, SI11, SI12 
              CALL GSVOLU(SIL_NAME,'PCON', SILI_MED_COLDAIR,           ! 
     &             PAR_SI10,9,IVOL1)                                   ! 
              CALL GSATT (SIL_NAME, 'COLO',7)                          ! This is to set the color of the volume SI10. make light blue
              CALL GSPOS (SIL_NAME,1,'SICG',                           ! Place the station (SI10) into SICG, which sits in SIEN.
     &             0.0, 0.0, IFVTX_ZSTATION(ISTATION-8), irstation,    ! 
     &             'ONLY')                                             !

              if (debug.eq.0.or.debug.eq.1.or.debug.eq.3) then         ! 3 = big planes only
                call gspos('PXLB', 1, sil_name, 0.0, 0.0,
     &                     +spacing/2 + par_pxlb(7) , iflip,'ONLY')    ! place pc board #1
                DO I=1,nmodb                                            ! placing the Si volumes onto the +z pc board in the station 
                   YSTRIP = (I-1)*(2*par_SIPB(2) + GAP_Y) + yhdi_start +      !
     &                  PAR_SIPB(2)                                    ! 
                   CALL GSPOS('SIPB',I,SIL_NAME,0.0,YSTRIP,            ! place nmodb multichip modules
     &                  +spacing/2 - par_sipb(3), IROTNULL,'ONLY')     
                ENDDO                                                  !
              endif
              if (debug.eq.0.or.debug.eq.2.or.debug.eq.3) then         ! 
                call gspos('PXLB', 2, sil_name, 0.0, 0.585,
     &                     -spacing/2 - par_pxlb(7) ,irotnull,'ONLY')  ! place pc board #2
                DO I=1,nmodb                                           ! Place the nmodb multi-strip modules onto the -z pcboard
                   YSTRIP = (I-1)*(2*par_SIPB(2) + GAP_Y) + yhdi_start +  
     &                  PAR_SIPB(2) + 0.58
                   CALL GSPOS('SIPB',I+nmodb,SIL_NAME,0.0,YSTRIP,
     &                  -spacing/2 + par_sipb(3), iflip, 'ONLY')       ! Note these are rotated so that the si faces inward.
                ENDDO                                                  !
              endif
           ENDIF                                                       ! small(9) or big(10,11,12) layer
        ENDDO                                                          !
                                                                       !
C//------ declare the sensitive volumes ------------------------------ !
                                                                   
        DO ISTATION = 9,12                                         
          WRITE (NAMESW(4),'(''SI'',I2.2)')  ISTATION              
          CALL GSDET (SET_ID, NAMESW(4), 7, NAMESW, NBITSV, IDTYPE,
     &                                    NWPA, NWSA, ISET, IDET)  
          CALL GSDETH(SET_ID, NAMESW(4), NHH,INRNMSH,INRNBITSH,    
     &              INRORIG,INRFACT)  
        ENDDO                                                      

        CALL GSATT( 'SIPB', 'COLO', 1)                   !  2 = Red
        CALL GSATT( 'SIPS', 'COLO', 1)                   !  2 = Red
        CALL GSATT( 'SISB', 'COLO', 7)                   !  4 = Blue
        CALL GSATT( 'SISI', 'COLO', 6)                   !  6 = Pink
        CALL GSATT( 'CHMR', 'COLO', 4)                   !  8 = White
        CALL GSATT( 'HDIB', 'COLO', 5)                               
        CALL GSATT( 'SICB', 'COLO', 3)                               

c---- end colors and other attributes----------------------------------!

        do i=1,5                                         ! copy ascii to hollerith
          call uctoh(namesv(i), LNAM(i), 4, 4)           !
        enddo                                            !

        do istation = 9,12                               ! loop over 4 planes
          write (sil_name, '(''SI'',I2.2)') istation     ! 1,2,3 = HALL, SIEN, SICG
          call uctoh(sil_name, LNAM(4), 4, 4)            !     4 = SI09 (10,11,12)
          if (istation.eq.9) then                        ! the name are kept as in fvtx  even though 
            call uctoh('SIPS', LNAM(5),4,4)              ! modules are identical in all 4 stations.
            do isen = 1, nmods*2                         ! 
              lnum(5) = isen                             ! copy number of the silicon
              call glvolu(5, LNAM, LNUM, ierr)           ! fills /GCVOLU/
            enddo                                        ! modules per station
          else 
            call uctoh('SIPB', LNAM(5),4,4)              !
            do isen = 1, nmodb*2                        ! 
              lnum(5) = isen                             ! copy number of the silicon
              call glvolu(5, LNAM, LNUM, ierr)           ! fills /GCVOLU/
            enddo                                        ! modules per station
          endif
        enddo                                            ! loop over 4, 8 lampshades

        return            ! from subroutine svx_ifvtx

 996    stop 'IFVTX - read error in sili_ifvtx_par segment.'
        end               ! end of subroutine svx_ifvtx

c=============================================================================c
