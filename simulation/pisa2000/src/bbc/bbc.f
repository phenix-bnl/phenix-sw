c $Id: bbc.f,v 1.3 2008/05/21 08:21:53 hpereira Exp $
c=====================================================================
        SUBROUTINE BBC(FULL,NH)
c=====================================================================

c    DESCRIPTION: This routine defines the geometry for the Beam-Beam
c                 Counter. Structure of BBC is involed in the mother
c                 volume of HALL.

c     HALL --- BBCM ---                         ! BBC mother volume
c                     /-- BBCE                  ! Electron absorber
c                     /-- BBCF                  ! Front panel
c                     /-- BBCS                  ! Structure tube
c                     /-- BBCB                  ! Back panel
c                     /-- BBCC                  ! Thin barrel cover
c                     /-- BBCD --               ! Detector volume
c                               /-- BBCA        ! Attachment plate
c                               /-- BBCQ        ! Quartz block
c                               /-- BBCP        ! PMT
c                               /-- BBCR        ! Breeder module
c                               /-- BBCH        ! Metal shield 

c    ARGUMENTS: FULL, NH

c    MAP:
c           1) callED BY; GUGEOM
c           2) callS;     BBC_VARS,GSVOLU,GSATT,GSROTH,GSPOS,GSDET,GSDETH

c    AUTHOR: Toru Sugitate [Hiroshima University]  18 May 1993

c    REVISIONS:  Date     Name               Modification
c               ------   ------  ----------------------------------------------
c              08/01/96  Kazuhiro Changed the material of PMT sleeve (BBCH).
c              02/12/96  Maguire  Fix column 72 overruns in two lines
c              02/03/96  Kazuhiro Add breeder module (BBCR) in BBCD.
c                                 Add metal shield (BBCS) in BBCD.
c                                 Remove BBCD at given position.
c                                 Update default geometry as of BBC model 
c                                 version 6.E.
c              11/23/94  Toru     Change the radiator shape from round to
c                                 hexagonal to match to the present design.
c                                 Volume structure was changed.
c              09/21/93  Toru     Defalut material of absorber is changed to
c                                 aluminum (103).
c              5/31/93   TS       Installed in bbcfortr.cmz.
c                                 Saved parameters in Zebra.
c                                 Electron absorber is installed.
c                                 Hit parameters are settled.

c=====================================================================
c     GLOBAL SPECIFICATIONS:
      IMPLICIT      NONE

#include "gcflag.inc"
#include "gclist.inc"
#include "gconst.inc"

#include "guphnx.inc"
#include "gugeom.inc"

#include "fstore.inc"
#include "sublink.inc"
#include "fpblink.inc"

c=====================================================================
c    EXTERNAL SPECIFICATIONS:

      CHARACTER*4   FULL
      INTEGER*4     NH
 
c=====================================================================
c    INTERNAL SPECIFICATIONS:

      INTEGER       MAXPMT       ! Maximum PMT to be installed.
      PARAMETER     (MAXPMT = 100)
      INTEGER       MAXREM       ! Maximum PMT to be removed.
      PARAMETER     (MAXREM = 10)

      INTEGER       BBCCOLOR     ! Color (visible/invisible)
      INTEGER       BBCSEENN     ! visible or invisible
      INTEGER       BBMEDABS     ! Electron absorber material #
      INTEGER       BBMEDATT     ! Attachment material #
      INTEGER       BBMEDBAC     ! BackBoad material #
      INTEGER       BBMEDBRE     ! Breeder material
      INTEGER       BBMEDCOV     ! Barrel cover material #
      INTEGER       BBMEDFRO     ! FrontBoad material #
      INTEGER       BBMEDMOT     ! MotherVomlume material #
      INTEGER       BBMEDPMT     ! PMT material #
      INTEGER       BBMEDQUA     ! Quartz material #
      INTEGER       BBMEDSHI     ! Shield material
      INTEGER       BBMEDSTR     ! StructureTube material #
      INTEGER       BBNUMREM     ! Number of PMTs removed

      INTEGER       IVOLU        ! Error flag returned by GSVOLU
      INTEGER       NROW, NCOL   ! Loop index of row and column
      INTEGER       MROW, MCOL   ! Maximum # of row and column
      INTEGER       NDET         ! # of detector elements installed
      INTEGER       LROW, LCOL   ! Flag to install detector elements
      INTEGER       NV           ! # of volume description
      INTEGER       NBITSV(3)    ! NV bit numbers for packing the volume
      INTEGER       IDTYPE       ! Detector type ???
      INTEGER       NWPA         ! # of words for the primary alloc. of HITS
      INTEGER       NWSA         ! # of words for the primary alloc. of DIGI
      INTEGER       ISET         ! Position of set in bank JSET
      INTEGER       IDET         ! Position of detector in bank JSET
      INTEGER       RD           ! Valuables to find PMT to be removed.
      INTEGER       IP1,IP2      ! Integer pointer.

      REAL          BBABSORB(3)  ! Electron absorber with Rmax,Rmin,thick
      REAL          BBATTACH(10) ! Attachment PGON.
      REAL          BBBACKBD(3)  ! BackBoad with Rmin,Rmax,thick
      REAL          BBBREEDE(3)  ! Breeder with Rmin,Rmax,thick.
      REAL          BBCOVERT     ! BarrelCover thickness
      REAL          BBFRONTB(3)  ! FrontBoad with Rmin,Rmax,thick
      REAL          BBPMTSIZ(3)  ! PMT size with Rmin,Rmax,thick
      REAL          BBQUARTZ(10) ! Quartz of PGON.
      REAL          BBSHITHI     ! Metal shield thickness.
      REAL          BBSPACIN     ! Space btwn straight edges of adj. element.
      REAL          BBSTRUCT(3)  ! StructureTube with Rmin,Rmax,thick
      REAL          BBZPOSIT(2)  ! BBC Z-position, (1) for +Z
      REAL          BBPOSREM(2,MAXREM)
                                 ! X-Y position of PMTs to be removed.
 
      REAL          BBBCOVER(3)  ! BarrelCover with Rmin,Rmax,thick
      REAL          BBDETECT(10) ! Detector element of PGON.
      REAL          BBMOTHER(3)  ! BBC MotherVolume with Rmin,Rmax,thick
      REAL          BBSHIELD(10) ! Metal shield PGON.
      REAL          XPOS, YPOS, ZPOS
                                 ! Element position with X,Y,Z
      REAL          XYPS(2,MAXPMT) ! Element position
      REAL          RMIN, RMAX   ! Boundary of BBC
      REAL          RRMA, RRMI   ! Square of boundaries
      REAL          RRAD         ! Square of radius of element
      REAL          RREM         ! Valuables to find PMT to be removed.
      REAL          XSTP, YSTP   ! X and Y Pitch of element position
      REAL          XX,YY,RR     ! Temporary valuables.

      LOGICAL       LXYP(MAXPMT) ! Mount detector if TRUE.
      CHARACTER*4   SET_ID       ! Set identification
      CHARACTER*4   NAMESV(3)    ! NV volume descriptors
c---------------------------------------------------------------------
c Initialize default values for Namelist.

      DATA          BBCCOLOR/    6/
      DATA          BBCSEENN/    1/
      DATA          BBMEDABS/   26/
      DATA          BBMEDATT/   26/
      DATA          BBMEDBAC/   26/
      DATA          BBMEDBRE/  902/
      DATA          BBMEDCOV/   26/
      DATA          BBMEDFRO/   26/
      DATA          BBMEDMOT/   32/
      DATA          BBMEDPMT/  202/
      DATA          BBMEDQUA/  201/
      DATA          BBMEDSHI/   46/
      DATA          BBMEDSTR/   26/
      DATA          BBNUMREM/    2/
 
      DATA          BBABSORB/     5.500,    14.500,     0.000/
      DATA          BBATTACH/     0.000,   360.000,     6.000,
     &                            2.000,    -0.500,     0.200,
     &                            1.400,     0.500,     0.200,
     &                            1.400/
      DATA          BBBACKBD/     5.500,    15.000,     0.500/
      DATA          BBBREEDE/     0.000,     1.200,     1.950/
      DATA          BBCOVERT/     0.200/
      DATA          BBFRONTB/     5.500,    15.000,     0.500/
      DATA          BBPMTSIZ/     1.090,     1.290,     2.200/
      DATA          BBPOSREM/     9.840,     0.000,
     &                           -9.840,     0.000, 16*0.0/
      DATA          BBQUARTZ/     0.000,   360.000,     6.000,
     &                            2.000,    -1.500,     0.000,
     &                            1.270,     1.500,     0.000,
     &                            1.270/
      DATA          BBSHITHI/     0.100/
      DATA          BBSPACIN/     0.300/
      DATA          BBSTRUCT/     5.000,     5.500,    12.500/
      DATA          BBZPOSIT/   144.350,  -144.350/
c---------------------------------------------------------------------
c Namelist for Beam-Beam Counter.

      NAMELIST     /BBC_PAR/
     &              BBCCOLOR,
     &              BBCSEENN,
     &              BBMEDABS,
     &              BBMEDATT,
     &              BBMEDBAC,
     &              BBMEDBRE,
     &              BBMEDCOV,
     &              BBMEDFRO,
     &              BBMEDMOT,
     &              BBMEDPMT,
     &              BBMEDQUA,
     &              BBMEDSHI,
     &              BBMEDSTR,
     &              BBNUMREM,
     &              BBABSORB,
     &              BBATTACH,
     &              BBBACKBD,
     &              BBBREEDE,
     &              BBCOVERT,
     &              BBFRONTB,
     &              BBPMTSIZ,
     &              BBPOSREM,
     &              BBQUARTZ,
     &              BBSHITHI,
     &              BBSPACIN,
     &              BBSTRUCT,
     &              BBZPOSIT
c---------------------------------------------------------------------
c Data statements.

      DATA          NV/        3/
      DATA          SET_ID/   'BBC '/
      DATA          NAMESV/   'BBCM','BBCD','BBCQ'/
      DATA          NBITSV/    2,8,2/
      DATA          IDTYPE/    0/            ! for now
      DATA          NWPA/      50/           ! for now
      DATA          NWSA/      50/           ! for now
      
c---------------------------------------------------------------------
c     geometry description logical unit       
      integer itf_lun
      common /interface/itf_lun
      

c=====================================================================
c               EXECUTABLE STATEMENTS FOLLOW BELOW
c---------------------------------------------------------------------

      write(6,*) 'bbc - install the beam-beam counter.'
 
c---------------------------------------------------------------------
c     Read geometery


      write( *,* ) 'bbc - reading parameter from common interface'
      rewind( unit = itf_lun )
      read( unit = itf_lun, nml=bbc_par, err = 992 )
 
c---------------------------------------------------------------------
c The BBC geometry parameters have been read, now save them
c in a Zebra bank.

      call BBC_VARS(BBCCOLOR,BBCSEENN,BBMEDABS,BBMEDATT,
     &              BBMEDBAC,BBMEDCOV,BBMEDFRO,BBMEDMOT,
     &              BBMEDPMT,BBMEDQUA,BBMEDSTR,BBABSORB,
     &              BBATTACH,BBBACKBD,BBCOVERT,BBFRONTB,
     &              BBPMTSIZ,BBQUARTZ,BBSPACIN,BBSTRUCT,
     &              BBZPOSIT)
 
c---------------------------------------------------------------------
c Booking only when volume option was chosen for this detector 'BBC'

      IF(CVOLU_OPT(1,2).NE.'FULL'.AND.
     &   CVOLU_OPT(1,2).NE.'VOLS') THEN
        write(6,*) 'bbc - No volumes defined.'
        RETURN
      ENDIF
 
c---------------------------------------------------------------------
c Initialize parameters not explicitly given in Namelist.

      BBDETECT(1) = BBQUARTZ(1)
      BBDETECT(2) = BBQUARTZ(2)
      BBDETECT(3) = BBQUARTZ(3)
      BBDETECT(4) = BBQUARTZ(4)
      BBDETECT(5) = BBATTACH(5)+BBQUARTZ(5)-BBPMTSIZ(3)-BBBREEDE(3)
      BBDETECT(6) = BBQUARTZ(6)
      BBDETECT(7) = MAX(BBATTACH(7),BBQUARTZ(7),BBPMTSIZ(2))
      BBDETECT(8) = BBATTACH(8)+BBQUARTZ(8)+BBPMTSIZ(3)+BBBREEDE(3)
      BBDETECT(9) = BBQUARTZ(9)
      BBDETECT(10)= MAX(BBATTACH(10),BBQUARTZ(10),BBPMTSIZ(2))
 
      BBBCOVER(1) = MAX(BBABSORB(2),BBFRONTB(2),BBSTRUCT(2),BBBACKBD(2))
      BBBCOVER(2) = BBBCOVER(1)+BBCOVERT
      BBBCOVER(3) = BBSTRUCT(3)
      BBMOTHER(1) = MIN(BBABSORB(1),BBFRONTB(1),BBSTRUCT(1),BBBACKBD(1))
      BBMOTHER(2) = BBBCOVER(2)
      BBMOTHER(3) = BBSTRUCT(3)
 
      BBSHIELD(1) = BBQUARTZ(1)
      BBSHIELD(2) = BBQUARTZ(2)
      BBSHIELD(3) = BBQUARTZ(3)
      BBSHIELD(4) = BBQUARTZ(4)
      BBSHIELD(5) = BBQUARTZ(5)-BBPMTSIZ(3)-BBBREEDE(3)
      BBSHIELD(6) = MAX(BBATTACH(7),BBQUARTZ(7),BBPMTSIZ(2)) - BBSHITHI
      BBSHIELD(7) = MAX(BBATTACH(7),BBQUARTZ(7),BBPMTSIZ(2))
      BBSHIELD(8) = BBQUARTZ(8)+BBPMTSIZ(3)+BBBREEDE(3)
      BBSHIELD(9) = MAX(BBATTACH(10),BBQUARTZ(10),BBPMTSIZ(2)) - 
     &              BBSHITHI
      BBSHIELD(10)= MAX(BBATTACH(10),BBQUARTZ(10),BBPMTSIZ(2))

      YSTP = BBQUARTZ(7)+BBSPACIN*0.500
      XSTP = YSTP*SQRT(FLOAT(3))
 
      MROW = INT(BBFRONTB(2)/YSTP)+1
      MCOL = INT(BBFRONTB(2)/XSTP)+1
      RMAX = BBFRONTB(2)-BBDETECT(7)*FLOAT(2)/SQRT(FLOAT(3))
      RMIN = BBFRONTB(1)+BBDETECT(7)*FLOAT(2)/SQRT(FLOAT(3))
      RRMA = RMAX*RMAX
      RRMI = RMIN*RMIN
      
      IF (BBNUMREM.GT.MAXREM) GOTO 994
      IF (YSTP.LE.BBDETECT(7)) GOTO 995
c---------------------------------------------------------------------
c Create a BBC mother volume BBCM.

      call GSVOLU('BBCM','TUBE',BBMEDMOT,BBMOTHER,3,IVOLU)
      call GSATT ('BBCM','SEEN',BBCSEENN)
      call GSATT ('BBCM','COLO',BBCCOLOR)
 
c---------------------------------------------------------------------
c Place the Mother volume BBCM in the HALL volume.
c BBZPOSIT gives the z position of a Quartz radiator front face.

      IROT = IROT + 1
      call GSROTM(IROT,90.,0.,90.,90.,180.,0.)   ! mirror symmetry
      ZPOS = BBZPOSIT(1)+BBMOTHER(3)-2.*BBABSORB(3)
     &      -2.*BBFRONTB(3)-(BBATTACH(8)-BBATTACH(5))
      call GSPOS ('BBCM',1,'HALL',0.,0.,ZPOS,IROTNULL,'ONLY')  ! for +z
      ZPOS = BBZPOSIT(2)-BBMOTHER(3)+2.*BBABSORB(3)
     &      +2.*BBFRONTB(3)+(BBATTACH(8)-BBATTACH(5))
      call GSPOS ('BBCM',2,'HALL',0.,0.,ZPOS,IROT,'ONLY')  ! for -z
 
c---------------------------------------------------------------------
c Create a volume BBCE which goes into the mother volume BBCM.

      call GSVOLU('BBCE','TUBE',BBMEDABS,BBABSORB,3,IVOLU)
      call GSATT ('BBCE','SEEN',BBCSEENN)
      call GSATT ('BBCE','COLO',BBCCOLOR)
 
c---------------------------------------------------------------------
c Create a volume BBCF which goes into the mother volume BBCM.

      call GSVOLU('BBCF','TUBE',BBMEDFRO,BBFRONTB,3,IVOLU)
      call GSATT ('BBCF','SEEN',BBCSEENN)
      call GSATT ('BBCF','COLO',BBCCOLOR)
 
c---------------------------------------------------------------------
c Create a volume BBCS which goes into the mother volume BBCM.

      call GSVOLU('BBCS','TUBE',BBMEDSTR,BBSTRUCT,3,IVOLU)
      call GSATT ('BBCS','SEEN',BBCSEENN)
      call GSATT ('BBCS','COLO',BBCCOLOR)
 
c---------------------------------------------------------------------
c Create a volume BBCB which goes into the mother volume BBCM.

      call GSVOLU('BBCB','TUBE',BBMEDBAC,BBBACKBD,3,IVOLU)
      call GSATT ('BBCB','SEEN',BBCSEENN)
      call GSATT ('BBCB','COLO',BBCCOLOR)
 
c---------------------------------------------------------------------
c Create a volume BBCC which goes into the mother volume BBCM.

      call GSVOLU('BBCC','TUBE',BBMEDCOV,BBBCOVER,3,IVOLU)
      call GSATT ('BBCC','SEEN',BBCSEENN)
      call GSATT ('BBCC','COLO',BBCCOLOR)
 
c---------------------------------------------------------------------
c Create a detector volume BBCD which goes into the mother volume BBCM.

      call GSVOLU('BBCD','PGON',BBMEDMOT,BBDETECT,10,IVOLU)
      call GSATT ('BBCD','SEEN',BBCSEENN)
      call GSATT ('BBCD','COLO',BBCCOLOR)
 
c---------------------------------------------------------------------
c Place the Volumes BBCE, BBCF, BBCS, BBCB and BBCC in to BBCM.

      ZPOS = 0.0
      call GSPOS ('BBCS',1,'BBCM',0.,0.,ZPOS,IROTNULL,'ONLY')
      ZPOS = -BBMOTHER(3)+BBABSORB(3)
      call GSPOS ('BBCE',1,'BBCM',0.,0.,ZPOS,IROTNULL,'ONLY')
      ZPOS = -BBMOTHER(3)+2.*BBABSORB(3)+BBFRONTB(3)
      call GSPOS ('BBCF',1,'BBCM',0.,0.,ZPOS,IROTNULL,'ONLY')
      ZPOS =  BBMOTHER(3)-BBBACKBD(3)
      call GSPOS ('BBCB',1,'BBCM',0.,0.,ZPOS,IROTNULL,'ONLY')
      ZPOS = 0.0
      call GSPOS ('BBCC',1,'BBCM',0.,0.,ZPOS,IROTNULL,'ONLY')
 
c---------------------------------------------------------------------
c Place the volume BBCD into BBCM.

      ZPOS = -BBMOTHER(3)+2.*BBFRONTB(3)+2.*BBABSORB(3)
     &       +(BBDETECT(8)-BBDETECT(5))*0.500
      NROW = MROW
      NCOL = MCOL
      NDET = 0
                                        ! Find detector element location
                                        ! inside the BBC boundaries.
      DO WHILE (NROW.GE.-MROW)          ! Start from top left, and go right.
        LROW = ABS(NROW/2*2-NROW)       ! 0 for even, 1 for odd
        YPOS = FLOAT(NROW)*YSTP
        NCOL = MCOL
        DO WHILE (NCOL.GE.-MCOL)
          LCOL = ABS(NCOL/2*2-NCOL)     ! 0 for even, 1 for odd
          IF(LROW.EQ.LCOL) THEN         ! since elements are staggered.
            XPOS = FLOAT(NCOL)*XSTP
            RRAD = XPOS*XPOS+YPOS*YPOS
            IF((RRAD.LE.RRMA).AND.(RRAD.GE.RRMI)) THEN ! in range ?
              NDET = NDET+1
              IF (NDET.LE.MAXPMT) THEN
                XYPS(1,NDET) = XPOS
                XYPS(2,NDET) = YPOS
                LXYP(NDET) = .TRUE.
              ELSE
                GOTO 993
              ENDIF
            ENDIF ! range
          ENDIF ! even-odd
          NCOL = NCOL - 1
        ENDDO ! columun loop
        NROW = NROW - 1
      ENDDO ! raw loop

      DO IP1 = 1, BBNUMREM
        RD = 1
        XX = XYPS(1,1)-BBPOSREM(1,IP1)
        YY = XYPS(2,1)-BBPOSREM(2,IP1)
        RREM = XX*XX + YY*YY
        DO IP2 = 2, NDET
          XX = XYPS(1,IP2)-BBPOSREM(1,IP1)
          YY = XYPS(2,IP2)-BBPOSREM(2,IP1)
          RR = XX*XX + YY*YY
          IF (RR.LE.RREM) THEN
            RD = IP2
            RREM = RR
          ENDIF
        ENDDO
        LXYP(RD) = .FALSE.
      ENDDO

      DO IP1 = 1, NDET
        IF (LXYP(IP1)) call GSPOS ('BBCD',IP1,'BBCM',
     &       XYPS(1,IP1),XYPS(2,IP1), ZPOS, IROTNULL,'ONLY')
      ENDDO

      write(6,*)
     &  'bbc - Number of detector elements installed is ',
     &  NDET-BBNUMREM
 
c---------------------------------------------------------------------
c Create volumes BBCA, BBCQ, BBCP, BBCR and BBCH for attachment, Quartz, PMT,
c Breeder and metal shield.

      call GSVOLU('BBCA','PGON',BBMEDATT,BBATTACH,10,IVOLU)
      call GSATT ('BBCA','SEEN',BBCSEENN)
      call GSATT ('BBCA','COLO',BBCCOLOR)
 
      call GSVOLU('BBCQ','PGON',BBMEDQUA,BBQUARTZ,10,IVOLU)
      call GSATT ('BBCQ','SEEN',BBCSEENN)
      call GSATT ('BBCQ','COLO',BBCCOLOR)
 
      call GSVOLU('BBCP','TUBE',BBMEDPMT,BBPMTSIZ,3,IVOLU)
      call GSATT ('BBCP','SEEN',BBCSEENN)
      call GSATT ('BBCP','COLO',BBCCOLOR)
 
      call GSVOLU('BBCR','TUBE',BBMEDBRE,BBBREEDE,3,IVOLU)
      call GSATT ('BBCR','SEEN',BBCSEENN)
      call GSATT ('BBCR','COLO',BBCCOLOR)
 
      call GSVOLU('BBCH','PGON',BBMEDSHI,BBSHIELD,10,IVOLU)
      call GSATT ('BBCH','SEEN',BBCSEENN)
      call GSATT ('BBCH','COLO',BBCCOLOR)
 
c---------------------------------------------------------------------
c Place the BBCA, BBCQ, BBCP, BBCR and BBCH in to BBCD.

      ZPOS = -(BBDETECT(8)-BBDETECT(5))*0.500
     &       +(BBATTACH(8)-BBATTACH(5))*0.500
      call GSPOS ('BBCA',1,'BBCD',0.,0.,ZPOS,IROTNULL,'ONLY')
      ZPOS = -(BBDETECT(8)-BBDETECT(5))*0.500+(BBATTACH(8)-BBATTACH(5))
     &       +(BBQUARTZ(8)-BBQUARTZ(5))*0.500
      call GSPOS ('BBCQ',1,'BBCD',0.,0.,ZPOS,IROTNULL,'ONLY')
      ZPOS = -(BBDETECT(8)-BBDETECT(5))*0.500+(BBATTACH(8)-BBATTACH(5))
     &       +(BBQUARTZ(8)-BBQUARTZ(5))+BBPMTSIZ(3)
      call GSPOS ('BBCP',1,'BBCD',0.,0.,ZPOS,IROTNULL,'ONLY')
      ZPOS = -(BBDETECT(8)-BBDETECT(5))*0.500+(BBATTACH(8)-BBATTACH(5))
     &       +(BBQUARTZ(8)-BBQUARTZ(5))+BBPMTSIZ(3)*2.000+BBBREEDE(3)
      call GSPOS ('BBCR',1,'BBCD',0.,0.,ZPOS,IROTNULL,'ONLY')
      ZPOS = -(BBDETECT(8)-BBDETECT(5))*0.500+(BBATTACH(8)-BBATTACH(5))
     &       +(BBSHIELD(8)-BBSHIELD(5))*0.500
      call GSPOS ('BBCH',1,'BBCD',0.,0.,ZPOS,IROTNULL,'ONLY')
 
c---------------------------------------------------------------------
c Book hit structure for the sensitive volume.

      IF(CVOLU_OPT(1,2).EQ.'FULL') THEN   ! check volume option
        call GSDET(SET_ID,'BBCQ',NV,NAMESV,NBITSV,
     &             IDTYPE,NWPA,NWSA,ISET,IDET)       ! assign the detector
        call GSDETH(SET_ID,'BBCQ',NH,NAMESH,NBITSH,ORIG,FACT) ! define hit
      ENDIF ! check volume option
      write(6,*) 'bbc - The beam-beam counter was installed.'
      RETURN
 
 992  write(*,'(a)') 'bbc - Read error in bbc_par segment.'
      stop
      
     &  'bbc - Namelist mis-match in bbc_par segment of phnx.par.'
 993  write(*,'(a)')'bbc - Number of detecter exceeded MAXPMT.'
      stop ' BBC: Number of detecter exceeded MAXPMT.'
 994  write(*,'(a)')'bbc - Number of detecter to be '//
     &     'removed exceeded MAXREM.'
      stop
     &     'bbc - Number of detecter to be removed exceeded MAXREM.'
 995  write(*,'(a)')'bbc - Confliction of spacing.'
      stop ' BBC: Confliction of spacing.'
      END
