      subroutine g_user

c     Original Author: K. Barish (UCR)
c     Creation Date: February 24, 1999

c     Purpose: This routine functions as a simulation trigger which checks the
c              pattern of hits in selected subsystems in order to determine
c              whether the event is worth writing to the PISAEvent.root file.
c              Some of the triggers are controlled by what is in the ISWIT array
c              which is set in the gffgo.dat input file.
c              Other triggers are set by forced acceptance conditions on the event
c              types, or special purpose software for Muon Arm event gating.


c     Revision History
c     Name          Date           Comment
c     C.F. Maguire  Jan. 7, 2000   Require only one PC2 hit (West Arm only)

c     C.F. Maguire  Jan. 12, 2000  Above "fix" introduces a West Arm bias
c                                  for the phi->K+K- to show up almost
c                                  entirely in the West Arm; so we have to
c                                  eliminate any PC2 condition

c    C.H. Pinkenburg Feb. 09, 2001  check on # of hits returned by gfhits
c                                   indicating a too small user hit buffer 
c                                   (and set nhits to size of user buffer,
c                                   gfhits: GEANT manual page156)

c    C.F. Maguire    Feb. 09, 2003  Add TOF pair check using ISWIT(7) = 2 to 5
c                                   ISWIT(7) = 2 requires opp sign pair in TOF + DC/PC1
c                                   ISWIT(7) = 3 returns after a pair of opp sign hits in TOF
c                                   ISWIT(7) = 4 returns after only a single hit in TOF
c                                   ISWIT(7) = 5 requires only one hit in PC3 East

c    C.F. Maguire    Apr 21, 2003   ISWIT(7) = 6 requires TOF + PC3/PbSc EMCal pair
c                                   ISWIT(7) = 7 requires pair in East Pc3/PbSc EMCal 

c    C.F. Maguire    Feb 20, 2003   ISWIT(7) = 8 Muon Arm forced accept

c    C.F. Maguire    Dec 22, 2004   ISWIT(7) = 9 PC3 opposite sign pair from near vertex

c    C.F. Maguire    Jan 27, 2007   Software for second implemtation of Muon Arm staging

c     Purpose: Check particles for presence in all of PC1/PC2/PC3, and
c              for all planes in drift chamber.
c              Extended to allow forced accept in the Muon Arm


      IMPLICIT NONE


c     For TOF pair

      integer tofPos
      integer tofNeg
      logical accTOF

c for PAD1
      INTEGER nmchmb
      CHARACTER*4 NAMECH
      CHARACTER*4 NMPAD1(16),  NMGAS(16)

      integer nvdim, NWDIM
      integer nhmax
      integer nhddr
      PARAMETER ( NVDIM=4,NWDIM=4, NHMAX=1999,NHDDR= 13)
 
      INTEGER ITRAH(NHMAX), NUMVS(NVDIM), NUMVV(NVDIM, NHMAX)
      INTEGER NUMWS(NWDIM), NUMVW(NWDIM,NHMAX), NUBV(2,1000)
      INTEGER NUVS(5)
      REAL  HITD(NHDDR,NHMAX), HITSH(11,1000)

      real xglobal, yglobal, zglobal, pc3Phi, pc3Theta
 
#include "gcflag.inc"
#include "gcbank.inc"
#include "gcnum.inc"
#include "gconst.inc"
#include "gcvolu.inc"
#include "fstore.inc"
#include "guphnx.inc"
#include "sublink.inc"
#include "fpilink.inc"
#include "fpplink.inc"
#include "guevgen.inc"
#include "g77trigdef.inc"

      integer pos_p1, neg_p1
      integer pos_p2, neg_p2
      integer pos_p3, neg_p3
      integer pc1_west_neg, pc1_east_neg
      integer pc2_west_neg, pc2_east_neg
      integer pc3_west_neg, pc3_east_neg
      integer pc1_west_pos, pc1_east_pos
      integer pc2_west_pos, pc2_east_pos
      integer pc3_west_pos, pc3_east_pos

c      integer pad_c1(16), pad_c2(8), pad_c3(8)

      integer parent

c ********************* Define DC volume names *****************************

      character*4 arm1name,arm2name
      character*2 parm1name,parm2name
      character*2 name40(40)
      character*4 namec,namep
      character*1 name20(20),namee,namew,name8(8)

      data arm1name /'DCAW'/
      data arm2name /'DCAE'/
      data namee /'F'/
      data namew /'D'/
      data parm1name /'DP'/
      data parm2name /'FP'/
      data name40 /'01','02','03','04','05','06','07','08','09','10',
     +             '11','12','13','14','15','16','17','18','19','20',
     +             '21','22','23','24','25','26','27','28','29','30',
     +             '31','32','33','34','35','36','37','38','39','40'/
      data name8 /'1','2','3','4','5','6','7','8'/
      data name20 /'1','2','3','4','5','6','7','8','9','A','B','C',
     +     'D','E','F','G','H','I','J','K'/

      integer igroup, iplane, jplane
      logical ACCDC, ACCPAD

      logical ACCDC_WEST, ACCDC_EAST
      logical ACCPAD_WEST, ACCPAD_EAST
      logical ACCDCPAD_WEST, ACCDCPAD_EAST

      INTEGER IC, NHITS, IH

      CHARACTER*4 VPC2SHELL(8), VPC3SHELL(8) 
      CHARACTER*4 VPC2GASGP(8), VPC3GASGP(8)

      integer nmchm    ! number of chamber subsections (fixed at 8)
      parameter (nmchm = 8)   ! change for UCDR version

      real vert(3), pvert(4), buf(10), amass, charge, tlife, ub(10)
      integer ipart, nvert, nbuf, itrtyp, nwb
      character*20 napart

      DATA NMPAD1 /'P101', 'P102', 'P103', 'P104', 'P105', 'P106',
     +'P107', 'P108', 'P109', 'P110', 'P111', 'P112', 'P113', 'P114',
     +'P115', 'P116' /

      DATA NMGAS /'ZZ01', 'ZZ02', 'ZZ03', 'ZZ04', 'ZZ05', 'ZZ06',
     +'ZZ07', 'ZZ08', 'ZZ09', 'ZZ10', 'ZZ11', 'ZZ12', 'ZZ13', 'ZZ14',
     +'ZZ15', 'ZZ16' /

      DATA VPC2SHELL /'PD21', 'PD22', 'PD23', 'PD24', 'PD25', 'PD26',
     +'PD27', 'PD28' /
      DATA VPC2GASGP /'AR21', 'AR22', 'AR23', 'AR24', 'AR25', 'AR26',
     +  'AR27', 'AR28' /
      DATA VPC3SHELL /'PD31', 'PD32', 'PD33', 'PD34', 'PD35', 'PD36',
     +'PD37', 'PD38' /
      DATA VPC3GASGP /'AR31', 'AR32', 'AR33', 'AR34', 'AR35', 'AR36',
     +'AR37', 'AR38' / 

      integer cnt /0/
      integer cnt1 /0/
      integer cnt3 /0/
      integer countPc3Pairs /0/
      integer dc_pos(40), dc_neg(40), i
      integer DC_WEST_NEG(40), DC_EAST_NEG(40)
      integer DC_WEST_POS(40), DC_EAST_POS(40)


c     Begin execution


      if(NSTAGE.gt.0)then
         call muonArmStageOutput(-1)
      endif                     ! muon Arm staging NTUPLE fill

      if(NSTAGESTACK.gt.0)then

c     Separate software for Muon Arm intra-event staging
c     Don't want to clutter up g_user anymore

         call muonArmStackStage(-1)  ! writes information on cloned particles to output NTUPLE
         call muidStageCheck         ! checks the MuID final filter condition for hits retention
         return                      ! immediate return since this is a Muon Arm intra-event staged  simulation
      endif                     ! muon Arm intra-event staged simulation check

      if(MUIDWRITE.GT.0)then
         MUIDACCEPTED = .FALSE.
         

c   Code from mun_digi, with some names changes of the arrays


         write( namep, '( a4)' ) 'MUGS'  ! mun_digi used CUDET variable name
         call gfhits(
     &        'MUN '            ! set identifier
     &        , namep           ! detector identifier
     &        , 2               ! dim of path identification
     &        , 11              ! dim of hit array
     &        , nhmax           ! max number of returned hits
     &        , 0               ! take all tracks
     &        , nuvs            ! volume descriptor
     &        , itrah           ! array of hit producing tracks
     &        , nubv            ! volume descriptor number
     &        , hitsh           ! hit values
     &        , nhits )         ! number of hits

         if(nhits.le.0)then
c            write(6,886)ievent,nhits
 886        format(' g_user <I>: At event ',i5,
     +             ' nhits = ', i6, ' immediate return',/)
            return
         endif                  ! immediate check if no MuID layers were hit

c     Now we decipher the plane number

         do ih = 1,nhits
            iplane = nubv(1,ih)

c     Assume that there a six MuID layers in each arm; could check with qf(lfn_para + 2)

            if(iplane.gt.6) then
               iplane = iplane - 6
            endif               !  check for South arm

c           write(6,881)ih, nhits, nubv(1,ih), iplane
 881        format(//, ' g_user <I>: ',
     +           ' ih, nhits, nubv(1,ih), iplane = ',
     +           4i8)
      
            if(iplane.lt.1 .or. iplane.gt.6)then
               write(6,888)ih, nhits, nubv(1,ih), iplane
 888           format(//, ' g_user <F>: bad MuID plane number ',
     +                ' ih, nhits, nubv(1,ih), iplane = ',
     +                4i8)
               stop ' program error'
            endif  ! safety check on plane number

            if(iplane.eq.MUIDWRITE)then
c              write(6,887)ievent, iplane, nubv(1,ih),
c    +                     nhits, ih
 887           format(' g_user <I>: accepted event ', i5,
     +                ', plane ', i2, ' nubv ',i3,
     +                ', nhits ', i5, ' ihit ', i5,/)
               MUIDACCEPTED = .TRUE.
               return
            endif ! found a hit in the specified MuID plane

         enddo                  ! check the hits
         
c        write(6,889)ievent, nhits
 889     format(' g_user <I>: rejected event ', i5, 
     +          ' nhits ', i5,/)
         return  !  will reach here if no matching MuID plane hit was found

      endif                     ! checking for hits in MuID Layer = MUIDWRITE

      if(.not.logacc.or.ikine2(2).eq.4.or.iswit(7).eq.8)then
         return  ! skip if not forced acceptance, or sngl_neutral, or muon arm
      endif


c     Default is that the event is not accepted for writing to output by PISA

      logaccepted = .FALSE.


c     Do first check on opposite sign pair in TOF, if requested
c     Also allow for single hit in TOF when iswit(7) = 4

      tofPos = 0
      tofNeg = 0
      if(iswit(7).eq.2.or.iswit(7).eq.3.or.iswit(7).eq.4.or.
     +   iswit(7).eq.6)then   ! checking TOF hits
         accTOF = .FALSE.

c     Short slat call

         namep = 'SCTS'
         call gfhits('TOF ', namep, 2, 11, nhmax, 0, numvs, itrah,
     +               numvw, hitd, nhits)
         do ih = 1,nhits
            call gfkine(itrah(ih),vert,pvert,ipart,nvert,buf,nbuf)
            if(nbuf.eq.3)then
               parent = buf(2)  ! parent ID of an "orphan" track
            endif
            call gfpart(ipart,napart,itrtyp,amass,charge,tlife,
     +           ub,nwb)
            if(itrah(ih).le.2 .or.
     +           (iswit(7).ge.1.and.itrah(ih).le.3)) then

c     We have a hit in the TOF

               if(charge.gt.0) tofPos = 1
               if(charge.lt.0) tofNeg = 1
               if(iswit(7).eq.6)then
                  accTOF = .true.   ! require only one TOF hit in TOF-East PC3/EMCal
               endif
               if(iswit(7).eq.4)then
                  logaccepted = .TRUE.
                  nrvacc_evt = nrvacc_evt+1
                  return   ! check only TOF single hit if ISWIT(7) = 4
               endif ! check on requiring only one hit in TOF
               if(tofPos.eq.1.and.tofNeg.eq.1)then
                  accTOF = .TRUE.
                  if(iswit(7).eq.3)then
                     logaccepted = .TRUE.
                     nrvacc_evt = nrvacc_evt+1
                     return  ! check only TOF pair if ISWIT(7) = 3
                  endif
               endif  ! check for opposite sign pair
            endif  ! check on charge sign of this hit
         enddo ! loop over short slat hits

c     Long slat call

         namep = 'SCTL'
         call gfhits('TOF ', namep, 2, 11, nhmax, 0, numvs, itrah,
     +               numvw, hitd, nhits)
         do ih = 1,nhits
            call gfkine(itrah(ih),vert,pvert,ipart,nvert,buf,nbuf)
            if(nbuf.eq.3)then
               parent = buf(2)  ! parent ID of an "orphan" track
            endif
            call gfpart(ipart,napart,itrtyp,amass,charge,tlife,
     +           ub,nwb)
            if(itrah(ih).le.2 .or.
     +           (iswit(7).ge.1.and.itrah(ih).le.3)) then

c     We have a hit in the TOF

               if(charge.gt.0) tofPos = 1
               if(charge.lt.0) tofNeg = 1
               if(iswit(7).eq.6)then
                  accTof = .true.
               endif
               if(iswit(7).eq.4)then
                  logaccepted = .TRUE.
                  nrvacc_evt = nrvacc_evt+1
                  return   ! check only TOF single hit if ISWIT(7) = 4
               endif ! check on requiring only one hit in TOF
               if(tofPos.eq.1.and.tofNeg.eq.1)then
                  accTOF = .TRUE.
                  if(iswit(7).eq.3)then
                     logaccepted = .TRUE.
                     nrvacc_evt = nrvacc_evt+1
                     return  ! check only TOF pair if ISWIT(7) = 3
                  endif
               endif  ! check for opposite sign pair
            endif  ! check on charge sign of this hit
         enddo ! loop over long slat hits
         if(.not.accTOF)then
            return
         endif ! did not find the requested pair in TOF or requested single hit in TOF
      endif ! check on pair in TOF


c     ISWIT(7) = 5 is single hit in PC3
c     ISWIT(7) = 6 is TOF-East PC3/EMCal pair
c     ISWIT(7) = 7 is East PC3/EMCal pair
c     ISWIT(7) = 9 is opposite sign PAIR in PC3
c     None of these requires a DC/PC1 or PC2 check

      if(ISWIT(7).EQ.5 .OR. ISWIT(7).EQ.6 .OR.
     +   ISWIT(7).EQ.7.OR.ISWIT(7).EQ.9)GO TO 550

      do i=1,40
         dc_pos(i) = 0
         dc_neg(i) = 0
         DC_WEST_NEG(I) = 0
         DC_EAST_NEG(I) = 0
         DC_WEST_POS(I) = 0
         DC_EAST_POS(I) = 0
      enddo

c Loop over the DC west arm hit information

      do igroup = 1,2   ! loop over wire groups
         do iplane = 1,20   ! loop over planes in group
            jplane = iplane
            if (igroup.eq.2) jplane = iplane + 20
            namec = arm1name
            namep = parm1name//name40(jplane)
            call gfhits('ITR ',namep,nvdim,nhddr,nhmax,0,
     +           numvs,itrah,numvv,hitd,nhits)

C /chp/ if user array hitd exceeded, nhits is returned as nhmax+1
            if (nhits .gt. nhmax) then
             write(6,*) '<W> ITR (g_user.f): number of hits exceeds',
     #       nhmax,' nhit truncated to ',nhmax,' for ',namep
             nhits = nhmax
            end if

            if (nhits.gt.0) then
               do ih=1,nhits    ! loop over hits on the plane
c                write(6,*) namep, itrah(ih), hitd(8,ih), iplane, jplane
                call gfkine(itrah(ih),vert,pvert,ipart,nvert,buf,nbuf)
                if(nbuf.eq.3)then
                   parent = buf(2)   ! parent ID of an "orphan" track
                endif
                call gfpart(ipart,napart,itrtyp,amass,charge,tlife,
     +                      ub,nwb)
                if(itrah(ih).le.2 .or.
     +             (iswit(7).ge.1.and.itrah(ih).le.3)) then
                   if(charge.gt.0) dc_pos(jplane)=1
                   if(charge.lt.0) dc_neg(jplane)=1

C - BKN - 03/13/2000

                   if(charge.lt.0) dc_west_neg(jplane)=1 ! for single particle
                   if(charge.gt.0) dc_west_pos(jplane)=1 ! for single particle
                endif
               enddo
            endif
         enddo
      enddo

c      do i = 1, 40
c         write(16,*)'*** west DC ***',dc_pos(i),dc_neg(i)
c      enddo
c Now loop over the DC east arm hit information

      do igroup = 1,2   ! loop over wire groups
         do iplane = 1,20   ! loop over planes in group
            jplane = iplane
            if (igroup.eq.2) jplane = iplane + 20
            namec = arm2name
            namep = parm2name//name40(jplane)
            call gfhits('ITR ',namep,nvdim,nhddr,nhmax,0,
     +           numvs,itrah,numvv,hitd,nhits)

C /chp/ if user array hitd exceeded, nhits is returned as nhmax+1
            if (nhits .gt. nhmax) then
             write(6,*) '<W> ITR (g_user.f): number of hits exceeds',
     #       nhmax,' nhit truncated to ',nhmax,' for ',namep
             nhits = nhmax
            end if

            if (nhits.gt.0) then
               do ih=1,nhits    ! loop over hits on the plane
                call gfkine(itrah(ih),vert,pvert,ipart,nvert,buf,nbuf)
                if(nbuf.eq.3)then
                   parent = buf(2)   ! parent ID of an "orphan" track
                endif
                call gfpart(ipart,napart,itrtyp,amass,charge,tlife,
     +                      ub,nwb)
                if(itrah(ih).le.2.or.
     +             (iswit(7).ge.1.and.itrah(ih).le.3)) then
                   if(charge.gt.0) dc_pos(jplane)=1
                   if(charge.lt.0) dc_neg(jplane)=1

C - BKN - 03/13/2000

                   if(charge.lt.0) dc_east_neg(jplane)=1 ! for single particle
                   if(charge.gt.0) dc_east_pos(jplane)=1 ! for single particle
                endif
c                write(6,*) namep, itrah(ih), hitd(8,ih), iplane, jplane
               enddo
            endif
         enddo
       enddo
c      do i = 1, 40
c         write(16,*)'*** east DC ***',dc_pos(i),dc_neg(i)
c      enddo


c     Now check pads

c      do i = 1, 16
c         pad_c1(i) = 0
c      enddo
c      do i = 1, 8
c         pad_c2(i) = 0
c         pad_c3(i) = 0
c      enddo

      CALL VZERO ( NUMVS, NVDIM)      ! PAD1
      nmchmb = iqf( lfi_para + 6)     ! number of PC1 sectors
      CALL VZERO ( NUMWS, NWDIM )     ! PAD23

*     The loop on the number of chambers -- PC2

      pos_p1=0
      neg_p1=0
      pc1_west_neg = 0
      pc1_east_neg = 0
      pc1_west_pos = 0
      pc1_east_pos = 0

      DO 133 IC = 1, NMCHMB
         NAMECH = NMGAS(IC)
         CALL GFHITS ( 'ITR ', NAMECH, NVDIM, NHDDR, NHMAX, 0,
     >                 NUMVS, ITRAH, NUMVV, HITD, NHITS )

C /chp/ if user array hitd exceeded, nhits is returned as nhmax+1
         if (nhits .gt. nhmax) then
           write(6,*) '<W> ITR (g_user.f): number of hits exceeds',
     #     nhmax,' nhits truncated to ',nhmax,' for ',namech
           nhits = nhmax
         end if

         IF ( NHITS .LE. 0 ) GO TO 133

         
         DO IH = 1, NHITS
            call gfkine(itrah(ih),vert,pvert,ipart,nvert,buf,nbuf)
            if(nbuf.eq.3)then
               parent = buf(2)   ! parent ID of an "orphan" track
            endif
            call gfpart(ipart,napart,itrtyp,amass,charge,tlife,ub,nwb)
c            write(6,*) ih,itrah(ih), vert, pvert, ipart, nvert, 
c     1                 buf, nbuf
c            write(6,*) ipart, napart, itrtyp, amass, charge, tlife, 
c     1                  ub, nwb
            
c            write(6,*) charge, ipart, namech, itrah(ih)

            if(itrah(ih).le.2.or.
     +         (iswit(7).ge.1.and.itrah(ih).le.3)) then
               if(charge.gt.0) pos_p1=1
               if(charge.lt.0) neg_p1=1

c               pad_c1(ic) = 1

C - BKN - 03/13/2000

C - For single particle

               if(IC.le.8)then
                  if(charge.lt.0) pc1_east_neg = 1
                  if(charge.gt.0) pc1_east_pos = 1
               else
                  if(charge.lt.0) pc1_west_neg = 1
                  if(charge.gt.0) pc1_west_pos = 1
               endif
            endif

         ENDDO ! loop on number of hits
133   CONTINUE

c      do i= 1, 16
c         write(16,*)'*** PAD1 ***',pad_c1(i),i
c      enddo



*     The loop on the number of chambers -- PC2

      pos_p2=0
      neg_p2=0
      pc2_west_neg = 0
      pc2_east_neg = 0
      pc2_west_pos = 0
      pc2_east_pos = 0

      DO 433 IC = 1, NMCHM
         NAMECH= VPC2GASGP(IC)
          CALL GFHITS ( 'PAD ', NAMECH, NWDIM, NHDDR, NHMAX, 0,
     >                 NUMWS, ITRAH, NUMVW, HITD, NHITS )

C /chp/ if user array hitd exceeded, nhits is returned as nhmax+1
          if (nhits .gt. nhmax) then
            write(6,*) '<W> PAD (g_user.f): number of hits exceeds',
     #      nhmax,' nhits truncated to ',nhmax,' for ',namech
            nhits = nhmax
          end if

          IF ( NHITS .GE. 1 ) then
             DO 400 IH = 1, NHITS
               call gfkine(itrah(ih),vert,pvert,ipart,nvert,buf,nbuf)
               if(nbuf.eq.3)then
                  parent = buf(2)   ! parent ID of an "orphan" track
               endif
               call gfpart(ipart,napart,itrtyp,amass,charge,
     >                     tlife,ub,nwb)
c              write(6,*) charge, ipart, namech, itrah(ih)
               if(itrah(ih).le.2.or.
     +            (iswit(7).ge.1.and.itrah(ih).le.3)) then
                 if(charge.gt.0) pos_p2=1
                 if(charge.lt.0) neg_p2=1

c                 pad_c2(ic) = 1

C - BKN - 03/13/2000

C - For single particle
                 if(IC.le.4)then
                    if(charge.lt.0) pc2_east_neg = 1
                    if(charge.gt.0) pc2_east_pos = 1
                 else
                    if(charge.lt.0) pc2_west_neg = 1
                    if(charge.gt.0) pc2_west_pos = 1
                 endif

               endif
400          CONTINUE
          ENDIF
433   CONTINUE   ! loop over chamber subsections  PAD2
c      do i = 1, 8
c         write(16,*)'*** PAD2 ***',pad_c2(i),i
c      enddo

550   continue  ! branch here for ISWIT(7) = 5 or 6 or 7 or 9

*    The loop on the number of chambers in PAD 3

      pos_p3 = 0
      neg_p3 = 0
      pc3_west_neg = 0
      pc3_east_neg = 0
      pc3_west_pos = 0
      pc3_east_pos = 0

      DO 633 IC = 1, NMCHM
         NAMECH= VPC3GASGP(IC)
         CALL GFHITS ( 'PAD ', NAMECH, NWDIM, NHDDR, NHMAX, 0,
     >                NUMWS, ITRAH, NUMVW, HITD, NHITS )

C /chp/ if user array hitd exceeded, nhits is returned as nhmax+1
         if (nhits .gt. nhmax) then
           write(6,*) '<W> PAD (g_user.f): number of hits exceeds',
     #     nhmax,' nhits truncated to ',nhmax,' for ',namech
           nhits = nhmax
         end if

         IF ( NHITS .GE. 1 )then

*      loop on the number of hits for the chamber sector

            DO 600 IH = 1, NHITS
               call gfkine(itrah(ih),vert,pvert,ipart,nvert,buf,nbuf)
               if(nbuf.eq.3)then
                  parent = buf(2) ! parent ID of an "orphan" track
               endif
               call gfpart(ipart,napart,itrtyp,amass,charge,
     >              tlife,ub,nwb)
               if(itrah(ih).le.2.or.
     +              (iswit(7).ge.1.and.itrah(ih).le.3)) then
                  if(charge.gt.0) pos_p3=1
                  if(charge.lt.0) neg_p3=1
                  if(iswit(7).eq.9.and.
     +               pos_p3.eq.1.and.neg_p3.eq.1)then

c     We have an opposite sign pair in PC3 with low track numbers
c     Indicates an initial decay pair
c     Mark event as accepted and return immediately

                      logaccepted=.TRUE.         
                      nrvacc_evt = nrvacc_evt+1
                      countPc3Pairs = countPc3Pairs + 1
                      if(countPc3Pairs.lt.10)then
                         write(6,551)nrvacc_evt
 551                     format(/, ' g_user<I>: ',
     +                         ', ISWIT(7) = 9 ',
     +                         ', nrvacc_evt = ', i4)
                      endif
                      return
                  endif

C - BKN - 03/13/2000

C - For single particle
                  if(IC.le.4)then
                     if(charge.lt.0) pc3_east_neg = 1
                     if(charge.gt.0) pc3_east_pos = 1

c     We have a hit in PC3 East

                     xglobal = hitd(10,ih)
                     yglobal = hitd(11,ih)
                     zglobal = hitd(12,ih)
                     pc3Phi = atan2d(yglobal, xglobal)
                     if(pc3Phi.lt.-90.0)then
                        pc3Phi = 360.0 + pc3Phi
                     endif
                     pc3Theta = acosd(zglobal/sqrt(xglobal*xglobal +
     +                    yglobal*yglobal + zglobal*zglobal))
                     if(iswit(7).eq.5)then
                        logaccepted = .TRUE.
                        nrvacc_evt = nrvacc_evt+1
                        return  ! check only PC3 East single hit if ISWIT(7) = 5
                     endif      ! check on requiring only one hit in PC3 East

                     if(ic.le.2 .and. iswit(7).eq.7) then
                        if(pc3_east_neg.eq.1.and.pc3_east_pos.eq.1)then
                           logaccepted = .TRUE.
                           nrvacc_evt = nrvacc_evt+1
                           if(nrvacc_evt.lt.101)then
                              write(6,598)nrvacc_evt, ic, 
     +                             pc3_east_pos, pc3_east_neg, ipart,
     +                             pc3Theta, pc3Phi
 598                          format(1h ,'nrvacc ', i4, ',  ic ', i4,
     +                             ',  pc3_pos ', i4, ',  pc3_neg ', i4,
     +                             ',  ipart ', i4, ' Theta ', f8.3,
     +                             ',  Phi ', f8.3)
                           endif
                           return ! opposite sign pair in East PC3/EMCal with ISWIT(7) = 7
                        endif   ! check for opposite sign pair in East PC3/PbSc EMCal

                     endif      ! check for East PC3 and ISWIT(7) = 7

                     if(ic.le.2 .and. iswit(7).eq.6) then
                        if((tofPos.eq.1.and.pc3_east_neg.eq.1).or.
     +                       (tofNeg.eq.1.and.pc3_east_pos.eq.1))then
                           logaccepted = .TRUE.
                           nrvacc_evt = nrvacc_evt+1
                           if(nrvacc_evt.lt.101)then
                              write(6,599)nrvacc_evt,ic,tofPos,tofNeg,
     +                             pc3_east_pos, pc3_east_neg, ipart,
     +                             pc3Theta, pc3Phi
 599                          format(1h ,'nrvacc ', i4, ',  ic ', i4,
     +                             ',  tofPos ', i4, ',  tofNeg ', i4,
     +                             ',  pc3_pos ', i4, ',  pc3_neg ', i4,
     +                             ',  ipart ', i4, ' Theta ', f8.3,
     +                             ',  Phi ', f8.3)
                           endif
                           return ! opposite sign pair in TOF-PC3/EMCal with ISWIT(7) = 6
                        endif   ! check that both TOF and PC3 fired, opposite sign for ISWIT(7) = 6
                     endif      ! check on require TOF-East PC3/EMCal Pair     
                  else
                     if(charge.lt.0) pc3_west_neg = 1
                     if(charge.gt.0) pc3_west_pos = 1
                  endif
                 
               endif

 600        CONTINUE
         ENDIF
633   CONTINUE  ! loop over chamber subsections  PAD3 

      if(ISWIT(7).EQ.5 .OR. ISWIT(7).eq.6 .OR.
     +   ISWIT(7).EQ.7)then
         logaccepted = .FALSE.  ! redundant
         return;  ! no single PC3 or TOF-East PC3/EMCal pair found
      endif

      IF(SINGLE_ACC.eq.1) GOTO 399


c     write(6,*) pos_p1,neg_p1,pos_p2, neg_p2,pos_p3,neg_p3

      if      (pos_p1.eq.1.and.neg_p1.eq.1) then
         acc_part(1)=3
      else if (pos_p1.eq.1.and.neg_p1.eq.0) then
         acc_part(1)=1
      else if (pos_p1.eq.0.and.neg_p1.eq.1) then
         acc_part(1)=2
      else
         acc_part(1)=0
      endif

      if      (pos_p2.eq.1.and.neg_p2.eq.1)  then
         acc_part(2)=3
      else if (pos_p2.eq.1.and.neg_p2.eq.0)  then
         acc_part(2)=1
      else if (pos_p2.eq.0.and.neg_p2.eq.1)  then
         acc_part(2)=2
      else                                       
         acc_part(2)=0
      endif

      if      (pos_p3.eq.1.and.neg_p3.eq.1)  then
         acc_part(3)=3
      else if (pos_p3.eq.1.and.neg_p3.eq.0)  then
         acc_part(3)=1
      else if (pos_p3.eq.0.and.neg_p3.eq.1)  then
         acc_part(3)=2
      else                                       
         acc_part(3)=0
      endif

      ACCDC = .TRUE.
      do 700 i=1,40
         if(dc_neg(i).eq.0) ACCDC=.FALSE.
         if(dc_pos(i).eq.0) ACCDC=.FALSE.
700   enddo

      ACCPAD = .FALSE.

c     Change by C.F. Maguire on January 7, 2000
c     PC2 default is now West-Arm only
c     So require only 1 PC2 hit (low p_t in opposite arms)

c     Change by C.F. Maguire on January 15, 2000
c     remove any requirement on PC2 to prevent biasing

      if(acc_part(1).eq.3.and.acc_part(2).ge.0.and.acc_part(3).eq.3)then 
          ACCPAD=.TRUE.
      endif
      if(ACCPAD.and.ACCDC) then
         logaccepted=.TRUE.         
         nrvacc_evt = nrvacc_evt+1
          cnt=cnt+1
c          write(6,*) "*ACCEPTED*", nrvacc_evt, ACCDC
c          write(6,*) "Total Number of Accepted Events = ", cnt
      endif
      if(.not.ACCDC.AND.ACCPAD) then
          cnt3=cnt3+1
      endif
c      write(6,*) "Total number passing PAD Not DC = ", cnt3
      cnt1=cnt1+1
c      write(6,*) "Total Number of Generated Events = ", cnt1


c     end pair check

      GOTO 499
      

C - BKN - 03/13/2000
C         Single particle check

 399  CONTINUE



C - Check whether electron is in all the planes of DC (east or west)

      ACCDC_WEST = .TRUE.
      ACCDC_EAST = .TRUE.
      do i=1,40
         IF(charge.LT.0)THEN
            if(dc_west_neg(i).eq.0) ACCDC_WEST=.FALSE.
            if(dc_east_neg(i).eq.0) ACCDC_EAST=.FALSE.
         ENDIF
         IF(charge.GT.0)THEN
            if(dc_west_pos(i).eq.0) ACCDC_WEST=.FALSE.
            if(dc_east_pos(i).eq.0) ACCDC_EAST=.FALSE.
         ENDIF
c         write(6,*)'*** dc_west_east ***',
c     &        dc_west_el(i),dc_east_el(i)
      enddo

c      write(6,*)'*** pad_west_east ***',
c     &     pc1_west_el,pc1_east_el,
c     &     pc2_west_el,pc2_east_el,
c     &     pc3_west_el,pc3_east_el


C - Check whether electron is in all the planes of PAD (east or west)

      ACCPAD_WEST = .FALSE.
      ACCPAD_EAST = .FALSE.
      IF(charge.LT.0)THEN
         if(PC1_EAST_NEG.eq.1.and.PC3_EAST_NEG.eq.1)ACCPAD_EAST = .TRUE.
         if(PC1_WEST_NEG.eq.1.and.PC2_WEST_NEG.eq.1.and.
     &        PC3_WEST_NEG.eq.1)ACCPAD_WEST = .TRUE.
      ENDIF
      IF(charge.GT.0)THEN
         if(PC1_EAST_POS.eq.1.and.PC3_EAST_POS.eq.1)ACCPAD_EAST = .TRUE.
         if(PC1_WEST_POS.eq.1.and.PC2_WEST_POS.eq.1.and.
     &        PC3_WEST_POS.eq.1)ACCPAD_WEST = .TRUE.
      ENDIF


C - Check the particle in all the planes of DC and PAD (east or west)

      ACCDCPAD_WEST = .FALSE.
      ACCDCPAD_EAST = .FALSE.
      if(ACCDC_WEST.and.ACCPAD_WEST)ACCDCPAD_WEST = .TRUE.
      if(ACCDC_EAST.and.ACCPAD_EAST)ACCDCPAD_EAST = .TRUE.

C - Check the particle in either arm (east or west)

      if(ACCDCPAD_WEST.or.ACCDCPAD_EAST)then
         logaccepted=.TRUE.         
         nrvacc_evt = nrvacc_evt+1
      endif

 499  CONTINUE

      if(nrvacc_evt.gt.0.and.
     +   (nrvacc_evt - 100*(nrvacc_evt/100).eq.0))then
        write(6,*)"  Forced accepted events = ", nrvacc_evt
      endif

      return
      end
