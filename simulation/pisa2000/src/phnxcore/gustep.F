c $Id: gustep.F,v 1.29 2018/07/05 16:50:12 lajoie Exp $

      SUBROUTINE GUSTEP
      IMPLICIT NONE
C.    ******************************************************************
C.    *                                                                *
C.    *                                                                *
C.    *       User routine called at the end of each tracking step     *
C.    *       INWVOL is different from 0 when the track has reached    *
C.    *              a volume boundary                                 *
C.    *       ISTOP is different from 0 if the track has stopped       *
C.    *                                                                *
C.    *                                                                *
C.    ******************************************************************

#include "geant321/gcking.inc"
#include "gckine.inc"
#include "gcbank.inc"
#include "gctrak.inc"
#include "gcflag.inc"
#include "gctmed.inc"
#include "gcsets.inc"
#include "gcvolu.inc"
#include "gugeom.inc"
#include "namlvl.inc"
#include "guphnx.inc"
#include "secubuf.inc"
#include "gcunit.inc"
#include "pisa_parts.inc"
#include "gclist.inc"
#include "subevt.inc"
#include "gcjloc.inc"

#include "guevgen.inc"

      integer igauto_save/-1/
      


c     local variables

 

c     CFM insertions for variable cut offs on MAGF line

      real ztest, rtest, zcut, rcut, pcut, ptest, phitest

c     Defining the (R,Z) limits for secondary parent particle storage

      real zlim_ecal, rmin_ecal, rmax_ecal   ! EMCal system
      parameter (zlim_ecal = 200.0)
      parameter (rmin_ecal = 505.0)
      parameter (rmax_ecal = 600.0)

      real zlim_rich, rmin_rich, rmax_rich   ! RICH system (replaces CERK)
      parameter (zlim_rich = 300.0)
      parameter (rmin_rich = 190.0)
      parameter (rmax_rich = 420.0)

      real zlim_mvd, rmin_mvd, rmax_mvd      ! MVD/beam pipe/vacuum
      parameter (zlim_mvd = 50.0)
      parameter (rmin_mvd = 0.0)
      parameter (rmax_mvd = 30.0)


      logical gstcut /.false./   ! switch to activate GUSTEP kills
 

c     cfm insertions for not storing hits if so requested

      integer iudet               ! detector name returned as an integer
      integer iuset               ! set name returned as an integer
      integer iwarn /0/           ! first time call
      integer js                  ! pointer to set name
      integer kset                ! loop index over set names
      character*4 d_name          ! detector name as a character
      character*4 s_name          ! set name as a character
      equivalence (iudet, d_name) ! dangerous on some machines ?  GSI does it
      equivalence (iuset, s_name) ! dangerous on some machines ?  GSI does it
      logical logwrn              ! one time warning message
 
      REAL*4   HIT_ALL(11)
 
      integer ihit, I, ITRASAV, IVOLNAME
 

c  October 8, 1994: Tonse/Maguire change

c  Special subsystem particles now defined in PISA_PARTS sequence

c      DATA PP_CRK_PHOT/48/, ID_TRDPHOT/58/

      LOGICAL FIRST,INTR_STORE, TRD_STORE(2)
      DATA FIRST/.TRUE./, INTR_STORE/.FALSE./, TRD_STORE/2*.FALSE./
      character*4 SETNAME,DETNAME,VOLNAME
      EQUIVALENCE(SETNAME,IHSET),(DETNAME,IHDET),(IVOLNAME,VOLNAME)
      real x_daughter(3), ELECTAG, ELECTYP, MUTYPE
      logical login, logout         ! vertex pair hit logic
      data login /.false./       ! vertex pair hit logic
      data logout /.true./       ! vertex pair hit logic

      integer stackStageTest

      save    ! CFM: August 26, 1999 (from g77 indicators)

c     begin execution

      if(first)then
        first = .false.

        if(cvolu_opt(6,8).eq.'CTRK')then        
           itckov = 1   ! CFM: might as well turn it on here
        else
           itckov = 0   ! Not using PbGl photon generation
        endif  ! check status of PbGl photon generation

        igauto_save = IGAUTO
        write(LOUT,999)idebug,do_jxyz
999     format('gustep - first entry: idebug = ',i4,
     1      '  do_jxyz = ',i4)

c     new CFM insertions for killing low momentum charged particles

        if(rpvolu_opt(4,1).gt.0.0.or.rpvolu_opt(5,1).gt.0.0.or.
     1     rpvolu_opt(6,1).gt.0.0)then
           rcut = rpvolu_opt(4,1)
           if(rcut.eq.0.0)rcut = 9999.0    ! safety
           zcut = rpvolu_opt(5,1)
           if(zcut.eq.0.0)zcut = 9999.0    ! safety
           pcut = rpvolu_opt(6,1)
           gstcut = .true.
           write(LOUT,998)rcut,zcut,pcut
998     format(1h ,' GUSTEP kill  rcut ',f9.3,'  zcut ',f9.3,
     1              '  pcut ',f9.3,/)
        else
           gstcut = .false.
        endif   ! end of check for GUSTEP kill variables
      ENDIF  ! first time call check and initialization

c     position variables used for stopping the tracking

      rtest = sqrt(vect(1)*vect(1) + vect(2)*vect(2))
      ztest =  abs(vect(3))
      phitest = atan(vect(2)/abs(vect(1)))*57.29578
      if(iswit(5).ge.1)then

         if(rtest.ge.240..and.rtest.le.480.and.IPART.eq.1) istop=1

         if(phitest.le.-33.0.and.rtest.ge.200.and.IPART.eq.1.and.
     +      rtest.le.480) istop=1

         if(phitest.ge.56.0.and.rtest.ge.200.and.IPART.eq.1.and.
     +      rtest.le.480) istop=1

         if(ztest.ge.0.35*rtest+15.and.ipart.eq.1) istop = 1

         if(istop.eq.1.and.iswit(5).eq.2)then
           write(6,1024)rtest, ztest, phitest
1024       format('gustep - photon tracking stopped -',
     +            'rtest = ', f8.3,',  ztest = ', f8.3,
     +            ',  phitest = ', f8.3,/) 
         endif  ! debug printout of when iswit(5) = 2
      endif ! checks for unconverted photon

      if(numed.ne.810) igauto = igauto_save
 
      if(cvolu_opt(6,8).eq.'CTRK'.and.ITCKOV.ne.1)then 
         write(6,995)itckov
 995     format('gustep - ITCKOV=', i12,/,
     +           '  should be set at 1')
         stop ' PISA is stopping'
      endif

      IF (DO_JXYZ.GT.0)THEN
        CALL GSXYZ
        IF (IDEBUG.NE.0)CALL GPCXYZ
      ENDIF

      !Insertions for Muon Arm forced acceptance
      if(iswit(7).eq.8) then
         call muonArmForcedAccept
      endif                     
      
      ! Insertions for Muon Arm staged output
      if(NSTAGE.gt.0) then
         call muonArmStageOutput(1)
      endif                    
      
      ! Writing out particles in Muon Arm stage
      if(NSTAGESTACK.gt.0) then
         stackStageTest = 1
         call muonArmStackStage(stackStageTest)
         if(stackStageTest.eq.0)then
          ! particle was killed, no further checking is needed
          return  
        endif
      endif

      if(gstcut)then
         if(rtest.gt.rcut.or.ztest.gt.zcut)istop = 1

c    Study of curl-up shows looping into DC region for R > 135.0 cm
         if(rtest.lt.135.0.and.ipart.lt.10.and.charge.ne.0.0.and.
     +      (rtest.gt.30.0.or.ztest.gt.40.0))then

c    Momentum curl-up (full field) ~ .090 - 0.0005*r (from R = 30 to R = 135)
c    Also test of 44 HIJING events with no cut shows no e+,e- with momentum
c    smaller than that given by this equation at the given radius r at PC1
            ptest = pcut*(.090 - 0.0005*rtest)
            if(iswit(6).eq.0)then
               if(vect(7).lt.ptest)istop = 1
            else
               if(vect(7)*sqrt(vect(4)*vect(4)+vect(5)*vect(5)).lt.
     +            ptest)istop = 1
            endif  ! check on ISWIT(6) = 0
         endif  !  check on R<200 and a charged particle
         if(idebug.ne.0.and.NSTAGESTACK.eq.0)then
            write(LOUT,997)rtest,ztest,vect(1),vect(2),vect(3),istop,
     1                  ipart,ptest,vect(7)
997         format(1h ,'r,z ',2e12.5,' v ',3e12.5, ' i',i2,
     1             /,1x,' ipart ',i3,'  ptest ',e12.5,'  ptot ',e12.5)
         endif
      endif   ! check if GUSTEP kill is enabled
      
      ! saving it in safe place
      ITRASAV = ITRA
      ! do not write user words to KINE (default)
      NUBUF = 0
      
c   CFM: May 9, 1997, store all ancestry information; previously only
c                     selected electron and muon ancestry was stored
c                     All the "weeding" software has been deleted.
      if(ngkine.gt.0)then

         !Store all parent track information
         ubuf(1) = itrasav ! parent track
         DO I = 1,NGKINE
            nubuf = 1
            IFLGK(I) = 1
            IF(GKIN(5,I) .EQ. 2 .OR. GKIN(5,I) .EQ. 3)THEN

C  If the secondary is an e+/e- we have a special measure to tag it so that
C  later we can know whether it is a Dalitz(1)/Pair prod(2)/Deltaray(3)/
C  compton or "other"(4). Warning: due to the crazy way the GSKING/GSSTAK is
C  written, all tracks stored by GSKING(0) will have the same UBUF info. (I do
C not expect dalitz's and deltas/ pair prod. to occur simultaneously though.)
C  Alternative is to loop through GSKING(I),I=1,NGKINE but then the tracks are
C  listed as having different production vertices in KINE! SRT.
               ELECTYP = ELECTAG(I)
               NUBUF = 2
               UBUF(2) = ELECTYP
              
c Tag the secondary mu+/mu- type
c mutype =  10  ; Unknown muon
c        =  11  ; Decay muon from vector mesons
c        =  12  ; Decay muon from other primary particles
c        =  13  ; Decay muon from secondaries
c        =  14  ; Shower muons
 
            else if(gkin(5,i) .eq. 5 .or. gkin(5,i) .eq. 6)then
               call mutager(mutype)
               nubuf = 2
               ubuf(2) = mutype 
            end if  ! checks on electron or muon
         end do  ! loop over secondary particle count
         call gsking(0)
      endif ! check if any secondary particles

      if (ngphot.gt.0) then  ! if cerenkov photons were produced, put then on the stack
c        write(6,*)'quick and dirty test'
         call gskpho(0)
      endif                  ! cerenkov photons were produced


      ! Logic to call separte detector gustep routines
      !  Silicon Vertex Detector
      IF(idtype.eq.2001)THEN
        CALL SVX_GUSTEP        
        return

      ! AER detector
      else if(idtype.eq.2014)then
        call AER_GUSTEP
        return
        
      ! TOF-West detector
      else if(idtype.eq.2012)then
        call TFW_GUSTEP
        return
        
      !HBD detector (WIS version)
      else if(idtype.eq.2015)then
        call HBD_GUSTEP
        return
        
      !TPC detector
      else if(idtype.eq.2017)then
        write(*,*) 'gustep - TPC removed from simulations' 
        return
      
      !NTC detector
      else if(idtype.eq.2016)then
        write(*,*) 'gustep - NTC removed from simulations' 
        return
        
      !RXN detector
      else if(idtype.eq.2018)then
        call RXN_GUSTEP
        return

      ! FCL detector
      else if(idtype.eq.2019)then
         call FCL_GUSTEP
         return
        
      ! NCC detector
      ! Now MXPS - JGL 3/26/2011
      else if((idtype.eq.20221).or.(idtype.eq.20222)) then 
         call mxps_gustep
         return   

      ! MPC-EX absorber hits - JGL 8/19/2015
      else if((idtype.eq.20223).or.(idtype.eq.20224).or.(idtype.eq.20225)) then 
         call mxps_gustep_abs
         return   
      
      !MPC detector
      else if(idtype.eq.2023) then 
         CALL MPC_GUSTEP
         return

      !Inserted by Y.AKIBA for Cerenkov CRK
      else if (nlevel.ge.3+ieastwest .and.
     +         (names(3+ieastwest).eq.csha).or.
     +         (names(3+ieastwest).eq.cshb)) then
         call crk_gustep()

c     change from intr to numed check for dc/pc1
c     east-west volume fix, april 30, 2000 (c.f. maguire)
      else if(numed.eq.1001 .or. numed.eq.1002 .or. numed.eq.1003) then
         call muonpad_stp  
         return
           
c...  call to rlt_stp if in the rpc1-3 material
      else if(numed.eq.4301.or. 
     +        numed.eq.4302.or. 
     +        numed.eq.4303) then
         call rlt_stp
         return
      else if(numed.eq.480.or.
     +        numed.eq.481.or.
     +        numed.eq.461.or.
     +        numed.eq.494)then
         call itrstp  !  don't process any further since the particle is known to be in dch/pc1
         return                 ! (insert by cfm 2/5/93; no need to process further??)
      else if(numed.eq.671.or.numed.eq.673.or.
     1        numed.eq.694.or.numed.eq.699)then
         call padstp
         return   ! don't process any further since the particle is known to be in pc2/pc3
      else if(numed.eq.700.or.numed.eq.710.or.
     1        numed.eq.720)then
         if(charge.ne.0.0)then
            call tofstp
         endif    ! do call to tofstp only for charged particles, don't store neutral hits
         return   ! don't process any further since the particle is known to be in tof
      else if(nlevel.ge.3 .and. names(3) .eq. sect) then
        
c   should really check on a detector type instead ? (cfm 5/25/93)
c   inserted by cfmaguire for trd (10/28/93)
         call trdstp            ! cfm: july 19, 1994 name change from trdstep
         return  ! don't process any further since the particle is known to be in tec
      end if
 

c     check on whether or not to store hits in sensitive detectors
      S_NAME = '?'     ! SAFETY RESET
      IF(ISVOL.EQ.1.AND.ISET.GT.0)then     ! sensitive so MAYBE store hits
      
c       debugger shows that for ISVOL = 1  ISET is sometimes 0 ??
c       means a sensitive material in a non-detector ??
        IUSET = IQ(JSET + ISET)            ! find set name
        JS = LQ(JSET-ISET)                 ! pointer to set parameter
        IUDET = IQ(JS + IDET)              ! find detector name
        LOGWRN = .TRUE.                    ! default as error
        DO KSET = 1,PHNX_DVOL
           IF(IVOL(KSET).EQ.IUSET)THEN
              LOGWRN = .FALSE.                       ! reset warning flag

C     USE STANDARD VOLUME OPTION POSITION #3 TO TAG AS A NO STORE HITS

              IF(CVOLU_OPT(3,KSET).EQ.'NONE')RETURN  ! immediate return
              GO TO 10
           ENDIF   ! CHECK ON FOUND THE DETECTOR SET
        ENDDO   ! LOOP OVER NUMBER OF PHENIX DETECTOR SETS
        IF(IWARN.LE.PHNX_DVOL.AND.LOGWRN)THEN
           IWARN = IWARN + 1                ! ALLOW A MAXIMUM OF 12 MESSAGES
           WRITE(*, '(/,A,A,A,I5,/)' )
     1              'gustep - DETECTOR SET  ', S_NAME,
     2              '  IS NOT RECOGNIZED;  SET # ', JSET
         ENDIF   ! CHECK ON ISSUE A WARNING MESSAGE
      ENDIF  ! CHECK ON SENSITIVE VOLUME
10    CONTINUE  ! BRANCH OUT OF SENSITIVE VOLUME CHECK
 

c   Possible prototype method for other detector sets (CFM: June 13, 1993)

      IF(ISVOL.EQ.1.AND.ISET.GT.0.AND.CHARGE.NE.0.0)THEN
         IF(S_NAME.EQ.CVOL(2))THEN       ! S_NAME ESTABLISHED ABOVE
            CALL BBCSTEP                 ! SPECIAL GUSTEP FOR BBC
            ITRA = ITRASAV               ! RESTORE TRACK NUMBER
            RETURN

c     Could have ELSE IF for other detector sets here

         ENDIF   ! CHECK ON S_NAME OF DETECTOR SET
      ENDIF  ! CHECK ON SENSITIVE DETECTOR, CHARGED PARTICLE, ISET > 0
c <<
 

c    Now contine for all other sensitive detectors in previous fashion

 
c     fill HIT array for sensitive volumes
CTON SRTonse 28-APR-1992 Will try having 3 types of detectors.
C IDTYPE=0
C will  be "tracker type" where we get coordinates, momenta, dedx deposited in
C  that step and integrated TOF. I think VER, PAD, ITR, TRD, TOF fall into this
C category. Hit info will be
C written to HITS bank every step for charged particles for INWVOL=0 or
C  2. Also provided ISTOP .ne. 1. For the case of ISTOP .eq. 2 i.e. the
C  particle falls below the threshold of 1 KeV I have fudged and compensated by
C  adding 0.5 KeV to the Delta E. Finally Delta E will have to be added
C  up in the DIGI code for those who want it integrated.
C  IDTYPE = 1 are calorimters-type detectors where if we stored every hit
C   from every secondary at every step might run into space problems. For these
C  integrate the delta E using GSCHIT.
C  IDTYPE = 2 is for Cerenkovs. For geantinos (cerenkov photons) want particle
C  have its parameters recorded and then to disappear. Other particles should
C  not be artificially destroyed as they may affect something else later.
C  IDTYPE=3 is silicon vertex.
C  IDTYPE=4 is for Intermediate tracker, where entry & exit points at drift and
C  pad chambers are recorded.

      IF(ISVOL.eq.1) then        ! sensitive so store hits

c***************************************************************************

c     cfm: This is what happens when GUSTEP gets detector specific

c-JHK
c         For the silicon vertex or the intemediate detectors:

          If (idtype.eq.3.or.idtype.eq.4) then

c     immediately eliminate neutral tracks

           if(charge.eq.0.0)then
         ITRA = ITRASAV    ! restore the track number
         return
           endif
             IF (INWVOL.eq.0.and.ISTOP.eq.0) THEN ! same volume, not stopping

c     no change in volume yet so just update the energy loss

                 hit_all(4) = hit_all(4) + DESTEP ! only update dele

c     same volume (neutral particle or charge particle) now returns

          ITRA = ITRASAV      ! restore the track number
                 return    ! nothing else to do

c     Either have stopped or exited the previous volume (or its mother),
c        so output the hit vector

             else if (INWVOL.eq.2.or.ISTOP.ne.0) then

c     INWVOL = 2 means exiting the volume; check the pair hit logic

         if(.not.login.or.logout)then

c     have an exiting hit without a corresponding entering hit ?

c            print *, 'IDTYPE = (3 VERTEX, 4 INTERMEDIATE) ',
c     1               idtype
c            stop ' Exiting VERT/INTR without entrance hit ?'
         endif
                hit_all(4) = hit_all(4) + DESTEP ! add last amount of eloss
c               write(6,877)nstep, tofg
877             format(/,' GUSTEP: nstep = ',i6,' tofg = ', e12.4,/) 
                hit_all(5) = TOFG*1.E9    ! time of flight
                hit_all(6) = IPART        ! particle ID
                hit_all(1) = VECT(1)      ! POS_X (high resolution)
                hit_all(2) = VECT(2)      ! POS_Y (high resolution)
                hit_all(3) = VECT(3)      ! POS_Z (high resolution)
                hit_all(7) = VECT(4)*VECT(7) ! PMO_X (standard resolution)
                hit_all(8) = VECT(5)*VECT(7) ! PMO_Y (standard resolution)
                hit_all(9) = VECT(6)*VECT(7) ! PMO_Z (standard resolution)
                hit_all(10) = SLENG   ! set path length to back of detector
                hit_all(11) = GETOT    ! TOTAL ENERGY

c     now store the exit hit

               call gsahit(ISET,IDET,ITRA,NUMBV,hit_all,ihit)
         if(idebug.ne.0)then
                  write(LOUT,992)iset,idet,itra,numbv(1),ihit,
     1         1.e+6*hit_all(4)
992   format(1x,'GUSTEP #2: iset,idet,itra,numbv(1),ihit:',5i4,2x,
     1      'DELE ',e13.5)
         endif
                if(ihit.eq.0)then
            print *, 'IDTYPE = (3 VERTEX, 4 INTERMEDIATE) ',
     1               idtype
             write(*,*)' <E> GUSTEP: HIT NOT STORED VERT/INTR',
     1         ' (INWVOL=2)'
                endif            ! check if hit stored
         logout=.true.     ! safety check reset for next entrance
         login=.false.     ! safety check reset for next entrance

C-->  Transform to local coords (test printout):

                CALL GMTOD( VECT, X_daughter, 1 )
         if(idebug.ne.0)then
            write(LOUT,777)vect(1),vect(2),vect(3),
     1         x_daughter(1),x_daughter(2),x_daughter(3)
777   format(2x,'X_M: ',3e13.5,'   777',/,2x,'X_D: ',3e13.5)
         endif
         ITRA = ITRASAV    ! restore the track number
                return
             else

c     by elimination this corresponds to INWVOL = 1 and ISTOP = 0

              if(login.or..not.logout)then
c            print *, 'IDTYPE = (3 VERTEX, 4 INTERMEDIATE) ',
c     1               idtype
c            stop ' GUSTEP <E> Two consec. entr. hits VERT/INTER'
         endif ! end INWVOL = 1 pair logic
                hit_all(4) = 0.0      ! initialize   dele
                hit_all(5) = TOFG*1.E9! + tof   ! time of flight
                hit_all(6) = IPART     ! particle ID
                hit_all(1) = VECT(1)      ! POS_X (high resolution)
                hit_all(2) = VECT(2)      ! POS_Y (high resolution)
                hit_all(3) = VECT(3)      ! POS_Z (high resolution)
                hit_all(7) = VECT(4)*VECT(7) ! PMO_X (standard resolution)
                hit_all(8) = VECT(5)*VECT(7) ! PMO_Y (standard resolution)
                hit_all(9) = VECT(6)*VECT(7) ! PMO_Z (standard resolution)
                hit_all(10) = SLENG   ! set path length to front of detector
                hit_all(11) = GETOT    ! TOTAL ENERGY

c     now store the entrance hit

                call gsahit(ISET,IDET,ITRA,NUMBV,hit_all,ihit)
         if(idebug.ne.0)then
                  write(LOUT,993)iset,idet,itra,numbv(1),ihit,
     1         1.e+6*hit_all(4)
993   format(1x,'GUSTEP #1: iset,idet,itra,numbv(1),ihit:',5i4,2x,
     1      'DELE ',e13.5)
         endif
                if(ihit.eq.0)then
            print *, 'IDTYPE = (3 VERTEX, 4 INTERMEDIATE) ',
     1               idtype
             write(*,*)' <E> GUSTEP: HIT NOT STORED VER',
     1         ' (INWVOL=1)'
                endif            ! check if hit stored
         logout=.false.       ! set up entrance safety logic
         login=.true.         ! set up entrance safety logic

C-->  Transform to local coords (test printout):

                CALL GMTOD( VECT, X_daughter, 1 )
         if(idebug.ne.0)then
            write(LOUT,778)vect(1),vect(2),vect(3),
     1         x_daughter(1),x_daughter(2),x_daughter(3)
778   format(2x,'X_M: ',3e13.5,'   778',/,2x,'X_D: ',3e13.5)
         endif
           endif           ! check on INWVOL and/or ISTOP
           ITRA = ITRASAV     ! restore the track number
             RETURN           ! return for INWVOL = 1
          endif               ! check on IDTYPE = 3
c-JHK
c***************************************************************************

c     Not the Vertex/Intr detector, so proceed with other logic


        if(cvolu_opt(6,8).eq.'CTRK')then

          if(numed.eq.810)then
            if(INWVOL.EQ.1)then   ! Entering PbGl volume

cMM   leave the step length and the maximum energy loss to GEANT
cMM   this is a MUCH better for the use of the parameterization

cMM              IGAUTO = 0          ! Disable automated stepsize
cMM              stemax = 0.1        ! Max. stepsize
cMM              deemax = 0.02       ! Max. E loss in a step
               CALL EMC_GUSTEP(1)    ! particle entering PbGl
            endif  ! check on inwvol = 1

cMM   NGPHOT is checked inside emc_gustep;
cMM   if one does it here, the saving of the detected
cMM   energies will not work properly

cMM            if(ngphot.gt.0)then
              xphot(8,1) = tofg*1.e9
               CALL EMC_GUSTEP(0)    ! particle inside PbGl
cMM            endif ! check on ngphot >0
          endif ! check if we are in lead glass (numed = 810)
        endif ! check if switch 'CTRK' is set


        IF(IDTYPE .EQ. 0 .OR.
     &     (IDTYPE .EQ. 2 .AND. IPART .NE. PP_CRK_PHOT))THEN
          IF(CHARGE .NE. 0.0 .AND. ISTOP .NE. 1
     &       .AND. (INWVOL .EQ. 0 .OR. INWVOL .EQ. 2))THEN
            hit_all(1)=VECT(1)
            hit_all(2)=VECT(2)
            hit_all(3)=VECT(3)
            hit_all(4) = DESTEP           ! energy lost in current step
            IF(ISTOP .EQ. 2)HIT_ALL(4) = HIT_ALL(4) + 0.0005
            hit_all(5) = TOFG*1.E9     ! INTEGRATED time of flight
            hit_all(6) = IPART
            hit_all(7)=VECT(4)*VECT(7)
            hit_all(8)=VECT(5)*VECT(7)
            hit_all(9)=VECT(6)*VECT(7)
            hit_all(10) = SLENG
            hit_all(11) = GETOT      ! TOTAL ENERGY

            if(cvolu_opt(6,8).ne.'CTRK'.or.numed.ne.810)then
               call gsahit(ISET,IDET,ITRA,NUMBV,hit_all,ihit)
               IF (IHIT.EQ.0)THEN
                  WRITE(*,'(1H ,A,A,5X,A)')
     &            ' GUSTEP: Hit not stored. SET, DET: ',SETNAME,DETNAME
                  WRITE(6,12345)NUMED, CVOLU_OPT(6,8)
12345             FORMAT('   NUMED = ',i5,'  EMC OPTION6 ', A)
                  WRITE(6,12346)ISET,IDET,ITRA,ISVOL
12346             FORMAT('   ISET = ',i4,' IDET = ',i4,' ITRA = ', i6,
     +                   '  ISVOL = ',i4)
               ENDIF  ! check on hit being stored
             endif  !  check on not being PbGl with Cerenkov
          END IF
        ELSE IF(IDTYPE .EQ. 1)THEN     !calorimeter type?
          IF(INWVOL .EQ. 0 .AND. ISTOP .EQ. 0)THEN !accumulate energy
            HIT_ALL(4) = HIT_ALL(4) + DESTEP
C  particle has gone below thresh. add Del E and store.
          ELSE IF(INWVOL .EQ. 0 .AND. ISTOP .EQ. 2)THEN  !accumulate energy
            HIT_ALL(4) = HIT_ALL(4) + DESTEP + 0.0005
            HIT_ALL(10) = SLENG - HIT_ALL(10)  ! path length in detector
              hit_all(11) = GETOT      ! TOTAL ENERGY
            call gsahit(ISET,IDET,ITRA,NUMBV,hit_all,ihit)
            if (ihit.eq.0) write(*,*)' GUSTEP: HIT NOT STORED'
C  particle leaving volume or has been destroyed
          ELSE IF(INWVOL .EQ. 2 .OR. (INWVOL .EQ. 0 .AND.
     &       ISTOP .EQ. 1))THEN
            HIT_ALL(4) = HIT_ALL(4) + DESTEP
            HIT_ALL(10) = SLENG - HIT_ALL(10)  ! path length in detector
              hit_all(11) = GETOT      ! TOTAL ENERGY
            call gsahit(ISET,IDET,ITRA,NUMBV,hit_all,ihit)
            if (ihit.eq.0) write(*,*)' GUSTEP: HIT NOT STORED'
C  entering the volume. Init and store position & momentum at entry point.
          ELSE IF(INWVOL .EQ. 1 .AND. ISTOP .EQ. 0)THEN
            hit_all(1)=VECT(1)
            hit_all(2)=VECT(2)
            hit_all(3)=VECT(3)
            hit_all(4) = 0.0
            hit_all(5) = TOFG*1.E9     ! INTEGRATED time of flight
            hit_all(6) = IPART
            hit_all(7)=VECT(4)*VECT(7)
            hit_all(8)=VECT(5)*VECT(7)
            hit_all(9)=VECT(6)*VECT(7)
            hit_all(10) = SLENG   ! set to path length upto front of det.
            hit_all(11) = GETOT   ! TOTAL ENERGY  at entry point
          END IF
        ELSE IF(IDTYPE .EQ. 2 .AND. IPART .EQ. PP_CRK_PHOT)THEN
C  specially for Cerenkov's DET's like Akiba's CPHO. : want particle to be
C  recorded and vanish as soon as
C  it hits the sensitive detector. But only if it is a geantino.(?)

           if(INWVOL.eq.1) then

c  Changed October 7, 1996 from Kenta Shigaki

              hit_all(1)=VECT(1)
              hit_all(2)=VECT(2)
              hit_all(3)=VECT(3)
              HIT_ALL(4)  = GEKIN ! Dump all kinetic energy
              hit_all(5) = TOFG*1.E9 ! INTEGRATED time of flight
              hit_all(6) = IPART
              hit_all(7)=VECT(4)*VECT(7)
              hit_all(8)=VECT(5)*VECT(7)
              hit_all(9)=VECT(6)*VECT(7)
              hit_all(10) = SLENG ! path length
              hit_all(11) = GETOT ! TOTAL ENERGY
              call gsahit(ISET,IDET,ITRA,NUMBV,hit_all,ihit)
              if (ihit.eq.0) write(*,*)' GUSTEP: HIT NOT STORED'
           endif
        ELSE IF(IDTYPE .EQ. 4 .AND. INTR_STORE)THEN
C   store hits from drift/pad chambers of intermediate tracker.  SRT.
          CALL GSAHIT(ISET,IDET,ITRA,NUMBV,HIT_ALL,IHIT)
          INTR_STORE = .FALSE.
          if (ihit.eq.0) write(*,*)' GUSTEP: HIT NOT STORED'
        ELSE IF(IDTYPE .EQ. 5 .AND. (TRD_STORE(1)
     &     .OR. TRD_STORE(2)))THEN
C   store hits from TRD/TRACKER SRT.
          CALL GSAHIT(ISET,IDET,ITRA,NUMBV,HIT_ALL,IHIT)
          if (ihit.eq.0) write(*,*)' GUSTEP: TRD HIT NOT STORED'
          TRD_STORE(1) = .FALSE.
        ENDIF
      END IF               !ISVOL = 1?
 
c >> Store Muon_arm Hits
c Muon_arm --> IDTYPE = 7

c  CFM: Previous version also checked on (cvolu_opt(1,10).ne.'   ') but this
c       would always be satisfied even if no MUM subsystem was installed.

      if(idtype .eq. 7)then
         call mua_gustep
      end if


      if(idtype.eq.8)then
         call zdc_gustep  ! check if inside the ZDC volume
      endif  ! check on ZDC
 
C all HIT bank filling should be done by now. Restore ITRA
      ITRA = ITRASAV

c     Particle track display at each step of the track if LGEOM(7) = 1

      if(lgeom(7).eq.1)then
         call gdcxyz(itra)
      endif  ! check for particle display switch      

      RETURN
      END
