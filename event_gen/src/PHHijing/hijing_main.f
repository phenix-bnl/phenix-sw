C      ============================================================
      Program Hijing_C

      implicit none
c
c     Interface to HIJING program
c
c     Original Author: Li Qun, May 1996
c
c     Function: begins HIJING program, calls control input, sets up ZEBRA
c
c     Calls: HIJING_INIT, MZEBRA, SETUP_FOR_ZEBRA, EGZINIT, HIJSET, HIJING_MAIN,
c            WRITE_EVENT_BANK, EGZEND, HIJINGPISA
c
c     Qualifications: Requires hijing.par namelist control file
c
c     Revision History
c
c     External block data names
c
      External hidata, ludata, pydata
c
c     Global variables
c

      include 'hijingpar.inc'
      include 'himain.inc'

      include 'evntcode.inc'

      external write_code_bank
      external write_run_bank
      external ranf

      integer*4 egzinit
      integer*4 write_event_bank
      integer*4 egzend

c
c      Local Declarations
c      ------------------
c
      integer iev, i, kk
      integer istat
      integer lhilis      ! temporary for egzend
      real atarg, aproj, ztarg, zproj

      real time1           ! CPU time to first call
      real time2           ! present CPU time

      real rcall
      integer skip_iseed

      real hipr1, hint1
      integer ihpr2, ihnt2
      integer nfp, pp, nft, pt
      integer Ntot
      real yp, yt

      integer ipcol, itcol

      COMMON/HIJCRDN/YP(3,300),YT(3,300), IPCOL(90000), ITCOL(90000)
      COMMON/HIPARNT/HIPR1(100),IHPR2(50),HINT1(100),IHNT2(50)
      common/histrng/nfp(300,15),pp(300,15),nft(300,15),pt(300,15)
      common /stat_ran/rcall, skip_iseed
        

c
c      Executable code
c      ===============
C
C      Call routine to initialize HIJING and ISAJET
C

c      OPEN(unit=5,file='out.dat',status='new')
c      write(5,*) '# OSC1997A'
c      write(5,*) '# final_id_p_x'

      CALL HIJING_INIT
c
c   Setup ZEBRA
c
      call MZEBRA(0)
c
c   Setup for ZEBRA code bank.  Cannot use common here or variables would
c   be multiply defined.
c
      atarg = iat
      aproj = iap
      ztarg = izt
      zproj = izp
      call setup_for_zebra(.false.,hijing,1.37,atarg,ztarg,
     1              aproj,zproj,efrm,0.0,bmax0)


      istat = egzinit()      ! setup to write events to tape
      if (istat.ne.-1) then
        nhievt = 0      ! force program exit
        write(6,*)'  ERROR on return from EGZINIT'
      end if
c     
c     start CPU time
c
      call timex(time1)
c
c     Call to initialize HIJING
c

      CALL HIJSET(EFRM,FRAME,HPROJ,HTARG,IAP,IZP,IAT,IZT)
C
C     Loop over HIJING events
C
      DO IEV=1,NHIEVT
         if(iev.eq.1)then
            write(6,1)time1
 1          format(/,' HIJING_C <I>: Begin event runs; ',
     +               'CPU Elapsed time = ',f8.2,' seconds',/)
         else
            call timex(time2)
            write(6,2)iev-1,(time2-time1)/float(iev-1),
     +                natt, naccepted, bimpact
 2          format(/,' HIJING_C <I>: Finished event #',i5,
     +               '; CPU time/event = ',f8.2,' seconds',/,
     +               '               Number of particles =',i6,
     +               ' (in Theta cut = ',i6,')',
     +               '; impact parameter =',f6.2,' fm',/)
            write(*,*) "The number of binary collisions is: ", nbinary
            
          endif                 ! check on first event


c
c     Call Hijing event generator
c
cscj         call HIJING_GEN(FRAME,BMIN0,BMAX0,BIMPACT)
         call HIJING_GEN(FRAME,BMIN0,BMAX0,BIMPACT)

c
c   Transfer the HIJING event information to the EVENT.INC common block
c
c
c SCJ  Calculation of the number of binary collisions.
c

c Loop over the number of nucleons in the 'projectile'
      nbinary = 0
      do i=1,iap
        nbinary = nbinary + nfp(i,11)
      enddo

      call hijingpisa

c
c  Write out event-by-event particle information
c

        istat = write_event_bank(hijing)      ! write out event via ZEBRA
         if (istat.ne.-1) then
            write(6,*)'  ERROR on return from WRITE_EVENT_BANK'
         end if




c     WRITE TO AN OSCAR FORMATTED FILE


c         Ntot=Natt

c         if (aproj.eq.1) then
c           write(5,*)  iev, Ntot, HINT1(19), HINT1(20), 
c     +          N0, N01, N10, N11, NP, NT, NFP(1,11), -1
c         else
c           write(5,*)  iev, Ntot, HINT1(19), HINT1(20), 
c     +          N0, N01, N10, N11, NP, NT, NFP(1,11), NFP(2,11)
c         endif

cscj         write(9,*) YP(1,1), YP(2,1), YP(3,1), YP(1,2), YP(2,2), YP(3,2)

c     N0=  # N-N COLLISIONS
c     N01= # N-Nwound COLLISIONS
c     N10= # Nwound-N COLLISIONS
c     N11= # Nwound-Nwound COLLISIONS
c     NP = Number of projectile participants
c     NT = Number of target participants
c     nfp(1,11) = number of interactions of projectile nucleon 1
c     nfp(2,11) = number of interactions of projectile nucleon 2

c         do i=1,Ntot
c           WRITE(5,*) i, KATT(I,1),(PATT(i,KK),KK=1,4), ' 0 0 0 0 0'
c         end do
c         WRITE(5,*) '0 0'
       enddo                    ! loop over requested events
       
       call timex(time2)
      write(6,2)nhievt,(time2-time1)/float(iev-1),natt,
     +          naccepted,bimpact
      write(6,3)nhievt,time2-time1
 3    format(//,' HIJING_C <I>: For all',i5,' events total CPU time',
     +          ' was ',f8.2,' seconds')

      write(6,4)rcall
 4    format(5x,'There were ',f11.0,' calls to the random ',
     +          'number generator.',/)
      istat = egzend(lhilis)      ! Finish up ZEBRA
      CLOSE(16)

c        close(5)
      STOP
      END
c
c--------------------------------------------------------------------
C      ============================================================
      SUBROUTINE HIJING_INIT
C      ============================================================
c
c      UNIX Version of code
C
C      This routine initializes HIJING and ISAJET. HI and ISAJET
C      parameters are read in. HI flags are read in. Various reference
C      frames are calculated. Several constants are passed on to common.
C
C     Revision History:
C     C.F. Maguire    March 4, 1999      Add jet production as per
C                                        HIJING document page 31
C
C      ========================================
c
      implicit none
c
C     HIJING Common
C
      include 'hijingpar.inc'

      real hipr1, hint1
      integer ihpr2, ihnt2
      COMMON/HIPARNT/HIPR1(100),IHPR2(50),HINT1(100),IHNT2(50)
c
c     Random number generator common  (taken from RQMD)
c
      integer ix, iy
      common /ranfbk/ix,iy  

      real rcall
      integer skip_iseed
      common /stat_ran/rcall, skip_iseed
                                          

C
c      Local Declarations
c      ------------------
c

      character*4 chproj
      character*4 chtarg
      character*4 ref
      integer nruns
      integer n1
      integer n2
      integer iz1
      integer iz2
      integer iseed /1/ 
      integer iseed_skip /0/
      integer jet_trigger /0/
      integer jet_quench /1/
      real loss_factor /1.0/
      real pthard /0.0/
      real elgev
      real bmin
      real bmax
      real pthetamin /0.0/
      real pthetamax /180.0/

      namelist /hijing_inp/ chproj, chtarg, nruns, elgev,
     +                  ref, n1, iz1, n2, iz2, bmin, bmax,
     +                  iseed, iseed_skip, jet_trigger,
     +                  pthard, jet_quench, loss_factor,
     +                  pthetamin, pthetamax

c
      OPEN(3,FILE='hijing.par', STATUS='OLD',FORM='FORMATTED',
     1     err=97)
c
c      READ(3,*) NHIEVT,NHIDMP
c      READ(3,*) EFRM,FRAME,HPROJ,HTARG,IAP,IZP,IAT,IZT
c      READ(3,*) hj_outfile
c
      read(3,nml=hijing_inp,err=99)
      close(3)
c
c     transfer information to hijingpar common block
c     (namelist variables cannot be in a common block)
c
      nhievt = nruns
      efrm = elgev
      htarg = chtarg
      hproj = chproj
      frame = ref
      iap = n1
      izp = iz1
      iat = n2
      izt = iz2
      bmin0 = bmin
      bmax0 = bmax
      thetamin = pthetamin
      thetamax = pthetamax

      ihpr2(4) = jet_quench   ! sets jet quenching
      if(ihpr2(4).eq.1)then
         write(6,1234)loss_factor
1234  format(//,'  New HIJING1.37  quenching is on, loss factor ',
     +       f8.4, ' times 1.0 GeV/fm', //)
         HIPR1(14) = loss_factor*HIPR1(14) 
      endif
      if(ihpr2(4).eq.0)then
         write(6,1235)
1235  format(//,'  New HIJING1.37  quenching is off ',//)
      endif

      if(thetamin.ne.0.0.or.thetamax.ne.180.0)then
         write(6,1236)thetamin, thetamax
 1236    format(//,' Theta limits = ', 2(f8.4,2x),//)
      endif

      if(jet_trigger.eq.1)then
c
c     Using documentation page 31
c
         ihpr2(4) = 0   ! turns off jet quenching
         ihpr2(3) = 1   ! turns on one hard scattering
         hipr1(10) = pthard ! hard process pt range
      endif
      trigger_jet = jet_trigger  ! transfer to common block HIJINGPAR for hijingpisa.f
c
c     Initialize the random number generator
c     Using GEANT random number sequences
c
      skip_iseed = iseed_skip
      call hijingseed(iseed)

      OPEN(16,FILE='hijing.lis',STATUS='UNKNOWN',
     +       FORM='FORMATTED')
C
C      Printout
C
      WRITE(16,30) nruns
30      FORMAT(' < < < < < < < <  HIJING 1.37  > > > > > > > > '//
     1     ' Nucleus - Nucleus Event Generator '//
     2     ' Number of Heavy Ion Interactions Generated = ',I6/)

      WRITE(16,40) ELGEV,FRAME,CHPROJ,N1,IZ1,CHTARG,
     +           N2,IZ2
40      FORMAT(' C.M. cms-energy Per Nucleon = ',F8.3,//
     6     ' which frame: ',A4,//,
     1     ' Projectile particle, mass, charge = ',A4,I6,I6,/,
     4     ' Target particle, mass, charge = ',A4,I6,I6,/)

      RETURN
c
c     Error branches
c
 97   continue
      stop ' Unable to open input control file hijing.par'
 99   continue
      stop ' HIJING <E>:  Error in reading namelist input file'
      END





















