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

