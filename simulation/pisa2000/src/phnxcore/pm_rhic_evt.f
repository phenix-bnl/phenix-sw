      integer function pm_rhic_evt()

      implicit none

#include "g77trigdef.inc"

c     Original author: Charles F. Maguire
c     Creation data: January 23, 1997
c     Purpose: read in external file of J/Psi particles created using
c     Pat McGaughey event generator called RHIC_ALL
      
c     Calling map: Called by GUEVGEN
c     Input variables are set in PKEVNT action routine via
c     PKINE array
      
c     Calls: GRANOR random number generator (Gaussian) from CERN

c     Global variables
#include "gckine.inc"
#include "gconst.inc"
#include "gcflag.inc"
#include "guphnx.inc"
#include "subevt.inc"
#include "guevgen.inc"
#include "cmcevt.inc"

c     Local variables
      logical logopn /.false./

      integer ieormu            ! electron or muon particles
      integer north_south       ! North only, or North and South
      real    thmin             ! Minimum theta angle
      real    thmax             ! Maximum theta angle
      real    pmin              ! Minimum momentum

      integer naccp             ! number of accepted particles
      integer ipt               ! index number
      real    xnor1, xnor2      ! used for GRANOR call
 
      real    ptot              ! total momentum (GeV/c)
      real    theta_n           ! actual particle angle
      real    theta_s           ! supplement of actual particle angle


c     Begin execution

      numevt = numevt + 1  ! increment Event number 
      pm_rhic_evt = 0   ! set for error return
      if(pkine(8).lt.0.0)then

c     initialize

         pkine(8) = 1.0
         ieormu = pkine(1)
         north_south = pkine(2)
         thmin = pkine(3)
         thmax = pkine(4)
         pmin = pkine(5)
         chevt_name = 'JPSI_EVENT from data file'
         write(6,6011)IEORMU, NORTH_SOUTH, THMIN, THMAX, PMIN
 6011 format(//,2x,'PM_RIHC Parameters ',/,
     + 2x, 'Electron (0) or Muon (1) choice = ',i2,/,
     + 2x, 'North only (0) or North and South (1) choice = ',i2,/,
     + 2x, 'Minimum angle = ',f7.2,' degrees ',/,
     + 2x, 'Maximum angle = ',f7.2,' degrees ',/,
     + 2x, 'Minimum momentum = ',f7.2,' GeV/c ')

         open(unit=17,file='dimuon.dat',status='old',
     +         access='sequential',form='formatted',
     +         err=6003)
      endif  ! end initialization


c     Read dimuon.dat ASCII data file for possible particle pair

      naccp = 0
      read(17,*,err=6005,end=6007)(pptot(2,ipt),
     +     pptot(3,ipt), pptot(4,ipt),
     +     ipt=1,ipopsub)
      do ipt=1,ipopsub
         ptot = pptot(2,ipt)*pptot(2,ipt)
     +   + pptot(3,ipt)*pptot(3,ipt) +
     +   pptot(4,ipt)*pptot(4,ipt)
         if(ptot.gt.0.0)then
            ptot = sqrt(ptot)
         endif  ! check on ptot > 0
         if(ptot.gt.pmin)then
            theta_n = acosd(pptot(4,ipt)/ptot)
            if(north_south.eq.0)then
               if(theta_n.ge.thmin.and.theta_n.le.thmax)then
                  naccp = naccp + 1
               endif  ! check on single Theta window with north_south = 0
            else
               theta_s = 180.0 - theta_n
               if((theta_n.ge.thmin.and.theta_n.le.thmax).or.
     +            (theta_s.ge.thmin.and.theta_s.le.thmax))then
                  naccp = naccp + 1
               endif  ! check on dual Theta window with north_south = 1
            endif  ! check on north_south = 0
         endif  ! check on ptot > min
         pptot(2,naccp) = pptot(2,ipt)
         pptot(3,naccp) = pptot(3,ipt)
         pptot(4,naccp) = pptot(4,ipt)
         if(ipt-2*(ipt/2).eq.0)then
            if(ieormu.eq.0)then   !come here for even ipt values
               idtot(naccp) = 3      !e-
            endif
            if(ieormu.eq.1)then
               idtot(naccp) = 6      !mu-
            endif  ! check on muon particle
            if(ieormu.eq.2)then
               idtot(naccp) = 12     !k-
            endif
         else
            if(ieormu.eq.0)then   !come here for odd ipt values
               idtot(naccp) = 2      !e+
            endif
            if(ieormu.eq.1)then
               idtot(naccp) = 5      !mu+
            endif
            if(ieormu.eq.2)then
               idtot(naccp) = 11     !k+
            endif  ! check on kaon particle
         endif  ! check on even or odd
      enddo
      mxtot=naccp
      xyz(1)=0.
      xyz(2)=0.
      xyz(3)=0.

      end_evtflg = .true.        ! simple events
      
      goto 9000
      
 6003 continue
      print 6004
 6004 format(/,3x,'Cannot open dimuon.dat file'/)
      return
 6005 continue
      print 6006
 6006 format(/,3x,'Error in reading dimuon.dat file'/)
      return
 6007 continue
      print 6008
 6008 format(/,3x,'EOF in reading dimuon.dat file'/)
      close(unit=17)
      return
 9000 continue
      
      ! signify successful return
      pm_rhic_evt = -1  
      end
