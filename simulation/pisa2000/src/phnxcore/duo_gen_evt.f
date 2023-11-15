      subroutine duo_gen_evt(ievstat)

      implicit none

#include "g77trigdef.inc"


c     Author: Charles F. Maguire, April 14, 1996

c     Purpose is to launch pairs of particles at given Theta and Azimuth
c     Particles will have correlated angular positions
c     Useful for both central arm and muon arm tracking simulations involving
c            resolution tests


c     Input is via namelist file duo.par

c     Sample duo.par namelist file for dielectron tests in central arm
c     Launches pairs of particles at fixed angles

c     $duo_par
c      duo_npairs = 1,
c      duo_partid = 2,2,
c      duo_theta  = 80.0, 80.0,
c      duo_phi    = 3.0, 9.0,
c      duo_ran_th1 = 0.0,
c      duo_ran_ph1 = 0.0,
c      duo_ptot   = 0.5, 0.5,
c      duo_xvert  = 0.0, 0.0,
c      duo_ran_x1 = 0.0,
c      duo_yvert  = 0.0, 0.0,
c      duo_ran_y1 = 0.0,
c      duo_zvert  = 0.0, 0.0,
c      duo_ran_z1 = 0.0,
c      duo_temperture = 0.0,
c      duo_pmin = 0.0;
c      duo_pmax = 0.0;
c     $end


c     Called by: GUEVGEN with 18 as number of event type
c     Invoked by: DUO_GEN_EVT command from PISA

c     Revision History:
c     Last revsion for temperature dependent spectrum

c    Comments below are part of generic event generator template which is
c    the basis of this two particle generic event generator

*****************************************************************************

c     "GENERIC" Event generator template (Muon Arm request: November, 1994)

c        !!!  NOT TO BE MODIFIED IN THE MASTER REPOSITORY BY ANY USER  !!!

c     You may edit this file in your local CVS directory to make your
c     own gen_evt event generator for your own specific purpose.

c     When you next do a cvs_upd_all for the PISA sources, you will get a
c     warning that the gen_evt.f file has been modified.  But that is what you
c     wanted to do anyway.

c     You may want to make a variety of gen_evt.f files, or else you can make
c     one complex gen_evt.f file, all designed to suit your particular needs.
c     You will probably find it useful to use an external namelist file in
c     order to give you more execution time flexibility for your "events".


c     Calling variable  IEVSTAT
c         If there are no errors, then you must return IEVSTAT = -1
c         If there are errors, then you must return IEVSTAT = 2

c     This routine is called by PKEVNT via the KUIP interface to PISA
c     At the GEANT> prompt in PISA, one typed in  GEN_EVT ipopsub
c     IPOPSUB is an integer variable giving the number of particles/subevent

c     Operation/interface to PISA:
c       All of the capitalized variables below are in the /evgen/ common.
c       The purpose of this routine is to fill the "pre-buffers" in /evgen/
c       with primary particle information.  Up to MAX_MXTOT = 20000 primaries
c       may be stored in a full event.  The actual number of particles in
c       your full event is MXTOT, and you must provide that number in this
c       subroutine !

c       You have a choice of storing all the particles with the same vertex
c       coordinates XYZ(3), or you can store a different vertex for each
c       particle.  If your particle generator provides you with a parent
c       id number, then you can also store that parent id number.  The choice
c       between single vertex and multiple vertex is made with the MULTI_XYZ
c       integer variable.  MULTI_XYZ = 0 means that you have the same vertex
c       for all the particles.  MULTI_XYZ = 1 means that you will provide a
c       different vertex for all particles.  The default is MULTI_XYZ = 0,
c       and XYZ(3) = (0,0,0) will also be set for you as a default.  There
c       NO default XYZMV array when MULTI_XYZ = 1; that is your responsibility.

c       These pre-buffer arrays are:
c          PPTOT(4, MAX_MXTOT)    etot, px, py, pz 4-momenta components
c          XYZ(3)                 vertex position (same for all particles)
c          IDTOT(MAX_MXTOT)       GEANT particle ID
c          XYZMV(3, MAX_MXTOT)    different vertex for all particles
c          ID_PARENT(MAX_MXTOT)   parent ID number for all particles

c       The XYZ array will be used if you put MULTI_XYZ = 0 (default value).
c       THE XYZMV array will be used if you put MULTI_XYZ = 1.

c       The XYZ(3) array is stored in the PISA output event header information

c       Be careful that you put the ith px momentum component in PPTOT(2,i)
c       Be careful that you put the ith py momentum component in PPTOT(3,i)
c       Be careful that you put the ith pz momentum component in PPTOT(4,i)
c       It is not necessary to fill in the pptot(1,i) element in this routine.

c       Lastly, you should provide a character string description of your
c       event.  That string, specified as a CHARACTER*40 variable CHEVT_NAME
c       exists in the /chevgen/ common block which is part of the GUEVGEN
c       sequence.

c    Revision History  (SIMULATION CORE GROUP MEMBERS ONLY !)
c       CORE MEMBER   DATE             COMMENT
c       C.F. Maguire  April 2, 1996    Cleaned out old CMZ references in header
c                                      Put in more user suggestions
c                                      Improved sample data comments

c       C.F. Maguire  June 7, 2005     Add exponential momentum dependence


*****************************************************************************

c     Global variables

      integer ievstat
#include "guevgen.inc"
#include "event.inc"

c     namelist variables for dual particle generation

      integer  duo_npairs      /1/
      integer  duo_partid(2)  /2,3/
      real     duo_theta(2 )  /80.0,80.0/
      real     duo_phi(2)     /3.0, 9.0/
      real     duo_ran_th1    /0.0/
      real     duo_ran_ph1    /0.0/
      real     duo_ptot(2)    /0.50, 0.50/
      real     duo_ran_ptot(2) /0.0, 0.0/
      real     duo_xvert(2)   /0.0, 0.0/
      real     duo_ran_x1     /0.0/
      real     duo_yvert(2)   /0.0, 0.0/
      real     duo_ran_y1     /0.0/
      real     duo_zvert(2)   /0.0, 0.0/
      real     duo_ran_z1     /0.0/

c     Temperature dependent exponential

      real     duo_temperature /0.0/
      real     duo_pmin        /0.0/
      real     duo_pmax        /0.0/

      namelist /duo_par/duo_partid, duo_theta, duo_phi, duo_ptot,
     +                  duo_zvert, duo_npairs, duo_ran_th1,
     +                  duo_ran_ph1, duo_ran_z1, 
     +                  duo_yvert, duo_ran_y1,
     +                  duo_xvert, duo_ran_x1, duo_ran_ptot,
     +                  duo_temperature, duo_pmin, duo_pmax

      integer  ifirst /0/

      real rndm


c     Local variables (simple sample)


      integer ipair   ! do loop index over pairs
      integer kpart   ! particle counter
      real theta_deg  ! polar angle
      real phi_deg    ! azimuthal angle
      real pptotal    ! total momentum

      real delt_th    ! initial delta theta
      real delt_ph    ! initial delta phi
      real delt_x     ! initial delta x
      real delt_y     ! initial delta y
      real delt_z     ! initial delta z
      save delt_th, delt_ph, delt_z, delt_x, delt_y

      logical logth1  ! logical for randomizing Theta1
      logical logph1  ! logical for randomizing Phi1
      logical logx1   ! logical for randomizing X1
      logical logy1   ! logical for randomizing Y1
      logical logz1   ! logical for randomizing Z1
      logical logp1   ! logical for randomizing ptot first particle
      logical logp2   ! logical for randomizing ptot second particle

      logical logTemperature /.false./
      real    normFactor /0.0/   ! normalization of exponential between two limits
      real    expFactor1 /0.0/   ! exp(-duo_pmin/duo_temperature)
      real    testArgument

      save logth1, logph1, logz1, logx1, logy1, logp1, logp2

c     begin execution (instructions below repeat what was said above)

      numevt = numevt + 1 ! do not change this statement

c     You must fill in the PPTOT, XYZ (or XYZMV), and IDTOT arrays with
c     MXTOT <= 20000 primary particles.

c     You must provide a 40 character or less event descriptor CHEVT_NAME.

c     You must return IEVSTAT = -1 (normal) or IEVSTAT = +2 (abnormal)



c     multiple vertex positions

      multi_xyz = 1      ! key variable used by GUKINE (1 is multiple vertex)

      if(ifirst.eq.0)then
         ifirst = 1
         open(unit=29,file='duo.par',form='formatted',
     +        status='old',access='sequential',err=99)
         read(29,nml=duo_par,err=97,end=95)
         close(29)
         write(6,nml=duo_par)
         delt_th = duo_theta(2) - duo_theta(1)
         delt_ph = duo_phi(2) - duo_phi(1)
         delt_x = duo_xvert(2) - duo_xvert(1)
         delt_y = duo_yvert(2) - duo_yvert(1)
         delt_z = duo_zvert(2) - duo_zvert(1)
         if(duo_ran_th1.gt.0.0)then
            logth1 = .true.
         else
            logth1 = .false.
         endif
         if(duo_ran_ph1.gt.0.0)then
            logph1 = .true.
         else
            logph1 = .false.
         endif
         if(duo_ran_x1.gt.0.0)then
            logx1 = .true.
         else
            logx1 = .false.
         endif
         if(duo_ran_y1.gt.0.0)then
            logy1 = .true.
         else
            logy1 = .false.
         endif
         if(duo_ran_z1.gt.0.0)then
            logz1 = .true.
         else
            logz1 = .false.
         endif
         if(duo_ran_ptot(1).gt.0.0)then
            logp1 = .true.
         else
            logp1 = .false.
         endif
        if(duo_ran_ptot(2).gt.0.0)then
            logp2 = .true.
         else
            logp2 = .false.
         endif
         if(duo_temperature.gt.0.0)then
            logTemperature = .true.
            normFactor = exp(-duo_pmax/duo_Temperature) -
     +           exp(-duo_pmin/duo_Temperature)
            if(normFactor.ge.0.0)then
              stop ' Temperature normalization error'
           endif  ! check on valid normalization factor
           expFactor1 = exp(-duo_pmin/duo_temperature)
         endif ! check on temperature dependent exponential
      endif  ! initialization check


c     Event-by-event calls


      mxtot = 2*duo_npairs     ! number of primary particles in this event

c     use same vertex position for all particles in the same event

      multi_xyz = 0      ! key variable used by GUKINE (0 is same vertex)
      ipair = 1
      if(logx1)then
         xyz(1) = 2.*(rndm(ipair) - 0.5)*duo_ran_x1 +
     +            duo_xvert(1) ! cm
      else
         xyz(1) = duo_xvert(1) ! cm
      endif                           ! check on X1 randomixation
      if(logy1)then
         xyz(2) = 2.*(rndm(ipair) - 0.5)*duo_ran_y1 +
     +            duo_yvert(1) ! cm
      else
         xyz(2) = duo_yvert(1) ! cm
      endif                           ! check on Y1 randomiyation
      if(logz1)then
         xyz(3) = 2.*(rndm(ipair) - 0.5)*duo_ran_z1 +
     +            duo_zvert(1) ! cm
      else
         xyz(3) = duo_zvert(1) ! cm
      endif                           ! check on Z1 randomization

      kpart = 0
      do ipair = 1,duo_npairs
         kpart = kpart + 1
         idtot(kpart) = duo_partid(1) ! GEANT ID
         id_parent(kpart) = 0            ! Parent ID number
         if(logth1)then
            theta_deg = 2.*(rndm(ipair) - 0.5)*duo_ran_th1 +
     +                  duo_theta(1)     ! degrees
         else
            theta_deg = duo_theta(1)     ! degrees
         endif                           ! check on Theta randomization
         if(logph1)then
            phi_deg = 2.*(rndm(ipair) - 0.5)*duo_ran_ph1 +
     +                  duo_phi(1)       ! degrees
         else
            phi_deg = duo_phi(1)         ! degrees
         endif                           ! check on Phi randomization
         if(logTemperature) then
            testArgument = expFactor1 + rndm(ipair)*normFactor
            if(testArgument.le.0.0) then
               stop ' Error in argument to log function'
            endif
            pptotal = -duo_temperature*log(testArgument)
            if(pptotal.lt.duo_pmin.or.pptotal.gt.duo_pmax)then
               stop ' Error in pptotal computation'
            endif
            else
               if(logp1)then
                  pptotal = 2.*(rndm(ipair) - 0.5)*duo_ran_ptot(1) +
     +                 duo_ptot(1)
               else
                  pptotal =  duo_ptot(1) ! GeV/c
               endif            ! check on logp1
         endif ! check on temperature dependent momentum

c     now fill the momentum arrays

         pptot(2,kpart) = pptotal*sind(theta_deg)*cosd(phi_deg)
         pptot(3,kpart) = pptotal*sind(theta_deg)*sind(phi_deg)
         pptot(4,kpart) = pptotal*cosd(theta_deg)

c     second particle (central arm)

         kpart = kpart + 1
         xyzmv(1,kpart) = xyzmv(1,kpart-1) + delt_x       ! cm
         xyzmv(2,kpart) = xyzmv(2,kpart-1) + delt_y       ! cm
         xyzmv(3,kpart) = xyzmv(3,kpart-1) + delt_z       ! cm
         idtot(kpart) = duo_partid(2)                     ! GEANT ID
         id_parent(kpart) = 0                             ! Parent ID number
         theta_deg = theta_deg + delt_th                  ! degrees
         phi_deg =  phi_deg + delt_ph                     ! degrees
         if(logTemperature) then
            testArgument = expFactor1 + rndm(ipair)*normFactor
            if(testArgument.le.0.0) then
               stop ' Error in argument to log function'
            endif
            pptotal = -duo_temperature*log(testArgument)
            if(pptotal.lt.duo_pmin.or.pptotal.gt.duo_pmax)then
               stop ' Error in pptotal computation'
            endif
            else
               if(logp2)then
                  pptotal = 2.*(rndm(ipair) - 0.5)*duo_ran_ptot(2) +
     +                 duo_ptot(2)
               else
                  pptotal =  duo_ptot(2) ! GeV/c
               endif            ! check on logp1
         endif ! check on temperature dependent momentum

c     now fill the momentum arrays

         pptot(2,kpart) = pptotal*sind(theta_deg)*cosd(phi_deg)
         pptot(3,kpart) = pptotal*sind(theta_deg)*sind(phi_deg)
         pptot(4,kpart) = pptotal*cosd(theta_deg)
      enddo  ! loop on number of pairs
 
      chevt_name = 'DUO_GEN_EVT Generator' 

c     Optional Event Header Information (in EVNT sequence)

      atarg = 197.
      ztarg = 79.
      aproj = 1.0
      zproj = 1.0
      sqrt_s = 200.
      bmin = 0.0
      bmax = 1.0
      nptls = mxtot
 
      ievstat = -1   ! successful return
      return

c     Internal error jumps

 95   continue
      write(6,96)
 96   format(/ , ' DUO_GEN_EVT  End-of-File in reading duo.par file')
      stop ' PISA stopping'
 97   continue
      write(6,98)
 98   format(/ ,' DUO_GEN_EVT <F>: Error in reading duo.par file')
      stop ' PISA stopping'
 99   continue
      write(6,100)
 100  format(/ ,' DUO_GEN_EVT <F>: Cannot find  duo.par   file')
      stop ' PISA stopping'
      end
