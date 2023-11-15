      subroutine gen_evt(ievstat)

      implicit none

#include "g77trigdef.inc"
 
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


*****************************************************************************

c     Global variables

      integer ievstat
#include "guevgen.inc"
#include "event.inc"
 

c     Local variables (simple sample)

      real theta_deg  ! polar angle
      real phi_deg    ! azimuthal angle
      real pptotal    ! total momentum

c     begin execution (instructions below repeat what was said above)

      numevt = numevt + 1 ! do not change this statement

c     You must fill in the PPTOT, XYZ (or XYZMV), and IDTOT arrays with
c     MXTOT <= 20000 primary particles.

c     You must provide a 40 character or less event descriptor CHEVT_NAME.

c     You must return IEVSTAT = -1 (normal) or IEVSTAT = +2 (abnormal)

      mxtot = 2      ! number of primary particles in this event
 

c     multiple vertex positions

      multi_xyz = 1      ! key variable used by GUKINE

c     first particle (muon arm)

      xyzmv(1,1) = 0.0   ! cm
      xyzmv(2,1) = 0.0   ! cm
      xyzmv(3,1) = 0.0   ! cm
      idtot(1) = 5       ! GEANT muon
      id_parent(1) = 8   ! Parent ID number
      theta_deg = 3.5    ! degrees
      phi_deg = 5.0      ! degrees
      pptotal = 5.0      ! GeV/c

c     now fill the momentum arrays

      pptot(2,1) = pptotal*sind(theta_deg)*cosd(phi_deg)
      pptot(3,1) = pptotal*sind(theta_deg)*sind(phi_deg)
      pptot(4,1) = pptotal*cosd(theta_deg)

c     second particle (central arm)

      xyzmv(1,2) = 0.1   ! cm
      xyzmv(2,2) = 0.2   ! cm
      xyzmv(3,2) = 0.3   ! cm
      idtot(2) = 3       ! GEANT electron
      id_parent(2) = 7   ! Parent ID number
      theta_deg = 80.0   ! degrees
      phi_deg = 5.0      ! degrees
      pptotal = 5.5      ! GeV/c

c     now fill the momentum arrays

      pptot(2,2) = pptotal*sind(theta_deg)*cosd(phi_deg)
      pptot(3,2) = pptotal*sind(theta_deg)*sind(phi_deg)
      pptot(4,2) = pptotal*cosd(theta_deg)
 
      chevt_name = 'My own very special event generator'  ! do better than this
 

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
      end
