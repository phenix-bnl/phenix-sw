      subroutine qed_ee_evt(ievstat)

      implicit none

#include "g77trigdef.inc"


c     Interface to QED Electron-pair event generator
c     C.F. Maguire: May 1995
c     Called from GUEVGEN
c     Calls QED_MAKE_PAIR, QED_GEN_PAIR_EVT
c     Generator developed by Mirek Fatyga, December 1993

c     Calling variable

      integer ievstat   ! event return status (called with value = 0)

c     Global event variables

#include "guevgen.inc"
#include "event.inc"
#include "subevt.inc"

c     External calls

      logical qed_make_pair
      logical qed_gen_pair_evt
      real rndm

c   CFM: QED ee NTUPLE diagnosis insertions (temporary use only)

      integer np98       ! number of NTUPLE parameters
      parameter (np98=9)
      integer nt98max    ! maximum number of entries
      parameter (nt98max=100000)  ! provisional value
      integer nt98k      ! running count value
      character*8 ch98(np98) /'NPAIR', 'P1TOTAL', 'P1THETA', 
     +                        'P1PHI', 'P2TOTAL', 'P2THETA',
     +                        'P2PHI', 'IKPAIR',
     +                        'EVENT'/
      real evt98(np98)

c     Local variables

      integer npmax    ! maximum number of pairs allowed per event
      parameter (npmax = 15) ! provisional value
      integer first/0/ ! first time call key for this routine
      integer iq       ! random number seed (provided by CERN routines)
      real    minv     ! invariant mass of the pair
      logical stat1    ! initialization key for QED_MAKE_PAIR routine
      real    rapid    ! pair rapidity
      real    p1(3)    ! single positron momentum spherical components
      real    p2(3)    ! single electron momentum spherical components
      real    pm1(npmax,3)   ! multiple positron momentum components
      real    pm2(npmax,3)   ! multiple electron momentum components
      integer stat0    ! control word for QED_GEN_PAIR_EVT subroutine
      real    ainp(3)  ! control words for QED_GEN_PAIR_EVT subroutine
      integer npair    ! number of pairs produced in the event
      logical try      ! logical result of pair generation try
      integer tran     ! random number 
      real    phi_deg  ! azimuthal angle
      integer ik       ! do loop counter
      integer ipart    ! particle ID

c     begin execution


caar  equivalate input parameters with control array
      ainp(1)=kevt_par(2)
      ainp(2)=kevt_par(3)
      ainp(3)=kevt_par(4)
      stat0=INT(kevt_par(1))
caar

      ievstat = 2       ! unsuccessful return (event will be aborted)

      if(first.eq.0)then
         first = 1

c   NOTE: IPOPSUB is normally put in via the KUIP interface
c         Initial version of QED_EE_EVT does not have KUIP interface

         ipopsub = 100  ! number of particles/subevent

c     NTUPLE booking (temporary insertion only)

         call hbookn(98,'QED ee generator',np98,'GEANHIST',
     1                 25000,ch98)
         write(6,*)'  NTUPLE 98 is booked for QED generator'
         nt98k = 0

c     initialization call to QED_MAKE_PAIR

         minv = 0.0
         rapid = 0.0
         stat1 = .true.
         try = qed_make_pair(iq, minv, rapid, stat1, p1, p2)
      endif ! first time call initialization

      numevt = numevt + 1 ! increment event number (required statement)
      multi_xyz = 0    ! key variable for GUKINE (same vertex all particles)
      xyz(1) = 0.0     ! X vertex position
      xyz(2) = 0.0     ! Y vertex position
      xyz(3) = 0.0     ! Z vertex position

c     Set control words for QED_MAKE_PAIR_EVT routine
c     For alternate values and meanings see the QED_MAKE_PAIR_EVT header

caar  stat0 = 10      ! will generate exactly one pair per event, no conditions
caar  ainp(1) = 0.0  ! would be the impact parameter condition
caar  ainp(2) = 0.0  ! would be the invariant mass condition
caar  ainp(3) = 0.0  ! would be the rapidity condition


c     Now throw the event

      try = qed_gen_pair_evt(stat0, ainp, pm1, pm2, npair, iq, npmax)
      do ik = 2,np98-1
         evt98(ik) = 0.0  ! initialize
      enddo
      evt98(1) = npair
      evt98(np98) = float(numevt)
      if(try.and.npair.gt.0)then
         if(npair.gt.npmax)then
            write(6,1)npair, npmax
 1          format(/,2x,' QED_EE_EVT <W>: Number of pairs ',
     +             ' generated = ',i3,'  exceeds maximum ',
     +             ' allowed = ',i3)
            write(6,*)' Reducing NPAIR to ', npmax
            write(6,*)' '
            npair = npmax
         endif ! safety check inserted by C.F. Maguire
         mxtot = npair + npair  ! total number of particles for PISA
         ipart = 0  ! count number of particles
         do ik = 1,npair

c     first particle of pair

            ipart = ipart + 1
            phi_deg = 360.0*rndm(ik)
            pptot(4,ipart) = .001*pm1(ik,1)   ! Z momentum component
            pptot(3,ipart) = .001*pm1(ik,2)*sind(phi_deg)  !  Y component
            pptot(2,ipart) = .001*pm1(ik,2)*cosd(phi_deg)  !  X component
            id_parent(ipart) = 1001  ! for future reference
            p1(1) = sqrt(pm1(ik,1)*pm1(ik,1) + pm1(ik,2)*pm1(ik,2))
            if(p1(1).gt.0.0)then
               p1(2) = acosd(pm1(ik,1)/p1(1))
               p1(3) = phi_deg
            else
               p1(2) = 0.0
               p1(3) = 0.0
            endif ! check on non-zero total momentum
            p1(1) = 0.001*p1(1)  ! convert to standard GeV unit

c     second particle of pair

            ipart = ipart + 1
            phi_deg = phi_deg + 180.0
            pptot(4,ipart) = .001*pm2(ik,1)   ! Z momentum component
            pptot(3,ipart) = .001*pm2(ik,2)*sind(phi_deg)  !  Y component
            pptot(2,ipart) = .001*pm2(ik,2)*cosd(phi_deg)  !  X component
            id_parent(ipart) = 1001  ! for future reference
            p2(1) = sqrt(pm2(ik,1)*pm2(ik,1) + pm2(ik,2)*pm2(ik,2))
            if(p2(1).gt.0.0)then
               p2(2) = acosd(pm2(ik,1)/p2(1))
               p2(3) = phi_deg
            else
               p2(2) = 0.0
               p2(3) = 0.0
            endif ! check on non-zero total momentum
            p2(1) = 0.001*p2(1)  ! convert to standard GeV unit

c     randomize the particle ID

            tran = rndm(ik)
            if(tran.lt.0.50)then
               idtot(ipart-1) = 2
               idtot(ipart) = 3
            else
               idtot(ipart-1) = 3
               idtot(ipart) = 2
            endif ! setting the particle IDs for PISA
            evt98(np98-1) = ik
            evt98(2) = p1(1)
            evt98(3) = p1(2)
            evt98(4) = p1(3)
            evt98(5) = p2(1)
            evt98(6) = p2(2)
            evt98(7) = p2(3)
            evt98(8) = float(ik)
            nt98k = nt98k + 1
            if(nt98k.le.nt98max)then
               call hfn(98,evt98)
            endif ! check on ntuple store
         enddo  ! loop over number of pairs
      else
         nt98k = nt98k + 1
         if(nt98k.le.nt98max)then
            call hfn(98,evt98)
         endif ! check on ntuple store
      endif  ! check on valid try return

      chevt_name = 'QED ee generator (M. Fatgya author)'  ! event name ID

c     Optional Event Header Information (in EVNT sequence)

      atarg = 197.
      ztarg = 79.
      aproj = 1.0
      zproj = 1.0
      sqrt_s = 200.
      bmin = 0.0
      bmax = 1.0
      nptls = mxtot

      ievstat = -1      ! successful return
      return
      end
