c     $Id: guevgen.f,v 1.19 2008/10/24 17:19:54 hpereira Exp $

      subroutine guevgen (ievstat)
      implicit none

c     Authors: S.R. Tonse and C.F. Maguire  (FOPI relic)
C.    Event generator switchyard for primary tracks
C.    ---------------------------------------------

c     Global variables
#include "gconst.inc"
#include "gckine.inc"
#include "guphnx.inc"
#include "guevgen.inc"
#include "subevt.inc"

c     Local variables
c     note: mxtot in guevegen.inc cannot be bigger than 20000 
c     Subroutine arguments
      
      ! 0 indicates failure
      integer ievstat
 

c     external declarations
      real rndm ! cern random number generator
      real gausin ! cern gaussian inversion function
      integer venus_evt ! event reading routine
      integer hijet_evt ! event reading routine
      integer luciae_evt ! event reading routine
      integer rqmd_evt ! event reading routine
      integer hijing_evt ! event reading routine
      integer pm_rhic_evt ! event reading routine
      integer vni_evt ! event reading routine 
      integer rqmdfile9_evt ! event reading routine
      integer nexusascii_evt ! event reading routine

c     local declarations
      integer idxprt ! index over particles
      real pmin, prnge, ! min value + range of p for smooth
     &  phimin, phirnge,! phi
     &  p, theta, phi, ! particle momentum and angles (deg)
     &  px, py, pz, ! particle momenta components
     &  random, ! temporary random number
     &  thmin, thrnge, ! theta
     &  thrad, phirad ! theta and phi in radians
         
      ! generate vertex offset for this event 
      call init_vertexOffset()
      call generate_vertex_offset()    
      
      ! generate random reaction plane angle
      call grndm(random, 1)
      reactionPlaneAngle = 360.0*random

      ! handle kuip interface
      if (kuip_evt) then

        ! generate particles for event
        if (evt_type .eq. evt_smooth) then

          ! loop over particles and generate smooth distribution
          do idxprt = 1, mxtot
            pmin = pkine(1)
            prnge = pkine(2)
 
            thmin = pkine(3)
            thrnge = pkine(4)
 
            phimin = pkine(5)
            phirnge = pkine(6)
 
c           note: the argument to rndm is a dummy but cern suggests using
c           index of loop as argument to prevent optimizer from
c           assuming rndm (0.) is the same number for all calls.
            p     = pmin   +   prnge*rndm (idxprt)
            theta = thmin  +  thrnge*rndm (idxprt + 1)
            phi   = phimin + phirnge*rndm (idxprt + 2)
 
            thrad = degrad*theta
            phirad = degrad*phi
 
            pz = p*cos (thrad)
            px = p*sin (thrad)*cos (phirad)
            py = p*sin (thrad)*sin (phirad)
 
            pptot (2, idxprt) = px
            pptot (3, idxprt) = py
            pptot (4, idxprt) = pz

            ! vertex is set at 0 since it is handled at the end of this switch statement
            xyzmv(1,idxprt) = 0 
            xyzmv(2,idxprt) = 0 
            xyzmv(3,idxprt) = 0 
            
            ! use the vertex of the first particle as event vertex for event header purpose
            if (idxprt.eq.1) then
              xyz(1) = xyzmv(1,1)
              xyz(2) = xyzmv(2,1)
              xyz(3) = xyzmv(3,1)
            endif
            
          end do  
        end if 
 
        ievstat = -1
        go to 90000
      else

        ! Event input controlled by IKINE switch
        go to (1000,2000,3000,4000,5000,6000,7000,8000,9000,
     1    10000,11000,12000,13000,14000,15000,16000,17000,18000,
     2    19000,20000,21000,22000, 23000, 24000, 25000, 26000,
     3    27000, 28000, 29000, 30000, 31000, 32000, 33000,
     4    34000,35000)                  
     5    iabs(ikine)
      endif
      goto 99999

1000  continue
      ! MC_RANCO needs to fill the SUBEVT common block
      call mc_ranco
      ievstat = -1
      goto 90000

2000  continue
      ievstat = venus_evt()
      if(ievstat .ne. -1)then
        write(6,*)' guevgen - reading encountered venus eof or ',
     &    'error condition'
      end if
      goto 90000

3000  continue
      ievstat = hijet_evt()
      if(ievstat .ne. -1)then
        write(6,*)' guevgen - reading encountered hijet eof or ',
     &    'error condition'
      end if
      goto 90000
      
4000  continue
      write(6,4001)
4001  format(/,'  guevgen -  mcuser obsolete event generator ' ,/) 
      stop ' guevgen stopping'

5000  continue               ! obsolete laurie waters interface
      write(6,5001)
5001  format(/,'  guevgen -  lwleak obsolete event generator ' ,/) 
      stop ' guevgen stopping'

6000  continue
      call jpsi_event        ! cfm call
      ievstat = -1
      goto 90000

7000  continue
      call ua1_evt
      ievstat = -1
      goto 90000

8000  continue
      call rv_jpsi
      ievstat = -1
      go to 90000
      
9000  continue                ! cfm call
      call cfm_sngp(ievstat)
      go to 90000
      
10000 continue
      call mcrap              ! Soren Sorensen call (formerly in GUKINE)
      ievstat = -1
      go to 90000
      
11000 continue
      call mcmum              ! Soren Sorensen call (formerly in GUKINE)
      ievstat = -1
      go to 90000
      
12000 continue
      call mcdim              ! Soren Sorensen call (formerly in GUKINE)
      ievstat = -1
      go to 90000
      
13000 continue
      call rv_phi
      ievstat = -1
      go to 90000
      
14000 continue
      call mu_spgen           ! Surender Saini generator for muon arm
      ievstat = -1
      go to 90000
      
15000 continue
      ievstat = 0
      call gen_evt(ievstat)  ! Generic event generator (Muon Arm, Nov. 1994)
      go to 90000
      
16000 continue
      ievstat = 0
      call qed_ee_evt(ievstat)  ! Electron pair generator (for BBC, May 1995)
      go to 90000
      
17000 continue
      ievstat = 0
      call beam_gas_evt(ievstat) ! Beam gas event generator (Paul Kirk, 7/95)
      go to 90000
      
18000 continue
      ievstat = 0
      call duo_gen_evt(ievstat)  ! Dual particle generator (C.Maguire, 4/14/96)
      go to 90000
      
19000 continue
      ievstat = luciae_evt()      ! luciae particle generator (li qun, 3/15/96)
      if(ievstat .ne. -1)then
        write(6,*)' guevgen - reading encountered luciae eof or ',
     &    'error condition'
      end if
      go to 90000
      
20000 continue
      ievstat = hijing_evt()
      if(ievstat .ne. -1)then
        write(6,*)' guevgen - reading encountered hijing eof or ',
     &    'error condition'
      end if
      go to 90000
      
21000 continue
      ievstat = rqmd_evt()
      IF(IEVSTAT .NE. -1)THEN
        WRITE(6,*)' guevgen - Reading encountered RQMD EOF or ',
     &    'error condition'
      END IF
      go to 90000
      
22000 continue
      ievstat = pm_rhic_evt()
      IF(IEVSTAT .NE. -1)THEN
        WRITE(6,*)' guevgen - Reading encountered PM_RHIC_EVT ',
     &    'error condition'
      END IF
      go to 90000
      
23000 continue
      ievstat = 0
      call au_venus_evt(ievstat)
      IF(IEVSTAT .NE. -1)THEN
        WRITE(6,*)' guevgen -  Error return from AU_VENUS_EVT'
      END IF
      go to 90000
      
24000 continue
      ievstat = 0
      call pythia_evt(ievstat)
      IF(IEVSTAT .NE. -1)THEN
        WRITE(6,*)' guevgen -  Error return from PYTHIA_EVT'
      END IF
      go to 90000
      
25000 continue
      call rv_chi
      ievstat = -1
      go to 90000
      
26000 continue
      ievstat = vni_evt()
      IF(IEVSTAT .NE. -1)THEN
        WRITE(6,*)' guevgen - Reading encountered VNI EOF or ',
     &    'error condition'
      END IF
      go to 90000
      
27000  continue
      call sngl_jpsi
      ievstat = -1
      go to 90000
      
28000 continue
      call sngl_phi
      ievstat = -1
      go to 90000
      
29000 continue
      call text_file
      ievstat = -1
      go to 90000
      
30000 continue
      ievstat = rqmdfile9_evt()
      IF(IEVSTAT .NE. -1)THEN
        WRITE(6,*)' guevgen - Reading encountered RQMD EOF or ',
     &    'error condition'
      END IF
      go to 90000
      
31000 continue
      ievstat = nexusascii_evt()
      IF(IEVSTAT .NE. -1)THEN
        WRITE(6,*)' guevgen - Reading encountered NEXUS EOF or ',
     &    'error condition'
      END IF
      go to 90000
32000 continue
      call sngl_neutral
      ievstat = -1
      go to 90000
      
33000 continue
      call oscarRootInput(ievstat)
      go to 90000
      
34000 continue
      call polrpl
      ievstat = -1
      go to 90000
      
35000 continue
      call pythiarootinput(ievstat)
      go to 90000
90000 continue

      if(stplv2.eq.1)then
         call setpolv2
      endif
      
      ! generate vertex position based on the 
      ! xyz0_input and vrms values, as defined in 
      ! the event.par file
              
      ! move particles origin by the generated vertex offset
      do idxprt=1,mxtot
      call apply_vertex_offset(
     +  xyzmv(1,idxprt),
     +  xyzmv(2,idxprt),
     +  xyzmv(3,idxprt) )
      enddo
      
      ! also smear the 'global' event vertex
      call apply_vertex_offset(
     +  xyz(1),
     +  xyz(2),
     +  xyz(3) )
      
99999 continue
      return
      end
