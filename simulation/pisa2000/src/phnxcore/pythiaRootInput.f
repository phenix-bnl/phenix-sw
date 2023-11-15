c $Id: pythiaRootInput.f,v 1.8 2009/08/22 02:52:02 hpereira Exp $

      subroutine pythiaRootInput(ievstat)
      implicit none

#include "g77trigdef.inc"

c     Author: Mickey Chiu Based on oscarRootInput by C.Maguire
c     Original creation date: May 21, 2007

c     Purpose: interface to ROOT input file (PHPythia in Fun4All format)

c     Called by: GUEVGEN with 33 as number of event type
c     Invoked by: PHPYTHIA command from PISA

c     First call opens the PYTHIA ROOT NTUPLE event file

c     All the particles for a given event are put
c     into the GEANT primary particle array

c     Call to getpythiaevent will retrieve the number of
c     particles in the event, the PDG particle ID,
c     the momentum of each particle, and the vertex of
c     each particle.

c     Need to do conversion from PYTHIA particle ID to GEANT ID
      
      ! Global variables
      integer ievstat
#include "kincut.inc"
#include "guevgen.inc"
#include "event.inc"
#include "guphnx.inc"

#include "gckine.inc"
#include "evntcode.inc"
#include "pythia_e.inc"

      integer gtran
      integer pdgtran
      integer tempID
      integer kpart
      integer istart
      integer nclone
      integer stagedPYTHIA /0/
      save istart, nclone
      save stagedPYTHIA

      integer lastStage /0/
      save lastStage

      integer finishFile /0/
      save finishFile

      integer cLength

      integer ifirst /1/
c     Begin execution

      ! test end of run condition
      if(finishFile.eq.1) then

        write(6,11)
 11     format(//,' pythiaRootInput - set END-OF-RUN condition ',//)

        ievstat = 1
        finishFile = 0
        return

      endif
      
      ! increment event number
      numevt = numevt + 1
      
      ! set multi vertex (one vertex per particle) flag to true
      ! it is used by GUKINE
      multi_xyz = 1     

      chevt_name = 'PYTHIA ROOT FILE INPUT'
      if(ifirst.eq.1 .or. lastStage.ne.NSTAGE) then

        ifirst = 0
        lastStage = NSTAGE
        istart = ikine2(1)
        nclone = ikine2(2)
        stagedPYTHIA = ikine2(3)
        cLength = index(CDMCI_FILE,' ')
        
        write(6,1)
     +    istart, nclone, stagedPYTHIA,
     +    CDMCI_FILE, cLength
          
 1      format(' pythiaRootInput - Starting at event ',i10,
     +    '  number of clones (recycles) = ', i3,/,
     +    '  stagedPYTHIA = ', i2, ', with file name ', a80,/,
     +    '  containing ', i2, ' characters')

      endif

      call getpythiaevent(
     +  mxtot,idtot, pptot, xyzmv, xyz, bimevt,
     +  istart, nclone,
     +  cLength, CDMCI_FILE)

      if(mxtot.lt.0)then

        ! Indicate that this is the last event in the PYTHIA file
        finishFile = 1
        mxtot = -mxtot

      endif

      if(mxtot.gt.0)then
        
        if(stagedPYTHIA.eq.0) then
          do kpart = 1, mxtot

c           Convert PDG ID to GEANT ID
c           Oscar format uses the pdg particle id scheme.  First convert it
c           to the StdHep numbering scheme.
            tempID = pdgtran(idtot(kpart),1)

c           Then convert that id to the geant scheme.
            idtot(kpart) = gtran(tempID,2)

          enddo ! loop over number of particles in event
        endif ! check if stagedPYTHIA = 0
      endif ! check on at least one particle
      
      ! successful return
      ievstat = -1 
      return
      end
