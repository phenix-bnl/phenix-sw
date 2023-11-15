      subroutine oscarRootInput(ievstat)
      implicit none


c     Author: Charles F. Maguire
c     Original creation date: June 3, 2002

c     Purpose: interface to ROOT NTUPLE input file (OSCAR format)
c     Called by: GUEVGEN with 33 as number of event type
c     Invoked by: OSCAR command from PISA
c     First call opens the OSCAR ROOT NTUPLE event file
c     All the particles for a given event are put
c     into the GEANT primary particle array
c     Call to getoscarrevent will retrieve the number of
c     particles in the event, the PDG particle ID,
c     the momentum of each particle, and the vertex of
c     each particle.
c     Need to do conversion from PDG particle ID to GEANT ID
c     OSCAR NTUPLE has vertex in fermi for each particle

c     Global variables

      integer ievstat

#include "g77trigdef.inc"
#include "guevgen.inc"
#include "event.inc"
#include "guphnx.inc"

      integer gtran
      integer pdgtran
      integer tempID
      integer kpart
      integer istart
      integer nclone
      integer stagedOSCAR /0/
      save istart, nclone
      save stagedOSCAR

      integer lastStage /0/
      save lastStage

      integer finishFile /0/
      save finishFile

      integer cLength

      integer ifirst /1/
      integer iWarn /0/  ! future use variable for two particle correlations
      real secondZ       ! future use variable for two particle correlations

c     Begin execution


      if(finishFile.eq.1)then

         write(6,11)
 11      format(//,' oscarRootInput <I>: set END-OF-RUN condition ',//)
         ievstat = 1
         finishFile = 0
         return

      endif                    

      numevt = numevt + 1 
      ! key variable used by GUKINE (1 is multiple vertex)
      multi_xyz = 0      

      chevt_name = 'OSCAR ROOT FILE INPUT'
      if(ifirst.eq.1 .or. lastStage.ne.NSTAGE)then

        ifirst = 0
        lastStage = NSTAGE
        istart = ikine2(1)
        nclone = ikine2(2)
        stagedOSCAR = ikine2(3)
        cLength = index(CDMCI_FILE,' ')
        write(6,1)istart, nclone, stagedOSCAR,
     +    CDMCI_FILE, cLength, multi_xyz
1       format(//,' oscarRootInput <I>: Starting at event ',i10,
     +    '  number of clones (recycles) = ', i3,/,
     +    '  stagedOSCAR = ', i2, ', with file name ', a80,/,
     +    '  containing ', i2, ' characters',
     +    '  with multi_xyz = ', i2,/)
          
      endif
        
      call getoscarevent(
     +  mxtot,idtot, pptot, xyzmv, istart, 
     +  nclone  , cLength, CDMCI_FILE)

      if(mxtot.lt.0) then

        
        !Indicate that this is the last event in the OSCAR file
        finishFile = 1
        mxtot = -mxtot
        
      endif

      if(mxtot.gt.0)then

c       Ancient code in e_put_dst picks up event vertex from xyz array
c       Vertex info is then transferred to event header
c       Use the first particle as the event vertex
        
c       If different OSCAR particles have different vertex points, then
c       the event really does not have a single vertex

         xyz(1) = xyzmv(1,1)
         xyz(2) = xyzmv(2,1)
         xyz(3) = xyzmv(3,1)
         
         if(phConvPosition.gt.0.0)then

c         correct for the fact that the input particles are artificially
c         moved to the converter (R=29.0037 cm in Run2) 
c         calculate back the original event vertex:
c         z_evt = z_part - R*p_z/p_t

          xyz(1) = 0.0
          xyz(2) = 0.0
          xyz(3) = 
     +      xyz(3)    - phConvPosition*pptot(4,1)/
     +      sqrt(pptot(2,1)**2+pptot(3,1)**2)

          if(iWarn.eq.0.and.mxtot.eq.2)then

            secondZ = xyzmv(3,2) - phConvPosition*pptot(4,2)/
     +        sqrt(pptot(2,2)**2+pptot(3,2)**2)

            if(abs(secondZ - xyz(3)).gt.0.01)then
              iWarn = 1  ! warning message is given only once
              write(6,222)xyz(1), secondZ
222           format(//, 'oscarToRoot <W>: One time worning',
     +          /, '  first Z = ', e14.5,
     +          /, '  second Z = ', e14.5,//)
              endif
              
            endif ! check for the second particle and no previous warning
          endif  ! check if the vertex must be reset from Converter position

         if(stagedOSCAR.eq.0)then
          
          do kpart = 1, mxtot

c           Convert PDG ID to GEANT ID
c           Oscar format uses the pdg particle id scheme.  First convert it
c           to the StdHep numbering scheme.

            tempID = pdgtran(idtot(kpart),1)
            tempID =  gtran(tempID,2)
            
c             write( *,* ) 
c      +        'oscarRootInput -',
c      +        ' input: ',  idtot(kpart),
c      +        ' output: ',  tempID
            
            idtot(kpart) = tempID            
            id_parent(kpart) = 0
                  
          enddo               ! loop over number of particles in event
        endif                 ! check if stagedOSCAR = 0
      endif                   ! check on at least one particle

      ievstat = -1              ! successful return
      return
      end
