      SUBROUTINE TEXT_FILE

      implicit none

#include "g77trigdef.inc"
#include "event.inc"
#include "gcflag.inc"
#include "guevgen.inc"
#include "guphnx.inc"
#include "gckine.inc"
#include "subevt.inc"
#include "pisa_parts.inc"
#include "secubuf.inc"

c     External declarations
c     =====================

      Integer gtran
      Integer pdgtran

c     local specifications
      real partx, party, partz, partt, ist, partmass
      integer nin, temp
      integer i,j,k
      integer nfirst, numcomments
      data nfirst /1/
      data numcomments /0/
      character*80 line
      integer pdgid, stdid
      
      real impact_parameter, reaction_plane
            
      logical is_oscar_1999;
      logical is_oscar_1997;
      
      ! check the first line to see if its compliant
      data is_oscar_1999 /.false./
      data is_oscar_1997 /.false./
            
      save is_oscar_1999, is_oscar_1997
              
      ! key variable used by GUKINE (1 is multiple vertex)
      multi_xyz = 1

      ! increment event number
      numevt = numevt + 1

      ! check file format from first line
      if (nfirst.eq.1) then

        ! First open the input file which should be oscar compliant
        nfirst = 0
        open( 95, file = cdmci_file )

        read(95,'(A)') line
        if (line(1:10).eq.'# OSC1999A') then
          
          write(*,*) 'text_file - OSCAR 1999A compliant file'
          is_oscar_1999 = .true.
          
          ! skip header. Header is tagged by "# as a first characted"
          do while (line(1:1).eq.'#')
            read(95,'(A)') line
            numcomments=numcomments+1
          enddo
                
          rewind 95
          do i=1,numcomments
            read(95,'(A)') line
          enddo

          ! Handle the nskip command if the user issued it
          if (ikine2(1) .gt. 0) then
            call do_oscar_nskip(ikine2(1))
          endif
               
               
        else if( line(1:8).eq.'OSC1997A' ) then
                
          write(*,*) 'text_file - OSCAR 1997A compliant file'
          is_oscar_1997 = .true.
                
          ! skip header. Header consists of 2 lines "exactly" after the is_oscar_1997 line
          numcomments = 2
          do i=1,numcomments
            read(95,'(A)') line
          enddo

        else
               
          write(*,*) 'text_file - file format not recognized'
        endif


      endif 

      ! The format of the oscar file should be very simple.
      ! There are a few cross-checks here to make sure everything is kosher
      mxtot = 0
            
      if( is_oscar_1999 ) then
  
        read(95,*, END=999) nin, mxtot
        if (nin.ne.0) then
          write(*,*) 'text_file - this file does not appear to be'
          write(*,*) 'text_file - a final list of particles'
          write(*,*) 'text_file - skipping incoming particles'
        endif
              
        do i=1,nin
          read(95,*) ipart, pdgid, ist, pptot(2,i), pptot(3,i),
     +      pptot(4,i), pptot(1,i), partmass, partx, party, partz,
     +      partt
        enddo
              
      else if( is_oscar_1997 ) then

        read(95,*, END=999) 
     +    nin, mxtot,
     +    impact_parameter,
     +    reaction_plane
           
        ! store impact parameter if necessary
        bimevt = impact_parameter
        write(*,*) 'text_file - bimevt: ', bimevt
              
      else 

        write(*,*) 'text_file - unrecognized file format'
              
      endif

      ! debug statement
      !write( *,* ) 'text_file - reading',
      !+  mxtot, ' particles'
            
      nptls = mxtot
            
      ! loop over particles
      do i=1,mxtot
              
        if( is_oscar_1999 ) then
                
          ! oscar99 format
          read(95,*) ipart, pdgid, ist, pptot(2,i), pptot(3,i),
     +      pptot(4,i), pptot(1,i), partmass, partx, party, partz,
     +      partt
                
        else if( is_oscar_1997 ) then
          
          ! oscar 97 format
          read(95,*) ipart, pdgid, pptot(2,i), pptot(3,i),
     +      pptot(4,i), pptot(1,i), partmass, partx, party, partz,
     +      partt
        
        else
                
          write(*,*) 'text_file - unrecognized file format'
                
        endif
            
        xyzmv(1,i) = partx/1.0e+13 ! convert from fm to cm
        xyzmv(2,i) = party/1.0e+13 ! convert from fm to cm
        xyzmv(3,i) = partz/1.0e+13 ! convert from fm to cm
           
        ! Oscar format uses the pdg particle id scheme.  First convert it
        ! to the StdHep numbering scheme.
        stdid = pdgtran(pdgid,1)
              
        ! Then convert that id to the geant scheme.
        idtot(i) = gtran(stdid,2)
        id_parent(i) = 0
              
c         write(*,*) 'text_file - the particle id ', pdgid, 
c         +    ' was translated to ',
c         +    idtot(i)
                
          if (ipart.eq.0.and.idtot(i).eq.0) then
            write(*,*) 'text_file - reached end of event before'
            write(*,*) ' designated number of tracks were read in.'
            write(*,*) ' check file format.'
          endif
        
                
        ! use the vertex of the first particle as event vertex for event header purpose
        if (i.eq.1) then

          xyz(1) = xyzmv(1,1)
          xyz(2) = xyzmv(2,1)
          xyz(3) = xyzmv(3,1)

c          write( *,* ) 'text_file - primary vertex: (',
c     +      xyz(1), ',', xyz(2), ',', xyz(3), ')'
          
          ! check if the vertex must be reset
          if(phConvPosition.gt.0.0)then

c           correct for the fact that the input particles are artificially
c           moved to the converter (R=29.0037 cm in Run2)
c           calculate back the original event vertex:
c           z_evt = z_part - R*p_z/p_t
            
c           Note: if an internal vertex distribution is used inside PISA
c           it is applied on top of this correction in guevgen.f
c           to use at your own risk (Hugo)
            xyz(1) = 0.0
            xyz(2) = 0.0
            xyz(3) = xyz(3) - phConvPosition*pptot(4,1)/
     +        sqrt(pptot(2,1)**2+pptot(3,1)**2)
              
          endif
        endif
      enddo
      
      ! event trailer line, for oscar1999 file format
      if( is_oscar_1999) then 
        read(95,*) nin, temp
        if (nin.ne.0.or.temp.ne.0) then
          write(*,*) 'error: in format of input file'
          write(*,*) 'did not reach end of event when i was supposed to'
        endif
      endif
            
      ntru_evt = ievent
      end_evtflg = .true.
      nsub_evt = 1

      return

 999  continue    ! branch point if end-of-file is detected

      ieorun = 1  ! set end-of-run condition for kilcon check
      return
      end

c     =================================================================
      subroutine do_oscar_nskip (NumSkip)
           
      ! Heavily copied-and-pasted from above.  It would be wise to at
      ! least check this over if anything above changes.

      ! Note (from Hugo)
      ! This is _really_ bad. All code duplication and copy/paste should be avoided at all cost
      ! because this will _always_ create problem in the future
      ! on _must_ put the duplicated code in a single place, and call it twice
            
      implicit none

      integer NumSkip
      integer i, n, nin, mxtot, temp
      character*80 line

      do n = 1, NumSkip

        read (95, *, END=1999) nin, mxtot

        if (nin.ne.0) then
          write(*,*) 'ERROR: THIS FILE APPEARS TO BE A FULL EVENT'
          write(*,*) 'HISTORY.  NOT A FINAL LIST OF PARTICLES!'
          write(*,*) 'SKIPPING INCOMING PARTICLES'
        endif

        do i = 1, nin
          read(95, '(A)') line
        enddo

        do i = 1, mxtot
          ! Not bothering to verify the format of the skipped lines
          read(95, '(A)') line
        enddo

        read(95,*) nin, temp
        if (nin.ne.0.or.temp.ne.0) then
          write(*,*) 'error: in format of input file'
          write(*,*) 'did not reach end of event when i was supposed to'
        endif

      enddo

1999  return
      end
