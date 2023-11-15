      SUBROUTINE TEXT_FILE

      implicit none

      include 'g77trigdef.inc'

c
c     Original Author: Stephen C. Johnson (scjohnson@llnl.gov)
c     Creation Date: July 14, 2000
c
c     This command reads in an oscar formatted file into pisa.
c     It currently only accepts the format as defined by OSCAR1999A
c     on http://rhic.phys.columbia.edu/oscar/
c
c     Revision History
c     Ralf Averbeck     April 30, 2002    Additions to get event vertex in PISA header record
c	
c    Anuj K. Purwar June 4, 2002
c    1) modified gtran.f and pdgtran.f to include deuterons/antideuterons
c
c

      include 'gcflag.inc'
      include 'guevgen.inc'
      include 'gckine.inc'
      include 'subevt.inc'
      include 'pisa_parts.inc'
      include 'secubuf.inc'
c
c     External declarations
c     =====================

      Integer gtran
      Integer pdgtran
c
c     local specifications
c     
      real partx, party, partz, partt, ist, partmass
      integer nin, temp
      integer i,j,k
      integer nfirst, numcomments
      data nfirst /1/
      data numcomments /0/
      character*80 line
      integer pdgid, stdid

      multi_xyz = 1      ! key variable used by GUKINE (1 is multiple vertex)

      if (nfirst.eq.1) then
         nfirst = 0
c     First open the input file which should be oscar compliant
         open(95,FILE='oscar.input')

         
c     check the first line to see if its compliant
         read(95,'(A)') line
         if (line(1:10).eq.'# OSC1999A') then
            write(*,*) 'OSCAR 1999A compliant file'
         else
            write(*,*) 'ERROR: THIS FILE IS NOT OSCAR 1999A COMPLIANT!!'
         endif
         do while (line(1:1).eq.'#')
            read(95,'(A)') line
            numcomments=numcomments+1
         enddo
         rewind 95
         do i=1,numcomments
            read(95,'(A)') line
         enddo
      endif ! if (nfirst.eq.1) then

c     The format of the oscar file should be very simple.
c     There are a few cross-checks here to make sure everything is kosher.
      mxtot = 0
      read(95,*, END=999) nin, mxtot
c      write(*,*) 'mxtot = ', mxtot, ' and nin = ', nin
      if (nin.ne.0) then
         write(*,*) 'ERROR: THIS FILE APPEARS TO BE A FULL EVENT'
         write(*,*) 'HISTORY.  NOT A FINAL LIST OF PARTICLES!'
         write(*,*) 'SKIPPING INCOMING PARTICLES'
         endif
      do i=1,nin
         read(95,*) ipart, pdgid, ist, pptot(2,i), pptot(3,i), 
     +        pptot(4,i), pptot(1,i), partmass, partx, party, partz,
     +        partt
      enddo
      do i=1,mxtot
         read(95,*) ipart, pdgid, ist, pptot(2,i), pptot(3,i), 
     +        pptot(4,i), pptot(1,i), partmass, partx, party, partz,
     +        partt


         xyzmv(1,i) = partx/1.0e+13 !convert from fm to cm
         xyzmv(2,i) = party/1.0e+13 !convert from fm to cm
         xyzmv(3,i) = partz/1.0e+13 !convert from fm to cm
         
c     Oscar format uses the pdg particle id scheme.  First convert it
c     to the StdHep numbering scheme.
         stdid = pdgtran(pdgid,1)
c     Then convert that id to the geant scheme.
         idtot(i) = gtran(stdid,2)
         id_parent(i) = 0

c         write(*,*) 'The particle id ', pdgid, ' was translated to ',
c     $        idtot(i)
         if (ipart.eq.0.and.idtot(i).eq.0) then
            write(*,*) 'ERROR: REACHED END OF EVENT BEFORE DESIGNATED'
            write(*,*) '       NUMBER OF TRACKS WERE READ IN'
            write(*,*) '       CHECK THE FORMAT OF YOUR FILE'
         endif


c     use the vertex of the first particle as event vertex
         if (i.eq.1) then
           xyz(1) = xyzmv(1,1)
           xyz(2) = xyzmv(2,1)
           xyz(3) = xyzmv(3,1)
c     correct for the fact that the input particles are artificially
c     moved to the converter (R=29.0037 cm) 
c     calculate back the original event vertex:
c     z_evt = z_part - R*p_z/p_t
           xyz(1) = 0.0
           xyz(2) = 0.0
           xyz(3) = xyz(3) - 
     +              29.0037*pptot(4,i)/sqrt(pptot(2,i)**2+pptot(3,i)**2)
c     x coordinate
           kvtx_par(1,1) = xyz(1)
           kvtx_par(1,2) = 0.0
c     y coordinate
           kvtx_par(2,1) = xyz(2)
           kvtx_par(2,2) = 0.0
c     z coordinate
           kvtx_par(3,1) = xyz(3)
           kvtx_par(3,2) = 0.0
           call evt_set_vertex(kvtx_par,IEVENT,xyz)
         endif

      enddo

      read(95,*) nin, temp
      if (nin.ne.0.or.temp.ne.0) then
         write(*,*) 'ERROR: IN FORMAT OF INPUT FILE'
         write(*,*) 'DID NOT REACH END OF EVENT WHEN I WAS SUPPOSED TO'
      endif

      NTRU_EVT = IEVENT
      END_EVTFLG = .TRUE.
      NSUB_EVT = 1
      
 999  RETURN
      END
