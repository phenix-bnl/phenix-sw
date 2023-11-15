c     $Id: pkevnt.f,v 1.43 2009/08/05 21:20:18 kempel Exp $
      subroutine pkevnt
      implicit none

c     Global declarations:
#include "guevgen.inc"
#include "subevt.inc"
#include "gckine.inc"
#include "guphnx.inc"
#include "gcflag.inc"
#include "sublink.inc"
#include "evntcode.inc"
#include "event.inc"
#include "kincut.inc"

      ! Local declarations
      integer evt_mult, ! mult of tracks in event
     &  itrk, ! index over tracks
     &  npar, ! number of kuip command line parm
     &  ieormu, ! dimuon (=0) or dielectron (=1)
     &  ipioreta, ! pizero (=9) or eta (=17) for sngl_neutral
     &  iselect, ! event filter selection (=0,1,2)
     &  istat, ! status from open of file
     &  lengc, ! length of character parameter
     &  lun, ! logical unit number
     &  hfile, ! hfile flag (beam gas)
     &  hefile, ! hefile flag (beam gas)
     &  nfile, ! nfile flag (beam gas)
     &  true_dis, ! true distribution flag (beam gas)
     &  north_south, ! north/south arm option
     &  num_rv, ! number of mesons in rv generators
     &  iranphi ! randomization in phi
            
      real pmin, pmax, ! min and max p for smooth
     &  phimin, phimax, ! min and max phi for smooth
     &  thmin, thmax, ! min and max theta for smooth
     &  yy_min, yy_max, ! min and max rapidity cuts
     &  phimin1, phimax1, ! min and max phi for arm 1
     &  phimin2, phimax2 ! min and max phi for arm 2 
        
      character*80 cmd ! kuip command causing pkevnt call
      integer ievstat, istatx, istart, nclone, stage
      real imp,invm,rapid

      logacc = .false. ! pair acceptance cut (ken barish)
      event_code = 0 ! set default value      
      single_acc = 0
       
      ! Get the KUIP command:
      call kupatl(cmd, npar)
      call cltou(cmd) 
       
      if (cmd .eq. 'SMOOTH') then
        
        call kugeti  (evt_mult)
        call kugetr (pmin)
        call kugetr (pmax)
        call kugetr (thmin)
        call kugetr (thmax)
        call kugetr (phimin)
        call kugetr (phimax)
        pkine(1) = pmin
        pkine(2) = pmax - pmin
        pkine(3) = thmin
        pkine(4) = thmax - thmin
        pkine(5) = phimin
        pkine(6) = phimax - phimin
              
        do itrk = 1, evt_mult
          call kugeti (idtot (itrk))
        end do

        ! set gukine flags
        numevt = 1
        mxtot = evt_mult
        ipopsub = mxtot
        evt_type = evt_smooth
        kuip_evt = .true.

        ! multi_xyz = 1
             
      else  if (cmd .eq. 'OLDEVENT') then
        
        write(6,*) ' old style event selection mode'
        kuip_evt = .false.   ! logical switch used in guevgen subroutine
        multi_xyz = 0        ! default value (single vertex)
         
      else  if (cmd .eq. 'VENUS') then
        
        write(6,*) ' Venus event source selected'
        kuip_evt = .false.   ! logical switch used in guevgen subroutine
        call kuopen(17, 'event.par', 'old', istat)
        if (istat .ne. 0) then
          write(6, '(/,a,a,i4,/,a,/,a,/)' )
     1      'attempt to open  event.par  filter file ',
     2      'gives error code = ',istat,
     3      'you need to create such a file.',
     4      'type  help venus  for an example.'
        endif
        
        close(17)
        call kugeti (ipopsub)
        pkine(1) = float(ipopsub)
        ikine = 2
        
        if (npar .ge. 2) then
          call kugets(cdmci_file,lengc)
        endif
        
        call kuinqf(cdmci_file,lun)
        if (lun .eq. -1) then
          write(6, '(/,2a,/,a,/)' )
     1      ' attempt to find venus input file  ',
     2      cdmci_file,
     3      '  was unsuccessful.  please correct error.'
        endif
        
        if (lun.gt.0) then

          ! venus file was found to be open
          write(6,*)'  venus input file was found to be open.'
          write(6,*)'  the file is being closed for re-use'

          ! note: venus input file was created/opened under regular fortran, not fz
          ! the event file creation/opening procedure should be standardized.
          close(unit=lun,iostat=istat)
          if (istat.ne.0) then
            write(6, '(/,a,i3,a,i6,/)' )
     1        'closing the file number ',lun,
     2        ' gave a return error code ',istat
          endif
          pkine(6) = -1.0     ! force re-open of file
        endif
        multi_xyz = 0   ! default value (single vertex)
         
        !---------------------------------------------
      else if (cmd .eq. 'HIJET') then
        
        write(6,*) ' hijet event source selected'
        
        ! logical switch used in guevgen subroutine
        kuip_evt = .false.  
        call kuopen(17, 'event.par', 'old', istat)
        if (istat .ne. 0) then
          write(6, '(/,a,a,i4,/,a,/,a,/)' )
     1      'attempt to open  event.par  filter file ',
     2      'gives error code = ',istat,
     3      'you need to create such a file.',
     4      'type  help hijet  for an example.'
        endif
        close(17)
        call kugeti (ipopsub)
        pkine(1) = float(ipopsub)
        ikine = 3
        
        if (npar .ge. 2) then
          call kugetf(cdmci_file,lengc)
        else
          lengc= index(cdmci_file,' ') - 1
        endif
        call kuinqf(cdmci_file(1:lengc),lun)
        if (lun .eq. -1) then
          write(6, '(/,2a,/,a,/)' )
     1      ' attempt to find hijet input file  ',
     2      cdmci_file,
     3      '  was unsuccessful.  please correct error.'
        endif
        if (lun.gt.0) then

          !  hijet file was found to be open
          write(6,*)'  hijet input file was found to be open.'
          write(6,*)'  the file is being closed for re-use'
          call fzendi(lun,'t')
          close(unit=lun,iostat=istat)
          if (istat.ne.0) then
            write(6, '(/,a,i3,a,i6,/)' )
     1        'closing the file number ',lun,
     2        ' gave a return error code ',istat
          endif
          pkine(6) = -1.0     ! force re-open of file
        endif
        multi_xyz = 0   ! default value (single vertex)
         
        !---------------------------------------------
      else  if (cmd .eq. 'UA1') then
        write(6,*) ' ua1 event type selected'
        kuip_evt = .false.   ! logical switch used in guevgen subroutine
        ikine = 7
        call kuopen(17, 'ua1event.par', 'old', istat)
        if (istat .ne. 0) then
          write(6, ' (4(/,a),/) ' )
     1      ' filter file  ua1event.par  does not exist',
     2      ' internal (no-filter) parameters are default.',
     3      ' you may create your own  ua1event.par  file.',
     4      ' type  help ua1 for more information.'
          endif
          close(17)
          call kugeti (ipopsub)
          pkine(1) = float(ipopsub)
          multi_xyz = 0   ! default value (single vertex)
           
        !---------------------------------------------
        else  if (cmd .eq. 'GEN_EVT') then
          write(6,*) 'generic event type selected (user supplied)'
          kuip_evt = .false.   ! logical switch used in guevgen subroutine
          ikine = 15  ! for use in guevgen
          call kugeti (ipopsub)
          pkine(1) = float(ipopsub)
          multi_xyz = 0   ! default value (single vertex)
          xyz(1) = 0.0    ! default x0 value for really lazy users
          xyz(2) = 0.0    ! default y0 value for really lazy users
          xyz(3) = 0.0    ! default z0 value for really lazy users

        !---------------------------------------------
        else  if (cmd .eq. 'DUO_GEN_EVT') then
          write(6,*) 'dual particle type event generator supplied'
          write(6,*) 'you must supply your own duo.par namelist file !!'
          kuip_evt = .false.   ! logical switch used in guevgen subroutine
          ikine = 18  ! for use in guevgen
          call kugeti (ipopsub)
          pkine(1) = float(ipopsub)
          multi_xyz = 0   ! default value (single vertex)
          xyz(1) = 0.0    ! default x0 value for really lazy users
          xyz(2) = 0.0    ! default y0 value for really lazy users
          xyz(3) = 0.0    ! default z0 value for really lazy users

        !---------------------------------------------
        else  if (cmd .eq. 'LUCIAE') then
          write(6,*) 'luciae event generator selected'
          kuip_evt = .false.   ! logical switch used in guevgen subroutine

          call kuopen(17, 'event.par', 'old', istat)
          if (istat .ne. 0) then
             write(6, '(/,a,a,i4,/,a,/,a,/)' )
     1        'attempt to open  event.par  filter file ',
     2        'gives error code = ',istat,
     3        'you need to create such a file.',
     4        'type  help luciae  for an example.'
          endif
          close(17)
          call kugeti (ipopsub)
          pkine(1) = float(ipopsub)
          ikine = 19
          if (npar .ge. 2) then 
             call kugetf(cdmci_file,lengc)
          else
            lengc= index(cdmci_file,' ') - 1
          endif
          call kuinqf(cdmci_file(1:lengc),lun)
          if (lun .eq. -1) then
             write(6, '(/,2a,/,a,/)' )
     1        ' attempt to find luciae input file  ',
     2        cdmci_file,
     3        ' was unsuccessful.  please correct error.'
          endif
          if (lun.gt.0) then

            write(6,*)'  luciae input file was found to be open.'
             write(6,*)'  the file is being closed for re-use'
             call fzendi(lun,'t')
             close(unit=lun,iostat=istat)
             if (istat.ne.0) then
                write(6, '(/,a,i3,a,i6,/)' )
     1                'closing the file number ',lun,
     2                ' gave a return error code ',istat
             endif
             pkine(6) = -1.0     ! force re-open of file
          endif
          multi_xyz = 1

        !---------------------------------------------
        ! oscar
        else  if (cmd .eq. 'OSCAR') then
          
          write(6,*) 'pkevnt - OSCAR ROOT NTUPLE used for input events'
          write(6,*) 'using 15 particles/subevent'
          IPOPSUB = 15
          IKINE = 33
          if (NPAR .GE. 2) then
            call KUGETF(CDMCI_FILE,LENGC)
          else
            LENGC= index(CDMCI_FILE,' ') - 1
            if (LENGC .GT. 80) then
              write(6,552)CDMCI_FILE, LENGC
 552          format('pkevnt - input events file ',
     1          ' has too many characters ', i2,/,
     2          ' use a softlink with fewer characters',/)
              stop ' pisa is stopping'
            endif
          endif
          call KUINQF(CDMCI_FILE(1:LENGC),LUN)
          if (LUN .EQ. -1) then
             write(6, '(/,2A,/,A,/)' )
     1                ' Attempt to find OSCAR input file  ',
     2                CDMCI_FILE,
     3                '  was unsuccessful.  Please correct error.'
             write(6,553)
 553         format(/ ' kuip interface does not allow for',
     1             ' uppercase characters in file name',/,
     2             ' use an generic lowercase softlink instead',/)
             stop ' pisa is stopping in pkevent '
          endif

          if (LUN.GT.0) then

            write(6,*)'  OSCAR input file was found to be open.'
            write(6,*)'  The file is being closed for re-use'
            close(UNIT=LUN,IOSTAT=ISTAT)
            if (ISTAT.NE.0) then
              write(6, '(/,A,I3,A,I6,/)' )
     1          'Closing the file number ',LUN,
     2          ' gave a return error code ',ISTAT
              stop ' PISA stopPING IN PKEVNT'
            endif
          endif
          call KUGETI(ISTART-1)   !  start event in NTUPLE
          IKINE2(1) = ISTART
          call KUGETI(NCLONE)   !  how many times to recycle the OSCAR input
          IKINE2(2) = NCLONE
          call KUGETI(STAGE)
          IKINE2(3) = STAGE
          IKINE2(4) = LENGC
          call KUGETR(phConvPosition)  ! used for resetting the Z0 of the event

          if (phConvPosition.gt.0.0) then
            write(6,554)phConvPosition
 554        format(//,' OSCAR/ROOT events will have their ',
     +        'Z position reset using a Converter ',
     +        'radial position at ', f10.6, ' cm',//)
          endif
            
c         OSCAR does provide vertex positions of particles
          multi_xyz = 1

        !---------------------------------------------
        ! phpythia input
        else  if (cmd .eq. 'PHPYTHIA') then
          write(6,*) 'pkevent - phpythia tree used for input events'
          KUIP_EVT = .FALSE.     ! logical switch used in GUEVGEN subroutine
          call KUGETI (IPOPSUB)
          PKINE(1) = float(IPOPSUB)
          IKINE = 35
          if (NPAR .GE. 2) then
            call KUGETF(CDMCI_FILE,LENGC)
          else
            LENGC= index(CDMCI_FILE,' ') - 1
            if (LENGC .GT. 80) then
              write(6,662)CDMCI_FILE, LENGC
 662          format(//,' PKEVNT <F>: INPUT EVENTS FILE ',
     1          ' HAS TOO MANY CHARACTERS ', I2,/,
     2          ' USE A SOFTLINK WITH FEWER CHARACTERS',/)
              stop ' PISA IS stopPING'
            endif
          endif
          call KUINQF(CDMCI_FILE(1:LENGC),LUN)
          if (LUN .EQ. -1) then
             write(6, '(/,2A,/,A,/)' )
     1        ' Attempt to find PHPYTHIA input file  ',
     2        CDMCI_FILE,
     3        '  was unsuccessful.  Please correct error.'
            write(6,663)
 663        format(/ ' In case you did not know the',/,
     +        ' KUIP interface does not allow for',/,
     +        ' UPPERCASE CHARACTERS in the file name',/,
     +        ' So use a generic lower case softlink name',/)
            stop ' PISA IS stopPING IN PKEVENT '
          endif

          if (LUN.GT.0) then

c                  oscar/pythia file was found to be open

            write(6,*)'  phpythia input file was found to be open.'
            write(6,*)'  the file is being closed for re-use'
            close(unit=lun,iostat=istat)
            if (istat.ne.0) then
              write(6, '(/,a,i3,a,i6,/)' )
     1          'closing the file number ',lun,
     2          ' gave a return error code ',istat
              stop ' pisa stopping in pkevnt'
            endif
          endif

c         phpythia does provide vertex positions of particles

          multi_xyz = 1   ! (individual vertex for all particles)
      
        !---------------------------------------------
        ! hijing input file    
        else  if (cmd .eq. 'HIJING') then
          write(6,*) 'hijing event generator selected'
          kuip_evt = .false.   ! logical switch used in guevgen subroutine

          call kuopen(17, 'event.par', 'old', istat)
          if (istat .ne. 0) then
             write(6, '(/,a,a,i4,/,a,/,a,/)' )
     1                'attempt to open  event.par  filter file ',
     2                'gives error code = ',istat,
     3                'you need to create such a file.',
     4                'type  help hijing  for an example.'
          endif
          close(17)
          call kugeti (ipopsub)
          pkine(1) = float(ipopsub)
          ikine = 20
          if (npar .ge. 2) then
            call kugetf(cdmci_file,lengc)
          else
            lengc= index(cdmci_file,' ') - 1
          endif
          call kuinqf(cdmci_file(1:lengc),lun)
          if (lun .eq. -1) then
            write(6, '(/,2a,/,a,/)' )
     1        ' attempt to find hijing input file  ',
     2        cdmci_file,
     3        '  was unsuccessful.  please correct error.'
            endif

            if (lun.gt.0) then
              write(6,*)'  hijing input file was found to be open.'
              write(6,*)'  the file is being closed for re-use'
              call fzendi(lun,'t')
              close(unit=lun,iostat=istat)
              if (istat.ne.0) then
                write(6, '(/,a,i3,a,i6,/)' )
     1            'closing the file number ',lun,
     2            ' gave a return error code ',istat
              endif
              pkine(6) = -1.0     ! force re-open of file
            endif
            
            ! hijing does not provide vertex positions of particles
            multi_xyz = 0   ! (single vertex for all particles)
            
          !------------------------------------------
          ! vnl input file
          else  if (cmd .eq. 'VNI') then
          write(6,*) 'vni event generator selected'
          kuip_evt = .false.   ! logical switch used in guevgen subroutine

          call kuopen(17, 'event.par', 'old', istat)
          if (istat .ne. 0) then
            write(6, '(/,a,a,i4,/,a,/,a,/)' )
     1        'attempt to open  event.par  filter file ',
     2        'gives error code = ',istat,
     3        'you need to create such a file.',
     4        'type  help vni  for an example.'
          endif
          close(17)
          
          call kugeti (ipopsub)
          pkine(1) = float(ipopsub)
          ikine = 26
          if (npar .ge. 2) then
             call kugetf(cdmci_file,lengc)
          else
            lengc= index(cdmci_file,' ') - 1
          endif
          
          call kuinqf(cdmci_file(1:lengc),lun)
          if (lun .eq. -1) then
            write(6, '(/,2a,/,a,/)' )
     1        ' attempt to find vni input file  ',
     2        cdmci_file,
     3        '  was unsuccessful.  please correct error.'
          endif
          
          if (lun.gt.0) then
            write(6,*)'  vni input file was found to be open.'
            write(6,*)'  the file is being closed for re-use'
            call fzendi(lun,'T')
            
            close(unit=lun,iostat=istat)
            if (istat.ne.0) then
              write(6, '(/,a,i3,a,i6,/)' )
     1          'closing the file number ',lun,
     2          ' gave a return error code ',istat
            endif
            pkine(6) = -1.0     ! force re-open of file
          endif

          ! VNI does provide vertex positions of particles, but
          ! these are typically at the fm level
          MULTI_XYZ = 0   ! (Single vertex for all particles)

        !------------------------------------------
        else  if (cmd .eq. 'RQMD') then
          write(6,*) 'RQMD event generator selected'
          
          ! logical switch used in guevgen subroutine
          kuip_evt = .false.   

          call kuopen(17, 'event.par', 'old', istat)
          if (istat .ne. 0) then
             write(6, '(/,a,a,i4,/,a,/,a,/)' )
     1                'attempt to open  event.par  filter file ',
     2                'gives error code = ',istat,
     3                'you need to create such a file.',
     4                'type  help rqmd  for an example.'
          endif
          close(17)
          call kugeti (ipopsub)
          ikine = 21
          if (ipopsub.lt.0) then
             ikine = 30
             ipopsub = -ipopsub
          endif
          pkine(1) = float(ipopsub)
          if (npar .ge. 2) then
             call kugetf(cdmci_file,lengc)
          else
            lengc= index(cdmci_file,' ') - 1
          endif
          call kuinqf(cdmci_file(1:lengc),lun)
          if (lun .eq. -1) then
            write(6, '(/,2a,/,a,/)' )
     1        ' attempt to find rqmd input file  ',
     2        cdmci_file,
     3        '  was unsuccessful.  please correct error.'
          endif
          
          if (lun.gt.0) then
            write(6,*)'  rqmd input file was found to be open.'
            write(6,*)'  the file is being closed for re-use'
            call fzendi(lun,'t')
            close(unit=lun,iostat=istat)
            if (istat.ne.0) then
              write(6, '(/,a,i3,a,i6,/)' )
     1          'closing the file number ',lun,
     2          ' gave a return error code ',istat
            endif
            pkine(6) = -1.0     ! force re-open of file
          endif

          multi_xyz = 0   ! (single vertex for all particles)

        ! -------------------------------------
        ! nexus input file
        else  if (cmd .eq. 'NEXUS') then
          write(6,*) 'nexus event generator selected'
          kuip_evt = .false.   ! logical switch used in guevgen subroutine
          ikine = 31
          call kugeti (ipopsub)
          pkine(1) = float(ipopsub)
          
          multi_xyz = 0   ! (single vertex for all particles)
 
        ! -------------------------------------
        else  if (cmd .eq. 'RV_JPSI') then
          
          write(6,*) ' rv_jpsi event type selected'
          event_code = rv_jpsi
          kuip_evt = .false.   ! logical switch used in guevgen subroutine
          ikine = 8
          ipopsub = 2
          call kugeti (ieormu)     ! double duty 0,1 or 10, 11

c         0,1 ===> J/PSI into e+,e- or mu+,mu-
c         10,11 ===> PSI-PRIME into e+,e- or mu+,mu-

          pkine(1) = float(ieormu)
          call kugeti (iselect)    ! kinematics filter option
          if (iselect.eq.3) then
             logacc = .true.
          else
             logacc = .false.    ! redundant
          endif
          pkine(2) = iselect
          call kugetr (thmin)
          pkine(3) = thmin   ! minimum theta angle for decay particle
          call kugetr (thmax)
          pkine(4) = thmax   ! maximum theta angle for decay particle
          call kugetr (pmin)
          pkine(5) = pmin    ! minimum total momentum (gev/c)
          call kugetr (phimin1)
          pkine(6) = phimin1    ! minimum phi angle arm 1 for decay part
          call kugetr (phimax1)
          pkine(7) = phimax1   ! maximum phi angle arm 1 for decay part
          call kugetr (phimin2)
          pkine(8) = phimin2  ! minimum phi angle arm 2 for decay part
          call kugetr (phimax2)
          pkine(9) = phimax2  ! maximum phi angle arm 2 for decay part
          pkine(10) = -1.0    ! forces reset of rv_jpsi initialization
          multi_xyz = 0   ! default value (single vertex)
 
        ! -------------------------------------
        else  if (cmd .eq. 'SNGL_JPSI') then
          write(6,*) ' sngl_jpsi event type selected'
          event_code = sngl_jpsi
          kuip_evt = .false.   ! logical switch used in guevgen subroutine
          ikine = 27
          ipopsub = 2
          call kugeti (ieormu)     ! double duty 0,1 or 10, 11

c         0,1 ===> J/PSI into e+,e- or mu+,mu-
c         10,11 ===> PSI-PRIME into e+,e- or mu+,mu-
          pkine(1) = float(ieormu)
          call kugeti (iselect)    ! kinematics filter option
          if (iselect.eq.3) then
             logacc = .true.
          else
             logacc = .false.    ! redundant
          endif
          pkine(2) = iselect
          call kugetr (yy_min) ! minimum rapidity for jpsi meson
          pkine(3) = yy_min
          call kugetr (yy_max) ! maximum rapdiity for jpsi meson
          pkine(4) = yy_max
          call kugetr (pt_min) ! minimum pt for jpsi meson
          pkine(5) = pt_min
          call kugetr (pt_max) ! maximum pt for jpsi meson
          pkine(6) = pt_max
          call kugetr (thmin)
          pkine(7) = thmin   ! minimum theta angle for decay particle
          call kugetr (thmax)
          pkine(8) = thmax   ! maximum theta angle for decay particle
          call kugetr (pmin)
          pkine(9) = pmin    ! minimum momentum (gev/c) for decay particle
          pkine(10) = -1.0    ! forces reset of rv_jpsi initialization
          multi_xyz = 0   ! default value (single vertex)
 
        ! -------------------------------------
        else  if (cmd .eq. 'CFM_MULTI') then
          write(6,*) ' CFM Multi-Particle EVENT TYPE SELECTED'
          KUIP_EVT = .FALSE.   ! logical switch used in GUEVGEN subroutine
          call KUINQF('cfmgmc.input',LUN)    ! general inquire
             if (LUN.GT.0) then

C               FILE WAS FOUND TO BE OPEN

                write(6,*)'  cfmgmc.input was found to be open'
                close(LUN)
                write(6,*)'  The file is being closed for re-use'
             endif
          call KUOPEN(17, 'cfmgmc.input', 'old', ISTAT)   ! ASCII inquire
          if (ISTAT .NE. 0) then
             write(6, '(/,A,A,I4,/,A,/,A,/)' )
     1                'Attempt to open  cfmgmc.input  text file ',
     2                'gives error code = ',ISTAT,
     3                'You need to create such a file.',
     4                'Type  HELP CFM_MULTI  for an example.'
          endif
          close(17)
          PKINE(6) = 1.0            ! force a reset of LOGOPN
          IKINE = -9
          MULTI_XYZ = 0   ! DEFAULT VALUE (SINGLE VERTEX)
          
         !==================================        
        else  if (cmd .eq. 'PYTHIA') then
          write(6,*)' pythia input selected'
          kuip_evt = .false.  ! logical for guevgen use
          call kugeti (ipopsub)
          ikine = 24
          multi_xyz = 0   ! default value (single vertex)
 
         !==================================        
        else  if (cmd .eq. 'CFM_SNGP') then
          
          write(6,*) ' cfm single particle event type selected'
          kuip_evt = .false.   ! logical switch used in guevgen subroutine
          
          single_acc = 0
          call kugeti (iselect) ! kinematics filter option
          if (iselect.eq.1) then
             logacc = .true.
             single_acc = 1
             write(6,*)'single particle accepted in either central arm'
          else
             logacc = .false.   ! redundant
             single_acc = 0
          endif
          
          call kugeti (iranphi) ! randomization in phi
          if (iranphi.eq.1) then
             iran_phi = 1
             write(6,*)' '
             write(6,*)'  single particle is generated randomly in phi'
             write(6,*)' '
          else
             iran_phi = 0
             write(6,*)' '
             write(6,*)'  single particle is generated in a theta cone'
             write(6,*)' '
          endif
          call kuinqf('cfmgmc.input',lun)    ! general inquire
             if (lun.gt.0) then

c               file was found to be open

                write(6,*)'  cfmgmc.input was found to be open'
                close(lun)
                write(6,*)'  the file is being closed for re-use'
             endif
          call kuopen(17, 'cfmgmc.input', 'old', istat)   ! ascii inquire
          if (istat .ne. 0) then
             write(6, '(/,a,a,i4,/,a,/,a,/)' )
     1                'attempt to open  cfmgmc.input  text file ',
     2                'gives error code = ',istat,
     3                'you need to create such a file.',
     4                'type  help cfm_sngp  for an example.'
          endif
          close(17)
          ikine = 9
          multi_xyz = 0   ! default value (single vertex)

        !===============================================
        else  if (cmd .eq. 'PM_RHIC') then
          write(6,*) ' PM_RHIC (Pat McGaughey) EVENT TYPE SELECTED'
          call KUINQF('dimuon.dat',LUN)
          if (LUN .EQ. -1) then
             write(6, '(/,2A,/)' )
     1                ' Attempt to find dimuon.dat input file  ',
     2                ' was unsuccessful.  Please correct error.'
          endif
          if (LUN.GT.0) then
C           INPUT FILE WAS FOUND TO BE OPEN
             write(6,*)'  dimuon.dat input file was found to be open.'
             write(6,*)'  The file is being closed for re-use'
             close(UNIT=LUN,IOSTAT=ISTAT)
             if (ISTAT.NE.0) then
              write(6, '(/,A,I3,A,I6,/)' )
     1          'Closing the file number ',LUN,
     2          ' gave a return error code ',ISTAT
            endif
            PKINE(8) = -1.0     ! force re-open of file
          endif

          KUIP_EVT = .FALSE.   ! logical switch used in GUEVGEN subroutine
          IKINE = 22
          IPOPSUB = 2
          call KUGETI (IEORMU)     ! e+/e- or mu+/mu - pairs

c         0 or 1 ===> J/PSI into e+,e- or mu+,mu- (Default)
          PKINE(1) = float(IEORMU)

c         Kinematics angular limits controlled by North_South option
c         North_South = 0 means only the North Arm (Default)
c         North_South = 1 means both North and South arms
          ! North/South Arm options
          call KUGETI (NORTH_SOUTH)   
          PKINE(2) = NORTH_SOUTH
          
          ! minimum theta angle for decay particle
          call KUGETR (THMIN)
          PKINE(3) = THMIN   
          
          ! maximum theta angle for decay particle
          call KUGETR (THMAX)
          PKINE(4) = THMAX   
          
          ! minimum total momentum (GeV/c)
          call KUGETR (PMIN)
          PKINE(5) = PMIN
          
          PKINE(6) = 0
          PKINE(7) = 0
          PKINE(8) = -1.0 ! force re-open of file
          multi_xyz = 0   ! default value (single vertex)

        !===============================================
        else  if (cmd .eq. 'AUVENUS') then
          
          write(6,*) ' AUVENUS EVENT TYPE SELECTED'
          KUIP_EVT = .FALSE.   ! logical switch used in GUEVGEN subroutine
          IKINE = 23
          call KUGETI (IPOPSUB)

          !===============================================
        else  if (cmd .eq. 'MCRAP') then
        
          write(6,*) ' MCRAP EVENT TYPE SELECTED (S. Sorensen)'
          KUIP_EVT = .FALSE.   ! logical switch used in GUEVGEN subroutine
          IKINE = 10
          call KUGETI (EVT_MULT)   ! number of particles mxtot
          PMC2(1) = EVT_MULT
          call KUGETI (ISELECT)    ! particle ID
          PMC2(2) = ISELECT
          call KUGETR (PMC2(3))    ! y_min
          call KUGETR (PMC2(4))    ! y_max
          call KUGETR (PMC2(5))    ! pt_min
          call KUGETR (PMC2(6))    ! pt_max
          call KUGETR (PMC2(7))    ! phi_min
          call KUGETR (PMC2(8))    ! phi_max
          call KUGETR (PKINE2(1))  ! temperature
          call KUGETI (IPOPSUB)
          
          !===============================================
        else  if (cmd .eq. 'STPLV2') then
          write(6,*) ' Setting polarization and v2 constants'
          call KUGETR (V2CONST)   ! anisotropy constant
          call KUGETI (IDPOLAR)   ! ID of particle to polarize decay in its rest frame
          call KUGETR (RHO000)    ! spin density matrix value (1/3 means no polarization)
          call KUGETR (PZCONST)   ! polarization constant (not yet developed)
          STPLV2 = 1              ! option to invoke polarization and v2 in GUEVEGEN
          
        !===============================================
        else  if (cmd .eq. 'MCMUM') then
          write(6,*) 'pkevnt - mcmum event type selected'
          kuip_evt = .false.   ! logical switch used in guevgen subroutine
          ikine = 11
          call kugeti (evt_mult)
          pmc2(1) = evt_mult
          call kugeti (iselect)
          pmc2(2) = iselect      ! choice of mu+ or mu- or both
          call kugetr (pmc2(3))
          call kugetr (pmc2(4))
          call kugetr (pmc2(5))
          call kugetr (pmc2(6))
          call kugetr (pmc2(7))
          call kugetr (pmc2(8))
          call kugetr (pmc2(9))
          call kugeti (ipopsub)
 
        !===============================================
        else  if (cmd .eq. 'MCDIM') then
          
          write(6,*) 'pkevnt - mcdim event type selected'
          kuip_evt = .false.   ! logical switch used in guevgen subroutine
          ikine = 12
          call kugeti (evt_mult)
          pmc2(1) = evt_mult
          call kugetr (pmc2(2))
          call kugetr (pmc2(3))
          call kugetr (pmc2(4))
          call kugetr (pmc2(5))
          call kugeti (ipopsub)

        else  if (cmd .eq. 'RV_PHI') then
          write(6,*) ' RV_PHI EVENT TYPE SELECTED'
          EVENT_CODE = RV_PHI
          KUIP_EVT = .FALSE.   ! logical switch used in GUEVGEN subroutine
          IKINE = 13
          IPOPSUB = 2
          call KUGETI (IEORMU)
          PKINE(1) = float(IEORMU)
          call KUGETI (ISELECT)
          if (ISELECT.EQ.3) then
             LOGACC = .TRUE.
          else
             LOGACC = .FALSE.    ! redundant
          endif
          PKINE(2) = float(ISELECT)
          call KUGETR (THMIN)
          PKINE(3) = THMIN   ! minimum theta angle for decay particle
          call KUGETR (THMAX)
          PKINE(4) = THMAX   ! maximum theta angle for decay particle
          call KUGETR (PMIN)
          PKINE(5) = PMIN    ! minimum total momentum (GeV/c)
          call KUGETR (YY_MIN) ! minimum rapidity for Phi meson
          PKINE(6) = YY_MIN
          call KUGETR (YY_MAX) ! maximum rapdiity for Phi meson
          PKINE(7) = YY_MAX
          call KUGETI (NUM_RV)  ! number of mesons requested
          PKINE(8) = NUM_RV
          PKINE(9) = -1.0    ! forces reset of RV_PHI initialization
          MULTI_XYZ = 0   ! DEFAULT VALUE (SINGLE VERTEX)

        else  if (cmd .eq. 'SNGL_PHI') then
          write(6,*) ' SNGL_PHI EVENT TYPE SELECTED'
          EVENT_CODE = SNGL_PHI
          KUIP_EVT = .FALSE.   ! logical switch used in GUEVGEN subroutine
          IKINE = 28
          IPOPSUB = 2
          call KUGETI (IEORMU)
          PKINE(1) = float(IEORMU)
          call KUGETI (ISELECT)
          if (ISELECT.EQ.3) then
             LOGACC = .TRUE.
          else
             LOGACC = .FALSE.    ! redundant
          endif
          IKINE2(2) = float(ISELECT)
          call KUGETR (YY_MIN) ! minimum rapidity for Phi meson
          PKINE2(1) = YY_MIN
          call KUGETR (YY_MAX) ! maximum rapdiity for Phi meson
          PKINE2(2) = YY_MAX
          call KUGETR (PT_MIN) ! minimum pt for Phi meson
          PKINE2(3) = PT_MIN
          call KUGETR (PT_MAX) ! maximum pt for Phi meson
          PKINE2(4) = PT_MAX
          call KUGETR (PHIMIN)
          PKINE2(5) = PHIMIN   ! minimum Phi angle for decay particle
          call KUGETR (PHIMAX)
          PKINE2(6) = PHIMAX   ! maximum Phi angle for decay particle
          call KUGETR (THMIN)
          PKINE2(7) = THMIN   ! minimum theta angle for decay particle
          call KUGETR (THMAX)
          PKINE2(8) = THMAX   ! maximum theta angle for decay particle
          call KUGETR (PMIN)
          PKINE2(9) = PMIN    ! minimum momentum (GeV/c) for decay particle
          PKINE2(10) = -1.0    ! forces reset of RV_PHI initialization
          MULTI_XYZ = 0   ! DEFAULT VALUE (SINGLE VERTEX)

        else  if (cmd .eq. 'SNGL_NEUTRAL') then
          write(6,*) ' SNGL_NEUTRAL EVENT TYPE SELECTED'
          EVENT_CODE = SNGL_NEUTRAL
          KUIP_EVT = .FALSE.   ! logical switch used in GUEVGEN subroutine
          IKINE = 32
          IPOPSUB = 2
          call KUGETI (IPIORETA)
          if (IPIORETA.NE.7 .AND. IPIORETA.NE.17) then
            stop ' PKEVENT <F>: Invalid choice of IPIORETA '
          endif  ! safety check on Pizero or Eta choice
          IKINE2(1) = IPIORETA
          call KUGETI (ISELECT)
          if (ISELECT.EQ.4) then
             LOGACC = .TRUE.
          else
             LOGACC = .FALSE.    ! redundant
          endif
          IKINE2(2) = ISELECT
          call KUGETR (YY_MIN) ! minimum rapidity for Neutral meson
          PKINE2(1) = YY_MIN
          call KUGETR (YY_MAX) ! maximum rapdiity for Neutral meson
          PKINE2(2) = YY_MAX
          call KUGETR (PT_MIN) ! minimum pt for Neutral meson
          PKINE2(3) = PT_MIN
          call KUGETR (PT_MAX) ! maximum pt for Neutral meson
          PKINE2(4) = PT_MAX
          call KUGETR (PHIMIN)
          PKINE2(5) = PHIMIN   ! minimum phi angle for decay particle
          call KUGETR (PHIMAX)
          PKINE2(6) = PHIMAX   ! maximum phi angle for decay particle
          call KUGETR (THMIN)
          PKINE2(7) = THMIN   ! minimum theta angle for decay particle
          call KUGETR (THMAX)
          PKINE2(8) = THMAX   ! maximum theta angle for decay particle
          call KUGETR (PMIN)
          PKINE2(9) = PMIN    ! minimum momentum (GeV/c) for decay particle
          PKINE2(10) = -1.0    ! forces reset of RV_NEUTRAL initialization
          call KUGETR (PKINE2(11)) ! Z width variation
          call KUGETI (IKINE2(3))  ! number of million pre-calls to GRNDM
          MULTI_XYZ = 0   ! DEFAULT VALUE (SINGLE VERTEX)

        else  if (cmd .eq. 'TEXT_FILE') then
          
          write(6,*) 'pkevnt - text_file event type selected'
          
          ! try load input file from command line
          call kugetf(cdmci_file,lengc)
          write(*,*) 'pkevnt - input file: ', CDMCI_FILE

          event_code = text_file
          
          ! logical switch used in guevgen subroutine
          kuip_evt = .false.   
          ikine = 29
          
          ! used for resetting the event vertex
          call kugetr(phconvposition)
          if (phConvPosition.gt.0.0) then
            write(6,555)phConvPosition
 555        format( 'pkevnt - OSCAR.TXT events will have their ',
     +        'Z position reset using a Converter ',
     +        'radial position at ', f10.6, ' cm',//)
          endif
                    
          call kugeti(ipopsub)
          MULTI_XYZ = 0   ! DEFAULT VALUE (SINGLE VERTEX)
      
        ! --------------------------------------------
        ! rv_chi input      
        else  if (cmd .eq. 'RV_CHI') then
          write(6,*) ' RV_CHI EVENT TYPE SELECTED'
          EVENT_CODE = RV_CHI
          KUIP_EVT = .FALSE.   ! logical switch used in GUEVGEN subroutine
          IKINE = 25
          IPOPSUB = 90
          call KUGETI (IEORMU)
          PKINE(1) = float(IEORMU)
          call KUGETI (ISELECT)
          if (ISELECT.EQ.3) then
             LOGACC = .TRUE.
          else
             LOGACC = .FALSE.    ! redundant
          endif
          PKINE(2) = float(ISELECT)
          call KUGETR (THMIN)
          PKINE(3) = THMIN   ! minimum theta angle for decay particle
          call KUGETR (THMAX)
          PKINE(4) = THMAX   ! maximum theta angle for decay particle
          call KUGETR (PMIN)
          PKINE(5) = PMIN    ! minimum total momentum (GeV/c)
          call KUGETR (YY_MIN) ! minimum rapidity for CHI meson
          PKINE(6) = YY_MIN
          call KUGETR (YY_MAX) ! maximum rapdiity for CHI meson
          PKINE(7) = YY_MAX
          call KUGETI (NUM_RV)  ! number of mesons requested
          PKINE(8) = NUM_RV
          PKINE(9) = -1.0    ! forces reset of RV_CHI initialization
          MULTI_XYZ = 0   ! DEFAULT VALUE (SINGLE VERTEX)

        else  if (cmd .eq. 'QED_EE') then
C*****************************************************
c          set kine variable switch
          ! use default value
          IPOPSUB = 100      
          IKINE = 16
          KUIP_EVT=.false.
c         get parameters....see documentation for desc.
          call KUGETI (ISTATX)     !ISTATX CONTROL WORD
          KEVT_PAR(1)=REAL(ISTATX)
          call KUGETR (IMP)     !IMPACT PARAMTER CONTROL...FORCE
          KEVT_PAR(2)=IMP
          call KUGETR (INVM)    !INVARIANT MASS CONTROL...FORCE
          KEVT_PAR(3)=INVM
          call KUGETR (RAPID)    !RAPIDITY CONTROL...FORCE
          KEVT_PAR(4)=RAPID

c         finish qed e+e- section.
        else  if (CMD.eq.'BEAM_GAS') then
          write (6,*) 
     +           '  Beam Gas simulator (Paul Kirk, LSU) requested'
           kuip_evt = .false.
           call KUGETI (IPOPSUB)
           call KUGETI (HFILE)
           KEVT_PAR(1) = HFILE    ! flag for Hydrogen file
           call KUGETI (HEFILE)
           KEVT_PAR(2) = HEFILE   ! flag for Helium file
           call KUGETI (NFILE)
           KEVT_PAR(3) = NFILE    ! flag for Nitrogen file
           call KUGETI (TRUE_DIS)
           KEVT_PAR(4) = TRUE_DIS ! flag for true distributions
           ikine = 17

c          finish Paul Kirk's generator

        else  if (cmd .eq. 'XY_RANDOM') then
          IXY_CHANGE = 0     ! set notification switch
          write(6,*) ' XY Randomization Change Requested'
          call KUGETI (IXY_RNDM)
          if (IXY_RNDM.EQ.0) then
             write(6,*)' There will be NO randomization in XY'
          endif
          if (IXY_RNDM.EQ.1) then
             write(6,*)' There will one XY randomization',
     1                 ' on a full event'
          endif
          if (IXY_RNDM.EQ.2) then
             write(6,*)' There will a different XY randomization',
     1                 ' for each particle'
          endif

       else  if (cmd .eq. 'STAGE') then
        call KUGETI(NSTAGE)
        call KUGETR(ZSTAGE)
        call KUGETR(PSTAGE)
        call KUGETI(FILTER)
        if (NSTAGE.ne.9) then
           write(6,447)NSTAGE, ZSTAGE, PSTAGE, FILTER
 447       format(/,3x,'Staged simulation number ', i3,
     +          /,5x,'Z boundary for this stage = ', f6.1,
     +          ' cm,  and mimimum momentum = ', f8.3, ' GeV/c',
     +          ', Filter = ', i2)
        else
           write(6,448)
 448       format(//, ' No more staged output files ',//)
        endif

        call openoscarfile(NSTAGE, ZSTAGE)

        if (NSTAGE.eq.9) then
           NSTAGE = 0
        endif                   !  indicate no output file for this stage

        ieorun = 0
        ieotri = 0

        return

      else  if (cmd .eq. 'MUIWR') then
         call KUGETI(MUIDwrite)
         write(6,450)MUIDwrite
 450     format(//,'  MuIDWrite Flag enabled.',
     +            '  MuIDLayer = ', i2)
         return

      else  if (cmd .eq. 'STACK_STAGE') then
         call KUGETI(NSTAGE)
         call KUGETR(ZSTAGE)
         call KUGETR(PSTAGE)
         call KUGETI(NCLONE)
         call KUGETI(FILTER)
         if (NSTAGE.eq.0) then

c        Indicates the final stage trigger condition

            finalFilter = filter
            write(6,451)filter
 451        format(//,' Final stage trigger filter condition', i5,/)
         endif
         if (NSTAGE.ne.0.and.NSTAGE.ne.NSTAGESTACK+1) then
           write(6,452)NSTAGE
 452       format(//,' PKEVENT fatal error:',
     +            ' NSTAGE on STACK_STAGE is out of order',
     +            2x,i5)
           stop ' PISA stops due to this error'
         endif
         if (NSTAGE.eq.1) then
            call openoscarfile(NSTAGE, ZSTAGE)
         endif
         if (NSTAGE.gt.1) then

c        Check that the Z values are in increasing order

            if (ZSTAGE.le.ZSTAGESTACK(NSTAGE-1)) then
               write(6,453)ZSTAGE, ZSTAGESTACK(NSTAGE-1)
 453           format(//, ' PKEVENT fatal error:',
     +         ' Successive stage Z values not increasing?? ',
     +         2f8.1)
               stop ' PISA stops due to this error'
            endif
         endif
         if (NSTAGE.gt.0) then
            NSTAGESTACK = NSTAGE
            ZSTAGESTACK(NSTAGE) = ZSTAGE
            PSTAGESTACK(NSTAGE) = PSTAGE
            FILTERSTACK(NSTAGE) = FILTER
            NCLONESTACK(NSTAGE) = NCLONE
            write(6,454)NSTAGE, ZSTAGE, PSTAGE, NCLONE, FILTER
 454        format(/,3x,'Stack-staged simulation number ', i3,
     +           /,5x,'Z boundary for this stage = ', f6.1,
     +           ' cm,  and mimimum momentum = ', f8.3, ' GeV/c',
     +           /,5x,'Number of clones for this stage = ', i3,
     +           ', Filter = ', i2)
         endif

      else  if (cmd .eq. 'NSKIP') then
         call KUGETI(ISTART)    !  start event in NTUPLE
         IKINE2(1) = ISTART     !  
         write(6,455) ISTART
 455     format(//,'  ISTART set.',
     +        '  NumSkip = ', i3)
         return
         
      end if
 
 9999 CONTINUE
      Return
      end
