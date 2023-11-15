c     $Id: guinit.f,v 1.14 2008/11/25 21:14:23 hpereira Exp $
c     This routine should perform all the necessary initializations
C     Opens the data card file (default gffgo.dat), calls the generic
C     Geant initialization routines GINIT,GZINIT etc., gets the input
C     data structure cards (if any) , ouput data structures, sees which
C     detector sets are on, and reads in the Steering keys (tells Pisa
C     to do tracking, digitization etc, or to not do them.

      subroutine guinit
      implicit none

#include "gcbank.inc"
#include "gccuts.inc"
#include "gcflag.inc"
#include "gckine.inc"
#include "gclist.inc"
#include "gcunit.inc"
#include "gcphys.inc"
#include "guphnx.inc"
#include "udst.inc"
#include "quest.inc"
#include "guevgen.inc"
#include "subevt.inc"
#include "zunit.inc"
#include "gactout.inc"

C  Local Variable definitions
      byte bgffgo
      character*1  ans, cx
      character*63 gffgo_file, gffgo_in
      equivalence (bgffgo, gffgo_in)
      integer*4 ident, ier
      integer*4 lin_save, newlun
      integer*4 nset, iset
      integer i, istat, j
      integer icall /0/
      logical init_ok, inp_opened, out_opened
      integer lss,lslash
      character*80 pisaout_file
      character*80 pisaout_cmd
      data gffgo_file /'gffgo.dat'/
      data  inp_opened/.false./, out_opened/.false./

      ! for in-line compression of zebra output
      external write_compress        

c     Executable code:
      if(icall.eq.0) then
        icall = 1
        init_ok = .false.

c       Initialize GEANT variables
        call ginit
        call g_kdef
        call kuexec('exec pisa')
        
C       able to read a new LUN from it, redirect all geant-realted
C       output to the LUN # in that file -- opening a file for
C       the output  (JPSullivan added this to minimize terminal
C       IO which is usually of no interest, Oct 5, 1993).
C       LOUT controls Geant-related output,
C       IQPRNT...IQTYPE are used to control Zebra-related output,
C       call HOUTPU directs hbook output

        open ( unit=1,file='mylout.dat',status='old',form='formatted',
     x    err=1900 )

        read ( 1,*, err=1900 ) newlun
        close ( unit=1 )
        
        if ( newlun.gt.1 .and. newlun.ne.5  .and. newlun.ne.11
     x    .and. newlun.ne.12 .and. newlun.ne.13
     x    .and. newlun.ne.14 .and. newlun.ne.15 ) then
          write ( 6,1910 ) newlun
1910      format ( 'guinit - newlun =',i5,' read from mylout.dat')
          open ( unit=newlun,file='mypisa.out',status='unknown',
     x      form='formatted',err=1900 )
          lout   = newlun
          iqprnt = newlun
          iqpr2  = newlun
          iqlog  = newlun
          iqtype = newlun
          call houtpu ( newlun )
        else
          write ( lout,1920 ) newlun
1920      format ( 'guinit - did not change lout value because',
     x      ' requested value conflicts with existing',
     x      ' luns'/'        requested value=',i10)
        endif
         
1900    continue
        
c       Initialize data structures
        call gzinit
        call gdinit
      endif

c     define initial event flags and contants
      kuip_evt = .false.
      end_evtflg = .true.
      numevt = 0
      ixy_rndm = 0    ! default as no XY randomization of momentum
      ixy_change = 1  ! no notification
      t0start = 0.0   ! initialize start time (TOFG) kept in subevt.inc

C     default  histogram and parameter file names
      chbk_file = ' '
      cdout_file = out_file
      cout_keys(1)= fdigi
      cste_keys(1)=stee1
      cste_keys(2)=stee2
      cste_keys(3)=stee3
      cste_keys(4)=stee4
            
c     Read data cards
      iu_ini_par = 0
      write(6,'(a,$)')' Read Data cards from terminal ? (y/N): '
      read(5,'(a1)')ans
      call cltou(ans) 
      if( ans.ne.'Y' ) then
        
        lin_save = LIN
        LIN = 1
        call ffset('LINP',LIN)  ! Set LIN to 1 for read

c       get file name for data cards, if <CR> then reads GFFGO_xxxx.DAT

        write(6,'(a,/,a,a)')
     &    ' Enter input data-card filename',
     &    ' <CR> for default: ',gffgo_file
        read(5,'(a)') gffgo_in
        
        if (bgffgo.ne.32.and.bgffgo.ne.48) gffgo_file = gffgo_in
        open(unit=1,file=gffgo_file,status='OLD',err=197)
        
        write(  lout,'(2x,a,a,/)')'Using data card file ',gffgo_in
        go to 198
197     continue
        
        write(6,196)gffgo_file
196     format(3x,'guinit - Unable to find file ',a)
        stop '  Missing GFFGO.DAT ?'
198     continue
        
        call gffgo
        close(unit=1)
        
        LIN = lin_save
        call ffset('LINP',LIN)  ! Restore LIN
      else
        call gffgo
      endif

      ! set exchange format for IO
      if(iswit(1).ne.0) cx='X'
      
c     open the histogram file and parameter file
c     open the direct access file to store the histograms

c     parameters:  lun,topdir,filename,status,lrec,iostat
c     topdir is the top directory name
c     status = 'n'=new, or, 'u'=update, or ' '=readonly
c     lrec = 1024 recommend for record length in words

c     put a zero byte after the last non-blank character
c     or at character 80 if no blanks - mjl 10/23/94
      i=1
      do while (i.lt.80 .and. chbk_file(i:i).ne.' ')
        i=i+1
      enddo
      
      write(*,*) 'guinit - checking hbk file'
      if(i.gt.1) then
        chbk_file(i:i)=char(0)
      else
        chbk_file = 'gintphnx.hbk'
      endif
      
      call hropen(13,'GEANHIST',chbk_file,'n',1024,istat)
      if(ISTAT.NE.0) then
        write(6, 199 ) 'cannot open file ',chbk_file
      else
        write( 6, 199 ) 'chbk_file: ',chbk_file
      endif
 199  format( ' guinit - ',a,a );
        
C     count input structures
      nin_keys = 0
            
      ! set default
      do_trak  = 1         
      do i=1,10
        if(abs(in_keys(i)).gt.1 ) nin_keys = nin_keys + 1
        
        ! cancel call to evgen
        if(cin_keys(i).eq.'KINE') ikine = 0       
        
        ! no traking necessary
        if( cin_keys(i).eq.'HITS'
     1    .or. cin_keys(i).eq.'JXYZ') do_trak = 0   
      enddo
      
      if(nin_keys.gt.0) then
        
        do i=1,10
          if(cin_keys(i).eq.'INIT') nin_keys = nin_keys - 1
        enddo
        
        if (inp_opened) then
          call fzendi(lun_dinp,'T')
          close (lun_dinp)
        endif
        
        open (lun_dinp, file=cdinp_file, status='OLD',
     1    form='UNFORMATTED')
              
        call gopen(lun_dinp,cx//'I',0,ier)   ! open input  file
        inp_opened = .true.
        call gfin(lun_dinp,'INIT',1,0,' ',ier)
        
C  to   read in any parameter banks PARA and PARU (see Zebra.Doc)
        call e_get_dst
        if (ident.ne.0) then
          write(6,*)'guinit - ',
     x      'NO INIT structure on data input file '
       endif
      endif

c     CFM: Save the cut thresholds (used in EMCal software)

      gcuts(1) = cutgam
      gcuts(2) = cutele
      gcuts(3) = cutneu
      gcuts(4) = cuthad
      gcuts(5) = cutmuo

c     create or update INIT files

      ! reinitialize according to data cards
      if(LGEOM(1).NE.0) then 
        call gidrop      ! drop old structures
        call gmate       ! define standard materials
        call gpart       ! define standard particles
        write(lout,'(/,2x,2a,/)')
     1    'Call to GUADDPARTS for special PISA/PHENIX ',
     2    'particle ID code'
        call guaddparts  ! some more particles (special for PISA/PHENIX)
        call gugeom      ! define geometry
        init_ok = .true. ! ok to output INIT bank if need be
      endif
      
      call gphysi       ! initialize GEANT physics processes (always)
      if(LGEOM(4).NE.0) then
        call gpmate(0) ! print for debugging purposes
        call gptmed(0) ! print for debugging purposes
        call gpvolu(0) ! print for debugging purposes
        call gpsets('*','*')! print for debugging purposes
        call gppart(0) ! print for debugging purposes
      endif

C     set 'set' pointers to volumes

      if(init_ok) then
        
        if(JSET .EQ. 0) then
          NSET = 0
        else
          NSET = IQ(JSET-1)
        endif
        
        write(lout,997)nset
997     format('guinit - number of detector sets initialized = ',i4)
        
        do iset = 1, nset
          do i=1,phnx_dvol
            if(ivol(i).eq.IQ(JSET+ISET)) p_volu(iset) = i ! store
          enddo
        enddo
        
        do iset = 1, nset
          if(p_volu(iset).eq.0) then          
            write(lout,'(a,a4,a)')
     1        'guinit - Set ',iq(jset+iset),' not recognized'
            p_volu(iset) = 10            ! fake entry
            write(lout,998)nset,phnx_dvol,iset,jset
998         format(2x,'nset,phnx_dvol,iset,jset',4i10)
          endif
        enddo
        
      endif

C     count steering parameters
C     these parameters are new in order to separate the path PISA takes
C     (tracking, digitization etc.) from the data structures that are written out
      nste_keys = 0
      
      do i=1,10
        if(abs(ste_keys(i)).gt.1) nste_keys = nste_keys + 1
        if(cste_keys(i).eq.'DIGI') do_digi = 1   ! set output flag
      enddo
      
      if(do_trak.eq.1) then
        do_trak=0
        do i=1,10
          if(cste_keys(i).eq.'JXYZ') do_jxyz = 1  ! set output flag
          if(cste_keys(i).eq.'JXYZ' .or.
     1      cste_keys(i).eq.'HITS') do_trak = 1! set tracking flag
        enddo
      endif

C     count output structures
C     these parameters now specify which of Geant data structures will get
C     written to the output file (CDOUT_FILE) after every event. Their old
C     function to also steer the path of the program through the do_%%%% variables
C     has been moved to the CSTE_KEY
      nout_keys = 0
      do i=1,10
        if(abs(out_keys(i)).gt.1) nout_keys = nout_keys + 1
      enddo
      
      ! no output file specified
      if(cdout_file(1:1).eq.' ') then     
        nout_keys=0
        write(lout,*) 'guinit - No output file opened! '
      endif
      
      if(nout_keys.gt.0) then
        if (out_opened) then
          call fzendi(lun_dout,'T')
          close (lun_dout)
        endif  ! check on file already opened
        
c       put a zero byte after the last non-blank character
c       or at character 80 if no blanks - mjl 10/23/94
        i=1
        do while (i.lt.80 .and. cdout_file(i:i).ne.' ')
          i=i+1
        enddo
        cdout_file(i:i)=char(0)

c       Protection against over-writing existing data file
c      This is to make sure we don't lose any MDC files
        call kuinqf(cdout_file, istat)
        if(istat.ne.-1) then
          write(6,999)cdout_file
999       format(/,'  The PISA hits output file which you have',
     +      ' specified as ',//,4x,a70,//,
     +      '  already exists.  PISA will not let you overwrite',/,
     +      '  an existing, possibly valuable PISA hits file.',/,
     +      '  You must rename or remove the file yourself',/)
          stop '  Apologies but PISA must stop as a safety measure' 
        endif
        zebra_output = 0
        
        ! exchange format
        if(cx.eq.'X') then        
          
          if(iswit(1).eq.1) then
            
            zebra_output = 1
            call cfopen(lun_dout,0, 0, 'w',0,
     +        cdout_file,istat)
            iquest(1) = lun_dout
            call gopen(lun_dout,'XODL',0,ier)  ! uncompressed output file
            write(6,*)'guinit - output file is EXCHANGE mode ', 
     +        'UNCOMPRESSED'
            
          else
            
            if(iswit(1).eq.2.or.iswit(1).eq.4.or.
     +        iswit(1).eq.6.or.iswit(1).eq.8) then
                
              ! pick a number here
              lun_dout = 11                      
              zebra_output = 1
              iquest(1) = lun_dout
              call lzoutfile(cdout_file)

c             According to the ZEBRA manual 900 is the default size for exchange
c             format, but any multiple of 30 is allowed.  Martin P. said to use
c             15360.  However, tests with his software show that 9990 is the maximum
c             posible size.  Checks with 12000, 13500, 15000, and 15360 all failed
c             on read back mode.  Possibly a Pro problem but the cause does not matter.

c             Decide to use 9990 for the buffer size

              call fzfile(lun_dout, 9990, 'XOC') ! max possible size on PPro?
              call fzhook(lun_dout, write_compress, 0)  ! hook to channel mode
              
              ! assume success for BUDST_OOPEN assignment
              ier = 0   
              write(6,*)'guinit - output file is EXCHANGE mode ',
     +          'COMPRESSED'
              
            ! check if ISWIT(1) = 2 or 4 or 6 or 8 (3, 5, 7 for ROOT file only)  
            endif
            
           ! check if normal or compressed output
          endif 
          
        ! native mode
        else
          
          open(lun_dout,file=cdout_file,status='NEW',
     1      form='UNFORMATTED')
            
          write(6,*)'guinit - output file is NATIVE'
          zebra_output = 1
          
          ! open output file
          call gopen(lun_dout,'O',0,ier)  
          
        endif

        if(zebra_output.eq.0) then
          budst_oopen = .false.
        else
          if(ier.ne.0) then
            write(6,123)ier,lun_dout,iswit(1)
123         format(/,'guinit - IER = ',i4, ',  lun_dout = ',i4,
     +        ',  iswit(1) = ' ,i4)
          endif
          BUDST_OOPEN = IER .EQ. 0     !  out file is opened OK
        endif
              
        if(BUDST_OOPEN) then
          
          write(*,*) 'guinit - writing geometry to ',
     +      'geometry.dat'
            
          !output INIT structures
          ! make an rz format file for use by root geometry
          ! make it possible to save larger rz file (default is 4MB)
          ident = 0   
          
          IQUEST(10) = 5000
          call GRFILE(21,'geometry.dat','NQ')
          call GROUT('VOLU',1,' ')
          call GROUT('MATE',1,' ')
          call GROUT('TMED',1,' ')
          call GROUT('ROTM',1,' ')
          call GROUT('SETS',1,' ')
                
          ! make a cZ format file:
          call GFOUT(lun_dout, 'INIT', 1, 0, ' ', ier)
          
          ! check status
          if(IER .EQ. -1) then
            write(6,*)'guinit - GFOUT wrote no output?',
     &        ' during INIT save'
          else if(IER .GT. 0) then
            write(6,*)'guinit - GFOUT returned IER = ',IER,
     &        ' during INIT save'
          else if(IER .EQ. 0) then
            write(6,*)'guinit - GFOUT O.K., returned IER = ',IER
          endif   
          
        else
          if(zebra_output.eq.1) then
            write(6,*)'guinit - no output file for geometry?'
          endif
        endif  ! check on budst_oopen
      endif  ! check on nout_keys > 0
            
      return
              
      end

c     External compress write interface

      subroutine write_compress (ibuf, ioway)
      implicit none

      integer ibuf(8), ioway, iquest, nw
      common /quest/ iquest(100)

      call compwrite(ibuf, iquest, nw)
      return
      end
