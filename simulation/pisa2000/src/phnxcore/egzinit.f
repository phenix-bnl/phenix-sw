        Integer function egzinit ()

c        UNIX Version of code

c        Routine to initialize Zebra structures for Monte Carlo event
c        generators HIJET, VENUS, FRITIOF, LUCIAE, RQMD, and HIJING

c        called by:
c            event_filter
c        calls:
c           fzfile,FZLOGL,mzdiv,mzlink,mzstor,mzlogl
c           egzpar
c           eg_read_code_bank
c           read_run_bank_hijet
c           read_run_bank_venus
c           read_run_bank_fritiof
c           read_run_bank_luciae
c           def_event_form


c  ZEBRA must have been initialzied outside egzinit !!
c  ===================================================
 
        implicit none

c        Global Declarations
c        ===================

#include "guphnx.inc"
#include "evtzebra.inc"
#include "event.inc"
#include "evntcode.inc"
#include "quest.inc"

c        External Declaration
c        ====================

        integer eg_read_code_bank
        integer write_code_bank
        integer write_run_bank
        integer read_run_bank_hijet
        integer read_run_bank_venus
        integer read_run_bank_fritiof
        integer read_run_bank_luciae
        integer read_run_bank_hijing
        integer read_run_bank_rqmd
        integer read_run_bank_vni

c        Local Declarations
c        ==================

        integer*4 div
        integer*4 istat, LENFIL
        integer*4 ifirst /0/
        save lenfil

c     Revision History

c         March 18, 1996  C.F. Maguire   Change name of ZSTORE common block
c                                        to ZSTOREB because of new IBM error
c         April 19, 1996  C.F. Maguire   Re-change name to EZSTOR: IBM and
c                                        HP have mutual incompatibilities !*&!
c         May 01 1996     C.F. Maguire   Add in LUCIAE capability
c         May 30 1996     C.F. Maguire   Change to multi-event capability
c         Mar 31 1997     C.F. Maguire   Change to CFOPEN exchange mode files
c                                        Only exchange mode files will be used
c                                        from now on !  It's too complicated to
c                                        allow native modes also.
c         May 05 1997     C.F. Maguire   Hiroshima group needs backward
c                                        compatibility option for native mode
c                                        files. So allow this complication.


c        Executable code
c        ===============

        egzinit = -1        ! assume success
        if(ifirst.eq.1)then

c     IFIRST = 1 indicates a forced re-open from PKEVNT KUIP handler
c     Z_LUN_IN is carried in common block

c     Close and re-open with FZENDI call

          call fzendi(z_lun_in,'T')

c          open(unit=z_lun_in,file=cdmci_file(1:LENFIL),
c     &    status='old',form='unformatted')


c     now read header records

          istat = eg_read_code_bank(6)   ! finds run, reads first bank (CODE)
          if (istat.ne.-1) then        ! error
             egzinit = 0
             goto 900   ! execute return
          end if  ! check on ISTAT after first read
          if (event_code.eq.hijet) then
            istat = read_run_bank_hijet(6)
          else if (event_code.eq.venus) then
            istat = read_run_bank_venus(6)
          else if (event_code.eq.fritiof) then
            istat = read_run_bank_fritiof(6)
          else if (event_code.eq.luciae) then
            istat = read_run_bank_luciae(6)
          else if (event_code.eq.hijing) then
            istat = read_run_bank_hijing(6)
          else if (event_code.eq.rqmd) then
            istat = read_run_bank_rqmd(6)
          else if (event_code.eq.vni) then
            istat = read_run_bank_vni(6)
          else
            write(6,*)' EGZINIT <E> Unknown event generator:',
     1                event_code
            egzinit = 0
            goto 900  ! execute return
          end if
          go to 900   ! execute return
        endif  ! check on forced re-open
        ifirst = 1


        div_label(code_div)  = 'CODE '
        div_label(run_div)   = 'RUN '
        div_label(event_div) = 'EVENT '

        div_nw(code_div)     = 100
        div_nw(run_div)      = 100
        div_nw(event_div)    = 360000

        div_nw_max(code_div) = 100
        div_nw_max(run_div)  = 100
        div_nw_max(event_div)= 370000

        div_opt(code_div)  = 'CL'
        div_opt(run_div)   = 'CL'
        div_opt(event_div) = 'CS'

        max_down_links(code_div)  = 1
        max_down_links(run_div)   = 1
        max_down_links(event_div) = 1

        start_link(code_div)  = 1
        end_link(code_div)    = 1
        start_link(run_div)   = 2
        end_link(run_div)     = 2
        start_link(event_div) = 3
        end_link(event_div)   = 3

        zebra_integer_flag    = 2
        zebra_zero_bank       = 0      ! zero the bank before starting
        zebra_stand_alone_bank = 2
        zebra_common_name     = '/zstore/'
        div_links_common      = 'div_lnk'

        top_num_links(code_div) =  0
        top_num_str_links(code_div) = 0
        top_num_links(run_div) =  0
        top_num_str_links(run_div) = 0
        top_num_links(EVENT_div) =  0
        top_num_str_links(EVENT_div) = 0

c  Get additional zebra parameters from EGZPAR.PAR

        call egzpar (6)

C       Create divisions

        call mzstor(zstore_index,'/XSTORE/','Q',
     >    fence,top_link_addr,
     >      lref,zstore,zstore(5000),endzs)
        call mzlogl(zstore_index, -2)
 
        do div =1,max_zebra_divisions
          call mzdiv(zstore_index,div_index(div),div_label(div),
     >      div_nw(div),div_nw_max(div),div_opt(div))

! create link area for each div with only structural links

          call mzlink(zstore_index,div_links_common,
     >        link_addr(start_link(div)),link_addr(end_link(div)),
     >        link_addr(start_link(div)) )

        end do

c  Now handle reading or writing initialization

c        If reading then
c                - open file for read
c                - read in ZEBRA run header
c                - read in first bank (code) and set generator flag, load
c                  common
c                - read in generator dependant run devision, load common
c                  set single or double precision flag based on IHEADER(2)
c                - exit to event loop.
c        If writing then
c                - open file for write
c                - write ZEBRA run header
c                - setup MZFORM for CODE bank, load and write bank
c                - setup generator dependant MZFORM for RUN bank, load and
c                  write with user single/double precision flag
c                  set as needed (IHEADER(2))
c                - setup MZFORM for EVENT bank
c                - exit to event loop

c--------------------------------------------------------------------------

        if (z_input) then

c     CFM: New version with CFOPEN gets the file number assigned internally


          LENFIL = index(cdmci_file,' ')
          write(6,*)' length = ',LENFIL
          LENFIL = LENFIL - 1
          if(fz_option_in.eq.'I'.or.fz_option_in.eq.'TI'.or.
     +       fz_option_in.eq.'TXI')then
             z_lun_in = lun_dmci
             open(unit=z_lun_in,file=cdmci_file(1:LENFIL),
     &            status='old',form='unformatted',err=790)
          else
             call cfopen(z_lun_in,0, 0, 'r',0,
     +                  cdmci_file(1:lenfil),istat)
             iquest(1) = z_lun_in
             if(istat.ne.0)go to 790
          endif  ! check on backward compatibility
          go to 792
790       continue
          write(6,791)z_lun_in,cdmci_file
791       format(/,3x,'EGZINIT <E>: File number ',i4,'  Name ',a,
     1           '  open error')
          stop 'Unable to open the ZEBRA input data file'
792       continue

          call fzfile(z_lun_in,0,fz_option_in)
          CALL FZLOGL(Z_LUN_IN, z_fzlogl)         ! Log= 2 lots of messages

          istat = eg_read_code_bank(6)   ! finds run, reads first bank (CODE)
          if (istat.ne.-1) then        ! error
            egzinit = 0
            goto 900
          end if

c  Generator dependent RUN bank

          if (event_code.eq.hijet) then
            istat = read_run_bank_hijet(6)
          else if (event_code.eq.venus) then
            istat = read_run_bank_venus(6)
          else if (event_code.eq.fritiof) then
            istat = read_run_bank_fritiof(6)
          else if (event_code.eq.luciae) then
            istat = read_run_bank_luciae(6)
          else if (event_code.eq.luciae) then
            istat = read_run_bank_luciae(6)
          else if (event_code.eq.hijing) then
            istat = read_run_bank_hijing(6)
          else if (event_code.eq.rqmd) then
            istat = read_run_bank_rqmd(6)
          else if (event_code.eq.vni) then
            istat = read_run_bank_vni(6)
          else
            write(6,*)'  EGZINIT <E> Unknown event generator:',
     +                event_code
            egzinit = 0
            goto 900
          end if

          call def_event_form
          if (istat.ne.-1) then        ! error reading run bank
            egzinit = 0
            goto 900
          end if
        else

c  Now initilization for writing

          z_lun_out = 37

          Open ( UNIT=z_lun_out, File='tape1',
     &         FORM='UNFORMATTED',
     &               Status='UNKNOWN')

          call fzfile(z_lun_out,z_blocksize/4,z_fz_option_out)
          CALL FZLOGL(Z_LUN_OUT, z_fzlogl)        ! Log= 2 lots of messages

          call fzrun(z_lun_out,0,0,0)

          istat = write_code_bank(6) ! writes CODE bank
          if (istat.ne.-1) then  ! error
            egzinit = 0
            goto 900
          end if

c   Now write out generator dependent RUN bank


          istat = write_run_bank(6)
          if (istat.ne.-1) then  ! error writing run bank
            egzinit = 0
            goto 900
          end if

c   Now define event bank form

          call def_event_form
        end if

 900        continue
        return
        end
