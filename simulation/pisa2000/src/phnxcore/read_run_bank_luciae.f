*CMZ :  2.04/00 01/06/93  15.44.53  by  Charles F. Maguire
*CMZ :  2.01/00 08/10/92  22.28.29  by  Charles F. Maguire
*-- Author :    Charles F. Maguire   08/10/92

c   Routines to read and write the generator dependant RUN banks

c        Routines:
c                read_run_bank_hijet        readjet
c                read_run_bank_venus        readnus
c                read_run_bank_fritiof      readiof
c                read_run_bank_luciae       readiae
c       called by:
c           egzinit 
c       call:
c           FZIN,mzwipe


c----------------------------------------------------------------------

        integer function read_run_bank_luciae(lun)

c   Routine to read thr LUCIAE run bank

        implicit none

#include "evtzebra.inc"
#include "event.inc"

        integer*4 iquest
        common /quest/ iquest(100)

c        Local Declarations
c        ==================

        integer lun        ! for messages
        integer*4 idiv
        integer*4 link
        integer*4 jbias
        integer*4 nuh
        integer*4 iheader(12)

c     Revision History

c         March 18, 1996  C.F. Maguire   Change name of ZSTORE common block
c                                        to ZSTOREB because of new IBM error

c        Executable code
c        ===============

        read_run_bank_luciae = -1        ! assume success

        nuh = 12
        jbias = 1
        CALL FZIN(z_lun_in, zstore_index, link, Jbias,'S',nuh,IHEADer)

        if(iquest(1) .ne. 0) then                     ! end of tape or error
           if(iquest(1) .eq. 2) then                ! eor
            write(lun,'(A)')  ' <W> Read end of run record ! '
           elseif(iquest(1) .eq. 3) then        ! Zebra eof
            write(lun,'(A)')  ' <W> Read ZEBRA end of file ! '
           elseif(iquest(1) .eq. 4) then        ! system eof
            write(lun,'(A)')  ' <W> Read SYSTEM end of file ! '
           elseif(iquest(1) .eq. 5) then        ! level 2 EOF
            write(lun,'(A)')  ' <E> Read level 2 End of file! '
           else                                        ! everything else
            write(lun,'(A,I10)')  ' <E> IQUEST(1) = ',IQUEST(1)
           endif
           read_run_bank_luciae = 0
        else

c                Normal run bank following

          idiv = iheader(1)                             ! division index
          if(idiv .eq. run_div) then! pending structure is run bank
            nuh = 12
            call fzin(z_lun_in, div_index(idiv), link_addr(idiv),
     1                   jbias, 'A', nuh, IHEADer)

            if(iquest(1) .ne. 0) then             ! end of tape or error
               if(iquest(1) .eq. 2) then                ! eor
                write(lun,'(A)')  ' <W> Read end of run record ! '
              elseif(iquest(1) .eq. 3) then        ! Zebra eof
                write(lun,'(A)')  ' <W> Read ZEBRA end of file ! '
              elseif(iquest(1) .eq. 4) then        ! system eof
                write(lun,'(A)')  ' <W> Read SYSTEM end of file ! '
              elseif(iquest(1) .eq. 5) then        ! level 2 EOF
                write(lun,'(A)')  ' <E> Read level 2 End of file! '
              else                                        ! everything else
                write(lun,'(A,I10)')  ' <E>  IQUEST(1) = ',IQUEST(1)
              endif
              read_run_bank_luciae = 0
            else
              link = link_addr(run_div)

c  Bank format is
c        RPROJ  - F - Projectile radius
c        RTARG  - F - Target radius
c        CENTRA - F - Centrality Parameter
c        HCONST - F - Secondaries formation time Parameter
c        IFWS   - I -
c        IFERMI - I -
c        IFIXTG - I -
c        IFJET  - I -
c        IFSINT - I -
c        IFSPEC - I -
c        IMBIAS - I -

              rextra(1) = qq(link+1)
              rextra(2) = qq(link+2)
              rextra(3) = qq(link+3)
              rextra(4) = qq(link+4)
              iextra(1) = iqq(link+5)
              iextra(2) = iqq(link+6)
              iextra(3) = iqq(link+7)
              iextra(4) = iqq(link+8)
              iextra(5) = iqq(link+9)
              iextra(6) = iqq(link+10)
              iextra(7) = iqq(link+11)

              call mzwipe( div_index(run_div))

            endif
          endif
        endif

        return
        end
