
c   Routines to read and write the generator dependant RUN banks

c        Routines:
c                read_run_bank_hijet    
c                read_run_bank_venus    
c                read_run_bank_fritiof      
c                read_run_bank_luciae 
c                read_run_bank_rqmd
c                read_run_bank_hijing

c----------------------------------------------------------------------

        integer function read_run_bank_hijing(lun)

c   Routine to read the HIJING run bank

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
        integer*4 i

c        Executable code
c        ===============

        read_run_bank_hijing = -1        ! assume success

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
           read_run_bank_hijing = 0
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
              read_run_bank_hijing = 0
            else
              link = link_addr(run_div)

              call mzwipe( div_index(run_div))

            endif
          endif
        endif

        return
        end
