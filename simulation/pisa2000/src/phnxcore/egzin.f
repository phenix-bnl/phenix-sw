c        ================================================================
        Integer function egzin ()
c        ================================================================

c        UNIX Version of code

c        Routine to read in event from a ZEBRA structure.

        implicit none
c        File egzin.for
c        ==============
c        This module contains the routines to read the EVENT bank.

c       called by:
c             EVENT_FILTER
c       call:
c             FZIN
c             mzwipe
c             calc_derived_quantities

c-------------------------------------------------------------------------

c        Global Declarations
c        ===================

#include "evtzebra.inc"
#include "event.inc"
#include "evntcode.inc"
#include "u_local.inc"
#include "gcunit.inc"

        integer*4 iquest
        common /quest/ iquest(100)

c        Local Declarations
c        ==================

        integer*4 idiv
        integer*4 link
        integer*4 jbias
        integer*4 nuh
        integer*4 iheader(12)
        integer*4 i

        integer ival(2)
        real*8  double
        real*4  single
        equivalence(ival, single)
        equivalence(ival, double)

c     Revision History

c         March 18, 1996  C.F. Maguire   Change name of ZSTORE common block
c                                        to ZSTOREB because of new IBM error
c         April 19, 1996  C.F. Maguire   Re-change name to EZSTOR: IBM and
c                                        HP have mutual incompatibilities !*&!
c         May 2, 1996     Li Qun         Add the reading LUCIAE data 
c         May 3, 1996     Li Qun         Correction for xyz vertex units in
c                                        the LUCIAE event reading
c         May 30, 1996    C.F. Maguire   Made into a "generic" routine, almost
c                                        independent of event generator



c        Executable code
c        ===============

        egzin = -1

        nuh = 12
        jbias = 1
        CALL FZIN(z_lun_in, zstore_index, link, Jbias,'S',nuh,IHEADer)

        if(iquest(1) .ne. 0) then                     ! end of tape or error
           if(iquest(1) .eq. 2) then                ! eor
            write(LOUT,'(A)')  ' <W> Read end of run record ! '
           elseif(iquest(1) .eq. 3) then        ! Zebra eof
            write(LOUT,'(A)')  ' <W> Read ZEBRA end of file ! '
           elseif(iquest(1) .eq. 4) then        ! system eof
            write(LOUT,'(A)')  ' <W> Read SYSTEM end of file ! '
           elseif(iquest(1) .eq. 5) then        ! level 2 EOF
            write(LOUT,'(A)')  ' <E> Read level 2 End of file! '
           else                                        ! everything else
            write(LOUt,'(A,I10)')  ' <E> IQUEST(1) = ',IQUEST(1)
           endif
           egzin = 0
        else

c                Normal event following

          idiv = iheader(1)                             ! division index
          if(idiv .eq. event_div) then! pending structure is event
            nuh = 12
            call fzin(z_lun_in, div_index(idiv), link_addr(idiv),
     1                   jbias, 'A', nuh, IHEADer)

            if(iquest(1) .ne. 0) then             ! end of tape or error
               if(iquest(1) .eq. 2) then                ! eor
                write(LOUT,'(A)')  ' <W> Read end of run record ! '
              elseif(iquest(1) .eq. 3) then        ! Zebra eof
                write(LOUT,'(A)')  ' <W> Read ZEBRA end of file ! '
              elseif(iquest(1) .eq. 4) then        ! system eof
                write(LOUT,'(A)')  ' <W> Read SYSTEM end of file ! '
              elseif(iquest(1) .eq. 5) then        ! level 2 EOF
                write(LOUT,'(A)')  ' <E> Read level 2 End of file! '
              else                                        ! everything else
                write(LOUT,'(A,I10)')  ' <E>  IQUEST(1) = ',IQUEST(1)
              endif
              egzin = 0
            else
               link = link_addr(event_div)

c  Do leading part of bank

               nptls    = iqq(link+1)
               nptarg   = iqq(link+2)
               nntarg   = iqq(link+3)
               npproj   = iqq(link+4)
               nnproj   = iqq(link+5)
               ntry     = iqq(link+6)
               bimevt   = qq(link+7)
               link = link + 7

c   Do trailing part of bank. This will add the particle ID,
c   4-momentum, vertex (?), daughter/parent information (?) to the global
c   event table.

               do i=1,nptls
                  idptl(i)  = iqq(link+(i-1)*part_data_size+1)
                  p4vec(1,i) = qq(link+(i-1)*part_data_size+2)
                  p4vec(2,i) = qq(link+(i-1)*part_data_size+3)
                  p4vec(3,i) = qq(link+(i-1)*part_data_size+4)
                  p4vec(4,i) = qq(link+(i-1)*part_data_size+5)
                  if(event_code.eq.luciae.or.
     +               event_code.eq.venus.or.
     +               event_code.eq.rqmd)then

c    Fermi unit length for event generator output of vertex information

                     xyzvec(1,i)=qq(link+(i-1)*part_data_size+6)*1.0e-15
                     xyzvec(2,i)=qq(link+(i-1)*part_data_size+7)*1.0e-15
                     xyzvec(3,i)=qq(link+(i-1)*part_data_size+8)*1.0e-15
                  endif
              end do
              call mzwipe( div_index(event_div))

c        Calculate secondary quantities
c        ==============================
             call calc_derived_quantities

            endif
          endif
        endif
        return
        end
