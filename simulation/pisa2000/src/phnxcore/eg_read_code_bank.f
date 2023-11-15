
c   This file contains routines to read and write the CODE bank

c        Routines:

c                eg_read_code_bank        eg_rank
c                eg_write_code_bank        eg_wank

c-------------------------------------------------------------------------

        integer function eg_read_code_bank (lun)

c   Routine to find the run, read in the code bank (first bank) and load
c   the information found into the common blocks

        implicit none

#include "event.inc"
#include "evntcode.inc"
#include "evtzebra.inc"

        integer*4 iquest
        common /quest/iquest(100)



c        Local Declarations
c        ==================

        integer lun        ! unit for messages

        integer            lsup                    ! link to data structure     
        integer     jbias               ! parameter to declare we require top
C                                       ! level bank

        integer     nmax           ,    ! max size of word in IHEADer
     &              array_size          ! size of array IHEADer stored in
                                        !    record 

        parameter (array_size = 300)        ! run header size + code header size

        character*8 name

        integer*4 div
        integer*4 link
        integer*4 nuh
        integer*4 iheader(array_size)

        integer ival(2)
        real*8  double
        real*4  single
        equivalence(ival, single)
        equivalence(ival, double)

c        Executable code
c        ===============

        eg_read_code_bank = -1                ! assume success

c  NO TAPE POSITIONING PERFORMED!!

c        Read the Zebra Run Header 

        jbias = 1
        nmax = array_size
        CALL FZIN (Z_LUN_IN, zstore_index, lsup,              ! go to next 
     1               jbias, 'R', nmax, iheader)              ! start of run        

c        Check for status of Read

        if(iquest(1).lt.0) then 
          write(lun,*)'  Error trying to read tape, Program aborts.'
          eg_read_code_bank = 0                 ! signal error 
          goto 900


c        Start of run record
c        Read in code division

        elseif (iquest(1).eq.1) then   

C              Read in code divisions
C              ======================
          div = 1
          nmax = array_size

c        skip pending SOR structure and read next header

          CALL FZIN (Z_LUN_IN, div_index(div), link_addr(div), 
     1                   jbias,'S',nmax,iheader)
          div = iheader(1)

          if(iquest(1).eq.0) then   

c                 process code info 

            If ( IHEADer(1). eq. code_div) then 
              CALL FZIN (Z_LUN_IN, div_index(code_div), 
     1               link_addr(code_div), jbias, 'A', nmax, iheader )

              link    = link_addr(code_div)
              ival(1) = iqq(link+1)
              ival(2) = iqq(link+2)

c        determine event code from code header
c        ====================================
              call uhtoc(ival,4,name,8)
              if(name .eq. 'VENUS') then
                event_code    = venus
              else if (name.eq.'HIJET') then
                event_code = hijet
              else if (name.eq.'FRITIOF') then
                event_code = fritiof
              else if (name.eq.'LUCIAE') then
                event_code = luciae
              else if (name.eq.'RQMD') then
                event_code = rqmd
              else if (name.eq.'HIJING') then
                event_code = hijing
              else if (name.eq.'PYTHIA') then
                event_code = pythia
              else if (name.eq.'VNI') then
                event_code = vni
              else
                event_code = 0
              endif
              event_version = qq(link+3)
              zproj        = qq(link+4)
              aproj        = qq(link+5)
              ztarg            = qq(link+6)
              atarg            = qq(link+7)
              sqrt_s           = qq(link+8)
c              bbmin            = qq(link+9)
              bmax            = qq(link+10)

c        get projectile / taget parameters
c        =================================

c  Now write out conditions read from tape

              write(lun,30) name, event_version
 30              format(' < < < < < < < < ZEBRA READ > > > > > > > '//,
     1              1x,a,' Nucleus - Nucleus Event Generator '/,
     2                  '   Version ',f6.2//)
              write(lun,40) sqrt_s, float(zproj), float(aproj),
     1                     float(ztarg), float(atarg)
 40              format(' Projectile Energy Per Nucleon = ',f8.3,//,
     1              ' Projectile Charge = ',f6.0,
     2              ' Projectile Mass = ',f6.0,/,
     3              ' Target Charge     = ',f6.0,
     4              ' Target Mass       = ',f6.0)

              call mzwipe(div_index(code_div))
            else
              write(lun,*)'  Did not find CODE_DIVISION as first entry.'
              write(lun,*)'   Program aborts.'
              eg_read_code_bank = 0
              goto 900
            end if
          else
            write(lun,*)'  Error reading CODE_DIVISION, program end.'
            eg_read_code_bank = 0
            goto 900
          end if
        endif

  900        continue
        return
        end
