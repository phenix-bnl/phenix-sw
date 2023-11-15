c-------------------------------------------------------------------------

        integer function eg_write_code_bank (lun)

c   Routine to load CODE bank and write it out

        implicit none

*KEEP,EVENT.
#include "event.inc"
*KEEP,EVNTCODE.
#include "evntcode.inc"
*KEEP,EVTZEBRA.
#include "evtzebra.inc"
*KEND.

        integer*4 iquest
        common /quest/iquest(100)

c        Local Declarations
c        ==================

        integer lun        ! unit for messages

        integer     array_size          ! size of array IHEADer stored in
                                        !  record

        parameter (array_size = 300)        ! run header size + code header size

        integer*4 link
        integer*4 nuh
        integer*4 iheader(array_size)

        integer z_form
        integer code_div_size

        integer*4 i_hijet(2)
        integer*4 i_venus(2)
        integer*4 i_fritiof(2)
        character*4 a_hijet(2)  /'HIJE','T   '/
        character*4 a_venus(2)  /'VENU','S   '/
        character*4 a_fritiof(2)  /'FRIT','IOF '/
        equivalence (i_hijet,a_hijet)
        equivalence (i_venus,a_venus)
        equivalence (i_fritiof,a_fritiof)

c        Executable code
c        ===============

        eg_write_code_bank = -1                ! assume success

c   First define form for code bank.

c        MCID   - H - Identifier for Monte carlo code e.g. 'VENUS   '
c        Mver   - F - Monte Carlo code version number e.g.  3.07
c       Zp     - F - Projectile Charge
c       Ap     - F - Projectile Mass
c       Zt     - F - target Charge
c       At     - F - Target Mass
c        S      - F - Sqrt(s) in NN system
c        Bmin   - F - Minimum impact parameter (fm)
c        Bmax   - F - maximum impact parameter (fm)
c        so form is '2H 8F'

        call mzform(div_label(code_div),
     1             '2H 8F',z_form)

        code_div_size = 10

c  Create, load and write run division

        call mzbook(div_index(code_div),link_addr(code_div),
     >                0,zebra_stand_alone_bank,
     >                div_label(code_div),top_num_links(code_div),
     >                top_num_str_links(code_div),
     >                code_div_size,
     >                z_form,zebra_zero_bank)

        link = link_addr(code_div)

        if (event_code.eq.hijet) then
          iqq(link+1) = i_hijet(1)
          iqq(link+2) = i_hijet(2)
        else if (event_code.eq.venus) then
          iqq(link+1) = i_venus(1)
          iqq(link+2) = i_venus(2)
        else if (event_code.eq.fritiof) then
          iqq(link+1) = i_fritiof(1)
          iqq(link+2) = i_fritiof(2)
        end if
 
        qq(link+3) = event_version
        qq(link+4) = zproj
        qq(link+5) = aproj
        qq(link+6) = ztarg
        qq(link+7) = atarg
        qq(link+8) = sqrt_s
        qq(link+9) = bmin
        qq(link+10) = bmax

c  Now write division to tape

        iheader(1) = code_div

        nuh = 1
        call fzout(z_lun_out,div_index(code_div),
     1            link_addr(code_div),1,'D',2,nuh,iheader)
        call mzwipe( div_index(code_div))

  900        continue
        return
        end
