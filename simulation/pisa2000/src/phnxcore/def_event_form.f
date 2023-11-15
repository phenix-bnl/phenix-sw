c ========================================================================
        subroutine def_event_form
c ========================================================================
        Implicit None

c        Description:-
c        =============
c        This form is independent of the event code

#include "evtzebra.inc"
#include "event.inc"
#include "evntcode.inc"


c    Header:
c        Ntot    - I - Number of particles
c        nptarg  - I - # of spectator TARG protons
c        nntarg  - I - # of spectator TARG neutrons
c        npproj  - I - # of spectator PROJ protons
c        nnproj  - I - # of spectator PROJ neutrons
c        ntry    - I - # of tries necessary before getting an interaction
c                        needed for cross section calculations.
c        b       - F - Impact parameter for event
c    Particles: (repeating regions)
c        ID# - I    - Particle identification number
c        Px  - F(D) - X momentum of particle
c        Py  - F(D) - Y momentum of particle
c        Pz  - F(D) - Z momentum of particle
c        E   - F(D) - Energy of particle in NN system
c        D1  - I    - Decay identifier: -999 MOTHER not stored; -1,0 :no MOTHER

c        so form is '6I 1F / 1I 4F 1I'
c       or         '6I 1F / 1I 8D 1I'  if z_double_out is true

        part_head_size = 7

        if(z_double_out) then
          if(event_code.eq.luciae.or.event_code.eq.rqmd.or.
     +       event_code.eq.venus)then
             call mzform(div_label(event_div),
     1                '6I 1F / 1I 14D 1I',zebra_form_event)
             part_data_size = 16
          else
             call mzform(div_label(event_div),
     1                '6I 1F / 1I 8D 1I',zebra_form_event)
             part_data_size = 10
          endif
        else
           if(event_code.eq.luciae.or.event_code.eq.rqmd.or.
     +        event_code.eq.venus.or.event_code.eq.vni)then
              call mzform(div_label(event_div),
     1                '6I 1F / 1I 7F 1I',zebra_form_event)
              part_data_size = 9
           else
              call mzform(div_label(event_div),
     1                '6I 1F / 1I 4F 1I',zebra_form_event)
              part_data_size = 6
           endif
        end if

        return
        end
