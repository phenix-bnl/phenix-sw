c     =================================================
      subroutine calc_derived_quantities
c     =================================================

c     Calculate the derived quantities which are stored in
c     common arrays.

c     Revision History

c     Charles F. Maguire    November 21, 1998    Change RQMD to lund_to_geant
c                                                from jetset_to_geant IDPART

      Implicit None

#include "event.inc"
#include "evntcode.inc"
#include "u_local.inc"

c     Local variables
c     ===============
      integer i, isa_to_geant,jset_to_geant
      real p, pi
      data pi /3.1415926/

c     Executable Statements:-
c     =======================
      naproj = nnproj + npproj
      natarg = nntarg + nptarg
      nbeam  = nbeam + ntry

c     Loop over particles
c       ===================
      do i = 1, nptls
        if(event_code.eq.venus) then
          gtype(i) = isa_to_geant(idptl(i))
        else if(event_code.eq.hijet) then
          gtype(i) = isa_to_geant(idptl(i))
        else if(event_code.eq.fritiof) then
          call lund_to_geant(idptl(i),gtype(i))
        else if(event_code.eq.luciae)then
          gtype(i)=jset_to_geant(idptl(i))
        else if(event_code.eq.hijing)then
          gtype(i)=jset_to_geant(idptl(i))
        else if(event_code.eq.rqmd)then
          call lund_to_geant(idptl(i),gtype(i))
        else if(event_code.eq.vni)then
          gtype(i)=jset_to_geant(idptl(i))
        end if

        if (use_p4vec) then
          pt(i) = sqrt(p4vec(1,i)**2 + p4vec(2,i)**2)
            p    =  sqrt(p4vec(1,i)**2+p4vec(2,i)**2+p4vec(3,i)**2)
          if(p .gt. 0.00000001) then
            if(abs(p4vec(4,i)-p4vec(3,i)).lt.0.00001) then
              y(i) = 100.
            else if(abs(p4vec(4,i)+p4vec(3,i)) .lt. 0.00001) then
                y(i) = -100.
              else
              y(i) = 0.5 * log((p4vec(4,i)+p4vec(3,i))/
     1                        (p4vec(4,i)-p4vec(3,i)))
              end if
            if(pt(i) .eq. 0) then
              if(p4vec(3,i) .gt. 0) then
                theta(i) = 0.0
              else
                theta(i) = pi
              endif
            else
              theta(i) = atan2(pt(i),p4vec(3,i))
              phi(i)   = atan2(p4vec(1,i),p4vec(2,i))
            endif
              mass(i)  = p4vec(4,i)**2 - p**2
              if(mass(i) .le. 0) then
                mass(i) = 0.0
              else
                mass(i) = sqrt(mass(i))
              end if
          else
              y(i)     = 0.0
              theta(i) = 0.0
              phi(i)   = 0.0
          end if
          endif

      end do

      return
      end
