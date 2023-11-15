C     File name: rxn_gustep.f
C       ---------
      subroutine rxn_gustep

      implicit none

#include "gctrak.inc"
#include "gckine.inc"
#include "gcsets.inc"


c    Local variables

      real xm(3), xd(3), hitsd(16), xms(3), xd1(3), xd2(3), dele
      integer inoutl, numdd, it, k

      data inoutl /0/
      save xms, xd1, xd2, inoutl, dele, hitsd

      if(inwvol.eq.1)then
         inoutl = 1
         dele = 0.0

         do k = 1,3
            xm(k) = vect(k)
            xms(k) = vect(k)
         enddo
         call gmedia(xm, numdd)
         call gmtod(xm, xd1, 1)
         

c   Global coordinates at entrance

         hitsd(10) = xm(1)
         hitsd(11) = xm(2)
         hitsd(12) = xm(3)

c   Momentum coordinates at entrance

         hitsd(14) = vect(7)*vect(4)
         hitsd(15) = vect(7)*vect(5)
         hitsd(16) = vect(7)*vect(6)
      endif


c     add the energy loss

      dele = dele + destep*1.e+6 ! convert from GeV to keV

      if((inwvol.eq.2 .or. istop.gt.0) .and. inoutl.gt.0)then
         do k = 1,3
            xm( K) = vect( K)
         enddo
         inoutl = 0
         call gmedia(xms, numdd)
         call gmtod(xm, xd2, 1)

         hitsd( 1) = xd1( 1)
         hitsd( 2) = xd1( 2)
         hitsd( 3) = xd1( 3)
         hitsd( 4) = xd2( 1)
         hitsd( 5) = xd2( 2)
         hitsd( 6) = xd2( 3)
         hitsd( 7) = tofg*1.e+09
         hitsd( 8) = float(ipart)
         hitsd( 9) = dele
         hitsd(13) = sleng

c     should check on it return value

         call gsahit(iset, idet, itra, numbv, hitsd, it)
      endif

      return

      end
