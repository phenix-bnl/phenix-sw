c $Id: tfw_gustep.f,v 1.9 2008/05/21 08:22:19 hpereira Exp $

      subroutine tfw_gustep
      implicit none

#include "gctrak.inc"
#include "gckine.inc"
#include "gcsets.inc"


c    Local variables
      real xm(3), xd(3), hitsd(13), xms(3), xd1(3), xd2(3), dele
      integer inoutl, numdd, it, k
      real entot

      data inoutl /0/
      save xms, xd1, xd2, inoutl, dele, hitsd

      integer iFirst/1/
      save iFirst

c     Begin executable code

      ! first time call check
      if(iFirst.eq.1)then
         write(*,*) 'tfw_gustep - first call'
         write(*,*) 'tfw_gustep - ver. Nov. 22, 2007'
         iFirst = 0
      endif  
      
      if(abs(charge).lt.0.5)then
        return     ! do not store neutral particles
      endif
      
      if(inwvol.eq.1)then
        inoutl = 1
        dele = 0.0
        
        do k = 1,3
          xm(k) = vect(k)
          xms(k) = vect(k)
        enddo
        
        call gmedia(xm, numdd)
        call gmtod(xm, xd1, 1)
              
c       Global coordinates at entrance
        hitsd(10) = xm(1)
        hitsd(11) = xm(2)
        hitsd(12) = xm(3)
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
        
        if(it.le.0)then
          write(6,6)it, itra, hitsd
 6        format(' tfw_gustep - fatal error,',
     +      ' hit not stored: it = ', i2,
     +      ', track =',i6, ', hit pattern',
     +      /, 13(1x,e11.4))
          stop
        endif
        
        
      endif
      entot=entot+dele
      
      return
      end
