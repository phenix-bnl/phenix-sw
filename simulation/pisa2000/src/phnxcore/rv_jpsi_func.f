      real function rv_jpsi_func(rapid,ptrn)
      implicit none

c
c     Original Author: Charles F. Maguire (based on RV_PHI)
c     Creation Date: June 17, 2002
c
c     Revision History
c
c     External function for RV_JPSI (Y,PT) distribution
c     From R. Vogt, Atomic and Nuclear Data Tables, vol. 50, #2, 1992
c     Use J/Psi Table 1 parameters
c
      real amsq
      parameter (amsq = 9.5906657)

      real rapid
      real ptrn
      real mtrn
      real rvtemp
      real ctemp

      real cfact
      parameter (cfact = 3.55)
      real bfact
      parameter (bfact = 2.08)
      real roots
      parameter (roots = 200.)

      real rvscale
      parameter (rvscale = 1.0e+7)
      
      mtrn = sqrt(amsq + ptrn*ptrn)
      ctemp =  1.0 - (mtrn/roots)*
     +         (exp(rapid) - exp(-rapid))

      if(ctemp.gt.0.0)then
         ctemp = ctemp**cfact
         rvtemp = ptrn*exp(-bfact*ptrn)*ctemp
      else
         rvtemp = 0.0
      endif

      rv_jpsi_func = rvscale*rvtemp

      return
      end
