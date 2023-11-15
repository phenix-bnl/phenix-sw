      real function rv_chi_func(rapid,ptrn)
      implicit none

c
c     Original Author: Charles F. Maguire (based on RV_PHI and RV_JPSI)
c     Creation Date: June 15, 1999
c
c     Revision History
c
c     External function for RV_CHI (Y,PT) distribution
c     From R. Vogt, Atomic and Nuclear Data Tables, vol. 50, #2, 1992
c     Use J/Psi Table 1 parameters
c
      real amsq
      parameter (amsq = 3.51*3.51)

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

      rv_chi_func = rvscale*rvtemp

      return
      end
