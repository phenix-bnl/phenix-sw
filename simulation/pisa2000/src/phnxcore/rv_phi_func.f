      real function rv_phi_func(rapid,ptrn)
      implicit none
c
c     External function for RV_PHI (Y,PT) distribution
c     From R. Vogt, Atomic and Nuclear Data Tables, vol. 50, #2, 1992
c
      real amsq
      parameter (amsq = 1.0404)

      real rapid
      real ptrn
      real mtrn
      real rvtemp
      real ctemp

      real cfact
      parameter (cfact = 4.06)
      real bfact
      parameter (bfact = 3.93)
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

      rv_phi_func = rvscale*rvtemp

      return
      end
