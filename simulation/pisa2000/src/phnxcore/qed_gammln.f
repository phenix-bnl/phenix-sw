      REAL FUNCTION QED_GAMMLN(XX)
      IMPLICIT NONE
C
c
c            this function returns ln(GAMMA(xx))
c            needed to generate the poisson probability
c            distribution
C
      real XX
      integer J
      double precision ser,stp,tmp,x,y,cof(6)
      save cof,stp
      data cof,stp/76.18009172947146d0,-86.50532032941677d0, 24.0140982
     +4083091d0,-1.231739572450155d0, 0.1208650973866179d-2,-
     +0.5395239384953d-5, 2.5066282746310005d0/
c
      x = xx
      y = x
      tmp = x + 5.5d0
      tmp = ((x+0.5d0)*log(tmp))-tmp
      ser = 1.000000000190015d0
      do j = 1,6
         y = y + 1.d0
         ser = ser + (cof(j)/y)
      end do
      qed_gammln = tmp + log(stp*ser/x)
      return
      end
