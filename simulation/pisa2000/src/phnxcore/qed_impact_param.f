      logical function qed_impact_param(iq,b)
      IMPLICIT NONE
c
C
C             THIS FUNCTION GENERATES THE IMPACT PARAMETER
c
c             INPUT:
C                     IQ  -   random number generator seed
c
c
c             OUTPUT: B   -   impact parameter  [fm]
c
c
      real rndm
      real b,rb
      real test
      integer iq
      real n1,n2
      real pi
      real gamma,nor_fac
      real bcompt,bmin,bmax
      logical close,distant
c
      data bcompt/385./
      data bmin/15./
      data gamma/100./
      data pi/3.14/
c
      bmax = 400*gamma
      nor_fac = log(bmax/bcompt)
c
      n1 = 1.4*pi*((bcompt**2) - (bmin**2))
      n2 = 13.4*pi*(bcompt**2)
      rb = n1/(n1+n2)
c
      test = rndm(iq)
c
      if(test.le.rb)then
         close = .true.
         distant = .false.
      else
         close = .false.
         distant = .true.
      end if
c
c
      test = rndm(iq)
c
      if(close)then
         b = sqrt((test*((bcompt**2)-(bmin**2))) + (bmin**2))
      else
         b = (test*nor_fac)
         b = bcompt*exp(b)
      end if
c
c
      qed_impact_param = .true.
c
c
      return
      end
