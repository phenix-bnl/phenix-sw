      logical function qed_pair_rapidity(iq,minv,rapid)
      IMPLICIT NONE
c
C
C              THIS FUNCTION GENERATES PAIR RAPIDITY
C
c
c              INPUT:
c                      IQ -  seed for the random number generator
c                      MINV - invariant mass of a pair
c              OUTPUT:
c                       RAPID: - pair rapidity
c
c
c
      real rndm
      real rapid
      integer iq
c
      real yl,yp
      real n1,n2
      real test
      real minv,rb
      logical plateau,tail
c
c
      if(minv.ge.100)then
         yl = -0.0016*minv + 5.16
         yp = -0.00091*minv + 2.08
      else
         yl = 5
         yp = 2
      end if
c
c
      n1 = yp
      n2 = (yl - yp)/2.
      rb = n1/(n1+n2)
      test = rndm(iq)
      if(test.le.rb)then
         plateau = .true.
         tail = .false.
      else
         plateau = .false.
         tail = .true.
      end if
c
      test = rndm(iq)
      if(plateau)then
         rapid = test*yp
      else
         rapid = yl+((yp-yl)*sqrt(1-test))
      end if
c
c
      test = rndm(iq)
      if(test.ge.0.5)rapid = -rapid
c
c
      qed_pair_rapidity = .true.
c
      return
      end
