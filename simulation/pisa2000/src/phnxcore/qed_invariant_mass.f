      logical function qed_invariant_mass(iq,b,gamma,minv)
      IMPLICIT NONE
c
C
C            THIS FUNCTION GENERATES THE INVARIANT MASS
c
c            INPUT:
C                   GAMMA -    Lorentz gamma of the beam
c                   B     -    impact parameter [fm]
c                   IQ    -    random number generator seed
c
c            OUTPUT:
c                   MINV  -    invariant mass of a pair.
c
c
      real rndm
      real b,gamma
      integer iq
      real minv
c
      real mlim,mmax
      real bcompt,bmin
      real anorm,anor_p
      real mass_s,mass_test
      real p0
      real m0,n1,n2,rb
      real tem1,tem2
      real test
c
c
      logical large,small
c
      data bcompt/385./
      data bmin/15.3/
      data m0/1./
c
c
      mass_test = 1.1*m0
      if(b.le.bcompt)then
         p0 =1.4
      else
         p0 = 1.4*((bcompt/b)**2)
      end if
c
c
      mlim = gamma*400./bmin
      mmax = mlim*bmin/b
      if(mmax.le.mass_test)then
         qed_invariant_mass = .false.
         return
      end if
      mass_s = 3*m0*bcompt/b
      if(mass_s.le.mass_test)then
         large = .true.
         small = .false.
         mass_s = m0
         test = rndm(iq)
         anor_p = ((1/(m0**2))-(1/(mmax**2)))
         anor_p = 2./anor_p
         minv = ((1/mass_s)**2) - (2*test/anor_p)
         minv = 1/minv
         minv = sqrt(minv)
         qed_invariant_mass = .true.
         return
      else
         tem1 = (1/m0) - (1/mass_s)
         tem2 = ((1/(mass_s**2)) - (1/(mmax**2)))*mass_s/2.
         anorm = p0/(tem1+tem2)
         n1 = anorm*tem1
         n2 = anorm*tem2
         rb = n2/(n1+n2)
         test = rndm(iq)
         if(test.le.rb)then
            large = .true.
            small = .false.
         else
            large = .false.
            small = .true.
         end if
c
      end if
c
      test = rndm(iq)
c
c
      if(small)then
         anor_p = ((1/m0) - (1/mass_s))
         anor_p = 1/anor_p
         minv = (1/m0) - (test/anor_p)
         minv = 1./minv
      else
         anor_p = (((1/mass_s)**2)-((1/mmax)**2))
         anor_p = 2./anor_p
         minv = ((1/mass_s)**2) - (2*test/anor_p)
         minv = 1/minv
         minv = sqrt(minv)
c
      end if
c
c
      qed_invariant_mass = .true.
c
      return
      end
