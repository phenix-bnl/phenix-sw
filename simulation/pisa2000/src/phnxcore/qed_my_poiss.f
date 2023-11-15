      real function qed_my_poiss(xm,iq)
      IMPLICIT NONE
c
c
c              this is a Posson probability distribution
c              generator. It uses the rejection method.
c              Ref: W.H.Press,W.T.Vetterling,S.A.Teukolsky,
c              B.P.Flannery, Numerical Recipes in Fortran,
c              2-nd edition. Cambridge University Press.
c
c             INPUT:
C                    XM - average
c                    IQ - random number generator  seed
c
c
      real rndm
      integer iq
      real xm,pi
      parameter(pi=3.141592654)
      real alxm,em,g,oldm,sq,t,y,qed_gammln
      real test
      save alxm,g,oldm,sq
      data oldm/-1./
c
c
      if(xm.lt.12.)then
         if(xm.ne.oldm)then
            oldm = xm
            g = exp(-xm)
         end if
         em = -1.
         t = 1.
    2    em = em + 1
         test = rndm(iq)
         t = t*test
         if(t.gt.g) go to 2
c
      else
         if(xm.ne.oldm)then
            oldm = xm
            sq = sqrt(2.*xm)
            alxm = log(xm)
            g = xm*alxm - qed_gammln(xm+1.)
c
         end if
c
         test = rndm(iq)
    1    y = tan(pi*test)
         em = sq*y + xm
         if(em.lt.0.)go to 1
         em = int(em)
c
         t = 0.9*(1.+y**2)*exp(em*alxm-qed_gammln(em+1.)-g)
         test = rndm(iq)
         if(test.gt.t)go to 1
      end if
      qed_my_poiss = em
      return
      end
