      logical function qed_make_pair(iq,minv,rapid,init,p1,p2)
      IMPLICIT NONE
c
c     C.F. Maguire: May  1995
c     Based on original routine MAKE_PAIR from M. Fatyga
c     Called by QED_EE_EVT
c     No subroutines called
c
C
c        THIS FUNCTION GENERATES LABORATORY MOMENTA
C        FOR AN E+E- PAIR WITH RAPIDITY 'Y' AND INVARIANT MASS 'MINV'
c        THE TRANSVERSE MOMENTUM OF A PAIR IS STRICTLY ZERO IN THIS
C        VERSION.
C
C
C        INPUT: MINV -       invariant mass  (REAL)
C               RAPID-       rapidity (of a pair)  (REAL)
C               INIT-        initialization flag  (LOGICAL)
c                            if .TRUE. --> Initialization call
c                            if .FALSE. --> Pair generation call
c                 IQ -       random number generator seed
c
c
c       OUTPUT: P1(1-3),P2(1-3)
C               P*(1) -      longitudinal momentum
c               P*(2) -      transverse momentum
c               P*(3) -      total lab energy
c
c
c
      integer index,i
      real rndm
      real compare,theta_norm,theta_com1,mass_loc
      real atestlim,auplim,alowlim
      integer uplim,lowlim,testlim
      integer iq
      real phix,tanx
      real p0_cm,pt_cm,pl_cm
      real pl1,pl2,pt1,pt2
      real tan1,tan2,angie1,angie2,angle,phi1,phi2
      real beta,betasq,gamma
      real check
      real angu1,angu2,angp1,angp2
c
      real rapid,minv
      logical init
      real p1(3),p2(3)
      real m0
C
      real theta_cm(10000)
      real theta_cm_low
      real pi
      save pi,m0
      save theta_cm
c
C
C         INITIALIZE CM ANGLE GENERATOR
c
      if(init)then
c
         pi = acos(-1.)
         m0 = 0.5
c
c
c       initialize the CM angle generator
c
         angle = (pi/20000.) + (pi/2.)
         theta_cm_low = cos(angle) + (2*log(tan(angle/2.)))
         do i = 1,9998
            angle = ((i+1)*pi/20000.) + (pi/2.)
c        theta_cm(i) = 0.5 - (3*(cos(angle))/8) - ((cos(angle)**3)/8)
            theta_cm(i) = cos(angle) + (2*log(tan(angle/2.))) - theta_
     +      cm_low
c
         end do
         qed_make_pair = .true.
         return
      end if
c
c
c         END OF THE INITIALIZATION SECTION
c
c
c        generate center of mass angle (theta)
c
c
c
c
      mass_loc = minv
      angle = 0.
      uplim = 9998
      lowlim = 1
      theta_norm = (2*log(2*mass_loc))-1
      compare = rndm(iq)
      compare = compare*theta_norm
      do i = 1,10
         atestlim = (uplim-lowlim)/2
         testlim = ifix(atestlim)
         theta_com1 = theta_cm(testlim)
         if(theta_com1.ge.compare)then
            auplim = uplim/2.
            uplim = ifix(auplim)
         else
            alowlim = uplim/2.
            lowlim = ifix(alowlim)
         end if
      end do
c
      do i = lowlim,uplim
         if(theta_cm(i).ge.compare)then
            angle = ((i*pi)/20000.) + (pi/2.)
            go to 1
         end if
      end do
      angle = pi
    1 continue
c
c    rapidity into beta,gamma
c
      beta = (exp(rapid) - exp(-rapid))
      beta = beta/(exp(-rapid) + exp(rapid))
      gamma = 1/(1-(beta**2))
      gamma = sqrt(gamma)
c
c
c
c compute momenta
c
      p0_cm = 0.5*minv
      pt_cm = p0_cm*sin(angle)
      pl_cm = p0_cm*cos(angle)
      pl1 = (gamma*pl_cm) + (gamma*beta*p0_cm)
      pl2 = (-gamma*pl_cm) + (gamma*beta*p0_cm)
      pt1 = pt_cm
      pt2 = pt_cm
c
c
c
c         fill output arrays
c
c
c
c
      p1(1) = pl1
      p2(1) = pl2
      p1(2) = pt1
      p2(2) = pt2
      p1(3) = sqrt((pl1**2)+(pt1**2)+(m0**2))
      p2(3) = sqrt((pl2**2)+(pt2**2)+(m0**2))
c
c
c
      qed_make_pair = .true.
c
      return
      end
