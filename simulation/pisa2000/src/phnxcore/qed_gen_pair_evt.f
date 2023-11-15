      logical function QED_GEN_PAIR_EVT(ISTATX,AINP,PM1,PM2,NPAIR,IQ,
     +                                  NPMAX)
      IMPLICIT NONE
c
c     Called by QED_EE_EVT
c     Calls QED_IMPACT_PARAM, QED_MAKE_PAIR, QED_INVARIANT_MASS
c           QED_PAIR_RAPIDITY, QED_MY_POISS
c
c
c
c        VERSION            1.0
C        CREATED BY:        M.FATYGA
C        DATE OF CREATION:  12-7-1993
C        LAST UPDATED:
C
C         THIS IS A MASTER FUNCTION FOR THE E+E- PAIR GENERATOR
C
C         INPUT:
C                ISTATX: status word
c                       0 - 3: generate one event with one pair
c                       0 - full event
c                       1 - force the impact parameter
c                           impact parameter = AINP(1)
C                       2 - force the invariant mass and the impact
c                           parameter.
c                           impact parameter = AINP(1)
C                           invariant mass =   AINP(2)
C                       3 - force all attributes
c                           impact parameter = AINP(1)
C                           invariant mass =   AINP(2)
c                           rapidity =         AINP(3)
C
C                      10 - 11: generate one event allowing for multiple
c                               pairs. Pair multiplicity is generated
c                               by the Poisson distribution.
c                       10 - full event
c                       11 - force the impact parameter
c                           impact parameter = AINP(1)
c
c                  NPMAX maximum number of pairs (set in calling routine)
c
c
c          OUTPUT:
C                  NPAIR - number of pairs
C
c                  PM1(I,J),PM2(I,J) - attributes of electrons/positrons
c                                      in pairs.
C                     I = 1,12       - pair index
C                     J = 1,3        - attribute index
c
c                     J = 1  -  laboratory longitudinal momentum
c                     J = 2  -  laboratory transverse momentum
c                     J = 3  -  total energy in the lab.
c
c
c
      logical qed_impact_param
      logical qed_make_pair
      logical qed_pair_rapidity
      logical qed_invariant_mass
      logical test,init
c
      real ainp(3)
      integer istat,istatx,Npair
      integer iq
      integer npmax
      real pm1(npmax,3),pm2(npmax,3)
      integer i,jj
      real rndm
c
c
      real p1(3),p2(3)
      real b,minv,rapid,gamma
      logical try
      logical multiple
      real qed_my_poiss
      real bcompt,bmin
      real ave_pro
      real pair_mult
c
c remove this common for a release
      common/temp/minv,rapid,b
c
c
      data gamma/100./
      data bcompt,bmin/385.,15.3/
c
c
c               end declarations/begin task
c
c                check if single or multiple pairs
c
      if(istatx.ge.10)then
         multiple = .true.
         istat = istatx-10
      else
         multiple = .false.
         istat = istatx
      end if
c
c           prepare to generate/fetch impact parameter
c           this is done ONCE PER EVENT.
c
      if(istat.gt.0)then
         b = ainp(1)
      else
         try = qed_impact_param(iq,b)
      end if
c
c
c             generate number of pairs
c
      if(multiple)then
         ave_pro = 1.4
         if(b.gt.bcompt)then
            ave_pro = ave_pro *((bcompt/b)**2)
         end if
         pair_mult = qed_my_poiss(ave_pro,iq)
         npair = ifix(pair_mult)
      else
         npair = 1
      end if
c
c
c      process individual pairs
c
c
      do i = 1,min0(npair,npmax)
c
         if(istat.lt.2)then
            try = qed_invariant_mass(iq,b,gamma,minv)
            if(.not.try)then
               qed_gen_pair_evt = .false.
               return
            end if
         else
            minv = ainp(2)
         end if
         if(istat.lt.3)then
            try = qed_pair_rapidity(iq,minv,rapid)
         else
            rapid = ainp(3)
         end if
         init = .false.
         try = qed_make_pair(iq,minv,rapid,init,p1,p2)
         do jj = 1,3
            pm1(i,jj) = p1(jj)
            pm2(i,jj) = p2(jj)
         end do
      end do
c
c
      qed_gen_pair_evt = .true.
c
      return
      end




