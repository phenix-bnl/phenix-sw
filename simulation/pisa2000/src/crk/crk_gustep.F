*CMZ :  2.04/00 16/12/94  08.23.54  by  Charles F. Maguire
*CMZ :  2.03/00 11/04/93  23.21.11  by  Y. Akiba
*-- Author :
*-- Author :
      subroutine crk_gustep()
*********************
*  FILE crk_gustep.f
*********************

c File: crk_gustep.f
c by Y. Akiba

      implicit none
C.    ******************************************************************
C.    *       CRK routine that is called from GUSTEP to simulate       *
C.    *       Cerenkov optics                                          *
C.    *  1) Generate Cerenkov light (as GEANTINO) if particle velocity *
C.    *     exceeds the Cerenkov threshold                             *
C.    *  2) Reflects the Cerenkov light at the surface of CMIR         *
C.    *  3)                                                            *
C.    *  4) GEANTINO is stopped when it entered to CSHE                *
C.    ******************************************************************

C    Revised for PISA2.04 to be compatible with GEANT3.21
C.
C.    ------------------------------------------------------------------
C DESTEP= ENERGY LOSS PER TRACKING STEP
C ISVOL=1 FOR SENSITIVE VOLUMES
C INWVOL=0 SAME VOLUME
c      1 Entering new volume
c      2 Exiting a volume
c      3 Exiting Mother
c ISTOP =0 Not stopped
c      1 Particle lost identity
c      2 Energy of particle below cut

#include "gugeom.inc"
#include "gconst.inc"
#include "gcbank.inc"
#include "gctrak.inc"
#include "gcflag.inc"
#include "geant321/gcking.inc"
#include "gctmed.inc"
#include "gcsets.inc"
#include "gckine.inc"
#include "gcvolu.inc"
#include "guphnx.inc"
#include "namlvl.inc"
#include "secubuf.inc"
#include "pisa_parts.inc"
#include "sublink.inc"
#include "fpclink.inc"
c constants
c      integer PP_CRK_PHOT
c      parameter (PP_CRK_PHOT = 48)   DEFINED now in PISA_PARTS sequence
c C2H6 gas radiator
      integer MED_C2H6
      parameter (MED_C2H6 = 501)
c CH4 gas radiator
      integer MED_CH4
      parameter (MED_CH4 = 502)
c N2 gas radiator (added by KS, 22.Jun.96)
      integer MED_N2
      parameter (MED_N2 = 4)
c CO2 gas radiator
      integer MED_CO2
      parameter (MED_CO2 = 503)
c Freon13 gas radiator
      integer MED_FR13
      parameter (MED_FR13 = 504)
c**********************************************************************
c Note on index of refraction and photo-electron yield in C2H6:
c The value n = 1.00082 for C2H6 gas is calcualted from a formula given by
c V.P. Zrelov ("Cerenkov radiation in HEP", Keter Press, Jerusalem, 1970)
c The formula calculate the index as function of wavelength, and I picked
c up the value for lambda = 300 nm. Here I assume that we use an array of
c UV-window PMT's as photon detector.

c The refractive index for CH4 is taken from Particle Data Book.

c Note on Photo-electron yield.

c The photo-electron yield is calculated by a formula
c        dN/dx = N0*sin(theta_ce)**2
c where N0 is a parameter. I use N0 value 100.0 (per cm) in this program.
c The Cerenkov photon yield (not photo-electron yield) in visible spectrum is
c approximately
c        dN/dx = 500*sin(theta_ce) (per cm).
c When we multiply 20 % (Q.E. for PMT), we have N0 = 100 per cm as a photo-
c electron yield. Since this formula is for visible spectrum, this gives a
c rather conservative estimate for photo-electron yield for UV-window PMT. A
c simple minded integral of emission spectrum of Cerenkov light, multiplied by
c QE of Hamamatsu PMT gives ~40 pe per 1m of C2H6 radiator.
c**************************************************
c Cerenkov photon radiation in volume CRAD (CRDA/B)

c Cerenkov photons are emitted if the current track is a charged particle,
c and its velocity exceeds the Cerenkov threshould of the media. The Cerenkov
c angle is calculated from the index of refraction and the particle velocity.
c The photon yield is calculated as 100*sin(Ce)**2 per cm. Since this already
c included the Q.E. of Photon detector, these photons are detected by
c 100% efficiency

c 02.Feb.96 by Y.Akiba
c     The RICH geometry is chagned. Now photon detector has
c     "dead area" about 30%. To compensate the loss, and keep the real N0
c     value to be 100, N0 parameter in this code is multiplied by 1/0.7 ~ 1.4

c 17.Oct.96 by K.Shigaki
c     N0 is set higher in fpclink.inc (default = 200.0)
c     Actual N0 is adjusted in PISORP

c 21.Oct.96 by K.Shigaki
c     N0 / (active area ratio) is re-calculated from the result of RICH
c     prototype test at BNL-AGS in 1993.
c     The N0 obtained at the test was 118.1 +- 1.4 (stat) +- 11.8 (sys)
c     and the active are ratio of the PMT array was 81.0 %.
c     N0 / (active area ratio) is now primarily set in
c     pisorp/src/crk/crksetpar.f at 118.1 / 0.810 = 146
c**************************************************
c Cerenkov N0 value
c      real N0
c      parameter (N0 = 100.0)
c      parameter (N0 = 140.0)
c margin to avoid round off error
      real EPS
      parameter (EPS=0.000002)
c flags used for initialization
      logical first /.true./
      save first

c paramters, vars for Cerenkov light generation

      real n_indx               ! index of refraction
      real beta_t               ! threshold velocity
      save n_indx, beta_t

      real dNpe                 ! # of pe detected per 1 cm gas radiator
      real sinCe,cosCe          ! sine and cosine of Cerenkov angle
      real beta                 ! velocity
      integer i
      real pos_old(3)           ! position at the previous step
      real dLpe                 ! average path length between Cerenkov emission
      real vect_save(3)         ! for saving current vect(1)-vect(3)
      real random_array(3)      ! used to store random values from GRNDM
      real phi                  ! azimuth of Cerenkov emission (relative to
                                ! current track direction
      real cosal, sinal,        ! angles used to transfrom Cerekov emission
     $     sinbt, cosbt         ! vector to lab. frame
      real dpath                ! path lenght to the Cerenkov photon emission
                                ! point. 0 < dpath < step
c******************************
c common, var, etc to compute reflection at the mirror surface
c******************************
      real RN(3)                !normal vector at the miror surface
      real VdotR                !dot-product of RN and V(Cerenkov direction)
      real RR                   !distance to the mirror center
      integer ii

c--> common to communicate with CRK.F

c We need to know the center of the MIRROR (CMIR) to compute the normal
c vector at the mirror surface. The center position is passed through
c common block CRK_COMMON
      real Mirr_cent(3)
      common/CRK_COMMON/Mirr_cent

c$$$      if(IPART .eq. PP_CRK_PHOT) call GSXYZ
      if(NUMED .eq. MED_CO2)then
         n_indx = n_CO2
      endif
      if ((NAMES(Nlevel).eq.CRDA).or.
     +    (NAMES(Nlevel).eq.CRDB)) then
c setup parameters
c this code assumes that there is only one kind of radiator in CRAD, and
c the radiator is not changed during one Monte Carlo Run.
        if(first) then
          if(NUMED .eq. MED_C2H6) then ! C2H6 gas radiator
            n_indx = n_C2H6
          else if(NUMED .eq. MED_CH4) then ! CH4 gas radiator
            n_indx = n_CH4
          else if(NUMED .eq. MED_N2) then ! N2 gas radiator
            n_indx = n_N2
          else if(NUMED .eq. MED_CO2) then ! CO2 gas radiator
            n_indx = n_CO2
          else if(NUMED .eq. MED_FR13) then ! Freon 13 gas radiator
            n_indx = n_FR13
          else
            print *,' CRK_GUSTEP: UNKNOWN GAS RADIATOR. DIE !'
            stop
          endif
          beta_t = 1./n_indx + EPS
          first = .false.
        endif

        if(GETOT.gt.0.0)then

c     CFM: sometimes a divide by 0 here ??
c     CFM: put in a sfety check, October 8, 1995

           beta = VECT(7)/GETOT    !VECT(7)...P, GETOT...Total Energy
        else
           beta = 0.0
        endif  ! safety check
c----------------------------
c simulation of scintillation     Added by YA 1/7/98

c this is a very bad programming called "cut and paste". This segment is
c made from a copy of Cerenkov emission code just after this segment.
c I modified the code so that light is emitted uniformly along the path
c of charged particle. The rate of the emission is controlled by dNpe
c parameter. To compare it with Cerenkov emission, I temporarily turns off
c scintillation for particle that is faster than Cerenkov threshold. This
c allows a easy comparison between scinitillation and Cerenkov light since
c electrons will not produce scintillation light.
c----------------------------
c-YA Switch for Scintillation option is provided by 2nd position of
C    CRK command in pisa.kumac
c if it is SNT0 --> scintillation is ON and Cerenkov is ON
c if it is SNT1 --> scintillaiton is ON but Cerenkov is OFF

        if(CVOLU_OPT(2,5).eq.'SNT0'.or.
     $     CVOLU_OPT(2,5).eq.'SNT1') then
        if(abs(Charge).ge.0.5.and. INWVOL. ne. 1) then
c$$$        if( beta .lt. beta_t. and. abs(Charge).ge.0.5
c$$$     $       .and. INWVOL. ne. 1) then
          dNpe = 0.5            ! one photon per cm
          dLpe = 1.0/dNpe       ! average step length between Cerenkov emission
          do i=1,3
            pos_old(i) = vect(i) - step*vect(i+3) !position in the prev. step
            vect_save(i) = vect(i) !save vect(1)-vect(3)
          enddo
          call grndm(random_array,1)
          dpath = -dLpe*log(random_array(1)) !note that log(random_array(1))<0.
                                     !therefore, dpath > 0.
c---> emit scintillation photons along this step
          do while (dpath .le. step)
            NGKINE = NGKINE + 1
            do i=1,3
              vect(i) = pos_old(i) + dpath*vect(i+3)
            enddo
            call grndm(random_array,3)
            phi = 360. * random_array(1)
            cosCe = 2.0*random_array(2) - 1.0
            sinCe = sqrt(1.0 - cosCe*cosCe)

            do i=1,3
               GPOS(i,NGKINE) = vect(i)
            enddo

c the photon is uniformly emitted.

            GKIN(4,NGKINE) = 1.
            GKIN(1,NGKINE) = GKIN(4,NGKINE)*sinCe*cos(phi/57.29578)
            GKIN(2,NGKINE) = GKIN(4,NGKINE)*sinCe*sin(phi/57.29578)
            GKIN(3,NGKINE) = GKIN(4,NGKINE)*cosCe
            GKIN(5,NGKINE) = PP_CRK_PHOT
            TOFD(NGKINE) = (dpath - step)/(Clight*beta)
            IFLGK(NGKINE) = 1
            NUBUF = 1           !store one user word in KINE
            UBUF(1) = ITRA      !ITRA....ITRA of the parent is stored
            call GSKING(NGKINE)
            dpath = dpath - dLpe*log(random_array(3))
          enddo !while(dpath)
c---> restore saved vect(1)-vect(3)
          do i=1,3
            vect(i) = vect_save(i)
          enddo
        endif
        endif
c------------------------------
c end of scintillation, and a very bad programming
c------------------------------
c------------------------------
c simulation of Cereknov light
c------------------------------
        if(.not.(cvolu_opt(2,5).eq.'SNT1')) then
        if(beta .gt. beta_t .and. abs(Charge).ge.0.5
     $       .and. INWVOL. ne. 1) then
          cosCe = 1.0/(n_indx*beta)
          if(cosCe .lt. 1.) then
            sinCe = sqrt(1.0 - cosCe**2)
          else
            sinCe = 0.
          endif
cks          dNpe = N0*sinCe*sinCe !photo-electron yield per cm
c     use N0_INIT in fpclink.ink, 16.Oct.96 by K.Shigaki
          dNpe = N0_INIT*sinCe*sinCe !photo-electron yield per cm
          dLpe = 1.0/dNpe       ! average step length between Cerenkov emission
          do i=1,3
            pos_old(i) = vect(i) - step*vect(i+3) !position in the prev. step
            vect_save(i) = vect(i) !save vect(1)-vect(3)
          enddo
          call grndm(random_array,1)
          dpath = -dLpe*log(random_array(1)) !note that log(random_array(1))<0.
                                     !therefore, dpath > 0.
c---> emit cerenkov photons along this step
          do while (dpath .le. step)
            NGKINE = NGKINE + 1
            do i=1,3
              vect(i) = pos_old(i) + dpath*vect(i+3)
            enddo
            call grndm(random_array,2)
            phi = 360. * random_array(1)
 

c Added by YA. 11/25/94. GEANT3.21 requires that the production verticies
c of the secondaries are set in GPOS() array.
c This loop has no effect in the GEANT3.15 library

            do i=1,3
               GPOS(i,NGKINE) = vect(i)
            enddo

c End of GEANT3.21 insertions

 
            GKIN(4,NGKINE) = 1.
            GKIN(1,NGKINE) = GKIN(4,NGKINE)*sinCe*cos(phi/57.29578)
            GKIN(2,NGKINE) = GKIN(4,NGKINE)*sinCe*sin(phi/57.29578)
            GKIN(3,NGKINE) = GKIN(4,NGKINE)*cosCe
            GKIN(5,NGKINE) = PP_CRK_PHOT
            TOFD(NGKINE) = (dpath - step)/(Clight*beta)
            cosal = vect(6)
            sinal = sqrt(abs(1 - cosal**2))
            if(sinal .ne. 0.) then
              cosbt = vect(4)/sinal
              sinbt = vect(5)/sinal
            else
              cosbt = 1.
              sinbt = 0.
            endif
            call GDROT(GKIN(1,NGKINE),cosal,sinal,cosbt,sinbt) !rotate to Lab
c Shaheen Tonse suggested me that it is better to store the generated
c secondary in KINE as well as STACK. This can be done by setting the
c IFLGK(NGKINE) as 1
            IFLGK(NGKINE) = 1
c In standard GEANT, there is no way to store "user word" in KINE bank
c by calling GSKING. Shaheen Tonse made a patch to this problem, and made
c it possible to store "User word". Here I store itra of the parent.
            NUBUF = 1           !store one user word in KINE
            UBUF(1) = ITRA      !ITRA....ITRA of the parent is stored
c now put the GEANTINO in STAK and KINE
            call GSKING(NGKINE)
            dpath = dpath - dLpe*log(random_array(2)) ! log(random_array)<0.
                                              !therefore, we increase dpath
          enddo !while(dpath)
c---> restore saved vect(1)-vect(3)
          do i=1,3
            vect(i) = vect_save(i)
          enddo
        endif
        endif  ! .not.(cvolu_opt(6,5).eq.'SNT1')

      endif !if('CRAD (CRDA/B)')
c**************************************************
c           Reflection at Mirror
c**************************************************
      if(IPART .eq. PP_CRK_PHOT) then
        if(INWVOL.eq.1.and.NAMES(Nlevel).eq.CMIR) then

c compute vector RN...vector from Mirror center to the hit position

          RN(1) = VECT(1) - Mirr_cent(1)
          RN(2) = VECT(2) - Mirr_cent(2)
c$$$          if(Number(Nlevel).eq.1) then
cc Nlevel-3 should be CSEC volume.
c          if(mod(Number(Nlevel-3),2).eq.1) then ! CSEC copy 1
c            RN(3) = VECT(3) - Mirr_cent(3)
c          else                                  ! CSEC copy 2
c            RN(3) = VECT(3) + Mirr_cent(3)
c          endif
c Nlevel = 2 should be CARM volume, 3 should be CSHA/B, 5 should be CMIR
          if (((Number(2+iEastWest).eq.1).and.
     +         (NAMES(3+iEastWest).eq.CSHA)).or.
     +        ((Number(2+iEastWest).eq.2).and.
     +         (NAMES(3+iEastWest).eq.CSHB))) then
            RN(3) = VECT(3) - Mirr_cent(3)
          else
            RN(3) = VECT(3) + Mirr_cent(3)
          endif
c normalize vector RN....now RN is the normal vector at the hit position
          RR = sqrt( RN(1)**2 + RN(2)**2 + RN(3)**2)
          RN(1) = RN(1)/RR
          RN(2) = RN(2)/RR
          RN(3) = RN(3)/RR
c reflect at the hit position
          VdotR = VECT(4)*RN(1) + VECT(5)*RN(2) + VECT(6)*RN(3)
          VECT(4) = VECT(4) - 2*VdotR*RN(1)
          VECT(5) = VECT(5) - 2*VdotR*RN(2)
          VECT(6) = VECT(6) - 2*VdotR*RN(3)
        endif
      endif
c**************************************************
c           Stop GEANTINO
c**************************************************
      if  ((NAMES(Nlevel).eq.CSHA) .or. ! changed by KS
     &     (NAMES(Nlevel).eq.CSHB) .or. ! changed by KS
     &     (NAMES(Nlevel).eq.CWI1) .or. ! added by KS
     &     (NAMES(Nlevel).eq.CWI2) .or. ! added by KS
     &     (NAMES(Nlevel).eq.CEND) .or. ! added by KS
     &     (NAMES(Nlevel).eq.CPMT) .or. ! added by YA
     &     (NAMES(Nlevel).eq.CPMS) .or. ! added by KS
     &     (NAMES(Nlevel).eq.CMG1) .or. ! added by KS
     &     (NAMES(Nlevel).eq.CMG2) .or. ! added by KS
     &     (NAMES(Nlevel).eq.CMG3) .or. ! added by KS
     &     (NAMES(Nlevel).eq.CMG4) .or. ! added by KS
     &     (NAMES(Nlevel).eq.CMCV) .or. ! added by KS
     &     (NAMES(Nlevel).eq.CMIF) .or. ! added by KS
     &     (NAMES(Nlevel).eq.CARM)) then
        if(IPART.eq.PP_CRK_PHOT) then
          ISTOP = 1
        endif
      endif

      return
      end
