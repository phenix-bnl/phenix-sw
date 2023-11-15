	SUBROUTINE GUADDPARTS
            
C  This routine is to add more particle definitions to the standard
C  Geant list. Most of them are specially defined unreal particles eg. a
C  Phi(1020) made to decay 100% of the time into e+e- pairs. There are also
C  Trd photons and cerenkov photons so that the respective sub-system
C  programmer can treat it specially when it reaches certain media. Elsewhere
C  in the code, these particles should be referred to by their parameter
C  name in include sequence PISA_PARTS, NOT by their numbers. In the event
C  that a change in Particle number is necessary, the PISA_PARTS sequence
C  should be edited. If there is need for additional particles then
C  increment NPISAPARTS, and add to the list
C  of data statements, and include the new particle with a parameter name
C  in PISA_PARTS.

* Modified so that the new Pisa-defined particle types and decays
* are to be accessed by named PARAMETERS, not by raw numbers. This is
* in case they need to be changed again, due to CERN adding more new particles
* in Geant as happenned when GPIONS (Fluka) was introduced.

      implicit none
      real eV_to_sec      
      ! eV to seconds conversion factor for getting lifetime from width
c     parameter (eV_to_sec = 4.135701e-15)    ! original conversion factor from Shaheen Tonse ??
      parameter (eV_to_sec = 6.58211898e-16)  ! this is hbar in eV-seconds, consistent with gdecay routine

c     The gdecay routine in GEANT converts from lifetime to width in order
c     to get a Breit-Wigner (a.k.a. Lorentz) line shape for the for the short-lived resonances

      integer nnwb      ! for ZDC
      integer nwb       ! user parameters if necessary
      parameter (nwb = 0)     ! for now (not used)
      real ub /0.0/        ! for now (not used)
      INTEGER I
      REAL TLIFE
      INTEGER NPISAPARTS
      PARAMETER(NPISAPARTS=43)
      real BRATIO(6,NPISAPARTS),
     &  amass(NPISAPARTS), charge(NPISAPARTS), width(NPISAPARTS)
      CHARACTER*20 PARTNAMES(NPISAPARTS)
      integer ipart(NPISAPARTS), itrtyp(NPISAPARTS),
     &  MDECAY(6,NPISAPARTS)

#include "pisa_parts.inc"

c     Set up the branching ratio and the decay channel arrays for GEANT
c     See the GSPART and the GSDK subroutine documentation in standard GEANT

c     J/Psi ---> u+ u- branch
        DATA IPART(1)/PP_JPSI_MM/
        DATA bratio(1,1),bratio(2,1),bratio(3,1),bratio(4,1),
     &  bratio(5,1),bratio(6,1) /100.0, 5*0.0/ ! 100% branch to first channel
        DATA mdecay(1,1),mdecay(2,1),mdecay(3,1),mdecay(4,1),
     &  mdecay(5,1),mdecay(6,1) /506,5*0/   ! first channel is mu+ mu-
        DATA amass(1)/3.09693/    ! mass in GeV/c**2
        DATA charge(1)/0.0/          ! neutral
        DATA width(1)/63.0e+03/      ! eV width
        DATA itrtyp(1)/3/            ! track as a weak
        DATA PARTNAMES(1)/'JPSI_MM$'/

c     J/Psi ---> e+ e- branch
        DATA IPART(2)/PP_JPSI_EE/
        DATA bratio(1,2),bratio(2,2),bratio(3,2),bratio(4,2),
     &  bratio(5,2),bratio(6,2) /100.0, 5*0.0/ ! 100% branch to first channel
        DATA mdecay(1,2),mdecay(2,2),mdecay(3,2),mdecay(4,2),
     &  mdecay(5,2),mdecay(6,2) /203,5*0/ ! first channel is e+ e-
        DATA amass(2)/3.09693/    ! mass in GeV/c**2
        DATA charge(2)/0.0/          ! neutral
        DATA width(2)/63.0e+03/      ! eV width
        DATA itrtyp(2)/3/            ! track as a weak
        DATA PARTNAMES(2)/'JPSI_EE$'/

c     Upsilon (1S) ---> u+ u- branch
        DATA IPART(3)/PP_UP1S_MM/
        DATA bratio(1,3),bratio(2,3),bratio(3,3),bratio(4,3),
     &  bratio(5,3),bratio(6,3) /100.0, 5*0.0/ ! 100% branch to first channel
        DATA mdecay(1,3),mdecay(2,3),mdecay(3,3),mdecay(4,3),
     &  mdecay(5,3),mdecay(6,3) /506,5*0/ ! first channel is mu mu
        DATA amass(3)/9.460/    ! mass in GeV/c**2
        DATA charge(3)/0.0/          ! neutral
        DATA width(3)/44.3e+03/      ! eV width
        DATA itrtyp(3)/3/            ! track as a weak
        DATA PARTNAMES(3)/'UP1S_MM$'/
 
c     Upsilon (2S) ---> u+ u- branch
        DATA IPART(4)/PP_UP2S_MM/
        DATA bratio(1,4),bratio(2,4),bratio(3,4),bratio(4,4),
     &  bratio(5,4),bratio(6,4) /100.0, 5*0.0/ ! 100% branch to first channel
        DATA mdecay(1,4),mdecay(2,4),mdecay(3,4),mdecay(4,4),
     &  mdecay(5,4),mdecay(6,4) /506,5*0/ ! first channel is mu mu
        DATA amass(4)/10.025/    ! mass in GeV/c**2
        DATA charge(4)/0.0/          ! neutral
        DATA width(4)/29.6e+03/      ! eV width
        DATA itrtyp(4)/3/            ! track as a weak
        DATA PARTNAMES(4)/'UP2S_MM$'/

c     Upsilon (3S) ---> u+ u- branch
        DATA IPART(5)/PP_UP3S_MM/
        DATA bratio(1,5),bratio(2,5),bratio(3,5),bratio(4,5),
     &  bratio(5,5),bratio(6,5) /100.0, 5*0.0/ ! 100% branch to first channel
        DATA mdecay(1,5),mdecay(2,5),mdecay(3,5),mdecay(4,5),
     &  mdecay(5,5),mdecay(6,5) /506,5*0/ ! first channel is mu mu
        DATA amass(5)/10.355/    ! mass in GeV/c**2
        DATA charge(5)/0.0/          ! neutral
        DATA width(5)/17.7e+03/      ! eV width
        DATA itrtyp(5)/3/            ! track as a weak
        DATA PARTNAMES(5)/'UP3S_MM$'/

c     Phi (1020) ---> u+ u- branch
        DATA IPART(6)/PP_PHI_MM/
        DATA bratio(1,6),bratio(2,6),bratio(3,6),bratio(4,6),
     &  bratio(5,6),bratio(6,6) /100.0, 5*0.0/ ! 100% branch to first channel
        DATA mdecay(1,6),mdecay(2,6),mdecay(3,6),mdecay(4,6),
     &  mdecay(5,6),mdecay(6,6) /506,5*0/ ! first channel is mu mu
        DATA amass(6)/1.019417/      ! mass in GeV/c**2  (change from 1.020)
        DATA charge(6)/0.0/          ! neutral
        DATA width(6)/4.458e+06/      ! eV width (change from 4.22)
        DATA itrtyp(6)/3/            ! track as a weak
        DATA PARTNAMES(6)/'PHI_MM$'/

c     Phi (1020) ---> e+ e- branch
        DATA IPART(7)/PP_PHI_EE/
        DATA bratio(1,7),bratio(2,7),bratio(3,7),bratio(4,7),
     &  bratio(5,7),bratio(6,7) /100.0, 5*0.0/ ! 100% branch to first channel
        DATA mdecay(1,7),mdecay(2,7),mdecay(3,7),mdecay(4,7),
     &  mdecay(5,7),mdecay(6,7) /203,5*0/ ! first channel is e+ e-
        DATA amass(7)/1.019417/      ! mass in GeV/c**2  (change from 1.020)
        DATA charge(7)/0.0/          ! neutral
        DATA width(7)/4.458e+06/      ! eV width (change from 4.22)
        DATA itrtyp(7)/3/            ! track as a weak
        DATA PARTNAMES(7)/'PHI_EE$'/

c     Omega (783) ---> e+ e- branch
        DATA IPART(8)/PP_OME_EE/
        DATA bratio(1,8),bratio(2,8),bratio(3,8),bratio(4,8),
     &  bratio(5,8),bratio(6,8) /100.0, 5*0.0/ ! 100% branch to first channel
        DATA mdecay(1,8),mdecay(2,8),mdecay(3,8),mdecay(4,8),
     &  mdecay(5,8),mdecay(6,8) /203,5*0/ ! first channel is e+ e-
        DATA amass(8)/0.783/    ! mass in GeV/c**2
        DATA charge(8)/0.0/          ! neutral
        DATA width(8)/9.89e+06/      ! eV width
        DATA itrtyp(8)/3/            ! track as a weak
        DATA PARTNAMES(8)/'OME_EE$'/

c     Rho (770) ---> e+ e- branch
        DATA IPART(9)/PP_RHO_EE/
        DATA bratio(1,9),bratio(2,9),bratio(3,9),bratio(4,9),
     &  bratio(5,9),bratio(6,9) /100.0, 5*0.0/ ! 100% branch to first channel
        DATA mdecay(1,9),mdecay(2,9),mdecay(3,9),mdecay(4,9),
     &  mdecay(5,9),mdecay(6,9) /203,5*0/ ! first channel is e+ e-
        DATA amass(9)/0.770/    ! mass in GeV/c**2
        DATA charge(9)/0.0/          ! neutral
        DATA width(9)/153.7e+06/      ! eV width
        DATA itrtyp(9)/3/            ! track as a weak
        DATA PARTNAMES(9)/'RHO_EE$'/

c     Psi-prime ---> u+ u- branch
        DATA IPART(10)/PP_PSIP_MM/
        DATA bratio(1,10),bratio(2,10),bratio(3,10),bratio(4,10),
     &  bratio(5,10),bratio(6,10) /100.0, 5*0.0/ ! 100% branch to first channel
        DATA mdecay(1,10),mdecay(2,10),mdecay(3,10),mdecay(4,10),
     &  mdecay(5,10),mdecay(6,10) /506,5*0/ ! first channel is mu mu
        DATA amass(10)/3.685/    ! mass in GeV/c**2
        DATA charge(10)/0.0/          ! neutral
        DATA width(10)/215.0e+03/      ! eV width
        DATA itrtyp(10)/3/            ! track as a weak
        DATA PARTNAMES(10)/'PSIP_MM$'/

c     Psi-prime ---> e+ e- branch
        DATA IPART(11)/PP_PSIP_EE/
        DATA bratio(1,11),bratio(2,11),bratio(3,11),bratio(4,11),
     &  bratio(5,11),bratio(6,11) /100.0, 5*0.0/ ! 100% branch to first channel
        DATA mdecay(1,11),mdecay(2,11),mdecay(3,11),mdecay(4,11),
     &  mdecay(5,11),mdecay(6,11) /203,5*0/ ! first channel is e+ e-
        DATA amass(11)/3.685/    ! mass in GeV/c**2
        DATA charge(11)/0.0/          ! neutral
        DATA width(11)/215.0e+03/      ! eV width
        DATA itrtyp(11)/3/            ! track as a weak
        DATA PARTNAMES(11)/'PSIP_EE$'/

c     TRD Photon
        DATA IPART(12)/PP_TRD_PHOT/
        DATA bratio(1,12),bratio(2,12),bratio(3,12),bratio(4,12),
     &  bratio(5,12),bratio(6,12) /6*-1.0/  !set to -1 means to not call GSDK
        DATA mdecay(1,12),mdecay(2,12),mdecay(3,12),mdecay(4,12),
     &  mdecay(5,12),mdecay(6,12) /6*0/
        DATA amass(12)/0.0/    ! mass in GeV/c**2
        DATA charge(12)/0.0/          ! neutral
        DATA width(12)/1.0e-15/      ! eV width
        DATA itrtyp(12)/6/
        DATA PARTNAMES(12)/'TRD_PHOT$'/
 
c     Cerenkov Photon (essentially non-interacting unless Akiba wants
C     it to bounce off something. Note that GEANT can now treat photons as
C     Cerenkov photons. Maybe Pisa should switch to these. SRTonse.
        DATA IPART(13)/PP_CRK_PHOT/
        DATA bratio(1,13),bratio(2,13),bratio(3,13),bratio(4,13),
     &  bratio(5,13),bratio(6,13) /6*-1.0/
        DATA mdecay(1,13),mdecay(2,13),mdecay(3,13),mdecay(4,13),
     &  mdecay(5,13),mdecay(6,13) /6*0/
        DATA amass(13)/0.0/    ! mass in GeV/c**2
        DATA charge(13)/0.0/          ! neutral
        DATA width(13)/1.0e-15/      ! eV width
        DATA itrtyp(13)/6/
        DATA PARTNAMES(13)/'CRK_PHOT$'/


c     Phi (1020) ---> K+ K- branch
        DATA IPART(14)/PP_PHI_KK/
        DATA bratio(1,14),bratio(2,14),bratio(3,14),bratio(4,14),
     &  bratio(5,14),bratio(6,14) /100.0, 5*0.0/ ! 100% branch to first channel
        DATA mdecay(1,14),mdecay(2,14),mdecay(3,14),mdecay(4,14),
     &  mdecay(5,14),mdecay(6,14) /1112,5*0/ ! first channel is K+ K-
        DATA amass(14)/1.019417/      ! mass in GeV/c**2  (change from 1.020)
        DATA charge(14)/0.0/          ! neutral
        DATA width(14)/4.458e+06/      ! eV width (change from 4.22)
        DATA itrtyp(14)/3/            ! track as a weak
        DATA PARTNAMES(14)/'PHI_KK$'/

c     K+ which does not decay
        DATA IPART(15)/PP_KPLUS_NODECAY/
        DATA bratio(1,15),bratio(2,15),bratio(3,15),bratio(4,15),
     &  bratio(5,15),bratio(6,15) /6*-1.0/  !set to -1 means to not call GSDK
        DATA mdecay(1,15),mdecay(2,15),mdecay(3,15),mdecay(4,15),
     &  mdecay(5,15),mdecay(6,15) /6*0/
        DATA amass(15)/0.493677/    ! mass in GeV/c**2
        DATA charge(15)/1.0/          ! positive
        DATA width(15)/1.0e-15/      ! eV width
        DATA itrtyp(15)/4/           ! track as hadronic (FLUKA bombs on Linux?)
        DATA PARTNAMES(15)/'KPLUS_ND$'/

c     K- which does not decay
        DATA IPART(16)/PP_KMINUS_NODECAY/
        DATA bratio(1,16),bratio(2,16),bratio(3,16),bratio(4,16),
     &  bratio(5,16),bratio(6,16) /6*-1.0/  !set to -1 means to not call GSDK
        DATA mdecay(1,16),mdecay(2,16),mdecay(3,16),mdecay(4,16),
     &  mdecay(5,16),mdecay(6,16) /6*0/
        DATA amass(16)/0.493677/    ! mass in GeV/c**2
        DATA charge(16)/-1.0/          ! negative
        DATA width(16)/1.0e-15/      ! eV width
        DATA itrtyp(16)/4/           ! track as hadronic (FLUKA bombs on Linux?)
        DATA PARTNAMES(16)/'KMINUS_ND$'/


c     Phi (1020) ---> K+ K- branch, no decays
        DATA IPART(17)/PP_PHI_NDKK/
        DATA bratio(1,17),bratio(2,17),bratio(3,17),bratio(4,17),
     &  bratio(5,17),bratio(6,17) /100.0, 5*0.0/ ! 100% branch to first channel
        DATA mdecay(1,17),mdecay(2,17),mdecay(3,17),mdecay(4,17),
     &  mdecay(5,17),mdecay(6,17) /5152,5*0/ ! first channel is K+ K-
        DATA amass(17)/1.019417/      ! mass in GeV/c**2  (change from 1.020)
        DATA charge(17)/0.0/          ! neutral
        DATA width(17)/4.458e+06/      ! eV width (change from 4.22)
        DATA itrtyp(17)/3/            ! track as a weak
        DATA PARTNAMES(17)/'PHI_KK_NDkk$'/


c     J/Psi ---> e+ e- branch (J/Psi coming from radiative Chi decay)
        DATA IPART(18)/PP_RJPSI_EE/
        DATA bratio(1,18),bratio(2,18),bratio(3,18),bratio(4,18),
     &  bratio(5,18),bratio(6,18) /100.0, 5*0.0/ ! 100% branch to first channel
        DATA mdecay(1,18),mdecay(2,18),mdecay(3,18),mdecay(4,18),
     &  mdecay(5,18),mdecay(6,18) /203,5*0/ ! first channel is e+ e-
        DATA amass(18)/3.09693/    ! mass in GeV/c**2
        DATA charge(18)/0.0/          ! neutral
        DATA width(18)/63.0e+03/      ! eV width
        DATA itrtyp(18)/3/            ! track as a weak
        DATA PARTNAMES(18)/'RJPSI_EE$'/


c     J/Psi ---> mu+ mu- branch (J/Psi coming from radiative Chi decay)
        DATA IPART(19)/PP_RJPSI_MM/
        DATA bratio(1,19),bratio(2,19),bratio(3,19),bratio(4,19),
     &  bratio(5,19),bratio(6,19) /100.0, 5*0.0/ ! 100% branch to first channel
        DATA mdecay(1,19),mdecay(2,19),mdecay(3,19),mdecay(4,19),
     &  mdecay(5,19),mdecay(6,19) /506,5*0/ ! first channel is mu+ mu-
        DATA amass(19)/3.09693/    ! mass in GeV/c**2
        DATA charge(19)/0.0/          ! neutral
        DATA width(19)/63.0e+03/      ! eV width
        DATA itrtyp(19)/3/            ! track as a weak
        DATA PARTNAMES(19)/'RJPSI_MM$'/


c     Chi(1P) ---> e+ e- branch
        DATA IPART(20)/PP_CHI_EE/
        DATA bratio(1,20),bratio(2,20),bratio(3,20),bratio(4,20),
     &  bratio(5,20),bratio(6,20) /100.0, 5*0.0/ ! 100% branch to first channel
        DATA mdecay(1,20),mdecay(2,20),mdecay(3,20),mdecay(4,20),
     &  mdecay(5,20),mdecay(6,20) /153,5*0/ ! first channel J/Psi(e+ e-) gamma
        DATA amass(20)/3.51/    ! mass in GeV/c**2
        DATA charge(20)/0.0/          ! neutral
        DATA width(20)/215.0e+03/      ! eV width
        DATA itrtyp(20)/3/            ! track as a weak
        DATA PARTNAMES(20)/'CHI_EE$'/

c     Chi(1P) ---> u+ u- branch
        DATA IPART(21)/PP_CHI_MM/
        DATA bratio(1,21),bratio(2,21),bratio(3,21),bratio(4,21),
     &  bratio(5,21),bratio(6,21) /100.0, 5*0.0/ ! 100% branch to first channel
        DATA mdecay(1,21),mdecay(2,21),mdecay(3,21),mdecay(4,21),
     &  mdecay(5,21),mdecay(6,21) /154,5*0/ ! first chan J/Psi(mu+ mu-) gamma
        DATA amass(21)/3.51/    ! mass in GeV/c**2
        DATA charge(21)/0.0/          ! neutral
        DATA width(21)/215.0e+03/      ! eV width
        DATA itrtyp(21)/3/            ! track as a weak
        DATA PARTNAMES(21)/'CHI_MM$'/

c     Pi0 (0.135) ---> 100% Dalitz Decay branch
        DATA IPART(22)/PP_PIZ_DALITZ/
        DATA bratio(1,22),bratio(2,22),bratio(3,22),bratio(4,22),
     &  bratio(5,22),bratio(6,22) /100.0, 5*0.0/ ! 100% branch to first channel
        DATA mdecay(1,22),mdecay(2,22),mdecay(3,22),mdecay(4,22),
     &  mdecay(5,22),mdecay(6,22) /10302,5*0/ ! first channel is 3-body photon + e+ + e-
        DATA amass(22)/0.1349743/    ! mass in GeV/c**2
        DATA charge(22)/0.0/          ! neutral
        DATA width(22)/49.23/      ! eV width of pizero  (T = 8.4 x 10**-17 seconds)
        DATA itrtyp(22)/4/            ! track as a hadronic
        DATA PARTNAMES(22)/'PIZ_DD$'/

c     d-bar (1.875613) ---> anti-deuteron as a hadron
        DATA IPART(23)/PP_DBAR/
        DATA bratio(1,23),bratio(2,23),bratio(3,23),bratio(4,23),
     &  bratio(5,23),bratio(6,23) /6*-1.0/ ! -1.0 means don't gall GSDK 
        DATA mdecay(1,23),mdecay(2,23),mdecay(3,23),mdecay(4,23),
     &  mdecay(5,23),mdecay(6,23) /1315,5*0/ ! dummy n-pbar decay 
        DATA amass(23)/1.875613/    ! mass in GeV/c**2
        DATA charge(23)/-1.0/        ! negative charge
        DATA width(23)/0.0/          ! no decay
        DATA itrtyp(23)/4/           ! track as a hadronic
        DATA PARTNAMES(23)/'ANTIDEU$'/


c     Photon ---> e+ e- forced conversion
        DATA IPART(24)/PP_PHO_EE/
        DATA bratio(1,24),bratio(2,24),bratio(3,24),bratio(4,24),
     &  bratio(5,24),bratio(6,24) /100.0, 5*0.0/ ! 100% branch to first channel
        DATA mdecay(1,24),mdecay(2,24),mdecay(3,24),mdecay(4,24),
     &  mdecay(5,24),mdecay(6,24) /203,5*0/ ! first channel e+ e-
        DATA amass(24)/0.0015/    ! mass in GeV/c**2 (needs a mass to decay)
        DATA charge(24)/0.0/          ! neutral
        DATA width(24)/4.22e+06/      ! eV width (doesn't matter, the decay is forced)
        DATA itrtyp(24)/3/            ! track as neutral
        DATA PARTNAMES(24)/'PHO_EE$'/


c     Lambda ---> proton pi- branch (special case of c*tau = 0)
        DATA IPART(25)/PP_LAM_PRPIM/
        DATA bratio(1,25),bratio(2,25),bratio(3,25),bratio(4,25),
     &  bratio(5,25),bratio(6,25) /100.0, 5*0.0/ ! 100% branch to first channel
        DATA mdecay(1,25),mdecay(2,25),mdecay(3,25),mdecay(4,25),
     &  mdecay(5,25),mdecay(6,25) /1409,5*0/ ! first channel is proton + pi-
        DATA amass(25)/1.11563/    ! mass in GeV/c**2
        DATA charge(25)/0.0/          ! neutral
        DATA width(25)/63.0e+03/      ! eV width
        DATA itrtyp(25)/3/            ! track as a weak
        DATA PARTNAMES(25)/'LAM_PRPI$'/


c     D0-meson ---> k- pi+ branch (special case of c*tau = 0)
        DATA IPART(26)/PP_D0_KMPIP/
        DATA bratio(1,26),bratio(2,26),bratio(3,26),bratio(4,26),
     &  bratio(5,26),bratio(6,26) /100.0, 5*0.0/ ! 100% branch to first channel
        DATA mdecay(1,26),mdecay(2,26),mdecay(3,26),mdecay(4,26),
     &  mdecay(5,26),mdecay(6,26) /1208,5*0/ ! first channel is proton + pi-
        DATA amass(26)/1.865/    ! mass in GeV/c**2
        DATA charge(26)/0.0/          ! neutral
        DATA width(26)/1.538e-03/      ! eV width
        DATA itrtyp(26)/3/            ! track as a hadronic
        DATA PARTNAMES(26)/'D0_KPI$'/


c     Theta+ (1540) pentaquark
        DATA IPART(27)/PP_THETA_PLUS/
        DATA bratio(1,27),bratio(2,27),bratio(3,27),bratio(4,27),
     &  bratio(5,27),bratio(6,27) /2*50.0, 4*0.0/ ! 50/50 branch to K+ n and p K0 channel
        DATA mdecay(1,27),mdecay(2,27),mdecay(3,27),mdecay(4,27),
     &  mdecay(5,27),mdecay(6,27) /1113,1614,4*0/ ! first channel is K+ n, second K0s p
        DATA amass(27)/1.540/         ! mass in GeV/c**2
        DATA charge(27)/1.0/          ! +1
        DATA width(27)/3.e+05/        ! eV width (30 MeV hep-ph/0401187 v1))
        DATA itrtyp(27)/3/            ! track as a hadronic
        DATA PARTNAMES(27)/'THETA+$'/


c     Theta- (1540) pentaquark (anti)
        DATA IPART(28)/PP_THETA_MINUS/
        DATA bratio(1,28),bratio(2,28),bratio(3,28),bratio(4,28),
     &  bratio(5,28),bratio(6,28) /100.0, 5*0.0/ ! 100% branch to first channel
        DATA mdecay(1,28),mdecay(2,28),mdecay(3,28),mdecay(4,28),
     &  mdecay(5,28),mdecay(6,28) /1225,5*0/ ! first channel is K- nbar
        DATA amass(28)/1.540/         ! mass in GeV/c**2
        DATA charge(28)/-1.0/         ! -1
        DATA width(28)/3.e+05/        ! eV width (30 MeV hep-ph/0401187 v1))
        DATA itrtyp(28)/3/            ! track as a hadronic
        DATA PARTNAMES(28)/'THETA-$'/


c     AntiSigma- 100% branch to nbar + pi-
        DATA IPART(29)/PP_ANTISIGMA_MINUS/
        DATA bratio(1,29),bratio(2,29),bratio(3,29),bratio(4,29),
     &  bratio(5,29),bratio(6,29) /100.0, 5*0.0/ ! 100% branch to first channel
        DATA mdecay(1,29),mdecay(2,29),mdecay(3,29),mdecay(4,29),
     &  mdecay(5,29),mdecay(6,29) /925,5*0/ ! first channel is pi- nbar
        DATA amass(29)/1.18937/       ! mass in GeV/c**2
        DATA charge(29)/-1.0/         ! -1
        DATA width(29)/8.2379e-06/    ! gives lifetime as 7.99e-11 seconds
        DATA itrtyp(29)/3/            ! track as a hadronic
        DATA PARTNAMES(29)/'PISA_ANTISIGMA-$'/


c     eta 100% branch to gamma+gamma or Dalitz (no pion decays)
        DATA IPART(30)/PP_ETA_GG/
        DATA bratio(1,30),bratio(2,30),bratio(3,30),bratio(4,30),
     &  bratio(5,30),bratio(6,30) /98.4, 1.6,4*0.0/ ! 98.4% gamma+gamma
	                                            ! 1.60% Dalitz
        DATA mdecay(1,30),mdecay(2,30),mdecay(3,30),mdecay(4,30),
     &  mdecay(5,30),mdecay(6,30) /101,30201,4*0/   !
        DATA amass(30)/0.54775/       ! mass in GeV/c**2
        DATA charge(30)/0.0/          ! 0
        DATA width(30)/1.29e+03/      ! eV width gives lifetime 0.51024E-18 sec
        DATA itrtyp(30)/3/            ! track as a neutral: 3 / hadronic: 4
        DATA PARTNAMES(30)/'ETA_GG$'/

c     Rho (770) ---> pi+ pi- branch
        DATA IPART(31)/PP_RHO_PIPI/
        DATA bratio(1,31),bratio(2,31),bratio(3,31),bratio(4,31),
     &  bratio(5,31),bratio(6,31) /100.0, 5*0.0/ ! 100% branch to first channel
        DATA mdecay(1,31),mdecay(2,31),mdecay(3,31),mdecay(4,31),
     &  mdecay(5,31),mdecay(6,31) /809,5*0/ ! first channel is pi+ pi-
        DATA amass(31)/0.770/    ! mass in GeV/c**2
        DATA charge(31)/0.0/          ! neutral
        DATA width(31)/153.7e+06/      ! eV width
        DATA itrtyp(31)/3/            ! track as a weak
        DATA PARTNAMES(31)/'RHO_PIPI$'/

c     Omega (782) ---> hadronic channels
        DATA IPART(32)/PP_OME_HAD/
        DATA bratio(1,32),bratio(2,32),bratio(3,32),bratio(4,32),
     &  bratio(5,32),bratio(6,32) /100.0, 0.0, 0.0, 0.0, 0.0, 0.0/
        DATA mdecay(1,32),mdecay(2,32),mdecay(3,32),mdecay(4,32),
     &  mdecay(5,32),mdecay(6,32) /701, 0, 0, 0, 0, 0/
        DATA amass(32)/0.78259/       ! mass in GeV/c**2
        DATA charge(32)/0.0/          ! neutral
        DATA width(32)/8.49e+06/      ! eV width
        DATA itrtyp(32)/3/            ! track as a weak
        DATA PARTNAMES(32)/'OME_HAD$'/

c     Ks ---> pi0 + pi0 channels
        DATA IPART(33)/PP_KS_HAD/
        DATA bratio(1,33),bratio(2,33),bratio(3,33),bratio(4,33),
     &  bratio(5,33),bratio(6,33) /100.0, 0.0, 0.0, 0.0, 0.0, 0.0/
        DATA mdecay(1,33),mdecay(2,33),mdecay(3,33),mdecay(4,33),
     &  mdecay(5,33),mdecay(6,33) /707, 0, 0, 0, 0, 0/
        DATA amass(33)/0.497672/      ! mass in GeV/c**2
        DATA charge(33)/0.0/          ! neutral
        DATA width(33)/7.35e-06/      ! eV width (0.8953e-10 sec, PDG)
        DATA itrtyp(33)/3/            ! track as a weak
        DATA PARTNAMES(33)/'OME_KS$'/

c     Rho (770)+ ---> pi+ pi0 branch
        DATA IPART(34)/PP_RHO_PL/
        DATA bratio(1,34),bratio(2,34),bratio(3,34),bratio(4,34),
     &  bratio(5,34),bratio(6,34) /100.0, 5*0.0/ ! 100% branch to first channel
        DATA mdecay(1,34),mdecay(2,34),mdecay(3,34),mdecay(4,34),
     &  mdecay(5,34),mdecay(6,34) /807,5*0/ ! first channel is pi+ + pi0
        DATA amass(34)/0.770/    ! mass in GeV/c**2
        DATA charge(34)/0.0/          ! neutral
        DATA width(34)/153.7e+06/      ! eV width
        DATA itrtyp(34)/3/            ! track as a weak
        DATA PARTNAMES(34)/'RHO_PIPL$'/

c     Rho (770)- ---> pi- pi0 branch
        DATA IPART(35)/PP_RHO_MI/
        DATA bratio(1,35),bratio(2,35),bratio(3,35),bratio(4,35),
     &  bratio(5,35),bratio(6,35) /100.0, 5*0.0/ ! 100% branch to first channel
        DATA mdecay(1,35),mdecay(2,35),mdecay(3,35),mdecay(4,35),
     &  mdecay(5,35),mdecay(6,35) /907,5*0/ ! first channel is pi- + pi0
        DATA amass(35)/0.770/    ! mass in GeV/c**2
        DATA charge(35)/0.0/          ! neutral
        DATA width(35)/153.7e+06/      ! eV width
        DATA itrtyp(35)/3/            ! track as a weak
        DATA PARTNAMES(35)/'RHO_PIMI$'/

c     K*(892) zero ---> pi- k+, pi0 k0_s 
        DATA IPART(36)/PP_KSTARZ_KPI/
        DATA bratio(1,36),bratio(2,36),bratio(3,36),bratio(4,36),
     &  bratio(5,36),bratio(6,36) /66.6667,33.3333,4*0.0/ 
        DATA mdecay(1,36),mdecay(2,36),mdecay(3,36),mdecay(4,36),
     &  mdecay(5,36),mdecay(6,36) /911,716,4*0/  
        DATA amass(36)/0.896/         ! mass in GeV/c**2
        DATA charge(36)/0.0/          ! neutral
        DATA width(36)/50.3e+06/     ! eV width
        DATA itrtyp(36)/3/            
        DATA PARTNAMES(36)/'KSTARZ_KPI$'/

c     K*(892)zero BAR ---> pi+ K-, pi0 K0_s 
        DATA IPART(37)/PP_KSTARZB_KPI/
        DATA bratio(1,37),bratio(2,37),bratio(3,37),bratio(4,37),
     &  bratio(5,37),bratio(6,37) /66.6667,33.3333 ,4*0.0/ 
        DATA mdecay(1,37),mdecay(2,37),mdecay(3,37),mdecay(4,37),
     &  mdecay(5,37),mdecay(6,37) /812,716,4*0/  
        DATA amass(37)/0.896/         ! mass in GeV/c**2
        DATA charge(37)/0.0/          ! neutral
        DATA width(37)/50.3e+06/     ! eV width
        DATA itrtyp(37)/3/            
        DATA PARTNAMES(37)/'KSTARZB_KPI$'/

c     K*(892)+ ---> pi+ K0_s, pi0 K+ 
        DATA IPART(38)/PP_KSTARP_KPI/
        DATA bratio(1,38),bratio(2,38),bratio(3,38),bratio(4,38),
     &  bratio(5,38),bratio(6,38) /66.6667,33.3333,4*0.0/ 
        DATA mdecay(1,38),mdecay(2,38),mdecay(3,38),mdecay(4,38),
     &  mdecay(5,38),mdecay(6,38) /816,711,4*0/  
        DATA amass(38)/0.892/         ! mass in GeV/c**2
        DATA charge(38)/1.0/          ! positive
        DATA width(38)/50.9e+06/     ! eV width
        DATA itrtyp(38)/3/            
        DATA PARTNAMES(38)/'KSTARP_KPI$'/

c     K*(892)- ---> pi- K0_s, pi0 K- 
        DATA IPART(39)/PP_KSTARM_KPI/
        DATA bratio(1,39),bratio(2,39),bratio(3,39),bratio(4,39),
     &  bratio(5,39),bratio(6,39) /66.6667,33.3333 ,4*0.0/ 
        DATA mdecay(1,39),mdecay(2,39),mdecay(3,39),mdecay(4,39),
     &  mdecay(5,39),mdecay(6,39) /916,712,4*0/  
        DATA amass(39)/0.892/         ! mass in GeV/c**2
        DATA charge(39)/-1.0/          ! negative
        DATA width(39)/50.9e+06/     ! eV width
        DATA itrtyp(39)/3/            
        DATA PARTNAMES(39)/'KSTARM_KPI$'/

c     Delta+ ---> P pi0, N pi+ 
        DATA IPART(40)/PP_DELTA_PL/
        DATA bratio(1,40),bratio(2,40),bratio(3,40),bratio(4,40),
     &  bratio(5,40),bratio(6,40) /66.6667,33.3333,4*0.0/ 
        DATA mdecay(1,40),mdecay(2,40),mdecay(3,40),mdecay(4,40),
     &  mdecay(5,40),mdecay(6,40) /714,813,4*0/  
        DATA amass(40)/1.232/         ! mass in GeV/c**2
        DATA charge(40)/1.0/          ! positive
        DATA width(40)/118.0e+06/     ! eV width
        DATA itrtyp(40)/3/            
        DATA PARTNAMES(40)/'DELTA_NPIPL$'/

c     Delta+ BAR ---> P-bar pi0, N-bar pi-
        DATA IPART(41)/PP_DELTAB_PL/
        DATA bratio(1,41),bratio(2,41),bratio(3,41),bratio(4,41),
     &  bratio(5,41),bratio(6,41) /66.6667,33.3333,4*0.0/ 
        DATA mdecay(1,41),mdecay(2,41),mdecay(3,41),mdecay(4,41),
     &  mdecay(5,41),mdecay(6,41) /715,925,4*0/  
        DATA amass(41)/1.232/         ! mass in GeV/c**2
        DATA charge(41)/-1.0/          ! negative
        DATA width(41)/118.0e+06/     ! eV width
        DATA itrtyp(41)/3/            
        DATA PARTNAMES(41)/'DELTAB_NPIPL$'/

c     Delta- ---> N pi-
        DATA IPART(42)/PP_DELTA_MI/
        DATA bratio(1,42),bratio(2,42),bratio(3,42),bratio(4,42),
     &  bratio(5,42),bratio(6,42) /100.0,5*0.0/ 
        DATA mdecay(1,42),mdecay(2,42),mdecay(3,42),mdecay(4,42),
     &  mdecay(5,42),mdecay(6,42) /913,5*0/  
        DATA amass(42)/1.232/         ! mass in GeV/c**2
        DATA charge(42)/-1.0/          ! negative
        DATA width(42)/118.0e+06/     ! eV width
        DATA itrtyp(42)/3/            
        DATA PARTNAMES(42)/'DELTA_NPIMI$'/

c     Delta- BAR ---> N-bar pi+
        DATA IPART(43)/PP_DELTAB_MI/
        DATA bratio(1,43),bratio(2,43),bratio(3,43),bratio(4,43),
     &  bratio(5,43),bratio(6,43) /100.0,5*0.0/ 
        DATA mdecay(1,43),mdecay(2,43),mdecay(3,43),mdecay(4,43),
     &  mdecay(5,43),mdecay(6,43) /825,5*0/  
        DATA amass(43)/1.232/         ! mass in GeV/c**2
        DATA charge(43)/1.0/          ! positive
        DATA width(43)/118.0e+06/     ! eV width
        DATA itrtyp(43)/3/            
        DATA PARTNAMES(43)/'DELTAB_NPIMI$'/

c Executable Statements
	DO I = 1,NPISAPARTS
	   IF(WIDTH(I) .GT. 1.0E-10)THEN
C  Lifetime = planck/width
	      tlife = eV_to_sec/width(I) ! GEANT wants lifetime
	   ELSE
C  stable particle
	      TLIFE = 1.0E+15
	   END IF
           call gspart(IPART(I),PARTNAMES(I),ITRTYP(I),AMASS(I),
     1     CHARGE(I),tlife,ub,nwb)                         ! define particle
c  set decay branch(es)
           IF(BRATIO(1,I) .GE. 0.0)
     &     call gsdk(ipart(i),bratio(1,I),mdecay(1,I))
           call gppart(ipart(i))
        END DO


c    A.Ster changed Deuteron tracking routine type from GTHION to GTHADR

      CALL gfpart(45,partnames(1),itrtyp(1),amass(1),
     1               charge(1),tlife,ub,nnwb) 
      itrtyp(1) = 4 
      CALL gspart(45,partnames(1),itrtyp(1),amass(1),
     1               charge(1),tlife,ub,nwb)  

      return
      end
