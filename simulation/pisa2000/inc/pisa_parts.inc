C Parameter list of particles specially defined by Pisa. All reference
C  to these particles should be through these names, not by explicit
C  reference to their numbers. Name is of the form PP_Name_DecayProducts
C where PP is short for Pisa Particle.
C
C      Feb. 12, 1997  C.F. Maguire  Added Phi ---> K+K- as 1063
C      Nov. 21, 1997  C.F. Maguire  Add minimum and maximum values
C      Feb. 18, 1999  C.F. Maguire  Add special K+K-, Phi ---> special K+K-
C      Jun. 14, 1999  C.F. Maguire  Add radiative chi decay channels for
C                                   chi(1P) ---> e+e- and u+u-, 3.51 GeV
C
C      Jul. 15, 2001  C.F. Maguire  Add "pi0" which has 100% Dalitz decay
C
C      May  29, 2002  C.F. Maguire  Add anti-deuteron
C
C      Jun. 16, 2002  C.F. Maguire  Add photon with forced "e+e- decay"
C
C      Dec. 29, 2002  C.F. Maguire  Add Lambda --> p + pi-, with c*tau = 0
C
C      Oct. 3, 2003   Chris Pinkenburg  Add theta+ and theta- as pid 1070/1080 
C
C      Jan. 24, 2004  C.F. Maguire  Add Anti-Sigma- -> nbar + pi- as pid 1069
C                     This is a 100% branch version of the GEANT particle 27
C
C      Oct. 27, 2004  F. Kajihara   Add eta -> gamma + gamma 58
C
C      Dec. 5, 2004   C.F. Maguire add rho(770) -> pi+ + pi- 
C
C      Feb. 11, 2005  R. Averbeck   Add omega meson decaying into hadrons
C				    particle PP_OME_HAD as pid 1072
C
C      March.30, 2006  V. Riabov    Add Ks meson decaying only in pi0 + pi0
C				    particle PP_KS_HAD as pid 1073
C
C      July 4, 2009   C.F. Maguire  Add rho(770)+ decaying into pi+ + pi0
C				    particle PP_RHO_PL as pid 1074
C
C      July 4, 2009   C.F. Maguire  Add rho(770)+ decaying into pi- + pi0
C				    particle PP_RHO_MI as pid 1075
C
C      July 14, 2009   D.P Roach    Add K*0, K*0-Bar, K*(892)+, K*(892)-,
C                                   Delta+, Delta-, Delta+ Bar, Delta- Bar
C                                   These additions for integration with HYDJET
C

	INTEGER PP_JPSI_MM,PP_JPSI_EE,PP_UP1S_MM,PP_UP2S_MM,
     &  PP_UP3S_MM,PP_PHI_MM,PP_PHI_EE,PP_OME_EE,PP_RHO_EE,
     &  PP_PSIP_MM,PP_PSIP_EE,PP_TRD_PHOT,PP_CRK_PHOT,
     &  PP_PHI_KK, PP_KPLUS_NODECAY, PP_KMINUS_NODECAY,
     &  PP_PHI_NDKK, PP_CHI_EE, PP_CHI_MM, PP_RJPSI_EE, PP_RJPSI_MM,
     &  PP_PIZ_DALITZ, PP_DBAR, PP_PHO_EE, PP_LAM_PRPIM,
     &  PP_ETA_GG,
     &  PP_D0_KMPIP,
     &  PP_THETA_PLUS, PP_THETA_MINUS, PP_ANTISIGMA_MINUS,
     &  PP_RHO_PIPI, PP_OME_HAD, PP_KS_HAD, PP_RHO_PL, PP_RHO_MI,
     &  PP_KSTARZ_KPI, PP_KSTARZB_KPI,
     &  PP_KSTARP_KPI, PP_KSTARM_KPI, PP_DELTA_PL, PP_DELTAB_PL,
     &  PP_DELTA_MI, PP_DELTAB_MI
 
	PARAMETER(PP_JPSI_MM = 1060,PP_JPSI_EE = 1050,
     &  PP_UP1S_MM = 1051,PP_UP2S_MM = 1052,PP_UP3S_MM = 1053,
     &  PP_PHI_MM = 1054,PP_PHI_EE = 1055,
     &  PP_OME_EE = 1056,PP_RHO_EE = 1057,
     &  PP_TRD_PHOT = 1058,PP_CRK_PHOT = 1059,
     &  PP_PSIP_MM = 1061,PP_PSIP_EE = 1062,
     &  PP_PHI_KK = 1063,
     &  PP_KPLUS_NODECAY = 51, PP_KMINUS_NODECAY = 52,
     &  PP_PHI_NDKK = 1064,
     &  PP_RJPSI_EE = 53, PP_RJPSI_MM = 54,
     &  PP_CHI_EE = 1065, PP_CHI_MM = 1066, PP_PIZ_DALITZ = 1067,
     &  PP_DBAR = 55, PP_PHO_EE = 56, PP_LAM_PRPIM = 57,
     &  PP_ETA_GG = 58,
     &	PP_D0_KMPIP = 1068, PP_THETA_PLUS = 1070, PP_THETA_MINUS = 1080,
     &  PP_ANTISIGMA_MINUS = 1069, PP_RHO_PIPI = 1071, 
     &  PP_OME_HAD = 1072, PP_KS_HAD = 1073,
     &  PP_RHO_PL = 1074, PP_RHO_MI = 1075,
     &  PP_KSTARZ_KPI = 1076, PP_KSTARZB_KPI = 1077,
     &  PP_KSTARP_KPI = 1078, PP_KSTARM_KPI = 1079, PP_DELTA_PL = 1081,
     &  PP_DELTAB_PL = 1082, PP_DELTA_MI = 1083, PP_DELTAB_MI = 1084)
     
C
C    UPDATE MINIMUM AND MAXIMUM AS NECESSARY
C    USED IN EVENT_FILTER ROUTINE
C
        INTEGER PISA_PART_MIN, PISA_PART_MAX 
        PARAMETER (PISA_PART_MIN = 1050)
        PARAMETER (PISA_PART_MAX = 1085)

