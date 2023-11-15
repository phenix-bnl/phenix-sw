**
      SUBROUTINE GKPHYS
      INTEGER MGUIDL
      PARAMETER (MGUIDL=199)
      CHARACTER*80 GUID
      COMMON /KCGUID/ GUID(MGUIDL)
      EXTERNAL GXPHYS
 
      CALL KUNWG(  10)
      CALL KUCMD(' ','PHYSICS','C')
      GUID(  1)='Commands to set physics parameters.'
      CALL KUGUID('PHYSICS',GUID,  1,'S')
 
      CALL KUCMD('PHYSICS',' ','SW')
 
      CALL KUNWG(  41)
      CALL KUCMD(' ','ANNI','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('ANNI','IANNI','Flag IANNI','IO','S')
      CALL KUPVAL('ANNI','IANNI',1,0.,' ','D')
      CALL KUPVAL('ANNI','IANNI',0,0.,' ','V')
      CALL KUPVAL('ANNI','IANNI',1,0.,' ','V')
      CALL KUPVAL('ANNI','IANNI',2,0.,' ','V')
      GUID(  1)='To control positron annihilation.'
      GUID(  2)=' IANNI=0 no annihilation'
      GUID(  3)='      =1 annihilation. Decays processed.'
      GUID(  4)='      =2 annihilation. No decay products'//
     +' stored.'
      CALL KUGUID('ANNI',GUID,  4,'S')
      CALL KUACT('ANNI',GXPHYS)
 
      CALL KUNWG(  40)
      CALL KUCMD(' ','BREM','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('BREM','IBREM','Flag IBREM','IO','S')
      CALL KUPVAL('BREM','IBREM',1,0.,' ','D')
      CALL KUPVAL('BREM','IBREM',0,0.,' ','V')
      CALL KUPVAL('BREM','IBREM',1,0.,' ','V')
      CALL KUPVAL('BREM','IBREM',2,0.,' ','V')
      GUID(  1)='To control bremstrahlung.'
      GUID(  2)=' IBREM=0 no bremstrahlung'
      GUID(  3)='      =1 bremstrahlung. Photon processed'//
     +'.'
      GUID(  4)='      =2 bremstrahlung. No photon stored'//
     +'.'
      CALL KUGUID('BREM',GUID,  4,'S')
      CALL KUACT('BREM',GXPHYS)
 
      CALL KUNWG(  37)
      CALL KUCMD(' ','COMP','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('COMP','ICOMP','Flag ICOMP','IO','S')
      CALL KUPVAL('COMP','ICOMP',1,0.,' ','D')
      CALL KUPVAL('COMP','ICOMP',0,0.,' ','V')
      CALL KUPVAL('COMP','ICOMP',1,0.,' ','V')
      CALL KUPVAL('COMP','ICOMP',2,0.,' ','V')
      GUID(  1)='To control Compton scattering'
      GUID(  2)=' ICOMP=0 no Compton'
      GUID(  3)='      =1 Compton. Electron processed.'
      GUID(  4)='      =2 Compton. No electron stored.'
      CALL KUGUID('COMP',GUID,  4,'S')
      CALL KUACT('COMP',GXPHYS)
 
      CALL KUNWG(  36)
      CALL KUCMD(' ','DCAY','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DCAY','IDCAY','Flag IDCAY','IO','S')
      CALL KUPVAL('DCAY','IDCAY',1,0.,' ','D')
      CALL KUPVAL('DCAY','IDCAY',0,0.,' ','V')
      CALL KUPVAL('DCAY','IDCAY',1,0.,' ','V')
      CALL KUPVAL('DCAY','IDCAY',2,0.,' ','V')
      GUID(  1)='To control Decay mechanism.'
      GUID(  2)=' IDCAY=0 no decays.'
      GUID(  3)='      =1 Decays. secondaries processed.'
      GUID(  4)='      =2 Decays. No secondaries stored.'
      CALL KUGUID('DCAY',GUID,  4,'S')
      CALL KUACT('DCAY',GXPHYS)
 
      CALL KUNWG(  40)
      CALL KUCMD(' ','DRAY','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DRAY','IDRAY','Flag IDRAY','IO','S')
      CALL KUPVAL('DRAY','IDRAY',1,0.,' ','D')
      CALL KUPVAL('DRAY','IDRAY',0,0.,' ','V')
      CALL KUPVAL('DRAY','IDRAY',1,0.,' ','V')
      CALL KUPVAL('DRAY','IDRAY',2,0.,' ','V')
      GUID(  1)='To control delta rays mechanism.'
      GUID(  2)=' IDRAY=0 no delta rays.'
      GUID(  3)='      =1 Delta rays. secondaries process'//
     +'ed.'
      GUID(  4)='      =2 Delta rays. No secondaries stor'//
     +'ed.'
      CALL KUGUID('DRAY',GUID,  4,'S')
      CALL KUACT('DRAY',GXPHYS)
 
      CALL KUNWG(  50)
      CALL KUCMD(' ','HADR','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('HADR','IHADR','Flag IHADR','IO','S')
      CALL KUPVAL('HADR','IHADR',1,0.,' ','D')
      GUID(  1)='To control hadronic interactions.'
      GUID(  2)=' IHADR=0 no hadronic interactions.'
      GUID(  3)='      =1 Hadronic interactions. secondar'//
     +'ies processed.'
      GUID(  4)='      =2 Hadronic interactions. No secon'//
     +'daries stored.'
      CALL KUGUID('HADR',GUID,  4,'S')
      CALL KUACT('HADR',GXPHYS)
 
      CALL KUNWG(  83)
      CALL KUCMD(' ','LOSS','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('LOSS','ILOSS','Flag ILOSS','IO','S')
      CALL KUPVAL('LOSS','ILOSS',2,0.,' ','D')
      CALL KUPVAL('LOSS','ILOSS',0,0.,' ','V')
      CALL KUPVAL('LOSS','ILOSS',1,0.,' ','V')
      CALL KUPVAL('LOSS','ILOSS',2,0.,' ','V')
      CALL KUPVAL('LOSS','ILOSS',3,0.,' ','V')
      CALL KUPVAL('LOSS','ILOSS',4,0.,' ','V')
      GUID(  1)='To control energy loss.'
      GUID(  2)=' ILOSS=0 no energy loss.'
      GUID(  3)='      =1 Energy loss.'
      GUID(  4)='      =2 Landau only.'
      GUID(  5)='      =3 Energy loss + Landau.'
      GUID(  6)='      =4 Average energy loss and no fluc'//
     +'tuations.'
      GUID(  7)='If the value ILOSS is changed, then cros'//
     +'s-sections and energy loss'
      GUID(  8)='tables must be recomputed via the comman'//
     +'d "PHYSI''.'
      CALL KUGUID('LOSS',GUID,  8,'S')
      CALL KUACT('LOSS',GXPHYS)
 
      CALL KUNWG(  49)
      CALL KUCMD(' ','MULS','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('MULS','IMULS','Flag IMULS','IO','S')
      CALL KUPVAL('MULS','IMULS',1,0.,' ','D')
      CALL KUPVAL('MULS','IMULS',0,0.,' ','V')
      CALL KUPVAL('MULS','IMULS',1,0.,' ','V')
      CALL KUPVAL('MULS','IMULS',2,0.,' ','V')
      CALL KUPVAL('MULS','IMULS',3,0.,' ','V')
      GUID(  1)='To control multiple scattering.'
      GUID(  2)=' IMULS=0 no multiple scattering.'
      GUID(  3)='      =1 Moliere or Coulomb scattering.'
      GUID(  4)='      =2 Moliere or Coulomb scattering.'
      GUID(  5)='      =3 Gaussian scattering.'
      CALL KUGUID('MULS',GUID,  5,'S')
      CALL KUACT('MULS',GXPHYS)
 
      CALL KUNWG(  53)
      CALL KUCMD(' ','MUNU','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('MUNU','IMUNU','Flag IMUNU','IO','S')
      CALL KUPVAL('MUNU','IMUNU',1,0.,' ','D')
      CALL KUPVAL('MUNU','IMUNU',0,0.,' ','V')
      CALL KUPVAL('MUNU','IMUNU',1,0.,' ','V')
      CALL KUPVAL('MUNU','IMUNU',2,0.,' ','V')
      GUID(  1)='To control muon nuclear interactions.'
      GUID(  2)=' IMUNU=0 no muon-nuclear interactions.'
      GUID(  3)='      =1 Nuclear interactions. Secondari'//
     +'es processed.'
      GUID(  4)='      =2 Nuclear interactions. Secondari'//
     +'es not processed.'
      CALL KUGUID('MUNU',GUID,  4,'S')
      CALL KUACT('MUNU',GXPHYS)
 
      CALL KUNWG(  45)
      CALL KUCMD(' ','PAIR','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('PAIR','IPAIR','Flag IPAIR','IO','S')
      CALL KUPVAL('PAIR','IPAIR',1,0.,' ','D')
      CALL KUPVAL('PAIR','IPAIR',0,0.,' ','V')
      CALL KUPVAL('PAIR','IPAIR',1,0.,' ','V')
      CALL KUPVAL('PAIR','IPAIR',2,0.,' ','V')
      GUID(  1)='To control pair production mechanism.'
      GUID(  2)=' IPAIR=0 no pair production.'
      GUID(  3)='      =1 Pair production. secondaries pr'//
     +'ocessed.'
      GUID(  4)='      =2 Pair production. No secondaries'//
     +' stored.'
      CALL KUGUID('PAIR',GUID,  4,'S')
      CALL KUACT('PAIR',GXPHYS)
 
      CALL KUNWG(  44)
      CALL KUCMD(' ','PFIS','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('PFIS','IPFIS','Flag IPFIS','IO','S')
      CALL KUPVAL('PFIS','IPFIS',1,0.,' ','D')
      CALL KUPVAL('PFIS','IPFIS',0,0.,' ','V')
      CALL KUPVAL('PFIS','IPFIS',1,0.,' ','V')
      CALL KUPVAL('PFIS','IPFIS',2,0.,' ','V')
      GUID(  1)='To control photo fission mechanism.'
      GUID(  2)=' IPFIS=0 no photo fission.'
      GUID(  3)='      =1 Photo fission. secondaries proc'//
     +'essed.'
      GUID(  4)='      =2 Photo fission. No secondaries s'//
     +'tored.'
      CALL KUGUID('PFIS',GUID,  4,'S')
      CALL KUACT('PFIS',GXPHYS)
 
      CALL KUNWG(  41)
      CALL KUCMD(' ','PHOT','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('PHOT','IPHOT','Flag IPHOT','IO','S')
      CALL KUPVAL('PHOT','IPHOT',1,0.,' ','D')
      CALL KUPVAL('PHOT','IPHOT',0,0.,' ','V')
      CALL KUPVAL('PHOT','IPHOT',1,0.,' ','V')
      CALL KUPVAL('PHOT','IPHOT',2,0.,' ','V')
      GUID(  1)='To control Photo effect.'
      GUID(  2)=' IPHOT=0 no photo electric effect.'
      GUID(  3)='      =1 Photo effect. Electron processe'//
     +'d.'
      GUID(  4)='      =2 Photo effect. No electron store'//
     +'d.'
      CALL KUGUID('PHOT',GUID,  4,'S')
      CALL KUACT('PHOT',GXPHYS)
 
      CALL KUNWG(  24)
      CALL KUCMD(' ','RAYL','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('RAYL','IRAYL','Flag IRAYL','IO','S')
      CALL KUPVAL('RAYL','IRAYL',1,0.,' ','D')
      CALL KUPVAL('RAYL','IRAYL',0,0.,' ','V')
      CALL KUPVAL('RAYL','IRAYL',1,0.,' ','V')
      GUID(  1)='To control Rayleigh scattering.'
      GUID(  2)=' IRAYL=0 no Rayleigh scattering.'
      GUID(  3)='      =1 Rayleigh.'
      CALL KUGUID('RAYL',GUID,  3,'S')
      CALL KUACT('RAYL',GXPHYS)
 
      CALL KUNWG( 107)
      CALL KUCMD(' ','CUTS','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('CUTS','CUTGAM','Cut for gammas','RO','S')
      CALL KUPVAL('CUTS','CUTGAM',0, 0.001,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('CUTS','CUTELE','Cut for electrons','RO','S')
      CALL KUPVAL('CUTS','CUTELE',0, 0.001,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('CUTS','CUTHAD','Cut for charged hadrons','RO','S')
      CALL KUPVAL('CUTS','CUTHAD',0, 0.01,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('CUTS','CUTNEU','Cut for neutral hadrons','RO','S')
      CALL KUPVAL('CUTS','CUTNEU',0, 0.01,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('CUTS','CUTMUO','Cut for muons','RO','S')
      CALL KUPVAL('CUTS','CUTMUO',0, 0.01,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('CUTS','BCUTE','Cut for electron brems.','RO','S')
      CALL KUPVAL('CUTS','BCUTE',0, -1.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('CUTS','BCUTM','Cut for muon Brems.','RO','S')
      CALL KUPVAL('CUTS','BCUTM',0, -1.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('CUTS','DCUTE','Cut for electron delta-rays','RO','S')
      CALL KUPVAL('CUTS','DCUTE',0, -1.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('CUTS','DCUTM','Cut for muon delta-rays','RO','S')
      CALL KUPVAL('CUTS','DCUTM',0, -1.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('CUTS','PPCUTM','Cut for e+e- pairs by muons','RO','S')
      CALL KUPVAL('CUTS','PPCUTM',0, 0.01,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('CUTS','TOFMAX','Time of flight cut','RO','S')
      CALL KUPVAL('CUTS','TOFMAX',0, 1.E+10,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('CUTS','GCUTS','5 user words','RO','S')
      CALL KUPVAL('CUTS','GCUTS',0, 0.,' ','D')
      GUID(  1)='To change physics cuts. If no parameter '//
     +'is given, the list'
      GUID(  2)='of the current cuts is printed.'
      GUID(  3)=' If the default values (-1.) for       B'//
     +'CUTE ,BCUTM ,DCUTE ,DCUTM'
      GUID(  4)=' are not modified, they will be set to C'//
     +'UTGAM,CUTGAM,CUTELE,CUTELE'
      GUID(  5)=' respectively.'
      GUID(  6)='If one of the parameters from CUTGAM to '//
     +'PPCUTM included'
      GUID(  7)='is modified, cross-sections and energy l'//
     +'oss tables must be'
      GUID(  8)='recomputed via the command ''PHYSI''.'
      CALL KUGUID('CUTS',GUID,  8,'S')
      CALL KUACT('CUTS',GXPHYS)
 
      CALL KUNWG(  48)
      CALL KUCMD(' ','PHYSI','C')
      GUID(  1)='Call the GEANT initialisation routine GP'//
     +'HYSI to recompute'
      GUID(  2)='the tables of cross-sections and energy '//
     +'loss. This command'
      GUID(  3)='must be invoked when CUTs or LOSS parame'//
     +'ters are changed.'
      CALL KUGUID('PHYSI',GUID,  3,'S')
      CALL KUACT('PHYSI',GXPHYS)
 
      CALL KUNWG(   0)
 
      CALL KUNDPV(   1,   1,   1,   0,   1)
 
      CALL KUCMD(' ',' ','E')
 
      CALL KUCMD('/',' ','SW')
 
      END
