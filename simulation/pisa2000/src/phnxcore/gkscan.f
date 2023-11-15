**
      SUBROUTINE GKSCAN
      INTEGER MGUIDL
      PARAMETER (MGUIDL=199)
      CHARACTER*80 GUID
      COMMON /KCGUID/ GUID(MGUIDL)
      EXTERNAL GXSCAN
 
      CALL KUNWG( 155)
      CALL KUCMD(' ','SCAN','C')
      GUID(  1)='To define parameters for the SCAN geomet'//
     +'ry. If the routine GUSTEP'
      GUID(  2)='and GUKINE are properly instrumented (se'//
     +'e examples in GEANX),'
      GUID(  3)='when the TRI command is entered NTETA Ge'//
     +'antinos will be'
      GUID(  4)='tracked through the real detector starti'//
     +'ng at the vertex position'
      GUID(  5)='defined by the command vertex. A simplif'//
     +'ied version of the geometry'
      GUID(  6)='is automatically generated in (ETA,PHI) '//
     +'or (THETA,PHI) following'
      GUID(  7)='the option given in the command TETA. Th'//
     +'e data structure LSCAN'
      GUID(  8)='generated may be saved on an RZ file for'//
     +' subsequent processing.'
      GUID(  9)='This data structure may be used for fast'//
     +' parametrization techniques.'
      CALL KUGUID('SCAN',GUID,  9,'S')
 
      CALL KUCMD('SCAN',' ','SW')
 
      CALL KUNWG(  32)
      CALL KUCMD(' ','PHI','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('PHI','NPHI','Number of PHI divisions','I','S')
      CALL KUPVAL('PHI','NPHI',90,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('PHI','PHIMIN','Minimum PHI in degrees','RO','S')
      CALL KUPVAL('PHI','PHIMIN',0,0.,' ','D')
      CALL KUPVAL('PHI','PHIMIN',0,1.,' ','L')
      CALL KUPVAL('PHI','PHIMIN',0,360.,' ','H')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('PHI','PHIMAX','Maximum PHI in degrees','RO','S')
      CALL KUPVAL('PHI','PHIMAX',0,360.,' ','D')
      CALL KUPVAL('PHI','PHIMAX',0,1.,' ','L')
      CALL KUPVAL('PHI','PHIMAX',0,360.,' ','H')
      GUID(  1)='To specify number of divisions along PHI'//
     +'. If no parameter is'
      GUID(  2)='given, the current values of the paramet'//
     +'ers are displayed.'
      CALL KUGUID('PHI',GUID,  2,'S')
      CALL KUACT('PHI',GXSCAN)
 
      CALL KUNWG(  73)
      CALL KUCMD(' ','TETA','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('TETA','NTETA','Number of TETA divisions','I','S')
      CALL KUPVAL('TETA','NTETA',90,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('TETA','TETMIN','Minimum value of TETA','RO','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('TETA','TETMAX','Maximum value of TETA','RO','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('TETA','DIVTYP','Type of TETA division','IO','S')
      CALL KUPVAL('TETA','DIVTYP',1,0.,' ','L')
      CALL KUPVAL('TETA','DIVTYP',3,0.,' ','H')
      GUID(  1)='To specify number of divisions along TET'//
     +'A.'
      GUID(  2)='If DIVTYP=1 divisions in pseudo-rapidity'//
     +' ETA.'
      GUID(  3)='If DIVTYP=2 divisions in degrees followi'//
     +'ng the THETA angle.'
      GUID(  4)='If DIVTYP=3 divisions in cos(TETA).'
      GUID(  5)='If no parameter is given, the current va'//
     +'lues of the parameters'
      GUID(  6)='are displayed.'
      CALL KUGUID('TETA',GUID,  6,'S')
      CALL KUACT('TETA',GXSCAN)
 
      CALL KUNWG(  61)
      CALL KUCMD(' ','SLIST','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('SLIST','LIST','List of master volumes','C','S')
      GUID(  1)='Only boundary crossings of volumes given'//
     +' in LIST will be seen'
      GUID(  2)='in the SCAN geometry. If no parameters a'//
     +'re given, the current'
      GUID(  3)='SCAN volumes will be listed. If a full s'//
     +'top (.) is given, the list'
      GUID(  4)='of scan volumes will be erased.'
      CALL KUGUID('SLIST',GUID,  4,'S')
      CALL KUACT('SLIST',GXSCAN)
 
      CALL KUNWG(  15)
      CALL KUCMD(' ','VERTEX','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('VERTEX','VX','Scan X-origin','R','S')
      CALL KUPVAL('VERTEX','VX',0,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('VERTEX','VY','Scan Y-origin','R','S')
      CALL KUPVAL('VERTEX','VY',0,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('VERTEX','VZ','Scan Z-origin','R','S')
      CALL KUPVAL('VERTEX','VZ',0,0.,' ','D')
      GUID(  1)='All Geantinos tracked will start from po'//
     +'sition VX,VY,VZ.'
      CALL KUGUID('VERTEX',GUID,  1,'S')
      CALL KUACT('VERTEX',GXSCAN)
 
      CALL KUNWG(  49)
      CALL KUCMD(' ','SFACTORS','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('SFACTORS','FACTX0','Scale factor for SX0','R','S')
      CALL KUPVAL('SFACTORS','FACTX0',0,100.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('SFACTORS','FACTL','Scale factor for SL','R','S')
      CALL KUPVAL('SFACTORS','FACTL',0,1000.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('SFACTORS','FACTR','Scale factor for R','R','S')
      CALL KUPVAL('SFACTORS','FACTR',0,100.,' ','D')
      GUID(  1)='Set scale factors for SX0,SL and R. The '//
     +'given scale factors must be'
      GUID(  2)='such that:'
      GUID(  3)='  SX0*FACTX0 < 2**15-1 (32767)'
      GUID(  4)='  SL*FACTL   < 2**10-1 (1023)'
      GUID(  5)='  SR*FACTR   < 2**17-1 (131071)'
      CALL KUGUID('SFACTORS',GUID,  5,'S')
      CALL KUACT('SFACTORS',GXSCAN)
 
      CALL KUNWG(  88)
      CALL KUCMD(' ','STURN','C')
      CALL KUNDPV(   1,   1,   1,   1,   3)
      CALL KUPAR('STURN','CHOPT','SCAN mode setting','C','S')
      CALL KUPVAL('STURN','CHOPT',0,0.,'ON,OFF,INIT','V')
      GUID(  1)='Switch on/off SCAN mode. If SCAN mode is'//
     +' on, SCAN geantinos'
      GUID(  2)='are generated and tracked to fill (or co'//
     +'mplete) the current'
      GUID(  3)='scan data structure. If SCAN mode is off'//
     +', normal kinematics'
      GUID(  4)='generation and tracking will take place.'//
     +' If INIT is given,'
      GUID(  5)='the current SCAN data structure (if any)'//
     +' will be dropped'
      GUID(  6)='and SCAN mode will be turned on.'
      CALL KUGUID('STURN',GUID,  6,'S')
      CALL KUACT('STURN',GXSCAN)
 
      CALL KUNWG(  90)
      CALL KUCMD(' ','PCUTS','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('PCUTS','IPARAM','Parametrization Flag','IO','S')
      CALL KUPVAL('PCUTS','IPARAM',0,0.,' ','L')
      CALL KUPVAL('PCUTS','IPARAM',1,0.,' ','H')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('PCUTS','PCUTGA','Parametrization Cut for gammas','RO',
     +'S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('PCUTS','PCUTEL','Parametrization Cut for electrons','R
     +O','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('PCUTS','PCUTHA','Parametrization Cut for charged hadro
     +ns','RO','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('PCUTS','PCUTNE','Parametrization Cut for neutral hadro
     +ns','RO','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('PCUTS','PCUTMU','Parametrization Cut for muons','RO'
     +,'S')
      GUID(  1)='Control parametrization at tracking time'//
     +'.'
      GUID(  2)='     IPARAM=0       No parametrization i'//
     +'s performed'
      GUID(  3)='     IPARAM=1       Parametrization is p'//
     +'erformed'
      GUID(  4)='If parametrization is active and a parti'//
     +'cle falls below its'
      GUID(  5)='parametrization cut, then the particle w'//
     +'ill be replaced by'
      GUID(  6)='a parametrized shower which will be trac'//
     +'ked in the SCAN'
      GUID(  7)='geometry.'
      CALL KUGUID('PCUTS',GUID,  7,'S')
      CALL KUACT('PCUTS',GXSCAN)
 
      CALL KUNWG( 108)
      CALL KUCMD(' ','LSCAN','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('LSCAN','ID','Lego plot identifier','I','S')
      CALL KUPVAL('LSCAN','ID',2000,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('LSCAN','VOLUME','Volume name','CO','S')
      CALL KUPVAL('LSCAN','VOLUME',0,0.,'XXXX','D')
      CALL KUNDPV(   1,   1,   1,   1,   3)
      CALL KUPAR('LSCAN','CHOPT','List of options','CO','S')
      CALL KUPVAL('LSCAN','CHOPT',0,0.,'OPX','D')
      CALL KUPVAL('LSCAN','CHOPT',0,0.,' ,O,P,I,X,L','V')
      GUID(  1)='Generates and plot a table of physics qu'//
     +'antities such as'
      GUID(  2)='the total number of radiation lengths or'//
     +' interaction lengths'
      GUID(  3)='in function of the SCAN parameters TETA,'//
     +'PHI.'
      GUID(  4)='  CHOPT=''O'' table is generated at Exit'//
     +'  of VOLUME.'
      GUID(  5)='  CHOPT=''I'' table is generated at Entr'//
     +'y of VOLUME.'
      GUID(  6)='  CHOPT=''X'' radiation lengths'
      GUID(  7)='  CHOPT=''L'' Interaction lengths'
      GUID(  8)='  CHOPT=''P'' Plot the table'
      GUID(  9)='If VOLUME=''XXXX'' Mother volume is used'//
     +'.'
      CALL KUGUID('LSCAN',GUID,  9,'S')
      CALL KUACT('LSCAN',GXSCAN)
 
      CALL KUNWG( 180)
      CALL KUCMD(' ','HSCAN','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('HSCAN','IDPHI','Histogram/phi identifier','I','S')
      CALL KUPVAL('HSCAN','IDPHI',1000,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('HSCAN','VOLUME','Volume name','CO','S')
      CALL KUPVAL('HSCAN','VOLUME',0,0.,'XXXX','D')
      CALL KUNDPV(   1,   1,   1,   1,   3)
      CALL KUPAR('HSCAN','CHOPT','List of options','CO','S')
      CALL KUPVAL('HSCAN','CHOPT',0,0.,'OPX','D')
      CALL KUPVAL('HSCAN','CHOPT',0,0.,' ,O,P,I,X,L','V')
      GUID(  1)='Generates and plots an histogram of phys'//
     +'ics quantities such as'
      GUID(  2)='the total number of radiation lengths or'//
     +' interaction lengths'
      GUID(  3)='as a function of the SCAN parameter TETA'//
     +' for a given value of PHI.'
      GUID(  4)='  CHOPT=''O'' histogram is generated at '//
     +'Exit  of VOLUME.'
      GUID(  5)='  CHOPT=''I'' histogram is generated at '//
     +'Entry of VOLUME.'
      GUID(  6)='  CHOPT=''X'' radiation lengths'
      GUID(  7)='  CHOPT=''L'' Interaction lengths'
      GUID(  8)='  CHOPT=''P'' Plot the histogram'
      GUID(  9)='If VOLUME=''XXXX'' Mother volume is used'//
     +'.'
      GUID( 10)='The histogram identifier IDPHI is used t'//
     +'o also identify which'
      GUID( 11)='PHI division to plot: IPHI=MOD(IDPHI,100'//
     +'0).'
      GUID( 12)='If IPHI=0, then all PHI divisions are ge'//
     +'nerated (not plotted)'
      GUID( 13)='with histogram identifiers IDPHI+PHI div'//
     +'ision number.'
      CALL KUGUID('HSCAN',GUID, 13,'S')
      CALL KUACT('HSCAN',GXSCAN)
 
      CALL KUNWG(   0)
 
      CALL KUNDPV(   1,   1,   1,   0,   1)
 
      CALL KUCMD(' ',' ','E')
 
      CALL KUCMD('/',' ','SW')
 
      END
