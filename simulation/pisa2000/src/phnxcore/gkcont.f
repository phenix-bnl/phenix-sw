 
 
**
      SUBROUTINE GKCONT
      INTEGER MGUIDL
      PARAMETER (MGUIDL=199)
      CHARACTER*80 GUID
      COMMON /KCGUID/ GUID(MGUIDL)
      EXTERNAL GXCONT
 
      CALL KUNWG(   6)
      CALL KUCMD(' ','CONTROL','C')
      GUID(  1)='Control commands.'
      CALL KUGUID('CONTROL',GUID,  1,'S')
 
      CALL KUCMD('CONTROL',' ','SW')
 
      CALL KUNWG(  13)
      CALL KUCMD(' ','KINE','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('KINE','IKINE','IKINE','I','S')
      CALL KUPVAL('KINE','IKINE',1,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('KINE','PKINE1','PKINE(1)','RO','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('KINE','PKINE2','PKINE(2)','RO','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('KINE','PKINE3','PKINE(3)','RO','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('KINE','PKINE4','PKINE(4)','RO','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('KINE','PKINE5','PKINE(5)','RO','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('KINE','PKINE6','PKINE(6)','RO','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('KINE','PKINE7','PKINE(7)','RO','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('KINE','PKINE8','PKINE(8)','RO','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('KINE','PKINE9','PKINE(9)','RO','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('KINE','PKINE10','PKINE(10)','RO','S')
      GUID(  1)='Set the variables in /GCFLAG/ IKINE, PKI'//
     +'NE(10)'
      CALL KUGUID('KINE',GUID,  1,'S')
      CALL KUACT('KINE',GXCONT)
 
      CALL KUNWG(  12)
      CALL KUCMD(' ','PATR','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('PATR','NJTMAX','NJTMAX','IO','S')
      CALL KUPVAL('PATR','NJTMAX',2000,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('PATR','NJTMIN','NJTMIN','IO','S')
      CALL KUPVAL('PATR','NJTMIN',1900,0.,' ','D')
      GUID(  1)='To activate the option parallel tracking'//
     +'.'
      CALL KUGUID('PATR',GUID,  1,'S')
      CALL KUACT('PATR',GXCONT)
 
      CALL KUNWG(  22)
      CALL KUCMD(' ','TRACK','C')
      GUID(  1)='Restart tracking, clearing the track and'//
     +' hit'
      GUID(  2)='banks, but keeping the kinematics.'
      CALL KUGUID('TRACK',GUID,  2,'S')
      CALL KUACT('TRACK',GXCONT)
 
      CALL KUNWG(   9)
      CALL KUCMD(' ','TRIGGER','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('TRIGGER','N','Number of events','IO','S')
      CALL KUPVAL('TRIGGER','N',1,0.,' ','D')
      GUID(  1)='Start one or more new events.'
      CALL KUGUID('TRIGGER',GUID,  1,'S')
      CALL KUACT('TRIGGER',GXCONT)
 
      CALL KUNWG(  28)
      CALL KUCMD(' ','RNDM','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('RNDM','ISEED1','First seed for the random number gener
     +ator','I','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('RNDM','ISEED2','Second seed for the random number gene
     +rator','I','S')
      GUID(  1)='Set the seeds for the random number gene'//
     +'rator. If no numbers are'
      GUID(  2)='given, the currents seeds are printed.'
      CALL KUGUID('RNDM',GUID,  2,'S')
      CALL KUACT('RNDM',GXCONT)
 
      CALL KUNWG(  14)
      CALL KUCMD(' ','SWITCH','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('SWITCH','ISWI','Switch number','I','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('SWITCH','IVAL','New switch value','I','S')
      GUID(  1)='Change one element of array ISWIT(10) in'//
     +' /GCFLAG/'
      CALL KUGUID('SWITCH',GUID,  1,'S')
      CALL KUACT('SWITCH',GXCONT)
 
      CALL KUNWG(  71)
      CALL KUCMD(' ','MZLOGL','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('MZLOGL','LEVEL','MZ log level','I','S')
      CALL KUPVAL('MZLOGL','LEVEL',0,0.,' ','D')
      GUID(  1)='Set the log level for the MZ package of '//
     +'ZEBRA: CALL MZLOGL(0,level)'
      GUID(  2)=' LEVEL = -3   no messages at all'
      GUID(  3)='         -2   error messages only'
      GUID(  4)='         -1   terse logging'
      GUID(  5)='          0   normal'
      GUID(  6)='         +1   log rare events'
      GUID(  7)='         +2   log calls to MZ routines'
      CALL KUGUID('MZLOGL',GUID,  7,'S')
      CALL KUACT('MZLOGL',GXCONT)
 
      CALL KUNWG(   8)
      CALL KUCMD(' ','PRINT','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('PRINT','NAME','Name','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('PRINT','NUMBER','Number','I','S')
      CALL KUPVAL('PRINT','NUMBER',0,0.,' ','D')
      GUID(  1)=' CALL GPRINT(name,number)'
      CALL KUGUID('PRINT',GUID,  1,'S')
      CALL KUACT('PRINT',GXCONT)
 
      CALL KUNWG(  26)
      CALL KUCMD(' ','OUTPUT_LP','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('OUTPUT_LP','LOUT','New output unit','I','S')
      GUID(  1)='To change lout in /GCUNIT/'
      GUID(  2)='Note: unit numbers 5,11,12,13,14,15 are '//
     +'reserved and cannot be used.'
      CALL KUGUID('OUTPUT_LP',GUID,  2,'S')
      CALL KUACT('OUTPUT_LP',GXCONT)
 
      CALL KUNWG(   8)
      CALL KUCMD(' ','PHITS','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('PHITS','IUSET','User set','CO','S')
      CALL KUPVAL('PHITS','IUSET',0,0.,'*','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('PHITS','IUDET','User detector','CO','S')
      CALL KUPVAL('PHITS','IUDET',0,0.,'*','D')
      GUID(  1)=' CALL GPHITS(iuset,iudet)'
      CALL KUGUID('PHITS',GUID,  1,'S')
      CALL KUACT('PHITS',GXCONT)
 
      CALL KUNWG(   8)
      CALL KUCMD(' ','PDIGI','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('PDIGI','IUSET','User set','CO','S')
      CALL KUPVAL('PDIGI','IUSET',0,0.,'*','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('PDIGI','IUDET','User detector','CO','S')
      CALL KUPVAL('PDIGI','IUDET',0,0.,'*','D')
      GUID(  1)=' CALL GPDIGI(iuset,iudet)'
      CALL KUGUID('PDIGI',GUID,  1,'S')
      CALL KUACT('PDIGI',GXCONT)
 
      CALL KUNWG(  12)
      CALL KUCMD(' ','PRMAT','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('PRMAT','IMATE','Material number','I','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('PRMAT','IPART','Particle number','I','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('PRMAT','MECAN','Mechanism','C','S')
      GUID(  1)=' CALL GPRMAT(imate,ipart,mecan,nekbin,el'//
     +'ow)'
      CALL KUGUID('PRMAT',GUID,  1,'S')
      CALL KUACT('PRMAT',GXCONT)
 
      CALL KUNWG(  77)
      CALL KUCMD(' ','PLMAT','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('PLMAT','IMATE','Material number','I','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('PLMAT','IPART','Particle number','I','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('PLMAT','MECAN','Mechanism','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('PLMAT','IDM','ID mode option','IO','S')
      CALL KUPVAL('PLMAT','IDM',0,0.,' ','D')
      GUID(  1)='CALL GPLMAT(imate,ipart,mecan,nekbin,elo'//
     +'w,idm)'
      GUID(  2)=' If MECAN = ''ALLG'' the graphical optio'//
     +'n is selected'
      GUID(  3)=' IDM convention for histogramming mode :'
      GUID(  4)=' IDM.gt.0  fill, print,   keep   histogr'//
     +'am(s)'
      GUID(  5)=' IDM.eq.0  fill, print,   delete histogr'//
     +'am(s)'
      GUID(  6)=' IDM.lt.0  fill, noprint, keep   histogr'//
     +'am(s)'
      CALL KUGUID('PLMAT',GUID,  6,'S')
      CALL KUACT('PLMAT',GXCONT)
 
      CALL KUNWG(  31)
      CALL KUCMD(' ','DEBUG','C')
      CALL KUNDPV(   1,   1,   1,   1,   2)
      CALL KUPAR('DEBUG','IDEB','Debug option','CO','S')
      CALL KUPVAL('DEBUG','IDEB',0,0.,'ON','D')
      CALL KUPVAL('DEBUG','IDEB',0,0.,'ON,OFF','V')
      GUID(  1)='If ideb=''ON  '' then :'
      GUID(  2)=' idebug=1, idemin=1, idemax=1000000, iti'//
     +'me=1'
      GUID(  3)='else :'
      GUID(  4)=' idebug=0, idemin=0, idemax=0'
      CALL KUGUID('DEBUG',GUID,  4,'S')
      CALL KUACT('DEBUG',GXCONT)
 
      CALL KUNWG(   0)
 
      CALL KUNDPV(   1,   1,   1,   0,   1)
 
      CALL KUCMD(' ',' ','E')
 
      CALL KUCMD('/',' ','SW')
 
      END
