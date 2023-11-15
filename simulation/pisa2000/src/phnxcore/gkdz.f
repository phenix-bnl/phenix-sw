**
      SUBROUTINE GKDZ
      INTEGER MGUIDL
      PARAMETER (MGUIDL=199)
      CHARACTER*80 GUID
      COMMON /KCGUID/ GUID(MGUIDL)
      EXTERNAL GXDZ
 
      CALL KUNWG(   0)
      CALL KUCMD(' ','DZ','C')
 
      CALL KUCMD('DZ',' ','SW')
 
      CALL KUNWG(  16)
      CALL KUCMD(' ','SURV','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('SURV','NAME','Bank name','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('SURV','NUMBER','Bank number','IO','S')
      CALL KUPVAL('SURV','NUMBER',1,0.,' ','D')
      GUID(  1)='Print a survey of the structure identifi'//
     +'ed by NAME, NUMBER.'
      CALL KUGUID('SURV',GUID,  1,'S')
      CALL KUACT('SURV',GXDZ)
 
      CALL KUNWG( 157)
      CALL KUCMD(' ','SHOW','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('SHOW','NAME','Bank name','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('SHOW','NUMBER','Bank number','IO','S')
      CALL KUPVAL('SHOW','NUMBER',1,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('SHOW','CHOPT','Options','CO','S')
      CALL KUPVAL('SHOW','CHOPT',0,0.,'BSV','D')
      GUID(  1)='Display the contents of a bank or a data'//
     +' structure'
      GUID(  2)='identified by its NAME and NUMBER.'
      GUID(  3)='The output format of the data part is co'//
     +'ntrolled by the internal'
      GUID(  4)='or external I/O characteristic.'
      GUID(  5)=' CHOPT=''B'' Print the bank.'
      GUID(  6)=' CHOPT=''S'' Print the bank contents fro'//
     +'m left to right Sideways'
      GUID(  7)='           with up to ten elements per l'//
     +'ine.'
      GUID(  8)=' CHOPT=''V'' Print the vertical (down) s'//
     +'tructure.'
      GUID(  9)=' CHOPT=''D'' Print the bank contents fro'//
     +'m top to bottom Downwards'
      GUID( 10)='           with five elements per line.'
      GUID( 11)=' CHOPT=''L'' Print the linear structure.'
      GUID( 12)=' CHOPT=''Z'' Print the data part of each'//
     +' bank in hexadecimal format'
      CALL KUGUID('SHOW',GUID, 12,'S')
      CALL KUACT('SHOW',GXDZ)
 
      CALL KUNWG( 176)
      CALL KUCMD(' ','SNAP','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('SNAP','IDIV','Division number','IO','S')
      CALL KUPVAL('SNAP','IDIV',2,0.,' ','D')
      CALL KUPVAL('SNAP','IDIV',0,0.,' ','L')
      CALL KUPVAL('SNAP','IDIV',24,0.,' ','H')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('SNAP','CHOPT','Options','CO','S')
      CALL KUPVAL('SNAP','CHOPT',0,0.,'M','D')
      GUID(  1)='Snap of one or more divisions.'
      GUID(  2)='Provides a snapshot of one or more divis'//
     +'ions in a ZEBRA store.'
      GUID(  3)='The kind of information provided is cont'//
     +'rolled by CHOPT.'
      GUID(  4)=' CHOPT=''M'' Print Map entry for each ba'//
     +'nk'
      GUID(  5)=' CHOPT=''E'' Extend map entry to dump al'//
     +'l links of each bank'
      GUID(  6)='           (otherwise only as many links'//
     +' as will fit on a line)'
      GUID(  7)=' CHOPT=''F'' Full. Dump all active banks'//
     +', links and data'
      GUID(  8)=' CHOPT=''K'' Kill. Dropped banks to be t'//
     +'reated as active'
      GUID(  9)='           (dropped banks are not normal'//
     +'ly dumped under D or F option)'
      GUID( 10)=' CHOPT=''L'' Dump all Link areas associa'//
     +'ted with the store'
      GUID( 11)=' CHOPT=''W'' Dump the Working space, lin'//
     +'ks and data'
      GUID( 12)=' CHOPT=''Z'' Dump the information in hex'//
     +'adecimal.'
      CALL KUGUID('SNAP',GUID, 12,'S')
      CALL KUACT('SNAP',GXDZ)
 
      CALL KUNWG( 104)
      CALL KUCMD(' ','VERIFY','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('VERIFY','IDIV','Division number','IO','S')
      CALL KUPVAL('VERIFY','IDIV',0,0.,' ','D')
      CALL KUPVAL('VERIFY','IDIV',0,0.,' ','L')
      CALL KUPVAL('VERIFY','IDIV',24,0.,' ','H')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('VERIFY','CHOPT','Options','CO','S')
      CALL KUPVAL('VERIFY','CHOPT',0,0.,'CLSU','D')
      GUID(  1)='Check the structure of one or more ZEBRA'//
     +' divisions.'
      GUID(  2)='The verification detail depends on the s'//
     +'ettings in CHOPT.'
      GUID(  3)=' CHOPT=''C'' Check chaining of banks onl'//
     +'y'
      GUID(  4)=' CHOPT=''L'' Check validity of the struc'//
     +'tural links (implies ''C'')'
      GUID(  5)=' CHOPT=''S'' Check the store parameters'
      GUID(  6)=' CHOPT=''U'' Check the validity of the u'//
     +'p and origin (implies ''C'')'
      GUID(  7)=' CHOPT=''F'' Errors are considered fatal'//
     +' and generate a call to ZFATAL'
      CALL KUGUID('VERIFY',GUID,  7,'S')
      CALL KUACT('VERIFY',GXDZ)
 
      CALL KUNWG(  53)
      CALL KUCMD(' ','STORE','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('STORE','IXSTOR','Store number','IO','S')
      CALL KUPVAL('STORE','IXSTOR',0,0.,' ','D')
      CALL KUPVAL('STORE','IXSTOR',0,0.,' ','L')
      CALL KUPVAL('STORE','IXSTOR',24,0.,' ','H')
      GUID(  1)='Display the structure of the ZEBRA store'//
     +' IXSTOR.'
      GUID(  2)='Output the parameters characterizing the'//
     +' store, followed by a'
      GUID(  3)='list of all divisions and all link areas'//
     +' associated with the store in'
      GUID(  4)='question.'
      CALL KUGUID('STORE',GUID,  4,'S')
      CALL KUACT('STORE',GXDZ)
 
      CALL KUNWG(  28)
      CALL KUCMD(' ','DDIV','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DDIV','IDIV','Division number','IO','S')
      CALL KUPVAL('DDIV','IDIV',2,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DDIV','PATH','Name of the doc file','CO','S')
      CALL KUPVAL('DDIV','PATH',0,0.,' ','D')
      GUID(  1)='Facility to display the layout of stores'//
     +' and divisions.'
      GUID(  2)=' CALL DZDDIV(idiv,LDUMMY,path,''IN'',1,0'//
     +',1,IWTYPE)'
      CALL KUGUID('DDIV',GUID,  2,'S')
      CALL KUACT('DDIV',GXDZ)
 
      CALL KUNWG(  22)
      CALL KUCMD(' ','DISP','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DISP','BANK','Name of the bank','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DISP','PATH','Name of the doc file','CO','S')
      CALL KUPVAL('DISP','PATH',0,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DISP','NUMBER','Number of the bank','IO','S')
      CALL KUPVAL('DISP','NUMBER',1,0.,' ','D')
      GUID(  1)='Interactive bank display tool.'
      GUID(  2)=' CALL DZDISP(IXSTOR,LBANK,path,''N'',1,0'//
     +',1,IWTYPE)'
      CALL KUGUID('DISP',GUID,  2,'S')
      CALL KUACT('DISP',GXDZ)
 
      CALL KUNWG(  22)
      CALL KUCMD(' ','DIRZ','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DIRZ','PATH','Name of the RZ file','CO','S')
      GUID(  1)='Facility to display RZ directory trees.'
      GUID(  2)=' CALL DZDIRZ(0,LDUMMY,1,path,''AN'',1,0,'//
     +'1)'
      CALL KUGUID('DIRZ',GUID,  2,'S')
      CALL KUACT('DIRZ',GXDZ)
 
      CALL KUNWG(   0)
 
      CALL KUNDPV(   1,   1,   1,   0,   1)
 
      CALL KUCMD(' ',' ','E')
 
      CALL KUCMD('/',' ','SW')
 
      END
