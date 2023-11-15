**
      SUBROUTINE GKPICT
      INTEGER MGUIDL
      PARAMETER (MGUIDL=199)
      CHARACTER*80 GUID
      COMMON /KCGUID/ GUID(MGUIDL)
      EXTERNAL GXPICT
 
      CALL KUNWG(  12)
      CALL KUCMD(' ','PICTURE','C')
      GUID(  1)='Creation and manipulation of HIGZ pictur'//
     +'es.'
      CALL KUGUID('PICTURE',GUID,  1,'S')
 
      CALL KUCMD('PICTURE',' ','SW')
 
      CALL KUNWG(  46)
      CALL KUCMD(' ','FILE','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('FILE','LUN','Logical unit number','I','S')
      CALL KUPVAL('FILE','LUN',1,0.,' ','L')
      CALL KUPVAL('FILE','LUN',128,0.,' ','H')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('FILE','FNAME','File name','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('FILE','LRECL','Record length in words','IO','S')
      CALL KUPVAL('FILE','LRECL',1024,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   1,   4)
      CALL KUPAR('FILE','CHOPT','Options','CO','S')
      CALL KUPVAL('FILE','CHOPT',0,0.,' ','D')
      CALL KUPVAL('FILE','CHOPT',0,0.,' ,A,N,U,AN,AU','V')
      GUID(  1)='Open a HIGZ direct access picture file.'
      GUID(  2)='  For CHOPT='' '', existing file is open'//
     +'ed.'
      GUID(  3)='  For CHOPT=''N'', a new file is opened.'
      GUID(  4)='  For CHOPT=''U'', existing file is modi'//
     +'fied.'
      CALL KUGUID('FILE',GUID,  4,'S')
      CALL KUACT('FILE',GXPICT)
 
      CALL KUNWG(  15)
      CALL KUCMD(' ','LIST','C')
      GUID(  1)='List all the HIGZ pictures currently sto'//
     +'red in memory.'
      CALL KUGUID('LIST',GUID,  1,'S')
      CALL KUACT('LIST',GXPICT)
 
      CALL KUNWG(  20)
      CALL KUCMD(' ','DELETE','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DELETE','PNAME','Picture name','C','S')
      CALL KUPVAL('DELETE','PNAME',0,0.,' ','D')
      GUID(  1)='Delete the picture PNAME from memory.'
      GUID(  2)='PNAME=''*'' means all pictures.'
      CALL KUGUID('DELETE',GUID,  2,'S')
      CALL KUACT('DELETE',GXPICT)
 
      CALL KUNWG(  15)
      CALL KUCMD(' ','SCRATCH','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('SCRATCH','PNAME','Picture name','C','S')
      CALL KUPVAL('SCRATCH','PNAME',0,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('SCRATCH','ICYCLE','Cycle number','IO','S')
      CALL KUPVAL('SCRATCH','ICYCLE',9999,0.,' ','D')
      GUID(  1)='Delete the picture PNAME from current di'//
     +'rectory on disk.'
      CALL KUGUID('SCRATCH',GUID,  1,'S')
      CALL KUACT('SCRATCH',GXPICT)
 
      CALL KUNWG(  26)
      CALL KUCMD(' ','PLOT','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('PLOT','PNAME','Picture name','CO','S')
      CALL KUPVAL('PLOT','PNAME',0,0.,' ','D')
      GUID(  1)='Plot the picture PNAME.'
      GUID(  2)='PNAME='' '' means the current picture.'
      GUID(  3)='PNAME=''*'' means all pictures.'
      CALL KUGUID('PLOT',GUID,  3,'S')
      CALL KUACT('PLOT',GXPICT)
 
      CALL KUNWG(   6)
      CALL KUCMD(' ','RENAME','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('RENAME','PNAME1','Old picture name','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('RENAME','PNAME2','New picture name','C','S')
      GUID(  1)='Rename a picture.'
      CALL KUGUID('RENAME',GUID,  1,'S')
      CALL KUACT('RENAME',GXPICT)
 
      CALL KUNWG(  42)
      CALL KUCMD(' ','IZOUT','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('IZOUT','PNAME','Picture name','CO','S')
      CALL KUPVAL('IZOUT','PNAME',0,0.,' ','D')
      GUID(  1)='Write the picture PNAME to a direct acce'//
     +'ss picture file'
      GUID(  2)='(see command PICTURE/FILE).'
      GUID(  3)='PNAME='' '' means the current picture.'
      GUID(  4)='PNAME=''*'' means all pictures.'
      CALL KUGUID('IZOUT',GUID,  4,'S')
      CALL KUACT('IZOUT',GXPICT)
 
      CALL KUNWG(  33)
      CALL KUCMD(' ','IZIN','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('IZIN','PNAME','Picture name','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('IZIN','ICYCLE','Cycle number','IO','S')
      CALL KUPVAL('IZIN','ICYCLE',9999,0.,' ','D')
      GUID(  1)='Read picture into memory from a direct a'//
     +'ccess picture file'
      GUID(  2)='(see command PICTURE/FILE).'
      GUID(  3)='PNAME=''*'' means all pictures.'
      CALL KUGUID('IZIN',GUID,  3,'S')
      CALL KUACT('IZIN',GXPICT)
 
      CALL KUNWG(  55)
      CALL KUCMD(' ','IGSET','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('IGSET','CHATT','Attribute name','CO','S')
      CALL KUPVAL('IGSET','CHATT',0,0.,'SHOW','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('IGSET','VALUE','Attribute value','RO','S')
      CALL KUPVAL('IGSET','VALUE',0,0.,' ','D')
      GUID(  1)='Set a HIGZ attribute.'
      GUID(  2)='If CHATT=''SHOW'' print default and curr'//
     +'ent values for all attributes.'
      GUID(  3)='If CHATT=''*'' restore default values fo'//
     +'r all attributes.'
      GUID(  4)='If VALUE=0, the attribute is set to its '//
     +'default value.'
      CALL KUGUID('IGSET',GUID,  4,'S')
      CALL KUACT('IGSET',GXPICT)
 
      CALL KUNWG(   0)
 
      CALL KUNDPV(   1,   1,   1,   0,   1)
 
      CALL KUCMD(' ',' ','E')
 
      CALL KUCMD('/',' ','SW')
 
      END
