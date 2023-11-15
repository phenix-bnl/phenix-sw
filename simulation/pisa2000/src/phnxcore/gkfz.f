**
      SUBROUTINE GKFZ
      INTEGER MGUIDL
      PARAMETER (MGUIDL=199)
      CHARACTER*80 GUID
      COMMON /KCGUID/ GUID(MGUIDL)
      EXTERNAL GXFZ
 
      CALL KUNWG(   6)
      CALL KUCMD(' ','FZ','C')
      GUID(  1)='ZEBRA/FZ commands'
      CALL KUGUID('FZ',GUID,  1,'S')
 
      CALL KUCMD('FZ',' ','SW')
 
      CALL KUNWG(  19)
      CALL KUCMD(' ','FZIN','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('FZIN','LUN','Fortran unit of the FZ file','I','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('FZIN','KEYSU','Name of the data structure to be retrie
     +ved','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('FZIN','IDENT','Version of the data structure to be ret
     +rieved','IO','S')
      CALL KUPVAL('FZIN','IDENT',0,0.,' ','D')
      GUID(  1)='Equivalent to a call to:'
      GUID(  2)='       CALL GFIN(LUN,KEYSU,1,IDENT,'' '''//
     +',IER)'
      CALL KUGUID('FZIN',GUID,  2,'S')
      CALL KUACT('FZIN',GXFZ)
 
      CALL KUNWG(  68)
      CALL KUCMD(' ','FZOPEN','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('FZOPEN','LUN','Fortran unit with which to open the fil
     +e','I','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('FZOPEN','FILE','Name of the file to be opened','C','S'
     +)
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('FZOPEN','LUNTYP','Type of FZ file to be opened by GOPE
     +N','C','S')
      CALL KUPVAL('FZOPEN','LUNTYP',0,0.,'XI','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('FZOPEN','LEN','Recordlenght of the file','I','S')
      CALL KUPVAL('FZOPEN','LEN',0,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('FZOPEN','CHOPT','Optional parameter to specify the act
     +ion','CO','S')
      CALL KUPVAL('FZOPEN','CHOPT',0,0.,' ','D')
      GUID(  1)='Equivalent to a call to:'
      GUID(  2)='       CALL GOPEN(LUN,FILE,LUNTYP,LEN,IE'//
     +'R)'
      GUID(  3)='If CHOPT = I then a call to GFIN or GFOU'//
     +'T will be performed in addition'
      GUID(  4)='according to the value of LUNTYP, with t'//
     +'he key INIT to save or retrieve'
      GUID(  5)='the whole initialization data structure.'
      CALL KUGUID('FZOPEN',GUID,  5,'S')
      CALL KUACT('FZOPEN',GXFZ)
 
      CALL KUNWG(  19)
      CALL KUCMD(' ','FZOUT','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('FZOUT','LUN','Fortran unit of the FZ file','I','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('FZOUT','KEYSU','Name of the data structure to be saved
     +','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('FZOUT','IDENT','Version of the data structure to be sa
     +ved','IO','S')
      CALL KUPVAL('FZOUT','IDENT',1,0.,' ','D')
      GUID(  1)='Equivalent to a call to:'
      GUID(  2)='       CALL GFOUT(LUN,KEYSU,1,IDENT,'' '''//
     +',IER)'
      CALL KUGUID('FZOUT',GUID,  2,'S')
      CALL KUACT('FZOUT',GXFZ)
 
      CALL KUNWG(  15)
      CALL KUCMD(' ','FZCLOSE','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('FZCLOSE','LUN','Fortran unit of the FZ to close','I'
     +,'S')
      GUID(  1)='Equivalent to a call to:'
      GUID(  2)='       CALL GCLOSE(LUN,IER)'
      CALL KUGUID('FZCLOSE',GUID,  2,'S')
      CALL KUACT('FZCLOSE',GXFZ)
 
      CALL KUNWG(   0)
 
      CALL KUNDPV(   1,   1,   1,   0,   1)
 
      CALL KUCMD(' ',' ','E')
 
      CALL KUCMD('/',' ','SW')
 
      END
