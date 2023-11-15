**
      SUBROUTINE GKGEOM
      INTEGER MGUIDL
      PARAMETER (MGUIDL=199)
      CHARACTER*80 GUID
      COMMON /KCGUID/ GUID(MGUIDL)
      EXTERNAL GXGEOM
 
      CALL KUNWG(   6)
      CALL KUCMD(' ','GEOMETRY','C')
      GUID(  1)='Geometry commands.'
      CALL KUGUID('GEOMETRY',GUID,  1,'S')
 
      CALL KUCMD('GEOMETRY',' ','SW')
 
      CALL KUNWG(  36)
      CALL KUCMD(' ','SVOLU','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('SVOLU','NAME','Volume name','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('SVOLU','SHAPE','Volume type','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('SVOLU','NUMED','Tracking medium number','I','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('SVOLU','NPAR','Number of shape parameters','I','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('SVOLU','PAR','Vector containing shape parameters','C',
     +'S')
      GUID(  1)=' CALL GSVOLU(name,shape,numed,par,npar,i'//
     +'volu)'
      GUID(  2)='where par is a KUIP vector.'
      GUID(  3)='It creates a new volume in the JVOLUM da'//
     +'ta structure.'
      CALL KUGUID('SVOLU',GUID,  3,'S')
      CALL KUACT('SVOLU',GXGEOM)
 
      CALL KUNWG(  29)
      CALL KUCMD(' ','SPOS','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('SPOS','NAME','Volume name','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('SPOS','NUMBER','Copy number of the volume','I','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('SPOS','MOTHER','Mother volume name','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('SPOS','X0','X coord. of the volume in mother ref. sys.
     +','R','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('SPOS','Y0','Y coord. of the volume in mother ref. sys.
     +','R','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('SPOS','Z0','Z coord. of the volume in mother ref. sys.
     +','R','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('SPOS','IROT','Rotation matrix number w.r.t. mother ref
     +. sys.','I','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('SPOS','ONLY','ONLY/MANY flag','C','S')
      GUID(  1)=' CALL GSPOS(name,number,mother,x0,y0,z0,'//
     +'irot,only)'
      GUID(  2)='It positions a previously defined volume'//
     +' in the mother.'
      CALL KUGUID('SPOS',GUID,  2,'S')
      CALL KUACT('SPOS',GXGEOM)
 
      CALL KUNWG(  36)
      CALL KUCMD(' ','SDVN','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('SDVN','NAME','Volume name','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('SDVN','MOTHER','Mother volume name','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('SDVN','NDIV','Number of divisions','I','S')
      CALL KUNDPV(   1,   1,   1,   1,   3)
      CALL KUPAR('SDVN','CAXIS','Axis value','C','S')
      CALL KUPVAL('SDVN','CAXIS',0,0.,'X,Y,Z,1,2,3','V')
      GUID(  1)=' CALL GSDVN(name,mother,ndiv,iaxis)'
      GUID(  2)='X,Y,Z of CAXIS will be translated to 1,2'//
     +',3 for IAXIS.'
      GUID(  3)='It divides a previously defined volume.'
      CALL KUGUID('SDVN',GUID,  3,'S')
      CALL KUACT('SDVN',GXGEOM)
 
      CALL KUNWG(  27)
      CALL KUCMD(' ','SROTM','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('SROTM','IROT','Rotation matrix number','I','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('SROTM','THETA1','Polar angle for axis I','R','S')
      CALL KUPVAL('SROTM','THETA1',0,0.,' ','V')
      CALL KUPVAL('SROTM','THETA1',0,180.,' ','V')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('SROTM','PHI1','Azimuthal angle for axis I','R','S')
      CALL KUPVAL('SROTM','PHI1',0,0.,' ','L')
      CALL KUPVAL('SROTM','PHI1',0,360.,' ','H')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('SROTM','THETA2','Polar angle for axis II','R','S')
      CALL KUPVAL('SROTM','THETA2',0,0.,' ','V')
      CALL KUPVAL('SROTM','THETA2',0,180.,' ','V')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('SROTM','PHI2','Azimuthal angle for axis II','R','S')
      CALL KUPVAL('SROTM','PHI2',0,0.,' ','L')
      CALL KUPVAL('SROTM','PHI2',0,360.,' ','H')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('SROTM','THETA3','Polar angle for axis III','R','S')
      CALL KUPVAL('SROTM','THETA3',0,0.,' ','V')
      CALL KUPVAL('SROTM','THETA3',0,180.,' ','V')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('SROTM','PHI3','Azimuthal angle for axis III','R','S')
      CALL KUPVAL('SROTM','PHI3',0,1.,' ','L')
      CALL KUPVAL('SROTM','PHI3',0,360.,' ','H')
      GUID(  1)=' CALL GSROTM(irot,theta1,phi1,theta2,phi'//
     +'2,theta3,phi3)'
      GUID(  2)='It defines the rotation matrix number IR'//
     +'OT.'
      CALL KUGUID('SROTM',GUID,  2,'S')
      CALL KUACT('SROTM',GXGEOM)
 
      CALL KUNWG(  85)
      CALL KUCMD(' ','STMED','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('STMED','NTMED','Tracking medium number','I','S')
      CALL KUPVAL('STMED','NTMED',1,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('STMED','NAME','Tracking medium name','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('STMED','NMAT','Material number','I','S')
      CALL KUPVAL('STMED','NMAT',1,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('STMED','ISVOL','Sensitive volume flag','I','S')
      CALL KUPVAL('STMED','ISVOL',0,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('STMED','IFIELD','Magnetic field','I','S')
      CALL KUPVAL('STMED','IFIELD',0,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('STMED','FIELDM','Max. field value (Kilogauss)','R','S'
     +)
      CALL KUPVAL('STMED','FIELDM',0,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('STMED','TMAXFD','Max. angle due to field (deg/step)'
     +,'R','S')
      CALL KUPVAL('STMED','TMAXFD',0,0.01,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('STMED','STEMAX','Max. step allowed','R','S')
      CALL KUPVAL('STMED','STEMAX',0,1.E+10,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('STMED','DEEMAX','Max. fraction of energy lost in a ste
     +p','R','S')
      CALL KUPVAL('STMED','DEEMAX',0,0.01,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('STMED','EPSIL','Tracking precision (cm)','R','S')
      CALL KUPVAL('STMED','EPSIL',0,0.01,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('STMED','STMIN','Min. step due to continuos processes (
     +cm)','R','S')
      CALL KUPVAL('STMED','STMIN',0,0.1,' ','D')
      GUID(  1)='      CALL GSTMED(ntmed,name,nmat,isvol,'//
     +'ifield,fieldm,tmaxfd,'
      GUID(  2)='     +            stemax,deemax,epsil,st'//
     +'min,0,0)'
      GUID(  3)='IFIELD = 0 if no magnetic field; IFIELD '//
     +'= -1 if user decision in GUSWIM;'
      GUID(  4)='IFIELD = 1 if tracking performed with GR'//
     +'KUTA; IFIELD = 2 if tracking'
      GUID(  5)='performed with GHELIX; IFIELD = 3 if tra'//
     +'cking performed with GHELX3.'
      CALL KUGUID('STMED',GUID,  5,'S')
      CALL KUACT('STMED',GXGEOM)
 
      CALL KUNWG( 157)
      CALL KUCMD(' ','EDITV','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('EDITV','ISEL','Options','IO','S')
      CALL KUPVAL('EDITV','ISEL',0,0.,' ','D')
      GUID(  1)=' CALL GEDITV(isel)'
      GUID(  2)='When the routine prompts for input param'//
     +'eters that do not need'
      GUID(  3)='to be changed, type return.'
      GUID(  4)='ISEL is used to select the editing opera'//
     +'tion to be performed:'
      GUID(  5)=' ISEL=0, CALL GGCLOS'
      GUID(  6)=' ISEL=1, to modify shape parameters PAR '//
     +'given by GSVOLU'
      GUID(  7)=' ISEL=2, to modify NAME given by GSVOLU'
      GUID(  8)=' ISEL=3, to delete NAME given by GSVOLU'
      GUID(  9)=' ISEL=4, to unlink NAME,NR given by GSPO'//
     +'S/GSDVN/GSDV..'
      GUID( 10)=' ISEL=5, to modify X0,Y0,Z0 of NAME,NR g'//
     +'iven by GSPOS'
      GUID( 11)=' ISEL=6, to modify IROT of NAME,NR given'//
     +' by GSPOS'
      GUID( 12)=' ISEL=7, to modify NDIV given by GSDVN'
      GUID( 13)=' ISEL=8, to modify IAXIS given by GSDVN'
      CALL KUGUID('EDITV',GUID, 13,'S')
      CALL KUACT('EDITV',GXGEOM)
 
      CALL KUNWG(  34)
      CALL KUCMD(' ','CREATE','C')
      GUID(  1)='It creates volumes of the given shape in'//
     +'teractively.'
      GUID(  2)='CALL GSVOLU(name,shape,numed,par,npar,iv'//
     +'olu)'
      GUID(  3)='where par is a KUIP vector'
      CALL KUGUID('CREATE',GUID,  3,'S')
 
      CALL KUCMD('CREATE',' ','SW')
 
      CALL KUNWG(   0)
      CALL KUCMD(' ','BOX','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('BOX','NAME','Volume name','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('BOX','NUMED','Tracking medium number','I','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('BOX','HALFX','Half X length','R','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('BOX','HALFY','Half Y length','R','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('BOX','HALFZ','Half Z length','R','S')
      CALL KUNDPV(   1,   1,   1,   1,   2)
      CALL KUPAR('BOX','YESNO','GSPOSP option','CO','S')
      CALL KUPVAL('BOX','YESNO',0,0.,'NO','D')
      CALL KUPVAL('BOX','YESNO',0,0.,'YES,NO','V')
      CALL KUGUID('BOX',GUID,  0,'S')
      CALL KUACT('BOX',GXGEOM)
 
      CALL KUNWG(   0)
      CALL KUCMD(' ','TRD1','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('TRD1','NAME','Volume name','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('TRD1','NUMED','Tracking medium number','I','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('TRD1','HLFDWX','Half X length in Lower Z Surface','R',
     +'S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('TRD1','HLFUPX','Half X length in Upper Z Surface','R',
     +'S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('TRD1','HALFY','Half Y length','R','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('TRD1','HALFZ','Half Z length','R','S')
      CALL KUNDPV(   1,   1,   1,   1,   2)
      CALL KUPAR('TRD1','YESNO','GSPOSP option','CO','S')
      CALL KUPVAL('TRD1','YESNO',0,0.,'NO','D')
      CALL KUPVAL('TRD1','YESNO',0,0.,'YES,NO','V')
      CALL KUGUID('TRD1',GUID,  0,'S')
      CALL KUACT('TRD1',GXGEOM)
 
      CALL KUNWG(   0)
      CALL KUCMD(' ','TRD2','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('TRD2','NAME','Volume name','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('TRD2','NUMED','Tracking medium number','I','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('TRD2','HLFDWX','Half X length in Lower Z Surface','R',
     +'S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('TRD2','HLFUPX','Half X length in Upper Z Surface','R',
     +'S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('TRD2','HLFDWY','Half Y length in Lower Z Surface','R',
     +'S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('TRD2','HLFUPY','Half Y length in Upper Z Surface','R',
     +'S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('TRD2','HALFZ','Half Z length','R','S')
      CALL KUNDPV(   1,   1,   1,   1,   2)
      CALL KUPAR('TRD2','YESNO','GSPOSP option','CO','S')
      CALL KUPVAL('TRD2','YESNO',0,0.,'NO','D')
      CALL KUPVAL('TRD2','YESNO',0,0.,'YES,NO','V')
      CALL KUGUID('TRD2',GUID,  0,'S')
      CALL KUACT('TRD2',GXGEOM)
 
      CALL KUNWG(   0)
      CALL KUCMD(' ','TUBE','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('TUBE','NAME','Volume name','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('TUBE','NUMED','Tracking medium number','I','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('TUBE','INRAD','Inside Radius','R','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('TUBE','OUTRAD','Outside Radius','R','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('TUBE','HALFZ','Half Z length','R','S')
      CALL KUNDPV(   1,   1,   1,   1,   2)
      CALL KUPAR('TUBE','YESNO','GSPOSP option','CO','S')
      CALL KUPVAL('TUBE','YESNO',0,0.,'NO','D')
      CALL KUPVAL('TUBE','YESNO',0,0.,'YES,NO','V')
      CALL KUGUID('TUBE',GUID,  0,'S')
      CALL KUACT('TUBE',GXGEOM)
 
      CALL KUNWG(   0)
      CALL KUCMD(' ','TUBS','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('TUBS','NAME','Volume name','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('TUBS','NUMED','Tracking medium number','I','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('TUBS','INRAD','Inside Radius','R','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('TUBS','OUTRAD','Outside Radius','R','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('TUBS','HALFZ','Half Z length','R','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('TUBS','SPHI','Start of section PHI','R','S')
      CALL KUPVAL('TUBS','SPHI',0,1.,' ','L')
      CALL KUPVAL('TUBS','SPHI',0,360.,' ','H')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('TUBS','EPHI','End of section PHI','R','S')
      CALL KUPVAL('TUBS','EPHI',0,1.,' ','L')
      CALL KUPVAL('TUBS','EPHI',0,360.,' ','H')
      CALL KUNDPV(   1,   1,   1,   1,   2)
      CALL KUPAR('TUBS','YESNO','GSPOSP option','CO','S')
      CALL KUPVAL('TUBS','YESNO',0,0.,'NO','D')
      CALL KUPVAL('TUBS','YESNO',0,0.,'YES,NO','V')
      CALL KUGUID('TUBS',GUID,  0,'S')
      CALL KUACT('TUBS',GXGEOM)
 
      CALL KUNWG(   0)
      CALL KUCMD(' ','CONE','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('CONE','NAME','Volume name','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('CONE','NUMED','Tracking medium number','I','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('CONE','INRDW','Inside Radius in Lower Z Surface','R'
     +,'S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('CONE','OUTRDW','Outside Radius in Lower Z Surface','R'
     +,'S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('CONE','INRUP','Inside Radius in Upper Z Surface','R'
     +,'S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('CONE','OUTRUP','Outside Radius in Upper Z Surface','R'
     +,'S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('CONE','HALFZ','Half Z length','R','S')
      CALL KUNDPV(   1,   1,   1,   1,   2)
      CALL KUPAR('CONE','YESNO','GSPOSP option','CO','S')
      CALL KUPVAL('CONE','YESNO',0,0.,'NO','D')
      CALL KUPVAL('CONE','YESNO',0,0.,'YES,NO','V')
      CALL KUGUID('CONE',GUID,  0,'S')
      CALL KUACT('CONE',GXGEOM)
 
      CALL KUNWG(   0)
      CALL KUCMD(' ','CONS','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('CONS','NAME','Volume name','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('CONS','NUMED','Tracking medium number','I','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('CONS','INRDW','Inside Radius in Lower Z Surface','R'
     +,'S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('CONS','OUTRDW','Outside Radius in Lower Z Surface','R'
     +,'S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('CONS','INRUP','Inside Radius in Upper Z Surface','R'
     +,'S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('CONS','OUTRUP','Outside Radius in Upper Z Surface','R'
     +,'S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('CONS','HALFZ','Half Z length','R','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('CONS','SPHI','Start of section PHI','R','S')
      CALL KUPVAL('CONS','SPHI',0,1.,' ','L')
      CALL KUPVAL('CONS','SPHI',0,360.,' ','H')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('CONS','EPHI','End of section PHI','R','S')
      CALL KUPVAL('CONS','EPHI',0,1.,' ','L')
      CALL KUPVAL('CONS','EPHI',0,360.,' ','H')
      CALL KUNDPV(   1,   1,   1,   1,   2)
      CALL KUPAR('CONS','YESNO','GSPOSP option','CO','S')
      CALL KUPVAL('CONS','YESNO',0,0.,'NO','D')
      CALL KUPVAL('CONS','YESNO',0,0.,'YES,NO','V')
      CALL KUGUID('CONS',GUID,  0,'S')
      CALL KUACT('CONS',GXGEOM)
 
      CALL KUNWG(   0)
      CALL KUCMD(' ','SPHE','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('SPHE','NAME','Volume name','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('SPHE','NUMED','Tracking medium number','I','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('SPHE','INRAD','Inside Radius','R','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('SPHE','OUTRAD','Outside Radius','R','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('SPHE','SPHI','Start of section PHI','R','S')
      CALL KUPVAL('SPHE','SPHI',0,1.,' ','L')
      CALL KUPVAL('SPHE','SPHI',0,360.,' ','H')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('SPHE','EPHI','End of section PHI','R','S')
      CALL KUPVAL('SPHE','EPHI',0,1.,' ','L')
      CALL KUPVAL('SPHE','EPHI',0,360.,' ','H')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('SPHE','STHETA','Start of section THETA','R','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('SPHE','ETHETA','End of section THETA','R','S')
      CALL KUNDPV(   1,   1,   1,   1,   2)
      CALL KUPAR('SPHE','YESNO','GSPOSP option','CO','S')
      CALL KUPVAL('SPHE','YESNO',0,0.,'NO','D')
      CALL KUPVAL('SPHE','YESNO',0,0.,'YES,NO','V')
      CALL KUGUID('SPHE',GUID,  0,'S')
      CALL KUACT('SPHE',GXGEOM)
 
      CALL KUNWG(   0)
      CALL KUCMD(' ','PARA','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('PARA','NAME','Volume name','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('PARA','NUMED','Tracking medium number','I','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('PARA','HALFX','Half X length','R','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('PARA','HALFY','Half Y length','R','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('PARA','HALFZ','Half Z length','R','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('PARA','AXIS','Angle of Y mid-faces segment to Y axis',
     +'R','S')
      CALL KUPVAL('PARA','AXIS',0,1.,' ','L')
      CALL KUPVAL('PARA','AXIS',0,360.,' ','H')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('PARA','PHI','PHI angle of Low Z mid-face to High Z mid
     +-face segment','R','S')
      CALL KUPVAL('PARA','PHI',0,1.,' ','L')
      CALL KUPVAL('PARA','PHI',0,360.,' ','H')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('PARA','THETA','THETA angle of mid-low-Z-face to mid-hi
     +gh-Z-face segment','R','S')
      CALL KUPVAL('PARA','THETA',0,1.,' ','L')
      CALL KUPVAL('PARA','THETA',0,360.,' ','H')
      CALL KUNDPV(   1,   1,   1,   1,   2)
      CALL KUPAR('PARA','YESNO','GSPOSP option','CO','S')
      CALL KUPVAL('PARA','YESNO',0,0.,'NO','D')
      CALL KUPVAL('PARA','YESNO',0,0.,'YES,NO','V')
      CALL KUGUID('PARA',GUID,  0,'S')
      CALL KUACT('PARA',GXGEOM)
 
      CALL KUNWG(   0)
 
      CALL KUNDPV(   1,   1,   1,   0,   1)
 
      CALL KUCMD(' ',' ','E')
 
      CALL KUCMD('/',' ','SW')
 
      END
