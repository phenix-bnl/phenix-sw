**
      SUBROUTINE GKGCON
      INTEGER MGUIDL
      PARAMETER (MGUIDL=199)
      CHARACTER*80 GUID
      COMMON /KCGUID/ GUID(MGUIDL)
      EXTERNAL GXGCON
 
      CALL KUNWG(   8)
      CALL KUCMD(' ','GRAPHICS_CONTROL','C')
      GUID(  1)='Graphics control commands.'
      CALL KUGUID('GRAPHICS_CONTROL',GUID,  1,'S')
 
      CALL KUCMD('GRAPHICS_CONTROL',' ','SW')
 
      CALL KUNWG( 245)
      CALL KUCMD(' ','METAFILE','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('METAFILE','LUN','Logical unit number','IO','S')
      CALL KUPVAL('METAFILE','LUN',0,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('METAFILE','METAFL','Metafile ID','IO','S')
      CALL KUPVAL('METAFILE','METAFL',0,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('METAFILE','CHMETA','Metafile name','CO','S')
      CALL KUPVAL('METAFILE','CHMETA',0,0.,' ','D')
      GUID(  1)='Set the metafile logical unit and metafi'//
     +'le type.'
      GUID(  2)='This command controls the destination of'//
     +' the subsequent graphics output.'
      GUID(  3)='Example:'
      GUID(  4)=' LUN =-10 output only on metafile opened'//
     +' on unit 10;'
      GUID(  5)=' LUN =  0 output only on screen;'
      GUID(  6)=' LUN = 10 output on both screen and meta'//
     +'file opened on unit 10;'
      GUID(  7)='The procedure to open a metafile with a '//
     +'given name is dependent on the'
      GUID(  8)='graphics package used. For packages usin'//
     +'g FORTRAN I/O the command'
      GUID(  9)='FORTRAN/FILE to open a new file and FORT'//
     +'RAN/CLOSE to close it should be'
      GUID( 10)='used. For the others, the metafile name '//
     +'should be given as third argument'
      GUID( 11)='in the METAFILE command.'
      GUID( 12)=' METAFL=   4 Appendix E GKS.'
      GUID( 13)=' METAFL=-111 HIGZ/PostScript (Portrait).'
      GUID( 14)=' METAFL=-112 HIGZ/PostScript (Landscape)'//
     +'.'
      GUID( 15)=' METAFL=-113 HIGZ/Encapsulated PostScrip'//
     +'t.'
      GUID( 16)=' METAFL=-114 HIGZ/PostScript Color (Port'//
     +'rait).'
      GUID( 17)=' METAFL=-115 HIGZ/PostScript Color (Land'//
     +'scape).'
      GUID( 18)=' METAFL=-777 HIGZ/LaTex Encapsulated.'
      GUID( 19)=' METAFL=-778 HIGZ/LaTex.'
      CALL KUGUID('METAFILE',GUID, 19,'S')
      CALL KUACT('METAFILE',GXGCON)
 
      CALL KUNWG( 143)
      CALL KUCMD(' ','DOPEN','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DOPEN','IVIEW','View number','I','S')
      GUID(  1)=' CALL GDOPEN(iview)'
      GUID(  2)='When a drawing is very complex and requi'//
     +'res a long time to be'
      GUID(  3)='executed, it can be useful to store it i'//
     +'n a view bank: after a'
      GUID(  4)='call to DOPEN and the execution of the d'//
     +'rawing (nothing will'
      GUID(  5)='appear on the screen), and after a neces'//
     +'sary call to DCLOSE,'
      GUID(  6)='the contents of the bank can be displaye'//
     +'d in a very fast way'
      GUID(  7)='through a call to DSHOW; therefore, the '//
     +'detector can be easily'
      GUID(  8)='zoomed many times in different ways. Ple'//
     +'ase note that the pictures'
      GUID(  9)='with solid colours can now be stored in '//
     +'a view bank or in ''PICTURE FILES''.'
      CALL KUGUID('DOPEN',GUID,  9,'S')
      CALL KUACT('DOPEN',GXGCON)
 
      CALL KUNWG(  34)
      CALL KUCMD(' ','DSHOW','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DSHOW','IVIEW','View number','IO','S')
      GUID(  1)=' CALL GDSHOW(iview)'
      GUID(  2)='It shows on the screen the contents of a'//
     +' view bank. It'
      GUID(  3)='can be called after a view bank has been'//
     +' closed.'
      CALL KUGUID('DSHOW',GUID,  3,'S')
      CALL KUACT('DSHOW',GXGCON)
 
      CALL KUNWG(  16)
      CALL KUCMD(' ','DELETE','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DELETE','IVIEW','View number','I','S')
      GUID(  1)=' CALL GDELET(iview)'
      GUID(  2)='It deletes a view bank from memory.'
      CALL KUGUID('DELETE',GUID,  2,'S')
      CALL KUACT('DELETE',GXGCON)
 
      CALL KUNWG(  32)
      CALL KUCMD(' ','DCLOSE','C')
      GUID(  1)=' CALL GDCLOS'
      GUID(  2)='It closes the currently open view bank; '//
     +'it must be called after the end of the'
      GUID(  3)='drawing to be stored.'
      CALL KUGUID('DCLOSE',GUID,  3,'S')
      CALL KUACT('DCLOSE',GXGCON)
 
      CALL KUNWG( 861)
      CALL KUCMD(' ','SATT','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('SATT','NAME','Volume name','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('SATT','IOPT','Name of the attribute to be set','C','S'
     +)
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('SATT','IVAL','Value to which the attribute is to be se
     +t','I','S')
      CALL KUPVAL('SATT','IVAL',1,0.,' ','D')
      GUID(  1)=' CALL GSATT(name,iopt,ival)'
      GUID(  2)='name=''*'' stands for all the volumes.'
      GUID(  3)='iopt can be chosen among the following :'
      GUID(  4)=' ''WORK''   0=volume name is inactive fo'//
     +'r the tracking'
      GUID(  5)='          1=volume name is active for th'//
     +'e tracking (default)'
      GUID(  6)=' ''SEEN''   0=volume name is invisible'
      GUID(  7)='          1=volume name is visible (defa'//
     +'ult)'
      GUID(  8)='         -1=volume invisible with all it'//
     +'s descendants in the tree'
      GUID(  9)='         -2=volume visible but not its d'//
     +'escendants in the tree'
      GUID( 10)=' ''LSTY''   line style 1,2,3,... (defaul'//
     +'t=1)'
      GUID( 11)='          LSTY=7 will produce a very pre'//
     +'cise approximation for'
      GUID( 12)='          revolution bodies.'
      GUID( 13)=' ''LWID''   line width -7,...,1,2,3,..7 '//
     +'(default=1)'
      GUID( 14)='          LWID<0 will act as abs(LWID) w'//
     +'as set for the volume'
      GUID( 15)='          and for all the levels below i'//
     +'t.'
      GUID( 16)=' ''COLO''   colour code -80,...,1,2,..80'//
     +' (default=1)'
      GUID( 17)='          n=1=black'
      GUID( 18)='          n=2=red'
      GUID( 19)='          n=3=green'
      GUID( 20)='          n=4=blue'
      GUID( 21)='          n=5=yellow'
      GUID( 22)='          n=6=violet'
      GUID( 23)='          n=7=light blue'
      GUID( 24)='          colour=n*10+m, m=1,2,...9, wil'//
     +'l produce the same colour'
      GUID( 25)='          as ''n'', but with increasing '//
     +'luminosity according to ''m'';'
      GUID( 26)='          COLO<0 will act as if abs(COLO'//
     +') was set for the volume'
      GUID( 27)='          and for all the levels below i'//
     +'t.'
      GUID( 28)='          When for a volume the attribut'//
     +'e FILL is > 1 (and the'
      GUID( 29)='          option SHAD is on), the ABS of'//
     +' its colour code must be < 8'
      GUID( 30)='          because an automatic shading o'//
     +'f its faces will be'
      GUID( 31)='          performed.'
      GUID( 32)=' ''FILL''   fill area  -7,...,0,1,...7 ('//
     +'default=0)'
      GUID( 33)='          when option SHAD is ''on'' the'//
     +' FILL attribute of any'
      GUID( 34)='          volume can be set different fr'//
     +'om 0 (normal drawing);'
      GUID( 35)='          if it is set to 1, the faces o'//
     +'f such volume will be filled'
      GUID( 36)='          with solid colours; if ABS(FIL'//
     +'L) is > 1, then a light'
      GUID( 37)='          source is placed along the obs'//
     +'erver line, and the faces of'
      GUID( 38)='          such volumes will be painted b'//
     +'y colours whose luminosity'
      GUID( 39)='          will depend on the amount of l'//
     +'ight reflected;'
      GUID( 40)='          if ABS(FILL) = 1, then it is p'//
     +'ossible to use all the 80'
      GUID( 41)='          colours of the colour table, b'//
     +'ecouse the automatic shading'
      GUID( 42)='          is not performed;'
      GUID( 43)='          for increasing values of FILL '//
     +'the drawing will be performed'
      GUID( 44)='          with higher and higher resolut'//
     +'ion improving the quality;'
      GUID( 45)='          it is possible to set differen'//
     +'t values of FILL'
      GUID( 46)='          for different volumes, in orde'//
     +'r to optimize at the same time'
      GUID( 47)='          the performance and the qualit'//
     +'y of the picture;'
      GUID( 48)='          FILL<0 will act as if abs(FILL'//
     +') was set for the volume'
      GUID( 49)='          and for all the levels below i'//
     +'t.'
      GUID( 50)='          This kind of drawing can be sa'//
     +'ved in ''picture files'''
      GUID( 51)='          or in view banks.'
      GUID( 52)='          0=drawing without fill area'
      GUID( 53)='          1=faces filled with solid colo'//
     +'urs and resolution = 6'
      GUID( 54)='          2=lowest resolution (very fast'//
     +')'
      GUID( 55)='          3=default resolution'
      GUID( 56)='          4=.................'
      GUID( 57)='          5=.................'
      GUID( 58)='          6=.................'
      GUID( 59)='          7=max resolution'
      GUID( 60)='          Finally, if a coloured backgro'//
     +'und is desired, the FILL'
      GUID( 61)='          attribute for the first volume'//
     +' of the tree must be set'
      GUID( 62)='          equal to -abs(colo), colo bein'//
     +'g >0 and <80.'
      GUID( 63)=' ''SET ''   set number associated to vol'//
     +'ume name'
      GUID( 64)=' ''DET ''   detector number associated t'//
     +'o volume name'
      GUID( 65)=' ''DTYP''   detector type (1,2)'
      CALL KUGUID('SATT',GUID, 65,'S')
      CALL KUACT('SATT',GXGCON)
 
      CALL KUNWG(  14)
      CALL KUCMD(' ','SCALE','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('SCALE','GSCU','Scale factor for U-coord.','R','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('SCALE','GSCV','Scale factor for V-coord.','R','S')
      GUID(  1)='Change the scale factors GSCU and GSCV i'//
     +'n /GCDRAW/.'
      CALL KUGUID('SCALE',GUID,  1,'S')
      CALL KUACT('SCALE',GXGCON)
 
      CALL KUNWG(   7)
      CALL KUCMD(' ','COLOR','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('COLOR','ICOL','Colour code','I','S')
      CALL KUPVAL('COLOR','ICOL',1,0.,' ','D')
      GUID(  1)=' CALL GDCOL(-abs(icol))'
      CALL KUGUID('COLOR',GUID,  1,'S')
      CALL KUACT('COLOR',GXGCON)
 
      CALL KUNWG(   7)
      CALL KUCMD(' ','LWID','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('LWID','LWIDTH','Line width code','I','S')
      CALL KUPVAL('LWID','LWIDTH',1,0.,' ','D')
      GUID(  1)=' CALL GDLW(-abs(lwidth))'
      CALL KUGUID('LWID',GUID,  1,'S')
      CALL KUACT('LWID',GXGCON)
 
      CALL KUNWG(  17)
      CALL KUCMD(' ','NEXT','C')
      GUID(  1)='Clear screen (start a new picture on gra'//
     +'phics file, if opened).'
      CALL KUGUID('NEXT',GUID,  1,'S')
      CALL KUACT('NEXT',GXGCON)
 
      CALL KUNWG( 184)
      CALL KUCMD(' ','DOPT','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DOPT','IOPT','Option name','CO','S')
      CALL KUPVAL('DOPT','IOPT',0,0.,'*','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DOPT','IVAL','Option value','CO','S')
      CALL KUPVAL('DOPT','IVAL',0,0.,'*','D')
      GUID(  1)=' CALL GDOPT(iopt,ival)'
      GUID(  2)='To set/modify the drawing options.'
      GUID(  3)='   IOPT   IVAL      Action'
      GUID(  4)='   THRZ    ON       Draw tracks in R vs '//
     +'Z'
      GUID(  5)='           OFF (D)  Draw tracks in X,Y,Z'
      GUID(  6)='           180'
      GUID(  7)='           360'
      GUID(  8)='   PROJ    PARA (D) Parallel projection'
      GUID(  9)='           PERS     Perspective'
      GUID( 10)='   TRAK    LINE (D) Trajectory drawn wit'//
     +'h lines'
      GUID( 11)='           POIN       " " with markers'
      GUID( 12)='   HIDE    ON       Hidden line removal '//
     +'using the CG package'
      GUID( 13)='           OFF (D)  No hidden line remov'//
     +'al'
      GUID( 14)='   SHAD    ON       Fill area and shadin'//
     +'g of surfaces.'
      GUID( 15)='           OFF (D)  Normal hidden line r'//
     +'emoval.'
      GUID( 16)='   EDGE    OFF      Does not draw contou'//
     +'rs when shad is on.'
      GUID( 17)='           ON  (D)  Normal shading.'
      CALL KUGUID('DOPT',GUID, 17,'S')
      CALL KUACT('DOPT',GXGCON)
 
      CALL KUNWG(  72)
      CALL KUCMD(' ','SIZE','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('SIZE','XSIZE','Size along X','RO','S')
      CALL KUPVAL('SIZE','XSIZE',0,20.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('SIZE','YSIZE','Size along Y','RO','S')
      CALL KUPVAL('SIZE','YSIZE',0,20.,' ','D')
      GUID(  1)='Set the size of the picture.'
      GUID(  2)='On the terminal, the pictures will have '//
     +'the ratio YSIZE/XSIZE, and,'
      GUID(  3)='if a metafile is produced, pictures will'//
     +' be YSIZE by XSIZE cm.'
      GUID(  4)='This command sets the parameters for the'//
     +' normalisation transformation'
      GUID(  5)='number 1 to [0-XSIZE], [0-YSIZE].'
      CALL KUGUID('SIZE',GUID,  5,'S')
      CALL KUACT('SIZE',GXGCON)
 
      CALL KUNWG(  29)
      CALL KUCMD(' ','SPERS','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('SPERS','DPERS','Distance from the origin','R','S')
      GUID(  1)='Set the variable dpers in /GCDRAW/, repr'//
     +'esenting'
      GUID(  2)='the distance from the origin when using '//
     +'option PERSpective.'
      CALL KUGUID('SPERS',GUID,  2,'S')
      CALL KUACT('SPERS',GXGCON)
 
      CALL KUNWG(  40)
      CALL KUCMD(' ','MAP_COLOR','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('MAP_COLOR','ICADD','Colour table index','IO','S')
      CALL KUPVAL('MAP_COLOR','ICADD',0,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('MAP_COLOR','ICVAL','Colour table value','IO','S')
      CALL KUPVAL('MAP_COLOR','ICVAL',0,0.,' ','D')
      GUID(  1)='Sets the color table LOOKTB(ICADD)=ICVAL'//
     +'.'
      GUID(  2)='If ICADD=0 then LOOKTB(1:16) is taken.'
      GUID(  3)='If ICVAL is omitted the current value of'//
     +' LOOKTB(ICADD) is shown.'
      CALL KUGUID('MAP_COLOR',GUID,  3,'S')
      CALL KUACT('MAP_COLOR',GXGCON)
 
      CALL KUNWG(   0)
 
      CALL KUNDPV(   1,   1,   1,   0,   1)
 
      CALL KUCMD(' ',' ','E')
 
      CALL KUCMD('/',' ','SW')
 
      END
