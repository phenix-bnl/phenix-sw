**
      SUBROUTINE GKHIST
      INTEGER MGUIDL
      PARAMETER (MGUIDL=199)
      CHARACTER*80 GUID
      COMMON /KCGUID/ GUID(MGUIDL)
      EXTERNAL GXHIST
 
      CALL KUNWG(  19)
      CALL KUCMD(' ','HISTOGRAM','C')
      GUID(  1)='Manipulation of histograms, Ntuples.'
      GUID(  2)='Interface to the HBOOK package.'
      CALL KUGUID('HISTOGRAM',GUID,  2,'S')
 
      CALL KUCMD('HISTOGRAM',' ','SW')
 
      CALL KUNWG(  52)
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
      CALL KUNDPV(   1,   1,   1,   1,   2)
      CALL KUPAR('FILE','CHOPT','Options','CO','S')
      CALL KUPVAL('FILE','CHOPT',0,0.,' ','D')
      CALL KUPVAL('FILE','CHOPT',0,0.,' ,N,U','V')
      GUID(  1)='Open an HBOOK direct access file.'
      GUID(  2)=' For CHOPT='' '', existing file is opene'//
     +'d (read mode only).'
      GUID(  3)=' For CHOPT=''N'', a new file is opened.'
      GUID(  4)=' For CHOPT=''U'', existing file is opene'//
     +'d to be modified.'
      CALL KUGUID('FILE',GUID,  4,'S')
      CALL KUACT('FILE',GXHIST)
 
      CALL KUNWG(  12)
      CALL KUCMD(' ','LIST','C')
      CALL KUNDPV(   1,   1,   1,   1,   1)
      CALL KUPAR('LIST','CHOPT','Options','CO','S')
      CALL KUPVAL('LIST','CHOPT',0,0.,' ','D')
      CALL KUPVAL('LIST','CHOPT',0,0.,' ,I','V')
      GUID(  1)='List histograms in the current directory'//
     +'.'
      CALL KUGUID('LIST',GUID,  1,'S')
      CALL KUACT('LIST',GXHIST)
 
      CALL KUNWG(  44)
      CALL KUCMD(' ','DELETE','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DELETE','ID','Histogram Identifier','I','S')
      GUID(  1)='Delete histogram/Ntuple ID in Current Di'//
     +'rectory (memory).'
      GUID(  2)='If ID=0 delete all histograms and Ntuple'//
     +'s.'
      GUID(  3)='To delete histograms in disk files use c'//
     +'ommand HIO/HSCRATCH.'
      CALL KUGUID('DELETE',GUID,  3,'S')
      CALL KUACT('DELETE',GXHIST)
 
      CALL KUNWG( 357)
      CALL KUCMD(' ','PLOT','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('PLOT','ID','Histogram Identifier','I','S')
      CALL KUNDPV(   1,   1,   1,   1,   6)
      CALL KUPAR('PLOT','CHOPT','Options','CO','S')
      CALL KUPVAL('PLOT','CHOPT',0,0.,' ','D')
      CALL KUPVAL('PLOT','CHOPT',0,0.,' ,C,S,+,B,L,P,*,K,U,E,A','V')
      GUID(  1)='Plot a single histogram or a 2-Dim proje'//
     +'ction.'
      GUID(  2)='Each plotted histogram will start'
      GUID(  3)='either a new picture or a new zone in th'//
     +'e current picture.'
      GUID(  4)='CHOPT may be a combination of the follow'//
     +'ing characters:'
      GUID(  5)=' ''C'' Draw a smooth curve.'
      GUID(  6)=' ''S'' Superimpose plot on top of existi'//
     +'ng picture.'
      GUID(  7)=' ''+'' Add contents of ID to last plotte'//
     +'d histogram.'
      GUID(  8)=' ''B'' Select Bar chart format.'
      GUID(  9)=' ''L'' Connect channels contents by a li'//
     +'ne.'
      GUID( 10)=' ''P'' Draw the current polymarker at ea'//
     +'ch channel or cell.'
      GUID( 11)=' ''*'' Draw a * at each channel.'
      GUID( 12)=' ''K'' must be given if option ''U'' is '//
     +'given later.'
      GUID( 13)=' ''U'' Update channels modified since la'//
     +'st call.'
      GUID( 14)=' ''E'' Draw error bars and current marke'//
     +'r.'
      GUID( 15)=' ''A'' Axis labels and tick marks are no'//
     +'t drawn.'
      GUID( 16)=' ''BOX''  draw 2-Dim with proportional b'//
     +'oxes.'
      GUID( 17)=' ''COL''  draw 2-Dim with a color table.'
      GUID( 18)=' ''SURF'' draw 2-Dim as a surface plot ('//
     +'angles are 30,30).'
      GUID( 19)=' ''LEGO'' draw 2-Dim as a lego plot (ang'//
     +'les are 30,30).'
      GUID( 20)=' ''CONT'' draw 2-Dim as a contour plot ('//
     +'15 levels).'
      GUID( 21)=' ''TEXT'' draw 2-Dim as a table.'
      GUID( 22)=' ''CHAR'' draw 2-Dim with characters (a '//
     +'la HBOOK).'
      GUID( 23)=' ''HIST'' draw only histogram (no errors'//
     +' or associated function).'
      GUID( 24)=' ''FUNC'' draw only the associated funct'//
     +'ion (not the histogram).'
      GUID( 25)='Note that this command provides only a s'//
     +'ubset of the equivalent'
      GUID( 26)='command in PAW. It can only plot histogr'//
     +'ams already in memory.'
      GUID( 27)='Use command HRIN to get an histogram fro'//
     +'m disk file.'
      CALL KUGUID('PLOT',GUID, 27,'S')
      CALL KUACT('PLOT',GXHIST)
 
      CALL KUNWG(  11)
      CALL KUCMD(' ','LEGO','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('LEGO','ID','Histogram Identifier','I','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('LEGO','THETA','Angle THETA in degrees','RO','S')
      CALL KUPVAL('LEGO','THETA',0,30.,' ','D')
      CALL KUPVAL('LEGO','THETA',0,0.,' ','L')
      CALL KUPVAL('LEGO','THETA',0,90.,' ','H')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('LEGO','PHI','Angle PHI in degrees','RO','S')
      CALL KUPVAL('LEGO','PHI',0,30.,' ','D')
      CALL KUPVAL('LEGO','PHI',0,0.,' ','L')
      CALL KUPVAL('LEGO','PHI',0,90.,' ','H')
      GUID(  1)='Draw a lego plot from a 2-Dim histogram.'
      CALL KUGUID('LEGO',GUID,  1,'S')
      CALL KUACT('LEGO',GXHIST)
 
      CALL KUNWG(  29)
      CALL KUCMD(' ','HRIN','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('HRIN','ID','Histogram Identifier','I','S')
      GUID(  1)='Read histogram ID from the current direc'//
     +'tory'
      GUID(  2)='on direct access file to memory.'
      GUID(  3)='If ID=0 read all histograms.'
      CALL KUGUID('HRIN',GUID,  3,'S')
      CALL KUACT('HRIN',GXHIST)
 
      CALL KUNWG(  22)
      CALL KUCMD(' ','HROUT','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('HROUT','ID','Histogram Identifier','I','S')
      CALL KUNDPV(   1,   1,   1,   1,   1)
      CALL KUPAR('HROUT','CHOPT','Options','CO','S')
      CALL KUPVAL('HROUT','CHOPT',0,0.,' ','D')
      CALL KUPVAL('HROUT','CHOPT',0,0.,' ,T','V')
      GUID(  1)='Write histo ID from memory to current di'//
     +'rectory.'
      GUID(  2)='If ID=0 write all histograms.'
      CALL KUGUID('HROUT',GUID,  2,'S')
      CALL KUACT('HROUT',GXHIST)
 
      CALL KUNWG(  43)
      CALL KUCMD(' ','PUT','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('PUT','ID','Histogram Identifier','I','S')
      CALL KUPVAL('PUT','ID',0,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('PUT','FNAME','Histogram file name','C','S')
      CALL KUNDPV(   1,   1,   1,   1,   2)
      CALL KUPAR('PUT','CHOPT','Options','CO','S')
      CALL KUPVAL('PUT','CHOPT',0,0.,'N','D')
      CALL KUPVAL('PUT','CHOPT',0,0.,' ,U,N','V')
      GUID(  1)='Write histo ID from memory to file FNAME'//
     +'.'
      GUID(  2)='If ID=0 write all histograms.'
      GUID(  3)='If CHOPT=''N'' (default), create a new f'//
     +'ile.'
      GUID(  4)='If CHOPT=''U'' update existing file.'
      CALL KUGUID('PUT',GUID,  4,'S')
      CALL KUACT('PUT',GXHIST)
 
      CALL KUNWG(  38)
      CALL KUCMD(' ','ZONE','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('ZONE','NX','Number of divisions along X','IO','S')
      CALL KUPVAL('ZONE','NX',1,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('ZONE','NY','Number of divisions along Y','IO','S')
      CALL KUPVAL('ZONE','NY',1,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('ZONE','IFIRST','First division number','IO','S')
      CALL KUPVAL('ZONE','IFIRST',1,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   1,   1)
      CALL KUPAR('ZONE','CHOPT','Option','CO','S')
      CALL KUPVAL('ZONE','CHOPT',0,0.,' ','D')
      CALL KUPVAL('ZONE','CHOPT',0,0.,' ,S','V')
      GUID(  1)='Subdivide the picture into NX by NY zone'//
     +'s,'
      GUID(  2)='starting at zone IFIRST (count along X f'//
     +'irst).'
      GUID(  3)='If CHOPT=''S'', redefine zones on curren'//
     +'t picture.'
      CALL KUGUID('ZONE',GUID,  3,'S')
      CALL KUACT('ZONE',GXHIST)
 
      CALL KUNWG(  58)
      CALL KUCMD(' ','SET','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('SET','CHATT','Attribute name','CO','S')
      CALL KUPVAL('SET','CHATT',0,0.,'SHOW','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('SET','VALUE','Attribute value','RO','S')
      CALL KUPVAL('SET','VALUE',0,0.,' ','D')
      GUID(  1)='Set a specific HPLOT attribute.'
      GUID(  2)='If CHATT=''SHOW'', print defaults and cu'//
     +'rrent values for all attributes.'
      GUID(  3)='If CHATT=''*'', restore default values f'//
     +'or all attributes.'
      GUID(  4)='If VALUE=0, the attribute is set to its '//
     +'default value.'
      CALL KUGUID('SET',GUID,  4,'S')
      CALL KUACT('SET',GXHIST)
 
      CALL KUNWG(  38)
      CALL KUCMD(' ','OPTION','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('OPTION','CHOPTN','Option name','CO','S')
      CALL KUPVAL('OPTION','CHOPTN',0,0.,'SHOW','D')
      GUID(  1)='Set general plotting options for HPLOT.'
      GUID(  2)='If CHOPTN=''SHOW'' print all current and'//
     +' default options.'
      GUID(  3)='If CHOPTN=''*'', restore all default opt'//
     +'ions.'
      CALL KUGUID('OPTION',GUID,  3,'S')
      CALL KUACT('OPTION',GXHIST)
 
      CALL KUNWG( 100)
      CALL KUCMD(' ','NULL','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('NULL','XMIN','Low range in X','RO','S')
      CALL KUPVAL('NULL','XMIN',0,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('NULL','XMAX','High range in X','RO','S')
      CALL KUPVAL('NULL','XMAX',0,1.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('NULL','YMIN','Low range in Y','RO','S')
      CALL KUPVAL('NULL','YMIN',0,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('NULL','YMAX','High range in Y','RO','S')
      CALL KUPVAL('NULL','YMAX',0,1.,' ','D')
      CALL KUNDPV(   1,   1,   1,   1,   2)
      CALL KUPAR('NULL','CHOPT','Options','CO','S')
      CALL KUPVAL('NULL','CHOPT',0,0.,' ','D')
      CALL KUPVAL('NULL','CHOPT',0,0.,' ,S,A','V')
      GUID(  1)='Draw a frame box only.'
      GUID(  2)='If XMIN, XMAX, etc. are given, draw a fr'//
     +'ame box with the window'
      GUID(  3)='coordinates set to XMIN, XMAX, YMIN, YMA'//
     +'X. Axis labels and tick marks'
      GUID(  4)='are drawn by default.'
      GUID(  5)='If option ''S'' is also specified, this '//
     +'command is a convenient way'
      GUID(  6)='to redefine the scale for the current zo'//
     +'ne.'
      GUID(  7)='If the option ''A'' is given then axis l'//
     +'abels and tick marks are not drawn.'
      CALL KUGUID('NULL',GUID,  7,'S')
      CALL KUACT('NULL',GXHIST)
 
      CALL KUNWG(   0)
 
      CALL KUNDPV(   1,   1,   1,   0,   1)
 
      CALL KUCMD(' ',' ','E')
 
      CALL KUCMD('/',' ','SW')
 
      END
