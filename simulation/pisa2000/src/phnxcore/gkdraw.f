**
      SUBROUTINE GKDRAW
      INTEGER MGUIDL
      PARAMETER (MGUIDL=199)
      CHARACTER*80 GUID
      COMMON /KCGUID/ GUID(MGUIDL)
      EXTERNAL GXDRAW
      EXTERNAL GXPICK
 
      CALL KUNWG( 228)
      CALL KUCMD(' ','CVOL','C')
      GUID(  1)='Clipping commands.'
      GUID(  2)='The hidden line removal technique is nec'//
     +'essary to visualize properly'
      GUID(  3)='very complex detectors. At the same time'//
     +', it can be useful to visualize'
      GUID(  4)='the inner elements of a detector in deta'//
     +'il. For this purpose, the'
      GUID(  5)='commands menu CVOL has been developed: t'//
     +'hese commands allow'
      GUID(  6)='subtractions (via boolean operation) of '//
     +'given shapes from any part of'
      GUID(  7)='the detector, therefore showing its inne'//
     +'r contents. It is possible'
      GUID(  8)='to clip each different volume by means o'//
     +'f a different shape (BOX ,'
      GUID(  9)='TUBE, CONE, SPHE are available). If ''*'''//
     +' is given as the name of the'
      GUID( 10)='volume to be clipped, all volumes are cl'//
     +'ipped by the given shape.'
      GUID( 11)='A volume can be clipped at most twice (e'//
     +'ven by'
      GUID( 12)='different shapes); if a volume is explic'//
     +'itely clipped'
      GUID( 13)='twice, the ''*'' will not act on it anym'//
     +'ore. Giving ''.'' as the name'
      GUID( 14)='of the volume to be clipped will reset t'//
     +'he clipping.'
      CALL KUGUID('CVOL',GUID, 14,'S')
 
      CALL KUCMD('CVOL',' ','SW')
 
      CALL KUNWG( 139)
      CALL KUCMD(' ','BOX','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('BOX','CNNV',' Name of volume to be clipped','C','S')
      CALL KUPVAL('BOX','CNNV',0,0.,'*   ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('BOX','XMIN',' Lower limit of the Shape X coordinate'
     +,'RO','S')
      CALL KUPVAL('BOX','XMIN',0,-10000.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('BOX','XMAX',' Upper limit of the Shape X coordinate'
     +,'RO','S')
      CALL KUPVAL('BOX','XMAX',0,-9999.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('BOX','YMIN',' Lower limit of the Shape Y coordinate'
     +,'RO','S')
      CALL KUPVAL('BOX','YMIN',0,-10000.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('BOX','YMAX',' Upper limit of the Shape Y coordinate'
     +,'RO','S')
      CALL KUPVAL('BOX','YMAX',0,-9999.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('BOX','ZMIN',' Lower limit of the Shape Z coordinate'
     +,'RO','S')
      CALL KUPVAL('BOX','ZMIN',0,-10000.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('BOX','ZMAX',' Upper limit of the Shape Z coordinate'
     +,'RO','S')
      CALL KUPVAL('BOX','ZMAX',0,-9999.,' ','D')
      GUID(  1)='This command performs a boolean subtract'//
     +'ion between the volume'
      GUID(  2)='CNVV and a box placed in the MARS accord'//
     +'ing the values of the given'
      GUID(  3)='coordinates. See also CVOL.'
      GUID(  4)='The following commands will clip by a bo'//
     +'x,'
      GUID(  5)='with a vertex at the origin, the volume '//
     +'specified by NAME (a valid'
      GUID(  6)='string for the NAME of the volume can be'//
     +' found using the DTREE command).'
      GUID(  7)=' EXAMPLE -'
      GUID(  8)=' dopt hide on'
      GUID(  9)=' satt * seen -2'
      GUID( 10)=' draw NAME 40 40 0 10 10 .01 .01'
      GUID( 11)=' next'
      GUID( 12)=' box NAME 0 1000 0 1000 0 1000'
      GUID( 13)=' draw NAME 40 40 0 10 10 .01 .01'
      GUID( 14)=' box .'
      CALL KUGUID('BOX',GUID, 14,'S')
      CALL KUACT('BOX',GXDRAW)
 
      CALL KUNWG( 155)
      CALL KUCMD(' ','TUBE','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('TUBE','CNVV',' Name of volume to be clipped','C','S')
      CALL KUPVAL('TUBE','CNVV',0,0.,'*   ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('TUBE','RMAX',' External radius of tube','RO','S')
      CALL KUPVAL('TUBE','RMAX',0,0.1,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('TUBE','ZDEM',' Half length of tube axis','RO','S')
      CALL KUPVAL('TUBE','ZDEM',0,0.1,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('TUBE','XMED',' Center X coordinate','RO','S')
      CALL KUPVAL('TUBE','XMED',0,-10000.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('TUBE','YMED',' Center Y coordinate','RO','S')
      CALL KUPVAL('TUBE','YMED',0,-10000.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('TUBE','ZMED',' Center Z coordinate','RO','S')
      CALL KUPVAL('TUBE','ZMED',0,-10000.,' ','D')
      GUID(  1)='This command performs a boolean subtract'//
     +'ion between the volume'
      GUID(  2)='CNVV and a tube; the tube has the given '//
     +'parameters and is placed in'
      GUID(  3)='the MARS according the given coordinates'//
     +' of its center.'
      GUID(  4)='See also CVOL.'
      GUID(  5)='The following commands will clip, by a t'//
     +'ube,'
      GUID(  6)='positioned according to the given parame'//
     +'ters, the volume specified'
      GUID(  7)='by NAME (a valid string for the NAME of '//
     +'the volume'
      GUID(  8)='can be found using the DTREE command).'
      GUID(  9)=' EXAMPLE -'
      GUID( 10)=' dopt hide on'
      GUID( 11)=' satt * seen -2'
      GUID( 12)=' draw NAME 40 40 0 10 10 .01 .01'
      GUID( 13)=' next'
      GUID( 14)=' tube * 500 1000 500 0 0'
      GUID( 15)=' draw NAME 40 40 0 10 10 .01 .01'
      GUID( 16)=' box .'
      CALL KUGUID('TUBE',GUID, 16,'S')
      CALL KUACT('TUBE',GXDRAW)
 
      CALL KUNWG( 156)
      CALL KUCMD(' ','CONE','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('CONE','CNVV',' Name of volume to be clipped','C','S')
      CALL KUPVAL('CONE','CNVV',0,0.,'*   ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('CONE','RMAX1',' Min external radius','RO','S')
      CALL KUPVAL('CONE','RMAX1',0,0.1,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('CONE','RMAX2',' Max external radius','RO','S')
      CALL KUPVAL('CONE','RMAX2',0,0.1,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('CONE','ZDEM',' Half length of cone axis','RO','S')
      CALL KUPVAL('CONE','ZDEM',0,0.1,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('CONE','XMED',' Center X coordinate','RO','S')
      CALL KUPVAL('CONE','XMED',0,-10000.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('CONE','YMED',' Center Y coordinate','RO','S')
      CALL KUPVAL('CONE','YMED',0,-10000.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('CONE','ZMED',' Center Z coordinate','RO','S')
      CALL KUPVAL('CONE','ZMED',0,-10000.,' ','D')
      GUID(  1)='This command performs a boolean subtract'//
     +'ion between the volume'
      GUID(  2)='CNVV and a cone; the cone has the given '//
     +'parameters and is placed in'
      GUID(  3)='the MARS according to the given coordina'//
     +'tes of its center.'
      GUID(  4)='See also CVOL.'
      GUID(  5)='The following commands will clip by a co'//
     +'ne,'
      GUID(  6)='positioned according the given parameter'//
     +'s, the volume specified'
      GUID(  7)='by NAME (a valid string for the NAME of '//
     +'the volume'
      GUID(  8)='can be found using the DTREE command).'
      GUID(  9)=' EXAMPLE -'
      GUID( 10)=' dopt hide on'
      GUID( 11)=' satt * seen -2'
      GUID( 12)=' draw NAME 40 40 0 10 10 .01 .01'
      GUID( 13)=' next'
      GUID( 14)=' cone * 1 750 1000 0 0 1000'
      GUID( 15)=' draw NAME 40 40 0 10 10 .01 .01'
      GUID( 16)=' box .'
      CALL KUGUID('CONE',GUID, 16,'S')
      CALL KUACT('CONE',GXDRAW)
 
      CALL KUNWG( 154)
      CALL KUCMD(' ','SPHE','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('SPHE','CNVV',' Name of volume to be clipped','C','S')
      CALL KUPVAL('SPHE','CNVV',0,0.,'*   ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('SPHE','RMAX',' External radius of sphere','RO','S')
      CALL KUPVAL('SPHE','RMAX',0,0.1,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('SPHE','XMED',' Center X coordinate','RO','S')
      CALL KUPVAL('SPHE','XMED',0,-10000.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('SPHE','YMED',' Center Y coordinate','RO','S')
      CALL KUPVAL('SPHE','YMED',0,-10000.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('SPHE','ZMED',' Center Z coordinate','RO','S')
      CALL KUPVAL('SPHE','ZMED',0,-10000.,' ','D')
      GUID(  1)='This command performs a boolean subtract'//
     +'ion between the volume'
      GUID(  2)='CNVV and a sphere; the sphere has the gi'//
     +'ven parameters and is placed in'
      GUID(  3)='the MARS according to the given coordina'//
     +'tes of its center.'
      GUID(  4)='See also CVOL. The following commands cl'//
     +'ip by a sphere,'
      GUID(  5)='positioned according to the given parame'//
     +'ters, the volume specified'
      GUID(  6)='by NAME (a valid string for the NAME of '//
     +'the volume'
      GUID(  7)='can be found using the DTREE command).'
      GUID(  8)='EXAMPLE -'
      GUID(  9)=' dopt hide on'
      GUID( 10)=' satt * seen -2'
      GUID( 11)=' draw NAME 40 40 0 10 10 .01 .01'
      GUID( 12)=' next'
      GUID( 13)=' sphe * 500 0 0 500'
      GUID( 14)=' draw NAME 40 40 0 10 10 .01 .01'
      GUID( 15)=' box .'
      CALL KUGUID('SPHE',GUID, 15,'S')
      CALL KUACT('SPHE',GXDRAW)
 
      CALL KUCMD('..',' ','SW')
 
      CALL KUNWG( 250)
      CALL KUCMD(' ','DRAWING','C')
      GUID(  1)='Drawing commands. These commands allow t'//
     +'he visualization in several ways'
      GUID(  2)='of the volumes defined in the geometrica'//
     +'l data structure. It is possible'
      GUID(  3)='to draw the logical tree of volumes belo'//
     +'nging to the detector (DTREE),'
      GUID(  4)='to show their geometrical specification '//
     +'(DSPEC,DFSPC), to draw them'
      GUID(  5)='and their cut views (DRAW, DCUT). Moreov'//
     +'er, it is possible to execute'
      GUID(  6)='these commands when the hidden line remo'//
     +'val option is activated; in'
      GUID(  7)='this case, the volumes can be also eithe'//
     +'r translated in the space'
      GUID(  8)='(SHIFT), or clipped by boolean operation'//
     +' (CVOL). In addition, it is'
      GUID(  9)='possible to fill the surfaces of the vol'//
     +'umes'
      GUID( 10)='with solid colours when the shading opti'//
     +'on (SHAD) is activated.'
      GUID( 11)='Several tools (ZOOM, LENS) have been dev'//
     +'eloped to zoom detailed parts'
      GUID( 12)='of the detectors or to scan physical eve'//
     +'nts as well.'
      GUID( 13)='Finally, the command MOVE will allow the'//
     +' rotation, translation and zooming'
      GUID( 14)='on real time parts of the detectors or t'//
     +'racks and hits of a simulated event.'
      CALL KUGUID('DRAWING',GUID, 14,'S')
 
      CALL KUCMD('DRAWING',' ','SW')
 
      CALL KUNWG( 488)
      CALL KUCMD(' ','DRAW','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DRAW','NAME','Volume name','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DRAW','THETA','Viewing angle theta (for 3D projection)
     +','RO','S')
      CALL KUPVAL('DRAW','THETA',0,0.,' ','L')
      CALL KUPVAL('DRAW','THETA',0,360.,' ','H')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DRAW','PHI','Viewing angle phi (for 3D projection)','R
     +O','S')
      CALL KUPVAL('DRAW','PHI',0,0.,' ','L')
      CALL KUPVAL('DRAW','PHI',0,360.,' ','H')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DRAW','PSI','Viewing angle psi (for 2D rotation)','RO'
     +,'S')
      CALL KUPVAL('DRAW','PSI',0,0.,' ','L')
      CALL KUPVAL('DRAW','PSI',0,180.,' ','H')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DRAW','U0','U-coord. (horizontal) of volume origin','R
     +O','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DRAW','V0','V-coord. (vertical) of volume origin','RO'
     +,'S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DRAW','SU','Scale factor for U-coord.','RO','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DRAW','SV','Scale factor for V-coord.','RO','S')
      GUID(  1)=' CALL GDRAW(name,theta,phi,psi,u0,v0,su,'//
     +'sv)'
      GUID(  2)='If optional parameters are missing, the '//
     +'corresponding values are'
      GUID(  3)='taken from the common /GCDRAW/. This com'//
     +'mand will draw the volumes,'
      GUID(  4)='selected with their graphical attributes'//
     +', set by the SATT'
      GUID(  5)='facility. The drawing may be performed w'//
     +'ith hidden line removal'
      GUID(  6)='and with shading effects according to th'//
     +'e value of the options HIDE'
      GUID(  7)='and SHAD; if the option SHAD is ON, the '//
     +'contour''s edges can be'
      GUID(  8)='drawn or not. If the option HIDE is ON, '//
     +'the detector can be'
      GUID(  9)='exploded (BOMB), clipped with different '//
     +'shapes (CVOL), and some'
      GUID( 10)='of its parts can be shifted from their o'//
     +'riginal'
      GUID( 11)='position (SHIFT). When HIDE is ON, if'
      GUID( 12)='the drawing requires more than the avail'//
     +'able memory, the program'
      GUID( 13)='will evaluate and display the number of '//
     +'missing words'
      GUID( 14)='(so that the user can increase the'
      GUID( 15)='size of its ZEBRA store). Finally, at th'//
     +'e end of each drawing (with HIDE on),'
      GUID( 16)='the program will print messages about th'//
     +'e memory used and'
      GUID( 17)='statistics on the volumes'' visibility.'
      GUID( 18)='The following commands will produce the '//
     +'drawing of a green'
      GUID( 19)='volume, specified by NAME, without using'//
     +' the hidden line removal'
      GUID( 20)='technique, using the hidden line removal'//
     +' technique,'
      GUID( 21)='with different linewidth and colour (red'//
     +'), with'
      GUID( 22)='solid colour, with shading of surfaces, '//
     +'and without edges. (A possible'
      GUID( 23)='string for the NAME of the volume can be'//
     +' found using the command DTREE).'
      GUID( 24)=' EXAMPLE -'
      GUID( 25)=' satt * seen -2'
      GUID( 26)=' satt NAME colo 3'
      GUID( 27)=' draw NAME 40 40 0 10 10 .01 .01'
      GUID( 28)=' next'
      GUID( 29)=' dopt hide on'
      GUID( 30)=' draw NAME 40 40 0 10 10 .01 .01'
      GUID( 31)=' next'
      GUID( 32)=' satt NAME colo 2'
      GUID( 33)=' satt NAME lwid 4'
      GUID( 34)=' draw NAME 40 40 0 10 10 .01 .01'
      GUID( 35)=' next'
      GUID( 36)=' dopt shad on'
      GUID( 37)=' satt * lwid 1'
      GUID( 38)=' satt NAME fill 1'
      GUID( 39)=' draw NAME 40 40 0 10 10 .01 .01'
      GUID( 40)=' next'
      GUID( 41)=' satt NAME fill 3'
      GUID( 42)=' draw NAME 40 40 0 10 10 .01 .01'
      GUID( 43)=' next'
      GUID( 44)=' dopt edge off'
      GUID( 45)=' draw NAME 40 40 0 10 10 .01 .01'
      CALL KUGUID('DRAW',GUID, 45,'S')
      CALL KUACT('DRAW',GXDRAW)
 
      CALL KUNWG( 154)
      CALL KUCMD(' ','DVOLUME','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DVOLUME','N','Number of elements in arrays LNAMES and
     +LNUMBS','I','S')
      CALL KUPVAL('DVOLUME','N',1,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DVOLUME','NAMNUM','Volume names and numbers (ex. "NAME
     +1,NR1,NAME2,NR2")','C','S')
      CALL KUNDPV(   1,   1,   1,   1,   2)
      CALL KUPAR('DVOLUME','CHNRS','Reference system used','C','S')
      CALL KUPVAL('DVOLUME','CHNRS',0,0.,'MARS','D')
      CALL KUPVAL('DVOLUME','CHNRS',0,0.,'MARS,DRS','V')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DVOLUME','THETA','Viewing angle theta (for 3D projecti
     +on)','RO','S')
      CALL KUPVAL('DVOLUME','THETA',0,1.,' ','L')
      CALL KUPVAL('DVOLUME','THETA',0,360.,' ','H')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DVOLUME','PHI','Viewing angle phi (for 3D projection)'
     +,'RO','S')
      CALL KUPVAL('DVOLUME','PHI',0,1.,' ','L')
      CALL KUPVAL('DVOLUME','PHI',0,360.,' ','H')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DVOLUME','PSI','Viewing angle psi (for 2D rotation)'
     +,'RO','S')
      CALL KUPVAL('DVOLUME','PSI',0,1.,' ','L')
      CALL KUPVAL('DVOLUME','PSI',0,180.,' ','H')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DVOLUME','U0','U-coord. (horizontal) of volume origin'
     +,'RO','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DVOLUME','V0','V-coord. (vertical) of volume origin'
     +,'RO','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DVOLUME','SU','Scale factor for U-coord.','RO','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DVOLUME','SV','Scale factor for V-coord.','RO','S')
      GUID(  1)=' CALL GDRVOL(n,lnames,lnumbs,nrs,theta,p'//
     +'hi,psi,u0,v0,su,sv)'
      GUID(  2)='N is the number of levels from the top o'//
     +'f the geometry structure'
      GUID(  3)='to the volume lnames(n),lnumbs(n) to be '//
     +'drawn.'
      GUID(  4)='NAMNUM contain the arrays lnames and lnu'//
     +'mbs,'
      GUID(  5)='identifying the path, in pairs and separ'//
     +'ated by commas; for'
      GUID(  6)='example (with n=2) :'
      GUID(  7)='''lname(1),lnumbs(1),lname(2),lnumbs(2) '//
     +''''
      GUID(  8)='CHNRS is the name of the reference syste'//
     +'m used: MARS for MAster Reference'
      GUID(  9)='System or DRS for Daughter Reference Sys'//
     +'tem.'
      GUID( 10)='NRS=0 for MARS or NRS<>0 for DRS'
      GUID( 11)='If optional parameters are missing, the '//
     +'current values in /GCDRAW/'
      GUID( 12)='are taken.'
      CALL KUGUID('DVOLUME',GUID, 12,'S')
      CALL KUACT('DVOLUME',GXDRAW)
 
      CALL KUNWG( 107)
      CALL KUCMD(' ','DCUT','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DCUT','NAME','Volume name','C','S')
      CALL KUNDPV(   1,   1,   1,   1,   2)
      CALL KUPAR('DCUT','CAXIS','Axis value','C','S')
      CALL KUPVAL('DCUT','CAXIS',0,0.,'X,Y,Z','V')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DCUT','CUTVAL','Cut plane distance from the origin alo
     +ng the axis','R','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DCUT','U0','U-coord. (horizontal) of volume origin','R
     +O','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DCUT','V0','V-coord. (vertical) of volume origin','RO'
     +,'S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DCUT','SU','Scale factor for U-coord.','RO','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DCUT','SV','Scale factor for V-coord.','RO','S')
      GUID(  1)=' CALL GDRAWC(name,iaxis,cutval,u0,v0,su,'//
     +'sv)'
      GUID(  2)='The cut plane is normal to caxis (X,Y,Z)'//
     +', corresponding to iaxis (1,2,3),'
      GUID(  3)='and placed at the distance cutval from t'//
     +'he origin.'
      GUID(  4)='The resulting picture is seen from the t'//
     +'he same axis.'
      GUID(  5)='If optional parameters are missing, the '//
     +'current values in /GCDRAW/'
      GUID(  6)='are taken.'
      GUID(  7)='When HIDE Mode is ON, it is possible to '//
     +'get the same effect with'
      GUID(  8)='the CVOL/BOX command.'
      CALL KUGUID('DCUT',GUID,  8,'S')
      CALL KUACT('DCUT',GXDRAW)
 
      CALL KUNWG(  90)
      CALL KUCMD(' ','DXCUT','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DXCUT','NAME','Volume name','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DXCUT','CUTTHE','Theta angle of the line normal to cut
     + plane','R','S')
      CALL KUPVAL('DXCUT','CUTTHE',0,1.,' ','L')
      CALL KUPVAL('DXCUT','CUTTHE',0,360.,' ','H')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DXCUT','CUTPHI','Phi angle of the line normal to cut p
     +lane','R','S')
      CALL KUPVAL('DXCUT','CUTPHI',0,1.,' ','L')
      CALL KUPVAL('DXCUT','CUTPHI',0,360.,' ','H')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DXCUT','CUTVAL','Cut plane distance from the origin al
     +ong the axis','R','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DXCUT','THETA','Viewing angle theta (for 3D projection
     +)','RO','S')
      CALL KUPVAL('DXCUT','THETA',0,1.,' ','L')
      CALL KUPVAL('DXCUT','THETA',0,360.,' ','H')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DXCUT','PHI','Viewing angle phi (for 3D projection)'
     +,'RO','S')
      CALL KUPVAL('DXCUT','PHI',0,1.,' ','L')
      CALL KUPVAL('DXCUT','PHI',0,360.,' ','H')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DXCUT','U0','U-coord. (horizontal) of volume origin'
     +,'RO','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DXCUT','V0','V-coord. (vertical) of volume origin','RO
     +','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DXCUT','SU','Scale factor for U-coord.','RO','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DXCUT','SV','Scale factor for V-coord.','RO','S')
      GUID(  1)=' CALL GDRAWX(name,cutthe,cutphi,cutval,t'//
     +'heta,phi,u0,v0,su,sv)'
      GUID(  2)='The cut plane is normal to the line give'//
     +'n by the cut angles'
      GUID(  3)='cutthe and cutphi and placed at the dist'//
     +'ance cutval from the origin.'
      GUID(  4)='The resulting picture is seen from the v'//
     +'iewing angles theta,phi.'
      GUID(  5)='If optional parameters are missing, the '//
     +'current values in /GCDRAW/'
      GUID(  6)='are taken.'
      CALL KUGUID('DXCUT',GUID,  6,'S')
      CALL KUACT('DXCUT',GXDRAW)
 
      CALL KUNWG( 205)
      CALL KUCMD(' ','SHIFT','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('SHIFT','CNVN',' Name of volume to be shifted','C','S')
      CALL KUPVAL('SHIFT','CNVN',0,0.,'*','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('SHIFT','XXXX',' Shift along X axis','R','S')
      CALL KUPVAL('SHIFT','XXXX',0,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('SHIFT','YYYY',' Shift along Y axis','R','S')
      CALL KUPVAL('SHIFT','YYYY',0,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('SHIFT','ZZZZ',' Shift along Z axis','R','S')
      CALL KUPVAL('SHIFT','ZZZZ',0,0.,' ','D')
      GUID(  1)='To draw a volume shifted from its initia'//
     +'l position when hidden'
      GUID(  2)='line removal is ON. It can be useful if '//
     +'you want to extract a'
      GUID(  3)='volume or some volumes from the detector'//
     +' to show them more clearly.'
      GUID(  4)='The last requested SHIFT for each volume'
      GUID(  5)='NAME is performed. Moreover, the SHIFT o'//
     +'f'
      GUID(  6)='each volume will be performed starting f'//
     +'rom where its mother has'
      GUID(  7)='been shifted, so that it''s easier to SH'//
     +'IFT nicely sets'
      GUID(  8)='of volumes using the mother-daughter rel'//
     +'ationships.'
      GUID(  9)='If ''.'' is given as the name of the vol'//
     +'ume'
      GUID( 10)='to be shifted, the shifts for all volume'//
     +'s will be reset.'
      GUID( 11)='The following commands will produce the '//
     +'translation along'
      GUID( 12)='the Z-axis of the previously drawn volum'//
     +'e:'
      GUID( 13)=' EXAMPLE -'
      GUID( 14)=' dopt hide on'
      GUID( 15)=' satt * seen -2'
      GUID( 16)=' draw NAME 40 40 0 10 10 .01 .01'
      GUID( 17)=' shift NAME 0 0 10'
      CALL KUGUID('SHIFT',GUID, 17,'S')
      CALL KUACT('SHIFT',GXDRAW)
 
      CALL KUNWG( 221)
      CALL KUCMD(' ','BOMB','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('BOMB','BOOM',' Exploding factor for volumes position',
     +'R','S')
      CALL KUPVAL('BOMB','BOOM',0,0.,' ','D')
      GUID(  1)='To ''explode'' the detector. If BOOM is '//
     +'positive (values smaller'
      GUID(  2)='than 1. are suggested, but any value is '//
     +'possible)'
      GUID(  3)='all the volumes are shifted by a distanc'//
     +'e'
      GUID(  4)='proportional to BOOM along the direction'//
     +' between their centre'
      GUID(  5)='and the origin of the MARS; the volumes '//
     +'which are symmetric'
      GUID(  6)='with respect to this origin are simply n'//
     +'ot shown.'
      GUID(  7)='BOOM equal to 0 resets the normal mode.'
      GUID(  8)='A negative (greater than -1.) value of'
      GUID(  9)='BOOM will cause an ''implosion''; for ev'//
     +'en lower values of BOOM'
      GUID( 10)='the volumes'' positions will be reflecte'//
     +'d respect to the origin.'
      GUID( 11)='This command can be useful to improve th'//
     +'e 3D effect for very'
      GUID( 12)='complex detectors. The following command'//
     +'s will make explode the'
      GUID( 13)='detector:'
      GUID( 14)=' EXAMPLE -'
      GUID( 15)=' dopt hide on'
      GUID( 16)=' satt * seen 1'
      GUID( 17)=' draw NAME 40 40 0 10 10 .01 .01'
      GUID( 18)=' bomb 1'
      GUID( 19)=' next'
      GUID( 20)=' draw NAME 40 40 0 10 10 .01 .01'
      CALL KUGUID('BOMB',GUID, 20,'S')
      CALL KUACT('BOMB',GXDRAW)
 
      CALL KUNWG( 379)
      CALL KUCMD(' ','DTREE','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DTREE','NAME','Volume name','CO','S')
      CALL KUPVAL('DTREE','NAME',0,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DTREE','LEVMAX','Depth level','IO','S')
      CALL KUPVAL('DTREE','LEVMAX',3,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DTREE','ISELT','Options','IO','S')
      CALL KUPVAL('DTREE','ISELT',111,0.,' ','D')
      GUID(  1)='This command allows the drawing of the l'//
     +'ogical tree,'
      GUID(  2)='displaying the name, the multiplicity an'//
     +'d other information about the volumes,'
      GUID(  3)='via a call to GDTREE(name,levmax,isel):'
      GUID(  4)='if the third parameter is not given (def'//
     +'ault), the command will'
      GUID(  5)='produce the drawing of the tree displayi'//
     +'ng, for each volume, the'
      GUID(  6)='number of the following levels (red arro'//
     +'ws) and of the preceeding'
      GUID(  7)='levels (green arrows); then the control '//
     +'is automatically given to the'
      GUID(  8)='mouse: clicking on the left button when '//
     +'the cursor is inside a volume''s'
      GUID(  9)='pave will perform a DSPEC for that volum'//
     +'e; doing the same when the cursor'
      GUID( 10)='is on a red arrow, will perform a DTREE '//
     +'for the relative volume (the'
      GUID( 11)='number of levels displayed depending on '//
     +'the clicked arrow); doing the'
      GUID( 12)='same for the ''i-th'' green arrow of a g'//
     +'iven volume, will perform a DTREE'
      GUID( 13)='for its mother-volume staying ''i'' leve'//
     +'ls before.'
      GUID( 14)='If running with X-windows, the drawing o'//
     +'f the specification (DSPEC)'
      GUID( 15)='is performed'
      GUID( 16)='in a different window to speed up the sc'//
     +'anning of the tree.'
      GUID( 17)='Iterating this procedure it is possible '//
     +'to analyse very easily and quickly'
      GUID( 18)='any kind of tree. Clicking the right but'//
     +'ton of the mouse will return'
      GUID( 19)='the control to the command mode.'
      GUID( 20)='If the ISELT parameter is given,'
      GUID( 21)='then the TREE will work as in the'
      GUID( 22)='previous version, with ISELT up to 10001'//
     +'.'
      GUID( 23)='The following command will perform a dra'//
     +'wing of the tree and give the'
      GUID( 24)='control to the user via the mouse:'
      GUID( 25)=' EXAMPLE -'
      GUID( 26)=' dtree NAME 3'
      CALL KUGUID('DTREE',GUID, 26,'S')
      CALL KUACT('DTREE',GXDRAW)
 
      CALL KUNWG(  87)
      CALL KUCMD(' ','DSPEC','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DSPEC','NAME','Volume name','C','S')
      GUID(  1)='Trough a call to GDSPEC(name), this comm'//
     +'and allows one to show three'
      GUID(  2)='views of the volume (two cut-views and a'//
     +' 3D view), together with'
      GUID(  3)='its geometrical specifications. The 3D d'//
     +'rawing will'
      GUID(  4)='be performed according the current value'//
     +'s of the options HIDE and'
      GUID(  5)='SHAD and according the current CVOL clip'//
     +'ping parameters for that'
      GUID(  6)='volume.'
      CALL KUGUID('DSPEC',GUID,  6,'S')
      CALL KUACT('DSPEC',GXDRAW)
 
      CALL KUNWG( 107)
      CALL KUCMD(' ','DFSPC','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DFSPC','NAME','Volume name','C','S')
      CALL KUNDPV(   1,   1,   1,   1,   2)
      CALL KUPAR('DFSPC','CSORT','Alphabetic sorting flag','CO','S')
      CALL KUPVAL('DFSPC','CSORT',0,0.,'N','D')
      CALL KUPVAL('DFSPC','CSORT',0,0.,'Y,N,0,1','V')
      CALL KUNDPV(   1,   1,   1,   1,   2)
      CALL KUPAR('DFSPC','CINTER','Interactive/Batch version','CO','S')
      CALL KUPVAL('DFSPC','CINTER',0,0.,'I','D')
      CALL KUPVAL('DFSPC','CINTER',0,0.,'I,B,0,1','V')
      GUID(  1)=' CALL GDFSPC(name,isort,inter)'
      GUID(  2)='Same as DSPEC, but it will draw the spec'//
     +'ifications for all the volumes.'
      GUID(  3)='If the alphabetic sorting flag is YES, a'//
     +'ll pictures will be drawn in ascending'
      GUID(  4)='alphabetic order; isort is set to 1.'
      GUID(  5)='If INTERACTIVE, (inter=1), the routine w'//
     +'ill prompt the user at each plot'
      GUID(  6)='before doing a clear screen, otherwise i'//
     +'t will clear automatically'
      GUID(  7)='the screen before starting a new frame.'
      CALL KUGUID('DFSPC',GUID,  7,'S')
      CALL KUACT('DFSPC',GXDRAW)
 
      CALL KUNWG(  27)
      CALL KUCMD(' ','DTEXT','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DTEXT','X0','X-coord. (horizontal) of text string','R'
     +,'S')
      CALL KUPVAL('DTEXT','X0',0,10.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DTEXT','Y0','Y-coord. (vertical) of text string','R'
     +,'S')
      CALL KUPVAL('DTEXT','Y0',0,10.,' ','D')
      CALL KUNDPV(   2,   1,   1,   0,   1)
      CALL KUPAR('DTEXT','TEXT','Text string','C','S')
      CALL KUPVAL('DTEXT','TEXT',0,0.,'GEANT','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DTEXT','SIZE','Character size (cm)','R','S')
      CALL KUPVAL('DTEXT','SIZE',0,.5,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DTEXT','ANGLE','Rotation angle (deg)','R','S')
      CALL KUPVAL('DTEXT','ANGLE',0,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DTEXT','LWID','Line width','I','S')
      CALL KUPVAL('DTEXT','LWID',4,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   1,   4)
      CALL KUPAR('DTEXT','CENT','Centering option','C','S')
      CALL KUPVAL('DTEXT','CENT',0,0.,'CENT','D')
      CALL KUPVAL('DTEXT','CENT',0,0.,'CENT,LEFT,RIGH','V')
      GUID(  1)=' CALL GDRAWT(x0,y0,text,size,angle,lwid,'//
     +'opt)'
      GUID(  2)='It allows one to draw some text in the c'//
     +'urrent picture.'
      CALL KUGUID('DTEXT',GUID,  2,'S')
      CALL KUACT('DTEXT',GXDRAW)
 
      CALL KUNWG(  33)
      CALL KUCMD(' ','DVECTOR','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DVECTOR','XVECT','Vector containing X-coord. (horizont
     +al)','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DVECTOR','YVECT','Vector containing Y-coord. (vertical
     +)','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DVECTOR','NPOINT','Number of coord.','I','S')
      GUID(  1)='Draw a polyline of ''npoint'' point via'
      GUID(  2)='a call to GDRAWV(xvect,yvect,npoint)'
      GUID(  3)='where xvect and yvect are two KUIP vecto'//
     +'rs'
      CALL KUGUID('DVECTOR',GUID,  3,'S')
      CALL KUACT('DVECTOR',GXDRAW)
 
      CALL KUNWG(  16)
      CALL KUCMD(' ','DSCALE','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DSCALE','U','U-coord. (horizontal) of the centre of sc
     +ale','R','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DSCALE','V','V-coord. (vertical) of the centre of scal
     +e','R','S')
      GUID(  1)=' CALL GDSCAL(u,v)'
      GUID(  2)='It draws a scale centered in U,V.'
      CALL KUGUID('DSCALE',GUID,  2,'S')
      CALL KUACT('DSCALE',GXDRAW)
 
      CALL KUNWG(  56)
      CALL KUCMD(' ','DAXIS','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DAXIS','X0','X-coord. of axis origin','R','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DAXIS','Y0','Y-coord. of axis origin','R','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DAXIS','Z0','Z-coord. of axis origin','R','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DAXIS','DX','Axis size','R','S')
      GUID(  1)=' CALL GDAXIS(x0,y0,z0,dx)'
      GUID(  2)='This commmand superimposes the axis of t'//
     +'he MARS on the'
      GUID(  3)='current picture. It is useful for findin'//
     +'g immediately the'
      GUID(  4)='orientation of the current drawing of th'//
     +'e detector in the space.'
      CALL KUGUID('DAXIS',GUID,  4,'S')
      CALL KUACT('DAXIS',GXDRAW)
 
      CALL KUNWG(  41)
      CALL KUCMD(' ','DMAN','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DMAN','U','U-coord. (horizontal) of the centre of man'
     +,'R','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DMAN','V','V-coord. (vertical) of the centre of man'
     +,'R','S')
      CALL KUNDPV(   1,   1,   1,   1,   2)
      CALL KUPAR('DMAN','TYPE','Man or Girl','C','S')
      CALL KUPVAL('DMAN','TYPE',0,0.,'MAN','D')
      CALL KUPVAL('DMAN','TYPE',0,0.,'MAN,GIRL','V')
      GUID(  1)=' CALL GDMAN(u,v), CALL GDGIRL(u,v)'
      GUID(  2)='It superimposes the picure of a man (or '//
     +'girl) with the'
      GUID(  3)='same scale factors as the detector in th'//
     +'e current drawing.'
      CALL KUGUID('DMAN',GUID,  3,'S')
      CALL KUACT('DMAN',GXDRAW)
 
      CALL KUNWG( 153)
      CALL KUCMD(' ','DHEAD','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DHEAD','ISEL','Option flag','IO','S')
      CALL KUPVAL('DHEAD','ISEL',111110,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DHEAD','NAME','Title','CO','S')
      CALL KUPVAL('DHEAD','NAME',0,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DHEAD','CHRSIZ','Character size (cm) of title NAME','R
     +O','S')
      CALL KUPVAL('DHEAD','CHRSIZ',0,0.6,' ','D')
      GUID(  1)=' CALL GDHEAD(isel,name,chrsiz)'
      GUID(  2)='ISEL ='
      GUID(  3)=' 0      to have only the header lines'
      GUID(  4)=' xxxxx1 to add the text name centered on'//
     +' top of header'
      GUID(  5)=' xxxx1x to add global detector name (fir'//
     +'st volume) on left'
      GUID(  6)=' xxx1xx to add date on right'
      GUID(  7)=' xx1xxx to select thick characters for t'//
     +'ext on top of header'
      GUID(  8)=' x1xxxx to add the text ''EVENT NR x'' o'//
     +'n top of header'
      GUID(  9)=' 1xxxxx to add the text ''RUN NR x'' on '//
     +'top of header'
      GUID( 10)='NOTE that ISEL=x1xxx1 or ISEL=1xxxx1 are'//
     +' illegal choices,'
      GUID( 11)='i.e. they generate overwritten text.'
      GUID( 12)='NAME is the title'
      GUID( 13)='and CHRSIZ the character size in cm of t'//
     +'ext name.'
      CALL KUGUID('DHEAD',GUID, 13,'S')
      CALL KUACT('DHEAD',GXDRAW)
 
      CALL KUNWG( 119)
      CALL KUCMD(' ','MEASURE','C')
      GUID(  1)='Position the cursor on the first point ('//
     +'u1,v1) and hit the space bar(GKS).'
      GUID(  2)='Position the cursor on the second point '//
     +'(u2,v2) and hit the space bar(GKS).'
      GUID(  3)='Clicking the left button of the mouse (X'//
     +'11) will have the same effect as'
      GUID(  4)='hiting the space bar (GKS).'
      GUID(  5)='The command will compute and print the d'//
     +'istance in space separating'
      GUID(  6)='the two points on the projection view. I'//
     +'t can be useful to measure'
      GUID(  7)='distances either between volumes or betw'//
     +'een tracks or hits.'
      CALL KUGUID('MEASURE',GUID,  7,'S')
      CALL KUACT('MEASURE',GXDRAW)
 
      CALL KUNWG( 108)
      CALL KUCMD(' ','PICK','C')
      GUID(  1)='Activates graphic input to identify dete'//
     +'ctor elements'
      GUID(  2)='in a cut view. Clicking on the left butt'//
     +'on of the mouse when'
      GUID(  3)='the cursor is in a given point of the dr'//
     +'awing and clicking again'
      GUID(  4)='(outside the detector) will produce the '//
     +'following effect:'
      GUID(  5)='a line joininig the two points will be d'//
     +'rawn together with'
      GUID(  6)='the name and the medium number of the vo'//
     +'lume picked'
      GUID(  7)='with the first clicking close to the sec'//
     +'ond point.'
      CALL KUGUID('PICK',GUID,  7,'S')
      CALL KUACT('PICK',GXPICK)
 
      CALL KUNWG( 736)
      CALL KUCMD(' ','MOVE','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('MOVE','NAME','Volume name','C','S')
      CALL KUPVAL('MOVE','NAME',0,0.,'    ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('MOVE','NOPT','T=tracks,H=hits,TH=both','CO','S')
      CALL KUPVAL('MOVE','NOPT',0,0.,'    ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('MOVE','SAMP','Sample mode','CO','S')
      CALL KUPVAL('MOVE','SAMP',0,0.,'OFF ','D')
      GUID(  1)='Positioning some daughter volumes inside'//
     +' a ''mother'', it can be'
      GUID(  2)='important to check if overlaps between s'//
     +'uch volumes have occurred.'
      GUID(  3)='Instead of putting the drawing in a view'//
     +' bank, zooming, and iterating'
      GUID(  4)='the process for different viewing angles'//
     +' of the same detector, the'
      GUID(  5)='MOVE facility has been developed (for ma'//
     +'chines running with X11):'
      GUID(  6)='it is sufficient to draw a view of the v'//
     +'olumes to be analysed (after'
      GUID(  7)='setting the proper SEEN, COLO, etc. attr'//
     +'ibutes) and then to enter'
      GUID(  8)='''MOVE'' followed by the same ''NAME'' u'//
     +'sed for the last command DRAW.'
      GUID(  9)='The detector will appear in a panel with'//
     +' five buttons at the'
      GUID( 10)='bottom: THETA, PHI, TRASL, ZOOM, OFF. Cl'//
     +'icking on the left button'
      GUID( 11)='of the mouse, when the cursor is inside '//
     +'the THETA area, will rotate the'
      GUID( 12)='detector along the polar angle theta acc'//
     +'ording to the'
      GUID( 13)='backward-to-forward movement of the mous'//
     +'e'
      GUID( 14)='(clicking up and down the left button if'
      GUID( 15)='not in sample mode); clicking on the rig'//
     +'ht button of'
      GUID( 16)='the mouse will stop the rotation; clicki'//
     +'ng now on the'
      GUID( 17)='left button of the mouse when inside the'//
     +' PHI area will activate a'
      GUID( 18)='rotation along the polar angle phi. In t'//
     +'he same way, activating the'
      GUID( 19)='TRASL button, the detector can be transl'//
     +'ated in the u,v plane'
      GUID( 20)='of the screen according to the 2D-moveme'//
     +'nt of the mouse. Finally,'
      GUID( 21)='activating the ZOOM button, the detector'//
     +' will be zoomed (or unzoomed)'
      GUID( 22)='according to the backward-to-forward mov'//
     +'ement of the mouse. Clicking on the'
      GUID( 23)='OFF button will return the control to th'//
     +'e ''command mode''. The MOVE'
      GUID( 24)='command will work also with hidden line '//
     +'removal and shading options'
      GUID( 25)='(when SHAD is on the background will be '//
     +'black);'
      GUID( 26)='moreover, if the volumes are clipped, ex'//
     +'ploded, shifted, etc., they'
      GUID( 27)='will be ''MOVED'' with these features as'//
     +' well.'
      GUID( 28)='Tracks and hits of a previously stored p'//
     +'hysical event can be moved'
      GUID( 29)='together with the detector, allowing a d'//
     +'ynamical 3-D analysis of the'
      GUID( 30)='simulated events. Clicking the central b'//
     +'utton of the mouse when a good'
      GUID( 31)='view of the event is found, will stop an'//
     +'y movement and the mouse will'
      GUID( 32)='allow the normal picking capabilities fi'//
     +'rst for the tracks and then for'
      GUID( 33)='the hits. After clicking of the right bu'//
     +'tton, the normal'
      GUID( 34)='movement will restart to find another in'//
     +'teresting view of the event'
      GUID( 35)='and to iterate the process.'
      GUID( 36)='The MOVE is also available in sample mod'//
     +'e when SAMP is equal to ''ON''.'
      GUID( 37)='The following commands will produce a dr'//
     +'awing of a volume'
      GUID( 38)='and then will give the control to the MO'//
     +'VE panel; try the following'
      GUID( 39)='possibilities:'
      GUID( 40)=' EXAMPLE 1 -'
      GUID( 41)=' dopt hide off'
      GUID( 42)=' satt * seen -2'
      GUID( 43)=' draw NAME 40 40 0 10 10 .01 .01'
      GUID( 44)=' move NAME'
      GUID( 45)=' EXAMPLE 2 -'
      GUID( 46)=' dopt hide on'
      GUID( 47)=' satt * seen -2'
      GUID( 48)=' draw NAME 40 40 0 10 10 .01 .01'
      GUID( 49)=' move NAME'
      GUID( 50)=' EXAMPLE 3 -'
      GUID( 51)=' dopt shad on'
      GUID( 52)=' satt * colo 3'
      GUID( 53)=' satt * fill 2'
      GUID( 54)=' dopt edge off'
      GUID( 55)=' draw NAME 40 40 0 10 10 .01 .01'
      GUID( 56)=' move NAME'
      CALL KUGUID('MOVE',GUID, 56,'S')
      CALL KUACT('MOVE',GXDRAW)
 
      CALL KUNWG( 431)
      CALL KUCMD(' ','LENS','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('LENS','KNUM','View bank identifier','I','S')
      CALL KUPVAL('LENS','KNUM',1,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('LENS','KSAM','Sample mode','CO','S')
      CALL KUPVAL('LENS','KSAM',0,0.,'OFF ','D')
      GUID(  1)='Interactive zooming for detectors and ev'//
     +'ents when running'
      GUID(  2)='with X-windows. Using this command, when'//
     +' showing the contents of a'
      GUID(  3)='view bank, it is possible to click (left'//
     +' button) in two points of the'
      GUID(  4)='drawing (which will represent the left u'//
     +'pper corner and the right'
      GUID(  5)='bottom corner of the part to be zoomed).'//
     +' After the second click'
      GUID(  6)='a new ''window'' will appear to fit the '//
     +'frame defined'
      GUID(  7)='by the two clicks and it will show a zoo'//
     +'med view as seen from a'
      GUID(  8)='lens with those dimensions. Clicking now'//
     +' the central button will'
      GUID(  9)='translate the lens over the drawing, whi'//
     +'le clicking the right button'
      GUID( 10)='will stop it. Moreover, clicking the lef'//
     +'t button of the'
      GUID( 11)='mouse, the lens will increase (or decrea'//
     +'se) its magnification'
      GUID( 12)='power according to the backward-to-forwa'//
     +'rd movement of the mouse.'
      GUID( 13)='A click on the right button will stop th'//
     +'is action and it is possible'
      GUID( 14)='to restart the translation of the lens o'//
     +'r, clicking'
      GUID( 15)='on the right button again, to make the l'//
     +'ens disappear. It is then possible'
      GUID( 16)='to open another ''window-lens'' with dif'//
     +'ferent dimensions. Thus,'
      GUID( 17)='this command can be useful to scan detai'//
     +'led parts of a detector or'
      GUID( 18)='to scan hits and showers for events. Cli'//
     +'cking the right'
      GUID( 19)='button when no lens is displayed will re'//
     +'turn the control to the'
      GUID( 20)='''command mode''. The LENS is also avail'//
     +'able in sample mode when KSAM is'
      GUID( 21)='''ON''.'
      GUID( 22)='The following commands will fill a view '//
     +'bank and will'
      GUID( 23)='allow to scan the detector and an event '//
     +'previously stored'
      GUID( 24)='via the use of LENS (when running'
      GUID( 25)='with X-windows):'
      GUID( 26)=' EXAMPLE -'
      GUID( 27)=' satt * seen 1'
      GUID( 28)=' dopen 1'
      GUID( 29)=' draw NAME 40 40 0 10 10 .01 .01'
      GUID( 30)=' dxyz 0'
      GUID( 31)=' dhits * * 0 0 .2'
      GUID( 32)=' dclose'
      GUID( 33)=' dsh 1'
      GUID( 34)=' lens 1 on'
      CALL KUGUID('LENS',GUID, 34,'S')
      CALL KUACT('LENS',GXDRAW)
 
      CALL KUNWG( 479)
      CALL KUCMD(' ','ZOOM','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('ZOOM','ZFU','Zoom factor for U-coord. (horizontal)','R
     +O','S')
      CALL KUPVAL('ZOOM','ZFU',0,2.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('ZOOM','ZFV','Zoom factor for V-coord. (vertical)','RO'
     +,'S')
      CALL KUPVAL('ZOOM','ZFV',0,2.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('ZOOM','ISEL','Options','IO','S')
      CALL KUPVAL('ZOOM','ISEL',1,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('ZOOM','UZ0','U-coord. of the centre of zoom rectangle'
     +,'RO','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('ZOOM','VZ0','V-coord. of the centre of zoom rectangle'
     +,'RO','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('ZOOM','U0','U-coord. of the centre of resulting zoomed
     + rectangle','RO','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('ZOOM','V0','V-coord. of the centre of resulting zoomed
     + rectangle','RO','S')
      GUID(  1)=' CALL GDZOOM(zfu,zfv,uz0,vz0,u0,v0)'
      GUID(  2)='This command sets the zoom parameters th'//
     +'at will be used by'
      GUID(  3)='subsequent calls to the drawing routines'//
     +'. Each zoom operation is always'
      GUID(  4)='relative to the status of the current zo'//
     +'om parameters.'
      GUID(  5)='The scale factors in u,v are respectivel'//
     +'y  zfu,zfv.'
      GUID(  6)='zfu=0 (or zfv=0) will act as a reset (i.'//
     +'e. unzoomed viewing).'
      GUID(  7)='The zoom is computed around uz0,vz0 (use'//
     +'r coordinates),'
      GUID(  8)='and the resulting picture will be center'//
     +'ed at u0,v0.'
      GUID(  9)='The use of the space bar is replaced by '//
     +'the left button of the mouse'
      GUID( 10)='running with X11:'
      GUID( 11)='If isel=0 :'
      GUID( 12)=' 1. position the cursor at (uz0,vz0)'
      GUID( 13)=' 2. type the space bar (GKS)'
      GUID( 14)='(u0,v0 are chosen at centre of screen)'
      GUID( 15)='If isel=1 :'
      GUID( 16)=' 1. position the cursor at first corner '//
     +'of zoom rectangle'
      GUID( 17)=' 2. type the space bar (GKS)'
      GUID( 18)=' 3. position the cursor at second corner'//
     +' of zoom rectangle'
      GUID( 19)=' 4. type the space bar (GKS)'
      GUID( 20)='(zfu,zfv are chosen according to the zoo'//
     +'m rectangle;'
      GUID( 21)='uz0,vz0 are chosen at the centre of the '//
     +'zoom rectangle;'
      GUID( 22)='u0,v0 are chosen at centre of screen)'
      GUID( 23)='If isel=2 :'
      GUID( 24)=' 1. position the cursor at (uz0,vz0)'
      GUID( 25)=' 2. type the space bar (GKS)'
      GUID( 26)=' 3. position the cursor at (u0,v0)'
      GUID( 27)=' 4. type the space bar (GKS)'
      GUID( 28)='If isel=1000+n and running with X-window'//
     +'s:'
      GUID( 29)=' 1. n must be the identifier of an activ'//
     +'e view bank'
      GUID( 30)=' 2. clicking on the left button of the m'//
     +'ouse will display'
      GUID( 31)='    a zoomed view (computed around the c'//
     +'ursor position) of'
      GUID( 32)='    the previous drawing in a new window'
      GUID( 33)=' 3. it is now possible to iterate the zo'//
     +'oming from the new window'
      GUID( 34)=' 4. clicking on the right button will re'//
     +'turn the control to the'
      GUID( 35)='    main window'
      GUID( 36)=' 5. clicking on the left button it is po'//
     +'ssible to open new windows'
      GUID( 37)='    zooming in other points of the detec'//
     +'tor'
      GUID( 38)=' 6. clicking on the right button when th'//
     +'e main window is active'
      GUID( 39)='    will return the control to the ''com'//
     +'mand mode''.'
      CALL KUGUID('ZOOM',GUID, 39,'S')
      CALL KUACT('ZOOM',GXDRAW)
 
      CALL KUNWG(  17)
      CALL KUCMD(' ','DXYZ','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DXYZ','ITRA','Track number','IO','S')
      CALL KUPVAL('DXYZ','ITRA',0,0.,' ','D')
      GUID(  1)=' CALL GDXYZ(itra)'
      GUID(  2)='Draw tracks previously stored via GSXYZ.'
      CALL KUGUID('DXYZ',GUID,  2,'S')
      CALL KUACT('DXYZ',GXDRAW)
 
      CALL KUNWG(  99)
      CALL KUCMD(' ','KXYZ','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('KXYZ','EPSILO','Delta angle','RO','S')
      CALL KUPVAL('KXYZ','EPSILO',0,0.25,' ','D')
      GUID(  1)=' CALL GKXYZ(epsilo)'
      GUID(  2)='The picking of track points requires the'//
     +' JXYZ data structure'
      GUID(  3)='and is  repeated until the character typ'//
     +'ed is ''Q'' or ''q'' (GKS)'
      GUID(  4)='or the right button of the mouse is clic'//
     +'ked (X11).'
      GUID(  5)='EPSILO is the delta angle used for picki'//
     +'ng; if EPSILO=0'
      GUID(  6)='there is no optimization performed and'
      GUID(  7)='over all the track points the one neares'//
     +'t to the pick'
      GUID(  8)='point is taken.'
      CALL KUGUID('KXYZ',GUID,  8,'S')
      CALL KUACT('KXYZ',GXDRAW)
 
      CALL KUNWG(  28)
      CALL KUCMD(' ','DPART','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DPART','ITRA','Track number','IO','S')
      CALL KUPVAL('DPART','ITRA',0,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DPART','ISEL','Option flag','IO','S')
      CALL KUPVAL('DPART','ISEL',11,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DPART','SIZE','Character size (cm) for particle names'
     +,'RO','S')
      CALL KUPVAL('DPART','SIZE',0,0.25,' ','D')
      GUID(  1)=' CALL GDPART(itra,isel,size)'
      GUID(  2)=' isel=x1 to draw the track number'
      GUID(  3)=' isel=1x to draw the particle name'
      CALL KUGUID('DPART',GUID,  3,'S')
      CALL KUACT('DPART',GXDRAW)
 
      CALL KUNWG( 158)
      CALL KUCMD(' ','DHITS','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DHITS','IUSET','User set identifier','CO','S')
      CALL KUPVAL('DHITS','IUSET',0,0.,'*','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DHITS','IUDET','User detector identifier','CO','S')
      CALL KUPVAL('DHITS','IUDET',0,0.,'*','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DHITS','ITRA','Number of the selected track','IO','S')
      CALL KUPVAL('DHITS','ITRA',0,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DHITS','ISYMB','Character selection number','IO','S')
      CALL KUPVAL('DHITS','ISYMB',0,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DHITS','SSYMB','Size of characters (cm)','RO','S')
      CALL KUPVAL('DHITS','SSYMB',0,0.1,' ','D')
      GUID(  1)='CALL GDHITS(iuset,iudet,itra,isymb,ssymb'//
     +').'
      GUID(  2)='The character plotted at each hit point '//
     +'may be chosen by isymb :'
      GUID(  3)='      -1   (small) hardware points      '//
     +'       (fast)'
      GUID(  4)='       0   software crosses             '//
     +'       (default)'
      GUID(  5)='   840,850   empty/full circles         '//
     +'         (slow)'
      GUID(  6)='   841,851   empty/full squares         '//
     +'         (slow)'
      GUID(  7)='   842,852   empty/full triangles (up)  '//
     +'         (slow)'
      GUID(  8)='   843,853   empty diamond/full triangle'//
     +' (down)  (slow)'
      GUID(  9)='   844,854   empty/full stars           '//
     +'         (slow)'
      GUID( 10)='Except for isymb=-1, the size of the cha'//
     +'racter on the screen can be'
      GUID( 11)='chosen by SSYMB cm.'
      CALL KUGUID('DHITS',GUID, 11,'S')
      CALL KUACT('DHITS',GXDRAW)
 
      CALL KUNWG( 177)
      CALL KUCMD(' ','KHITS','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('KHITS','IUSET','User set identifier','CO','S')
      CALL KUPVAL('KHITS','IUSET',0,0.,'*','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('KHITS','IUDET','User detector identifier','CO','S')
      CALL KUPVAL('KHITS','IUDET',0,0.,'*','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('KHITS','EPSILO','Pick aperture','RO','S')
      CALL KUPVAL('KHITS','EPSILO',0,0.1,' ','D')
      GUID(  1)=' CALL GKHITS(iuset,iudet,epsilo)'
      GUID(  2)='The picking of hit points requires the a'//
     +'ppropriate JSET data structure'
      GUID(  3)='have been filled'
      GUID(  4)='and is  repeated until the character typ'//
     +'ed is ''Q'' or ''q'' (GKS) or the'
      GUID(  5)='right button of the mouse is clicked (X1'//
     +'1).'
      GUID(  6)='If the character typed to pick is ''K'' '//
     +'or ''k'' then the'
      GUID(  7)='kinematics of the corresponding track is'//
     +' also printed.'
      GUID(  8)='The search is made of all the hits of al'//
     +'l tracks in'
      GUID(  9)='detector IUDET of set IUSET.'
      GUID( 10)='EPSILO is the pick aperture; if EPSILO<0'//
     +' its absolute value is taken'
      GUID( 11)='and in addition the pick aperture is dra'//
     +'wn; if EPSILO=0'
      GUID( 12)='there is an infinite pick aperture and'
      GUID( 13)='over all the hits the one nearest to the'//
     +' pick point is taken.'
      CALL KUGUID('KHITS',GUID, 13,'S')
      CALL KUACT('KHITS',GXDRAW)
 
      CALL KUNWG( 223)
      CALL KUCMD(' ','DCHIT','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DCHIT','IUSET','User set identifier','CO','S')
      CALL KUPVAL('DCHIT','IUSET',0,0.,'*','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DCHIT','IUDET','User detector identifier','CO','S')
      CALL KUPVAL('DCHIT','IUDET',0,0.,'*','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DCHIT','ITRA','Number of the selected track','IO','S')
      CALL KUPVAL('DCHIT','ITRA',0,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DCHIT','ISYMB','Character selection number','IO','S')
      CALL KUPVAL('DCHIT','ISYMB',0,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DCHIT','SIZMAX','Maximum character size (cm)','RO','S'
     +)
      CALL KUPVAL('DCHIT','SIZMAX',0,1.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DCHIT','IHIT','Index of array HITS','IO','S')
      CALL KUPVAL('DCHIT','IHIT',4,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DCHIT','HITMIN','Lower boundary of HITS(IHIT)','RO','S
     +')
      CALL KUPVAL('DCHIT','HITMIN',0,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DCHIT','HITMAX','Upper boundary of HITS(IHIT)','RO','S
     +')
      CALL KUPVAL('DCHIT','HITMAX',0,0.,' ','D')
      GUID(  1)=' CALL GDCHIT(iuset,iudet,itra,isymb,sizm'//
     +'ax,ihit,hitmin,hitmax)'
      GUID(  2)='The character plotted at each hit point '//
     +'may be chosen via'
      GUID(  3)='CSYMB; isymb is composed as:'
      GUID(  4)='      -1   (small) hardware points      '//
     +'       (fast)'
      GUID(  5)='       0   software crosses             '//
     +'       (default)'
      GUID(  6)=' 840,850   empty/full circles           '//
     +'       (slow)'
      GUID(  7)=' 841,851   empty/full squares           '//
     +'       (slow)'
      GUID(  8)=' 842,852   empty/full triangles (up)    '//
     +'       (slow)'
      GUID(  9)=' 843,853   empty diamond/full triangle ('//
     +'down)  (slow)'
      GUID( 10)=' 844,854   empty/full stars             '//
     +'       (slow)'
      GUID( 11)='Except for isymb=-1 the SIZE of the char'//
     +'acter on the screen'
      GUID( 12)='is a function of HITS(IHIT), the array c'//
     +'ontaining the calorimeter'
      GUID( 13)='quantity, with HITMIN and HITMAX definin'//
     +'g its range.'
      GUID( 14)='The maximum character size (used in over'//
     +'flow) is SIZMAX.'
      GUID( 15)=' SIZE = SIZMAX * ( HITS(IHIT) - HITMIN )'//
     +' / HITMAX'
      CALL KUGUID('DCHIT',GUID, 15,'S')
      CALL KUACT('DCHIT',GXDRAW)
 
      CALL KUNWG(  10)
      CALL KUCMD(' ','DUVIEW','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DUVIEW','NAME','Detector name','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DUVIEW','TYPE','View name','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DUVIEW','CPXTYP','Complexity name','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DUVIEW','IVIEW','View number where picture is stored',
     +'IO','S')
      CALL KUPVAL('DUVIEW','IVIEW',0,0.,' ','D')
      GUID(  1)=' CALL GUVIEW(name,type,cpxtyp,iview)'
      CALL KUGUID('DUVIEW',GUID,  1,'S')
      CALL KUACT('DUVIEW',GXDRAW)
 
      CALL KUNWG(   0)
 
      CALL KUNDPV(   1,   1,   1,   0,   1)
 
      CALL KUCMD(' ',' ','E')
 
      CALL KUCMD('/',' ','SW')
 
      END
