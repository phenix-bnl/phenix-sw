*CMZ :  2.04/00 01/04/93  15.54.33  by  S.R.Tonse
*-- Author :
      SUBROUTINE GKFORT
      INTEGER MGUIDL
      PARAMETER (MGUIDL=199)
      CHARACTER*80 GUID
      COMMON /KCGUID/ GUID(MGUIDL)
      EXTERNAL GXFORT
 
      CALL KUNWG(  12)
      CALL KUCMD(' ','FORTRAN','C')
      GUID(  1)='Interface to COMIS and FORTRAN Input/Out'//
     +'put.'
      CALL KUGUID('FORTRAN',GUID,  1,'S')
 
      CALL KUCMD('FORTRAN',' ','SW')
 
      CALL KUNWG( 197)
      CALL KUCMD(' ','CALL','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('CALL','UROUT','User routine','C','S')
      GUID(  1)='Invoke the COMIS FORTRAN interpreter.'
      GUID(  2)='COMIS allows to execute FORTRAN routines'//
     +' without recompiling'
      GUID(  3)='and relinking. It communicates'
      GUID(  4)='with GXINT commands through vectors and '//
     +'functions.'
      GUID(  5)='Execute the routine UROUT.'
      GUID(  6)='UROUT may be a routine compiled and link'//
     +'ed with GEANT.'
      GUID(  7)='For example : CALL GPRINT(''KINE'',0).'
      GUID(  8)='UROUT may also be the name of a file whi'//
     +'ch can be edited interactively'
      GUID(  9)='with the command EDIT. For example if fi'//
     +'le UROUT.FOR contains:'
      GUID( 10)='     SUBROUTINE UROUT(N)'
      GUID( 11)='     SUM=0.'
      GUID( 12)='     DO 10 I=1,N'
      GUID( 13)='       SUM=SUM+I'
      GUID( 14)='  10 CONTINUE'
      GUID( 15)='     PRINT *,SUM'
      GUID( 16)='     END'
      GUID( 17)='Then one can type CALL UROUT.FOR(10).  T'//
     +'he routine UROUT may also'
      GUID( 18)='contains references to some of the KERNL'//
     +'IB routines,HBOOK,ZEBRA'
      GUID( 19)='and most of the GEANT routines.'
      CALL KUGUID('CALL',GUID, 19,'S')
      CALL KUACT('CALL',GXFORT)
 
      CALL KUNWG(  10)
      CALL KUCMD(' ','FILE','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('FILE','LUN','Logical unit number','I','S')
      CALL KUPVAL('FILE','LUN',1,0.,' ','L')
      CALL KUPVAL('FILE','LUN',128,0.,' ','H')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('FILE','FNAME','File name','C','S')
      GUID(  1)='Open a FORTRAN formatted text file.'
      CALL KUGUID('FILE',GUID,  1,'S')
      CALL KUACT('FILE',GXFORT)
 
      CALL KUNWG(  51)
      CALL KUCMD(' ','CLOSE','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('CLOSE','LUN','Logical unit number','I','S')
      CALL KUPVAL('CLOSE','LUN',1,0.,' ','L')
      CALL KUPVAL('CLOSE','LUN',128,0.,' ','H')
      GUID(  1)='Close the file on unit LUN.'
      GUID(  2)='If the file has been opened with HISTO/F'//
     +'ILE, PICTURE/FILE, etc, then'
      GUID(  3)='before closing the unit, GXINT will clos'//
     +'e correctly the file with'
      GUID(  4)='CALL HREND or RZEND,etc.'
      CALL KUGUID('CLOSE',GUID,  4,'S')
      CALL KUACT('CLOSE',GXFORT)
 
      CALL KUNWG( 294)
      CALL KUCMD(' ','FORTRAN','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('FORTRAN','FNAME','File name','C','S')
      GUID(  1)='The routines in the file FNAME will be c'//
     +'ompiled by COMIS.'
      GUID(  2)='If routines with names: UGEOM,GUKINE,GUO'//
     +'UT,UGLAST are found,'
      GUID(  3)='then they will be automatically called b'//
     +'y GXINT instead of'
      GUID(  4)='the routines with the same names compile'//
     +'d with the standard'
      GUID(  5)='Fortran compiler and linked with the app'//
     +'lication.'
      GUID(  6)='The user callable routines from the GEAN'//
     +'T library as well as'
      GUID(  7)='routines from PACKLIB (HBOOK,HPLOT,HIGZ,'//
     +'ZEBRA) may be called'
      GUID(  8)='from these user routines. All GEANT comm'//
     +'on blocks may be'
      GUID(  9)='referenced.'
      GUID( 10)='In case where the routine UGEOM is calle'//
     +'d several times,'
      GUID( 11)='it is important to DROP all the initiali'//
     +'sation data structures'
      GUID( 12)='JVOLUM,JMATE,JTMED,etc already in memory'//
     +' by using the routine GIDROP.'
      GUID( 13)=' Example of an interactive session where'//
     +' the routine UGEOM is modified:'
      GUID( 14)='.'
      GUID( 15)='   GEANT > Edit ugeom.for'
      GUID( 16)='   GEANT > Fortran ugeom.for'
      GUID( 17)='   GEANT > Call GIDROP'
      GUID( 18)='   GEANT > Call UGEOM'
      GUID( 19)='   GEANT > Dtree'
      GUID( 20)='   GEANT > Edit ugeom.for'
      GUID( 21)='   GEANT > Fortran ugeom.for'
      GUID( 22)='   GEANT > Call GIDROP'
      GUID( 23)='   GEANT > Call UGEOM'
      GUID( 24)='   GEANT > Dtree'
      GUID( 25)='If FNAME=''-'', calls to user routines i'//
     +'s reset and standard'
      GUID( 26)='routines called instead.'
      CALL KUGUID('FORTRAN',GUID, 26,'S')
      CALL KUACT('FORTRAN',GXFORT)
 
      CALL KUNWG(   0)
 
      CALL KUNDPV(   1,   1,   1,   0,   1)
 
      CALL KUCMD(' ',' ','E')
 
      CALL KUCMD('/',' ','SW')
 
      END
