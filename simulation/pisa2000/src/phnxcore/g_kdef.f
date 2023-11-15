*CMZ :  2.04/00 02/06/93  17.46.21  by  Charles F. Maguire
*CMZ :  2.01/00 15/04/93  20.46.17  by  S.R.Tonse
*-- Author :
      SUBROUTINE G_KDEF
c
c     Revision History
c 
c     Change from 100000 to 200000 the limit on PTRIG (J. Lajoie request)
c     Change from 200000 to 999999 the limit on PTRIG (Single Particles into Muon Arm)
c
      INTEGER MGUIDL
      PARAMETER (MGUIDL=199)
      CHARACTER*80 GUID
      COMMON /KCGUID/ GUID(MGUIDL)
      EXTERNAL PTRIG
 
      CALL PKEVMENU
      CALL PISAMENU
      CALL KUNWG(   0)
      CALL KUCMD(' ','PISA','C')
 
      CALL KUCMD('PISA',' ','SW')
 
      CALL KUNWG(   0)
      CALL KUCMD(' ','GLOBAL','C')
 
      CALL KUCMD('GLOBAL',' ','SW')
 
      CALL KUNWG(  70)
      CALL KUCMD(' ','PTRIG','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('PTRIG','NTRUEV','No. of true events to do','I','S')
      CALL KUPVAL('PTRIG','NTRUEV',0,0.,' ','L')
      CALL KUPVAL('PTRIG','NTRUEV',9999999,0.,' ','H')
      CALL KUACT('PTRIG',PTRIG)
      GUID(  1)='To avoid changing the gxint routines, th'//
     +'e command PISA/GLOBAL/PTRIG n will'
      GUID(  2)='run n true monte carlo events. Internall'//
     +'y it executes the normal Geant TRIGGER'
      GUID(  3)='command, but keeps its own loop count. S'//
     +'ee routine PTRIG which is where the'
      GUID(  4)='work is done if you are curious.'
      CALL KUGUID('PTRIG',GUID,  4,'S')
 
      CALL KUNWG(   0)
 
      CALL KUNDPV(   1,   1,   1,   0,   1)
 
      CALL KUCMD(' ',' ','E')
 
      CALL KUCMD('/',' ','SW')
 
      END
