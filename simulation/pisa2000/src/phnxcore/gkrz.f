*CMZ :  2.04/00 07/04/92  19.00.08  by  Federico Carminati
*-- Author :
      SUBROUTINE GKRZ
      INTEGER MGUIDL
      PARAMETER (MGUIDL=199)
      CHARACTER*80 GUID
      COMMON /KCGUID/ GUID(MGUIDL)
      EXTERNAL GXRZ
 
      CALL KUNWG(   6)
      CALL KUCMD(' ','RZ','C')
      GUID(  1)='ZEBRA/RZ commands.'
      CALL KUGUID('RZ',GUID,  1,'S')
 
      CALL KUCMD('RZ',' ','SW')
 
      CALL KUNWG(  10)
      CALL KUCMD(' ','PQUEST','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('PQUEST','IQ1','Lower limit for IQ index','IO','S')
      CALL KUPVAL('PQUEST','IQ1',1,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('PQUEST','IQ2','Upper limit for IQ index','IO','S')
      CALL KUPVAL('PQUEST','IQ2',20,0.,' ','D')
      GUID(  1)='Print the array IQUEST in /QUEST/.'
      CALL KUGUID('PQUEST',GUID,  1,'S')
      CALL KUACT('PQUEST',GXRZ)
 
      CALL KUNWG(  52)
      CALL KUCMD(' ','FILE','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('FILE','LUN','Logical unit number','I','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('FILE','FNAME','File name','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('FILE','LRECL','Record lenght in words','IO','S')
      CALL KUPVAL('FILE','LRECL',1024,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   1,   2)
      CALL KUPAR('FILE','CHOPT','Options','CO','S')
      CALL KUPVAL('FILE','CHOPT',0,0.,' ','D')
      CALL KUPVAL('FILE','CHOPT',0,0.,' ,A,N,U','V')
      GUID(  1)='Open a GRZ file.'
      GUID(  2)=' CHOPT='' '' readonly mode'
      GUID(  3)=' CHOPT=''U'' update mode'
      GUID(  4)=' CHOPT=''N'' create new file'
      GUID(  5)=' CHOPT=''I'' Read all structures from ex'//
     +'isting file'
      GUID(  6)=' CHOPT=''O'' Write all structures on fil'//
     +'e'
      CALL KUGUID('FILE',GUID,  6,'S')
      CALL KUACT('FILE',GXRZ)
 
      CALL KUNWG(  21)
      CALL KUCMD(' ','REND','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('REND','LUNRZ','Logical unit number','I','S')
      GUID(  1)='Close an RZ file opened by GRFILE on log'//
     +'ical unit LUNRZ.'
      GUID(  2)=' CALL GREND(LUNRZ)'
      CALL KUGUID('REND',GUID,  2,'S')
      CALL KUACT('REND',GXRZ)
 
      CALL KUNWG(  31)
      CALL KUCMD(' ','MDIR','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('MDIR','CHDIR','Directory name','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('MDIR','CHOPT','Options','CO','S')
      CALL KUPVAL('MDIR','CHOPT',0,0.,' ','D')
      GUID(  1)='To create a new RZ directory below the c'//
     +'urrent directory.'
      GUID(  2)='with'
      GUID(  3)=' RZTAGS(1)=''Object'''
      GUID(  4)=' RZTAGS(2)=''Idvers-NR '''
      CALL KUGUID('MDIR',GUID,  4,'S')
      CALL KUACT('MDIR',GXRZ)
 
      CALL KUNWG(  53)
      CALL KUCMD(' ','CDIR','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('CDIR','CHPATH','Path name','CO','S')
      CALL KUPVAL('CDIR','CHPATH',0,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('CDIR','CHOPT','CHOPT','CO','S')
      CALL KUPVAL('CDIR','CHOPT',0,0.,' ','D')
      GUID(  1)='Change or print the current directory.'
      GUID(  2)=' Ex.  CD dir1         ; make DIR1 the ne'//
     +'w CWD'
      GUID(  3)='      CD //file1/dir2 ; make //FILE1/DIR'//
     +'2 the new CWD'
      GUID(  4)='      CD              ; print the name o'//
     +'f the CWD'
      CALL KUGUID('CDIR',GUID,  4,'S')
      CALL KUACT('CDIR',GXRZ)
 
      CALL KUNWG(  83)
      CALL KUCMD(' ','IN','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('IN','OBJECT','Structure name','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('IN','IDVERS','Version number','IO','S')
      CALL KUPVAL('IN','IDVERS',1,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('IN','CHOPT','Option','CO','S')
      CALL KUPVAL('IN','CHOPT',0,0.,' ','D')
      GUID(  1)='Read data structure identified by OBJECT'//
     +',IDVERS into memory.'
      GUID(  2)='  MATE read JMATE structure'
      GUID(  3)='  TMED read JTMED structure'
      GUID(  4)='  VOLU read JVOLUM structure'
      GUID(  5)='  ROTM read JROTM structure'
      GUID(  6)='  SETS read JSET  structure'
      GUID(  7)='  PART read JPART structure'
      GUID(  8)='  SCAN read LSCAN structure'
      GUID(  9)='  INIT read all above data structures'
      CALL KUGUID('IN',GUID,  9,'S')
      CALL KUACT('IN',GXRZ)
 
      CALL KUNWG(  84)
      CALL KUCMD(' ','OUT','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('OUT','OBJECT','Structure name','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('OUT','IDVERS','Version number','IO','S')
      CALL KUPVAL('OUT','IDVERS',1,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('OUT','CHOPT','Option','CO','S')
      CALL KUPVAL('OUT','CHOPT',0,0.,' ','D')
      GUID(  1)='Write data structure identified by OBJEC'//
     +'T,IDVERS to RZ file.'
      GUID(  2)='  MATE write JMATE structure'
      GUID(  3)='  TMED write JTMED structure'
      GUID(  4)='  VOLU write JVOLUM structure'
      GUID(  5)='  ROTM write JROTM structure'
      GUID(  6)='  SETS write JSET  structure'
      GUID(  7)='  PART write JPART structure'
      GUID(  8)='  SCAN write LSCAN structure'
      GUID(  9)='  INIT write all above data structures'
      CALL KUGUID('OUT',GUID,  9,'S')
      CALL KUACT('OUT',GXRZ)
 
      CALL KUNWG(  28)
      CALL KUCMD(' ','LDIR','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('LDIR','CHPATH','Path name','CO','S')
      CALL KUPVAL('LDIR','CHPATH',0,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('LDIR','CHOPT','CHOPT','CO','S')
      CALL KUPVAL('LDIR','CHOPT',0,0.,' ','D')
      GUID(  1)='List the contents of a directory (memory'//
     +' or disk).'
      GUID(  2)='To list all RZ files currently open, typ'//
     +'e ''LD //''.'
      CALL KUGUID('LDIR',GUID,  2,'S')
      CALL KUACT('LDIR',GXRZ)
 
      CALL KUNWG(   7)
      CALL KUCMD(' ','PURGE','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('PURGE','NKEEP','Number of cycles to keep','IO','S')
      CALL KUPVAL('PURGE','NKEEP',1,0.,' ','D')
      GUID(  1)='Purge an RZ directory.'
      CALL KUGUID('PURGE',GUID,  1,'S')
      CALL KUACT('PURGE',GXRZ)
 
      CALL KUNWG(  42)
      CALL KUCMD(' ','SCR','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('SCR','OBJECT','Structure name','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('SCR','IDVERS','Version number','IO','S')
      CALL KUPVAL('SCR','IDVERS',1,0.,' ','D')
      GUID(  1)='Delete entry identified by OBJECT,IDVERS'//
     +' on RZ file.'
      GUID(  2)='OBJECT may be : MATE,TMED,VOLU,ROTM,SETS'//
     +',PART,SCAN, *'
      GUID(  3)='If OBJECT= *    delete all entries with '//
     +'IDVERS.'
      CALL KUGUID('SCR',GUID,  3,'S')
      CALL KUACT('SCR',GXRZ)
 
      CALL KUNWG(   7)
      CALL KUCMD(' ','LOCK','C')
      CALL KUNDPV(   2,   1,   1,   0,   1)
      CALL KUPAR('LOCK','CHDIR','Lock identifier','C','S')
      CALL KUPVAL('LOCK','CHDIR',0,0.,'RZFILE','D')
      GUID(  1)='Lock an RZ directory.'
      CALL KUGUID('LOCK',GUID,  1,'S')
      CALL KUACT('LOCK',GXRZ)
 
      CALL KUNWG(   7)
      CALL KUCMD(' ','FREE','C')
      CALL KUNDPV(   2,   1,   1,   0,   1)
      CALL KUPAR('FREE','CHDIR','Lock identifier','C','S')
      CALL KUPVAL('FREE','CHDIR',0,0.,'RZFILE','D')
      GUID(  1)='Free an RZ directory.'
      CALL KUGUID('FREE',GUID,  1,'S')
      CALL KUACT('FREE',GXRZ)
 
      CALL KUNWG(   0)
 
      CALL KUNDPV(   1,   1,   1,   0,   1)
 
      CALL KUCMD(' ',' ','E')
 
      CALL KUCMD('/',' ','SW')
 
      END
