*CMZ :  2.04/00 05/10/92  11.19.33  by  Charles F. Maguire
*-- Author :
*-- Author :
      SUBROUTINE E_KDEF_USER
      INTEGER MGUIDL
      PARAMETER (MGUIDL=199)
      CHARACTER*80 GUID
      COMMON /KCGUID/ GUID(MGUIDL)
      EXTERNAL E_USER_ACT
      CALL KUNWG(   6)
      CALL KUCMD(' ','USER','C')
      GUID(  1)='User''s private Menu'
      CALL KUGUID('USER',GUID,  1,'S')
      CALL KUCMD('USER',' ','SW')
      CALL KUNWG(   7)
      CALL KUCMD(' ','USERCOMMAND','C')
      GUID(  1)='User''s private Command'
      CALL KUGUID('USERCOMMAND',GUID,  1,'S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('USERCOMMAND','PRIVATE','Private Stuff','C','S')
      CALL KUACT('USERCOMMAND',E_USER_ACT)
      CALL KUNWG(   0)
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUCMD(' ',' ','E')
      CALL KUCMD('/',' ','SW')
      END
