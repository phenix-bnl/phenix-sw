*CMZ :  2.04/00 05/10/92  11.19.39  by  Charles F. Maguire
*-- Author :
*-- Author :
      FUNCTION JSET_TO_GEANT(JSET)
C
c Translates JSET (JETJET particle code) to ID (GEANT code) Li Qun
c 10-Apr-1996
c
      implicit none
c
      integer JSET,id,is,i, JSET_TO_GEANT
C
c      CHARACTER CHAP*16    
c      REAL PM,PCHRG
c      EXTERNAL LUNAME,ULMASS,LUCHGE
c      REAL ULMASS
c      INTEGER LUCHGE
c
      INTEGER JSETCOD(44)
c                      1     2     3     4     5     
      DATA JSETCOD/   22,  -11,   11,   12,  -13,
     *                13,  111,  211, -211,  130,
     *               321, -321, 2112, 2212,-2212,
     *               310,  221, 3122, 3222, 3212,
     *              3112, 3322, 3312, 3334,-2112,
     *             -3122,-3112,-3212,-3222,-3322,
     *             -3312,-3334,  -15,   15,  411,
c for 39,40, id(39,40)=4 temporatory. 
     *              -411,  421, -421,    4,    4,
     *              4122,   24,  -24,   23/

C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
        IS=IABS(JSET)

c
c     Additions for VNI generator, June 23 1999 (C.F. Maguire)
c
        if(is.eq.311.or.is.eq.313)then
           id = 10   ! K0 
           go to 5
        endif

        IF(IS.EQ.12.or.IS.EQ.14.or.IS.EQ.16)JSET = 11    !mu/tau neutrino

        DO I=1,44
          IF(JSET.EQ.JSETCOD(I))THEN
            ID = I
            GO TO 5       !jump out of loop
          END IF
        END DO

c        call LUNAME(jset,chap)
c        PM=ULMASS(JSET)
c        PCHRG=LUCHGE(JSET)
        WRITE(6,*)' Particle type not found. JSET = ',JSET,
c     &   'name is ',chap,'mass is ', PM, 'Charge is ',pchrg,
     &       ' Setting to e neutrino'
          ID = 4

5       CONTINUE

      JSET_TO_GEANT = ID

      RETURN
      END
