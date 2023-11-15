c
c     Common for variables read by PISA_ACT routine and needed elsewere in PISA
c
      	COMMON/GACTOUT/PAR_FILE,OUT_FILE,
     1	FDIGI,STEE1,STEE2,STEE3,STEE4
	Character*80 PAR_FILE,OUT_FILE
	Character*80 STEE1,STEE2
	Character*80 STEE3,STEE4,EMC3,EMC4,EMC5

	Character*80 EMC6  ! added by Gabor David, 8/26/99

	Character*80 fdigi,MAGF1,EMC1,EMC2
	REAL MAGF2, MAGF4, MAGF5, MAGF6
        REAL MAGF7   !  ZDC addition for DX magnetic field, 2/09/2000
	INTEGER*4 MAGF3
