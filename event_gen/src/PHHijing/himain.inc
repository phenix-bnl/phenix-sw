        integer NATT,EATT,JATT,NT,NP,N0,N01,N10,N11,KATT
        real PATT, VATT

        COMMON /HIMAIN1/ NATT,EATT,JATT,NT,NP,N0,N01,N10,N11
        COMMON /HIMAIN2/KATT(130000,4),PATT(130000,4),VATT(130000,4)

        real bimpact
	integer nbinary
        common /bpar/bimpact, nbinary
