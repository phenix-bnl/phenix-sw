c $Id: gtran.f,v 1.7 2009/03/09 16:34:55 hpereira Exp $ 
      integer function gtran(id,mconv)

c convert (mconv=1) from GEANT numbering scheme to StdHep numbering scheme
c      or (mconv=2) from StdHep numbering scheme to GEANT numbering scheme

************************************************************************

*     Particles unknown to GEANT are entered as geantino's.
*     A warning message is also printed.

*     Doug Wright, LLNL   Nov 1993
*     wright20@llnl.gov

*     July 94  D.Wright, NWHINE added, GEANTINO fixed
*     Oct. 95  L.Garren, extract particle ID part of HEP2GEANT
************************************************************************

      implicit none


#include "stdhep.inc"
#include "stdlun.inc"
#include "pisa_parts.inc"

      integer id, mconv
      integer    nwhine / 0 /
      integer    nwhine_max
      parameter( nwhine_max = 10 )
      integer    IDMAX
      parameter( IDMAX = 55 )
            
C.......... too many continuation lines for some compilers
c     integer IDG2H(IDMAX) /
c    +      22,  ! gamma
c    +     -11,  ! e+
c    +      11,  ! e-
c    +      12,  ! nu (nu_e)
c    +     -13,  ! mu+
c    +      13,  ! mu-
c    +     111,  ! pi0
c    +     211,  ! pi+
c    +    -211,  ! pi-
c    +     130,  ! K0L
c    +     321,  ! K+
c    +    -321,  ! K-
c    +    2112,  ! n
c    +    2212,  ! p
c    +   -2212,  ! p~
c    +     310,  ! K0s
c    +     221,  ! eta
c    +    3122,  ! Lambda
c    +    3222,  ! Sigma+
c    +    3212,  ! Sigma0
c    +    3112,  ! Sigma-
c    +    3322,  ! Xi0
c    +    3312,  ! Xi-
c    +    3334,  ! Omega-
c    +   -2112,  ! n~
c    +   -3122,  ! Lambda~
c    +   -3112,  ! Sigma+~
c    +   -3212,  ! Sigma0~
c    +   -3222,  ! sigma-~
c    +   -3322,  ! Xi0~
c    +   -3312,  ! Xi-~
c    +   -3334,  ! Omega-~
c    +     -15,  ! tau+
c    +      15,  ! tau-
c    +     411,  ! D+
c    +    -411,  ! D-
c    +     421,  ! D0
c    +    -421,  ! D0~
c    +     431,  ! Ds+
c    +    -431,  ! Ds-
c    +    4122,  ! Lambda_c+
c    +      24,  ! W+
c    +     -24,  ! W-
c    +      23/  ! Z
c    +      45   ! deuteron
c    +  1003001002    ! tritium
c    +  1004002001    ! alpha
c    +       0   ! geantino
c    +  1003002001    ! He3
c    +       0   ! geantino
c    +       0   ! geantino
c    +       0   ! geantino
c    +       0   ! geantino
c    +       0   ! geantino
c    +      55   ! antideuteron

      integer IDG2H(IDMAX) / 22, -11, 11, 12, -13, 13, 
 
     +     111, 211, -211, 130, 321, -321, 2112, 2212, -2212, 
     +     310, 221, 3122, 3222, 3212, 3112, 3322, 3312, 3334,
     +   -2112, -3122, -3112, -3212, -3222, -3322, -3312, -3334,
     +     -15, 15, 411, -411, 421, -421, 431, -431,
     +    4122, 24, -24, 23, 45, 1003001002,
     +   1004002001, 0, 1003002001, 0, 0, 0, 0, 0, 55/

      character*20 chnpar
      real amass, charge,tlife,ub
      integer itrtyp, nwb
            
      ! do not change 'user defined' particles
      if( 
     +  id .ge. PISA_PART_MIN .and. 
     +  id .le. PISA_PART_MAX ) then
        
        gtran = id
  
      else if(mconv.eq.1)then
              
        nwhine = 0
*..............geantino
        if(id .eq. 48) then
          gtran = 0
          if( nwhine .le. nwhine_max ) then
            write(lnhout,1003) id
            nwhine = nwhine + 1
          endif
*...............normal translation
        elseif( id .le. IDMAX ) then
          gtran = IDG2H(id)          
*...............if id is 50 to 54 since gtran=0 spit out Err message
          if( (gtran .eq. 0) .and. (nwhine .le. nwhine_max) ) then
             write(lnhout,1004) id
             nwhine = nwhine + 1
          endif
*............anything else
        else
          gtran = 0
          if( nwhine .le. nwhine_max ) then
            write(lnhout,1004) id
            nwhine = nwhine + 1
          endif
        endif
              
      elseif(mconv.eq.2)then
*...........first check if its an e, mu or tau neutrino
        if( abs(id) .eq. 12 .or. abs(id) .eq. 14 .or. 
     +          abs(id) .eq. 16 ) then
           gtran = 4
        else
*..............loop over all GEANT particles, see if it matches
*..............the current HEP particle
           gtran = 1
           do WHILE ( id .ne. IDG2H(gtran) .and. gtran .le. IDMAX )
              gtran = gtran + 1
           enddo
        endif
        if( gtran .gt. IDMAX .or. gtran .eq. 48 ) then
c    test if particle id itself is known to geant, if it is
c    itrtyp (tracking routine) is non zero
           call gfpart(id, chnpar, itrtyp, amass, charge, tlife, 
     +                 ub, nwb)
           if (itrtyp .ne. 0) then
             gtran = id
           elseif(id.eq.223) then
             gtran = 1072
           elseif(id.eq.3101) then
             gtran = 1073
           else
             gtran = 48 ! geantino
             if( nwhine .le. nwhine_max ) then
               write(lnhout,1001) id
               nwhine = nwhine + 1
             endif
           endif
        endif
      else
              
        gtran = 0
        write(lnhout,1002)
              
      endif
            
      return
1001  format(5X,'GTRAN: HEP particle ',I7,
     +            ' not known to GEANT (converted to geantino)')
1002  format(5x,'GTRAN: unallowed conversion option')
1003  format(5X,'GTRAN: geantino ',I7,
     +            ' not known to HEP (set to 0)')
1004  format(5X,'GTRAN: GEANT particle ',I7,
     +            ' not known to HEP (set to 0)')
      end
