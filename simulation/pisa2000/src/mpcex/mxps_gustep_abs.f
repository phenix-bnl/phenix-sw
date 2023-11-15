      SUBROUTINE MXPS_GUSTEP_ABS
      implicit none

#include "gckine.inc"
#include "gctrak.inc"
#include "gcsets.inc"
#include "gctmed.inc"
#include "gcvolu.inc"
#include "gcflag.inc"
#include "geant321/gcking.inc"
#include "mpcex_ps_data.inc"
#include "guphnx.inc"

      real hit_arr(6) /6*0.0/
      real ahit_arr(3) /3*0.0/
      real fncc, fdist, hitenergy
      integer ncc, ncchit
      integer ihit

      integer i
      Character*4 NAMES_TMP(15)
      Equivalence (NAMES_TMP,NAMES)

* Flag particles *entering* the first absorber volume

      IF( (idtype.eq.20225).and.(isvol.gt.0).and.(iset.gt.0).and.(idet.gt.0).and.(inwvol.eq.1) ) THEN

* Check for tungsten layer

        if( (NLEVEL.eq.6).and.(NAMES_TMP(NLEVEL).eq.'W_DS')) then  

* FIRST tungsten layer:

        if(NUMBER(NLEVEL-1).eq.1) THEN

* ARM 1=positive z, 2=negative z
* only store particle entering volume, traveling 
* in the correct direction, and within 100um of the 
* front surface with E>0.5GeV

            if(vect(3).gt.0.0) then 
              ncc = 1
              fncc = 1.0 
            else
              ncc = 2
              fncc = -1.0
            endif

            fdist = ((fncc*VECT(3))-203.655); 

            if( ((fncc*vect(6)).gt.0.0).and.(DESTEP.eq.0.0)
     +         .and.(fdist>0.0).and.(fdist<0.01).and.(VECT(7).gt.0.5) ) then

c               print *, 'ncc = ',ncc,' vx =',VECT(1),' vy =',VECT(2),
c     +               ' vz =',VECT(3),' id = ',IPART,
c     +               'itra = ',ITRA,' ptot = ',VECT(7),' pz = ',VECT(6)*VECT(7)

               numbv(1) = ncc 
               numbv(2) = 1

               hit_arr(1) = VECT(1) ! X position
               hit_arr(2) = VECT(2) ! Y position
               hit_arr(3) = VECT(3) ! Z position
               hit_arr(4) = VECT(4)*VECT(7) ! px
               hit_arr(5) = VECT(5)*VECT(7) ! py
               hit_arr(6) = VECT(6)*VECT(7) ! pz

               call gsahit(iset,idet,ITRA,NUMBV,hit_arr,ihit)   
               IF(IHIT.EQ.0)THEN
                 print *,"Failure calling GSAHIT (mxps_gustep_abs, idtype=20225)"
               ENDIF

            end if

        end if

        end if
        
      ENDIF


      IF( ((idtype.eq.20223).or.(idtype.eq.20224)).and.(isvol.gt.0).and.(iset.gt.0).and.(idet.gt.0) ) THEN

         IF(DESTEP.GT.0.) THEN

            hitenergy = destep*1.e+6 ! GeV to KeV

* 20224 = MPC Al skin, etc. 

            if(idtype.eq.20224) THEN

* ARM 1=positive z, 2=negative z

              if(vect(3).gt.0.0) then 
                ncc = 1
              else
                ncc = 2
              endif
              
c inner plates NCC+10; back plate NCC+20
c outer skin NCC+30
c explicitly STOP particles in the outer
c skin or back plate
              
              ncchit = ncc; 
              if(NAMES_TMP(NLEVEL).eq.'SKIN') THEN
                 ncchit = ncchit + 10
              end if
              if(NAMES_TMP(NLEVEL).eq.'BPLT') THEN
                 ncchit = ncchit + 20
                 hitenergy = GEKIN*1.e+6 
                 ISTOP = 1
                 
*                 print *,"BPLT stopping = ",hitenergy

              endif
              if(NAMES_TMP(NLEVEL).eq.'PLAT') THEN
                 ncchit = ncchit + 30
                 hitenergy = GEKIN*1.e+6 
                 ISTOP = 1

*                 print *,"PLAT stopping = ",hitenergy

              endif

            ELSE
* 20223 = MPC-EX absorber material 
              ncc  = number(3)
              ncchit = ncc
            ENDIF

            if(iset.eq.0.or.idet.eq.0) then
               print *, 'Problem ',ihset,' ',ihdet,' ',iset,' ',idet,
     +                  ' ',idtype,' ',nvname
            endif
    
*     Energy. deposited in MPC-EX/MPC absorber

* Just sum every type in the same arm 
            numbv(1) = ncc
            numbv(2) = 1

            ahit_arr(1) = IEVENT
            ahit_arr(2) = ncchit
            ahit_arr(3) = hitenergy

c            print *,'mxps_gustep_abs - gschit called!, idtype = ',idtype
	
	    call gschit(iset,idet,ITRA,NUMBV,ahit_arr,1,ihit)
            IF(IHIT.EQ.0)THEN
              print *,"Failure calling GSCHIT (mxps_gustep_abs, idtype=20223/4)"
            ENDIF

         ENDIF

      ENDIF
 10   CONTINUE


 9000 format(5a10/5i10/2i10/3f10.1,5(e10.1)/8i6/4i5,2A5)
 9001 format(10i10)

      end
