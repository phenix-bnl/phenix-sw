*CMZ :  2.04/00 19/05/93  22.32.03  by  Charles F. Maguire
      SUBROUTINE FE_PUT_DST

C  to write out the PISA Electromagnetic Calorimeter output zebra banks to
C  a Zebra FZ file
C  SRTonse 28-JUL-1992
C  UPDATE BY CFM 9/20/92

      IMPLICIT NONE
#include "udst.inc"
#include "fstore.inc"
#include "guphnx.inc"
#include "sublink.inc"
#include "subevt.inc"
#include "fpelink.inc"

      integer dcode /10/
      integer lk

      integer iset
      character *4 EMCSET(8) /'EC11', 'EC21', 'EC31', 'EC41',
     +                        'EC51', 'EC61', 'EC72', 'EC82'/

      integer emc_par_rows, nrows, nwalls
      parameter (emc_par_rows = 80)
      real upar(emc_par_rows)

      IF (CUDST_OTAG_TYP .EQ. 'PARA') THEN

C     at beginning of run write out parameters associated with EMC

         CALL U_PUT_DS(IXDIV_FR,LFE_PARA,'PISA','EMC ','PARA',' ')

c        No PARU bank as of now (05/18/93 GD)

cgd***         CALL U_PUT_DS(IXDIV_FR,LFE_PARU,'PISA','EMC ','PARU',' ')


c     Code for ROOT formatted PISA in PHOOL
c     The EMCal parameter passing cannot be by GFDETU
c     So we set up a EmcPISAPara class to handle the parameter data

         if(root_output.eq.1)then

c     find out how many walls are in use

            call gfdetu('EMC ', emcset(1), emc_par_rows, nrows, 
     +                  upar)
            nwalls = upar(1)
            if(nwalls.gt.0.and.nwalls.lt.9)then
               do iset = 1, nwalls
                  call gfdetu('EMC ', emcset(iset), emc_par_rows, 
     +                        nrows, upar)
                  if(nrows.ne.emc_par_rows)then
                     STOP ' FE_PUT_DST  nrows <> emc_par_rows '
                  endif
                  call parrootout(dcode, iset, upar)
               enddo  ! loop over nwalls of EMCal
            else
               write(6,111)nwalls
 111           format(/,'  FE_PUT_DST <I>: EMCal walls = ',
     +                i5,' ??',/)
            endif  ! check on nwalls value
         endif

      ENDIF ! CHECK ON CALL WITH A 'PARA' TAG
      IF (CUDST_OTAG_TYP .EQ. 'EVNT') THEN

C     event data written to DST  (WHAT HAPPENS TO PISORP IF LFE_CAL = 0 ?)

         if (lFE_Cal(1) .ne. 0)
     &   call u_put_ds(ixdiv_fe,lFE_Cal(1),'PISA','EMC ','ECAL',' ')

         lk = lfe_cal(1)  ! NOTE: EMCal has one unit offset above usual position
         if(root_output.eq.1)then
            if(nsub_evt.eq.nsubevents.or.iqf(lk+1).gt.0)then

c     NOTE: EMCal has one unit offset above usual position

               call dstrootout(dcode, 0, iqf(lk+1),
     +                         iqf(lk+3), qf(lk+3))
            endif  !  check on last subevent o rhits >0
         endif  ! check on ROOT output flag

      ENDIF ! CHECK ON CALL WITH AN 'EVNT' TAG
      RETURN
      END
