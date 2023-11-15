c $Id: bbc_digi.f,v 1.3 2008/05/21 08:21:53 hpereira Exp $

*-- Author :    Toru Sugitate   01/06/93

C=====================================================================
        subroutine bbc_digi
C=====================================================================

C    DESCRIPTION: This routine digitizes hits in the sensitive part
C                 of the Beam-Beam Counter. The BBC consists of two
C                 identical counters, positioned at BBZPOSIT (around
C                 150 cm) upstream and downstream from the vertex point,
C                 along the Z axis. Each counter is composed of identical
C                 detector elements. The nominal number of counter elements
C                 is 66 per each, but can be different by chosing different
C                 size of the detector components.

C    ARGUMENTS: (none)

C    MAP:
C           1) CALLED BY; GUDIGI
C           2) CALLS;     MZFORM,MZBOOK,BBC_HITS

C    AUTHOR: Toru Sugitate [Hiroshima University]  01 June 1993

C    REVISIONS:  Date     Name               Modification
C               ------   ------  ----------------------------------------------
C               06/05/93 Maguire Changed to "official" BBC designation
C               09/21/93 Toru    Minor change to suppress messages.
C               03/25/98 Maguire Change CHFORM to add an integer (track)
C=====================================================================
C    GLOBAL SPECIFICATIONS:
      implicit none
      
C    GUPHNX contains "variables of general interest", data-card defs, flags.
C           in particular, CVOLU_OPT is used in this subroutine.
C           Unfortunately, this also defines the local variable HIT_ALL,
C           which is not used here and therefore results in an undefined
C           variable warning message when this is compiled.
#include "guphnx.inc"
#include "fstore.inc"

C    FPBLINK contains the zebra links for the beam detector. In particular
C           it has the pointers into the common inside FSTORE where the beam
C           detector event data and geometry parameters are stored.
C           It also contains various offset parameters related the the
C           structure of the beam detector data banks.  It is also used in
C           PISORP.

#include "sublink.inc"
#include "fpblink.inc"


C    SUBEVT contains data related to the sub-event structure, for example,
C           the sub-event number and the true event number.
#include "subevt.inc"


C=====================================================================
C    EXTERNAL SPECIFICATIONS:
C     (none)

C=====================================================================
C    INTERNAL SPECIFICATIONS:

      integer icall/0/! to compile on sgi.
      save icall

      character*4 cudet
      integer*4 iudet
      equivalence  (iudet,cudet)

      integer iopval ! return from mzform
      integer blen ! maximum bank length
      integer bmul ! number of hit
      character*10  chform
      save iopval 
 
C=====================================================================
C               EXECUTABLE STATEMENTS FOLLOW BELOW
c---------------------------------------------------------------------
c Initialize and check conditions.

      if(icall.eq.0) then  
        write(*,*) 'bbc_digi - initialization'
        write(6,900) cvolu_opt(2,2),cvolu_opt(4,2)
        icall=1
 
        if(cvolu_opt(2,2).ne.'ETOT') then
          write(6,*) 'bbc_digi - inconsistent hit structure'
          stop
        endif
 
        ! check if mapped ADC/TDC data or cal ADC/TDC data are simulated
        if( cvolu_opt(4,2).ne.'BMAP' .and. cvolu_opt(4,2).ne.'BCAL' ) 
     &    then 
          write(6,*) 
     &      'bbc_digi - neither BMAP nor  BCAL selected. ',
     &      'Slow output'
          write(6,*) 'bbc_digi - valid DIGI options are: BMAP, BCAL.'
        endif
        
c---------------------------------------------------------------------
c Book IO characteristic for event banks.

C --- offsets of BMAP bank ---------

c     PARAMETER( mFB_Map   = 11)
c     PARAMETER( oFBM_PMT  =  0) ! Offset PMT #
c     PARAMETER( oFBM_X    =  1) ! Offset X position
c     PARAMETER( oFBM_Y    =  2) ! Offset Y position
c     PARAMETER( oFBM_Z    =  3) ! Offset Z Position
c     PARAMETER( oFBM_DEL  =  4) ! Offset energy loss
c     PARAMETER( oFBM_TOF  =  5) ! Offset TOF value
c     PARAMETER( oFBM_PID  =  6) ! Offset particle ID of hit
c     PARAMETER( oFBM_PX   =  7) ! Offset Px of particle
c     PARAMETER( oFBM_PY   =  8) ! Offset Py of particle
c     PARAMETER( oFBM_PZ   =  9) ! Offset Pz of particle
c     PARAMETER( oFBM_LEN  = 10) ! Offset path length of hit

        chform='1I / 1I 5F 1I 4F 1I'
        call mzform('BMAP',chform,iopval) ! book characteristic
        call mzform('BCAL',chform,iopval) ! book characteristic
 
      endif ! initialize
      
c---------------------------------------------------------------------
c Reset event valiables.

      BMUL=0
      if(cvolu_opt(4,2).eq.'BMAP') then
        
        blen=mfb_map*mfb_alldets+1
        
        ! this will be returned from mzbook
        lfb_map(1) = 0  
        call mzbook(ixdiv_fe,lfb_map(1),lfb_map(1),1,
     +    'BMAP',0,0,blen,iopval,-1)
        ! preset
        iqf(lfb_map(1)+1)=bmul         
        
      elseif(cvolu_opt(4,2).eq.'BCAL') then
        
        blen=mfb_cal*mfb_alldets+1
        
        ! this will be returned from mzbook
        lfb_cal(1) = 0  
        call mzbook(ixdiv_fe,lfb_cal(1),lfb_cal(1),1,
     +    'BCAL',0,0,blen,iopval,-1)
          
        ! preset
        iqf(lfb_cal(1)+1)=bmul    
             
      endif 

c---------------------------------------------------------------------
c Extract hit information from Zebra/Geant hit banks.

      cudet='BBCQ'
      call bbc_hits(cudet,bmul)

c---------------------------------------------------------------------
c Reduce the size of the output bank.

c     WRITE(6,*) ' <I> : Call to MZPUSH to reduce the size.'
c     IF(CVOLU_OPT(4,2).EQ.'BMAP') THEN
c       INCND=(BMUL-MFB_ALLDETS)*MFB_MAP
c       CALL MZPUSH(IXDIV_FE,LFB_MAP,0,INCND,'I')
c     ELSEIF(CVOLU_OPT(4,2).EQ.'BCAL') THEN
c       INCND=(BMUL-MFB_ALLDETS)*MFB_CAL
c       CALL MZPUSH(IXDIV_FE,LFB_CAL,0,INCND,'I')
c     ENDIF ! reduce
 
      return
900   format(
     &       ' bbc_digi - CVOLU_OPT(2,2)=',A4,/,
     &       ' bbc_digi - CVOLU_OPT(4,2)=',A4)
      end   ! bbc_digi
