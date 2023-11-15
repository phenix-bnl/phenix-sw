c     $Id: gudigi.f,v 1.24 2018/07/05 16:50:12 lajoie Exp $
c     Original PISA routine written by S. Tonse
c     Creation date: April, 1992
      subroutine gudigi
      implicit none


#include "gcbank.inc"
#include "gcflag.inc"
#include "guphnx.inc"
#include "guhitdet.inc"
#include "sublink.inc"
#include "fpxlink.inc"
#include "fstore.inc"
#include "udst.inc"
 
C     Local variable definitions
      integer i
      integer startflag /0/
c from FOPI FORTRAN portability guide:
c10. Bad:  ii = 4hPARA                   Better:  CHARACTER*4 cc
c    ====                                =======  EQUIVALENCE(ii,cc)
c                                                 cc = 'PARA'   
      character*4 cc
      equivalence(iudst_otag_typ,cc)
c     initialization
      if(iu_ini_par.eq.0) then
        iu_ini_par = 1
              
        ! check if root output is needed and open
        ! initialize output
        root_output = 0
        zebra_output = 1
        
        ! check output initialization at first call
        if(iswit(1).ge.3.and.iswit(1).le.8)then
          root_output = 1
          startflag = 0   ! default as subevent output for root clone objects
          if(iswit(1).gt.4)then
                  
            ! change to full event output for root clone objects
            startflag = 1  
                  
            ! for phool formatted objects
            if(iswit(1).eq.7.or.iswit(1).eq.8)startflag = 2
                  
          endif
                
          call rootstart(startflag, pisa_file )
          if(iswit(1).eq.3.or.iswit(1).eq.5.or.iswit(1).eq.7)then
            zebra_output = 0
          endif
                
        endif
        
        ! check if zebra output file is opened  
        if(.not.budst_oopen.and.zebra_output.eq.1)
     1   write(6,*) 'gudigi - no output file opened.'
      endif
      
      if(do_digi.eq.1) then

c       Following call removed to gukine as part of primary particle store
c       initialize the track stacker
        call trkstack(-111)            
 
c       Collect detector hits and write to "phnx.dat" ZEBRA file
        do I=1,phnx_dvol
          hits_det = 'NULL' 
          if(ivolu_opt(4,i).gt.1 .and. cvolu_opt(4,i).ne.'NONE') then
            goto(100,200,300,400,500,600,700,800,900,1000,
     1        1100,1200,1300,1400, 1500, 1600, 1700, 
     *        1800, 1900, 2000, 2100, 2400, 2500, 2600, 2700, 2800, 2800, 2900),I
            stop 'Programming error in gu_digi !!'
c----------------------------------------------------------------------------
100         continue
            write( *,* ) 'VER detector has been removed from the code'
            goto 9000
c----------------------------------------------------------------------------
200         continue
            hits_det = 'BBC'
            call bbc_digi
            goto 9000
c----------------------------------------------------------------------------
300         continue
            hits_det = 'SVX'
            call svx_digi
            goto 9000
c-----      -----------------------------------------------------------------------
400         continue
            hits_det = 'ITR'
            call itr_digi
            goto 9000
500         continue
            hits_det = 'CRK'
            call crk_digi
            goto 9000
c-----      -----------------------------------------------------------------------
600         continue
            hits_det = 'TRD'
            call trd_digi
            goto 9000
c-----      -----------------------------------------------------------------------
700         continue
            hits_det = 'TOF'
            call tof_digi
            goto 9000
c-----      -----------------------------------------------------------------------
800         continue
            hits_det = 'EMC'
            if(cvolu_opt(1,8).eq.'SPEC')then
cgd***        call emc_digi_spec
            elseif(cvolu_opt(1,8).eq.'FOLD')then
cgd***        call emc_digi
            else
              call emc_hits
            endif  ! special emc_digi routine check
            goto 9000
c----------------------------------------------------------------------------
900         continue
            hits_det = 'PAD'
            call pad_digi
            goto 9000
c-----      -----------------------------------------------------------------------
1000        continue
            hits_det = 'MUM'
            call mum_digi
            goto 9000
c-----      -----------------------------------------------------------------------
1100        continue
            hits_det = 'MUN'
            call mun_digi   ! new for the Muon ID detector subsystem
            goto 9000
c-----      -----------------------------------------------------------------------
1200        continue
            hits_det = 'TFW'
            call tfw_digi   ! new for the TOF-West central arm subsystem CFM
            goto 9000
c-----      -----------------------------------------------------------------------
1300        continue
            hits_det = 'ZDC'
            call zdc_digi
            goto 9000
c-----      -----------------------------------------------------------------------
1400        continue
            hits_det = 'AER'
            call aer_digi
            goto 9000
c-----      -----------------------------------------------------------------------
1500        continue
            hits_det = 'HBD'
            call hbd_digi
            goto 9000
c-----      -----------------------------------------------------------------------
1600        continue
            write( *,* ) 'NTC detector has been removed from the code'
            goto 9000
c-----      -----------------------------------------------------------------------
1700        continue
            write( *,* ) 'TPC detector has been removed from the code'
            goto 9000
c-----      -----------------------------------------------------------------------
1800        continue
            hits_det = 'RXN'
            call rxn_digi
            goto 9000
c-----      -----------------------------------------------------------------------
1900        continue
            hits_det = 'FCL'
            call fcl_digi
            goto 9000
c-----      -----------------------------------------------------------------------
2000        continue
            hits_det = 'MUPC'
            call MuonPad_digi
            goto 9000
c-----      -----------------------------------------------------------------------
2100        continue
            hits_det = 'RLT'
            call RLT_digi
            goto 9000
c-----      -----------------------------------------------------------------------
2400        continue
            hits_det = 'NCC'
            call NCC_digi
            goto 9000
c-----      -----------------------------------------------------------------------
2500        continue
            hits_det = 'MPC'
            call MPC_digi
            goto 9000
c-----      -----------------------------------------------------------------------
c-----      MPC-EX, digitized as MPC
2600        continue
            hits_det = 'MPC'
            call MPC_digi
            goto 9000
c-----      -----------------------------------------------------------------------
c-----      MXPS, digitized as MXPS
2700        continue
            hits_det = 'NCC'
            call MXPS_digi
            goto 9000
c----       EXAB, part of MXPS_digi
2800        continue
            goto 9000
c----       EXNT, part of MXPS_digi
2900        continue
            goto 9000
c-----      -----------------------------------------------------------------------
          endif
9000      continue
        enddo

c       end-of-subevent

        call trkstack(-999) ! close the track stacker

c       Call of PHNX analysis routines

        call g_user

c       Output of PHNX Zebra banks

        if(budst_oopen.or.root_output.eq.1) then
    
          ! initialize dst file & write PARA's
          if(iu_ini_par.eq.1) then 
            
            iu_ini_par = 2
            
            ! write parameter banks
c         put string PARA into integer zebra bank field
             cc = 'PARA'
c            iudst_otag_typ = 4hPARA 
            call e_put_dst
          endif
c         put string EVNT into integer zebra bank field
          cc = 'EVNT'
c          iudst_otag_typ = 4hEVNT ! write event banks
          call e_put_dst
        endif

      endif

9999  return
      end
