      subroutine  g_ini_zeb
      implicit none
c
c Init ZEBRA stores for GEANT interface
c
c Revision History
c    C. F. Maguire   April 1, 1996   Added call to fp_ini_zeb (PAD data)
c
c    C. F. Maguire   May  30, 1998   Added call to fx_ini_zeb (Primary data)
c
c    C. F. Maguire   July 6, 2001    Added upgrade volumes (Sili, NTC)
c
c    C. F. Maguire   July 20, 2001   Added TZR
c
c    C. F. Maguire   January 25, 2003 Added FCL
c
c    C. F. Maguire   June 5, 2003    Added AER
c    
c    C. F. Maguire   March 7, 2004   Added TPC (name change from HBD)
c                                    Added WIS version of HBD
c
c    V. Dzhordzhadze July 16 2004    Added NCC   
c    V. Dzhordzhadze June 25 2005    Added MPC   

      call f_ini_zeb
c
c Init detector subsystems
c
      call fv_ini_zeb       ! PHNX VER
      call fb_ini_zeb       ! PHNX BBC
      call fd_ini_zeb       ! PHNX INR (Silicon inner tracker upgrade)
      call fi_ini_zeb       ! PHNX ITR
      call fc_ini_zeb       ! PHNX CRK
      call ft_ini_zeb       ! PHNX TRD (Only TEC as of April, 1996)
      call fp_ini_zeb       ! PHNX PAD (PC2/PC3 as split from TEC)
      call fa_ini_zeb       ! PHNX AER (aerogel)
      call ff_ini_zeb       ! PHNX TOF
      call fe_ini_zeb       ! PHNX EMC
      call fm_ini_zeb       ! PHNX MUM (Muon Trackers)
      call fn_ini_zeb       ! PHNX MUN (Muon ID)
      call fw_ini_zeb       ! PHNX MUW (Central Muon ID, Mike Leitch)
      call fk_ini_zeb       ! PHNX KIN (particles in dectectors)
      call fx_ini_zeb       ! PHNX PRI (primary particles)
      call fg_ini_zeb       ! PHNX GEANT
c
c     Upgrade volumes starting in 2001
c
      call fq_ini_zeb       ! PHNX NTC (Normalization Trigger Counter)
      call fs_ini_zeb       ! PHNX TZR (T0 Counter)
      call fr_ini_zeb       ! PHNX TPC (combined HBD/TPC Counter, changed from fh_ini_zeb)
c
c     Upgrade volume 2003
c
      call fy_ini_zeb       ! PHNX FCL (Forward calorimeter)

      call fmupc_ini_zeb    ! PHNX muon trigger upgrade Pad chamber
c
c     Upgrade volume 2004
c
      call fh_ini_zeb       ! PHNX HBD, WIS version

      call FRLT_INI_ZEB     ! PHNX RLT detector

      call FNCC_INI_ZEB     ! PHNX NCC detector    

      call fmpc_ini_zeb     ! PHNX MPC detector    

      return
      end
