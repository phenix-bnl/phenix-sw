c $Id: MuonPadGeom.f,v 1.6 2015/10/23 21:17:36 hubert Exp $
* pad chamber geometry for Muon trigger upgrade
*-- Author :    Wei Xie  09/25/2003
* May 2013 HvH - disabled call to RPC2. Moved the GSPOS calls into the _vol.f code.

       
       SUBROUTINE MuonPadGeom
 
       IMPLICIT NONE

       real           utrgPC1_north, utrgPC1_south,
     &               utrgPC2_north, utrgPC2_south, 
     *               utrgPC3_north, utrgPC3_south
      common /uPCpos/utrgPC1_north, utrgPC1_south, 
     *               utrgPC2_north, utrgPC2_south,
     *               utrgPC3_north, utrgPC3_south

       real uPC1_rin, uPC1_rout, uPC1_DZ, 
     *     uPC2N_rin, uPC2N_rout, uPC2N_DZ,
     *     uPC2S_rin, uPC2S_rout, uPC2S_DZ,
     *     uPC3N_rin, uPC3N_rout, uPC3N_DZ,
     *     uPC3S_rin, uPC3S_rout, uPC3S_DZ

      common /size/uPC1_rin, uPC1_rout, uPC1_DZ, 
     *             uPC2N_rin, uPC2N_rout, uPC2N_DZ, 
     *             uPC2S_rin, uPC2S_rout, uPC2S_DZ, 
     *             uPC3N_rin, uPC3N_rout, uPC3N_DZ, 
     *             uPC3S_rin, uPC3S_rout, uPC3S_DZ 

       namelist /mupc_coord/
     &  utrgPC1_north ,  utrgPC1_south ,
     &  utrgPC2_north ,  utrgPC2_south ,
     &  utrgPC3_north ,  utrgPC3_south ,
     &  uPC1_rin   ,  uPC1_rout  ,  uPC1_DZ   ,
     &  uPC2N_rin  ,  uPC2N_rout ,  uPC2N_DZ  ,
     &  uPC2S_rin  ,  uPC2S_rout ,  uPC2S_DZ  ,
     &  uPC3N_rin  ,  uPC3N_rout ,  uPC3N_DZ  ,
     &  uPC3S_rin  ,  uPC3S_rout ,  uPC3S_DZ  

c---------------------------------------------------------------------
c      geometry description logical unit       
       integer itf_lun
       common /interface/itf_lun

       write(6,2003)
 2003  format(/,'MuonPadGeom (=RPCs) - ',
     &             'reading parameter from phnx.par')
       rewind(itf_lun)
       read  (itf_lun, nml = mupc_coord, err=2004 )

       call MuonPadGeom_material  !.. material needed for the utrgPC
       call utrgPC1_vol           !..  small RPC, define and place
       call utrgPC3_vol           !..  large RPC, define and place

c      call utrgPC2_vol           !..  no RPC2 May 2013 HvH
c      call MuonPadGeom_pos       !..  place (moved into *_vol.f May 2013 HvH)

       call MuonPad_det           ! define active volumes
       return
c---------------------------------------------------------------------

2004   write (6,2005)
     &        utrgPC1_north, utrgpc1_south, 
     *        utrgPC2_north, utrgPC2_south, 
     *        utrgPC3_north, utrgPC3_south, 
     *        uPC1_rin,   uPC1_rout,  uPC1_DZ,
     &        uPC2N_rin,  uPC2N_rout, uPC2N_DZ, 
     *        uPC2S_rin,  uPC2S_rout, uPC2S_DZ,
     &        uPC3N_rin,  uPC3N_rout, uPC3N_DZ,
     *        uPC3S_rin,  uPC3S_rout, uPC3S_DZ

 2005  format(/,'***Possible ERROR reading RPC namelist, using ',
     &           'the following values:',/,
     &    ' utrgPC1_north = ',f8.3,' utrgPC1_south = ',f8.3,/, 
     *    ' utrgPC2_north = ',f8.3,' utrgPC2_south = ',f8.3,/, 
     *    ' utrgPC3_north = ',f8.3,' utrgPC3_south = ',f8.3,/,
     *    ' uPC1_rin =  ',f8.3,' uPC1_rout  = ',f8.3,
     &    ' uPC1_DZ  = ', f8.3,/,
     &    ' uPC2N_rin = ',f8.3,' uPC2N_rout = ',f8.3,
     &    ' uPC2N_DZ = ', f8.3,/,
     *    ' uPC2S_rin = ',f8.3,' uPC2S_rout = ',f8.3,
     &    ' uPC2S_DZ = ', f8.3,/, 
     &    ' uPC3N_rin = ',f8.3,' uPC3N_rout = ',f8.3,
     &    ' uPC3N_DZ = ', f8.3,/,
     *    ' uPC3S_rin = ',f8.3,' uPC3S_rout = ',f8.3,
     &    ' uPC3S_DZ = ', f8.3,/,
     &    '*** END RPC namelist possible ERROR ****************' )

c        2         3         4         5         6         7         7
c23456789*123456789*123456789*123456789*123456789*123456789*123456789*12
c       stop 'test stop'

       return 
       end
