*-- Author :    Anita Trivedi and A. K. Mohanty  30/01/95

c     ================================================================
      Subroutine PISA_ACT
c     ================================================================

c     Descriptions:
c     =============

c     Routine to handle  PISA.KUMAC  KUIP event commands

c     MAP:
c     ====

c     Called by: (KUIP)
c     Calls:     KUPATL         Fetch lowest level of KUIP command
c     KUGETI         Get integer parameter from command line
c     KUGETR         Get real parameter from command line
c     KUGETF         Get file parameter from command line
c     KUOPEN         Open a formatted file with KUIP
c     KUINQF         Inquire about a file's status
c
c     12 Nov 2008 HvH: Allow status= 'off', 'OFF', 'Off' etc. Affected RLT switching
c

      Implicit None
            
#include "guevgen.inc"
#include "guphnx.inc"
#include "gactout.inc"

      Integer*4 iinit_file(20)
      character*1 chbk_temp(80),cpar_temp(80)
      character*1 cinit_temp(80),cdinp_temp(80)
      character*1 cdout_temp(80), CDMCI_TEMP(80)
      equivalence (cinit_temp(1),cinit_file)
      equivalence (cdinp_temp(1),cdinp_file)
      equivalence (cdout_temp(1),cdout_file)
      equivalence (cdmci_temp(1),cdmci_file)
      equivalence (cinit_temp(1), iinit_file(1) )
      integer*4   idinp_file(20)
      equivalence (cdinp_temp(1), idinp_file(1) )
      integer*4   idout_file(20)
      equivalence (cdout_temp(1), idout_file(1) )
      integer*4   idmci_file(20)
      equivalence (cdmci_temp(1), idmci_file(1) )

      integer*4   i


      equivalence (chbk_temp(1),chbk_file)
      equivalence (cpar_temp(1),cpar_file)
      integer*4 ihbk_file(20)
      equivalence (chbk_temp(1), ihbk_file(1) )
      integer*4 ipar_file(20)
      equivalence (cpar_temp(1), ipar_file(1) )

      character*10 bbc1,bbc2,bbc3,bbc4,bbc5
      character*10 svx1,svx2,svx3,svx4,svx5,svx6
      character*10 ntc1,ntc2,ntc3,ntc4,ntc5
      character*10 rxn1,rxn2,rxn3,rxn4,rxn5
      character*10 hbd1,hbd2,hbd3,hbd4,hbd5
      character*10 fcl1,fcl2,fcl3,fcl4,fcl5
      character*10 aer1,aer2,aer3,aer4,aer5
      character*10 crk1,crk2,crk3,crk4,crk5
      character*80 fmci1,fout1,fpar1,geop1,mname,file
      character*10 geop2,geop3,itr1,itr2,itr3
      character*10 itr4,itr6,mum1,mum2,mum3
      character*10 mum4,mui1,mui2,mui3,mui4
      character*10 trd1,trd2,trd3,trd4
      character*10 pad1,pad2,pad3,pad4
      character*10 utg1,utg2,utg3,utg4
      character*10 rlt1,rlt2,rlt3,rlt4
      character*10 ncc1,ncc2,ncc3,ncc4
      character*10 mpc1,mpc2,mpc3,mpc4
      character*10 geop4,geop5,geop6,geop7,geop8,geop9
      character*10 ver1,ver2,tfw1,tfw2,tfw3,tfw4
      character*10 ver3,ver4,ver5,mum5,mum6,mum7,mum8,mum9
      character*10 dout1,dout2,dout3,dout4,mui5
      character*10 mui6,mui7,mui8,mui9,tfw5,tfw6,tfw7,tfw8,tfw9
      character*4 ste1,ste2,ste3,ste4
      character*10 tof1,tof2,tof3,tof4,tof5,tof6
      character*10 zdc1,zdc2,zdc3,zdc4,zdc5,zdc6
      real zdc7


      integer*4   itr5,itr7,npar
      integer*4   len1, len2, len3, len4, len, lengc
      integer*4   len5,len6,len7,len8,len9,geom1,geom2,geom3
      integer*4   geom4,geom5,geom6,auto1
      integer*4   runn1, runn2, runn3, runn4
            
      character*80 cmd,status

c     Executable code            
      do i = 1,20
        ihbk_file(i) = 0
        ipar_file(i) = 0
        iinit_file(i) = 0
        idinp_file(i) = 0
        idout_file(i) = 0
      enddo

      DO I=1,10
        IN_KEYS(I) = 0
        OUT_KEYS(I) = 0
        STE_KEYS(I) = 0
      ENDDO

C     Get the KUIP command:
C     =====================
      call kupatl(cmd, npar)
            
*     Set BBC flags
      If( cmd .eq. 'BBC') then
        call kugets(status,len1)
        call cltou(status)         ! convert to upper case
        if (status.ne.'OFF') then
          call kugets (bbc1,len1)
          call kugets (bbc2,len2)
          call kugets (bbc3,len3)
          call kugets (bbc4,len4)
          call kugets (bbc5,len5)

          cvolu_opt(1,2) = bbc1
          cvolu_opt(2,2) = bbc2
          cvolu_opt(3,2) = bbc3
          cvolu_opt(4,2) = bbc4
          cvolu_opt(5,2) = bbc5
        endif
        return

*       Set SVX flags
      else if( cmd .eq. 'SVX') then
        call kugets(status, len1)
        call cltou(status)
        if (status.ne.'OFF') then
          call kugets (svx1,len1)
          call kugets (svx2,len2)
          call kugets (svx3,len3)
          call kugets (svx4,len4)
          call kugets (svx5,len5)
          call kugets (svx6,len6)
          cvolu_opt(1,3) = SVX1
          cvolu_opt(2,3) = SVX2
          cvolu_opt(3,3) = SVX3
          cvolu_opt(4,3) = SVX4
          cvolu_opt(5,3) = SVX5
          cvolu_opt(6,3) = SVX6
        ENDIF
        return
             
*       Set RXN flags
      else if( cmd .eq. 'RXN') then
        call kugets(status, len1)
        call cltou(status)
        if (status.ne.'OFF') then
          call kugets (rxn1,len1)
          call kugets (rxn2,len2)
          call kugets (rxn3,len3)
          call kugets (rxn4,len4)
          !call kugets (rxn5,len5)
          cvolu_opt(1,18) = rxn1
          cvolu_opt(2,18) = rxn2
          cvolu_opt(3,18) = rxn3
          cvolu_opt(4,18) = rxn4
          !cvolu_opt(5,18) = rxn5
        ENDIF
        return
              
*       Set HBD flags
      else if( cmd .eq. 'HBD') then
        call kugets(status, len1)
        call cltou(status)
        if (status.ne.'OFF') then
          call kugets (hbd1,len1)
          call kugets (hbd2,len2)
          call kugets (hbd3,len3)
          call kugets (hbd4,len4)
          call kugets (hbd5,len5)
          cvolu_opt(1,15) = hbd1
          cvolu_opt(2,15) = hbd2
          cvolu_opt(3,15) = hbd3
          cvolu_opt(4,15) = hbd4
          cvolu_opt(5,15) = hbd5
        ENDIF
        return
              
*       Set FCL flags
      else if( cmd .eq. 'FCL') then
        call kugets(status, len1)
        call cltou(status)
        if (status.ne.'OFF') then
          call kugets (fcl1,len1)
          call kugets (fcl2,len2)
          call kugets (fcl3,len3)
          call kugets (fcl4,len4)
          call kugets (fcl5,len5)
          cvolu_opt(1,19) = fcl1
          cvolu_opt(2,19) = fcl2
          cvolu_opt(3,19) = fcl3
          cvolu_opt(4,19) = fcl4
          cvolu_opt(5,19) = fcl5
        ENDIF
        return

*         Set AER flags
      else if( cmd .eq. 'AER') then
        call kugets(status, len1)
        call cltou(status)
        if (status.ne.'OFF') then
          call kugets (aer1,len1)
          call kugets (aer2,len2)
          call kugets (aer3,len3)
          call kugets (aer4,len4)
          call kugets (aer5,len5)
          cvolu_opt(1,14) = aer1
          cvolu_opt(2,14) = aer2
          cvolu_opt(3,14) = aer3
          cvolu_opt(4,14) = aer4
          cvolu_opt(5,14) = aer5
        ENDIF
        return

*      Set CRK flags
      else if( cmd .eq. 'CRK') then
        call kugets(status,len1)
        call cltou(status)
        if(status.ne.'OFF')then
          call kugets (crk1,len1)
          call kugets (crk2,len2)
          call kugets (crk3,len3)
          call kugets (crk4,len4)
          call kugets (crk5,len5)
          cvolu_opt(1,5) = crk1
          cvolu_opt(2,5) = crk2
          cvolu_opt(3,5) = crk3
          cvolu_opt(4,5) = crk4
          cvolu_opt(5,5) = crk5
        endif
        return

*       Set DOUT flags
      else if( cmd .eq. 'DOUT') then
        call kugets (dout1,len1)
        call kugets (dout2,len2)
        call kugets (dout3,len3)
        call kugets (dout4,len4)
        fdigi  = dout1
        cout_keys(2) = dout2
        cout_keys(3) = dout3
        cout_keys(4) = dout4
        return

*      EMCAL
      else if( cmd .eq. 'EMC') then
        call kugets(status,len1)
        call cltou(status)
        if(status.ne.'OFF')then
          call kugets (emc1,lengc)
          call kugets (emc2,len2)
          call kugets (emc3,len3)
          call kugets (emc4,len4)
          call kugets (emc5,len5)
          call kugets (emc6,len6)
          cvolu_opt(1,8) = emc1
          cvolu_opt(2,8) = emc2
          cvolu_opt(3,8) = emc3
          cvolu_opt(4,8) = emc4
          cvolu_opt(5,8) = emc5
          cvolu_opt(6,8) = emc6
        ENDIF
        return

*       Set FMCI flags
      else if( cmd .eq. 'FMCI') then
        call kugets(fmci1,lengc)
        cdmci_file = fmci1
        return

*       Set FOUT
      else if( cmd .eq. 'FOUT') then
        call kugets(FOUT1,LEN1)
        OUT_FILE = FOUT1
        return

*       Set FPAR flags
      else if( cmd .eq. 'FPAR') then
        call kugets (FPAR1,LEN)
        PAR_FILE = FPAR1
        return


*       Set GEOP flags

      else if( cmd .eq. 'GEOP') then
        call kugets(geop1,len1)
        call kugets(geop2,len2)
        call kugets(geop3,len3)
        call kugets(geop4,len4)
        call kugets(geop5,len5)
        call kugets(geop6,len6)
        call kugets(geop7,len7)
        call kugets(geop8,len8)
        call kugets(geop9,len9)

c  c    fm: there should be provision for more optional volumes here in geop line
c  cfm  : 12/20/95, two more passive volume options added

        if(len1.gt.0)cpvol(1) = geop1
        if(len2.gt.0)cpvol(2) = geop2
        if(len3.gt.0)cpvol(3) = geop3
        if(len4.gt.0)cpvol(4) = geop4
        if(len5.gt.0)cpvol(5) = geop5
        if(len6.gt.0)cpvol(6) = geop6
        if(len7.gt.0)cpvol(7) = geop7
        if(len8.gt.0)cpvol(8) = geop8
        if(len9.gt.0)cpvol(9) = geop9
        return

*       Set ITR flags
      else if( cmd .eq. 'ITR') then
        call kugets(status,len1)
        call cltou(status)
        if(status.ne.'OFF') then
          call kugets(itr1,len1)
          call kugets(itr2,len2)
          call kugets(itr3,len3)
          call kugets(itr4,len4)
          cvolu_opt(1,4) = itr1
          cvolu_opt(2,4) = itr2
          cvolu_opt(3,4) = itr3
          cvolu_opt(4,4) = itr4
          ivolu_opt(5,4) = itr5
          cvolu_opt(6,4) = itr6
          ivolu_opt(7,4) = itr7
        endif
        return


*       Set MUM flags

      else if( cmd .eq. 'MUM') then
        call kugets(status,len1)
        call cltou(status)
        if(status.ne.'OFF') then
          call kugets(mum1,len1)
          call kugets(mum2,len2)
          call kugets(mum3,len3)
          call kugets(mum4,len4)
          call kugets(mum5,len5)
          call kugets(mum6,len6)
          call kugets(mum7,len7)
          call kugets(mum8,len8)
          call kugets(mum9,len9)
          cvolu_opt(1,10) = mum1
          cvolu_opt(2,10) = mum2
          cvolu_opt(3,10) = mum3
          cvolu_opt(4,10) = mum4
          cvolu_opt(5,10) = mum5
          cvolu_opt(6,10) = mum6
          cvolu_opt(7,10) = mum7
          cvolu_opt(8,10) = mum8
          cvolu_opt(9,10) = mum9
        endif
        return

*       Set MUI flags
      else if( cmd .eq. 'MUI')then
        call kugets(status,len1)
        call cltou(status)
        if(status.ne.'OFF')then
          call kugets(mui1,len1)
          call kugets(mui2,len2)
          call kugets(mui3,len3)
          call kugets(mui4,len4)
          call kugets(mui5,len5)
          call kugets(mui6,len6)
          call kugets(mui7,len7)
          call kugets(mui8,len8)
          call kugets(mui9,len9)
          
          cvolu_opt(1,11) = mui1
          cvolu_opt(2,11) = mui2
          cvolu_opt(3,11) = mui3
          cvolu_opt(4,11) = mui4
          cvolu_opt(5,11) = mui5
          cvolu_opt(6,11) = mui6
          cvolu_opt(7,11) = mui7
          cvolu_opt(8,11) = mui8
          cvolu_opt(9,11) = mui9
        endif
        return

*       Set TFW flags
      else if( cmd .eq. 'TFW')then
        call kugets(status,len1)
        call cltou(status)
        if(status.ne.'OFF')then
          call kugets(tfw1,len1)
          call kugets(tfw2,len2)
          call kugets(tfw3,len3)
          call kugets(tfw4,len4)
          call kugets(tfw5,len5)
          call kugets(tfw6,len6)
          call kugets(tfw7,len7)
          call kugets(tfw8,len8)
          call kugets(tfw9,len9)

          cvolu_opt(1,12) = tfw1
          cvolu_opt(2,12) = tfw2
          cvolu_opt(3,12) = tfw3
          cvolu_opt(4,12) = tfw4
          cvolu_opt(5,12) = tfw5
          cvolu_opt(6,12) = tfw6
          cvolu_opt(7,12) = tfw7
          cvolu_opt(8,12) = tfw8
          cvolu_opt(9,12) = tfw9
        endif
        return

*       Set PC2/PC3 flags
      else if( cmd .eq. 'PAD') then
        call kugets(status,len1)
        call cltou(status)
        if(status.ne.'OFF') then
          call kugets(pad1,len1)
          call kugets(pad2,len2)
          call kugets(pad3,len3)
          call kugets(pad4,len4)
          cvolu_opt(1,9) = pad1
          cvolu_opt(2,9) = pad2
          cvolu_opt(3,9) = pad3
          cvolu_opt(4,9) = pad4
        endif
        return
        
*       Set TRD flags
      else if( cmd .eq. 'TRD') then
        call kugets(status,len1)
        call cltou(status)
        if(status.ne.'OFF') then
          call kugets(trd1,len1)
          call kugets(trd2,len2)
          call kugets(trd3,len3)
          call kugets(trd4,len4)
          cvolu_opt(1,6) = trd1
          cvolu_opt(2,6) = trd2
          cvolu_opt(3,6) = trd3
          cvolu_opt(4,6) = trd4
        endif
        return

*       Set TOF flags
      else if( cmd .eq. 'TOF') then
        call kugets(status,len1)
        call cltou(status)
        if(status.ne.'OFF') then
          call kugets(tof1,len1)
          call kugets(tof2,len2)
          call kugets(tof3,len3)
          call kugets(tof4,len4)
          call kugets(tof5,len5)
          call kugets(tof6,len6)
          
          cvolu_opt(1,7) = tof1
          cvolu_opt(2,7) = tof2
          cvolu_opt(3,7) = tof3
          cvolu_opt(4,7) = tof4
          cvolu_opt(5,7) = tof5
          cvolu_opt(6,7) = tof6
        endif
        return
*       ZDC subsystem flags

      else if (cmd .eq. 'ZDC') then
        call kugets(status,len1)
        call cltou(status)
        if(status.ne.'OFF') then
          call kugets(zdc1,len1)
          call kugets(zdc2,len2)
          call kugets(zdc3,len3)
          call kugets(zdc4,len4)
          call kugets(zdc5,len5)
          call kugets(zdc6,len6)
          call kugetr(zdc7)
          
          cvolu_opt(1,13) = zdc1
          cvolu_opt(2,13) = zdc2
          cvolu_opt(3,13) = zdc3
          cvolu_opt(4,13) = zdc4
          cvolu_opt(5,13) = zdc5
          cvolu_opt(6,13) = zdc6
          rvolu_opt(7,13) = zdc7
        endif
        return
        
*       Set STEE flags
      else if( cmd .eq. 'STEE') then
        call kugets(stee1,len1)
        call kugets(stee2,len2)
        call kugets(stee3,len3)
        call kugets(stee4,len4)
        return
        
*       Set RUNN numbers
*       intput run number, output run number, project number, version number
      else if( cmd .eq. 'RUNN') then
        call kugeti(runn1)
        call kugeti(runn2)
        call kugeti(runn3)
        call kugeti(runn4)
        pisainputrunnumber = runn1
        pisaoutputrunnumber = runn2
        pisaprojectnumber = runn3
        versionnumber = runn4
        write(6,444)pisaInputRunNumber, pisaOutputRunNumber,
     +    pisaProject number, versionNumber
 444    format('pisa_act - input run number = ',i10,
     +    1x,'Output run number = ', i10,
     +    1x,'Project number = ', i10,
     +    1x,'Version number = ', i10)
        return
        
*       Set forced photon conversion values
      else if( cmd .eq. 'PHCONV') then
        call kugetr(phconvrmin)
        call kugetr(phconvrmax)
        call kugeti(phconvopt)
        write(6,445)phConvRmin, phConvRmax, phConvOpt
 445    format( 'pisa_act - Forced photon conversion parameters ',
     +    1x,'minimum R = ', f8.2, ' cm',
     +    1x,'maximum R = ', f8.2, ' cm',
     +    1x,'radial distribution option = ', i3)
        return

      else if( cmd .eq. 'SETRHIC') then
        call kugeti(rhicrun)
        call kugeti(rhicsubrun)
        write(6,446)rhicrun, rhicsubrun
 446    format('pisa_act -',
     +    ' rhicrun = ', i2, ',',
     +    ' rhicsubrun = ', i2)
        return
              
*       pisa output file
      else if (cmd .eq. 'PISAFILE') then
        call kugets(file,lengc)
        pisa_file = file
        write(*,447) pisa_file
 447    format('pisa_act - pisa output ',a )
        return
      
*       Set MuonPad flags
      else if( cmd .eq. 'MUPC') then
        call kugets(status,len1)
        call cltou(status)
        print*, 'muonpad status = ', status
        if(status.ne.'OFF') then
          call kugets(utg1,len1)
          call kugets(utg2,len2)
          call kugets(utg3,len3)
          call kugets(utg4,len4)
          cvolu_opt(1,20) = utg1
          cvolu_opt(2,20) = utg2
          cvolu_opt(3,20) = utg3
          cvolu_opt(4,20) = utg4
        endif
        return

*     Set RLT Flags
      else if( cmd .eq. 'RLT ') then
        call kugets(status,len1)
        call cltou(status)
        print*, 'rlt status = ', status
        if(status.ne.'OFF') then
          call kugets(rlt1,len1)
          call kugets(rlt2,len2)
          call kugets(rlt3,len3)
          call kugets(rlt4,len4)
          cvolu_opt(1,21) = rlt1
          cvolu_opt(2,21) = rlt2
          cvolu_opt(3,21) = rlt3
          cvolu_opt(4,21) = rlt4
        endif
        return

C     Set NCC Flags
      else if( cmd .eq. 'NCC ') then
        call kugets(status,len1)
        call cltou(status)
        print*, 'ncc status = ', status
        if(status.ne.'OFF') then
          call kugets(ncc1,len1)
          call kugets(ncc2,len2)
          call kugets(ncc3,len3)
          call kugets(ncc4,len4)
          cvolu_opt(1,22) = ncc1
          cvolu_opt(2,22) = ncc2
          cvolu_opt(3,22) = ncc3
          cvolu_opt(4,22) = ncc4
        endif
        return

C       Set MPC Flags
      else if( cmd .eq. 'MPC ') then
        call kugets(status,len1)
        call cltou(status)
        print*, 'mpc status = ', status
        if(status.ne.'OFF') then
          call kugets(mpc1,len1)
          call kugets(mpc2,len2)
          call kugets(mpc3,len3)
          call kugets(mpc4,len4)
          cvolu_opt(1,23) = mpc1
          cvolu_opt(2,23) = mpc2
          cvolu_opt(3,23) = mpc3
          cvolu_opt(4,23) = mpc4
        endif
        return

C       Set MPCEX Flags
      else if( cmd .eq. 'MPCX') then
        call kugets(status,len1)
        call cltou(status)
        print*, 'mpcex status = ', status
        if(status.ne.'OFF') then
          call kugets(mpc1,len1)
          call kugets(mpc2,len2)
          call kugets(mpc3,len3)
          call kugets(mpc4,len4)
          cvolu_opt(1,24) = mpc1
          cvolu_opt(2,24) = mpc2
          cvolu_opt(3,24) = mpc3
          cvolu_opt(4,24) = mpc4
        endif
        return

C     Set MPCEX preshower Flags
      else if( cmd .eq. 'MXPS') then
        call kugets(status,len1)
        call cltou(status)
        print*, 'mxps status = ', status
        if(status.ne.'OFF') then
          call kugets(ncc1,len1)
          call kugets(ncc2,len2)
          call kugets(ncc3,len3)
          call kugets(ncc4,len4)
          cvolu_opt(1,25) = ncc1
          cvolu_opt(2,25) = ncc2
          cvolu_opt(3,25) = ncc3
          cvolu_opt(4,25) = ncc4
        endif
        return

C     Set EXAB preshower Flags
      else if( cmd .eq. 'EXAB') then
        call kugets(status,len1)
        call cltou(status)
        print*, 'exab status = ', status
        if(status.ne.'OFF') then
          call kugets(ncc1,len1)
          call kugets(ncc2,len2)
          call kugets(ncc3,len3)
          call kugets(ncc4,len4)
          cvolu_opt(1,26) = ncc1
          cvolu_opt(2,26) = ncc2
          cvolu_opt(3,26) = ncc3
          cvolu_opt(4,26) = ncc4
        endif
        return

C     Set EXNT preshower Flags
      else if( cmd .eq. 'EXNT') then
        call kugets(status,len1)
        call cltou(status)
        print*, 'exnt status = ', status
        if(status.ne.'OFF') then
          call kugets(ncc1,len1)
          call kugets(ncc2,len2)
          call kugets(ncc3,len3)
          call kugets(ncc4,len4)
          cvolu_opt(1,28) = ncc1
          cvolu_opt(2,28) = ncc2
          cvolu_opt(3,28) = ncc3
          cvolu_opt(4,28) = ncc4
        endif
        return

*       Set MAGF flags
      else if( cmd.eq.'MNAM') then
        call kugets(mname,lengc)
        cmname_file = mname   
        return

*         Set MAGF flags
      else if( cmd.eq.'MAGF') then
        call kugets(magf1,len1)
        call kugetr(magf2)
        call kugeti(magf3)
        call kugetr(magf4)
        call kugetr(magf5)
        call kugetr(magf6)
        cpvolu_opt(1,1) = magf1
        rpvolu_opt(2,1) = magf2
        ipvolu_opt(3,1) = magf3
        rpvolu_opt(4,1) = magf4
        rpvolu_opt(5,1) = magf5
        rpvolu_opt(6,1) = magf6
        return
      endif
      Write(6,*)' PISA_ACT <E>: Control unrecognized ', CMD
      return
      End
      
