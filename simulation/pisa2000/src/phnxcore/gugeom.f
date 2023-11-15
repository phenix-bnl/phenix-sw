c $Id: gugeom.f,v 1.36 2018/07/05 16:50:12 lajoie Exp $
      subroutine gugeom
      implicit none

c     Purpose is to set up the active and passive volumes for PHENIX geometry

c     Author: Charles F. Maguire
c     History: Orignal source from FOPI code, then CMZ, and then CVS

c     Revisions

C  JPSullivan Oct 5, 1993 -- add GCUNIT and use LOUT for output,
C  previously LUN#6 was hard-wired into this code
c  CFM:  July 11, 1995  Put in a call to rhic_magnet_install for Paul Kirk
c                       This call is implemented only if the BGAS cpp is set
c  CFM:  Feb 29, 1996  Added call to carriage passive volume
c  CFM:  March 29, 1996 Added call to PAC (PC2/PC3) routine
c  CFM:  July 30, 1997  Put in call to Helium bag volume, set HEBAG variable
c  CFM:  May 12, 1998   Put in call to South Muon Arm shield (beam collar)
c  CFM:  Aug 18, 1999   Take out BGAS cpp variable, use LGEOM(2) instead
c  CFM:  Oct 17, 1999   Fix and comment NH loop
c  CFM:  Feb 2,  2000   Add ZDC installation
c  CFM:  Jul 6,  2001   Add NTC
c  CFM:  Jul 20, 2001   Add TZR
c  CFM:  Sep 30, 2001   Add HBD
c  CFM:  Feb 16, 2002   Add photon converter (Takashi Hachiya originator)
c  CFM:  Jan 25, 2003   Add FCL forward calorimeter
c  CFM:  Jun 5, 2003    AER aerogel (#14) is now implemented
c  W Xie:  Oct 3, 2003    MuPC for muon trigger upgrade is implemented
C  L. A. Linden Levy 20.02.2004 RPCs for RLT implemented
c  V. Dzhordzhadze 07.19.2004 Implemented NCC
c  CFM:  Mar 7, 2004    Name change of existing HBD to TPC
c  V. Dzhordzhadze 06.25.2005 Implemented MPC
c  CFM:  August 16, 2006  Change of MUW (Central ARM Muon Wall) to TFW (TOF-West)
c  CFM:  August 25, 2006  Change of TZR to RXN as subsystem #18
c  HvH 4 Apr 08: pass argument CPVOL to beam_pipe() and helium_bag()
c  HvH Jan 2010: when ZDC is installed, call HALL again to expand.
c  IR Feb 2010: HBD suppost has been added to the "PASSIVE VOLUMES"
c  Disabled the call to the RLT rpc's  - May 2013 HvH

c     Global variables

#include "gclist.inc"
#include "gugeom.inc"
#include "guphnx.inc"
#include "gcunit.inc"
#include "gactout.inc"


c     interface logical unit


      integer itf_lun
      common /interface/itf_lun


c     local declarations

      integer*4 lgeo


      integer*4 nh                ! from user card and passed
      character*4 full /'null'/   ! and passed to called routine
      integer*4 namei(11),nime, i
      character*4 name
      equivalence(namei(1),cvol(1))
      equivalence(nime,name)

      integer*4 mpc_installed; 
      integer*4 mpcex_installed; 
      integer*4 mpcex_ps_installed; 

c     initialize geometry interface
c     need to add a NULL character at the end of the string before passing it to the C function
      write( *,* ) 'gugeom - initializing phnx interface'
      call init_phnx_interface(
     +   par_file//char(0),
     +   rhicrun,
     +   rhicsubrun )



c     define materials, mixture and tracking media

      call mat_mixt_med

c     medium definitions in mumater will be placed in mat_mixt_med

      call mumater

c     whatever happens the hall gets defined (as well as the
c     logical boxes (tubes) around the target

      call hall

c     Indicate whether symmetric or asymmetric nosecones are being sued
c     ??? todo: look for that in the log; try understand (hugo pereira)
      if(pos_vert(3).eq.0.0) then
        write(  6,101)pos_vert(3)
101	    format(3x,'gugeom - Symmetric nosecone version of ',
     +  'MVD mother volume centered at Z =',f4.1,' cm')
      else
        write(6,102)pos_vert(3)
102     format(3x,'gugeom - Asymmetric nosecone version of ',
     +  'MVD mother volume centered at Z =',f4.1, ' cm')
      endif

C     CHECK ON PASSIVE VOLUMES (MAGNETs, BEAM_PIPE, NOSECONE)
C      (PB SHIELD, PISTON PLUG, NEUTRON SHIELD, HEAT TAPE, CARRIAGE)

      HEBAG = 0
      do i=1,10
        if(cpvol(i).eq.'CENT') then
          call magn_cent
        elseif(cpvol(i).eq.'ENDC') then
          call magn_endc

        ! beam pipe
        elseif( cpvol(i).eq.'PIPE'
     +    .or.cpvol(i).eq.'PIPN') then
          call beam_pipe(cpvol(i))

        elseif(cpvol(i).eq.'NOSE') then
          call nosecone
        elseif(cpvol(i).eq.'PLUG') then
          call mupplug
        elseif(cpvol(i).eq.'NTSH') then
          call muntsh
        elseif(cpvol(i).eq.'PBSH') then
          call mupbsh
        elseif(cpvol(i).eq.'BCOL') then
          call mun_collar
        elseif(cpvol(i).eq.'BHTP') then
          call beamhtr
        elseif(cpvol(i).eq.'CARR') then
          call carriage

        ! helium bag
        elseif( cpvol(i).eq.'HBAG'
     +    .or.cpvol(i).eq.'HBG1'
     +    .or.cpvol(i).eq.'HBG2'
     +    .or.cpvol(i).eq.'HBGN') then
         hebag = 1
         call helium_bag(cpvol(i))

       ! photon converter
       elseif(cpvol(i).eq.'CONV') then
         call photon_converter
       elseif(cpvol(i).eq.'PHSH') then
         call dcshieldrunx

       ! absorber
       elseif(cpvol(i).eq.'ABSO') then
         call absorber
       elseif(cpvol(i).eq.'RABS') then
         call newabsorber

        ! hbd support
       elseif(cpvol(i).eq.'HBDS') then
         call hbd_support

       elseif(ipvol(i).eq.0) then
       else
         write(6,*) 'gugeom - passive volume not recognized: ',ipvol(i)
       endif
      enddo


C     CHECK THE ACTIVE VOLUME LIST REQUESTED BY DATA CARDS

      mpc_installed = 0
      mpcex_installed = 0

      do LGEO=1,PHNX_DVOL
         IF(IVOLU_OPT(1,LGEO).GT.1) THEN
            NAME=CVOLU_OPT(2,LGEO)

C     RELIC LOOP FROM FOPI CODE (NO LONGER USEFUL IN PISA)

            do NH=1,11
               IF(NAME.EQ.NAMESH(NH)) goto 22
            enddo
            NH = 11
22          continue
            FULL=CVOLU_OPT(1,LGEO)
             print*, 'LGEO = ', LGEO
            goto(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,
     *          20,21,24,25,26,27,28,29,30),LGEO
         ENDIF
      goto 100

1     continue
      write(LOUT,*) 'gugeom - VER - MVD removed from simulations'
      goto 100

      !bbc initialization
2     continue
      call BBC(FULL,NH)
      write(LOUT,*) 'gugeom - finished BBC '
      goto 100

      !svx initialization
3     continue
      call SVX(FULL,NH)
      write(LOUT,*) 'gugeom - finished SVX '
      goto 100

4     continue
      call ITR(FULL,NH)
      write(LOUT,*) 'gugeom - finished ITR '
      goto 100

5     continue
      call CRK(FULL,NH)
      write(LOUT,*) 'gugeom - finished CRK '
      goto 100

6     continue
      call TRD(FULL,NH)
      write(LOUT,*) 'gugeom - finished TRD '
      goto 100

7     continue
      call TOF(FULL,NH)
      write(LOUT,*) 'gugeom - finished TOF '
      goto 100

8     continue
      call EMC(FULL,NH)
      write(LOUT,*) 'gugeom - finished EMC '
      goto 100

9     continue
      call PAD(FULL,NH)
      write(LOUT,*) 'gugeom - finished PAD (PC2/PC3) '
      goto 100

10    continue
      call MUM(FULL,NH)
      write(LOUT,*) 'gugeom - finished MUM '
      goto 100

11    continue
      ! new Muon ID geometry call
      call MUN(FULL,NH)
      write(LOUT,*) 'gugeom - finished MUN '
      goto 100

12    continue
      call TFW(FULL,NH)
      write(LOUT,*) 'gugeom - finished TFW '
      goto 100

13    continue
      lgeom(2) = 2        ! expand the hall to include
      call hall           ! the ZDC  Jan'10 HvH
      call ZDC(FULL,NH)
      write(LOUT,*) 'gugeom - finished ZDC '
      goto 100

14    continue
      call AER(FULL,NH)
      write(LOUT,*) 'gugeom - finished AER '
      goto 100

15    continue
      call hbd(FULL,NH)
      write(LOUT,*) 'gugeom - finished HBD '
      goto 100

16    continue
      write(LOUT,*) 'gugeom - NTC removed from simulations'
      goto 100

17    continue
      write(LOUT,*) 'gugeom - VER - MVD removed from simulations'
      goto 100

18    continue
      call RXN(FULL, NH)
      write(LOUT,*) 'gugeom - finished RXN '
      goto 100
19    continue
      call FCL(FULL, NH)
      write(LOUT,*) 'gugeom - finished FCL '
      goto 100
20    continue
      call MuonPadGeom
      write(LOUT,*) 'gugeom - finished MuonPadGeom '
      goto 100
21    continue
*      call RLTGeom
*      write(LOUT,*) 'gugeom - finished RLT '
      write (LOUT,*) 'gugeom - RLT implementation disabled ' ! June 2013 HvH
      goto 100
24    continue
c      call ncc
c      write(LOUT,*) 'gugeom - finished NCC '
       write(LOUT,*) 'gugeom - NCC removed from simulations'	
      goto 100
25    continue
      if( (mpcex_installed.eq.0) .and. (mpcex_ps_installed.eq.0)) then 
        call MPC
        write(LOUT,*) 'gugeom - finished MPC '
        mpc_installed = 1
      else
        write(LOUT,*) 'gugeom - MPCX or MXPS already installed - MPC ignored!'
      end if
      goto 100
26    continue
      if(mpc_installed.eq.0) then 
        call MPCEX
        write(LOUT,*) 'gugeom - finished MPCEX '
        mpcex_installed = 1
      else
        write(LOUT,*) 'gugeom - MPC already installed, MPCX ignored!'
      end if
      goto 100
27    continue
      if(mpc_installed.eq.0) then 
        call MPCEX_PS
        write(LOUT,*) 'gugeom - finished MPCEX_PS '
        mpcex_ps_installed = 1
      else
        write(LOUT,*) 'gugeom - MPC alread installed, MXPS ignored!'
      end if
      goto 100
28    continue
      write(LOUT,*) 'gugeom - MPC-EX absorber hits (EXAB) in place'
      goto 100
29    continue
c MPAL installed with EXAB 
      goto 100
30    continue
      write(LOUT,*) 'gugeom - MPC-EX entry hits (EXNT) in place'
      goto 100

100   continue

      enddo

      if(lgeom(2).eq.1)then

c     Insertion of RHIC magnet install

         call rhic_magnet_install
         write(LOUT,*) 'gugeom - RHIC magnets installed'
      endif

      call ggclos

c     close phnx interface
      write( *,* ) 'gugeom - closing phnx interface'
      call close_phnx_interface()

      write(LOUT,*) 'gugeom - done '
      RETURN
      end
