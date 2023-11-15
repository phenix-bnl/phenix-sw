*CMZ :  2.04/00 27/10/94  21.27.31  by  Charles F. Maguire
*CMZ :  2.03/00 26/07/93  14.20.09  by  S R Tonse
*-- Author :
*CMZ :          18/04/93  12.43.46  by  Surender Saini
*-- Author :    Surender Saini   18/04/93
 
      subroutine mum_digi

c    *************************************************************
c    *                                                           *
c    *  MUM_DIGI (vsn 1.00) muon_arm digitization                *
c    *                                                           *
c    *  Called by ==> ::  < gudigi >                             *
c    *  IN   :: none                                             *
c    *  OUT  :: none                                             *
c    *                                                           *
c    *  written  by ::  Surender Saini, 18/04/93 12.43.46        *
c    *  modified by ::  M. Brooks, 09/16/96, Fix octant          *
c    *   calculation for (-22.5,0) degrees                       *
c    *                  MLB, 10/10/96, Add call to trkstack      *
c    *                                                           *
c    *************************************************************

c sps     muon tracking                   August 14, 1992
 
        implicit none
c -----------------
*KEEP,GUPHNX.
#include "guphnx.inc"
*KEEP,GCFLAG.
#include "gcflag.inc"
*KEEP,SUBLINK.
#include "sublink.inc"
*KEEP,FPMLINK.
#include "fpmlink.inc"
*KEEP,FSTORE.
#include "fstore.inc"
*KEND.
c -----------------
 
      real*4 fakvol1(3),fakvol2(5),fakvol3(5),fakvol4(5),fakvol5(5)
      real*4 fakvol6(3)
      real*4 zfake1,zfake2,zfake3,zfake4,zfake5,fak3_thick,
     +         fak4_thick
      integer*4 ifak_flg(6),nmed_fake,color_fake
      common /mugeom_fakv/ifak_flg,nmed_fake,fakvol1,fakvol2,
     +   fakvol3,fakvol4,fakvol5,zfake1,zfake2,zfake3,zfake4,zfake5,
     +   fak3_thick,fak4_thick,color_fake,fakvol6
 
        integer           nhmax
        parameter       ( nhmax = 1000 )
        integer           numlev
        parameter       ( numlev = 5 )
 
        integer     i, j, ia, ioct, is, ns, ip, mmul, lf_mbase
        integer     lf_m, incnd, iudet, blen, iod, iaa, mupush
        integer     itrah( nhmax ), nubv( 2, nhmax ), nhits
        integer     nubv1( 1, nhmax)
        integer     nuvs1( 1 )
        integer     nuvs( numlev )   / numlev*0 /
        integer     itr, itr_old, itr_old2, ipl, ipl_old, ipl_old2
        integer     mum_alldets
 
c Hit_structure
c hit(11) :: x, y, z, dele, ptof, pid, px, py, pz, plen, etot

      integer     ipid
        real        xpos, ypos, zpos, dele, ptof, px, py, pz, phihit
        real        hitsh( 11, nhmax )
        real        xnor, r
 
        character*4 cudet
        character*10   chform
 
        equivalence ( cudet, iudet )
 
        logical     first / .true. /
        logical     new, old

        logical firstloop
 
        real            zfpos( 5 )
        logical         mum_fict_vol
        common        / mum_common_fict / mum_fict_vol, zfpos
 
        integer muahits/400/
        save
 
cc ================================================================
c initialize
 
        if ( first ) then
          first   = .false.
           if ( cvolu_opt( 2, 10 ) .ne. 'ETOT' ) then
             print *, ' MUM_DIGI: error #1. cvolu_opt = ',
     +       cvolu_opt( 2, 10 )
             stop
           end if
 
           if ( cvolu_opt( 4, 10 ) .ne. 'MCAL' ) then
              print *, ' MUM_DIGI: Error #2. cvolu_opt = ',
     +        cvolu_opt( 4, 10 )
              print *, '           valid options: MCAL'
           end if
 
c Book parameter banks
c Book IO characteristic for event banks
 
                chform = '2I / 3I 8F'
                call mzform( 'MCAL', chform, iod )
 
           mum_alldets  = mum_arms
     +       *mum_stations* mum_planes_max* mum_channels
     +       + 6*mum_channels
 
        end if
 
c Reset event variables
 
      mmul = 0
 
      if ( cvolu_opt( 4, 10 ) .eq. 'MCAL' ) then   ! cal data
 
        blen = sfmc_cal * mum_alldets + 1
        mupush = muahits*sfmc_cal
        call mzbook(   ixdiv_fe,
     &            lfm_cal(1),
     &            lfm_cal(1),
     &            1,
     &            'MCAL',
     &            0,
     &            0,
     &            blen,
     &            iod,
     &            0 )       ! mother bank
 
        iqf( lfm_cal(1) + 1 )   = mmul      ! preset
        iqf( lfm_cal(1) + 2 )   = mum_alldets      ! preset
 
c***  end if     ! Moved this to end of the routine
 

c Extract hit information from Zebra/Geant Hit Banks

 

c Hits in Fake Volumes in the Muon arm

      do is = 1, 6
        if( ifak_flg(is) .eq. 1) then
          ns = is
          write ( cudet, '( a3, i1 )' ) 'MUF', is
 
          call gfhits(
     &      'MUM '                  ! set identifier
     &      , cudet                 ! detector identifier
     &      , 1                     ! dim of path identification
     &      , 11                    ! dim of hit array
     &      , nhmax                 ! max number of returned hits
     &      , 0                     ! take all tracks
     &      , nuvs                  ! volume descriptor
     &      , itrah                 ! array of hit producing tracks
     &      , nubv                  ! volume descriptor numbers
     &      , hitsh                 ! hit values
     &      , nhits )               ! number of hits
 
          itr_old  = 0
          itr_old2 = 0
 
          ipl_old  = 0
          ipl_old2 = 0
 
          if ( nhits .gt. 0 ) then
 
            do i = 1, nhits
              call trkstack(itrah(i))     !put track # in PISA stack
              itr = itrah( i )
              ipl = is * 10 + 1
cmjl              print *,'fake mu: ipl = ',ipl
              new = itr .ne. itr_old      .or.
     &          ipl .ne. ipl_old
              itr_old = itr
              ipl_old = ipl
 
              if ( new ) then
                dele= hitsh(  4, i )
              else
                dele= dele + hitsh(  4, i )
              end if
 
              xpos = hitsh(  1, i )
              ypos = hitsh(  2, i )
              zpos = hitsh(  3, i )
              ptof = hitsh(  5, i )
              ipid = hitsh(  6, i )
              px   = hitsh(  7, i )
              py   = hitsh(  8, i )
              pz   = hitsh(  9, i )
 
              if ( dele .ge. rvolu_opt(7,10))  then
                old   = itr .eq. itr_old2  .and.
     &            ipl .eq. ipl_old2
 
c                if ( rvolu_opt( 6, 10 ) .ne. 0. ) then
c                  call norran( xnor )
c                  dele = dele *
c     &              ( 1.0 + rvolu_opt( 6, 10 ) * xnor )
c                end if
 
C**             if ( cvolu_opt( 4, 10 ) .eq. 'MCAL' )
C**  1            then
 
                  if ( .not. old ) mmul = mmul + 1
 
                  if ( mmul .gt. mum_alldets ) then
                    print *,
     &                '  mmul, mum_alldets = ',
     &                mmul, mum_alldets
                    mmul = mum_alldets
                  end if
 
                  if(mmul .ge. iqf(lfm_cal(1)+2))then
                    incnd = mupush
                    call mzpush( ixdiv_fe, lfm_cal(1), 0, incnd, 'I')
                    iqf( lfm_cal(1) + 2 )=iqf(lfm_cal(1)+2)+muahits
                  end if
                  lf_mbase = lfm_cal(1)
                  lf_m = lfm_cal(1) + 3 + (mmul-1)*sfmc_cal
 
                  iqf( lf_m + ofmc_plane )  = ipl
                  iqf( lf_m + ofmc_track )  = itr
                  iqf( lf_m + ofmc_pid )    = ipid
                  qf( lf_m + ofmc_ptof)     = ptof
                  qf( lf_m + ofmc_e )       = dele
                  qf( lf_m + ofmc_x )       = xpos
                  qf( lf_m + ofmc_y )       = ypos
                  qf( lf_m + ofmc_z )       = zpos
                  qf( lf_m + ofmc_px )      = px
                  qf( lf_m + ofmc_py )      = py
                  qf( lf_m + ofmc_pz )      = pz
                  if(cvolu_opt( 8, 10) .eq. 'STCK')then
                     call trkstack(itr)
                  endif  ! check for storing on the track stacker
                  iqf( lfm_cal(1) + 1 )        = mmul
 
c      if ( .not. old ) print '( 3x, a, 3i7, 2( 3x, 3f10.2 ) )',
c     &           cudet, mmul, ipl, itr, xpos, ypos, zpos,
c     &           px, py, pz
 
                  itr_old2   = itr
                  ipl_old2   = ipl
 
C**               end if         ! Check on 'MCAL'
                end if              ! Check on dele
              end do                  ! loop on NHITS /module end
            end if                      ! condition on NHITS > 0 end
 
          end if      ! Check on fake_volumes
        end do
 
c Hits in the Muon Endcap Tracking stations

 
        do ia = 1, mum_arms
          iaa = 2 * mod( ia, 2 ) - 1
 
          do is = 1, mum_stations
            ns = mum_stations * ( ia - 1 ) + is
 
            do ip = 1, PlanesPerStation( is,ia )
              firstloop = .true.
 
              write( cudet, '( a2, i1, i1 )' ) 'MT', ns, ip
 
100           continue

              call gfhits(
     &                    'MUM '                ! set identifier
     &                  , cudet                 ! detector identifier
     &                  , 1                     ! dim of path identification
     &                  , 11                    ! dim of hit array
     &                  , nhmax                 ! max number of returned hits
     &                  , 0                     ! take all tracks
     &                  , nuvs1                 ! volume descriptor
     &                  , itrah                 ! array of hit producing tracks
     &                  , nubv1                 ! volume descriptor numbers
     &                  , hitsh                 ! hit values
     &                  , nhits )               ! number of hits

C          write(6,*)cudet,nuvs1(1),nhits
 
              itr_old  = 0
              itr_old2 = 0
 
              ipl_old  = 0
              ipl_old2 = 0
 
              if ( nhits .gt. 0 ) then
 
              do i = 1, nhits
 
              if(cvolu_opt( 8, 10) .eq. 'STCK')then
                call trkstack(itrah(i))     !put track # in PISA stack
              end if

c****         if ( hitsh( 4, i ) .gt. 0.0 ) then
 
              itr = itrah( i )
 
              xpos = hitsh(  1, i )
              ypos = hitsh(  2, i )
              zpos = hitsh(  3, i )
              ptof = hitsh(  5, i )
              ipid = hitsh(  6, i )
              px   = hitsh(  7, i )
              py   = hitsh(  8, i )
              pz   = hitsh(  9, i )
 
c pack into ipl: arm(a), oct(o), station(s), and plane(p) number as "aosp"
c e.g. 1321 is 1st arm, 3rd octant, 2nd station, 1st plane

              phihit = atan2(ypos,xpos)
              if (phihit .lt. 0) phihit = phihit + 6.283185
              ioct = int((phihit + 0.3927)/0.785398) + 1
              if (ioct .eq. 9) ioct = 1

              ipl = ia * 1000 + ioct * 100 + is * 10 + ip
cmjl              print *,'endcap mutracking: ipl = ',ipl
 
              new = itr .ne. itr_old      .or.
     &          ipl .ne. ipl_old
              itr_old  = itr
              ipl_old  = ipl
 
              if ( new ) then
                dele = hitsh(  4, i )
              else
                dele = dele + hitsh(  4, i )
              end if
 
              if ( dele .ge. rvolu_opt(7,10))  then
                old = itr .eq. itr_old2  .and.
     &            ipl .eq. ipl_old2
 
c                if ( rvolu_opt( 6, 10 ) .ne. 0. ) then
c                  call norran( xnor )
c                  dele = dele *
c     &              ( 1.0 + rvolu_opt( 6, 10 ) * xnor )
c                end if
 
c**             if ( cvolu_opt( 4, 10 ) .eq. 'MCAL' )
c**  1            then
 
                  if ( .not. old ) mmul = mmul + 1
 
                  if ( mmul .gt. mum_alldets ) then
                    print *,
     &                '  mmul, mum_alldets = ',
     &                mmul, mum_alldets
                    mmul = mum_alldets
                  end if
 
                  if(mmul .ge. iqf(lfm_cal(1)+2))then
                    incnd = mupush
                    call mzpush( ixdiv_fe, lfm_cal(1), 0, incnd, 'I')
                    iqf( lfm_cal(1) + 2 )=iqf(lfm_cal(1)+2)+muahits
                  end if
                  lf_mbase = lfm_cal(1)
                  lf_m = lfm_cal(1) + (mmul-1)*sfmc_cal+3
 
                  iqf( lf_m + ofmc_plane )  = ipl
                  iqf( lf_m + ofmc_track )  = itr
                  iqf( lf_m + ofmc_pid   )  = ipid
                  qf( lf_m + ofmc_ptof)     = ptof
                  qf( lf_m + ofmc_e )       = dele
                  qf( lf_m + ofmc_x )       = xpos
                  qf( lf_m + ofmc_y )       = ypos
                  qf( lf_m + ofmc_z )       = zpos
                  qf( lf_m + ofmc_px )      = px
                  qf( lf_m + ofmc_py )      = py
                  qf( lf_m + ofmc_pz )      = pz
                  iqf( lfm_cal(1) + 1 )        = mmul
 
c      if ( .not. old ) print '( 3x, a, 3i7, 2( 3x, 3f10.2 ) )',
c     &           cudet, mmul, ipl, itr, xpos, ypos, zpos,
c     &                       px, py, pz
 
                                   itr_old2   = itr
                                   ipl_old2   = ipl
 
c**                   end if          !Check on 'MCAL'
                    end if              ! Check on dele
c***              end if                ! dele .gt. 0.0
                end do                  ! loop on NHITS /module end
              end if                      ! condition on NHITS > 0 end
              if ((is .eq. 2 .or. is .eq. 1) .and. firstloop) then
                firstloop = .false.
                write( cudet, '( a2, i1, i1 )' ) 'MU', ns, ip
                go to 100
              end if
            end do                  ! loop on planes
          end do                      ! loop on stations
        end do                          ! loop on +/- Z sides
 
c Reduce size of output bank
 
c**     if ( cvolu_opt( 4, 10 ) .eq. 'MCAL' ) then
          incnd = mmul * sfmc_cal + 3 - blen
          call mzpush( ixdiv_fe, lfm_cal(1), 0, incnd, 'I')
c**     end if
 
      end if   ! Check on 'MCAL '
 
      return
      end
