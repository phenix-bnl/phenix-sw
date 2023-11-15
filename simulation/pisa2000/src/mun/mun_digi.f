*CMZ :  2.04/00 27/10/94  21.23.19  by  Charles F. Maguire
*CMZ :  2.03/00 22/06/93  14.32.04  by  Surender Saini
*CMZ :          18/04/93  13.08.26  by  Surender Saini
*-- Author :    Surender Saini   18/04/93

      subroutine mun_digi
      implicit none

c    *************************************************************
c    *                                                           *
c    *  MUN_DIGI (vsn 1.00) muon_arm ID digitization             *
c    *                                                           *
c    *  Called by ==> :: < gudigi >                              *
c    *  IN   ::  none                                            *
c    *  OUT  ::  none                                            *
c    *                                                           *
c    *  written  by ::  Surender Saini, 18/04/93 13.08.26        *
c    *  modified by ::  Charles F. Maguire 10/26/94              *
c    *                                                           *
c    *************************************************************

c    C.H. Pinkenburg Feb. 09, 2001  check on # of hits returned by gfhits
c                                   indicating a too small user hit buffer
c                                   (and set nhits to size of user buffer,
c                                   gfhits: GEANT manual page156)


c -----------------
*KEEP,GUPHNX.
#include "guphnx.inc"
*KEEP,GCFLAG.
#include "gcflag.inc"
*KEEP,SUBLINK.
#include "sublink.inc"
*KEEP,FPNLINK.
#include "fpnlink.inc"
*KEEP,FSTORE.
#include "fstore.inc"
*KEND.
c -----------------


c    local variables

        integer nnhits    !  total number of hits in all MUN
        integer nhmax
        parameter ( nhmax = 1000 )
        integer numlev
        parameter ( numlev = 5 )

        real theta
        integer i, j, ia, ioct, is, ns, ip, idpart, mmul, lf_nbase
        integer lf_n, incnd, iudet, blen, iod, iaa, mupush
        integer itrah( nhmax ), nubv( 2, nhmax ), nhits
        integer nubv3( 3, nhmax), nubv1( 1, nhmax)
        integer nuvs3( 3 ), nuvs1( 1 )
        integer nuvs( numlev ) / numlev*0 /
        integer itr, itr_old, itr_old2, ipl, ipl_old, ipl_old2
        integer lss, nss, lstring
        integer muid_alldets
        integer nr_muid_planes
        common /muid_hitcom/ muid_alldets

c Hit_structure
c hit(11) :: x, y, z, dele, ptof, pid, px, py, pz, plen, etot

        integer ipid
        real xpos, ypos, zpos, dele, ptof, px, py, pz, pl, etot
        real hitsh( 11, nhmax )
        real pp, xnor, r, phi, z_boundary, rpos

        character*4 cudet
        character*10   chform

        equivalence ( cudet, iudet )

        logical first / .true. /
        logical new, old


        real zfpos( 5 )
        logical mum_fict_vol
        common / mum_common_fict / mum_fict_vol, zfpos

        integer muahits/400/
        save

cc ================================================================


c     begin execution

c      write(6,*)'  Call to MUN_DIGI'

c initialize

        if ( first ) then
          first   = .false.
           if ( cvolu_opt( 2, 11 ) .ne. 'ETOT' ) then
             print *, ' MUN_DIGI: error #1. cvolu_opt = ',
     +       cvolu_opt( 2, 11 )
             stop
           end if

           if ( cvolu_opt( 4, 11 ) .ne. 'NCAL' ) then
              print *, ' MUN_DIGI: Error #2. cvolu_opt = ',
     +        cvolu_opt( 4, 11 )
              print *, '           valid options: WCAL'
           end if

c Book parameter banks
c Book IO characteristic for event banks

                chform = '2I / 3I 8F'
                call mzform( 'NCAL', chform, iod )
        end if  ! initialization check


c Hits in Endcap Muon ID

c reset event variables
        mmul = 0

c Hits in the Central Muon ID


        nnhits = muid_alldets

c    Book the ZEBRA array with total size based on all hits

        blen = sfnc_cal * nnhits + 1  !  length is at least 1
        call mzbook(   ixdiv_fe,
     &            lfn_cal(1),
     &            lfn_cal(1),
     &            1,
     &            'NCAL',
     &            0,
     &            0,
     &            blen,
     &            iod,
     &            0 )       ! mother bank

        iqf( lfn_cal(1) + 1 )   = mmul      ! preset
        iqf( lfn_cal(1) + 2 )   = nnhits      ! preset

        write( cudet, '( a4)' ) 'MUGS'

        call gfhits(
     &                    'MUN '                ! set identifier
     &                  , cudet                 ! detector identifier
     &                  , 2                     ! dim of path identification
     &                  , 11                    ! dim of hit array
     &                  , nhmax                 ! max number of returned hits
     &                  , 0                     ! take all tracks
     &                  , nuvs                  ! volume descriptor
     &                  , itrah                 ! array of hit producing tracks
     &                  , nubv                  ! volume descriptor numbers
     &                  , hitsh                 ! hit values
     &                  , nhits )               ! number of hits

C /chp/ if user array hitsh exceeded, nhits is returned as nhmax+1
        if (nhits .gt. nhmax) then
          write(6,*) '<W> MUN (mun_digi.f): number of hits exceeds',
     #    nhmax,' nhits truncated to ',nhmax,' for ',cudet
          nhits = nhmax
        end if

        itr_old  = 0
        itr_old2 = 0

        ipl_old  = 0
        ipl_old2 = 0

        if ( nhits .gt. 0 ) then

        do i = 1, nhits

c****   if ( hitsh( 4, i ) .gt. 0.0 ) then

          itr = itrah( i )

          xpos = hitsh(  1, i )
          ypos = hitsh(  2, i )
          zpos = hitsh(  3, i )
          ptof = hitsh(  5, i )
          ipid = hitsh(  6, i )
          px   = hitsh(  7, i )
          py   = hitsh(  8, i )
          pz   = hitsh(  9, i )

c pack into ipl: arm(a) and plane(p) number as "a00p"
c e.g. 1001 is 1st arm, 1st plane
c this in distinguished from muon tracking hits by the 0 in the third digit
c assume 1st plane only for now
c get number of muon-id planes from geometry bank
          nr_muid_planes = qf(lfn_para + 2)
c get plane number from volume number
          ia = 1
          ip = nubv(1,i)
c adjust plane number if 2nd arm
          if( ip.gt.nr_muid_planes ) then
            ia = 2
            ip = ip - nr_muid_planes
          endif
          ipl = 1000*ia + ip
c          ipl = 1000*nubv(1,i) + nubv(2,i)
cmjl          print *,'endcap muid: ipl = ',ipl

          new      = itr .ne. itr_old      .or.
     &      ipl .ne. ipl_old
            itr_old  = itr
            ipl_old  = ipl

            if ( new ) then
              dele = hitsh(  4, i )
            else
              dele = dele + hitsh(  4, i )
            end if

            if ( dele .ge. rvolu_opt(7,11))  then
              old   = itr .eq. itr_old2  .and.
     &          ipl .eq. ipl_old2

c              if ( rvolu_opt( 6, 11 ) .ne. 0. ) then
c                call norran( xnor )
c                dele = dele *
c     &            ( 1.0 + rvolu_opt( 6, 11 ) * xnor )
c              end if

c**           if ( cvolu_opt( 4, 11 ) .eq. 'NCAL' )
c**  1          then

                if ( .not. old ) mmul = mmul + 1

                if ( mmul .gt. nnhits ) then
                  print *,
     &              '  mmul, nnhits = ',
     &              mmul, nnhits
                  mmul = nnhits
                end if

                if(mmul .ge. iqf(lfn_cal(1)+2))then
                  incnd = mupush
                  call mzpush( ixdiv_fe, lfn_cal(1), 0, incnd, 'I')
                  iqf( lfn_cal(1) + 2 )=iqf(lfn_cal(1)+2)+muahits
                end if

                lf_nbase = lfn_cal(1)
                lf_n = lfn_cal(1) + (mmul-1)*sfnc_cal+3

                iqf( lf_n + ofnc_plane )  = ipl
                iqf( lf_n + ofnc_track )  = itr
                iqf( lf_n + ofnc_pid   )  = ipid
                qf( lf_n + ofnc_ptof)     = ptof
                qf( lf_n + ofnc_e )       = dele
                qf( lf_n + ofnc_x )       = xpos
                qf( lf_n + ofnc_y )       = ypos
                qf( lf_n + ofnc_z )       = zpos
                qf( lf_n + ofnc_px )      = px
                qf( lf_n + ofnc_py )      = py
                qf( lf_n + ofnc_pz )      = pz
                if(cvolu_opt( 8, 11) .eq. 'STCK')then
                   call trkstack(itr)
                endif  ! check for storing on the track stacker
                iqf( lfn_cal(1) + 1 )        = mmul

c      if ( .not. old ) print '( 3x, a, 3i7, 2( 3x, 3f10.2 ) )',
c     &           cudet, mmul, ipl, itr, xpos, ypos, zpos,
c     &                       px, py, pz

                itr_old2   = itr
                ipl_old2   = ipl

c***          end if          ! Check on 'NCAL'
            end if              ! Check on dele
c***      end if                 ! dele .gt. 0.0
        end do                  ! loop on NHITS /module end
      end if                      ! condition on NHITS > 0 end

c Reduce size of output bank  (dropped by CFM for now; could re-insert)

c**     if ( cvolu_opt( 4, 11 ) .eq. 'NCAL' ) then
          incnd = mmul * sfnc_cal + 3 - blen
          call mzpush( ixdiv_fe, lfn_cal(1), 0, incnd, 'I')
c**     end if

      return
      end
