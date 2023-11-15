      subroutine mum_user

      implicit none

#include "g77trigdef.inc"


c    *************************************************************
c    *                                                           *
c    *  MUM_USER (vsn 1.00) muon_arm analysis routine            *
c    *                                                           *
c    *  Called by ==> ::  < g_user >                             *
c    *  IN   :: none                                             *
c    *  OUT  :: none                                             *
c    *                                                           *
c    *  written  by ::  Soren P. Sorensen   11/03/93             *
c    *  modified by ::  Surender Saini, 18/04/93 12.51.59        *
c    *                  JPSullivan and MWRawool-Sullivan         *
c    *                  Jan 19, 1994 -- add keep GCKINE to get   *
c    *                  Geant track # and get event number from  *
c    *                  GCFLAG.                                  *
c    *                                                           *
c    *************************************************************

c -----------
*KEEP,GUPHNX.
#include "guphnx.inc"
*KEEP,GCFLAG.
#include "gcflag.inc"
*KEEP,FSTORE.
#include "fstore.inc"
*KEEP,SUBLINK.
#include "sublink.inc"
*KEEP,FPMLINK.
#include "fpmlink.inc"
*KEEP,FPNLINK.
#include "fpnlink.inc"
*KEEP,MUM_NTPL.
#include "mum_ntpl.inc"
*KEEP,CMCEVT.
#include "cmcevt.inc"
*KEEP,GCLIST.
#include "gclist.inc"
*KEEP,GCKINE.
#include "gckine.inc"
*KEEP,SUBEVT.
#include "subevt.inc"
*KEND.
c -----------
 
        integer         ifirst  / 0 /
        integer         ip, ip_old, it_old, itrack, lf_m, idpart, idele
        integer         mmul, i, id, nvert, nubuf,  nvbuf, j, l
        integer         my_event,my_track
        integer		lf_n,lf_w
c Hits_structure
c hits(11) :: x, y, z, dele, ptof, ipid, px, py, pz, plen, etot

c HITS_file structure  :: evt#, track#, det_volume#, pid,tof, dele,
c            x, y, z, px, py, pz
 
c TRACKS_file structure:: evt#, track#, rvertex(3), pvertex(4), pid,
c             amass, charge, parent

        integer         ipid
        real            x, y, z, de, ptof, px, py, pz, plen, etot
 
        real            pt, pp, the, phi
        real            rvertex( 3 ), pvertex( 4 ), ubuf( 10 )
        real            vbuf( 10 )
        character*20    cpart
        integer         maxtrack
        parameter     ( maxtrack = 3000 )
        integer         mt_track_no( maxtrack ), ntrack
        integer         parent( maxtrack ), nn_track
        integer         event_old  / -1 /
        real*8          sum_mmul( 0:3 ), sum_ntrack
        integer         n_his, t_his( 300 )
        integer         i_print / 1 /

        integer          n1, n2, n3, n4, n5, lt, n7, n8, n9, n10, n11
        common / gcnum / n1, n2, n3, n4, n5, lt, n7, n8, n9, n10, n11

        real*8      acceptance( -1:2 )
        common    / cmcfast / acceptance
 
        integer     iam, mynode
        integer      lss, nss, lstring
        logical     lfalse/.false./
 
      character*4 cudet
      integer NV,NHMAX,NHITS,NUMVS,NUMLEV,HITDIM
      parameter (HITDIM=11)
      parameter (NHMAX=100)
      INTEGER ITRA_FAK6(NHMAX),NUMBV(NHMAX)
      REAL HITS_FAK6(HITDIM,NHMAX)
      INTEGER MMUL2,MMULSUM
 
        save
 
        mc_track_no = itra           !taken from geant gckine keep
        mc_event_no = idevt          !taken from geant gcflag keep
 
 
 
        if ( ifirst .eq. 0 ) then
         ifirst     = 1
c kludge to make ascii hits and tracks files work
c mjl
         lprin(1) = 0

c        CFM: April 20, 1996

c        ifdef statements related to non-existent machine have been removed

c#ifdef I860
c         iam        = mynode()
c#endif
c#ifndef I860
c         iam        = 0
c#endif

c        Use the default value

         iam = 0
         sum_mmul( 0 )   = 0.0
         sum_mmul( 1 )   = 0.0
         sum_mmul( 2 )   = 0.0
         sum_mmul( 3 )   = 0.0
         sum_ntrack = 0.0
 
         if ( cvolu_opt( 5, 10 ) .eq. 'NTUP' .or.
     +        cvolu_opt( 5, 10 ) .eq. 'BOTH' ) then
 
           call hbookn( 1000, 'MUON ARM TRACKING', np1000,
     +                  'GEANHIST', 55000, ch1000 )
           call hbookn( 1010, 'MUON ORIGINAL TRACKS', np1010,
     +                  'GEANHIST', 55000, ch1010 )
         end if
 
         if ( cvolu_opt( 5, 10 ) .eq. 'FILE' .or.
     +        cvolu_opt( 5, 10 ) .eq. 'BOTH' ) then
           nss = len(chbk_file)
           lss = nss
 
             do while ( lss .ge. 1  .and.
     +                  chbk_file( lss:lss ) .ne. '.' )
                lss = lss - 1
             end do
 
           if(lprin(1) .eq. 0)then
 
c             open( unit = 90,
c    +              form = 'FORMATTED',
c    +              file = chbk_file( 1:lss-1 ) // '.hits',
c    +              status = 'UNKNOWN' )
 
c             open( unit = 91,
c    +              form = 'FORMATTED',
c    +              file = chbk_file( 1:lss-1 ) // '.tracks',
c    +              status = 'UNKNOWN' )
           else if(lprin(1) .eq. 1)then
 
c             open( unit = 90,
c    +              file = chbk_file( 1:lss-1 ) // '.hits',
c    +              status = 'UNKNOWN' )
 
c             open( unit = 91,
c    +              file = chbk_file( 1:lss-1 ) // '.tracks',
c    +              status = 'UNKNOWN' )
           end if
 
         end if
        end if         ! if(first.eq.0)
 
 
        if ( mc_event_no / i_print .ne. event_old ) then
 
cmjl       print 9000,
cmjl     x   iam, mc_event_no, acceptance( 0 ),
cmjl     x   nint( sum_ntrack ),
cmjl     x   nint( sum_mmul( 1 ) ),
cmjl     x   nint( sum_mmul( 2 ) ),
cmjl     x   nint( sum_mmul( 3 ) ),
cmjl     x   nint( sum_mmul( 0 ) )
9000      format( 2x,'hit info = ', i4, i10, f7.4, i10,
     x      3x, 4i8 )
 
          event_old = mc_event_no / i_print
 
        end if
 
 
c Call mum_check_vertex
 
        mmul    = iqf( lfm_cal(1) + 1 )
        ntrack  = 0
 
        do i = 1, maxtrack
          mt_track_no( i ) = 0
        end do
 
        do i = 1, lt
          l = i
          call gfkine( l, rvertex,
     1      pvertex, id, nvert, ubuf, nubuf )
 
          if ( nvert .eq. 1 ) then
 
            do j = 1, ntrack
              if ( mt_track_no( j ) .eq. i ) go to 80
            end do
 
            ntrack = min( ntrack + 1, maxtrack )
            mt_track_no( ntrack ) = i
 
80          continue
 
          end if
 
        end do
 

c use hits from muon ID also
        mmul2 = iqf(lfn_cal(1) + 1)
 
        mmulsum = mmul+mmul2
 
ccc     store hit information
c        write ( 6,* ) ' mmul=',mmul
 
        if ( mmulsum .gt. 0 ) then
 
          do i = 1, mmulsum
            if ( i.le.mmul ) then
c endcap muon tracking hits
              lf_m    = lfm_cal(1) + ( i - 1 ) * sfmc_cal + 3
 
              itrack  = iqf( lf_m + ofmc_track )
              ip      = iqf( lf_m + ofmc_plane )
              ipid    = iqf( lf_m + ofmc_pid   )
              ptof    =  qf( lf_m + ofmc_ptof )
              de      =  qf( lf_m + ofmc_e )
              x       =  qf( lf_m + ofmc_x )
              y       =  qf( lf_m + ofmc_y )
              z       =  qf( lf_m + ofmc_z )
              px      =  qf( lf_m + ofmc_px )
              py      =  qf( lf_m + ofmc_py )
              pz      =  qf( lf_m + ofmc_pz )

            else if( i.le.mmul+mmul2) then
c muon ID hits
              lf_n   = lfn_cal(1) + ( i-mmul-1 ) * sfnc_cal + 3
 
              itrack  = iqf( lf_n + ofnc_track )
              ip      = iqf( lf_n + ofnc_plane )
              ipid    = iqf( lf_n + ofnc_pid   )
              ptof    =  qf( lf_n + ofnc_ptof )
              de      =  qf( lf_n + ofnc_e )
              x       =  qf( lf_n + ofnc_x )
              y       =  qf( lf_n + ofnc_y )
              z       =  qf( lf_n + ofnc_z )
              px      =  qf( lf_n + ofnc_px )
              py      =  qf( lf_n + ofnc_py )
              pz      =  qf( lf_n + ofnc_pz )

            end if
 
            if ( cvolu_opt( 5, 10 ) .eq. 'NTUP' .or.
     +        cvolu_opt( 5, 10 ) .eq. 'BOTH' ) then
 
              if(ikine.eq.3)then
                my_event=ntru_evt
                my_track=ievprt-51+itrack
              else
                my_event=mc_event_no
                my_track=itrack
              endif
 
                evt1000(  1 )   =  my_event * 10**4
                evt1000(  2 )   =  mc_track_no*10**4+my_track
                evt1000(  3 )   = real( ip )
                evt1000(  4 )   = real( ipid )
                evt1000(  5 )   = ptof
                evt1000(  6 )   = de
                evt1000(  7 )   = x
                evt1000(  8 )   = y
                evt1000(  9 )   = z
                evt1000( 10 )   = px
                evt1000( 11 )   = py
                evt1000( 12 )   = pz
                call gfkine( itrack, rvertex,
     1            pvertex, id, nvert, ubuf, nubuf )
                evt1000( 13 )   = pvertex(1)
                evt1000( 14 )   = pvertex(2)
                evt1000( 15 )   = pvertex(3)
 
                call hfn( 1000, evt1000 )
 
                evt1000(  1 )   =  my_event * 10**4
                evt1000(  2 )   =  mc_track_no*10**4+my_track
                evt1000(  3 )   = real( ip )
                evt1000(  4 )   = real( ipid )
                evt1000(  5 )   = ptof
                evt1000(  6 )   = de
                evt1000(  7 )   = x
                evt1000(  8 )   = y
                evt1000(  9 )   = z
                evt1000( 10 )   = px
                evt1000( 11 )   = py
                evt1000( 12 )   = pz
 
                call hfn( 1020, evt1000 )
 
c         print *
c         print *, ' evt1000 ', i
c         print *, evt1000
 
csps                if ( mod( ip, 10 ) .eq. 1 ) then
 
            end if
 
            if ( cvolu_opt( 5, 10 ) .eq. 'FILE' .or.
     +        cvolu_opt( 5, 10 ) .eq. 'BOTH' ) then
              if(ikine.eq.3)then
                my_event=ntru_evt
                my_track=ievprt-51+itrack
              else
                my_event=mc_event_no
                my_track=itrack
              endif
 
              if(lprin(1) .eq. 0)then
                write( 90,* )
     +            my_event * 10**4 + iam,
     +            mc_track_no * 10**4 + my_track,
     +            ip,ipid,ptof,
     +            de,
     +            x, y, z,
     +            px, py, pz
              else if(lprin(1) .eq. 1)then
                write( 90,* )
     +            my_event * 10**4 + iam,
     +            mc_track_no * 10**4 + my_track,
     +            ip,ipid,ptof,
     +            de,
     +            x, y, z,
     +            px, py, pz
              end if
 
            end if
            j = ip / 100
            if ( j .ge. 1  .and.  j .le. 3 )
     x        sum_mmul( j )    = sum_mmul( j ) + 1.0d0
 
 
            if ( lfalse  ) then
 
              pt  = px*px + py*py
              pp  = sqrt( pt + pz*pz )
              pt  = sqrt( pt )
 
              if ( pt .ne. 0.0  .or. pz .ne. 0.0 ) then
                the = atan2d( pt, pz )
              else
                the = 0.0
              end if
 
              if ( px .ne. 0.0  .or. py .ne. 0.0 ) then
                phi = atan2d( py, px )
              else
                phi = 0.0
              end if
 
c             if ( ip .lt. 100 ) then
                call gfkine( itrack, rvertex,
     1            pvertex, id, nvert, ubuf, nubuf )
 
                if ( id .eq. 5 .or. id .eq. 6 ) then
                  print '( 5x, ''acc. muon '', 3i6, 6f10.3 )',
     x              ip, id, itrack, px, py, pz, pp, the, phi
                else
                  print '( 5x, ''acc.      '', 3i6, 6f10.3 )',
     x              ip, id, itrack, px, py, pz, pp, the, phi
                end if
 
c             end if
 
            end if
 
csps        end if
 
            do j = 1, ntrack
              if ( mt_track_no( j ) .eq. itrack ) go to 100
            end do
 
            ntrack = min( ntrack + 1, maxtrack )
            mt_track_no( ntrack ) = itrack
 
100         continue
 
          end do         ! do i=1,mmulsum
 
          nn_track = ntrack
 
          do i = 1, nn_track
 
            call mum_history( mt_track_no( i ), n_his, t_his )
 
            if ( n_his .gt. 0 ) then
              parent( i ) = t_his( 1 )
 
              do l = 1, n_his
 
                do j = 1, ntrack
                  if ( mt_track_no( j ) .eq. t_his( l ) )
     x              go to 82
                end do
 
                ntrack = min( ntrack + 1, maxtrack )
                mt_track_no( ntrack ) = t_his( l )
                if(l .lt. n_his)then
                  parent( ntrack ) = t_his(l+1)
                else
                  parent( ntrack ) = mt_track_no(i)
                end if
82              continue
 
              end do
 
            else
 
              parent( i ) = 0.
 
            end if
 
            j = 0
            call gfkine( mt_track_no( i ), rvertex,
     1        pvertex, id, nvert, ubuf, nubuf )
 
            if ( n_his .ge. 1 ) then
 
              do j = 1, n_his
                call gfkine( t_his( j ), rvertex,
     1            pvertex, id, nvert, ubuf, nubuf )
              end do
 
            end if
 
          end do           !do i=1,nn_track
 
 
c***    store original track info in ntuple 1010
 
          do i = 1, ntrack
 
            call gfkine( mt_track_no( i ), rvertex,
     1        pvertex, id, nvert, ubuf, nubuf )
            call gfpart( id, cpart, itrtyp, amass,
     1        charge, tlife, vbuf, nvbuf )
 
            if ( cvolu_opt( 5, 10 ) .eq. 'NTUP' .or.
     +        cvolu_opt( 5, 10 ) .eq. 'BOTH' ) then
              if(ikine.eq.3)then
                my_event=ntru_evt
                my_track=ievprt-51+mt_track_no(i)
              else
                my_event=mc_event_no
                my_track=mt_track_no(i)
              endif
              evt1010(  1 )   =  my_event * 10**4
              evt1010(  2 )   =  mc_track_no*10**4+my_track
              evt1010(  3 )   = real( id )
              evt1010(  4 )   = rvertex( 1 )
              evt1010(  5 )   = rvertex( 2 )
              evt1010(  6 )   = rvertex( 3 )
              evt1010(  7 )   = pvertex( 1 )
              evt1010(  8 )   = pvertex( 2 )
              evt1010(  9 )   = pvertex( 3 )
              evt1010(  10 )  = amass
              evt1010(  11 )  = charge
              evt1010(  12 )  = mc_track_no*10**4+parent( i )
 
              call hfn( 1010, evt1010 )
c             print *
c             print *, ' evt1010 ', i
c             print *, evt1010
 
            end if
 
            if ( cvolu_opt( 5, 10 ) .eq. 'FILE' .or.
     +        cvolu_opt( 5, 10 ) .eq. 'BOTH' ) then
              if(ikine.eq.3)then
                 my_event=ntru_evt
                 my_track=ievprt-51+mt_track_no( i )
              else
                 my_event=mc_event_no
                 my_track=mt_track_no( i )
              endif
 
              if(lprin(1) .eq. 0)then
                write( 91,* )
     x            my_event * 10**4 + iam,
     x            mc_track_no * 10**4 + my_track,
     x            rvertex,
     x            pvertex,
     x            id, amass, charge,
     x            mc_track_no * 10**4 + parent( i )
              else if(lprin(1) .eq. 1)then
                write( 91,* )
     x            my_event * 10**4 + iam,
     x            mc_track_no * 10**4 + my_track,
     x            rvertex,
     x            pvertex,
     x            id, amass, charge,
     x            mc_track_no * 10**4 + parent( i )
              end if
 
            end if
 
            sum_ntrack  = sum_ntrack + 1.0d0
 
 
          end do             !do i=1,ntrack loop for vertex tracks
 
        end if             ! if(mmulsum.gt.0)
 
       return
       end
