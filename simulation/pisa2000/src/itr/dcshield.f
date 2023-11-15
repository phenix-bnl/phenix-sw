      subroutine dcshield(chopt)

c *********************************************************************

c Author: Kenta Shigaki, BNL
c Date: 24/Jun/96
c Revision History:
c     05/Aug/96 KS, added Al support
c     07/Jul/97 CFM, provide for installation into HALL or HEB2 mother
c                    volume depending on whether Helium bag is installed.
c                    Previous mother volume was INTR, but now the INTR
c                    volume includes only DC/PC1

c *********************************************************************

      IMPLICIT NONE
 
#include "guphnx.inc"
#include "gconst.inc"
#include "gugeom.inc"
#include "sublink.inc"
#include "fpilink.inc"
#include "fstore.inc"

      character*4 chopt
      character*4 v_mother

c KSH1: it is PCON
c     created on 24.Jun.96 by K.Shigaki

      character*4 v_i_name, v_m_name
      integer nmed, nr, ivolu
      integer irot_xref, irot_zref, irot_zxref

      integer n_type
      parameter (n_type = 5)
      integer shld_type

      integer nksh1_poly
      parameter (nksh1_poly = 15)
      real ksh1_poly(nksh1_poly)
      real pos_ksh1(3) / 00.0, 00.0, 00.0 /

      real ksh1_poly_data (nksh1_poly, n_type)
     O   / -36.75,  96.00,   4.0, ! SHL1; PHI1, DPHI, # ZPLANES
     1     68.61,   188.50,   188.50, ! Z plane 1
     2     72.25,   188.50,   198.50, ! Z plane 2
     3     96.11,   188.50,   198.50, ! Z plane 3
     4     99.75,   198.50,   198.50, ! Z plane 4
     O     -36.75,  96.00,   4.0, ! SHL2; PHI1, DPHI, # ZPLANES
     1     70.43,   193.50,   193.50, ! Z plane 1
     2     72.25,   193.50,   198.50, ! Z plane 2
     3     97.93,   193.50,   198.50, ! Z plane 3
     4     99.75,   198.50,   198.50, ! Z plane 4
     O     -36.75,  96.00,   4.0, ! SHL3; PHI1, DPHI, # ZPLANES
     1     73.34,   188.50,   188.50, ! Z plane 1
     2     76.17,   188.50,   198.50, ! Z plane 2
     3     96.11,   188.50,   198.50, ! Z plane 3
     4     99.75,   198.50,   198.50, ! Z plane 4
     O     -36.75,  96.00,   4.0, ! SHL4; PHI1, DPHI, # ZPLANES
     1     54.05,   148.50,   148.50, ! Z plane 1
     2     57.69,   148.50,   158.50, ! Z plane 2
     3     81.55,   148.50,   158.50, ! Z plane 3
     4     85.19,   158.50,   158.50, ! Z plane 4
     O     -36.75,  96.00,   4.0, ! SHL5; PHI1, DPHI, # ZPLANES
     1     62.02,   148.50,   148.50, ! Z plane 1
     2     64.85,   148.50,   158.50, ! Z plane 2
     3     81.55,   148.50,   158.50, ! Z plane 3
     4     85.19,   158.50,   158.50/ ! Z plane 4

      integer nksha_poly
      parameter (nksha_poly = 15)
      real ksha_poly(nksha_poly)
      real pos_ksha(3) / 00.0, 00.0, 00.0 /

      real ksha_poly_data (nksha_poly, n_type)
     O   / -36.75,  96.00,   4.0, ! SHL1; PHI1, DPHI, # ZPLANES
     1     73.06,   187.00,   187.00, ! Z plane 1
     2     73.61,   187.00,   188.50, ! Z plane 2
     3     97.56,   187.00,   188.50, ! Z plane 3
     4     98.11,   188.50,   188.50, ! Z plane 4
     O     -36.75,  96.00,   4.0, ! SHL2; PHI1, DPHI, # ZPLANES
     1     74.88,   192.00,   192.00, ! Z plane 1
     2     75.43,   192.00,   193.50, ! Z plane 2
     3     99.38,   192.00,   193.50, ! Z plane 3
     4     99.93,   193.50,   193.50, ! Z plane 4
     O     -36.75,  96.00,   4.0, ! SHL3; PHI1, DPHI, # ZPLANES
     1     77.92,   187.00,   187.00, ! Z plane 1
     2     78.34,   187.00,   188.50, ! Z plane 2
     3     97.56,   187.00,   188.50, ! Z plane 3
     4     98.11,   188.50,   188.50, ! Z plane 4
     O     -36.75,  96.00,   4.0, ! SHL4; PHI1, DPHI, # ZPLANES
     1     58.50,   147.00,   147.00, ! Z plane 1
     2     59.05,   147.00,   148.50, ! Z plane 2
     3     83.00,   147.00,   148.50, ! Z plane 3
     4     83.55,   148.50,   148.50, ! Z plane 4
     O     -36.75,  96.00,   4.0, ! SHL5; PHI1, DPHI, # ZPLANES
     1     66.60,   147.00,   147.00, ! Z plane 1
     2     67.02,   147.00,   148.50, ! Z plane 2
     3     83.00,   147.00,   148.50, ! Z plane 3
     4     83.55,   148.50,   148.50/ ! Z plane 4

      integer nkshb_poly
      parameter (nkshb_poly = 15)
      real kshb_poly(nkshb_poly)
      real pos_kshb(3) / 00.0, 00.0, 00.0 /

      real kshb_poly_data (nkshb_poly, n_type)
     O   / -36.75,  96.00,   4.0, ! SHL1; PHI1, DPHI, # ZPLANES
     1     82.25,   198.50,   198.50, ! Z plane 1
     2     82.79,   198.50,   200.00, ! Z plane 2
     3    101.75,   198.50,   200.00, ! Z plane 3
     4    102.29,   200.00,   200.00, ! Z plane 4
     O     -36.75,  96.00,   4.0, ! SHL2; PHI1, DPHI, # ZPLANES
     1     82.25,   198.50,   198.50, ! Z plane 1
     2     82.79,   198.50,   200.00, ! Z plane 2
     3    101.75,   198.50,   200.00, ! Z plane 3
     4    102.29,   200.00,   200.00, ! Z plane 4
     O     -36.75,  96.00,   4.0, ! SHL3; PHI1, DPHI, # ZPLANES
     1     86.17,   198.50,   198.50, ! Z plane 1
     2     86.59,   198.50,   200.00, ! Z plane 2
     3    101.75,   198.50,   200.00, ! Z plane 3
     4    102.29,   200.00,   200.00, ! Z plane 4
     O     -36.75,  96.00,   4.0, ! SHL4; PHI1, DPHI, # ZPLANES
     1     67.69,   158.50,   158.50, ! Z plane 1
     2     68.24,   158.50,   160.00, ! Z plane 2
     3     87.19,   158.50,   160.00, ! Z plane 3
     4     87.74,   160.00,   160.00, ! Z plane 4
     O     -36.75,  96.00,   4.0, ! SHL5; PHI1, DPHI, # ZPLANES
     1     74.85,   158.50,   158.50, ! Z plane 1
     2     75.28,   158.50,   160.00, ! Z plane 2
     3     87.19,   158.50,   160.00, ! Z plane 3
     4     87.74,   160.00,   160.00/ ! Z plane 4

      integer nkshc_poly
      parameter (nkshc_poly = 15)
      real kshc_poly(nkshc_poly)
      real pos_kshc(3) / 00.0, 00.0, 00.0 /

      real kshc_poly_data (nkshc_poly, n_type)
     O   / -36.75,  96.00,   4.0, ! SHL1; PHI1, DPHI, # ZPLANES
     1     96.11,   188.50,   188.50, ! Z plane 1
     2     98.11,   188.50,   193.99, ! Z plane 2
     3     99.75,   193.01,   198.50, ! Z plane 3
     4    101.75,   198.50,   198.50, ! Z plane 4
     O     -36.75,  96.00,   4.0, ! SHL2; PHI1, DPHI, # ZPLANES
     1     97.93,   193.50,   193.50, ! Z plane 1
     2     99.93,   193.50,   198.99, ! Z plane 2
     3     99.75,   193.01,   198.50, ! Z plane 3
     4    101.75,   198.50,   198.50, ! Z plane 4
     O     -36.75,  96.00,   4.0, ! SHL3; PHI1, DPHI, # ZPLANES
     1     96.11,   188.50,   188.50, ! Z plane 1
     2     98.11,   188.50,   193.99, ! Z plane 2
     3     99.75,   193.01,   198.50, ! Z plane 3
     4    101.75,   198.50,   198.50, ! Z plane 4
     O     -36.75,  96.00,   4.0, ! SHL4; PHI1, DPHI, # ZPLANES
     1     81.55,   148.50,   148.50, ! Z plane 1
     2     83.55,   148.50,   153.99, ! Z plane 2
     3     85.19,   153.01,   158.50, ! Z plane 3
     4     87.19,   158.50,   158.50, ! Z plane 4
     O     -36.75,  96.00,   4.0, ! SHL5; PHI1, DPHI, # ZPLANES
     1     81.55,   148.50,   148.50, ! Z plane 1
     2     83.55,   148.50,   153.99, ! Z plane 2
     3     85.19,   153.01,   158.50, ! Z plane 3
     4     87.19,   158.50,   158.50/ ! Z plane 4

      integer i

c starting

      write(6,*) 'DCSHLD-I: DCSHIELD execution starting.'

c     Check if Helium Bag installed

      if(hebag.eq.0)then
         v_mother = 'HALL'
      else
         v_mother = 'HEB2'
      endif

c select SHLD type

      if (chopt.eq.'SHL1') then
        write(6,*) 'DCSHLD-I: DCSHIELD type 1 installed'
        shld_type = 1
      elseif (chopt.eq.'SHL2') then
        write(6,*) 'DCSHLD-I: DCSHIELD type 2 installed'
        shld_type = 2
      elseif (chopt.eq.'SHL3') then
        write(6,*) 'DCSHLD-I: DCSHIELD type 3 installed'
        shld_type = 3
      elseif (chopt.eq.'SHL4') then
        write(6,*) 'DCSHLD-I: DCSHIELD type 4 installed'
        shld_type = 4
      elseif (chopt.eq.'SHL5') then
        write(6,*) 'DCSHLD-I: DCSHIELD type 5 installed'
        shld_type = 5
      else
        write(6,*) 'DCSHLD-E: unknown DCSHIELD type !'
        write(6,*) 'DCSHLD-E: no DCSHIELD installed'
        return
      endif

      do i = 1, nksh1_poly
        ksh1_poly(i) = ksh1_poly_data(i,shld_type)
      enddo
      do i = 1, nksha_poly
        ksha_poly(i) = ksha_poly_data(i,shld_type)
      enddo
      do i = 1, nkshb_poly
        kshb_poly(i) = kshb_poly_data(i,shld_type)
      enddo
      do i = 1, nkshc_poly
        kshc_poly(i) = kshc_poly_data(i,shld_type)
      enddo

c define rotation matrix for reflection relative to z=0 plane.

      irot = irot + 1           ! new cfm
      call gsrotm(irot, 90., 0., 90., 90., 180., 0.) !x=x',y=y',z=-z
      irot_zref = irot          ! 05.Feb.96 KS

c define rotation matrix for reflection relative to x=0 plane.

      irot = irot + 1           ! new cfm
      call gsrotm(irot, 90., 180., 90., 90., 0., 0.) !x=-x',y=y',z=z
      irot_xref = irot          ! 05.Feb.96 KS

c define rotation matrix for reflection relative to z=0 & x=0 planes.

      irot = irot + 1
      call gsrotm(irot, 90., 180., 90., 90., 180., 0.) !x=-x',y=y',z=-z
      irot_zxref = irot          ! 05.Feb.96 KS

c cretae KSH1

      nmed = 9                  ! lead
      v_i_name = 'KSH1'
      v_m_name = v_mother
      call gsvolu(v_i_name,'PCON',nmed,ksh1_poly,nksh1_poly,ivolu)
      call gsatt(v_i_name,'SEEN',1)

c place KSH1

      nr = 1                    ! copy number
      call gspos(v_i_name, nr, v_m_name,
     &     pos_ksh1(1), pos_ksh1(2), pos_ksh1(3),
     &     irotnull, 'ONLY')

      nr = 2                    ! copy number
      call gspos(v_i_name, nr, v_m_name,
     &     pos_ksh1(1), pos_ksh1(2), -pos_ksh1(3),
     &     irot_zref, 'ONLY')

      nr = 3                    ! copy number
      call gspos(v_i_name, nr, v_m_name,
     &     -pos_ksh1(1), pos_ksh1(2), pos_ksh1(3),
     &     irot_xref, 'ONLY')

      nr = 4                    ! copy number
      call gspos(v_i_name, nr, v_m_name,
     &     -pos_ksh1(1), pos_ksh1(2), -pos_ksh1(3),
     &     irot_zxref, 'ONLY')

c cretae KSHA

      nmed = 26                 ! Al
      v_i_name = 'KSHA'
      v_m_name = v_mother
      call gsvolu(v_i_name,'PCON',nmed,ksha_poly,nksha_poly,ivolu)
      call gsatt(v_i_name,'SEEN',1)

c place KSHA

      nr = 1                    ! copy number
      call gspos(v_i_name, nr, v_m_name,
     &     pos_ksha(1), pos_ksha(2), pos_ksha(3),
     &     irotnull, 'ONLY')

      nr = 2                    ! copy number
      call gspos(v_i_name, nr, v_m_name,
     &     pos_ksha(1), pos_ksha(2), -pos_ksha(3),
     &     irot_zref, 'ONLY')

      nr = 3                    ! copy number
      call gspos(v_i_name, nr, v_m_name,
     &     -pos_ksha(1), pos_ksha(2), pos_ksha(3),
     &     irot_xref, 'ONLY')

      nr = 4                    ! copy number
      call gspos(v_i_name, nr, v_m_name,
     &     -pos_ksha(1), pos_ksha(2), -pos_ksha(3),
     &     irot_zxref, 'ONLY')

c cretae KSHB

      nmed = 26                 ! Al
      v_i_name = 'KSHB'
      v_m_name = v_mother
      call gsvolu(v_i_name,'PCON',nmed,kshb_poly,nkshb_poly,ivolu)
      call gsatt(v_i_name,'SEEN',1)

c place KSHB

      nr = 1                    ! copy number
      call gspos(v_i_name, nr, v_m_name,
     &     pos_kshb(1), pos_kshb(2), pos_kshb(3),
     &     irotnull, 'ONLY')

      nr = 2                    ! copy number
      call gspos(v_i_name, nr, v_m_name,
     &     pos_kshb(1), pos_kshb(2), -pos_kshb(3),
     &     irot_zref, 'ONLY')

      nr = 3                    ! copy number
      call gspos(v_i_name, nr, v_m_name,
     &     -pos_kshb(1), pos_kshb(2), pos_kshb(3),
     &     irot_xref, 'ONLY')

      nr = 4                    ! copy number
      call gspos(v_i_name, nr, v_m_name,
     &     -pos_kshb(1), pos_kshb(2), -pos_kshb(3),
     &     irot_zxref, 'ONLY')

c cretae KSHC

      nmed = 26                 ! Al
      v_i_name = 'KSHC'
      v_m_name = v_mother
      call gsvolu(v_i_name,'PCON',nmed,kshc_poly,nkshc_poly,ivolu)
      call gsatt(v_i_name,'SEEN',1)

c place KSHC

      nr = 1                    ! copy number
      call gspos(v_i_name, nr, v_m_name,
     &     pos_kshc(1), pos_kshc(2), pos_kshc(3),
     &     irotnull, 'ONLY')

      nr = 2                    ! copy number
      call gspos(v_i_name, nr, v_m_name,
     &     pos_kshc(1), pos_kshc(2), -pos_kshc(3),
     &     irot_zref, 'ONLY')

      nr = 3                    ! copy number
      call gspos(v_i_name, nr, v_m_name,
     &     -pos_kshc(1), pos_kshc(2), pos_kshc(3),
     &     irot_xref, 'ONLY')

      nr = 4                    ! copy number
      call gspos(v_i_name, nr, v_m_name,
     &     -pos_kshc(1), pos_kshc(2), -pos_kshc(3),
     &     irot_zxref, 'ONLY')

c wrap it up

      write(6,*) 'DCSHLD-I: DCSHIELD execution completed successfully.'

      return
      end
