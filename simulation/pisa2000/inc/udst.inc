c
c   Original authors: S.R. Tonse and C.F. Maguire (FOPI relic)
c
C
C   User DST I/O control common
c
c
c   Revision history (since 1996)
c	Hubert van Hecke 2/7/1996  Increase to 40 number of header parameters
c
c       Andrew Rose   5/31/98      Increase header array from 40 to 100
c
c=====begin udst==============================================================
c
c    Global variables
c
      common       /udst/
     1   iudst_pr_nuh,iudst_pr_uhead,
     1   budst_end_evt,budst_new_evt,
     1   iudst_otag_typ,iudst_ologl,
     1   budst_oopen,
     #       iudst_iOpen, iudst_oOpen
c
c    Global specifications
c
      integer           iudst_pr_nuh               ! nuh for pilot record
      integer           iudst_pr_uhead(100)      ! uhead for pilot record

      real              fudst_pr_uhead(100)
      equivalence (iudst_pr_uhead(1),fudst_pr_uhead(1))

      logical           budst_end_evt             ! true if end of input evt
      logical           budst_new_evt             ! true if next write new evt
      integer      iudst_otag_typ         ! output tags
      integer iudst_iOpen, iudst_oOpen
      character*4          cudst_otag_typ
      EQUIVALENCE(cudst_otag_typ,iudst_otag_typ)
      integer      iudst_ologl               ! output log level
      logical           budst_oopen         ! output open flag
c
c=== end of udst ===========================================================
