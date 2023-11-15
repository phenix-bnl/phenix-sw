*CMZ :  2.04/00 20/08/93  17.56.01  by  Charles F. Maguire
*-- Author :    Surender Saini   13/05/93
 
      subroutine mutager(mutype)
c
c    *************************************************************
c    *                                                           *
c    *  MUTAGER (vsn 1.00)                                       *
c    *                                                           *
c    *  Called by ==> ::                                         *
c    *  IN   ::                                                  *
c    *  OUT  ::                                                  *
c    *                                                           *
c    *  written  by ::  Surender Saini, 13/05/93 16.41.21        *
c    *  modified by ::                                           *
c    *                                                           *
c    *************************************************************
c
      implicit none
 
c ------------------
*KEEP,GCKING.
      INTEGER MXGKIN
      PARAMETER (MXGKIN=100)
      COMMON/GCKING/KCASE,NGKINE,GKIN(5,MXGKIN),
     +                           TOFD(MXGKIN),IFLGK(MXGKIN)
      INTEGER       KCASE,NGKINE ,IFLGK
      REAL          GKIN,TOFD
C
*KEEP,GCKINE.
      COMMON/GCKINE/IKINE,PKINE(10),ITRA,ISTAK,IVERT,IPART,ITRTYP
     +      ,NAPART(5),AMASS,CHARGE,TLIFE,VERT(3),PVERT(4),IPAOLD
C
      INTEGER       IKINE,ITRA,ISTAK,IVERT,IPART,ITRTYP,NAPART,IPAOLD
      REAL          PKINE,AMASS,CHARGE,TLIFE,VERT,PVERT
C
*KEEP,GCTRAK.
      INTEGER NMEC,LMEC,NAMEC,NSTEP ,MAXNST,IGNEXT,INWVOL,ISTOP,MAXMEC
     + ,IGAUTO,IEKBIN,ILOSL, IMULL,INGOTO,NLDOWN,NLEVIN,NLVSAV,ISTORY
      REAL  VECT,GETOT,GEKIN,VOUT,DESTEP,DESTEL,SAFETY,SLENG ,STEP
     + ,SNEXT,SFIELD,TOFG  ,GEKRAT,UPWGHT
      PARAMETER (MAXMEC=30)
      COMMON/GCTRAK/VECT(7),GETOT,GEKIN,VOUT(7),NMEC,LMEC(MAXMEC)
     + ,NAMEC(MAXMEC),NSTEP ,MAXNST,DESTEP,DESTEL,SAFETY,SLENG
     + ,STEP  ,SNEXT ,SFIELD,TOFG  ,GEKRAT,UPWGHT,IGNEXT,INWVOL
     + ,ISTOP ,IGAUTO,IEKBIN, ILOSL, IMULL,INGOTO,NLDOWN,NLEVIN
     + ,NLVSAV,ISTORY
C
*KEND.
c ------------------
      integer it, iucomp
      real mutype
 
      mutype = 10.0
 
c  pair production
 
       if(ipart .ge. 8 .and. ipart .le. 12)then
        if( ngkine .eq. 2)then
         it = 5
          if(iucomp(it,lmec,nmec) .ne. 0)then
           if(itra .eq. 1)then
            mutype = 12.
           else
            mutype = 13.
           end if
          end if
        else
         mutype = 14.
        end if
       else if(ipart .ge. 49)then
        mutype = 11.
       end if
      return
      end
