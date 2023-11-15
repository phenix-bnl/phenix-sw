c
c     ISA_TO_GEANT exists previously as a FUNCTION call (not a subroutine)
c
c     =========================================
      subroutine Lund_To_Geant(Lund_Id,Geant_Id)
c     ==========================================
c
      Implicit None
c
c     Local variables
c     ===============
      Integer Lund_Id, geant_Id
c
        INTEGER numid
        PARAMETER (numid=41)
      Integer I
      save first
      logical first
        INTEGER LUNDID_Table(numid),gltble(-100:100)
        Integer jantid(numid)
c
c     Data statements
c     ===============
      data first /.true./
        DATA LUNDID_Table /
     +    1, -7,  7,  8, -8, -9,  9, 10,-10, 12,-12, 17,-17,
     +   18,-18, 37, 38, 41,-41, 42,-42, 43,-43, 45,-45, 46,-46,
     +   47,-47, 57,-57, 70,-70, 11,-11, 44,-44, 24,-24, 23,-23/
      DATA JANTID/
     +    1,  2,  3,  4,  4,  5,  6,  4,  4,  4,  4,  8,  9,
     +   11, 12, 16, 10, 14, 15, 13, 25, 19, 29, 21, 27, 22, 30,
     +   23, 31, 18, 26, 24, 32, 34, 33, 20, 28, 17, 17,  7,  7/
c
c     Revision History
c
c     Charles F. Maguire  June 2, 1996
c         RQMD TRANSL routine produces "LUND particle IDs" with numbers
c         -19, +19, and 35.  The +/-19 masses are those of the K0
c         The ID = 35 mass is close to that of the Phi meson ??
c
c
c     Executable code
c     ================
      If(First) then
C
C     Make Lund to Geant code lookup table
C
          do i=-100,100
             gltble(i)=0
          end do
          do i=1,numid
            gltble(lundid_Table(i))=jantid(i)
        end do
        first = .false.
      end if
c
c     Convert the type
c     ================

      if(iabs(Lund_Id).eq.19.or.Lund_Id.eq.35)then
c
c     Special RQMD inserts
c
          if(iabs(Lund_Id).eq.19)then
             if(Lund_Id.eq.19)Geant_ID = 10
             if(Lund_Id.eq.-19)Geant_ID =16
          else
c
c     Must check this further (is #35 the PHI Meson ?)
c
             Geant_ID = 1060  ! (special PISA particle ID)
           endif
           return
       endif ! Special RQMD insertions
c
c    Previous code
c
       if(Lund_Id .le. 100) then
          Geant_Id = gltble(Lund_Id)
       else
          Geant_Id = 0
       endif
      return
      end









