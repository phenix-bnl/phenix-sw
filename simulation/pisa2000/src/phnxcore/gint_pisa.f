c     $Id: gint_pisa.f,v 1.28 2020/01/29 20:08:50 lajoie Exp $
c     Authors: S.R. Tonse and C.F. Maguire (based on GSI FOPI code)
      program gint_pisa
      implicit none

c     Global variables (main memory size)
      integer nwgean, nwpaw, nwkuip
      real geant, paw
c Increased nwgean for high energy showers
c      parameter (nwgean=30000000,nwpaw=2000000,nwkuip=30000)
c      parameter (nwgean=60000000,nwpaw=2000000,nwkuip=30000)
      parameter (nwgean=200000000,nwpaw=1500000,nwkuip=30000)
      common/gcbank/geant(nwgean)
      common/pawc/paw(nwpaw)

c     Other GEANT common blocks
#include "gcflag.inc"
#include "gctime.inc"
#include "gcomis.inc"
#include "gcxlun.inc"
#include "guphnx.inc"
#include "guevgen.inc"

      integer iwk
      common/cwk/iwk
      integer  ierr, nh, i, lg, lnblnk

C     These externals because linkers on some platforms miss PISA versions,
C     so must force them to link. (SRT: 1992)
      external guaddparts, gubook, gudigi, guevgen, gufld
      external guhadr, guinit, gukine, gulast, guloren, guphad, gurot
      external gustep, gutrak, gutrev, gugeom

      external gdecay, gdgirl, gen_evt, getpars
      external gsstak 

c     These are the gxxxx.f files
      external gintri, gxfca
      external gsngtr
      external igterm
      external pisaquit
      external gkgcon, gkgeom, gkhist, gkphys, gkpict, gkrz
      external gkcont, gkdraw, gkdz, gkfort, gkfz, gkscan 
      external setosf
      character*80 batchf
      character*80 gtlog
      character*80 logfil

C     Begin execution      
c     To stop in this main program with gdb use "break MAIN__"
c     Note the double underscore at the end
c     Initialize Run ID numbers for simulation projects
      pisaOutputRunNumber = -1
      pisaInputRunNumber = -1
      pisaProjectNumber = 0
      versionNumber = 0


c     Initialize RHICRUN and RHICSUBRUN

      rhicrun = 2 ! run 2, for backward compatibility
      rhicsubrun = 1 ! au + au beam combination
      lorentzdisable = 0 ! allow natural width broadening in gdecay routihne
      nrvacc_evt = 0 ! initialize number of accepted events for a forced accept request
      nstage = 0 ! default as not a non-stack staged simulation (used by gustep)
      nstagestack = 0 ! default as not a staged simulation using geant stack (used by gustep) 
      muidwrite = 0 ! default as no muid layer acceptance condition
      muidaccepted = .true. ! default as event passing muid acceptance condition
      cmname_file = '' ! null name initialization for magnetic field map file name
      pisa_file = 'PISAEvent.root' ! default output filename
           
c     Reaction plane variables
      stplv2 = 0
      pzconst = 0.0
      stplv2 = 0

c     Batch or Interactive version ?
      batchf = 'pisa_batch.kumac'
      gtlog = 'glogon.kumac'
 
      call kuargs('GEANT',gtlog,batchf,logfil,ierr)
      if (ierr .ne. 0) goto 999
      nolog=.false.
      batch=.false.
      if (batchf.ne.' ') batch = .true.
      if (gtlog.eq.' ')  nolog = .true.

*     Initialize Memory Manager (ZEBRA)
      call gzebra(nwgean)
      nh=-nwpaw
      call hlimit(nh)

      call vzero(icomis,7)
      call vzero(lunit,128)
      lunit(5)=6
      lunit(6)=6
      lunit(7)=6
      lunit(10)=8
      do 5 i=11,18
         lunit(i)=6
   5  continue
      lunit(19)=8
      lunit(81)=7
      lunit(82)=7
      lunit(83)=7
      lunit(84)=7
      lunit(89)=7
      lunit(91)=8
      lunit(97)=7
      
*     Initialize Command Processor (KUIP)
      call kuinit(nwkuip)
      call kuquit(pisaquit)
      call kuexit(gulast)
      call kuterm(igterm)

*     define menus and commands
      call gintri
      call g_ini_paw

*     initialize graphics (higz with gks-gral and hplot)
      call iginit(0)

*     request workstation type
      if(batch)then
         iwk=0
      else
         call igwkty(iwk)
      endif

*     initialize hplot
      call hplint(iwk)
      call hermes(6)
      if(.not.batch)call igsa(0)

*     define user geometry
      call guinit      
      call setosf
      call gdinit

*     keep starting time
      ievent=0
      call timest(1.e10)
      call timel(timint)
      call kuexec('SET/PROMPT ''GEANT >''')

*           Execute LOGON macro
      IF (.NOT.NOLOG) THEN
          LG=LNBLNK(GTLOG)
          CALL KUEXEC('EXEC glogon.kumac')
      ENDIF

*            Reset history
      call kuexec('LAST 0')

      if(batch)then
         call kuexec('EXEC pisa_batch.kumac')
      else
         call kuwhag
      endif
 999  continue
      stop
      end
