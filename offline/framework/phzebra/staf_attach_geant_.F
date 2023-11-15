      subroutine staf_attach_geant(
     &aGcbank,aGcflag,aPstore,aQuest,aStosto,aIuhead)

      integer aGcbank,aGcflag,aPstore,aQuest,aStosto,aIuhead
#include "pawc.inc"
 

      call attach_gcbank(aGcbank)
      call attach_gcflag(aGcflag)
      call attach_pstore(aPstore)
      call attach_quest (aQuest )
      call attach_stosto(aStosto)
      call attach_iuhead(aIuhead)
      return
      end

      subroutine attach_gcbank(iaddr)
      integer iaddr
#include "gcbank.inc"
      iaddr = loc(NZEBRA   )
      end
      subroutine attach_gcflag(iaddr)
      integer iaddr
#include "gcflag.inc"
      iaddr = loc(IDEBUG   )
      end

      subroutine attach_pstore(iaddr)
      integer iaddr
#include "pstore.inc"
      iaddr = loc(ixdiv_fp )
      end

      subroutine attach_quest(iaddr)
      integer iaddr
#include "quest.inc"
      iaddr = loc( iquest(1))
      end      

      subroutine attach_stosto(iaddr)
      integer iaddr
#include "stosto.inc"
      iaddr = loc(ixstor(1) )
      end

      subroutine attach_iuhead(iaddr)
      integer iaddr
      integer iuhead(100)
      common/iuhead/iuhead
      iaddr = loc(iuhead(1))
      end
