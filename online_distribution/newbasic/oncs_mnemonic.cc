#include <oncsSubConstants.h>


// ---------------------------------------------------------------------

const char *oncs_get_mnemonic (const int structure, const int format)
{
  // if we are not "Unformatted", we return nothing for now.
  // later we will also return the hit formats.

  if (structure) return "";

  switch (format)
    {
    case(IDCRAW): return "IDCRAW";
    case(IDDGEN): return "IDDGEN";
    case(IDHCPY): return "IDHCPY";
    case(ID1STR): return "ID1STR";
    case(IDCSTR): return "IDCSTR";
    case(ID2EVT): return "ID2EVT";
    case(ID4EVT): return "ID4EVT";
    case(ID2SUP): return "ID2SUP";
    case(IDHAMMOND): return "IDHAMMOND";
    case(IDSAM): return "IDSAM";
    case(IDDCFEM): return "IDDCFEM";
    case(IDMIZNHC): return "IDMIZNHC";
    case(IDSIS3300): return "IDSIS3300";
    case(IDCAENV792): return  "IDCAENV792";
    case(IDCAENV785N): return "IDCAENV785N";
    case(IDFIFOBOARD): return "IDFIFOBOARD";
    case(IDBSPETDATA): return "IDBSPETDATA";
    case(IDRCPETDATA): return "IDRCPETDATA";


  }
  return "UNKNOWN";
}

