#include "PHTable.hh"
#include "emlLib.h"

int
ds2ReallocTable(TABLE_HEAD_ST** ppHeader, char** ppData,
		size_t newRowCount)
{
  PHTable* table = (PHTable*)(*ppHeader)->dsl_pointer;
  table->SetMaxRowCount(newRowCount);
  *ppData = (char*)table->RawTableData();

  return true;
}
