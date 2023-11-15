#include <PHTable.hh>
#include <cstring>
#include <iostream>

ClassImp(PHTable)

using namespace std;

  //PHTable::PHTable()
  //{}

PHTable::PHTable(const char* name, const size_t& max_rows)
{
  fTableHeader = new TABLE_HEAD_ST();
  fTableHeader->nok    = 0;
  fTableHeader->maxlen = max_rows;
  strncpy(fTableHeader->name,name,19);
}

// PHTable::PHTable(const PHTable& source)
// { }

// PHTable&
// PHTable::operator=(const PHTable& source)
// { }

PHTable::~PHTable()
{
  delete fTableHeader;
}

void*
PHTable::RawTableData()
{
  return 0;
}

TABLE_HEAD_ST
PHTable::TableHeader() const
{
  // Return a copy so that important variables can't be modified!
  return *fTableHeader;
}

size_t
PHTable::RowCount() const
{
  return fTableHeader->nok;
}

size_t
PHTable::RowSize() const
{
  return fTableHeader->rbytes;
}

size_t
PHTable::MaxRowCount() const
{
  return fTableHeader->maxlen;
}

bool
PHTable::IsEmpty() const
{
  return (fTableHeader->nok == 0);
}

void
PHTable::Show() const
{
  cout << "Table " << fTableHeader->name
       << " type " << fTableHeader->type << "\n"
       << " row size = " << fTableHeader->rbytes
       << " max. row count = " << fTableHeader->maxlen
       << " current row count = " << fTableHeader->nok
       << endl;
}

void
PHTable::SetType(const char* type)
{
  strncpy(fTableHeader->type,type,19);
}

void PHTable::Streamer(TBuffer& b)
{
  // Stream a PHTable object.

  if (b.IsReading()) {
    b.ReadStaticArray(fTableHeader->name);         /* table name */
    b.ReadStaticArray(fTableHeader->type);         /* table type */
    b >> fTableHeader->maxlen;       /* # rows allocated */
    b >> fTableHeader->nok;          /* # rows filled */
    b >> fTableHeader->rbytes;       /* number of bytes per row */
    b >> fTableHeader->dsl_pointer;  /* swizzled (DS_DATASET_T*) */
    b >> fTableHeader->data_pointer; /* swizzled (char*) */
  }
  else {
    b.WriteArray(fTableHeader->name,20);         /* table name */
    b.WriteArray(fTableHeader->type,20);         /* table type */
    b << fTableHeader->maxlen;       /* # rows allocated */
    b << fTableHeader->nok;          /* # rows filled */
    b << fTableHeader->rbytes;       /* number of bytes per row */
    b << fTableHeader->dsl_pointer;  /* swizzled (DS_DATASET_T*) */
    b << fTableHeader->data_pointer; /* swizzled (char*) */
  }
}

//void PHTable::ShowMembers(TMemberInspector &R__insp, char *R__parent)
//{
//   // Inspect the data members of an object of class PHTable.
//
//   TClass *R__cl  = PHTable::IsA();
//   Int_t   R__ncp = strlen(R__parent);
//   if (R__ncp || R__cl || R__insp.IsA()) { }
//   R__insp.Inspect(R__cl, R__parent, "fTableHeader->name[20]", fTableHeader->name);
//   R__insp.Inspect(R__cl, R__parent, "fTableHeader->type[20]", fTableHeader->type);
//   R__insp.Inspect(R__cl, R__parent, "fTableHeader->maxlen", &(fTableHeader->maxlen));
//   R__insp.Inspect(R__cl, R__parent, "fTableHeader->nok", &(fTableHeader->nok));
//   R__insp.Inspect(R__cl, R__parent, "fTableHeader->rbytes", &(fTableHeader->rbytes));
//   R__insp.Inspect(R__cl, R__parent, "fTableHeader->dsl_pointer", &(fTableHeader->dsl_pointer));
//   R__insp.Inspect(R__cl, R__parent, "fTableHeader->data_pointer", &(fTableHeader->data_pointer));
//   TObject::ShowMembers(R__insp, R__parent);
//}
