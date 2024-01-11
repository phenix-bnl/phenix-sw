#include <cstring>
#include <iostream>
#include <iomanip>
#include "dBbcFEMWrapper.h"

using namespace std;

ClassImp(dBbcFEMWrapper)

dBbcFEMWrapper::dBbcFEMWrapper(const char* name, const size_t& max_rows)
  : PHTable(name,max_rows)
{
  size_t rowSize = sizeof(DBBCFEM_ST);
  if (max_rows > 0) { // Avoid allocating a space of zero size!
     fTableData = new DBBCFEM_ST[max_rows];
     SetMaxRowCount(max_rows);
  }
  else {
     fTableData = new DBBCFEM_ST[1];
     SetMaxRowCount(1);
  }

  SetRowSize(rowSize);
  SetType("dBbcFEM");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
}

dBbcFEMWrapper::dBbcFEMWrapper(const dBbcFEMWrapper& source)
  : PHTable(source)
{
  // The row count, the max. row count, and the name are
  // already set in the PHTable copy ctor.

  fTableData = new DBBCFEM_ST[source.MaxRowCount()];
  SetRowSize(sizeof(DBBCFEM_ST));
  SetType("dBbcFEM");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
  
  for (size_t row = 0; row < RowCount(); row++) {
     fTableData[row] = source.fTableData[row];
  }

}

void*
dBbcFEMWrapper::RawTableData()
{
  return static_cast<void*>(fTableData);
}

dBbcFEMWrapper&
dBbcFEMWrapper::operator=(const dBbcFEMWrapper& source)
{
  if (this != &source) {
     // The row count will be set by the PHTable assignment operator.

     PHTable::operator=(source);
     
     // Just copy the data from the source table.
     for (size_t row = 0; row < RowCount(); row++) {
        fTableData[row] = source.fTableData[row];
     }

  }

  return *this;
}

dBbcFEMWrapper::~dBbcFEMWrapper()
{
  delete [] fTableData;
}

DBBCFEM_ST*
dBbcFEMWrapper::TableData()
{
  return fTableData;
}

DBBCFEM_ST&
dBbcFEMWrapper::operator[](const size_t& row)
{
  return fTableData[row];
}

const DBBCFEM_ST&
dBbcFEMWrapper::operator[](const size_t& row) const
{
  return fTableData[row];
}

void
dBbcFEMWrapper::Print(const size_t num_rows, const size_t first_row) const
{
  cout << "   row :";
  cout << " " << setw(11) << "CAV1";
  cout << " " << setw(11) << "det";
  cout << " " << setw(11) << "Ecounter";
  cout << " " << setw(11) << "adr";
  cout << " " << setw(11) << "Flag";
  cout << " " << setw(11) << "Bcounter";
  cout << " Word[0]";
  cout << " Word[1]";
  cout << " Word[2]";
  cout << " Word[3]";
  cout << " Word[4]";
  cout << " Word[5]";
  cout << " Word[6]";
  cout << " Word[7]";
  cout << " Word[8]";
  cout << " Word[9]";
  cout << " Word[10]";
  cout << " Word[11]";
  cout << " Word[12]";
  cout << " Word[13]";
  cout << " Word[14]";
  cout << " Word[15]";
  cout << " Word[16]";
  cout << " Word[17]";
  cout << " Word[18]";
  cout << " Word[19]";
  cout << " Word[20]";
  cout << " Word[21]";
  cout << " Word[22]";
  cout << " Word[23]";
  cout << " Word[24]";
  cout << " Word[25]";
  cout << " Word[26]";
  cout << " Word[27]";
  cout << " Word[28]";
  cout << " Word[29]";
  cout << " Word[30]";
  cout << " Word[31]";
  cout << " Word[32]";
  cout << " Word[33]";
  cout << " Word[34]";
  cout << " Word[35]";
  cout << " Word[36]";
  cout << " Word[37]";
  cout << " Word[38]";
  cout << " Word[39]";
  cout << " Word[40]";
  cout << " Word[41]";
  cout << " Word[42]";
  cout << " Word[43]";
  cout << " Word[44]";
  cout << " Word[45]";
  cout << " Word[46]";
  cout << " Word[47]";
  cout << " Word[48]";
  cout << " Word[49]";
  cout << " Word[50]";
  cout << " Word[51]";
  cout << " Word[52]";
  cout << " Word[53]";
  cout << " Word[54]";
  cout << " Word[55]";
  cout << " Word[56]";
  cout << " Word[57]";
  cout << " Word[58]";
  cout << " Word[59]";
  cout << " Word[60]";
  cout << " Word[61]";
  cout << " Word[62]";
  cout << " Word[63]";
  cout << " Word[64]";
  cout << " Word[65]";
  cout << " Word[66]";
  cout << " Word[67]";
  cout << " Word[68]";
  cout << " Word[69]";
  cout << " Word[70]";
  cout << " Word[71]";
  cout << " Word[72]";
  cout << " Word[73]";
  cout << " Word[74]";
  cout << " Word[75]";
  cout << " Word[76]";
  cout << " Word[77]";
  cout << " Word[78]";
  cout << " Word[79]";
  cout << " Word[80]";
  cout << " Word[81]";
  cout << " Word[82]";
  cout << " Word[83]";
  cout << " Word[84]";
  cout << " Word[85]";
  cout << " Word[86]";
  cout << " Word[87]";
  cout << " Word[88]";
  cout << " Word[89]";
  cout << " Word[90]";
  cout << " Word[91]";
  cout << " Word[92]";
  cout << " Word[93]";
  cout << " Word[94]";
  cout << " Word[95]";
  cout << " Word[96]";
  cout << " Word[97]";
  cout << " Word[98]";
  cout << " Word[99]";
  cout << " Word[100]";
  cout << " Word[101]";
  cout << " Word[102]";
  cout << " Word[103]";
  cout << " Word[104]";
  cout << " Word[105]";
  cout << " Word[106]";
  cout << " Word[107]";
  cout << " Word[108]";
  cout << " Word[109]";
  cout << " Word[110]";
  cout << " Word[111]";
  cout << " Word[112]";
  cout << " Word[113]";
  cout << " Word[114]";
  cout << " Word[115]";
  cout << " Word[116]";
  cout << " Word[117]";
  cout << " Word[118]";
  cout << " Word[119]";
  cout << " Word[120]";
  cout << " Word[121]";
  cout << " Word[122]";
  cout << " Word[123]";
  cout << " Word[124]";
  cout << " Word[125]";
  cout << " Word[126]";
  cout << " Word[127]";
  cout << " Word[128]";
  cout << " Word[129]";
  cout << " Word[130]";
  cout << " Word[131]";
  cout << " Word[132]";
  cout << " Word[133]";
  cout << " Word[134]";
  cout << " Word[135]";
  cout << " Word[136]";
  cout << " Word[137]";
  cout << " Word[138]";
  cout << " Word[139]";
  cout << " Word[140]";
  cout << " Word[141]";
  cout << " Word[142]";
  cout << " Word[143]";
  cout << " Word[144]";
  cout << " Word[145]";
  cout << " Word[146]";
  cout << " Word[147]";
  cout << " Word[148]";
  cout << " Word[149]";
  cout << " Word[150]";
  cout << " Word[151]";
  cout << " Word[152]";
  cout << " Word[153]";
  cout << " Word[154]";
  cout << " Word[155]";
  cout << " Word[156]";
  cout << " Word[157]";
  cout << " Word[158]";
  cout << " Word[159]";
  cout << " Word[160]";
  cout << " Word[161]";
  cout << " Word[162]";
  cout << " Word[163]";
  cout << " Word[164]";
  cout << " Word[165]";
  cout << " Word[166]";
  cout << " Word[167]";
  cout << " Word[168]";
  cout << " Word[169]";
  cout << " Word[170]";
  cout << " Word[171]";
  cout << " Word[172]";
  cout << " Word[173]";
  cout << " Word[174]";
  cout << " Word[175]";
  cout << " Word[176]";
  cout << " Word[177]";
  cout << " Word[178]";
  cout << " Word[179]";
  cout << " Word[180]";
  cout << " Word[181]";
  cout << " Word[182]";
  cout << " Word[183]";
  cout << " Word[184]";
  cout << " Word[185]";
  cout << " Word[186]";
  cout << " Word[187]";
  cout << " Word[188]";
  cout << " Word[189]";
  cout << " Word[190]";
  cout << " Word[191]";
  cout << " Word[192]";
  cout << " Word[193]";
  cout << " Word[194]";
  cout << " Word[195]";
  cout << " Word[196]";
  cout << " Word[197]";
  cout << " Word[198]";
  cout << " Word[199]";
  cout << " Word[200]";
  cout << " Word[201]";
  cout << " Word[202]";
  cout << " Word[203]";
  cout << " Word[204]";
  cout << " Word[205]";
  cout << " Word[206]";
  cout << " Word[207]";
  cout << " Word[208]";
  cout << " Word[209]";
  cout << " Word[210]";
  cout << " Word[211]";
  cout << " Word[212]";
  cout << " Word[213]";
  cout << " Word[214]";
  cout << " Word[215]";
  cout << " Word[216]";
  cout << " Word[217]";
  cout << " Word[218]";
  cout << " Word[219]";
  cout << " Word[220]";
  cout << " Word[221]";
  cout << " Word[222]";
  cout << " Word[223]";
  cout << " Word[224]";
  cout << " Word[225]";
  cout << " Word[226]";
  cout << " Word[227]";
  cout << " Word[228]";
  cout << " Word[229]";
  cout << " Word[230]";
  cout << " Word[231]";
  cout << " Word[232]";
  cout << " Word[233]";
  cout << " Word[234]";
  cout << " Word[235]";
  cout << " Word[236]";
  cout << " Word[237]";
  cout << " Word[238]";
  cout << " Word[239]";
  cout << " Word[240]";
  cout << " Word[241]";
  cout << " Word[242]";
  cout << " Word[243]";
  cout << " Word[244]";
  cout << " Word[245]";
  cout << " Word[246]";
  cout << " Word[247]";
  cout << " Word[248]";
  cout << " Word[249]";
  cout << " Word[250]";
  cout << " Word[251]";
  cout << " Word[252]";
  cout << " Word[253]";
  cout << " Word[254]";
  cout << " Word[255]";
  cout << " Word[256]";
  cout << " Word[257]";
  cout << " Word[258]";
  cout << " Word[259]";
  cout << " Word[260]";
  cout << " Word[261]";
  cout << " Word[262]";
  cout << " Word[263]";
  cout << " Word[264]";
  cout << " Word[265]";
  cout << " Word[266]";
  cout << " Word[267]";
  cout << " Word[268]";
  cout << " Word[269]";
  cout << " Word[270]";
  cout << " Word[271]";
  cout << " Word[272]";
  cout << " Word[273]";
  cout << " Word[274]";
  cout << " Word[275]";
  cout << " Word[276]";
  cout << " Word[277]";
  cout << " Word[278]";
  cout << " Word[279]";
  cout << " Word[280]";
  cout << " Word[281]";
  cout << " Word[282]";
  cout << " Word[283]";
  cout << " Word[284]";
  cout << " Word[285]";
  cout << " Word[286]";
  cout << " Word[287]";
  cout << " Word[288]";
  cout << " Word[289]";
  cout << " Word[290]";
  cout << " Word[291]";
  cout << " Word[292]";
  cout << " Word[293]";
  cout << " Word[294]";
  cout << " Word[295]";
  cout << " Word[296]";
  cout << " Word[297]";
  cout << " Word[298]";
  cout << " Word[299]";
  cout << " Word[300]";
  cout << " Word[301]";
  cout << " Word[302]";
  cout << " Word[303]";
  cout << " Word[304]";
  cout << " Word[305]";
  cout << " Word[306]";
  cout << " Word[307]";
  cout << " Word[308]";
  cout << " Word[309]";
  cout << " Word[310]";
  cout << " Word[311]";
  cout << " Word[312]";
  cout << " Word[313]";
  cout << " Word[314]";
  cout << " Word[315]";
  cout << " Word[316]";
  cout << " Word[317]";
  cout << " Word[318]";
  cout << " Word[319]";
  cout << " Word[320]";
  cout << " Word[321]";
  cout << " Word[322]";
  cout << " Word[323]";
  cout << " Word[324]";
  cout << " Word[325]";
  cout << " Word[326]";
  cout << " Word[327]";
  cout << " Word[328]";
  cout << " Word[329]";
  cout << " Word[330]";
  cout << " Word[331]";
  cout << " Word[332]";
  cout << " Word[333]";
  cout << " Word[334]";
  cout << " Word[335]";
  cout << " Word[336]";
  cout << " Word[337]";
  cout << " Word[338]";
  cout << " Word[339]";
  cout << " Word[340]";
  cout << " Word[341]";
  cout << " Word[342]";
  cout << " Word[343]";
  cout << " Word[344]";
  cout << " Word[345]";
  cout << " Word[346]";
  cout << " Word[347]";
  cout << " Word[348]";
  cout << " Word[349]";
  cout << " Word[350]";
  cout << " Word[351]";
  cout << " Word[352]";
  cout << " Word[353]";
  cout << " Word[354]";
  cout << " Word[355]";
  cout << " Word[356]";
  cout << " Word[357]";
  cout << " Word[358]";
  cout << " Word[359]";
  cout << " Word[360]";
  cout << " Word[361]";
  cout << " Word[362]";
  cout << " Word[363]";
  cout << " Word[364]";
  cout << " Word[365]";
  cout << " Word[366]";
  cout << " Word[367]";
  cout << " Word[368]";
  cout << " Word[369]";
  cout << " Word[370]";
  cout << " Word[371]";
  cout << " Word[372]";
  cout << " Word[373]";
  cout << " Word[374]";
  cout << " Word[375]";
  cout << " Word[376]";
  cout << " Word[377]";
  cout << " Word[378]";
  cout << " Word[379]";
  cout << " Word[380]";
  cout << " Word[381]";
  cout << " Word[382]";
  cout << " Word[383]";
  cout << " Word[384]";
  cout << " Word[385]";
  cout << " Word[386]";
  cout << " Word[387]";
  cout << " Word[388]";
  cout << " Word[389]";
  cout << " Word[390]";
  cout << " Word[391]";
  cout << " Word[392]";
  cout << " Word[393]";
  cout << " Word[394]";
  cout << " Word[395]";
  cout << " Word[396]";
  cout << " Word[397]";
  cout << " Word[398]";
  cout << " Word[399]";
  cout << " " << setw(11) << "parity";
  cout << " " << setw(11) << "CAV2";
  cout << endl;

  size_t last_row = RowCount();
  if (last_row > first_row+num_rows) last_row = first_row+num_rows;
  for (size_t row = first_row; row < last_row; row++) {
     cout << setw(7) << row << ":";

     cout << " " << setw(11) << fTableData[row].CAV1;
     cout << " " << setw(11) << fTableData[row].det;
     cout << " " << setw(11) << fTableData[row].Ecounter;
     cout << " " << setw(11) << fTableData[row].adr;
     cout << " " << setw(11) << fTableData[row].Flag;
     cout << " " << setw(11) << fTableData[row].Bcounter;
     for(int i0=0; i0<400; i0++) {
        cout << " " << setw(11) << fTableData[row].Word[i0];
     }
     cout << " " << setw(11) << fTableData[row].parity;
     cout << " " << setw(11) << fTableData[row].CAV2;

     cout << endl;
  }

}

void
dBbcFEMWrapper::Print(Option_t* option) const
{
   // This version of Print overrides the one in the TObject
   // base class, and provides a way to call Print with no
   // arguments.  If Print(const size_t, const size_t) const
   // could be called with no arguments, there would be an
   // ambiguity.  I hope that this explanation makes sense!

   if (!option || (strlen(option) <= 0) ) {
     // default:  call Print(const size_t, const size_t)
     Print(10, 0);
   } else {
     // non-null option:  call PHTable::Print, for lack of
     // anything better to do ...
     PHTable::Print(option);
   }
}

void
dBbcFEMWrapper::SetMaxRowCount(const size_t& max_rows)
{
  // Avoid reallocing a space of zero size!
  if (max_rows <= 0) {
     return;
  }

  // Ensure that the current row count is not out of range.
  if ((size_t) fTableHeader->nok > max_rows) {
     fTableHeader->nok = max_rows;
  }

  // If table needs to grow, allocate a new area for it.
  if (max_rows > (size_t) fTableHeader->maxlen) {
     DBBCFEM_ST* newData = new DBBCFEM_ST[max_rows];
     if (fTableData) {
        for (long i = 0; i < fTableHeader->nok; i++) {
           newData[i] = fTableData[i];
        }
        delete [] fTableData;
     }
     fTableData = newData;
     fTableHeader->data_pointer = (long)fTableData;
  }

  fTableHeader->maxlen = max_rows;
}
void
dBbcFEMWrapper::SetRowCount(const size_t& n)
{
  if (n > (size_t) fTableHeader->maxlen) {
     fTableHeader->nok = fTableHeader->maxlen;
  }
  else if (n >= 0) {
     fTableHeader->nok = n;
  }
}

void
dBbcFEMWrapper::SetRowSize(const size_t& row_size)
{
  if (row_size > 0) {
     fTableHeader->rbytes = row_size;
  }
}

void
dBbcFEMWrapper::Streamer(TBuffer& b)
{
   // Stream an object of class dBbcFEMWrapper.
   // What should be done on output if the table is empty?

   if (b.IsReading()) {
     b.ReadVersion();
     PHTable::Streamer(b);         // Read the table header.
     if (RowSize() != sizeof(DBBCFEM_ST)) {
       // Sanity check failed.  Need some error message here.
       return;
     }

     // Reallocate the table explicitly here; the size of the data array
     // may be inconsistent with the max. row count variable in the header
     // (since the ROOT I/O default-constructs the former, and reads
     // the header for the latter).
     size_t max_rows = MaxRowCount();
     if (max_rows <= 0) { // Avoid allocating a space of zero size!
        max_rows = 1;
     }

     delete [] fTableData;
     fTableData = new DBBCFEM_ST[max_rows];
     fTableHeader->data_pointer = (long)fTableData;

     SetMaxRowCount(max_rows);
     SetType("dBbcFEMWrapper");

     for (unsigned long i=0; i<RowCount(); i++) {
        b >> fTableData[i].CAV1;
        b >> fTableData[i].det;
        b >> fTableData[i].Ecounter;
        b >> fTableData[i].adr;
        b >> fTableData[i].Flag;
        b >> fTableData[i].Bcounter;
        b.ReadStaticArray(fTableData[i].Word);
        b >> fTableData[i].parity;
        b >> fTableData[i].CAV2;
     }
   }
   else {
     b.WriteVersion(IsA());
     PHTable::Streamer(b);         // Write the table header.
     for (unsigned long i=0; i<RowCount(); i++) {
        b << fTableData[i].CAV1;
        b << fTableData[i].det;
        b << fTableData[i].Ecounter;
        b << fTableData[i].adr;
        b << fTableData[i].Flag;
        b << fTableData[i].Bcounter;
        b.WriteArray(fTableData[i].Word,400);
        b << fTableData[i].parity;
        b << fTableData[i].CAV2;
     }
   }

}
/* Automatically generated.  Do not edit. */
