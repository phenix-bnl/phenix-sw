#include <cstring>
#include <iostream>
#include <iomanip>
#include "dBbcDCMWrapper.h"

using namespace std;

ClassImp(dBbcDCMWrapper)

dBbcDCMWrapper::dBbcDCMWrapper(const char* name, const size_t& max_rows)
  : PHTable(name,max_rows)
{
  size_t rowSize = sizeof(DBBCDCM_ST);
  if (max_rows > 0) { // Avoid allocating a space of zero size!
     fTableData = new DBBCDCM_ST[max_rows];
     SetMaxRowCount(max_rows);
  }
  else {
     fTableData = new DBBCDCM_ST[1];
     SetMaxRowCount(1);
  }

  SetRowSize(rowSize);
  SetType("dBbcDCM");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
}

dBbcDCMWrapper::dBbcDCMWrapper(const dBbcDCMWrapper& source)
  : PHTable(source)
{
  // The row count, the max. row count, and the name are
  // already set in the PHTable copy ctor.

  fTableData = new DBBCDCM_ST[source.MaxRowCount()];
  SetRowSize(sizeof(DBBCDCM_ST));
  SetType("dBbcDCM");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
  
  for (size_t row = 0; row < RowCount(); row++) {
     fTableData[row] = source.fTableData[row];
  }

}

void*
dBbcDCMWrapper::RawTableData()
{
  return static_cast<void*>(fTableData);
}

dBbcDCMWrapper&
dBbcDCMWrapper::operator=(const dBbcDCMWrapper& source)
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

dBbcDCMWrapper::~dBbcDCMWrapper()
{
  delete [] fTableData;
}

DBBCDCM_ST*
dBbcDCMWrapper::TableData()
{
  return fTableData;
}

DBBCDCM_ST&
dBbcDCMWrapper::operator[](const size_t& row)
{
  return fTableData[row];
}

const DBBCDCM_ST&
dBbcDCMWrapper::operator[](const size_t& row) const
{
  return fTableData[row];
}

void
dBbcDCMWrapper::Print(const size_t num_rows, const size_t first_row) const
{
  cout << "   row :";
  cout << " " << setw(11) << "nWord";
  cout << " " << setw(11) << "scheme";
  cout << " " << setw(11) << "packetID";
  cout << " DCM[0]";
  cout << " DCM[1]";
  cout << " DCM[2]";
  cout << " DCM[3]";
  cout << " DCM[4]";
  cout << " DCM[5]";
  cout << " DCM[6]";
  cout << " DCM[7]";
  cout << " DCM[8]";
  cout << " DCM[9]";
  cout << " DCM[10]";
  cout << " DCM[11]";
  cout << " DCM[12]";
  cout << " DCM[13]";
  cout << " DCM[14]";
  cout << " DCM[15]";
  cout << " DCM[16]";
  cout << " DCM[17]";
  cout << " DCM[18]";
  cout << " DCM[19]";
  cout << " DCM[20]";
  cout << " DCM[21]";
  cout << " DCM[22]";
  cout << " DCM[23]";
  cout << " DCM[24]";
  cout << " DCM[25]";
  cout << " DCM[26]";
  cout << " DCM[27]";
  cout << " DCM[28]";
  cout << " DCM[29]";
  cout << " DCM[30]";
  cout << " DCM[31]";
  cout << " DCM[32]";
  cout << " DCM[33]";
  cout << " DCM[34]";
  cout << " DCM[35]";
  cout << " DCM[36]";
  cout << " DCM[37]";
  cout << " DCM[38]";
  cout << " DCM[39]";
  cout << " DCM[40]";
  cout << " DCM[41]";
  cout << " DCM[42]";
  cout << " DCM[43]";
  cout << " DCM[44]";
  cout << " DCM[45]";
  cout << " DCM[46]";
  cout << " DCM[47]";
  cout << " DCM[48]";
  cout << " DCM[49]";
  cout << " DCM[50]";
  cout << " DCM[51]";
  cout << " DCM[52]";
  cout << " DCM[53]";
  cout << " DCM[54]";
  cout << " DCM[55]";
  cout << " DCM[56]";
  cout << " DCM[57]";
  cout << " DCM[58]";
  cout << " DCM[59]";
  cout << " DCM[60]";
  cout << " DCM[61]";
  cout << " DCM[62]";
  cout << " DCM[63]";
  cout << " DCM[64]";
  cout << " DCM[65]";
  cout << " DCM[66]";
  cout << " DCM[67]";
  cout << " DCM[68]";
  cout << " DCM[69]";
  cout << " DCM[70]";
  cout << " DCM[71]";
  cout << " DCM[72]";
  cout << " DCM[73]";
  cout << " DCM[74]";
  cout << " DCM[75]";
  cout << " DCM[76]";
  cout << " DCM[77]";
  cout << " DCM[78]";
  cout << " DCM[79]";
  cout << " DCM[80]";
  cout << " DCM[81]";
  cout << " DCM[82]";
  cout << " DCM[83]";
  cout << " DCM[84]";
  cout << " DCM[85]";
  cout << " DCM[86]";
  cout << " DCM[87]";
  cout << " DCM[88]";
  cout << " DCM[89]";
  cout << " DCM[90]";
  cout << " DCM[91]";
  cout << " DCM[92]";
  cout << " DCM[93]";
  cout << " DCM[94]";
  cout << " DCM[95]";
  cout << " DCM[96]";
  cout << " DCM[97]";
  cout << " DCM[98]";
  cout << " DCM[99]";
  cout << " DCM[100]";
  cout << " DCM[101]";
  cout << " DCM[102]";
  cout << " DCM[103]";
  cout << " DCM[104]";
  cout << " DCM[105]";
  cout << " DCM[106]";
  cout << " DCM[107]";
  cout << " DCM[108]";
  cout << " DCM[109]";
  cout << " DCM[110]";
  cout << " DCM[111]";
  cout << " DCM[112]";
  cout << " DCM[113]";
  cout << " DCM[114]";
  cout << " DCM[115]";
  cout << " DCM[116]";
  cout << " DCM[117]";
  cout << " DCM[118]";
  cout << " DCM[119]";
  cout << " DCM[120]";
  cout << " DCM[121]";
  cout << " DCM[122]";
  cout << " DCM[123]";
  cout << " DCM[124]";
  cout << " DCM[125]";
  cout << " DCM[126]";
  cout << " DCM[127]";
  cout << " DCM[128]";
  cout << " DCM[129]";
  cout << " DCM[130]";
  cout << " DCM[131]";
  cout << " DCM[132]";
  cout << " DCM[133]";
  cout << " DCM[134]";
  cout << " DCM[135]";
  cout << " DCM[136]";
  cout << " DCM[137]";
  cout << " DCM[138]";
  cout << " DCM[139]";
  cout << " DCM[140]";
  cout << " DCM[141]";
  cout << " DCM[142]";
  cout << " DCM[143]";
  cout << " DCM[144]";
  cout << " DCM[145]";
  cout << " DCM[146]";
  cout << " DCM[147]";
  cout << " DCM[148]";
  cout << " DCM[149]";
  cout << " DCM[150]";
  cout << " DCM[151]";
  cout << " DCM[152]";
  cout << " DCM[153]";
  cout << " DCM[154]";
  cout << " DCM[155]";
  cout << " DCM[156]";
  cout << " DCM[157]";
  cout << " DCM[158]";
  cout << " DCM[159]";
  cout << " DCM[160]";
  cout << " DCM[161]";
  cout << " DCM[162]";
  cout << " DCM[163]";
  cout << " DCM[164]";
  cout << " DCM[165]";
  cout << " DCM[166]";
  cout << " DCM[167]";
  cout << " DCM[168]";
  cout << " DCM[169]";
  cout << " DCM[170]";
  cout << " DCM[171]";
  cout << " DCM[172]";
  cout << " DCM[173]";
  cout << " DCM[174]";
  cout << " DCM[175]";
  cout << " DCM[176]";
  cout << " DCM[177]";
  cout << " DCM[178]";
  cout << " DCM[179]";
  cout << " DCM[180]";
  cout << " DCM[181]";
  cout << " DCM[182]";
  cout << " DCM[183]";
  cout << " DCM[184]";
  cout << " DCM[185]";
  cout << " DCM[186]";
  cout << " DCM[187]";
  cout << " DCM[188]";
  cout << " DCM[189]";
  cout << " DCM[190]";
  cout << " DCM[191]";
  cout << " DCM[192]";
  cout << " DCM[193]";
  cout << " DCM[194]";
  cout << " DCM[195]";
  cout << " DCM[196]";
  cout << " DCM[197]";
  cout << " DCM[198]";
  cout << " DCM[199]";
  cout << " DCM[200]";
  cout << " DCM[201]";
  cout << " DCM[202]";
  cout << " DCM[203]";
  cout << " DCM[204]";
  cout << " DCM[205]";
  cout << " DCM[206]";
  cout << " DCM[207]";
  cout << " DCM[208]";
  cout << " DCM[209]";
  cout << " DCM[210]";
  cout << " DCM[211]";
  cout << " DCM[212]";
  cout << " DCM[213]";
  cout << " DCM[214]";
  cout << " DCM[215]";
  cout << " DCM[216]";
  cout << " DCM[217]";
  cout << " DCM[218]";
  cout << " DCM[219]";
  cout << " DCM[220]";
  cout << " DCM[221]";
  cout << " DCM[222]";
  cout << " DCM[223]";
  cout << " DCM[224]";
  cout << " DCM[225]";
  cout << " DCM[226]";
  cout << " DCM[227]";
  cout << " DCM[228]";
  cout << " DCM[229]";
  cout << " DCM[230]";
  cout << " DCM[231]";
  cout << " DCM[232]";
  cout << " DCM[233]";
  cout << " DCM[234]";
  cout << " DCM[235]";
  cout << " DCM[236]";
  cout << " DCM[237]";
  cout << " DCM[238]";
  cout << " DCM[239]";
  cout << " DCM[240]";
  cout << " DCM[241]";
  cout << " DCM[242]";
  cout << " DCM[243]";
  cout << " DCM[244]";
  cout << " DCM[245]";
  cout << " DCM[246]";
  cout << " DCM[247]";
  cout << " DCM[248]";
  cout << " DCM[249]";
  cout << " DCM[250]";
  cout << " DCM[251]";
  cout << " DCM[252]";
  cout << " DCM[253]";
  cout << " DCM[254]";
  cout << " DCM[255]";
  cout << " DCM[256]";
  cout << " DCM[257]";
  cout << " DCM[258]";
  cout << " DCM[259]";
  cout << " DCM[260]";
  cout << " DCM[261]";
  cout << " DCM[262]";
  cout << " DCM[263]";
  cout << " DCM[264]";
  cout << " DCM[265]";
  cout << " DCM[266]";
  cout << " DCM[267]";
  cout << " DCM[268]";
  cout << " DCM[269]";
  cout << " DCM[270]";
  cout << " DCM[271]";
  cout << " DCM[272]";
  cout << " DCM[273]";
  cout << " DCM[274]";
  cout << " DCM[275]";
  cout << " DCM[276]";
  cout << " DCM[277]";
  cout << " DCM[278]";
  cout << " DCM[279]";
  cout << " DCM[280]";
  cout << " DCM[281]";
  cout << " DCM[282]";
  cout << " DCM[283]";
  cout << " DCM[284]";
  cout << " DCM[285]";
  cout << " DCM[286]";
  cout << " DCM[287]";
  cout << " DCM[288]";
  cout << " DCM[289]";
  cout << " DCM[290]";
  cout << " DCM[291]";
  cout << " DCM[292]";
  cout << " DCM[293]";
  cout << " DCM[294]";
  cout << " DCM[295]";
  cout << " DCM[296]";
  cout << " DCM[297]";
  cout << " DCM[298]";
  cout << " DCM[299]";
  cout << " DCM[300]";
  cout << " DCM[301]";
  cout << " DCM[302]";
  cout << " DCM[303]";
  cout << " DCM[304]";
  cout << " DCM[305]";
  cout << " DCM[306]";
  cout << " DCM[307]";
  cout << " DCM[308]";
  cout << " DCM[309]";
  cout << " DCM[310]";
  cout << " DCM[311]";
  cout << " DCM[312]";
  cout << " DCM[313]";
  cout << " DCM[314]";
  cout << " DCM[315]";
  cout << " DCM[316]";
  cout << " DCM[317]";
  cout << " DCM[318]";
  cout << " DCM[319]";
  cout << " DCM[320]";
  cout << " DCM[321]";
  cout << " DCM[322]";
  cout << " DCM[323]";
  cout << " DCM[324]";
  cout << " DCM[325]";
  cout << " DCM[326]";
  cout << " DCM[327]";
  cout << " DCM[328]";
  cout << " DCM[329]";
  cout << " DCM[330]";
  cout << " DCM[331]";
  cout << " DCM[332]";
  cout << " DCM[333]";
  cout << " DCM[334]";
  cout << " DCM[335]";
  cout << " DCM[336]";
  cout << " DCM[337]";
  cout << " DCM[338]";
  cout << " DCM[339]";
  cout << " DCM[340]";
  cout << " DCM[341]";
  cout << " DCM[342]";
  cout << " DCM[343]";
  cout << " DCM[344]";
  cout << " DCM[345]";
  cout << " DCM[346]";
  cout << " DCM[347]";
  cout << " DCM[348]";
  cout << " DCM[349]";
  cout << " DCM[350]";
  cout << " DCM[351]";
  cout << " DCM[352]";
  cout << " DCM[353]";
  cout << " DCM[354]";
  cout << " DCM[355]";
  cout << " DCM[356]";
  cout << " DCM[357]";
  cout << " DCM[358]";
  cout << " DCM[359]";
  cout << " DCM[360]";
  cout << " DCM[361]";
  cout << " DCM[362]";
  cout << " DCM[363]";
  cout << " DCM[364]";
  cout << " DCM[365]";
  cout << " DCM[366]";
  cout << " DCM[367]";
  cout << " DCM[368]";
  cout << " DCM[369]";
  cout << " DCM[370]";
  cout << " DCM[371]";
  cout << " DCM[372]";
  cout << " DCM[373]";
  cout << " DCM[374]";
  cout << " DCM[375]";
  cout << " DCM[376]";
  cout << " DCM[377]";
  cout << " DCM[378]";
  cout << " DCM[379]";
  cout << " DCM[380]";
  cout << " DCM[381]";
  cout << " DCM[382]";
  cout << " DCM[383]";
  cout << " DCM[384]";
  cout << " DCM[385]";
  cout << " DCM[386]";
  cout << " DCM[387]";
  cout << " DCM[388]";
  cout << " DCM[389]";
  cout << " DCM[390]";
  cout << " DCM[391]";
  cout << " DCM[392]";
  cout << " DCM[393]";
  cout << " DCM[394]";
  cout << " DCM[395]";
  cout << " DCM[396]";
  cout << " DCM[397]";
  cout << " DCM[398]";
  cout << " DCM[399]";
  cout << " DCM[400]";
  cout << " DCM[401]";
  cout << " DCM[402]";
  cout << " DCM[403]";
  cout << " DCM[404]";
  cout << " DCM[405]";
  cout << " DCM[406]";
  cout << endl;

  size_t last_row = RowCount();
  if (last_row > first_row+num_rows) last_row = first_row+num_rows;
  for (size_t row = first_row; row < last_row; row++) {
     cout << setw(7) << row << ":";

     cout << " " << setw(11) << fTableData[row].nWord;
     cout << " " << setw(11) << fTableData[row].scheme;
     cout << " " << setw(11) << fTableData[row].packetID;
     for(int i0=0; i0<407; i0++) {
        cout << " " << setw(11) << fTableData[row].DCM[i0];
     }

     cout << endl;
  }

}

void
dBbcDCMWrapper::Print(Option_t* option) const
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
dBbcDCMWrapper::SetMaxRowCount(const size_t& max_rows)
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
     DBBCDCM_ST* newData = new DBBCDCM_ST[max_rows];
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
dBbcDCMWrapper::SetRowCount(const size_t& n)
{
  if (n > (size_t) fTableHeader->maxlen) {
     fTableHeader->nok = fTableHeader->maxlen;
  }
  else if (n >= 0) {
     fTableHeader->nok = n;
  }
}

void
dBbcDCMWrapper::SetRowSize(const size_t& row_size)
{
  if (row_size > 0) {
     fTableHeader->rbytes = row_size;
  }
}

void
dBbcDCMWrapper::Streamer(TBuffer& b)
{
   // Stream an object of class dBbcDCMWrapper.
   // What should be done on output if the table is empty?

   if (b.IsReading()) {
     b.ReadVersion();
     PHTable::Streamer(b);         // Read the table header.
     if (RowSize() != sizeof(DBBCDCM_ST)) {
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
     fTableData = new DBBCDCM_ST[max_rows];
     fTableHeader->data_pointer = (long)fTableData;

     SetMaxRowCount(max_rows);
     SetType("dBbcDCMWrapper");

     for (unsigned long i=0; i<RowCount(); i++) {
        b >> fTableData[i].nWord;
        b >> fTableData[i].scheme;
        b >> fTableData[i].packetID;
        b.ReadStaticArray(fTableData[i].DCM);
     }
   }
   else {
     b.WriteVersion(IsA());
     PHTable::Streamer(b);         // Write the table header.
     for (unsigned long i=0; i<RowCount(); i++) {
        b << fTableData[i].nWord;
        b << fTableData[i].scheme;
        b << fTableData[i].packetID;
        b.WriteArray(fTableData[i].DCM,407);
     }
   }

}
/* Automatically generated.  Do not edit. */
