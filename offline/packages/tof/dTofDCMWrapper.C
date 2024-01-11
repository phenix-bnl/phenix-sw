#include <cstring>
#include <iostream>
#include <iomanip>
#include "dTofDCMWrapper.h"

ClassImp(dTofDCMWrapper);

using namespace std;

dTofDCMWrapper::dTofDCMWrapper(const char* name, const size_t& max_rows)
  : PHTable(name,max_rows)
{
  size_t rowSize = sizeof(DTOFDCM_ST);
  if (max_rows > 0) { // Avoid allocating a space of zero size!
     fTableData = new DTOFDCM_ST[max_rows];
     SetMaxRowCount(max_rows);
  }
  else {
     fTableData = new DTOFDCM_ST[1];
     SetMaxRowCount(1);
  }

  SetRowSize(rowSize);
  SetType("dTofDCM");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
}

dTofDCMWrapper::dTofDCMWrapper(const dTofDCMWrapper& source)
  : PHTable(source)
{
  // The row count, the max. row count, and the name are
  // already set in the PHTable copy ctor.

  fTableData = new DTOFDCM_ST[source.MaxRowCount()];
  SetRowSize(sizeof(DTOFDCM_ST));
  SetType("dTofDCM");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
  
  for (size_t row = 0; row < RowCount(); row++) {
     fTableData[row] = source.fTableData[row];
  }

}

void*
dTofDCMWrapper::RawTableData()
{
  return static_cast<void*>(fTableData);
}

dTofDCMWrapper&
dTofDCMWrapper::operator=(const dTofDCMWrapper& source)
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

dTofDCMWrapper::~dTofDCMWrapper()
{
  delete [] fTableData;
}

DTOFDCM_ST*
dTofDCMWrapper::TableData()
{
  return fTableData;
}

DTOFDCM_ST&
dTofDCMWrapper::operator[](const size_t& row)
{
  return fTableData[row];
}

const DTOFDCM_ST&
dTofDCMWrapper::operator[](const size_t& row) const
{
  return fTableData[row];
}

void
dTofDCMWrapper::Print(const size_t num_rows, const size_t first_row) const
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
  cout << " DCM[407]";
  cout << " DCM[408]";
  cout << " DCM[409]";
  cout << " DCM[410]";
  cout << " DCM[411]";
  cout << " DCM[412]";
  cout << " DCM[413]";
  cout << " DCM[414]";
  cout << " DCM[415]";
  cout << " DCM[416]";
  cout << " DCM[417]";
  cout << " DCM[418]";
  cout << " DCM[419]";
  cout << " DCM[420]";
  cout << " DCM[421]";
  cout << " DCM[422]";
  cout << " DCM[423]";
  cout << " DCM[424]";
  cout << " DCM[425]";
  cout << " DCM[426]";
  cout << " DCM[427]";
  cout << " DCM[428]";
  cout << " DCM[429]";
  cout << " DCM[430]";
  cout << " DCM[431]";
  cout << " DCM[432]";
  cout << " DCM[433]";
  cout << " DCM[434]";
  cout << " DCM[435]";
  cout << " DCM[436]";
  cout << " DCM[437]";
  cout << " DCM[438]";
  cout << " DCM[439]";
  cout << " DCM[440]";
  cout << " DCM[441]";
  cout << " DCM[442]";
  cout << " DCM[443]";
  cout << " DCM[444]";
  cout << " DCM[445]";
  cout << " DCM[446]";
  cout << " DCM[447]";
  cout << " DCM[448]";
  cout << " DCM[449]";
  cout << " DCM[450]";
  cout << " DCM[451]";
  cout << " DCM[452]";
  cout << " DCM[453]";
  cout << " DCM[454]";
  cout << " DCM[455]";
  cout << " DCM[456]";
  cout << " DCM[457]";
  cout << " DCM[458]";
  cout << " DCM[459]";
  cout << " DCM[460]";
  cout << " DCM[461]";
  cout << " DCM[462]";
  cout << " DCM[463]";
  cout << " DCM[464]";
  cout << " DCM[465]";
  cout << " DCM[466]";
  cout << " DCM[467]";
  cout << " DCM[468]";
  cout << " DCM[469]";
  cout << " DCM[470]";
  cout << " DCM[471]";
  cout << " DCM[472]";
  cout << " DCM[473]";
  cout << " DCM[474]";
  cout << " DCM[475]";
  cout << " DCM[476]";
  cout << " DCM[477]";
  cout << " DCM[478]";
  cout << " DCM[479]";
  cout << " DCM[480]";
  cout << " DCM[481]";
  cout << " DCM[482]";
  cout << " DCM[483]";
  cout << " DCM[484]";
  cout << " DCM[485]";
  cout << " DCM[486]";
  cout << " DCM[487]";
  cout << " DCM[488]";
  cout << " DCM[489]";
  cout << " DCM[490]";
  cout << " DCM[491]";
  cout << " DCM[492]";
  cout << " DCM[493]";
  cout << " DCM[494]";
  cout << " DCM[495]";
  cout << " DCM[496]";
  cout << " DCM[497]";
  cout << " DCM[498]";
  cout << " DCM[499]";
  cout << " DCM[500]";
  cout << " DCM[501]";
  cout << " DCM[502]";
  cout << " DCM[503]";
  cout << " DCM[504]";
  cout << " DCM[505]";
  cout << " DCM[506]";
  cout << " DCM[507]";
  cout << " DCM[508]";
  cout << " DCM[509]";
  cout << " DCM[510]";
  cout << " DCM[511]";
  cout << " DCM[512]";
  cout << " DCM[513]";
  cout << " DCM[514]";
  cout << " DCM[515]";
  cout << " DCM[516]";
  cout << " DCM[517]";
  cout << " DCM[518]";
  cout << " DCM[519]";
  cout << " DCM[520]";
  cout << " DCM[521]";
  cout << " DCM[522]";
  cout << " DCM[523]";
  cout << " DCM[524]";
  cout << " DCM[525]";
  cout << " DCM[526]";
  cout << " DCM[527]";
  cout << " DCM[528]";
  cout << " DCM[529]";
  cout << " DCM[530]";
  cout << " DCM[531]";
  cout << " DCM[532]";
  cout << " DCM[533]";
  cout << " DCM[534]";
  cout << " DCM[535]";
  cout << " DCM[536]";
  cout << " DCM[537]";
  cout << " DCM[538]";
  cout << " DCM[539]";
  cout << " DCM[540]";
  cout << " DCM[541]";
  cout << " DCM[542]";
  cout << " DCM[543]";
  cout << " DCM[544]";
  cout << " DCM[545]";
  cout << " DCM[546]";
  cout << " DCM[547]";
  cout << " DCM[548]";
  cout << " DCM[549]";
  cout << " DCM[550]";
  cout << " DCM[551]";
  cout << " DCM[552]";
  cout << " DCM[553]";
  cout << " DCM[554]";
  cout << " DCM[555]";
  cout << " DCM[556]";
  cout << " DCM[557]";
  cout << " DCM[558]";
  cout << " DCM[559]";
  cout << " DCM[560]";
  cout << " DCM[561]";
  cout << " DCM[562]";
  cout << " DCM[563]";
  cout << " DCM[564]";
  cout << " DCM[565]";
  cout << " DCM[566]";
  cout << " DCM[567]";
  cout << " DCM[568]";
  cout << " DCM[569]";
  cout << " DCM[570]";
  cout << " DCM[571]";
  cout << " DCM[572]";
  cout << " DCM[573]";
  cout << " DCM[574]";
  cout << " DCM[575]";
  cout << " DCM[576]";
  cout << " DCM[577]";
  cout << " DCM[578]";
  cout << " DCM[579]";
  cout << " DCM[580]";
  cout << " DCM[581]";
  cout << " DCM[582]";
  cout << " DCM[583]";
  cout << " DCM[584]";
  cout << " DCM[585]";
  cout << " DCM[586]";
  cout << " DCM[587]";
  cout << " DCM[588]";
  cout << " DCM[589]";
  cout << " DCM[590]";
  cout << " DCM[591]";
  cout << " DCM[592]";
  cout << " DCM[593]";
  cout << " DCM[594]";
  cout << " DCM[595]";
  cout << " DCM[596]";
  cout << " DCM[597]";
  cout << " DCM[598]";
  cout << " DCM[599]";
  cout << " DCM[600]";
  cout << " DCM[601]";
  cout << " DCM[602]";
  cout << " DCM[603]";
  cout << " DCM[604]";
  cout << " DCM[605]";
  cout << " DCM[606]";
  cout << " DCM[607]";
  cout << " DCM[608]";
  cout << " DCM[609]";
  cout << " DCM[610]";
  cout << " DCM[611]";
  cout << " DCM[612]";
  cout << " DCM[613]";
  cout << " DCM[614]";
  cout << " DCM[615]";
  cout << " DCM[616]";
  cout << " DCM[617]";
  cout << " DCM[618]";
  cout << " DCM[619]";
  cout << " DCM[620]";
  cout << " DCM[621]";
  cout << " DCM[622]";
  cout << " DCM[623]";
  cout << " DCM[624]";
  cout << " DCM[625]";
  cout << " DCM[626]";
  cout << " DCM[627]";
  cout << " DCM[628]";
  cout << " DCM[629]";
  cout << " DCM[630]";
  cout << " DCM[631]";
  cout << " DCM[632]";
  cout << " DCM[633]";
  cout << " DCM[634]";
  cout << " DCM[635]";
  cout << " DCM[636]";
  cout << " DCM[637]";
  cout << " DCM[638]";
  cout << " DCM[639]";
  cout << " DCM[640]";
  cout << " DCM[641]";
  cout << " DCM[642]";
  cout << " DCM[643]";
  cout << " DCM[644]";
  cout << " DCM[645]";
  cout << " DCM[646]";
  cout << " DCM[647]";
  cout << " DCM[648]";
  cout << " DCM[649]";
  cout << " DCM[650]";
  cout << " DCM[651]";
  cout << " DCM[652]";
  cout << " DCM[653]";
  cout << " DCM[654]";
  cout << " DCM[655]";
  cout << " DCM[656]";
  cout << " DCM[657]";
  cout << " DCM[658]";
  cout << " DCM[659]";
  cout << " DCM[660]";
  cout << " DCM[661]";
  cout << " DCM[662]";
  cout << " DCM[663]";
  cout << " DCM[664]";
  cout << " DCM[665]";
  cout << " DCM[666]";
  cout << " DCM[667]";
  cout << " DCM[668]";
  cout << " DCM[669]";
  cout << " DCM[670]";
  cout << " DCM[671]";
  cout << " DCM[672]";
  cout << " DCM[673]";
  cout << " DCM[674]";
  cout << " DCM[675]";
  cout << " DCM[676]";
  cout << " DCM[677]";
  cout << " DCM[678]";
  cout << " DCM[679]";
  cout << " DCM[680]";
  cout << " DCM[681]";
  cout << " DCM[682]";
  cout << " DCM[683]";
  cout << " DCM[684]";
  cout << " DCM[685]";
  cout << " DCM[686]";
  cout << " DCM[687]";
  cout << " DCM[688]";
  cout << " DCM[689]";
  cout << " DCM[690]";
  cout << " DCM[691]";
  cout << " DCM[692]";
  cout << " DCM[693]";
  cout << " DCM[694]";
  cout << " DCM[695]";
  cout << " DCM[696]";
  cout << " DCM[697]";
  cout << " DCM[698]";
  cout << " DCM[699]";
  cout << " DCM[700]";
  cout << " DCM[701]";
  cout << " DCM[702]";
  cout << " DCM[703]";
  cout << " DCM[704]";
  cout << " DCM[705]";
  cout << " DCM[706]";
  cout << " DCM[707]";
  cout << " DCM[708]";
  cout << " DCM[709]";
  cout << " DCM[710]";
  cout << " DCM[711]";
  cout << " DCM[712]";
  cout << " DCM[713]";
  cout << " DCM[714]";
  cout << " DCM[715]";
  cout << " DCM[716]";
  cout << " DCM[717]";
  cout << " DCM[718]";
  cout << " DCM[719]";
  cout << " DCM[720]";
  cout << " DCM[721]";
  cout << " DCM[722]";
  cout << " DCM[723]";
  cout << " DCM[724]";
  cout << " DCM[725]";
  cout << " DCM[726]";
  cout << " DCM[727]";
  cout << " DCM[728]";
  cout << " DCM[729]";
  cout << " DCM[730]";
  cout << " DCM[731]";
  cout << " DCM[732]";
  cout << " DCM[733]";
  cout << " DCM[734]";
  cout << " DCM[735]";
  cout << " DCM[736]";
  cout << " DCM[737]";
  cout << " DCM[738]";
  cout << " DCM[739]";
  cout << " DCM[740]";
  cout << " DCM[741]";
  cout << " DCM[742]";
  cout << " DCM[743]";
  cout << " DCM[744]";
  cout << " DCM[745]";
  cout << " DCM[746]";
  cout << " DCM[747]";
  cout << " DCM[748]";
  cout << " DCM[749]";
  cout << " DCM[750]";
  cout << " DCM[751]";
  cout << " DCM[752]";
  cout << " DCM[753]";
  cout << " DCM[754]";
  cout << " DCM[755]";
  cout << " DCM[756]";
  cout << " DCM[757]";
  cout << " DCM[758]";
  cout << " DCM[759]";
  cout << " DCM[760]";
  cout << " DCM[761]";
  cout << " DCM[762]";
  cout << " DCM[763]";
  cout << " DCM[764]";
  cout << " DCM[765]";
  cout << " DCM[766]";
  cout << " DCM[767]";
  cout << " DCM[768]";
  cout << " DCM[769]";
  cout << " DCM[770]";
  cout << " DCM[771]";
  cout << " DCM[772]";
  cout << " DCM[773]";
  cout << " DCM[774]";
  cout << " DCM[775]";
  cout << " DCM[776]";
  cout << " DCM[777]";
  cout << " DCM[778]";
  cout << " DCM[779]";
  cout << " DCM[780]";
  cout << " DCM[781]";
  cout << " DCM[782]";
  cout << " DCM[783]";
  cout << " DCM[784]";
  cout << " DCM[785]";
  cout << " DCM[786]";
  cout << " DCM[787]";
  cout << " DCM[788]";
  cout << " DCM[789]";
  cout << " DCM[790]";
  cout << " DCM[791]";
  cout << " DCM[792]";
  cout << " DCM[793]";
  cout << " DCM[794]";
  cout << " DCM[795]";
  cout << " DCM[796]";
  cout << " DCM[797]";
  cout << " DCM[798]";
  cout << " DCM[799]";
  cout << " DCM[800]";
  cout << " DCM[801]";
  cout << " DCM[802]";
  cout << " DCM[803]";
  cout << " DCM[804]";
  cout << " DCM[805]";
  cout << " DCM[806]";
  cout << " DCM[807]";
  cout << " DCM[808]";
  cout << " DCM[809]";
  cout << " DCM[810]";
  cout << " DCM[811]";
  cout << " DCM[812]";
  cout << " DCM[813]";
  cout << " DCM[814]";
  cout << " DCM[815]";
  cout << " DCM[816]";
  cout << " DCM[817]";
  cout << " DCM[818]";
  cout << " DCM[819]";
  cout << " DCM[820]";
  cout << " DCM[821]";
  cout << " DCM[822]";
  cout << " DCM[823]";
  cout << " DCM[824]";
  cout << " DCM[825]";
  cout << " DCM[826]";
  cout << " DCM[827]";
  cout << " DCM[828]";
  cout << " DCM[829]";
  cout << " DCM[830]";
  cout << " DCM[831]";
  cout << " DCM[832]";
  cout << " DCM[833]";
  cout << " DCM[834]";
  cout << " DCM[835]";
  cout << " DCM[836]";
  cout << " DCM[837]";
  cout << " DCM[838]";
  cout << " DCM[839]";
  cout << " DCM[840]";
  cout << " DCM[841]";
  cout << " DCM[842]";
  cout << " DCM[843]";
  cout << " DCM[844]";
  cout << " DCM[845]";
  cout << " DCM[846]";
  cout << " DCM[847]";
  cout << " DCM[848]";
  cout << " DCM[849]";
  cout << " DCM[850]";
  cout << " DCM[851]";
  cout << " DCM[852]";
  cout << " DCM[853]";
  cout << " DCM[854]";
  cout << " DCM[855]";
  cout << " DCM[856]";
  cout << " DCM[857]";
  cout << " DCM[858]";
  cout << " DCM[859]";
  cout << " DCM[860]";
  cout << " DCM[861]";
  cout << " DCM[862]";
  cout << " DCM[863]";
  cout << " DCM[864]";
  cout << " DCM[865]";
  cout << " DCM[866]";
  cout << " DCM[867]";
  cout << " DCM[868]";
  cout << " DCM[869]";
  cout << " DCM[870]";
  cout << " DCM[871]";
  cout << " DCM[872]";
  cout << " DCM[873]";
  cout << " DCM[874]";
  cout << " DCM[875]";
  cout << " DCM[876]";
  cout << " DCM[877]";
  cout << " DCM[878]";
  cout << " DCM[879]";
  cout << " DCM[880]";
  cout << " DCM[881]";
  cout << " DCM[882]";
  cout << " DCM[883]";
  cout << " DCM[884]";
  cout << " DCM[885]";
  cout << " DCM[886]";
  cout << " DCM[887]";
  cout << " DCM[888]";
  cout << " DCM[889]";
  cout << " DCM[890]";
  cout << " DCM[891]";
  cout << " DCM[892]";
  cout << " DCM[893]";
  cout << " DCM[894]";
  cout << " DCM[895]";
  cout << " DCM[896]";
  cout << " DCM[897]";
  cout << " DCM[898]";
  cout << " DCM[899]";
  cout << " DCM[900]";
  cout << " DCM[901]";
  cout << " DCM[902]";
  cout << " DCM[903]";
  cout << " DCM[904]";
  cout << " DCM[905]";
  cout << " DCM[906]";
  cout << " DCM[907]";
  cout << " DCM[908]";
  cout << " DCM[909]";
  cout << " DCM[910]";
  cout << " DCM[911]";
  cout << " DCM[912]";
  cout << " DCM[913]";
  cout << " DCM[914]";
  cout << " DCM[915]";
  cout << " DCM[916]";
  cout << " DCM[917]";
  cout << " DCM[918]";
  cout << " DCM[919]";
  cout << " DCM[920]";
  cout << " DCM[921]";
  cout << " DCM[922]";
  cout << " DCM[923]";
  cout << " DCM[924]";
  cout << " DCM[925]";
  cout << " DCM[926]";
  cout << " DCM[927]";
  cout << " DCM[928]";
  cout << " DCM[929]";
  cout << " DCM[930]";
  cout << " DCM[931]";
  cout << " DCM[932]";
  cout << " DCM[933]";
  cout << " DCM[934]";
  cout << " DCM[935]";
  cout << " DCM[936]";
  cout << " DCM[937]";
  cout << " DCM[938]";
  cout << " DCM[939]";
  cout << " DCM[940]";
  cout << " DCM[941]";
  cout << " DCM[942]";
  cout << " DCM[943]";
  cout << " DCM[944]";
  cout << " DCM[945]";
  cout << " DCM[946]";
  cout << " DCM[947]";
  cout << " DCM[948]";
  cout << " DCM[949]";
  cout << " DCM[950]";
  cout << " DCM[951]";
  cout << " DCM[952]";
  cout << " DCM[953]";
  cout << " DCM[954]";
  cout << " DCM[955]";
  cout << " DCM[956]";
  cout << " DCM[957]";
  cout << " DCM[958]";
  cout << " DCM[959]";
  cout << " DCM[960]";
  cout << " DCM[961]";
  cout << " DCM[962]";
  cout << " DCM[963]";
  cout << " DCM[964]";
  cout << " DCM[965]";
  cout << " DCM[966]";
  cout << " DCM[967]";
  cout << " DCM[968]";
  cout << " DCM[969]";
  cout << " DCM[970]";
  cout << " DCM[971]";
  cout << " DCM[972]";
  cout << " DCM[973]";
  cout << " DCM[974]";
  cout << " DCM[975]";
  cout << " DCM[976]";
  cout << " DCM[977]";
  cout << " DCM[978]";
  cout << " DCM[979]";
  cout << " DCM[980]";
  cout << " DCM[981]";
  cout << " DCM[982]";
  cout << " DCM[983]";
  cout << " DCM[984]";
  cout << " DCM[985]";
  cout << " DCM[986]";
  cout << " DCM[987]";
  cout << " DCM[988]";
  cout << " DCM[989]";
  cout << " DCM[990]";
  cout << " DCM[991]";
  cout << " DCM[992]";
  cout << " DCM[993]";
  cout << " DCM[994]";
  cout << " DCM[995]";
  cout << " DCM[996]";
  cout << " DCM[997]";
  cout << " DCM[998]";
  cout << " DCM[999]";
  cout << " DCM[1000]";
  cout << " DCM[1001]";
  cout << " DCM[1002]";
  cout << " DCM[1003]";
  cout << " DCM[1004]";
  cout << " DCM[1005]";
  cout << " DCM[1006]";
  cout << " DCM[1007]";
  cout << " DCM[1008]";
  cout << " DCM[1009]";
  cout << " DCM[1010]";
  cout << " DCM[1011]";
  cout << " DCM[1012]";
  cout << " DCM[1013]";
  cout << " DCM[1014]";
  cout << " DCM[1015]";
  cout << " DCM[1016]";
  cout << " DCM[1017]";
  cout << " DCM[1018]";
  cout << " DCM[1019]";
  cout << " DCM[1020]";
  cout << " DCM[1021]";
  cout << " DCM[1022]";
  cout << " DCM[1023]";
  cout << " DCM[1024]";
  cout << " DCM[1025]";
  cout << " DCM[1026]";
  cout << " DCM[1027]";
  cout << " DCM[1028]";
  cout << " DCM[1029]";
  cout << " DCM[1030]";
  cout << " DCM[1031]";
  cout << " DCM[1032]";
  cout << " DCM[1033]";
  cout << " DCM[1034]";
  cout << " DCM[1035]";
  cout << " DCM[1036]";
  cout << " DCM[1037]";
  cout << " DCM[1038]";
  cout << " DCM[1039]";
  cout << " DCM[1040]";
  cout << " DCM[1041]";
  cout << " DCM[1042]";
  cout << " DCM[1043]";
  cout << " DCM[1044]";
  cout << " DCM[1045]";
  cout << " DCM[1046]";
  cout << " DCM[1047]";
  cout << " DCM[1048]";
  cout << " DCM[1049]";
  cout << " DCM[1050]";
  cout << " DCM[1051]";
  cout << " DCM[1052]";
  cout << " DCM[1053]";
  cout << " DCM[1054]";
  cout << " DCM[1055]";
  cout << " DCM[1056]";
  cout << " DCM[1057]";
  cout << " DCM[1058]";
  cout << " DCM[1059]";
  cout << " DCM[1060]";
  cout << " DCM[1061]";
  cout << " DCM[1062]";
  cout << " DCM[1063]";
  cout << " DCM[1064]";
  cout << " DCM[1065]";
  cout << " DCM[1066]";
  cout << " DCM[1067]";
  cout << " DCM[1068]";
  cout << " DCM[1069]";
  cout << " DCM[1070]";
  cout << " DCM[1071]";
  cout << " DCM[1072]";
  cout << " DCM[1073]";
  cout << " DCM[1074]";
  cout << " DCM[1075]";
  cout << " DCM[1076]";
  cout << " DCM[1077]";
  cout << " DCM[1078]";
  cout << " DCM[1079]";
  cout << " DCM[1080]";
  cout << " DCM[1081]";
  cout << " DCM[1082]";
  cout << " DCM[1083]";
  cout << " DCM[1084]";
  cout << " DCM[1085]";
  cout << " DCM[1086]";
  cout << " DCM[1087]";
  cout << " DCM[1088]";
  cout << " DCM[1089]";
  cout << " DCM[1090]";
  cout << " DCM[1091]";
  cout << " DCM[1092]";
  cout << " DCM[1093]";
  cout << " DCM[1094]";
  cout << " DCM[1095]";
  cout << " DCM[1096]";
  cout << " DCM[1097]";
  cout << " DCM[1098]";
  cout << " DCM[1099]";
  cout << " DCM[1100]";
  cout << " DCM[1101]";
  cout << " DCM[1102]";
  cout << " DCM[1103]";
  cout << " DCM[1104]";
  cout << " DCM[1105]";
  cout << " DCM[1106]";
  cout << " DCM[1107]";
  cout << " DCM[1108]";
  cout << " DCM[1109]";
  cout << " DCM[1110]";
  cout << " DCM[1111]";
  cout << " DCM[1112]";
  cout << " DCM[1113]";
  cout << " DCM[1114]";
  cout << " DCM[1115]";
  cout << " DCM[1116]";
  cout << " DCM[1117]";
  cout << " DCM[1118]";
  cout << " DCM[1119]";
  cout << " DCM[1120]";
  cout << " DCM[1121]";
  cout << " DCM[1122]";
  cout << " DCM[1123]";
  cout << " DCM[1124]";
  cout << " DCM[1125]";
  cout << " DCM[1126]";
  cout << " DCM[1127]";
  cout << " DCM[1128]";
  cout << " DCM[1129]";
  cout << " DCM[1130]";
  cout << " DCM[1131]";
  cout << " DCM[1132]";
  cout << " DCM[1133]";
  cout << " DCM[1134]";
  cout << " DCM[1135]";
  cout << " DCM[1136]";
  cout << " DCM[1137]";
  cout << " DCM[1138]";
  cout << " DCM[1139]";
  cout << " DCM[1140]";
  cout << " DCM[1141]";
  cout << " DCM[1142]";
  cout << " DCM[1143]";
  cout << endl;

  size_t last_row = RowCount();
  if (last_row > first_row+num_rows) last_row = first_row+num_rows;
  for (size_t row = first_row; row < last_row; row++) {
     cout << setw(7) << row << ":";

     cout << " " << setw(11) << fTableData[row].nWord;
     cout << " " << setw(11) << fTableData[row].scheme;
     cout << " " << setw(11) << fTableData[row].packetID;
     for(int i0=0; i0<1144; i0++) {
        cout << " " << setw(11) << fTableData[row].DCM[i0];
     }

     cout << endl;
  }

}

void
dTofDCMWrapper::Print(Option_t* option) const
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
dTofDCMWrapper::SetMaxRowCount(const size_t& max_rows)
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
     DTOFDCM_ST* newData = new DTOFDCM_ST[max_rows];
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
dTofDCMWrapper::SetRowCount(const size_t& n)
{
  if (n > (size_t) fTableHeader->maxlen) {
     fTableHeader->nok = fTableHeader->maxlen;
  }
  else if (n >= 0) {
     fTableHeader->nok = n;
  }
}

void
dTofDCMWrapper::SetRowSize(const size_t& row_size)
{
  if (row_size > 0) {
     fTableHeader->rbytes = row_size;
  }
}

void
dTofDCMWrapper::Streamer(TBuffer& b)
{
   // Stream an object of class dTofDCMWrapper.
   // What should be done on output if the table is empty?

   if (b.IsReading()) {
     b.ReadVersion();
     PHTable::Streamer(b);         // Read the table header.
     if (RowSize() != sizeof(DTOFDCM_ST)) {
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
     fTableData = new DTOFDCM_ST[max_rows];
     fTableHeader->data_pointer = (long)fTableData;

     SetMaxRowCount(max_rows);
     SetType("dTofDCMWrapper");

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
        b.WriteArray(fTableData[i].DCM,1144);
     }
   }

}
/* Automatically generated.  Do not edit. */
