#include <cstring>
#include <iostream>
#include <iomanip>
#include "dTofFEMWrapper.h"

ClassImp(dTofFEMWrapper);

using namespace std;

dTofFEMWrapper::dTofFEMWrapper(const char* name, const size_t& max_rows)
  : PHTable(name,max_rows)
{
  size_t rowSize = sizeof(DTOFFEM_ST);
  if (max_rows > 0) { // Avoid allocating a space of zero size!
     fTableData = new DTOFFEM_ST[max_rows];
     SetMaxRowCount(max_rows);
  }
  else {
     fTableData = new DTOFFEM_ST[1];
     SetMaxRowCount(1);
  }

  SetRowSize(rowSize);
  SetType("dTofFEM");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
}

dTofFEMWrapper::dTofFEMWrapper(const dTofFEMWrapper& source)
  : PHTable(source)
{
  // The row count, the max. row count, and the name are
  // already set in the PHTable copy ctor.

  fTableData = new DTOFFEM_ST[source.MaxRowCount()];
  SetRowSize(sizeof(DTOFFEM_ST));
  SetType("dTofFEM");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
  
  for (size_t row = 0; row < RowCount(); row++) {
     fTableData[row] = source.fTableData[row];
  }

}

void*
dTofFEMWrapper::RawTableData()
{
  return static_cast<void*>(fTableData);
}

dTofFEMWrapper&
dTofFEMWrapper::operator=(const dTofFEMWrapper& source)
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

dTofFEMWrapper::~dTofFEMWrapper()
{
  delete [] fTableData;
}

DTOFFEM_ST*
dTofFEMWrapper::TableData()
{
  return fTableData;
}

DTOFFEM_ST&
dTofFEMWrapper::operator[](const size_t& row)
{
  return fTableData[row];
}

const DTOFFEM_ST&
dTofFEMWrapper::operator[](const size_t& row) const
{
  return fTableData[row];
}

void
dTofFEMWrapper::Print(const size_t num_rows, const size_t first_row) const
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
  cout << " Word[400]";
  cout << " Word[401]";
  cout << " Word[402]";
  cout << " Word[403]";
  cout << " Word[404]";
  cout << " Word[405]";
  cout << " Word[406]";
  cout << " Word[407]";
  cout << " Word[408]";
  cout << " Word[409]";
  cout << " Word[410]";
  cout << " Word[411]";
  cout << " Word[412]";
  cout << " Word[413]";
  cout << " Word[414]";
  cout << " Word[415]";
  cout << " Word[416]";
  cout << " Word[417]";
  cout << " Word[418]";
  cout << " Word[419]";
  cout << " Word[420]";
  cout << " Word[421]";
  cout << " Word[422]";
  cout << " Word[423]";
  cout << " Word[424]";
  cout << " Word[425]";
  cout << " Word[426]";
  cout << " Word[427]";
  cout << " Word[428]";
  cout << " Word[429]";
  cout << " Word[430]";
  cout << " Word[431]";
  cout << " Word[432]";
  cout << " Word[433]";
  cout << " Word[434]";
  cout << " Word[435]";
  cout << " Word[436]";
  cout << " Word[437]";
  cout << " Word[438]";
  cout << " Word[439]";
  cout << " Word[440]";
  cout << " Word[441]";
  cout << " Word[442]";
  cout << " Word[443]";
  cout << " Word[444]";
  cout << " Word[445]";
  cout << " Word[446]";
  cout << " Word[447]";
  cout << " Word[448]";
  cout << " Word[449]";
  cout << " Word[450]";
  cout << " Word[451]";
  cout << " Word[452]";
  cout << " Word[453]";
  cout << " Word[454]";
  cout << " Word[455]";
  cout << " Word[456]";
  cout << " Word[457]";
  cout << " Word[458]";
  cout << " Word[459]";
  cout << " Word[460]";
  cout << " Word[461]";
  cout << " Word[462]";
  cout << " Word[463]";
  cout << " Word[464]";
  cout << " Word[465]";
  cout << " Word[466]";
  cout << " Word[467]";
  cout << " Word[468]";
  cout << " Word[469]";
  cout << " Word[470]";
  cout << " Word[471]";
  cout << " Word[472]";
  cout << " Word[473]";
  cout << " Word[474]";
  cout << " Word[475]";
  cout << " Word[476]";
  cout << " Word[477]";
  cout << " Word[478]";
  cout << " Word[479]";
  cout << " Word[480]";
  cout << " Word[481]";
  cout << " Word[482]";
  cout << " Word[483]";
  cout << " Word[484]";
  cout << " Word[485]";
  cout << " Word[486]";
  cout << " Word[487]";
  cout << " Word[488]";
  cout << " Word[489]";
  cout << " Word[490]";
  cout << " Word[491]";
  cout << " Word[492]";
  cout << " Word[493]";
  cout << " Word[494]";
  cout << " Word[495]";
  cout << " Word[496]";
  cout << " Word[497]";
  cout << " Word[498]";
  cout << " Word[499]";
  cout << " Word[500]";
  cout << " Word[501]";
  cout << " Word[502]";
  cout << " Word[503]";
  cout << " Word[504]";
  cout << " Word[505]";
  cout << " Word[506]";
  cout << " Word[507]";
  cout << " Word[508]";
  cout << " Word[509]";
  cout << " Word[510]";
  cout << " Word[511]";
  cout << " Word[512]";
  cout << " Word[513]";
  cout << " Word[514]";
  cout << " Word[515]";
  cout << " Word[516]";
  cout << " Word[517]";
  cout << " Word[518]";
  cout << " Word[519]";
  cout << " Word[520]";
  cout << " Word[521]";
  cout << " Word[522]";
  cout << " Word[523]";
  cout << " Word[524]";
  cout << " Word[525]";
  cout << " Word[526]";
  cout << " Word[527]";
  cout << " Word[528]";
  cout << " Word[529]";
  cout << " Word[530]";
  cout << " Word[531]";
  cout << " Word[532]";
  cout << " Word[533]";
  cout << " Word[534]";
  cout << " Word[535]";
  cout << " Word[536]";
  cout << " Word[537]";
  cout << " Word[538]";
  cout << " Word[539]";
  cout << " Word[540]";
  cout << " Word[541]";
  cout << " Word[542]";
  cout << " Word[543]";
  cout << " Word[544]";
  cout << " Word[545]";
  cout << " Word[546]";
  cout << " Word[547]";
  cout << " Word[548]";
  cout << " Word[549]";
  cout << " Word[550]";
  cout << " Word[551]";
  cout << " Word[552]";
  cout << " Word[553]";
  cout << " Word[554]";
  cout << " Word[555]";
  cout << " Word[556]";
  cout << " Word[557]";
  cout << " Word[558]";
  cout << " Word[559]";
  cout << " Word[560]";
  cout << " Word[561]";
  cout << " Word[562]";
  cout << " Word[563]";
  cout << " Word[564]";
  cout << " Word[565]";
  cout << " Word[566]";
  cout << " Word[567]";
  cout << " Word[568]";
  cout << " Word[569]";
  cout << " Word[570]";
  cout << " Word[571]";
  cout << " Word[572]";
  cout << " Word[573]";
  cout << " Word[574]";
  cout << " Word[575]";
  cout << " Word[576]";
  cout << " Word[577]";
  cout << " Word[578]";
  cout << " Word[579]";
  cout << " Word[580]";
  cout << " Word[581]";
  cout << " Word[582]";
  cout << " Word[583]";
  cout << " Word[584]";
  cout << " Word[585]";
  cout << " Word[586]";
  cout << " Word[587]";
  cout << " Word[588]";
  cout << " Word[589]";
  cout << " Word[590]";
  cout << " Word[591]";
  cout << " Word[592]";
  cout << " Word[593]";
  cout << " Word[594]";
  cout << " Word[595]";
  cout << " Word[596]";
  cout << " Word[597]";
  cout << " Word[598]";
  cout << " Word[599]";
  cout << " Word[600]";
  cout << " Word[601]";
  cout << " Word[602]";
  cout << " Word[603]";
  cout << " Word[604]";
  cout << " Word[605]";
  cout << " Word[606]";
  cout << " Word[607]";
  cout << " Word[608]";
  cout << " Word[609]";
  cout << " Word[610]";
  cout << " Word[611]";
  cout << " Word[612]";
  cout << " Word[613]";
  cout << " Word[614]";
  cout << " Word[615]";
  cout << " Word[616]";
  cout << " Word[617]";
  cout << " Word[618]";
  cout << " Word[619]";
  cout << " Word[620]";
  cout << " Word[621]";
  cout << " Word[622]";
  cout << " Word[623]";
  cout << " Word[624]";
  cout << " Word[625]";
  cout << " Word[626]";
  cout << " Word[627]";
  cout << " Word[628]";
  cout << " Word[629]";
  cout << " Word[630]";
  cout << " Word[631]";
  cout << " Word[632]";
  cout << " Word[633]";
  cout << " Word[634]";
  cout << " Word[635]";
  cout << " Word[636]";
  cout << " Word[637]";
  cout << " Word[638]";
  cout << " Word[639]";
  cout << " Word[640]";
  cout << " Word[641]";
  cout << " Word[642]";
  cout << " Word[643]";
  cout << " Word[644]";
  cout << " Word[645]";
  cout << " Word[646]";
  cout << " Word[647]";
  cout << " Word[648]";
  cout << " Word[649]";
  cout << " Word[650]";
  cout << " Word[651]";
  cout << " Word[652]";
  cout << " Word[653]";
  cout << " Word[654]";
  cout << " Word[655]";
  cout << " Word[656]";
  cout << " Word[657]";
  cout << " Word[658]";
  cout << " Word[659]";
  cout << " Word[660]";
  cout << " Word[661]";
  cout << " Word[662]";
  cout << " Word[663]";
  cout << " Word[664]";
  cout << " Word[665]";
  cout << " Word[666]";
  cout << " Word[667]";
  cout << " Word[668]";
  cout << " Word[669]";
  cout << " Word[670]";
  cout << " Word[671]";
  cout << " Word[672]";
  cout << " Word[673]";
  cout << " Word[674]";
  cout << " Word[675]";
  cout << " Word[676]";
  cout << " Word[677]";
  cout << " Word[678]";
  cout << " Word[679]";
  cout << " Word[680]";
  cout << " Word[681]";
  cout << " Word[682]";
  cout << " Word[683]";
  cout << " Word[684]";
  cout << " Word[685]";
  cout << " Word[686]";
  cout << " Word[687]";
  cout << " Word[688]";
  cout << " Word[689]";
  cout << " Word[690]";
  cout << " Word[691]";
  cout << " Word[692]";
  cout << " Word[693]";
  cout << " Word[694]";
  cout << " Word[695]";
  cout << " Word[696]";
  cout << " Word[697]";
  cout << " Word[698]";
  cout << " Word[699]";
  cout << " Word[700]";
  cout << " Word[701]";
  cout << " Word[702]";
  cout << " Word[703]";
  cout << " Word[704]";
  cout << " Word[705]";
  cout << " Word[706]";
  cout << " Word[707]";
  cout << " Word[708]";
  cout << " Word[709]";
  cout << " Word[710]";
  cout << " Word[711]";
  cout << " Word[712]";
  cout << " Word[713]";
  cout << " Word[714]";
  cout << " Word[715]";
  cout << " Word[716]";
  cout << " Word[717]";
  cout << " Word[718]";
  cout << " Word[719]";
  cout << " Word[720]";
  cout << " Word[721]";
  cout << " Word[722]";
  cout << " Word[723]";
  cout << " Word[724]";
  cout << " Word[725]";
  cout << " Word[726]";
  cout << " Word[727]";
  cout << " Word[728]";
  cout << " Word[729]";
  cout << " Word[730]";
  cout << " Word[731]";
  cout << " Word[732]";
  cout << " Word[733]";
  cout << " Word[734]";
  cout << " Word[735]";
  cout << " Word[736]";
  cout << " Word[737]";
  cout << " Word[738]";
  cout << " Word[739]";
  cout << " Word[740]";
  cout << " Word[741]";
  cout << " Word[742]";
  cout << " Word[743]";
  cout << " Word[744]";
  cout << " Word[745]";
  cout << " Word[746]";
  cout << " Word[747]";
  cout << " Word[748]";
  cout << " Word[749]";
  cout << " Word[750]";
  cout << " Word[751]";
  cout << " Word[752]";
  cout << " Word[753]";
  cout << " Word[754]";
  cout << " Word[755]";
  cout << " Word[756]";
  cout << " Word[757]";
  cout << " Word[758]";
  cout << " Word[759]";
  cout << " Word[760]";
  cout << " Word[761]";
  cout << " Word[762]";
  cout << " Word[763]";
  cout << " Word[764]";
  cout << " Word[765]";
  cout << " Word[766]";
  cout << " Word[767]";
  cout << " Word[768]";
  cout << " Word[769]";
  cout << " Word[770]";
  cout << " Word[771]";
  cout << " Word[772]";
  cout << " Word[773]";
  cout << " Word[774]";
  cout << " Word[775]";
  cout << " Word[776]";
  cout << " Word[777]";
  cout << " Word[778]";
  cout << " Word[779]";
  cout << " Word[780]";
  cout << " Word[781]";
  cout << " Word[782]";
  cout << " Word[783]";
  cout << " Word[784]";
  cout << " Word[785]";
  cout << " Word[786]";
  cout << " Word[787]";
  cout << " Word[788]";
  cout << " Word[789]";
  cout << " Word[790]";
  cout << " Word[791]";
  cout << " Word[792]";
  cout << " Word[793]";
  cout << " Word[794]";
  cout << " Word[795]";
  cout << " Word[796]";
  cout << " Word[797]";
  cout << " Word[798]";
  cout << " Word[799]";
  cout << " Word[800]";
  cout << " Word[801]";
  cout << " Word[802]";
  cout << " Word[803]";
  cout << " Word[804]";
  cout << " Word[805]";
  cout << " Word[806]";
  cout << " Word[807]";
  cout << " Word[808]";
  cout << " Word[809]";
  cout << " Word[810]";
  cout << " Word[811]";
  cout << " Word[812]";
  cout << " Word[813]";
  cout << " Word[814]";
  cout << " Word[815]";
  cout << " Word[816]";
  cout << " Word[817]";
  cout << " Word[818]";
  cout << " Word[819]";
  cout << " Word[820]";
  cout << " Word[821]";
  cout << " Word[822]";
  cout << " Word[823]";
  cout << " Word[824]";
  cout << " Word[825]";
  cout << " Word[826]";
  cout << " Word[827]";
  cout << " Word[828]";
  cout << " Word[829]";
  cout << " Word[830]";
  cout << " Word[831]";
  cout << " Word[832]";
  cout << " Word[833]";
  cout << " Word[834]";
  cout << " Word[835]";
  cout << " Word[836]";
  cout << " Word[837]";
  cout << " Word[838]";
  cout << " Word[839]";
  cout << " Word[840]";
  cout << " Word[841]";
  cout << " Word[842]";
  cout << " Word[843]";
  cout << " Word[844]";
  cout << " Word[845]";
  cout << " Word[846]";
  cout << " Word[847]";
  cout << " Word[848]";
  cout << " Word[849]";
  cout << " Word[850]";
  cout << " Word[851]";
  cout << " Word[852]";
  cout << " Word[853]";
  cout << " Word[854]";
  cout << " Word[855]";
  cout << " Word[856]";
  cout << " Word[857]";
  cout << " Word[858]";
  cout << " Word[859]";
  cout << " Word[860]";
  cout << " Word[861]";
  cout << " Word[862]";
  cout << " Word[863]";
  cout << " Word[864]";
  cout << " Word[865]";
  cout << " Word[866]";
  cout << " Word[867]";
  cout << " Word[868]";
  cout << " Word[869]";
  cout << " Word[870]";
  cout << " Word[871]";
  cout << " Word[872]";
  cout << " Word[873]";
  cout << " Word[874]";
  cout << " Word[875]";
  cout << " Word[876]";
  cout << " Word[877]";
  cout << " Word[878]";
  cout << " Word[879]";
  cout << " Word[880]";
  cout << " Word[881]";
  cout << " Word[882]";
  cout << " Word[883]";
  cout << " Word[884]";
  cout << " Word[885]";
  cout << " Word[886]";
  cout << " Word[887]";
  cout << " Word[888]";
  cout << " Word[889]";
  cout << " Word[890]";
  cout << " Word[891]";
  cout << " Word[892]";
  cout << " Word[893]";
  cout << " Word[894]";
  cout << " Word[895]";
  cout << " Word[896]";
  cout << " Word[897]";
  cout << " Word[898]";
  cout << " Word[899]";
  cout << " Word[900]";
  cout << " Word[901]";
  cout << " Word[902]";
  cout << " Word[903]";
  cout << " Word[904]";
  cout << " Word[905]";
  cout << " Word[906]";
  cout << " Word[907]";
  cout << " Word[908]";
  cout << " Word[909]";
  cout << " Word[910]";
  cout << " Word[911]";
  cout << " Word[912]";
  cout << " Word[913]";
  cout << " Word[914]";
  cout << " Word[915]";
  cout << " Word[916]";
  cout << " Word[917]";
  cout << " Word[918]";
  cout << " Word[919]";
  cout << " Word[920]";
  cout << " Word[921]";
  cout << " Word[922]";
  cout << " Word[923]";
  cout << " Word[924]";
  cout << " Word[925]";
  cout << " Word[926]";
  cout << " Word[927]";
  cout << " Word[928]";
  cout << " Word[929]";
  cout << " Word[930]";
  cout << " Word[931]";
  cout << " Word[932]";
  cout << " Word[933]";
  cout << " Word[934]";
  cout << " Word[935]";
  cout << " Word[936]";
  cout << " Word[937]";
  cout << " Word[938]";
  cout << " Word[939]";
  cout << " Word[940]";
  cout << " Word[941]";
  cout << " Word[942]";
  cout << " Word[943]";
  cout << " Word[944]";
  cout << " Word[945]";
  cout << " Word[946]";
  cout << " Word[947]";
  cout << " Word[948]";
  cout << " Word[949]";
  cout << " Word[950]";
  cout << " Word[951]";
  cout << " Word[952]";
  cout << " Word[953]";
  cout << " Word[954]";
  cout << " Word[955]";
  cout << " Word[956]";
  cout << " Word[957]";
  cout << " Word[958]";
  cout << " Word[959]";
  cout << " Word[960]";
  cout << " Word[961]";
  cout << " Word[962]";
  cout << " Word[963]";
  cout << " Word[964]";
  cout << " Word[965]";
  cout << " Word[966]";
  cout << " Word[967]";
  cout << " Word[968]";
  cout << " Word[969]";
  cout << " Word[970]";
  cout << " Word[971]";
  cout << " Word[972]";
  cout << " Word[973]";
  cout << " Word[974]";
  cout << " Word[975]";
  cout << " Word[976]";
  cout << " Word[977]";
  cout << " Word[978]";
  cout << " Word[979]";
  cout << " Word[980]";
  cout << " Word[981]";
  cout << " Word[982]";
  cout << " Word[983]";
  cout << " Word[984]";
  cout << " Word[985]";
  cout << " Word[986]";
  cout << " Word[987]";
  cout << " Word[988]";
  cout << " Word[989]";
  cout << " Word[990]";
  cout << " Word[991]";
  cout << " Word[992]";
  cout << " Word[993]";
  cout << " Word[994]";
  cout << " Word[995]";
  cout << " Word[996]";
  cout << " Word[997]";
  cout << " Word[998]";
  cout << " Word[999]";
  cout << " Word[1000]";
  cout << " Word[1001]";
  cout << " Word[1002]";
  cout << " Word[1003]";
  cout << " Word[1004]";
  cout << " Word[1005]";
  cout << " Word[1006]";
  cout << " Word[1007]";
  cout << " Word[1008]";
  cout << " Word[1009]";
  cout << " Word[1010]";
  cout << " Word[1011]";
  cout << " Word[1012]";
  cout << " Word[1013]";
  cout << " Word[1014]";
  cout << " Word[1015]";
  cout << " Word[1016]";
  cout << " Word[1017]";
  cout << " Word[1018]";
  cout << " Word[1019]";
  cout << " Word[1020]";
  cout << " Word[1021]";
  cout << " Word[1022]";
  cout << " Word[1023]";
  cout << " Word[1024]";
  cout << " Word[1025]";
  cout << " Word[1026]";
  cout << " Word[1027]";
  cout << " Word[1028]";
  cout << " Word[1029]";
  cout << " Word[1030]";
  cout << " Word[1031]";
  cout << " Word[1032]";
  cout << " Word[1033]";
  cout << " Word[1034]";
  cout << " Word[1035]";
  cout << " Word[1036]";
  cout << " Word[1037]";
  cout << " Word[1038]";
  cout << " Word[1039]";
  cout << " Word[1040]";
  cout << " Word[1041]";
  cout << " Word[1042]";
  cout << " Word[1043]";
  cout << " Word[1044]";
  cout << " Word[1045]";
  cout << " Word[1046]";
  cout << " Word[1047]";
  cout << " Word[1048]";
  cout << " Word[1049]";
  cout << " Word[1050]";
  cout << " Word[1051]";
  cout << " Word[1052]";
  cout << " Word[1053]";
  cout << " Word[1054]";
  cout << " Word[1055]";
  cout << " Word[1056]";
  cout << " Word[1057]";
  cout << " Word[1058]";
  cout << " Word[1059]";
  cout << " Word[1060]";
  cout << " Word[1061]";
  cout << " Word[1062]";
  cout << " Word[1063]";
  cout << " Word[1064]";
  cout << " Word[1065]";
  cout << " Word[1066]";
  cout << " Word[1067]";
  cout << " Word[1068]";
  cout << " Word[1069]";
  cout << " Word[1070]";
  cout << " Word[1071]";
  cout << " Word[1072]";
  cout << " Word[1073]";
  cout << " Word[1074]";
  cout << " Word[1075]";
  cout << " Word[1076]";
  cout << " Word[1077]";
  cout << " Word[1078]";
  cout << " Word[1079]";
  cout << " Word[1080]";
  cout << " Word[1081]";
  cout << " Word[1082]";
  cout << " Word[1083]";
  cout << " Word[1084]";
  cout << " Word[1085]";
  cout << " Word[1086]";
  cout << " Word[1087]";
  cout << " Word[1088]";
  cout << " Word[1089]";
  cout << " Word[1090]";
  cout << " Word[1091]";
  cout << " Word[1092]";
  cout << " Word[1093]";
  cout << " Word[1094]";
  cout << " Word[1095]";
  cout << " Word[1096]";
  cout << " Word[1097]";
  cout << " Word[1098]";
  cout << " Word[1099]";
  cout << " Word[1100]";
  cout << " Word[1101]";
  cout << " Word[1102]";
  cout << " Word[1103]";
  cout << " Word[1104]";
  cout << " Word[1105]";
  cout << " Word[1106]";
  cout << " Word[1107]";
  cout << " Word[1108]";
  cout << " Word[1109]";
  cout << " Word[1110]";
  cout << " Word[1111]";
  cout << " Word[1112]";
  cout << " Word[1113]";
  cout << " Word[1114]";
  cout << " Word[1115]";
  cout << " Word[1116]";
  cout << " Word[1117]";
  cout << " Word[1118]";
  cout << " Word[1119]";
  cout << " Word[1120]";
  cout << " Word[1121]";
  cout << " Word[1122]";
  cout << " Word[1123]";
  cout << " Word[1124]";
  cout << " Word[1125]";
  cout << " Word[1126]";
  cout << " Word[1127]";
  cout << " Word[1128]";
  cout << " Word[1129]";
  cout << " Word[1130]";
  cout << " Word[1131]";
  cout << " Word[1132]";
  cout << " Word[1133]";
  cout << " Word[1134]";
  cout << " Word[1135]";
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
     for(int i0=0; i0<1136; i0++) {
        cout << " " << setw(11) << fTableData[row].Word[i0];
     }
     cout << " " << setw(11) << fTableData[row].parity;
     cout << " " << setw(11) << fTableData[row].CAV2;

     cout << endl;
  }

}

void
dTofFEMWrapper::Print(Option_t* option) const
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
dTofFEMWrapper::SetMaxRowCount(const size_t& max_rows)
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
     DTOFFEM_ST* newData = new DTOFFEM_ST[max_rows];
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
dTofFEMWrapper::SetRowCount(const size_t& n)
{
  if (n > (size_t) fTableHeader->maxlen) {
     fTableHeader->nok = fTableHeader->maxlen;
  }
  else if (n >= 0) {
     fTableHeader->nok = n;
  }
}

void
dTofFEMWrapper::SetRowSize(const size_t& row_size)
{
  if (row_size > 0) {
     fTableHeader->rbytes = row_size;
  }
}

void
dTofFEMWrapper::Streamer(TBuffer& b)
{
   // Stream an object of class dTofFEMWrapper.
   // What should be done on output if the table is empty?

   if (b.IsReading()) {
     b.ReadVersion();
     PHTable::Streamer(b);         // Read the table header.
     if (RowSize() != sizeof(DTOFFEM_ST)) {
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
     fTableData = new DTOFFEM_ST[max_rows];
     fTableHeader->data_pointer = (long)fTableData;

     SetMaxRowCount(max_rows);
     SetType("dTofFEMWrapper");

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
        b.WriteArray(fTableData[i].Word,1136);
        b << fTableData[i].parity;
        b << fTableData[i].CAV2;
     }
   }

}
/* Automatically generated.  Do not edit. */
