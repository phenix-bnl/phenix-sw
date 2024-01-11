#include <MpcTrigEmulator.h>
#include <mpcSampleContainer.h>
#include <mpcSample.h>
#include <getClass.h>

#include <iostream>
#include <iomanip>

using namespace std;

MpcTrigEmulator::MpcTrigEmulator(const std::string &name): SubsysReco(name)
{
  //mpcmap = 0;
  SetSumTrigger();
}

MpcTrigEmulator::~MpcTrigEmulator()
{
}

int MpcTrigEmulator::EndRun(const int runnumber)
{
  return 0;
}

int MpcTrigEmulator::InitRun(PHCompositeNode *topNode)
{
  // Set the energy scale based on run
/*
  int run_number = -999999;
  RunHeader *runheader = findNode::getClass<RunHeader>(topNode,"RunHeader");
  if ( runheader!=0 )
    {
      run_number = runheader->get_RunNumber();
    }

  if ( run_number == -999999 )
    {
      // Didn't get good run number from sync,
      // try again using recoConst
      recoConsts *rc = recoConsts::instance();
      run_number = rc->get_IntFlag("RUNNUMBER");
    } 
*/

  SetSumTrigger();

//  cout << "MpcTrigEmulator RUNNUMBER " << run_number << endl;

  //if ( mpcmap != 0 ) mpcmap->Download_Maps(topNode);

  return 0;
}

int MpcTrigEmulator::process_event(PHCompositeNode *topNode)
{
  mpcSampleContainer *mpcsamples = findNode::getClass<mpcSampleContainer>(topNode,"mpcSampleContainer");
  if (!mpcsamples)
    {
      cout << PHWHERE << "Unable to get mpcSamplesContainer, is Node missing?" << endl;
      return 0;
    }

  CalcSumTrigger( mpcsamples );

  return 0;
}

//
// n[3], s[3] = A, B, C inclusive decision
// a[12], b[12], c[12] are the module decisions
//
void MpcTrigEmulator::TriggerDecision(int *n, int *s, int *a, int *b, int *c)
{
  // Clear the n, s bits
  for (int itrig=0; itrig<NTRIGS; itrig++)
    {
      n[itrig] = 0;
      s[itrig] = 0;
    }
  for (int imodule=0; imodule<NMODULES; imodule++)
    {
      a[imodule] = fTrig[0][imodule];
      b[imodule] = fTrig[1][imodule];
      c[imodule] = fTrig[2][imodule];

      for (int itrig=0; itrig<NTRIGS; itrig++)
        {
          if ( fTrig[itrig][imodule] == 1 )
            {
              if ( imodule<6 ) s[itrig] = 1;
              else             n[itrig] = 1;
            }
        }
    }
}

void MpcTrigEmulator::SetTrigChThreshold(const int ch, const int32_t t)
{
  if ( ch<0 )
    {
      for (int ich=0; ich<NUMCH; ich++)
        {
          trig_ch_threshold[ich] = t;
        }
    }
  else
    {
      trig_ch_threshold[ch] = t;
    }
}

void MpcTrigEmulator::SetPtTrigger()
{
  // Set scale for each channel to (sin(theta)/0.04)*64
  trig_scalefactor[0] = 64;//
  trig_scalefactor[1] = 64;//
  trig_scalefactor[2] = 64;//
  trig_scalefactor[3] = 64;//
  trig_scalefactor[4] = 64;//
  trig_scalefactor[5] = 64;//
  trig_scalefactor[6] = 64;//
  trig_scalefactor[7] = 64;//
  trig_scalefactor[8] = 64;//
  trig_scalefactor[9] = 64;//
  trig_scalefactor[10] = 64;//
  trig_scalefactor[11] = 64;//
  trig_scalefactor[12] = 64;//
  trig_scalefactor[13] = 147;
  trig_scalefactor[14] = 143;
  trig_scalefactor[15] = 141;
  trig_scalefactor[16] = 64;//
  trig_scalefactor[17] = 64;//
  trig_scalefactor[18] = 64;//
  trig_scalefactor[19] = 145;
  trig_scalefactor[20] = 138;
  trig_scalefactor[21] = 131;
  trig_scalefactor[22] = 127;
  trig_scalefactor[23] = 125;
  trig_scalefactor[24] = 64;//
  trig_scalefactor[25] = 64;//
  trig_scalefactor[26] = 141;
  trig_scalefactor[27] = 131;
  trig_scalefactor[28] = 123;
  trig_scalefactor[29] = 116;
  trig_scalefactor[30] = 111;
  trig_scalefactor[31] = 109;
  trig_scalefactor[32] = 64;//
  trig_scalefactor[33] = 141;
  trig_scalefactor[34] = 129;
  trig_scalefactor[35] = 118;
  trig_scalefactor[36] = 109;
  trig_scalefactor[37] = 101;
  trig_scalefactor[38] = 96;
  trig_scalefactor[39] = 93;
  trig_scalefactor[40] = 145;
  trig_scalefactor[41] = 131;
  trig_scalefactor[42] = 118;
  trig_scalefactor[43] = 106;
  trig_scalefactor[44] = 96;
  trig_scalefactor[45] = 87;
  trig_scalefactor[46] = 80;
  trig_scalefactor[47] = 76;
  trig_scalefactor[48] = 64;//
  trig_scalefactor[49] = 148;
  trig_scalefactor[50] = 144;
  trig_scalefactor[51] = 142;
  trig_scalefactor[52] = 142;
  trig_scalefactor[53] = 144;
  trig_scalefactor[54] = 148;
  trig_scalefactor[55] = 64;//
  trig_scalefactor[56] = 138;
  trig_scalefactor[57] = 132;
  trig_scalefactor[58] = 128;
  trig_scalefactor[59] = 126;
  trig_scalefactor[60] = 126;
  trig_scalefactor[61] = 128;
  trig_scalefactor[62] = 132;
  trig_scalefactor[63] = 138;
  trig_scalefactor[64] = 123;
  trig_scalefactor[65] = 117;
  trig_scalefactor[66] = 112;
  trig_scalefactor[67] = 110;
  trig_scalefactor[68] = 110;
  trig_scalefactor[69] = 112;
  trig_scalefactor[70] = 117;
  trig_scalefactor[71] = 123;
  trig_scalefactor[72] = 109;
  trig_scalefactor[73] = 102;
  trig_scalefactor[74] = 96;
  trig_scalefactor[75] = 94;
  trig_scalefactor[76] = 94;
  trig_scalefactor[77] = 96;
  trig_scalefactor[78] = 102;
  trig_scalefactor[79] = 109;
  trig_scalefactor[80] = 96;
  trig_scalefactor[81] = 87;
  trig_scalefactor[82] = 81;
  trig_scalefactor[83] = 77;
  trig_scalefactor[84] = 77;
  trig_scalefactor[85] = 81;
  trig_scalefactor[86] = 87;
  trig_scalefactor[87] = 96;
  trig_scalefactor[88] = 83;
  trig_scalefactor[89] = 64;//
  trig_scalefactor[90] = 64;//
  trig_scalefactor[91] = 64;//
  trig_scalefactor[92] = 64;//
  trig_scalefactor[93] = 64;//
  trig_scalefactor[94] = 64;//
  trig_scalefactor[95] = 83;
  trig_scalefactor[96] = 64;//
  trig_scalefactor[97] = 64;//
  trig_scalefactor[98] = 64;//
  trig_scalefactor[99] = 64;//
  trig_scalefactor[100] = 64;//
  trig_scalefactor[101] = 64;//
  trig_scalefactor[102] = 64;//
  trig_scalefactor[103] = 64;//
  trig_scalefactor[104] = 64;//
  trig_scalefactor[105] = 64;//
  trig_scalefactor[106] = 64;//
  trig_scalefactor[107] = 64;//
  trig_scalefactor[108] = 64;//
  trig_scalefactor[109] = 147;
  trig_scalefactor[110] = 143;
  trig_scalefactor[111] = 141;
  trig_scalefactor[112] = 64;//
  trig_scalefactor[113] = 64;//
  trig_scalefactor[114] = 64;//
  trig_scalefactor[115] = 145;
  trig_scalefactor[116] = 138;
  trig_scalefactor[117] = 131;
  trig_scalefactor[118] = 127;
  trig_scalefactor[119] = 125;
  trig_scalefactor[120] = 64;//
  trig_scalefactor[121] = 64;//
  trig_scalefactor[122] = 141;
  trig_scalefactor[123] = 131;
  trig_scalefactor[124] = 123;
  trig_scalefactor[125] = 116;
  trig_scalefactor[126] = 111;
  trig_scalefactor[127] = 109;
  trig_scalefactor[128] = 64;//
  trig_scalefactor[129] = 141;
  trig_scalefactor[130] = 129;
  trig_scalefactor[131] = 118;
  trig_scalefactor[132] = 109;
  trig_scalefactor[133] = 101;
  trig_scalefactor[134] = 96;
  trig_scalefactor[135] = 93;
  trig_scalefactor[136] = 145;
  trig_scalefactor[137] = 131;
  trig_scalefactor[138] = 118;
  trig_scalefactor[139] = 106;
  trig_scalefactor[140] = 96;
  trig_scalefactor[141] = 87;
  trig_scalefactor[142] = 80;
  trig_scalefactor[143] = 76;
  trig_scalefactor[144] = 64;//
  trig_scalefactor[145] = 64;//
  trig_scalefactor[146] = 64;//
  trig_scalefactor[147] = 64;//
  trig_scalefactor[148] = 64;//
  trig_scalefactor[149] = 64;//
  trig_scalefactor[150] = 64;//
  trig_scalefactor[151] = 64;//
  trig_scalefactor[152] = 64;//
  trig_scalefactor[153] = 64;//
  trig_scalefactor[154] = 64;//
  trig_scalefactor[155] = 64;//
  trig_scalefactor[156] = 64;//
  trig_scalefactor[157] = 147;
  trig_scalefactor[158] = 143;
  trig_scalefactor[159] = 141;
  trig_scalefactor[160] = 64;//
  trig_scalefactor[161] = 64;//
  trig_scalefactor[162] = 64;//
  trig_scalefactor[163] = 145;
  trig_scalefactor[164] = 138;
  trig_scalefactor[165] = 131;
  trig_scalefactor[166] = 127;
  trig_scalefactor[167] = 125;
  trig_scalefactor[168] = 64;//
  trig_scalefactor[169] = 64;//
  trig_scalefactor[170] = 141;
  trig_scalefactor[171] = 131;
  trig_scalefactor[172] = 123;
  trig_scalefactor[173] = 116;
  trig_scalefactor[174] = 111;
  trig_scalefactor[175] = 109;
  trig_scalefactor[176] = 64;//
  trig_scalefactor[177] = 141;
  trig_scalefactor[178] = 129;
  trig_scalefactor[179] = 118;
  trig_scalefactor[180] = 109;
  trig_scalefactor[181] = 101;
  trig_scalefactor[182] = 96;
  trig_scalefactor[183] = 93;
  trig_scalefactor[184] = 145;
  trig_scalefactor[185] = 131;
  trig_scalefactor[186] = 118;
  trig_scalefactor[187] = 106;
  trig_scalefactor[188] = 96;
  trig_scalefactor[189] = 87;
  trig_scalefactor[190] = 80;
  trig_scalefactor[191] = 76;
  trig_scalefactor[192] = 64;//
  trig_scalefactor[193] = 148;
  trig_scalefactor[194] = 144;
  trig_scalefactor[195] = 142;
  trig_scalefactor[196] = 142;
  trig_scalefactor[197] = 144;
  trig_scalefactor[198] = 148;
  trig_scalefactor[199] = 64;//
  trig_scalefactor[200] = 138;
  trig_scalefactor[201] = 132;
  trig_scalefactor[202] = 128;
  trig_scalefactor[203] = 126;
  trig_scalefactor[204] = 126;
  trig_scalefactor[205] = 128;
  trig_scalefactor[206] = 132;
  trig_scalefactor[207] = 138;
  trig_scalefactor[208] = 123;
  trig_scalefactor[209] = 117;
  trig_scalefactor[210] = 112;
  trig_scalefactor[211] = 110;
  trig_scalefactor[212] = 110;
  trig_scalefactor[213] = 112;
  trig_scalefactor[214] = 117;
  trig_scalefactor[215] = 123;
  trig_scalefactor[216] = 109;
  trig_scalefactor[217] = 102;
  trig_scalefactor[218] = 96;
  trig_scalefactor[219] = 94;
  trig_scalefactor[220] = 94;
  trig_scalefactor[221] = 96;
  trig_scalefactor[222] = 102;
  trig_scalefactor[223] = 109;
  trig_scalefactor[224] = 96;
  trig_scalefactor[225] = 87;
  trig_scalefactor[226] = 81;
  trig_scalefactor[227] = 77;
  trig_scalefactor[228] = 77;
  trig_scalefactor[229] = 81;
  trig_scalefactor[230] = 87;
  trig_scalefactor[231] = 96;
  trig_scalefactor[232] = 83;
  trig_scalefactor[233] = 64;//
  trig_scalefactor[234] = 64;//
  trig_scalefactor[235] = 64;//
  trig_scalefactor[236] = 64;//
  trig_scalefactor[237] = 64;//
  trig_scalefactor[238] = 64;//
  trig_scalefactor[239] = 83;
  trig_scalefactor[240] = 64;//
  trig_scalefactor[241] = 64;//
  trig_scalefactor[242] = 64;//
  trig_scalefactor[243] = 64;//
  trig_scalefactor[244] = 64;//
  trig_scalefactor[245] = 64;//
  trig_scalefactor[246] = 64;//
  trig_scalefactor[247] = 64;//
  trig_scalefactor[248] = 64;//
  trig_scalefactor[249] = 64;//
  trig_scalefactor[250] = 64;//
  trig_scalefactor[251] = 64;//
  trig_scalefactor[252] = 64;//
  trig_scalefactor[253] = 147;
  trig_scalefactor[254] = 143;
  trig_scalefactor[255] = 141;
  trig_scalefactor[256] = 64;//
  trig_scalefactor[257] = 64;//
  trig_scalefactor[258] = 64;//
  trig_scalefactor[259] = 145;
  trig_scalefactor[260] = 138;
  trig_scalefactor[261] = 131;
  trig_scalefactor[262] = 127;
  trig_scalefactor[263] = 125;
  trig_scalefactor[264] = 64;//
  trig_scalefactor[265] = 64;//
  trig_scalefactor[266] = 141;
  trig_scalefactor[267] = 131;
  trig_scalefactor[268] = 123;
  trig_scalefactor[269] = 116;
  trig_scalefactor[270] = 111;
  trig_scalefactor[271] = 109;
  trig_scalefactor[272] = 64;//
  trig_scalefactor[273] = 141;
  trig_scalefactor[274] = 129;
  trig_scalefactor[275] = 118;
  trig_scalefactor[276] = 109;
  trig_scalefactor[277] = 101;
  trig_scalefactor[278] = 96;
  trig_scalefactor[279] = 93;
  trig_scalefactor[280] = 145;
  trig_scalefactor[281] = 131;
  trig_scalefactor[282] = 118;
  trig_scalefactor[283] = 106;
  trig_scalefactor[284] = 96;
  trig_scalefactor[285] = 87;
  trig_scalefactor[286] = 80;
  trig_scalefactor[287] = 76;
  trig_scalefactor[288] = 64;//
  trig_scalefactor[289] = 64;//
  trig_scalefactor[290] = 64;//
  trig_scalefactor[291] = 64;//
  trig_scalefactor[292] = 64;//
  trig_scalefactor[293] = 147;
  trig_scalefactor[294] = 144;
  trig_scalefactor[295] = 142;
  trig_scalefactor[296] = 64;//
  trig_scalefactor[297] = 64;//
  trig_scalefactor[298] = 64;//
  trig_scalefactor[299] = 145;
  trig_scalefactor[300] = 138;
  trig_scalefactor[301] = 132;
  trig_scalefactor[302] = 128;
  trig_scalefactor[303] = 125;
  trig_scalefactor[304] = 64;//
  trig_scalefactor[305] = 64;//
  trig_scalefactor[306] = 142;
  trig_scalefactor[307] = 132;
  trig_scalefactor[308] = 123;
  trig_scalefactor[309] = 117;
  trig_scalefactor[310] = 112;
  trig_scalefactor[311] = 109;
  trig_scalefactor[312] = 64;//
  trig_scalefactor[313] = 141;
  trig_scalefactor[314] = 130;
  trig_scalefactor[315] = 119;
  trig_scalefactor[316] = 109;
  trig_scalefactor[317] = 102;
  trig_scalefactor[318] = 96;
  trig_scalefactor[319] = 93;
  trig_scalefactor[320] = 145;
  trig_scalefactor[321] = 131;
  trig_scalefactor[322] = 119;
  trig_scalefactor[323] = 107;
  trig_scalefactor[324] = 96;
  trig_scalefactor[325] = 87;
  trig_scalefactor[326] = 80;
  trig_scalefactor[327] = 77;
  trig_scalefactor[328] = 137;
  trig_scalefactor[329] = 123;
  trig_scalefactor[330] = 109;
  trig_scalefactor[331] = 96;
  trig_scalefactor[332] = 84;
  trig_scalefactor[333] = 73;
  trig_scalefactor[334] = 65;
  trig_scalefactor[335] = 61;
  trig_scalefactor[336] = 64;//
  trig_scalefactor[337] = 64;//
  trig_scalefactor[338] = 135;
  trig_scalefactor[339] = 119;
  trig_scalefactor[340] = 104;
  trig_scalefactor[341] = 90;
  trig_scalefactor[342] = 75;
  trig_scalefactor[343] = 62;
  trig_scalefactor[344] = 64;//
  trig_scalefactor[345] = 147;
  trig_scalefactor[346] = 131;
  trig_scalefactor[347] = 115;
  trig_scalefactor[348] = 99;
  trig_scalefactor[349] = 83;
  trig_scalefactor[350] = 68;
  trig_scalefactor[351] = 64;//
  trig_scalefactor[352] = 64;//
  trig_scalefactor[353] = 145;
  trig_scalefactor[354] = 129;
  trig_scalefactor[355] = 113;
  trig_scalefactor[356] = 96;
  trig_scalefactor[357] = 80;
  trig_scalefactor[358] = 64;
  trig_scalefactor[359] = 64;//
  trig_scalefactor[360] = 64;//
  trig_scalefactor[361] = 145;
  trig_scalefactor[362] = 129;
  trig_scalefactor[363] = 113;
  trig_scalefactor[364] = 96;
  trig_scalefactor[365] = 80;
  trig_scalefactor[366] = 64;
  trig_scalefactor[367] = 64;//
  trig_scalefactor[368] = 64;//
  trig_scalefactor[369] = 147;
  trig_scalefactor[370] = 131;
  trig_scalefactor[371] = 115;
  trig_scalefactor[372] = 99;
  trig_scalefactor[373] = 83;
  trig_scalefactor[374] = 68;
  trig_scalefactor[375] = 64;//
  trig_scalefactor[376] = 64;//
  trig_scalefactor[377] = 64;//
  trig_scalefactor[378] = 135;
  trig_scalefactor[379] = 119;
  trig_scalefactor[380] = 104;
  trig_scalefactor[381] = 90;
  trig_scalefactor[382] = 75;
  trig_scalefactor[383] = 62;
  trig_scalefactor[384] = 64;//
  trig_scalefactor[385] = 64;//
  trig_scalefactor[386] = 64;//
  trig_scalefactor[387] = 64;//
  trig_scalefactor[388] = 64;//
  trig_scalefactor[389] = 147;
  trig_scalefactor[390] = 143;
  trig_scalefactor[391] = 141;
  trig_scalefactor[392] = 64;//
  trig_scalefactor[393] = 64;//
  trig_scalefactor[394] = 64;//
  trig_scalefactor[395] = 145;
  trig_scalefactor[396] = 138;
  trig_scalefactor[397] = 132;
  trig_scalefactor[398] = 128;
  trig_scalefactor[399] = 125;
  trig_scalefactor[400] = 64;//
  trig_scalefactor[401] = 64;//
  trig_scalefactor[402] = 142;
  trig_scalefactor[403] = 132;
  trig_scalefactor[404] = 123;
  trig_scalefactor[405] = 116;
  trig_scalefactor[406] = 112;
  trig_scalefactor[407] = 109;
  trig_scalefactor[408] = 64;//
  trig_scalefactor[409] = 141;
  trig_scalefactor[410] = 130;
  trig_scalefactor[411] = 119;
  trig_scalefactor[412] = 109;
  trig_scalefactor[413] = 102;
  trig_scalefactor[414] = 96;
  trig_scalefactor[415] = 93;
  trig_scalefactor[416] = 145;
  trig_scalefactor[417] = 131;
  trig_scalefactor[418] = 119;
  trig_scalefactor[419] = 107;
  trig_scalefactor[420] = 96;
  trig_scalefactor[421] = 87;
  trig_scalefactor[422] = 80;
  trig_scalefactor[423] = 77;
  trig_scalefactor[424] = 137;
  trig_scalefactor[425] = 123;
  trig_scalefactor[426] = 109;
  trig_scalefactor[427] = 96;
  trig_scalefactor[428] = 84;
  trig_scalefactor[429] = 73;
  trig_scalefactor[430] = 65;
  trig_scalefactor[431] = 61;
  trig_scalefactor[432] = 64;//
  trig_scalefactor[433] = 64;//
  trig_scalefactor[434] = 64;//
  trig_scalefactor[435] = 64;//
  trig_scalefactor[436] = 64;//
  trig_scalefactor[437] = 147;
  trig_scalefactor[438] = 143;
  trig_scalefactor[439] = 141;
  trig_scalefactor[440] = 64;//
  trig_scalefactor[441] = 64;//
  trig_scalefactor[442] = 64;//
  trig_scalefactor[443] = 145;
  trig_scalefactor[444] = 138;
  trig_scalefactor[445] = 132;
  trig_scalefactor[446] = 128;
  trig_scalefactor[447] = 125;
  trig_scalefactor[448] = 64;//
  trig_scalefactor[449] = 64;//
  trig_scalefactor[450] = 142;
  trig_scalefactor[451] = 132;
  trig_scalefactor[452] = 123;
  trig_scalefactor[453] = 116;
  trig_scalefactor[454] = 112;
  trig_scalefactor[455] = 109;
  trig_scalefactor[456] = 64;//
  trig_scalefactor[457] = 141;
  trig_scalefactor[458] = 130;
  trig_scalefactor[459] = 119;
  trig_scalefactor[460] = 109;
  trig_scalefactor[461] = 102;
  trig_scalefactor[462] = 96;
  trig_scalefactor[463] = 93;
  trig_scalefactor[464] = 145;
  trig_scalefactor[465] = 131;
  trig_scalefactor[466] = 119;
  trig_scalefactor[467] = 107;
  trig_scalefactor[468] = 96;
  trig_scalefactor[469] = 87;
  trig_scalefactor[470] = 80;
  trig_scalefactor[471] = 77;
  trig_scalefactor[472] = 137;
  trig_scalefactor[473] = 123;
  trig_scalefactor[474] = 109;
  trig_scalefactor[475] = 96;
  trig_scalefactor[476] = 84;
  trig_scalefactor[477] = 73;
  trig_scalefactor[478] = 65;
  trig_scalefactor[479] = 61;
  trig_scalefactor[480] = 64;//
  trig_scalefactor[481] = 64;//
  trig_scalefactor[482] = 135;
  trig_scalefactor[483] = 119;
  trig_scalefactor[484] = 104;
  trig_scalefactor[485] = 90;
  trig_scalefactor[486] = 75;
  trig_scalefactor[487] = 62;
  trig_scalefactor[488] = 64;//
  trig_scalefactor[489] = 147;
  trig_scalefactor[490] = 131;
  trig_scalefactor[491] = 115;
  trig_scalefactor[492] = 99;
  trig_scalefactor[493] = 83;
  trig_scalefactor[494] = 68;
  trig_scalefactor[495] = 64;//
  trig_scalefactor[496] = 64;//
  trig_scalefactor[497] = 145;
  trig_scalefactor[498] = 129;
  trig_scalefactor[499] = 113;
  trig_scalefactor[500] = 96;
  trig_scalefactor[501] = 80;
  trig_scalefactor[502] = 64;
  trig_scalefactor[503] = 64;//
  trig_scalefactor[504] = 64;//
  trig_scalefactor[505] = 145;
  trig_scalefactor[506] = 129;
  trig_scalefactor[507] = 113;
  trig_scalefactor[508] = 96;
  trig_scalefactor[509] = 80;
  trig_scalefactor[510] = 64;
  trig_scalefactor[511] = 64;//
  trig_scalefactor[512] = 64;//
  trig_scalefactor[513] = 147;
  trig_scalefactor[514] = 131;
  trig_scalefactor[515] = 115;
  trig_scalefactor[516] = 99;
  trig_scalefactor[517] = 83;
  trig_scalefactor[518] = 68;
  trig_scalefactor[519] = 64;//
  trig_scalefactor[520] = 64;//
  trig_scalefactor[521] = 64;//
  trig_scalefactor[522] = 135;
  trig_scalefactor[523] = 119;
  trig_scalefactor[524] = 104;
  trig_scalefactor[525] = 90;
  trig_scalefactor[526] = 75;
  trig_scalefactor[527] = 62;
  trig_scalefactor[528] = 64;//
  trig_scalefactor[529] = 64;//
  trig_scalefactor[530] = 64;//
  trig_scalefactor[531] = 64;//
  trig_scalefactor[532] = 64;//
  trig_scalefactor[533] = 147;
  trig_scalefactor[534] = 144;
  trig_scalefactor[535] = 142;
  trig_scalefactor[536] = 64;//
  trig_scalefactor[537] = 64;//
  trig_scalefactor[538] = 64;//
  trig_scalefactor[539] = 145;
  trig_scalefactor[540] = 138;
  trig_scalefactor[541] = 132;
  trig_scalefactor[542] = 128;
  trig_scalefactor[543] = 125;
  trig_scalefactor[544] = 64;//
  trig_scalefactor[545] = 64;//
  trig_scalefactor[546] = 142;
  trig_scalefactor[547] = 132;
  trig_scalefactor[548] = 123;
  trig_scalefactor[549] = 117;
  trig_scalefactor[550] = 112;
  trig_scalefactor[551] = 109;
  trig_scalefactor[552] = 64;//
  trig_scalefactor[553] = 141;
  trig_scalefactor[554] = 130;
  trig_scalefactor[555] = 119;
  trig_scalefactor[556] = 109;
  trig_scalefactor[557] = 102;
  trig_scalefactor[558] = 96;
  trig_scalefactor[559] = 93;
  trig_scalefactor[560] = 145;
  trig_scalefactor[561] = 131;
  trig_scalefactor[562] = 119;
  trig_scalefactor[563] = 107;
  trig_scalefactor[564] = 96;
  trig_scalefactor[565] = 87;
  trig_scalefactor[566] = 80;
  trig_scalefactor[567] = 77;
  trig_scalefactor[568] = 137;
  trig_scalefactor[569] = 123;
  trig_scalefactor[570] = 109;
  trig_scalefactor[571] = 96;
  trig_scalefactor[572] = 84;
  trig_scalefactor[573] = 73;
  trig_scalefactor[574] = 65;
  trig_scalefactor[575] = 61;
}

int MpcTrigEmulator::SetSumTrigger()
{
  // Reset values
  for (int ifeech=0; ifeech<576; ifeech++) {
    trig_scalefactor[ifeech] = 64;	// scales to 1
    trig_ch_threshold[ifeech] = 1;	// sgl ch threshold for hit consideration
  }

  trig_adcsum_threshold[0] = 40;		// TP2 (A)
  trig_adcsum_threshold[1] = 48;		// TP3 (B)
  trig_adcsum_threshold[2] = 20;		// TP4 (C)

  trig_nhit_threshold[0] = 0;		// TP2
  trig_nhit_threshold[1] = 0;		// TP3
  trig_nhit_threshold[2] = 0;		// TP4

  trig_prepost_delay = 3;	// According to Chi, add 2 to get true post-pre delay

  // Noisy Channel (13,9)
  //trig_scalefactor[229] = 0;

  // Pin Diode Channels
  trig_scalefactor[0] = 0;
  trig_scalefactor[48] = 0;
  trig_scalefactor[96] = 0;
  trig_scalefactor[144] = 0;
  trig_scalefactor[192] = 0;
  trig_scalefactor[240] = 0;

  //SetPtTrigger();

  return 1;
}

int MpcTrigEmulator::CalcSumTrigger( mpcSampleContainer *samples )
{
  // Reset trigger bits
  for (int imodule=0; imodule<NMODULES; imodule++)
    {
      for (int itrig=0; itrig<NTRIGS; itrig++)
        {
          fTrig[itrig][imodule] = 0;
          fTrig[itrig][imodule] = 0;
          fTrig[itrig][imodule] = 0;
        }
      total_sum[imodule] = 0;
      total_nhits[imodule] = 0;
    }

  uint32_t adc[576][12] = {{0}};		// raw adc values, [ch][samp]
  uint32_t adc_scaled[576][12] = {{0}};		// scaled values, [ch][samp]
  uint32_t nhit[576][12] = {{0}};		// nhits, [ch][samp]

  uint32_t total_sum_sample[12][12] = {{0}};	// sum in each of 12 modules, for each sampling
  uint32_t total_nhits_sample[12][12] = {{0}};	// nhits in each of 12 modules, for each sampling

  int prev_module = 0;
  int nsamples = samples->size();
  for (int isamp=0; isamp<nsamples; isamp++)
    {
      mpcSample *adcsamp = samples->GetSample(isamp);
      if ( adcsamp==0 ) continue;
 
      int feech = adcsamp->get_ch();     // fee576ch
      int samp = adcsamp->get_sample();  // sample number

      if ( feech<0||feech>575||samp<0||samp>11 )
        {
          cout << PHWHERE << "ERROR, feech samp out of bounds "
               << feech << "\t" << samp << endl;
          continue;
        }

      adc[feech][samp] = adcsamp->get_adc();
      //int module = feech/48;
      //cout << "feech/module " << feech << "\t" << module << endl;

      if ( verbosity )
        {
          if ( samp==0 ) cout << feech << "\t";
          cout << setw(6) << adc[feech][samp];
        }

      if ( samp >= (trig_prepost_delay+2) )
        {
          // Get ADC value (8 bits) for trigger alg
          int presamp = samp-trig_prepost_delay-2;
          int adc_diff = adc[feech][samp] - adc[feech][presamp];
          if ( adc_diff < 0 )            adc_diff = 0;
          else if ( adc_diff < 0x7f8 )   adc_diff = adc_diff/8;
          else                           adc_diff = 0xff;

          // Check if tower is above single channel threshold
          if ( adc_diff >= trig_ch_threshold[feech] )
            {
              nhit[feech][samp] = 1;
            }

          adc_scaled[feech][samp] = adc_diff*trig_scalefactor[feech];

          if ( adc_scaled[feech][samp] <= 0x3fff )
            {
              // lose 2 top bits + bottom 6 bits
              adc_scaled[feech][samp] = (adc_scaled[feech][samp]&0x3fff) >> 6;
            }
          else
            {
              adc_scaled[feech][samp] = 0xff;
            }
        }

       if ( samp==11 && verbosity )
         {
           // print max found and the sample
           uint32_t maxadc = 0;
           int maxsamp = -1;
           for (int idiff=trig_prepost_delay+2; idiff<12; idiff++ )
             {
               if ( adc_scaled[feech][idiff]>maxadc )
                 {
                   maxadc = adc_scaled[feech][idiff];
                   maxsamp = idiff;
                 }
             }
           cout << "\t" << maxsamp << "\t" << maxadc << endl;

           // separate by module
           if ( prev_module != feech/48 )
             {
               cout << endl;
             }
           prev_module = feech/48;
         }
    }

  // Calculate sums
  if ( verbosity ) cout << "MpcTrigEmulator Summing" << endl;
  for (int isamp=trig_prepost_delay+2; isamp<12; isamp++)
    {
      for (int imodule=0; imodule<NMODULES; imodule++)
        {
          for (int icolumn=0; icolumn<8; icolumn++)
            {
              uint32_t adc_sum = 0;
              for (int irow=0; irow<6; irow++)
                {
                  int feech = imodule*48 + icolumn*6 + irow;
                  adc_sum += adc_scaled[feech][isamp];

                  if ( adc_scaled[feech][isamp]!= 0 && verbosity )
                    {
                      cout << "feech " << feech << "\t" << imodule << "\t"
                           << isamp << "\t"
                           << adc_scaled[feech][isamp] << "\t" << adc_sum << endl;
                    }

                  total_nhits_sample[imodule][isamp] += nhit[feech][isamp];
                }
    
              // Check for overflow
              if ( adc_sum >= 0xff )
                {
                  adc_sum = 0xff;
                }
              else
                {
                  adc_sum = (adc_sum&0xff);
                }
              total_sum_sample[imodule][isamp] += adc_sum;
/*
              if ( verbosity && adc_sum!=0 )
                {
                  cout << "imod " << imodule << "\t" << icolumn << "\t" << adc_sum << endl;
                }
*/
            }
    
          if ( total_sum_sample[imodule][isamp] >= 0xff )
            {
              total_sum_sample[imodule][isamp] = 0xff;
            }
          else
            {
              total_sum_sample[imodule][isamp] = (total_sum_sample[imodule][isamp]&0xff);
            }
        }	// loop over modules

    }	// loop over samples

  for (int isamp=trig_prepost_delay+2; isamp<12; isamp++)
    {
      for (int itrig=0; itrig<NTRIGS; itrig++)
        {
          for (int imodule=0; imodule<NMODULES; imodule++)
            {
              // Store max value into total_sum, total_nhits
              if ( total_sum_sample[imodule][isamp] > total_sum[imodule] )
                {
                  total_sum[imodule] = total_sum_sample[imodule][isamp];
                }
              if ( total_nhits_sample[imodule][isamp] > total_nhits[imodule] )
                {
                  total_nhits[imodule] = total_nhits_sample[imodule][isamp];
                }

              if ( (total_sum[imodule] >= trig_adcsum_threshold[itrig]) &&
                   (total_nhits[imodule] >= trig_nhit_threshold[itrig]) )
                {
                  fTrig[itrig][imodule] = 1;
                }
              else
                {
                  fTrig[itrig][imodule] = 0;
                }
            }
        }
    }

  if ( verbosity )
    {
      cout << "MpcTrigEmulator Total Sums" << endl;
      for (int imodule=0; imodule<NMODULES; imodule++)
        {
          cout << imodule;
          for (int isamp=trig_prepost_delay+2; isamp<12; isamp++)
            {
              cout << "\t" << total_sum_sample[imodule][isamp]
                   << "(" << total_nhits_sample[imodule][isamp] << ")";
            }
          cout << endl;
        }
    }

  // Now check which modules have fired
  return 1;
}


