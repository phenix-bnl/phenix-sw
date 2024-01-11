#include "RecalEneNonLinearity.h"
#include "EmcAnaCommon.h"

#include <Fun4AllReturnCodes.h>
#include <Fun4AllServer.h>
#include <RunHeader.h>
#include <emcClusterContainer.h>
#include <emcClusterContent.h>
#include <getClass.h>


#include <cstdlib>
#include <iostream>

using namespace std;
using namespace EmcAnaCommon;

int RecalEneNonLinearity::process_event(PHCompositeNode *topNode)
{
   RunHeader* runheader =
      findNode::getClass<RunHeader>(topNode, "RunHeader");
   if (runheader == 0) {
      cout << PHWHERE << "RunHeader Node missing.  Abort." << endl;
      exit(1);
   }

   emcClusterContainer* emcclustercontainer = 
      findNode::getClass<emcClusterContainer>(topNode, "emcClusterContainer");
   if (emcclustercontainer == 0) {
      cout << PHWHERE << "emcClusterContainer Node missing.  Abort." << endl;
      exit(1);
   }

   int run = runheader->get_RunNumber();
   int run_year;
   if      ( 88115 <= run && run <=  92446) run_year = 3;
   else if (168000 <= run && run <= 180000) run_year = 5;
   else return EVENT_OK;

   Int_t Nclus = emcclustercontainer->size();
   for (Int_t iclus = 0; iclus < Nclus; iclus++) {
      emcClusterContent* clus = emcclustercontainer->getCluster(iclus);
      int arm     = clus->arm();
      int sector  = clus->sector();
      int armsect = 4 * arm + sector;
//      Int_t ypos   = clus->iypos();
//      Int_t zpos   = clus->izpos();
      Float_t ecore = clus->ecore();

      if      (run_year == 3) RecalRun3(ecore, armsect);
      else if (run_year == 5) RecalRun5(run, ecore, armsect);

      clus->set_ecore(ecore);
   }

   return 0;
}

void RecalEneNonLinearity::RecalRun3(float& ecore, int armsect)
{
   float ecore_org = ecore;

   // 1st step (PbGl only)
   if (IsPbGl(armsect)) {
      const float a0 = 9.62944e-01;
      const float a1 = 1.35644e-01;
      const float a2 = -1.10904e+00;
      
      // new energy scale correction
      float fCorrFacPP = a0+a1*exp(a2*ecore);
      ecore *= fCorrFacPP;
   }
   // 2nd step
   float ak_0 = 1;
   float ak_1 = 0;
   float ak_2 = 0;
   if        (armsect == 4) { // E0
      ak_0 = 1.01507;
      ak_1 =-3.00000e-02; 
      ak_2 =-7.27427e-01;
   } else if (armsect == 5) {
      ak_0 = 1.01000e+00;
      ak_1 =-3.74228e-02;
      ak_2 =-1.77314e+00;
   } else if (armsect == 6) {
      ak_0 = 9.96038e-01;
      ak_1 = 7.00000e-02;
      ak_2 =-1.77171e+00;
   } else if (armsect == 7) {
      ak_0 = 1.00000e+00;
      ak_1 = 6.00000e-02;
      ak_2 =-1.21716e+00;
   } else if (armsect == 0) { // W0
      ak_0 = 9.98415e-01;
      ak_1 = 5.00000e-02;
      ak_2 =-1.03033e+00;
   } else if (armsect == 1) {
      ak_0 = 1.00000e+00;
      ak_1 = 4.00000e-02;
      ak_2 =-6.70500e-01;
   } else if (armsect == 2) {
      ak_0 = 9.80000e-01;
      ak_1 = 5.00000e-02;
      ak_2 =-6.52392e-01;
   } else if (armsect == 3) {
      ak_0 = 1.00000e+00;
      ak_1 = 3.00000e-02;
      ak_2 =-8.92531e-01;
   }

   const float fCorrFac_ak = ak_0 + ak_1 * exp(ak_2 * ecore);
   const float glob_scale = 0.99;
   ecore *= fCorrFac_ak * glob_scale;

   if (Verbosity() > 0) {
      cout << armsect << ":  "
           << ecore_org << " -> " << ecore << endl;
   }
}

void RecalEneNonLinearity::RecalRun5(int run, float& ecore, int armsect)
{
//   float ecore_org = ecore;

   ////
   //// run-by-run energy scale correction
   ////  from /ccj/w/r01/okada/work/12122005/offline/src_Mystruct/Mystruct.C
   ////
   const int N_RUN_PERIOD = 10;
   const int RUN_PERIOD[N_RUN_PERIOD + 1] = {
      168000, 170021, 170695, 171210, 173345,
      174696, 175348, 177000, 179000, 179500, 180000
   };
   const float RUN_FACT[N_ARMSECT][N_RUN_PERIOD] = {
      { 1.00941, 1.00615, 1.00312, 1.00188 , 1, 0.999597, 0.999061, 0.984671, 0.976019, 0.977254 },
      { 1.01547, 1.01259, 1.00666, 1.00266 , 1, 0.999083, 1.00017 , 0.989112, 0.981463, 0.983101 },
      { 1.01217, 1.00835, 1.00283, 1.00033 , 1, 1.00042 , 1.00248 , 0.991425, 0.984725, 0.982252 },
      { 1.01025, 1.01013, 1.0022 , 1.001   , 1, 0.998883, 1.00073 , 0.990836, 0.986888, 0.983577 },
      { 1.00577, 1.0111 , 1.00606, 0.995657, 1, 1.00197 , 0.999057, 0.997538, 0.991315, 0.992105 },
      { 1.00941, 1.00653, 1.00594, 0.998929, 1, 1.00195 , 1.0059  , 1.00362 , 0.998004, 0.991963 },
      { 1.00368, 1.00162, 1.00055, 1.00042 , 1, 1.00022 , 1.00067 , 0.99662 , 0.992117, 0.994355 },
      { 1.00518, 1.00552, 1.0043 , 1.00737 , 1, 0.992717, 0.999734, 0.998658, 0.996299, 0.994097 }
   };
   int armsect_sasha = armsect < 4  ?  armsect  :  11 - armsect;
   int run_prd = -1;
   for (int i = 0; i < N_RUN_PERIOD; i++) {
      if (RUN_PERIOD[i] <= run && run < RUN_PERIOD[i + 1]) {
         run_prd = i;
         break;
      }
   }
   if (run_prd == -1) {
      cout << "Run-by-run energy scale correction doesn't cover run " << run 
           << ".  Abort.";
      exit(1);
   }
   float fact_run = RUN_FACT[armsect_sasha][run_prd];
   ecore /= fact_run;

   ////
   //// non-linearity correction
   ////  also from /ccj/w/r01/okada/work/12122005/offline/src_Mystruct/Mystruct.C
   ////
//   float fact_non_lin = 1.0;
//   if (ecore > 0.1) {
//      if( ! IsPbGl(armsect)){
//         fact_non_lin = 1.012 - 0.0127 / ecore;
//      } else {
//         fact_non_lin = 1.033 - 0.0269 / ecore;
//      }
//      ecore /= fact_non_lin;
//   }
//
//   if (Verbosity() > 0) {
//      cout << run << "  " << armsect << ":  "
//           << ecore_org << " / " << fact_run << " / " << fact_non_lin << " = "
//           << ecore << endl;
//   }


   //// from pi0 analysis
//   if (IsPbGl(armsect)) {
//      corr = 0.021 + (1 - 0.02 / ecore);
//   } else {
//      corr = 0.003 + (1 - 0.01 / ecore);
//   }
//   ecore /= corr;
}
