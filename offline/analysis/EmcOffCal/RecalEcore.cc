#include "RecalEcore.h"

#include <TVector3.h>

#include <PHNodeIterator.h>
#include <PHTypedNodeIterator.h>
#include <PHCompositeNode.h>
#include <PHIODataNode.h>

#include <Fun4AllReturnCodes.h>
#include <Fun4AllServer.h>
#include <recoConsts.h>
#include <emcClusterContainer.h>
#include <emcClusterContentv5.h>
#include <getClass.h>

#include "Coefficient.h"

#include <cstdlib>
#include <iostream>
#include <fstream>

using namespace std;

RecalEcore::RecalEcore(const char *name)
{
   ThisName = name;
   m_coef = new Coefficient();
   m_cluscont = 0;
}

int RecalEcore::Init(PHCompositeNode *topNode)
{
   return EVENT_OK;
}

int RecalEcore::InitRun(PHCompositeNode *topNode)
{
   if (Verbosity() > 0) {
      recoConsts *rc = recoConsts::instance();
      // this rc flag is set by the framework
      cout << "RecalEcore: calling InitRun for Run"
           << rc->get_IntFlag("RUNNUMBER") << endl;
   }
   return EVENT_OK;
}

int RecalEcore::process_event(PHCompositeNode *topNode)
{
   if (Verbosity() > 0) cout << "RecalEcore: Calling process_event" << endl;
   
   //// get nodes
   emcClusterContainer *emcclustercontainer = 
      findNode::getClass<emcClusterContainer>(topNode, "emcClusterContainer");
   if (emcclustercontainer == 0) {
      cout << PHWHERE << "emcClusterContainer Node missing.  Abort."
           << endl;
      exit(1);
   }
   
   //// recal
   Int_t n_clus = emcclustercontainer->size();
   for (Int_t iclus = 0; iclus < n_clus; iclus++) {
      emcClusterContent* cluscont = emcclustercontainer->getCluster(iclus);
      ExecRecal(cluscont);
   }
   
   return EVENT_OK;
}

int RecalEcore::End(PHCompositeNode *topNode)
{
   if (Verbosity() > 0) cout << "Calling End" << endl;
   if (m_cluscont) delete m_cluscont;

   return EVENT_OK;
}

void RecalEcore::ReadCoef(const Char_t* coef_table, const Char_t* coef_table_supermod)
{
  m_coef->ReadCoef(coef_table,coef_table_supermod);
}

double RecalEcore::GetCoef(const int as, const int y, const int z)
{
   return m_coef->GetCoef(as, y, z);
}

void RecalEcore::ExecRecal(emcClusterContent* cluscont)
{
   Double_t e_org  = 0.0;
   Double_t e_corr = 0.0;
   
   Int_t multiplicity = cluscont->multiplicity();
   for (Int_t imulti = 0; imulti < multiplicity; imulti++) {
      Int_t towerid = cluscont->towerid(imulti);
      Double_t coef = m_coef->GetCoef(towerid);
      
      Float_t parte;
      if (imulti == 0) 
         parte = cluscont->partesum(imulti);
      else 
         parte = cluscont->partesum(imulti) - cluscont->partesum(imulti-1);

      e_org  += parte;
      e_corr += parte * coef;
     }

   Double_t ecore_org = cluscont->ecore();
   Double_t ecore_recal = ecore_org * e_corr / e_org;
   cluscont->set_ecore(ecore_recal);

   if (Verbosity() > 0) {
      int armsect = 4 * cluscont->arm() + cluscont->sector();
      int ypos = cluscont->iypos();
      int zpos = cluscont->izpos();
      cout << armsect << "  " << ypos << "  " << zpos << "  "
           << ecore_org << "  " << ecore_recal << endl;
   }
}

void RecalEcore::ExecRecal(Int_t n_clus, Float_t* ecore, Int_t* multiplicity, Int_t* towerid, Float_t* partesum)
{
   Int_t i_multi_total = 0;
   for (int icl = 0; icl < n_clus; icl++) {
      Double_t e_org  = 0.0;
      Double_t e_corr = 0.0;
      for (int itower = 0; itower < multiplicity[icl]; itower++) {
         Double_t parte;
         if (itower == 0)
            parte = partesum[i_multi_total];
         else 
            parte = partesum[i_multi_total] - partesum[i_multi_total - 1];
         
         e_org  += parte;
         e_corr += parte * m_coef->GetCoef(towerid[i_multi_total]);
         i_multi_total++;
      }
      ecore[icl] *= e_corr / e_org;
   }
}
