#include "NCCPISAHit.h"
#include "NCCPisaHit.h"
#include "NCCSnglPisaHit.h"
typedef PHIODataNode <NCCPisaHit> NCCPisaHitNode_t;

int Fun4AllPisaInputManager::NCCConvert(PHCompositeNode* topNode)
{
  d_NCCPisaHit=0;
  PHTypedNodeIterator<NCCPisaHit> iNCC(topNode);
  NCCPisaHitNode_t *NCC = iNCC.find("NCCPisaHit"); 
  if(NCC) {
    d_NCCPisaHit=NCC->getData();
  }
  else{
    cout  << PHWHERE <<"Unable to find NCCPisaHit node; program exiting " << endl;
    exit(1);
  }

  NCCPISAHit *event = NCCPISAHit::GetNCCHitEvt();
  Int_t NCCRows = NCCPISAHit::GetNCCCount();   
  for(int i=0; i<NCCRows; i++) {
    d_NCCPisaHit->AddPisaHit(i);
    d_NCCPisaHit->SetnHit(i+1);
    d_NCCPisaHit->GetHit(i)->SetNCCCount(event[i]. GetNCCCount());
    d_NCCPisaHit->GetHit(i)->SetNCC1Count(event[i]. GetNCC1Count());
    d_NCCPisaHit->GetHit(i)->SetNCC2Count(event[i]. GetNCC2Count());
    d_NCCPisaHit->GetHit(i)->SetIsubevent(event[i]. GetIsubevent());
    d_NCCPisaHit->GetHit(i)->SetNtrack(event[i]. GetNtrack());
    d_NCCPisaHit->GetHit(i)->SetId(event[i]. GetId());
    d_NCCPisaHit->GetHit(i)->SetIarm(event[i]. GetIarm());
    d_NCCPisaHit->GetHit(i)->SetMctrack(event[i]. GetMctrack());
    d_NCCPisaHit->GetHit(i)->SetIncc(event[i]. GetIncc());
    d_NCCPisaHit->GetHit(i)->SetNEvent(event[i]. GetNEvent());
    d_NCCPisaHit->GetHit(i)->SetSENID(event[i]. GetSENID());
    d_NCCPisaHit->GetHit(i)->SetTWRID(event[i]. GetTWRID());
    d_NCCPisaHit->GetHit(i)->SetTOFIN(event[i]. GetTOFIN());
    d_NCCPisaHit->GetHit(i)->SetDedx(event[i]. GetDedx());
    d_NCCPisaHit->GetHit(i)->SetNfile(event[i]. GetNfile());
  }

 return 0;
}
