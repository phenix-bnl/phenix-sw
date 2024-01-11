#include <NCCPISAHit.h>
#include <NCCPisaHit.h>
#include <NCCGetGEA.h>
#include <NCCSnglPisaHit.h>

#include <MPCEXABSPISAHit.h>
#include <MPCEXABSPisaHit.h>
#include <MPCEXABSSnglPisaHit.h>

#include <MPCFPLTPISAHit.h>
#include <MPCFPLTPisaHit.h>
#include <MPCFPLTSnglPisaHit.h>

#include <MPCEXEntryPISAHit.h>
#include <MPCEXENTPisaHit.h>
#include <MPCEXENTSnglPisaHit.h>

#include <getClass.h>
#include <phool.h>
#include <PHCompositeNode.h>

#include <cstdlib>
#include <iostream>

using namespace std;

long
NCCGetGEA(PHCompositeNode* topNode)
{

  NCCPISAHit *event = NCCPISAHit::GetNCCHitEvt();
  Int_t nccRows = NCCPISAHit::GetNCC1Count() +  NCCPISAHit::GetNCC2Count();    // variable number of rows

  //cout << "NCC:" <<  NCCPISAHit::GetNCCCount() << " " <<  NCCPISAHit::GetNCC1Count() << " " <<  NCCPISAHit::GetNCC2Count() << endl; 
  //cout << " nccRows = " << nccRows << endl; 

  NCCPisaHit* ncc = findNode::getClass<NCCPisaHit>(topNode,"NCCPisaHit");
  if (!ncc) {
    cerr << "\n NCCGetGEA<E>: unable to find pointer to NCCPisaHit object. " ;
    cerr << " program is exiting at this point. " << endl;
    exit(2);
  }


  for(int i=0; i<nccRows; i++) {
    ncc->AddPisaHit(i);
    ncc->SetnHit(i+1);
    (ncc->GetHit(i))->SetNCCCount(event[i]. GetNCCCount());
    (ncc->GetHit(i))->SetNCC1Count(event[i]. GetNCC1Count());
    (ncc->GetHit(i))->SetNCC2Count(event[i]. GetNCC2Count());
    (ncc->GetHit(i))->SetIsubevent(event[i]. GetIsubevent());
    (ncc->GetHit(i))->SetNtrack(event[i]. GetNtrack());
    (ncc->GetHit(i))->SetId(event[i]. GetId());
    (ncc->GetHit(i))->SetIarm(event[i]. GetIarm());
    (ncc->GetHit(i))->SetMctrack(event[i]. GetMctrack());
    (ncc->GetHit(i))->SetIncc(event[i]. GetIncc());
    (ncc->GetHit(i))->SetNEvent(event[i]. GetNEvent());
    (ncc->GetHit(i))->SetSENID(event[i]. GetSENID());
    (ncc->GetHit(i))->SetTWRID(event[i]. GetTWRID());
    (ncc->GetHit(i))->SetTOFIN(event[i]. GetTOFIN());
    (ncc->GetHit(i))->SetDedx(event[i]. GetDedx());
    (ncc->GetHit(i))->SetNfile(event[i]. GetNfile());
  }

  // Get absorber hits as well

  MPCEXABSPISAHit *abs_event = MPCEXABSPISAHit::GetMPCEXABSHitEvt();
  Int_t abs_nccRows = MPCEXABSPISAHit::GetMPCEXABS1Count() +  MPCEXABSPISAHit::GetMPCEXABS2Count();    // variable number of rows

  //cout << "MPCEXABS:" <<  MPCEXABSPISAHit::GetMPCEXABSCount() << " " <<  MPCEXABSPISAHit::GetMPCEXABS1Count() << " " <<  MPCEXABSPISAHit::GetMPCEXABS2Count() << endl; 
  //cout << " abs_nccRows = " << abs_nccRows << endl; 

  MPCEXABSPisaHit* abs_ncc = findNode::getClass<MPCEXABSPisaHit>(topNode,"MPCEXABSPisaHit");
  if (!abs_ncc) {
    cerr << "\n NCCGetGEA<E>: unable to find pointer to MPCEXABSPisaHit object. " ;
    cerr << " program is exiting at this point. " << endl;
    exit(2);
  }

  for(int i=0; i<abs_nccRows; i++) {
    abs_ncc->AddPisaHit(i);
    abs_ncc->SetnHit(i+1);
    (abs_ncc->GetHit(i))->SetMPCEXABSCount(abs_event[i]. GetMPCEXABSCount());
    (abs_ncc->GetHit(i))->SetMPCEXABS1Count(abs_event[i]. GetMPCEXABS1Count());
    (abs_ncc->GetHit(i))->SetMPCEXABS2Count(abs_event[i]. GetMPCEXABS2Count());
    (abs_ncc->GetHit(i))->SetIsubevent(abs_event[i]. GetIsubevent());
    (abs_ncc->GetHit(i))->SetNtrack(abs_event[i]. GetNtrack());
    (abs_ncc->GetHit(i))->SetId(abs_event[i]. GetId());
    (abs_ncc->GetHit(i))->SetIarm(abs_event[i]. GetIarm());
    (abs_ncc->GetHit(i))->SetMctrack(abs_event[i]. GetMctrack());
    (abs_ncc->GetHit(i))->SetIncc(abs_event[i]. GetIncc());
    (abs_ncc->GetHit(i))->SetNEvent(abs_event[i]. GetNEvent());
    (abs_ncc->GetHit(i))->SetDedx(abs_event[i]. GetDedx());
    (abs_ncc->GetHit(i))->SetNfile(abs_event[i]. GetNfile());
  }

  // Get MPC Al front plate hits

  MPCFPLTPISAHit *fplt_event = MPCFPLTPISAHit::GetMPCFPLTHitEvt();
  Int_t fplt_nccRows = MPCFPLTPISAHit::GetMPCFPLT1Count() +  MPCFPLTPISAHit::GetMPCFPLT2Count();    // variable number of rows

  //cout << "MPCFPLT:" <<  MPCFPLTPISAHit::GetMPCFPLTCount() << " " <<  MPCFPLTPISAHit::GetMPCFPLT1Count() << " " <<  MPCFPLTPISAHit::GetMPCFPLT2Count() << endl; 
  //cout << " fplt_nccRows = " << fplt_nccRows << endl; 

  MPCFPLTPisaHit* fplt_ncc = findNode::getClass<MPCFPLTPisaHit>(topNode,"MPCFPLTPisaHit");
  if (!fplt_ncc) {
    cerr << "\n NCCGetGEA<E>: unable to find pointer to MPCFPLTPisaHit object. " ;
    cerr << " program is exiting at this point. " << endl;
    exit(2);
  }

  for(int i=0; i<fplt_nccRows; i++) {
    fplt_ncc->AddPisaHit(i);
    fplt_ncc->SetnHit(i+1);
    (fplt_ncc->GetHit(i))->SetMPCFPLTCount(fplt_event[i]. GetMPCFPLTCount());
    (fplt_ncc->GetHit(i))->SetMPCFPLT1Count(fplt_event[i]. GetMPCFPLT1Count());
    (fplt_ncc->GetHit(i))->SetMPCFPLT2Count(fplt_event[i]. GetMPCFPLT2Count());
    (fplt_ncc->GetHit(i))->SetIsubevent(fplt_event[i]. GetIsubevent());
    (fplt_ncc->GetHit(i))->SetNtrack(fplt_event[i]. GetNtrack());
    (fplt_ncc->GetHit(i))->SetId(fplt_event[i]. GetId());
    (fplt_ncc->GetHit(i))->SetIarm(fplt_event[i]. GetIarm());
    (fplt_ncc->GetHit(i))->SetMctrack(fplt_event[i]. GetMctrack());
    (fplt_ncc->GetHit(i))->SetIncc(fplt_event[i]. GetIncc());
    (fplt_ncc->GetHit(i))->SetNEvent(fplt_event[i]. GetNEvent());
    (fplt_ncc->GetHit(i))->SetDedx(fplt_event[i]. GetDedx());
    (fplt_ncc->GetHit(i))->SetNfile(fplt_event[i]. GetNfile());
  }

  // Get MPC-EX entry particles

  MPCEXEntryPISAHit *exent_event = MPCEXEntryPISAHit::GetMPCEXEntryHitEvt();
  Int_t exent_nccRows = MPCEXEntryPISAHit::GetMPCEXEntryCount();

  MPCEXENTPisaHit* exent_ncc = findNode::getClass<MPCEXENTPisaHit>(topNode,"MPCEXENTPisaHit");
  if (!exent_ncc) {
    cerr << "\n NCCGetGEA<E>: unable to find pointer to MPCEXENTPisaHit object. " ;
    //cerr << " program is exiting at this point. " << endl;
    //exit(2);
  }
  else{

    //cout << " Number of entry particles = " << exent_nccRows << endl; 

    for(int i=0; i<exent_nccRows; i++) {
      exent_ncc->AddPisaHit(i);
      exent_ncc->SetnHit(i+1);
      (exent_ncc->GetHit(i))->SetMPCEXENTCount(exent_event[i]. GetMPCEXEntryCount());
      (exent_ncc->GetHit(i))->SetMctrack(exent_event[i]. GetMctrack());
      (exent_ncc->GetHit(i))->SetVx(exent_event[i]. GetVx());
      (exent_ncc->GetHit(i))->SetVy(exent_event[i]. GetVy());
      (exent_ncc->GetHit(i))->SetVz(exent_event[i]. GetVz());
      (exent_ncc->GetHit(i))->SetPx(exent_event[i]. GetPx());
      (exent_ncc->GetHit(i))->SetPy(exent_event[i]. GetPy());
      (exent_ncc->GetHit(i))->SetPz(exent_event[i]. GetPz());
      (exent_ncc->GetHit(i))->SetIsubevent(exent_event[i]. GetIsubevent());
      (exent_ncc->GetHit(i))->SetNtrack(exent_event[i]. GetNtrack());
      (exent_ncc->GetHit(i))->SetNfile(exent_event[i]. GetNfile());

      //cout << " vx = " << exent_event[i]. GetVx() << " vy = " << exent_event[i]. GetVy() << " vz = " << exent_event[i]. GetVz()
      //    << " px = " << exent_event[i]. GetPx() << " py = " << exent_event[i]. GetPy() << " pz = " << exent_event[i]. GetPz()
      //    << " id = " << exent_event[i]. GetId() << endl;

    }
  }

 return 0;
}
