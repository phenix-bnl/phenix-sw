// $Id: SvxGetGEA.cc,v 1.7 2011/08/12 20:19:15 akiba Exp $ 
/*!
	\file SvxGetGEA.cc
	\brief put root Pisa forward vertex hits into Phool compliant structure
	\version $Revision: 1.7 $
	\date $Date: 2011/08/12 20:19:15 $
*/



#include <SvxPISAHit.h>
#include <SvxPisaHit.h>
#include <getClass.h>

#include <cstdlib>
#include <iostream>
#include <iomanip>
using namespace std;

//_____________________________________________________
long SvxGetGEA(PHCompositeNode* topNode)
{
	
	SvxPISAHit *event = SvxPISAHit::GetSvxHitEvt();
	Int_t svxRows = SvxPISAHit::GetSvxCount();    // variable number of rows
	
	SvxPisaHit* svx = findNode::getClass<SvxPisaHit>(topNode,"SvxPisaHit");
	if (!svx) {
		cerr << "\n SvxGetGEA<E>: unable to find pointer tro SvxPisaHit object. " ;
		cerr << " program is exiting at this point. " << endl;
		exit(2);
	}
	
	
	for(int i=0; i<svxRows; i++) {
		svx->AddPisaHit(i);
		svx->SetnHit(i+1);
		(svx->GetHit(i))->SetMctrack (event[i].GetMctrack());
		(svx->GetHit(i))->SetIdPart (event[i].GetIdPart());
		
		// global "mid" point and momentum
		(svx->GetHit(i))->SetXGlobal (event[i].GetX());
		(svx->GetHit(i))->SetYGlobal (event[i].GetY());
		(svx->GetHit(i))->SetZGlobal (event[i].GetZ());
		(svx->GetHit(i))->SetDele (event[i].GetDele());
		(svx->GetHit(i))->SetPmomX (event[i].GetPx());
		(svx->GetHit(i))->SetPmomY (event[i].GetPy());
		(svx->GetHit(i))->SetPmomZ (event[i].GetPz());
		(svx->GetHit(i))->SetTof (event[i].GetTof());
		
		// local input and output points in detector volume
		(svx->GetHit(i))->SetXLocalIn (event[i].GetXLI());
		(svx->GetHit(i))->SetYLocalIn (event[i].GetYLI());
		(svx->GetHit(i))->SetZLocalIn (event[i].GetZLI());
		
		(svx->GetHit(i))->SetXLocalOut (event[i].GetXLO());
		(svx->GetHit(i))->SetYLocalOut (event[i].GetYLO());
		(svx->GetHit(i))->SetZLocalOut (event[i].GetZLO());
		
		// global input and output points in detector volume
		(svx->GetHit(i))->SetXGlobalIn (event[i].GetXGI());
		(svx->GetHit(i))->SetYGlobalIn (event[i].GetYGI());
		(svx->GetHit(i))->SetZGlobalIn (event[i].GetZGI());
		
		(svx->GetHit(i))->SetXGlobalOut (event[i].GetXGO());
		(svx->GetHit(i))->SetYGlobalOut (event[i].GetYGO());
		(svx->GetHit(i))->SetZGlobalOut (event[i].GetZGO());
		
		// volume id
		(svx->GetHit(i))->SetHitVolume (0, event[i].GetHitVol0());
		(svx->GetHit(i))->SetHitVolume (1, event[i].GetHitVol1());
		(svx->GetHit(i))->SetHitVolume (2, event[i].GetHitVol2());
		(svx->GetHit(i))->SetHitVolume (3, event[i].GetHitVol3());
		(svx->GetHit(i))->SetHitVolume (4, event[i].GetHitVol4());
		(svx->GetHit(i))->SetHitVolume (5, event[i].GetHitVol5());
		//Fvtx part
                (svx->GetHit(i))->SetHitVolume (6, event[i].GetHitVol6());
                (svx->GetHit(i))->SetHitVolume (7, event[i].GetHitVol7());
                (svx->GetHit(i))->SetHitVolume (8, event[i].GetHitVol8());
	
		// track and event id
		(svx->GetHit(i))->SetTrack (event[i].GetNtrack());
		(svx->GetHit(i))->SetLayer (event[i].GetLayer());
		(svx->GetHit(i))->SetIsubevent (event[i].GetIsubevent());
		(svx->GetHit(i))->SetNfile (event[i].GetNfile());

/*
                cout<<setprecision(4);
                cout<<"Global x:y:z="<<setw(7)<<event[i].GetX()<<",";
                cout<<setw(7)<<event[i].GetY()<<","<<setw(7)<<event[i].GetZ()<<" ";
                cout<<"LocalIn x:y:z="<<setw(7)<<event[i].GetXLI()<<",";
		cout<<setw(7)<<event[i].GetYLI()<<","<<setw(7)<<event[i].GetZLI()<<" ";
                cout<<"GlobalIn x:y:z="<<setw(7)<<event[i].GetXGI()<<",";
		cout<<setw(7)<<event[i].GetYGI()<<","<<setw(7)<<event[i].GetZGI()<<" ";
                cout<<endl;
*/

/*
    { // To check
      cout<<"  Layer="<<", Ladder="<<", Sensor= ";
      cout<<setprecision(4);
      cout<<"    Global x:y:z="<<setw(6)<<event[i].GetX()<<",";
      cout<<setw(6)<<event[i].GetY()<<","<<setw(6)<<event[i].GetZ()<<" ";
      cout<<"    GlobalIn x:y:z="<<setw(6)<<event[i].GetXGI()<<",";
      cout<<setw(6)<<event[i].GetYGI()<<","<<setw(6)<<event[i].GetZGI()<<" ";
      cout<<"    GlobalOut x:y:z="<<setw(6)<<event[i].GetXGO()<<",";
      cout<<setw(6)<<event[i].GetYGO()<<","<<setw(6)<<event[i].GetZGO()<<" ";
      cout<<"LocalIn x:y:z="<<setw(6)<<event[i].GetXLI()<<",";
      cout<<setw(6)<<event[i].GetYLI()<<","<<setw(6)<<event[i].GetZLI()<<" ";
      cout<<"LocalOut x:y:z="<<setw(6)<<event[i].GetXLO()<<",";
      cout<<setw(6)<<event[i].GetYLO()<<","<<setw(6)<<event[i].GetZLO()<<" ";
      cout<<"Hit sec:ix:iz:chan0:chan1=";
      cout<<endl;
    }
*/

	}
	
	return 0;
}
