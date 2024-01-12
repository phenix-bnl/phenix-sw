#include "TH1.h"
#include "iostream.h"

float EvtMix(TH1F* HlikeSignP, 
	     TH1F* HlikeSignM, 
	     TH1F* HFlikeSignP, 
	     TH1F* HFlikeSignM, 
	     TH1F* HFoppSign, 
	     Float_t xMin, Float_t xMax)
{
   cout<<endl<<"=== START COMPUTING NORMALISATION ==="<<endl;
   cout<<endl<<"======== over the entire mass range :"<<endl;
   cout<<" number of fake ++ dimuons = "<<HFlikeSignP->GetEntries()<<endl;
   cout<<" number of fake -- dimuons = "<<HFlikeSignM->GetEntries()<<endl;
   cout<<" number of fake +- dimuons = "<<HFoppSign->GetEntries()<<endl;
   float Rfactor = HFoppSign->GetEntries()/(2*sqrt(HFlikeSignP->GetEntries()*HFlikeSignM->GetEntries()));
   cout<<"N+- = "<<Rfactor<<" x 2 x sqrt("<<HFlikeSignP->GetEntries()<<" x "<<HFlikeSignM->GetEntries()<<")"<<endl;
   //
   cout<<endl<<"======== over the range  ["<<xMin<<","<<xMax<<"] GeV :"<<endl; 
   Int_t binMin = HFlikeSignP->FindBin(xMin); Int_t binMax = HFlikeSignP->FindBin(xMax);
   cout<<" number of fake ++ dimuons = "<<HFlikeSignP->Integral(binMin,binMax)<<endl;
   cout<<" number of fake -- dimuons = "<<HFlikeSignM->Integral(binMin,binMax)<<endl;
   cout<<" number of fake +- dimuons = "<<HFoppSign->Integral(binMin,binMax)<<endl;
   float RfactorR = HFoppSign->Integral(binMin,binMax)/
     (2*sqrt(HFlikeSignP->Integral(binMin,binMax)*HFlikeSignM->Integral(binMin,binMax)));
   cout<<"N+- = "<<RfactorR<<" x 2 x sqrt("<<HFlikeSignP->Integral(binMin,binMax)<<" x "
       <<HFlikeSignM->Integral(binMin,binMax)<<")"<<endl<<"==="<<endl;
   cout<<"number of true ++ dimuons = "<<HlikeSignP->Integral(binMin,binMax)<<endl;
   cout<<"number of true -- dimuons = "<<HlikeSignM->Integral(binMin,binMax)<<endl;
   Float_t scale = RfactorR*2*sqrt(HlikeSignP->Integral(binMin,binMax)*HlikeSignM->Integral(binMin,binMax)); 
   cout<<"combinatorial background normalisation : N+- = "
       <<RfactorR<<" x 2 x sqrt("<<HlikeSignP->Integral(binMin,binMax)
       <<" x "<<HlikeSignM->Integral(binMin,binMax)<<") = "<<scale<<endl;
   cout<<"fake +- spectrum scaling factor = "<<scale<<" / "<<HFoppSign->Integral(binMin,binMax)<<" = ";
   scale /= HFoppSign->Integral(binMin,binMax);
   cout<<scale<<endl<<endl;
   cout<<"=== NORMALISATION COMPUTING ENDED ==="<<endl<<endl;
   return scale;
//   
}

