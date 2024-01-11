#include "BbcEventStorage.hh"
#include "BbcEvent.hh"
#include "TFile.h"
#include "TNtuple.h"

using namespace std;

BbcEventStorage::BbcEventStorage(void): isStorage(false){
}

BbcEventStorage::BbcEventStorage(const char* const file): isStorage(true)
{
  hfile = new TFile(file,"RECREATE","Results of BBC");
  bbcntuple0=new TNtuple("bbcntuple0","End Products of BBC",
			 "EV:TZ:ZV:HN:HS:CN:CS:TN:TS");
  bbcntuple1=new TNtuple("bbcntuple1","PMTs Products of BBC",
			 "Event:nPMT:Charge:Time0:Time1:ADC:TDC0:TDC1");
}

BbcEventStorage::~BbcEventStorage(void)
{
  if(isStorage==true)  hfile->Write();
}

void 
BbcEventStorage::fill(const BbcEvent& bbc)
{
  bbcntuple0->Fill((float)bbc.getEventNumber(),
		   bbc.getTimeZero(),
		   bbc.getZVertex(),
		   (float)bbc.getnHitPmt(Bbc::North),
		   (float)bbc.getnHitPmt(Bbc::South),
		   bbc.getChargeSum(Bbc::North),
		   bbc.getChargeSum(Bbc::South),
		   bbc.getArmHitTime(Bbc::North),
		   bbc.getArmHitTime(Bbc::South));


  for(int i=0; i<128; i++){
    bbcntuple1->Fill(bbc.getEventNumber(),
		     (float) i,
		     bbc.getCharge(i),
		     bbc.getHitTime0(i)-bbc.TimeLagOfTransitTime(i,bbc.getZVertex()),
		     bbc.getHitTime1(i),
		     (float) bbc.getAdc(i),
		     (float) bbc.getTdc0(i),
		     (float) bbc.getTdc1(i));
  }
}

void BbcEventStorage::print(const BbcEvent& bbc, ostream& output, const int PrintLevel){
  const BbcEvent* thisEvent=&bbc;
  if(PrintLevel==0)return;  // no display
  output << "Event No. : " << thisEvent->getEventNumber()
	 << "  ===================================" << endl;
  if(PrintLevel<=1)return;
  output << "    Time Zero = " << setw(5) << thisEvent->getTimeZero()
	 << " (" << setw(5) << thisEvent->getTimeZeroError()
	 << ") [ps]" << endl;
  output << "    Z-vertex  = " << setw(5) << thisEvent->getZVertex()
	 << " (" << setw(5) << thisEvent->getZVertexError()
	 << ") [cm]" << endl;
  if(PrintLevel<=2)return;
  output << endl;
  output << "=== arm product ===="
	 << "          (south)           "
	 << "              (north)" << endl;
  output << "    Number of Hits  |"
	 << "        = " << setw(8) << thisEvent->getnHitPmt(Bbc::South)
	 << setw(13) << ""
	 << "        = " << setw(8) << thisEvent->getnHitPmt(Bbc::North)
	 << endl;
  output << "    Hit Time        |"
	 << "        = " << setw(8) << thisEvent->getArmHitTime(Bbc::South)
	 << " (" << setw(5) << thisEvent->getArmHitTimeError(Bbc::South)
	 << ") [ps]"
	 << "       = " << setw(8) << thisEvent->getArmHitTime(Bbc::North)
	 << " (" << setw(5) << thisEvent->getArmHitTimeError(Bbc::North)
	 << ") [ps]"
	 << endl;
  output << "    Charge Sum      |"
	 << "        = " << setw(8) << thisEvent->getChargeSum(Bbc::South)
	 << setw(13) << ""
	 << "        = " << setw(8) << thisEvent->getChargeSum(Bbc::North)
	 << endl;
  if(PrintLevel<=5)return;
  output << endl;
  output << "=== hit data ======================" << endl;
  for(int i=0; i<4; i++){
    output << setw(3) << "No." << " : "	 << setw(9) << "Charge"
	   << setw(9) << "Time0"	 << setw(9) << "Time1";
    if(i==3)   output << endl;
    else       output << "    ";
  }
  for(int i=0; i<4; i++){
    output << setw(3) << "---" << "   "	 << setw(9) << "--------"
	   << setw(9) << "--------" << setw(9) << "--------";
    if(i==3)   output << endl;
    else       output << "    ";
  }
  for(int j=0; j<32; j++){
    for(int i=0; i<4; i++){
      int k=j+32*i;
      output << setw(3) << k +1 << " :  "
	     << setw(8) << thisEvent->getCharge(k) << " "
	     << setw(8) << thisEvent->getHitTime0(k) << " " 
	     << setw(8) << thisEvent->getHitTime1(k);
      if(i==3)   output << endl;
      else       output << "    ";
    }
  }
  output << "=== raw data ======================" << endl;
  if(PrintLevel<=7)return;
  for(int i=0; i<4; i++){
    output << setw(3) << "No." << " : "	 << setw(5) << "ADC"
	   << setw(5) << "TDC0"	 << setw(5) << "TDC1";
    if(i==3)   output << endl;
    else       output << "    ";
  }

  for(int i=0; i<32; i++){
    for(int j=0; j<4; j++){
      int k=i+j*32;
      output << setw(3) << k +1 << " : "	   
	     << setw(5) << thisEvent->getAdc(k)
	     << setw(5) << thisEvent->getTdc0(k)
	     << setw(5) << thisEvent->getTdc1(k);
      if(i==3)   output << endl;
      else       output << "    ";
    }
  }
  return;
}
