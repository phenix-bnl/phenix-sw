#include <iostream>

//INCLUDECHECKER: Removed this line: #include "phool.h"
#include "Event.h"

#include "FclIndexer.h"
#include "FclCalib.h"
#include "FclRaw.h"
#include "FclOutv1.h"

ClassImp(FclOutv1)

using namespace std;

FclOutv1::FclOutv1()
{
  Reset();
  whichSide=-1;
}

void FclOutv1::Reset(){

  for (int channel = 0; channel < CHANTOT; channel++){
    setLowGain(channel,ADCZERO);
    setSumAll(ADCZERO);
    setSumGrey(ADCZERO);
  }
}

void FclOutv1::identify(ostream& out) const
{
  out<<"processed fcal data for the dst: FclOutv1"<<endl;
  return;
}

int FclOutv1::isValid() const
{
  if(whichSide==-1) return 0;
  return 1;
}

void FclOutv1::readData(Packet *packet, int iSide){
  
  whichSide = iSide;
  for (int channel = 0; channel < CHANTOT; channel++){
    setLowGain(channel, packet->iValue(channel,4)-packet->iValue(channel,2));
  } 
}

void FclOutv1::readData(FclRaw *data){
  
  whichSide = data->getSide();
  for (int channel = 0; channel < CHANTOT; channel++){
    setLowGain(channel, data->getLowGain(channel));
  } 
}

void FclOutv1::print() const{
  FclIndexer* indexer = FclIndexer::Instance();

  cout<<endl<<endl<<"FclOut object dump"<<endl;
  cout<<"side:"<<whichSide<<" sumAll:"<<sumAll<<" sumGrey:"<<sumGrey<<endl;
  cout<<"gain:       row:     col:"<<endl;
  for (int channel = 0; channel < CHANTOT; channel++){
    cout<<gain[channel]<<"  "<<indexer->getRow(whichSide,channel);
    cout<<"  "<<indexer->getColumn(whichSide,channel)<<endl;
  } 
}

float FclOutv1::getLowGain(int row, int col){
  FclIndexer* indexer = FclIndexer::Instance();

  for (int channel = 0; channel < CHANTOT; channel++){
    if(indexer->getRow(whichSide,channel)==row &&
       indexer->getColumn(whichSide,channel)==col)
      {
	return gain[channel];
      }
  } 
  return -1;
}

void FclOutv1::calibrateData(FclCalib &calib){
  FclIndexer* indexer = FclIndexer::Instance();
  
  for (int channel = 0; channel < CHANTOT; channel++){
    if(indexer->getRow(whichSide,channel)>-1 &&
       indexer->getRow(whichSide,channel)<ROWUSE && 
       indexer->getColumn(whichSide,channel)>-1 &&
       indexer->getColumn(whichSide,channel)<COLUSE)
      {
	if(calib.getCalib(channel)<=0)
	{
	  setLowGain(channel, 0.0);
	}else{
	  setLowGain(channel, getLowGain(channel)/calib.getCalib(channel));
	}
      }
  } 
}

int FclOutv1::chanInBounds(int channel){
  if (channel<CHANTOT && channel>=0) return 1;
  else return 0;
}


int FclOutv1::computeSums(){
  float grey=0;
  float all=0;
  FclIndexer* indexer = FclIndexer::Instance();

  for(int ch=0;ch<CHANTOT;ch++){
    if(indexer->getRow(whichSide,ch)<ROWUSE &&
       indexer->getRow(whichSide,ch)>-1 &&
       indexer->getColumn(whichSide,ch)<COLUSE &&
       indexer->getColumn(whichSide,ch)>-1){
      all=all+gain[ch];
      int column = indexer->getColumn(whichSide,ch);
      int row = indexer->getRow(whichSide,ch);
      if(column>3
	 ||(column==3&&row!=3&&row!=4&&row!=5&&row!=6)
	 ){
	grey=grey+gain[ch];
      }
    }
  }
  sumAll=all;
  sumGrey=grey;
  return 0;
}






