#include <L2TofTwoHitTriggerMicroV1.h>
#include <L2TofHitMicroV1.h>

ClassImp(L2TofTwoHitTriggerMicroV1);

using namespace std;

//
//______________________________________________________
L2TofTwoHitTriggerMicroV1::L2TofTwoHitTriggerMicroV1()
{
  numSlatsHit=0;
  L2TofHits = new TClonesArray("L2TofHitMicroV1",100);
}

//
//______________________________________________________
L2TofTwoHitTriggerMicroV1::~L2TofTwoHitTriggerMicroV1()
{
  L2TofHits->Clear();
}

//
//______________________________________________________
void L2TofTwoHitTriggerMicroV1::Reset()
{
  L2TofHits->Clear();
  numSlatsHit = 0;
}

//
//______________________________________________________
int L2TofTwoHitTriggerMicroV1::isValid() const
{
  return ((numSlatsHit>0) ? 1 : 0);
}

//
//______________________________________________________
void L2TofTwoHitTriggerMicroV1::addHit(int index)
{
  TClonesArray &tmpList = *L2TofHits;
  new (tmpList[index]) L2TofHitMicroV1();
  numSlatsHit++;
}

//
//______________________________________________________
void L2TofTwoHitTriggerMicroV1::set_SlatID(int index, int id)
{
  L2TofHitMicroV1 *tofHit = (L2TofHitMicroV1*) L2TofHits->At(index);

  if(tofHit)
    {
      tofHit->set_SlatID(id);
    } else {
      cout<<PHWHERE<<"ERROR no L2TofHitMicroV1 found"<<endl;
    }
}

//
//______________________________________________________
int L2TofTwoHitTriggerMicroV1::get_SlatID(int index)
{
  L2TofHitMicroV1 *tofHit = (L2TofHitMicroV1*) L2TofHits->At(index);
  return ((tofHit) ? tofHit->get_SlatID() : -9999);
}

//
//______________________________________________________
void L2TofTwoHitTriggerMicroV1::set_EnergyLoss(int index, float loss)
{
  L2TofHitMicroV1 *tofHit = (L2TofHitMicroV1*) L2TofHits->At(index);

  if(tofHit)
    {
      tofHit->set_EnergyLoss(loss);
    } else {
      cout<<PHWHERE<<"ERROR no L2TofHitMicroV1 found"<<endl;
    }
}

//
//______________________________________________________
float L2TofTwoHitTriggerMicroV1::get_EnergyLoss(int index)
{
  L2TofHitMicroV1 *tofHit = (L2TofHitMicroV1*) L2TofHits->At(index);
  return ((tofHit) ? tofHit->get_EnergyLoss() : -9999);
}

//
//______________________________________________________
void L2TofTwoHitTriggerMicroV1::set_ySlat(int index, float y)
{
  L2TofHitMicroV1 *tofHit = (L2TofHitMicroV1*) L2TofHits->At(index);

  if(tofHit)
    {
      tofHit->set_ySlat(y);
    } else {
      cout<<PHWHERE<<"ERROR no L2TofHitMicroV1 found"<<endl;
    }
}

//
//______________________________________________________
float L2TofTwoHitTriggerMicroV1::get_ySlat(int index)
{
  L2TofHitMicroV1 *tofHit = (L2TofHitMicroV1*) L2TofHits->At(index);
  return ((tofHit) ? tofHit->get_ySlat() : -9999);
}
