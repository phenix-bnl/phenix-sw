#include "EmcCalibTowerv1.h"
#include "EmcSnglCalibTowerv1.h"
#include "dEmcCalibTowerWrapper.h"
//INCLUDECHECKER: Removed this line: #include "phool.h"
#include "TClonesArray.h"
#include <iostream>

ClassImp(EmcCalibTowerv1);

using namespace std;

static const unsigned int EMCNTOWER = 200;

EmcCalibTowerv1::EmcCalibTowerv1()
{
  EmcNTower = 0;
  EmcTower = new TClonesArray("EmcSnglCalibTowerv1",EMCNTOWER);
  return;
}

EmcCalibTowerv1::~EmcCalibTowerv1()
{
  EmcTower->Clear();
  return;
}

void EmcCalibTowerv1::identify(ostream& os) const
{
  os << "identify yourself: EmcCalibTowerv1 Object" << endl;
  os << "No of Towers: " << EmcNTower << endl;
  return;
}

void EmcCalibTowerv1::Reset()
{
  EmcTower->Clear();
  if (EmcNTower>EMCNTOWER)
    {
      EmcTower->Expand(EMCNTOWER);
    }
  EmcNTower = 0;
  return;
}

int EmcCalibTowerv1::isValid() const
{
  return((EmcNTower>0) ? 1 : 0);
}

void EmcCalibTowerv1::FillFromWrapper(dEmcCalibTowerWrapper *wrap)
{
  unsigned int iclus;
  int index;
  if (wrap)
    {
      EmcNTower = wrap->RowCount();
      set_TClonesArraySize(wrap->RowCount());
      for (iclus = 0;iclus < wrap->RowCount();iclus++)
        {
          AddEmcTower(iclus);
          set_id(iclus, wrap->get_id(iclus));
          set_type(iclus, wrap->get_type(iclus));

          set_deadmap(iclus, wrap->get_deadmap(iclus));
          set_hwkey(iclus, wrap->get_hwkey(iclus));
          index = 100000 * wrap->get_arm(iclus) +
	    10000 * wrap->get_sector(iclus) +
	    100 * wrap->get_ind(1, iclus) +
	    wrap->get_ind(0, iclus);
          set_index(iclus,index);
          set_swkey(iclus, wrap->get_swkey(iclus));
          set_warnmap(iclus, wrap->get_warnmap(iclus));

          set_adc(iclus, wrap->get_adc(iclus));
          set_ecal(iclus, wrap->get_ecal(iclus));
          set_tac(iclus, wrap->get_tac(iclus));
          set_tof(iclus, wrap->get_tof(iclus));
	}
    }
  return;
}

int EmcCalibTowerv1::set_TClonesArraySize(const unsigned int ntower)
{
  if (ntower > EMCNTOWER)
    {
      EmcTower->Expand(ntower);
    }
  return ntower;
}

void  EmcCalibTowerv1::AddEmcTower(const unsigned int itower)
{
  TClonesArray &emctower = *EmcTower;
  new(emctower[itower]) EmcSnglCalibTowerv1();
  return;
}

short EmcCalibTowerv1::get_arm(const unsigned int itower) const
{
  
  int key=get_index(itower);
  return key/100000;
  
}

short EmcCalibTowerv1::get_sector(const unsigned int itower) const
{

  int key=get_index(itower);
  return (key%100000)/10000;

}

short EmcCalibTowerv1::get_ind(const unsigned int itower, const short i) const
{
  // i==0 corresponds to the X direction in the local EMC sector frame
  // (which coincides with the Z direction in the global PHENIX frame)
  // i==1 corresponds to the Y direction in the local EMC sector fram

  int key=get_index(itower);

  if(i==0) return key%100;
  else if(i==1) return (key%10000)/100;

  cerr<<PHWHERE<<"index "<<i<<"out of range"<<endl;
  return -999;

}

short EmcCalibTowerv1::get_id(const unsigned int itower) const
{

  EmcSnglCalibTowerv1 *emcclus = (EmcSnglCalibTowerv1 *) GetEmcTower()->UncheckedAt(itower);
  return((emcclus) ? emcclus->get_id() : -999);
}

void EmcCalibTowerv1::set_id(const unsigned int itower, const short ival)
{
  EmcSnglCalibTowerv1 *emcclus = (EmcSnglCalibTowerv1 *) GetEmcTower()->UncheckedAt(itower);
  if (emcclus)
    {
      emcclus->set_id(ival);
    }
  else
    {
      cout << PHWHERE << "ERROR no EmcSnglCalibTowerv1 object found" << endl;
    }
  return;
}


short EmcCalibTowerv1::get_type(const unsigned int itower) const
{

  EmcSnglCalibTowerv1 *emcclus = (EmcSnglCalibTowerv1 *) GetEmcTower()->UncheckedAt(itower);
  return((emcclus) ? emcclus->get_type() : -999);
}

void EmcCalibTowerv1::set_type(const unsigned int itower, const short ival)
{
  EmcSnglCalibTowerv1 *emcclus = (EmcSnglCalibTowerv1 *) GetEmcTower()->UncheckedAt(itower);
  if (emcclus)
    {
      emcclus->set_type(ival);
    }
  else
    {
      cout << PHWHERE << "ERROR no EmcSnglCalibTowerv1 object found" << endl;
    }
  return;
}

int EmcCalibTowerv1::get_deadmap(const unsigned int itower) const
{

  EmcSnglCalibTowerv1 *emcclus = (EmcSnglCalibTowerv1 *) GetEmcTower()->UncheckedAt(itower);
  return((emcclus) ? emcclus->get_deadmap() : -999);
}

void EmcCalibTowerv1::set_deadmap(const unsigned int itower, const int ival)
{
  EmcSnglCalibTowerv1 *emcclus = (EmcSnglCalibTowerv1 *) GetEmcTower()->UncheckedAt(itower);
  if (emcclus)
    {
      emcclus->set_deadmap(ival);
    }
  else
    {
      cout << PHWHERE << "ERROR no EmcSnglCalibTowerv1 object found" << endl;
    }
  return;
}

int EmcCalibTowerv1::get_hwkey(const unsigned int itower) const
{

  EmcSnglCalibTowerv1 *emcclus = (EmcSnglCalibTowerv1 *) GetEmcTower()->UncheckedAt(itower);
  return((emcclus) ? emcclus->get_hwkey() : -999);
}

void EmcCalibTowerv1::set_hwkey(const unsigned int itower, const int ival)
{
  EmcSnglCalibTowerv1 *emcclus = (EmcSnglCalibTowerv1 *) GetEmcTower()->UncheckedAt(itower);
  if (emcclus)
    {
      emcclus->set_hwkey(ival);
    }
  else
    {
      cout << PHWHERE << "ERROR no EmcSnglCalibTowerv1 object found" << endl;
    }
  return;
}

int EmcCalibTowerv1::get_index(const unsigned int itower) const
{

  EmcSnglCalibTowerv1 *emcclus = (EmcSnglCalibTowerv1 *) GetEmcTower()->UncheckedAt(itower);
  return((emcclus) ? emcclus->get_index() : -999);
}

void EmcCalibTowerv1::set_index(const unsigned int itower, const int ival)
{
  EmcSnglCalibTowerv1 *emcclus = (EmcSnglCalibTowerv1 *) GetEmcTower()->UncheckedAt(itower);
  if (emcclus)
    {
      emcclus->set_index(ival);
    }
  else
    {
      cout << PHWHERE << "ERROR no EmcSnglCalibTowerv1 object found" << endl;
    }
  return;
}

int EmcCalibTowerv1::get_swkey(const unsigned int itower) const
{

  EmcSnglCalibTowerv1 *emcclus = (EmcSnglCalibTowerv1 *) GetEmcTower()->UncheckedAt(itower);
  return((emcclus) ? emcclus->get_swkey() : -999);
}

void EmcCalibTowerv1::set_swkey(const unsigned int itower, const int ival)
{
  EmcSnglCalibTowerv1 *emcclus = (EmcSnglCalibTowerv1 *) GetEmcTower()->UncheckedAt(itower);
  if (emcclus)
    {
      emcclus->set_swkey(ival);
    }
  else
    {
      cout << PHWHERE << "ERROR no EmcSnglCalibTowerv1 object found" << endl;
    }
  return;
}

int EmcCalibTowerv1::get_warnmap(const unsigned int itower) const
{

  EmcSnglCalibTowerv1 *emcclus = (EmcSnglCalibTowerv1 *) GetEmcTower()->UncheckedAt(itower);
  return((emcclus) ? emcclus->get_warnmap() : -999);
}

void EmcCalibTowerv1::set_warnmap(const unsigned int itower, const int ival)
{
  EmcSnglCalibTowerv1 *emcclus = (EmcSnglCalibTowerv1 *) GetEmcTower()->UncheckedAt(itower);
  if (emcclus)
    {
      emcclus->set_warnmap(ival);
    }
  else
    {
      cout << PHWHERE << "ERROR no EmcSnglCalibTowerv1 object found" << endl;
    }
  return;
}

float EmcCalibTowerv1::get_adc(const unsigned int itower) const
{

  EmcSnglCalibTowerv1 *emcclus = (EmcSnglCalibTowerv1 *) GetEmcTower()->UncheckedAt(itower);
  return((emcclus) ? emcclus->get_adc() : -9999.9);
}

void EmcCalibTowerv1::set_adc(const unsigned int itower, const float rval)
{
  EmcSnglCalibTowerv1 *emcclus = (EmcSnglCalibTowerv1 *) GetEmcTower()->UncheckedAt(itower);
  if (emcclus)
    {
      emcclus->set_adc(rval);
    }
  else
    {
      cout << PHWHERE << "ERROR no EmcSnglCalibTowerv1 object found" << endl;
    }
  return;
}

float EmcCalibTowerv1::get_ecal(const unsigned int itower) const
{

  EmcSnglCalibTowerv1 *emcclus = (EmcSnglCalibTowerv1 *) GetEmcTower()->UncheckedAt(itower);
  return((emcclus) ? emcclus->get_ecal() : -9999.9);
}

void EmcCalibTowerv1::set_ecal(const unsigned int itower, const float rval)
{
  EmcSnglCalibTowerv1 *emcclus = (EmcSnglCalibTowerv1 *) GetEmcTower()->UncheckedAt(itower);
  if (emcclus)
    {
      emcclus->set_ecal(rval);
    }
  else
    {
      cout << PHWHERE << "ERROR no EmcSnglCalibTowerv1 object found" << endl;
    }
  return;
}


float EmcCalibTowerv1::get_tac(const unsigned int itower) const
{

  EmcSnglCalibTowerv1 *emcclus = (EmcSnglCalibTowerv1 *) GetEmcTower()->UncheckedAt(itower);
  return((emcclus) ? emcclus->get_tac() : -9999.9);
}

void EmcCalibTowerv1::set_tac(const unsigned int itower, const float rval)
{
  EmcSnglCalibTowerv1 *emcclus = (EmcSnglCalibTowerv1 *) GetEmcTower()->UncheckedAt(itower);
  if (emcclus)
    {
      emcclus->set_tac(rval);
    }
  else
    {
      cout << PHWHERE << "ERROR no EmcSnglCalibTowerv1 object found" << endl;
    }
  return;
}

float EmcCalibTowerv1::get_tof(const unsigned int itower) const
{

  EmcSnglCalibTowerv1 *emcclus = (EmcSnglCalibTowerv1 *) GetEmcTower()->UncheckedAt(itower);
  return((emcclus) ? emcclus->get_tof() : -9999.9);
}

void EmcCalibTowerv1::set_tof(const unsigned int itower, const float rval)
{
  EmcSnglCalibTowerv1 *emcclus = (EmcSnglCalibTowerv1 *) GetEmcTower()->UncheckedAt(itower);
  if (emcclus)
    {
      emcclus->set_tof(rval);
    }
  else
    {
      cout << PHWHERE << "ERROR no EmcSnglCalibTowerv1 object found" << endl;
    }
  return;
}
