#include "EmcClusterLocalExtv1.h"
#include "EmcSnglClusterLocalExtv1.h"
#include "dEmcClusterLocalExtWrapper.h"
//INCLUDECHECKER: Removed this line: #include "phool.h"
#include "TClonesArray.h"
#include <iostream>

ClassImp(EmcClusterLocalExtv1);

using namespace std;

static const unsigned int EMCNCLUSV1 = 200;

EmcClusterLocalExtv1::EmcClusterLocalExtv1()
{
  EmcNCluster = 0;
  EmcClus = new TClonesArray("EmcSnglClusterLocalExtv1", EMCNCLUSV1);
  return ;
}

EmcClusterLocalExtv1::~EmcClusterLocalExtv1()
{
  EmcClus->Clear();
  return ;
}

void EmcClusterLocalExtv1::identify(ostream& os) const
{
  os << "identify yourself: EmcClusterLocalExtv1 Object" << endl;
  os << "No of Clusters: " << EmcNCluster << endl;
  return ;
}

void EmcClusterLocalExtv1::Reset()
{
  EmcClus->Clear();
  if (EmcNCluster > EMCNCLUSV1)
    {
      EmcClus->Expand(EMCNCLUSV1);
    }
  EmcNCluster = 0;
  return ;
}

int EmcClusterLocalExtv1::isValid() const
{
  return ((EmcNCluster > 0) ? 1 : 0);
}

void EmcClusterLocalExtv1::FillFromWrapper(dEmcClusterLocalExtWrapper *wrap)
{
  unsigned int iclus;
  int index;
  short i;
  if (wrap)
    {
      EmcNCluster = wrap->RowCount();
      set_TClonesArraySize(wrap->RowCount());
      for (iclus = 0;iclus < wrap->RowCount();iclus++)
        {
          AddEmcCluster(iclus);
          set_clusno(iclus, wrap->get_clusno(iclus));
          set_id(iclus, wrap->get_id(iclus));
          set_method(iclus, wrap->get_method(iclus));
          set_nsh(iclus, wrap->get_nsh(iclus));
          set_twrhit(iclus, wrap->get_twrhit(iclus));
          set_type(iclus, wrap->get_type(iclus));

          set_deadmap(iclus, wrap->get_deadmap(iclus));
          index = 100000 * wrap->get_arm(iclus) +
	    10000 * wrap->get_sector(iclus) +
	    100 * wrap->get_ind(1, iclus) +
	    wrap->get_ind(0, iclus);
          set_index(iclus,index);
          set_warnmap(iclus, wrap->get_warnmap(iclus));

          set_chi2(iclus, wrap->get_chi2(iclus));
          set_chi2_sh(iclus, wrap->get_chi2_sh(iclus));
          set_de(iclus, wrap->get_de(iclus));
          set_dtof(iclus, wrap->get_dtof(iclus));
          set_e(iclus, wrap->get_e(iclus));
          set_ecent(iclus, wrap->get_ecent(iclus));
          set_ecore(iclus, wrap->get_ecore(iclus));
          set_ecorr(iclus, wrap->get_ecorr(iclus));
          set_etofmax(iclus, wrap->get_etofmax(iclus));
          set_etofmin(iclus, wrap->get_etofmin(iclus));
          set_e9(iclus, wrap->get_e9(iclus));
          set_phi(iclus, wrap->get_phi(iclus));
          set_pid(iclus, wrap->get_pid(iclus));
          set_prob_neuhad(iclus, wrap->get_prob_neuhad(iclus));
          set_prob_photon(iclus, wrap->get_prob_photon(iclus));
          set_prob_photon_sh(iclus, wrap->get_prob_photon_sh(iclus));
          set_qual(iclus, wrap->get_qual(iclus));
          set_re9(iclus, wrap->get_re9(iclus));
          set_theta(iclus, wrap->get_theta(iclus));
          set_tof(iclus, wrap->get_tof(iclus));
          set_tofcorr(iclus, wrap->get_tofcorr(iclus));
          set_tofmax(iclus, wrap->get_tofmax(iclus));
          set_tofmaxcorr(iclus, wrap->get_tofmaxcorr(iclus));
          set_tofmean(iclus, wrap->get_tofmean(iclus));
          set_tofmin(iclus, wrap->get_tofmin(iclus));
          set_tofmincorr(iclus, wrap->get_tofmincorr(iclus));

          for (i = 0;i < 2;i++)
            {
              set_de_sh(iclus, i, wrap->get_de_sh(i, iclus));
              set_disp(iclus, i, wrap->get_disp(i, iclus));
              set_ecorr_sh(iclus, i, wrap->get_ecorr_sh(i, iclus));
              set_e_sh(iclus, i, wrap->get_e_sh(i, iclus));
              set_padisp(iclus, i, wrap->get_padisp(i, iclus));
              set_yz_cg(iclus, i, wrap->get_yz_cg(i, iclus));
              for (short j = 0;j < 2;j++)
                {
                  set_dxyz_sh(iclus, i, j, wrap->get_dxyz_sh(j, i, iclus));
                  set_xyz_sh(iclus, i, j, wrap->get_xyz_sh(j, i, iclus));
                }
            }
          for (i = 0;i < 3;i++)
            {
              set_dxyz(iclus, i, wrap->get_dxyz(i, iclus));
              set_unitv(iclus, i, wrap->get_unitv(i, iclus));
              set_xyz(iclus, i, wrap->get_xyz(i, iclus));
            }
          for (i = 0;i < 16;i++)
            {
              set_twrlist(iclus, i, wrap->get_twrlist(i, iclus));
              set_partesum(iclus, i, wrap->get_partesum(i, iclus));
            }
        }
    }
  return ;
}

int EmcClusterLocalExtv1::set_TClonesArraySize(const unsigned int nclus)
{
  if (nclus > EMCNCLUSV1)
    {
      EmcClus->Expand(nclus);
    }
  return nclus;
}

void EmcClusterLocalExtv1::AddEmcCluster(const unsigned int iclus)
{
  TClonesArray &emcclus = *EmcClus;
  new(emcclus[iclus]) EmcSnglClusterLocalExtv1();
  return ;
}

short EmcClusterLocalExtv1::get_arm(const unsigned int iclus) const
{

  int key = get_index(iclus);
  return key / 100000;

}

short EmcClusterLocalExtv1::get_clusno(const unsigned int iclus) const
{

  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  return ((emcclus) ? emcclus->get_clusno() : -999);
}

void EmcClusterLocalExtv1::set_clusno(const unsigned int iclus, const short ival)
{
  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  if (emcclus)
    {
      emcclus->set_clusno(ival);
    }
  else
    {
      cout << PHWHERE << "ERROR no EmcSnglClusterLocalExtv1 object found" << endl;
    }
  return ;
}

short EmcClusterLocalExtv1::get_id(const unsigned int iclus) const
{

  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  return ((emcclus) ? emcclus->get_id() : -999);
}

void EmcClusterLocalExtv1::set_id(const unsigned int iclus, const short ival)
{
  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  if (emcclus)
    {
      emcclus->set_id(ival);
    }
  else
    {
      cout << PHWHERE << "ERROR no EmcSnglClusterLocalExtv1 object found" << endl;
    }
  return ;
}

short EmcClusterLocalExtv1::get_ind(const unsigned int iclus, const short i) const
{
  // i==0 corresponds to the X direction in the local EMC sector frame
  // (which coincides with the Z direction in the global PHENIX frame)
  // i==1 corresponds to the Y direction in the local EMC sector fram

  int key = get_index(iclus);

  if (i == 0)
    return key % 100;
  else if (i == 1)
    return (key % 10000) / 100;

  cout << PHWHERE << "ERROR index " << i << "out of range" << endl;
  return -999;

}

short EmcClusterLocalExtv1::get_method(const unsigned int iclus) const
{

  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  return ((emcclus) ? emcclus->get_method() : -999);
}

void EmcClusterLocalExtv1::set_method(const unsigned int iclus, const short ival)
{
  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  if (emcclus)
    {
      emcclus->set_method(ival);
    }
  else
    {
      cout << PHWHERE << "ERROR no EmcSnglClusterLocalExtv1 object found" << endl;
    }
  return ;
}

short EmcClusterLocalExtv1::get_nsh(const unsigned int iclus) const
{

  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  return ((emcclus) ? emcclus->get_nsh() : -999);
}

void EmcClusterLocalExtv1::set_nsh(const unsigned int iclus, const short ival)
{
  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  if (emcclus)
    {
      emcclus->set_nsh(ival);
    }
  else
    {
      cout << PHWHERE << "ERROR no EmcSnglClusterLocalExtv1 object found" << endl;
    }
  return ;
}

short EmcClusterLocalExtv1::get_sector(const unsigned int iclus) const
{

  int key = get_index(iclus);
  return (key % 100000) / 10000;

}

short EmcClusterLocalExtv1::get_twrhit(const unsigned int iclus) const
{

  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  return ((emcclus) ? emcclus->get_twrhit() : -999);
}

void EmcClusterLocalExtv1::set_twrhit(const unsigned int iclus, const short ival)
{
  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  if (emcclus)
    {
      emcclus->set_twrhit(ival);
    }
  else
    {
      cout << PHWHERE << "ERROR no EmcSnglClusterLocalExtv1 object found" << endl;
    }
  return ;
}

short EmcClusterLocalExtv1::get_type(const unsigned int iclus) const
{

  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  return ((emcclus) ? emcclus->get_type() : -999);
}

void EmcClusterLocalExtv1::set_type(const unsigned int iclus, const short ival)
{
  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  if (emcclus)
    {
      emcclus->set_type(ival);
    }
  else
    {
      cout << PHWHERE << "ERROR no EmcSnglClusterLocalExtv1 object found" << endl;
    }
  return ;
}

int EmcClusterLocalExtv1::get_deadmap(const unsigned int iclus) const
{

  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  return ((emcclus) ? emcclus->get_deadmap() : -999);
}

void EmcClusterLocalExtv1::set_deadmap(const unsigned int iclus, int ival)
{
  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  if (emcclus)
    {
      emcclus->set_deadmap(ival);
    }
  else
    {
      cout << PHWHERE << "ERROR no EmcSnglClusterLocalExtv1 object found" << endl;
    }
  return ;
}

int EmcClusterLocalExtv1::get_index(const unsigned int iclus) const
{

  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  return ((emcclus) ? emcclus->get_index() : -999);
}

void EmcClusterLocalExtv1::set_index(const unsigned int iclus, int ival)
{
  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  if (emcclus)
    {
      emcclus->set_index(ival);
    }
  else
    {
      cout << PHWHERE << "ERROR no EmcSnglClusterLocalExtv1 object found" << endl;
    }
  return ;
}

int EmcClusterLocalExtv1::get_warnmap(const unsigned int iclus) const
{

  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  return ((emcclus) ? emcclus->get_warnmap() : -999);
}

void EmcClusterLocalExtv1::set_warnmap(const unsigned int iclus, int ival)
{
  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  if (emcclus)
    {
      emcclus->set_warnmap(ival);
    }
  else
    {
      cout << PHWHERE << "ERROR no EmcSnglClusterLocalExtv1 object found" << endl;
    }
  return ;
}

int EmcClusterLocalExtv1::get_twrlist(const unsigned int iclus, const short i) const
{
  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  return ((emcclus) ? emcclus->get_twrlist(i) : -999);
}

void EmcClusterLocalExtv1::set_twrlist(const unsigned int iclus, const short i, const int ival)
{
  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  if (emcclus)
    {
      emcclus->set_twrlist(ival, i);
    }
  else
    {
      cout << PHWHERE << "ERROR no EmcSnglClusterLocalExtv1 object found" << endl;
    }
  return ;
}


float EmcClusterLocalExtv1::get_chi2(const unsigned int iclus) const
{

  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  return ((emcclus) ? emcclus->get_chi2() : -9999.9);
}

void EmcClusterLocalExtv1::set_chi2(const unsigned int iclus, const float rval)
{
  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  if (emcclus)
    {
      emcclus->set_chi2(rval);
    }
  else
    {
      cout << PHWHERE << "ERROR no EmcSnglClusterLocalExtv1 object found" << endl;
    }
  return ;
}

float EmcClusterLocalExtv1::get_chi2_sh(const unsigned int iclus) const
{

  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  return ((emcclus) ? emcclus->get_chi2_sh() : -9999.9);
}

void EmcClusterLocalExtv1::set_chi2_sh(const unsigned int iclus, const float rval)
{
  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  if (emcclus)
    {
      emcclus->set_chi2_sh(rval);
    }
  else
    {
      cout << PHWHERE << "ERROR no EmcSnglClusterLocalExtv1 object found" << endl;
    }
  return ;
}

float EmcClusterLocalExtv1::get_de(const unsigned int iclus) const
{

  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  return ((emcclus) ? emcclus->get_de() : -9999.9);
}

void EmcClusterLocalExtv1::set_de(const unsigned int iclus, const float rval)
{
  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  if (emcclus)
    {
      emcclus->set_de(rval);
    }
  else
    {
      cout << PHWHERE << "ERROR no EmcSnglClusterLocalExtv1 object found" << endl;
    }
  return ;
}

float EmcClusterLocalExtv1::get_dtof(const unsigned int iclus) const
{

  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  return ((emcclus) ? emcclus->get_dtof() : -9999.9);
}

void EmcClusterLocalExtv1::set_dtof(const unsigned int iclus, const float rval)
{
  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  if (emcclus)
    {
      emcclus->set_dtof(rval);
    }
  else
    {
      cout << PHWHERE << "ERROR no EmcSnglClusterLocalExtv1 object found" << endl;
    }
  return ;
}

float EmcClusterLocalExtv1::get_e(const unsigned int iclus) const
{

  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  return ((emcclus) ? emcclus->get_e() : -9999.9);
}

void EmcClusterLocalExtv1::set_e(const unsigned int iclus, const float rval)
{
  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  if (emcclus)
    {
      emcclus->set_e(rval);
    }
  else
    {
      cout << PHWHERE << "ERROR no EmcSnglClusterLocalExtv1 object found" << endl;
    }
  return ;
}

float EmcClusterLocalExtv1::get_ecent(const unsigned int iclus) const
{

  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  return ((emcclus) ? emcclus->get_ecent() : -9999.9);
}

void EmcClusterLocalExtv1::set_ecent(const unsigned int iclus, const float rval)
{
  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  if (emcclus)
    {
      emcclus->set_ecent(rval);
    }
  else
    {
      cout << PHWHERE << "ERROR no EmcSnglClusterLocalExtv1 object found" << endl;
    }
  return ;
}

float EmcClusterLocalExtv1::get_ecore(const unsigned int iclus) const
{

  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  return ((emcclus) ? emcclus->get_ecore() : -9999.9);
}

void EmcClusterLocalExtv1::set_ecore(const unsigned int iclus, const float rval)
{
  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  if (emcclus)
    {
      emcclus->set_ecore(rval);
    }
  else
    {
      cout << PHWHERE << "ERROR no EmcSnglClusterLocalExtv1 object found" << endl;
    }
  return ;
}

float EmcClusterLocalExtv1::get_ecorr(const unsigned int iclus) const
{

  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  return ((emcclus) ? emcclus->get_ecorr() : -9999.9);
}

void EmcClusterLocalExtv1::set_ecorr(const unsigned int iclus, const float rval)
{
  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  if (emcclus)
    {
      emcclus->set_ecorr(rval);
    }
  else
    {
      cout << PHWHERE << "ERROR no EmcSnglClusterLocalExtv1 object found" << endl;
    }
  return ;
}

float EmcClusterLocalExtv1::get_etofmax(const unsigned int iclus) const
{

  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  return ((emcclus) ? emcclus->get_etofmax() : -9999.9);
}

void EmcClusterLocalExtv1::set_etofmax(const unsigned int iclus, const float rval)
{
  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  if (emcclus)
    {
      emcclus->set_etofmax(rval);
    }
  else
    {
      cout << PHWHERE << "ERROR no EmcSnglClusterLocalExtv1 object found" << endl;
    }
  return ;
}

float EmcClusterLocalExtv1::get_etofmin(const unsigned int iclus) const
{

  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  return ((emcclus) ? emcclus->get_etofmin() : -9999.9);
}

void EmcClusterLocalExtv1::set_etofmin(const unsigned int iclus, const float rval)
{
  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  if (emcclus)
    {
      emcclus->set_etofmin(rval);
    }
  else
    {
      cout << PHWHERE << "ERROR no EmcSnglClusterLocalExtv1 object found" << endl;
    }
  return ;
}

float EmcClusterLocalExtv1::get_e9(const unsigned int iclus) const
{

  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  return ((emcclus) ? emcclus->get_e9() : -9999.9);
}

void EmcClusterLocalExtv1::set_e9(const unsigned int iclus, const float rval)
{
  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  if (emcclus)
    {
      emcclus->set_e9(rval);
    }
  else
    {
      cout << PHWHERE << "ERROR no EmcSnglClusterLocalExtv1 object found" << endl;
    }
  return ;
}

float EmcClusterLocalExtv1::get_phi(const unsigned int iclus) const
{

  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  return ((emcclus) ? emcclus->get_phi() : -9999.9);
}

void EmcClusterLocalExtv1::set_phi(const unsigned int iclus, const float rval)
{
  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  if (emcclus)
    {
      emcclus->set_phi(rval);
    }
  else
    {
      cout << PHWHERE << "ERROR no EmcSnglClusterLocalExtv1 object found" << endl;
    }
  return ;
}

float EmcClusterLocalExtv1::get_pid(const unsigned int iclus) const
{

  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  return ((emcclus) ? emcclus->get_pid() : -9999.9);
}

void EmcClusterLocalExtv1::set_pid(const unsigned int iclus, const float rval)
{
  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  if (emcclus)
    {
      emcclus->set_pid(rval);
    }
  else
    {
      cout << PHWHERE << "ERROR no EmcSnglClusterLocalExtv1 object found" << endl;
    }
  return ;
}

float EmcClusterLocalExtv1::get_prob_neuhad(const unsigned int iclus) const
{

  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  return ((emcclus) ? emcclus->get_prob_neuhad() : -9999.9);
}

void EmcClusterLocalExtv1::set_prob_neuhad(const unsigned int iclus, const float rval)
{
  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  if (emcclus)
    {
      emcclus->set_prob_neuhad(rval);
    }
  else
    {
      cout << PHWHERE << "ERROR no EmcSnglClusterLocalExtv1 object found" << endl;
    }
  return ;
}

float EmcClusterLocalExtv1::get_prob_photon(const unsigned int iclus) const
{

  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  return ((emcclus) ? emcclus->get_prob_photon() : -9999.9);
}

void EmcClusterLocalExtv1::set_prob_photon(const unsigned int iclus, const float rval)
{
  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  if (emcclus)
    {
      emcclus->set_prob_photon(rval);
    }
  else
    {
      cout << PHWHERE << "ERROR no EmcSnglClusterLocalExtv1 object found" << endl;
    }
  return ;
}

float EmcClusterLocalExtv1::get_prob_photon_sh(const unsigned int iclus) const
{

  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  return ((emcclus) ? emcclus->get_prob_photon_sh() : -9999.9);
}

void EmcClusterLocalExtv1::set_prob_photon_sh(const unsigned int iclus, const float rval)
{
  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  if (emcclus)
    {
      emcclus->set_prob_photon_sh(rval);
    }
  else
    {
      cout << PHWHERE << "ERROR no EmcSnglClusterLocalExtv1 object found" << endl;
    }
  return ;
}

float EmcClusterLocalExtv1::get_qual(const unsigned int iclus) const
{

  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  return ((emcclus) ? emcclus->get_qual() : -9999.9);
}

void EmcClusterLocalExtv1::set_qual(const unsigned int iclus, const float rval)
{
  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  if (emcclus)
    {
      emcclus->set_qual(rval);
    }
  else
    {
      cout << PHWHERE << "ERROR no EmcSnglClusterLocalExtv1 object found" << endl;
    }
  return ;
}

float EmcClusterLocalExtv1::get_re9(const unsigned int iclus) const
{

  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  return ((emcclus) ? emcclus->get_re9() : -9999.9);
}

void EmcClusterLocalExtv1::set_re9(const unsigned int iclus, const float rval)
{
  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  if (emcclus)
    {
      emcclus->set_re9(rval);
    }
  else
    {
      cout << PHWHERE << "ERROR no EmcSnglClusterLocalExtv1 object found" << endl;
    }
  return ;
}

float EmcClusterLocalExtv1::get_theta(const unsigned int iclus) const
{

  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  return ((emcclus) ? emcclus->get_theta() : -9999.9);
}

void EmcClusterLocalExtv1::set_theta(const unsigned int iclus, const float rval)
{
  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  if (emcclus)
    {
      emcclus->set_theta(rval);
    }
  else
    {
      cout << PHWHERE << "ERROR no EmcSnglClusterLocalExtv1 object found" << endl;
    }
  return ;
}

float EmcClusterLocalExtv1::get_tof(const unsigned int iclus) const
{

  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  return ((emcclus) ? emcclus->get_tof() : -9999.9);
}

void EmcClusterLocalExtv1::set_tof(const unsigned int iclus, const float rval)
{
  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  if (emcclus)
    {
      emcclus->set_tof(rval);
    }
  else
    {
      cout << PHWHERE << "ERROR no EmcSnglClusterLocalExtv1 object found" << endl;
    }
  return ;
}

float EmcClusterLocalExtv1::get_tofcorr(const unsigned int iclus) const
{

  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  return ((emcclus) ? emcclus->get_tofcorr() : -9999.9);
}

void EmcClusterLocalExtv1::set_tofcorr(const unsigned int iclus, const float rval)
{
  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  if (emcclus)
    {
      emcclus->set_tofcorr(rval);
    }
  else
    {
      cout << PHWHERE << "ERROR no EmcSnglClusterLocalExtv1 object found" << endl;
    }
  return ;
}

float EmcClusterLocalExtv1::get_tofmax(const unsigned int iclus) const
{

  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  return ((emcclus) ? emcclus->get_tofmax() : -9999.9);
}

void EmcClusterLocalExtv1::set_tofmax(const unsigned int iclus, const float rval)
{
  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  if (emcclus)
    {
      emcclus->set_tofmax(rval);
    }
  else
    {
      cout << PHWHERE << "ERROR no EmcSnglClusterLocalExtv1 object found" << endl;
    }
  return ;
}

float EmcClusterLocalExtv1::get_tofmaxcorr(const unsigned int iclus) const
{

  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  return ((emcclus) ? emcclus->get_tofmaxcorr() : -9999.9);
}

void EmcClusterLocalExtv1::set_tofmaxcorr(const unsigned int iclus, const float rval)
{
  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  if (emcclus)
    {
      emcclus->set_tofmaxcorr(rval);
    }
  else
    {
      cout << PHWHERE << "ERROR no EmcSnglClusterLocalExtv1 object found" << endl;
    }
  return ;
}

float EmcClusterLocalExtv1::get_tofmean(const unsigned int iclus) const
{

  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  return ((emcclus) ? emcclus->get_tofmean() : -9999.9);
}

void EmcClusterLocalExtv1::set_tofmean(const unsigned int iclus, const float rval)
{
  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  if (emcclus)
    {
      emcclus->set_tofmean(rval);
    }
  else
    {
      cout << PHWHERE << "ERROR no EmcSnglClusterLocalExtv1 object found" << endl;
    }
  return ;
}

float EmcClusterLocalExtv1::get_tofmin(const unsigned int iclus) const
{

  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  return ((emcclus) ? emcclus->get_tofmin() : -9999.9);
}

void EmcClusterLocalExtv1::set_tofmin(const unsigned int iclus, const float rval)
{
  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  if (emcclus)
    {
      emcclus->set_tofmin(rval);
    }
  else
    {
      cout << PHWHERE << "ERROR no EmcSnglClusterLocalExtv1 object found" << endl;
    }
  return ;
}

float EmcClusterLocalExtv1::get_tofmincorr(const unsigned int iclus) const
{

  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  return ((emcclus) ? emcclus->get_tofmincorr() : -9999.9);
}

void EmcClusterLocalExtv1::set_tofmincorr(const unsigned int iclus, const float rval)
{
  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  if (emcclus)
    {
      emcclus->set_tofmincorr(rval);
    }
  else
    {
      cout << PHWHERE << "ERROR no EmcSnglClusterLocalExtv1 object found" << endl;
    }
  return ;
}


float EmcClusterLocalExtv1::get_de_sh(const unsigned int iclus, const short i) const
{
  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  return ((emcclus) ? emcclus->get_de_sh(i) : -9999.9);
}

void EmcClusterLocalExtv1::set_de_sh(const unsigned int iclus, const short i, const float rval)
{
  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  if (emcclus)
    {
      emcclus->set_de_sh(rval, i);
    }
  else
    {
      cout << PHWHERE << "ERROR no EmcSnglClusterLocalExtv1 object found" << endl;
    }
  return ;
}

float EmcClusterLocalExtv1::get_disp(const unsigned int iclus, const short i) const
{
  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  return ((emcclus) ? emcclus->get_disp(i) : -9999.9);
}

void EmcClusterLocalExtv1::set_disp(const unsigned int iclus, const short i, const float rval)
{
  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  if (emcclus)
    {
      emcclus->set_disp(rval, i);
    }
  else
    {
      cout << PHWHERE << "ERROR no EmcSnglClusterLocalExtv1 object found" << endl;
    }
  return ;
}

float EmcClusterLocalExtv1::get_dxyz(const unsigned int iclus, const short i) const
{
  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  return ((emcclus) ? emcclus->get_dxyz(i) : -9999.9);
}

void EmcClusterLocalExtv1::set_dxyz(const unsigned int iclus, const short i, const float rval)
{
  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  if (emcclus)
    {
      emcclus->set_dxyz(rval, i);
    }
  else
    {
      cout << PHWHERE << "ERROR no EmcSnglClusterLocalExtv1 object found" << endl;
    }
  return ;
}

float EmcClusterLocalExtv1::get_ecorr_sh(const unsigned int iclus, const short i) const
{
  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  return ((emcclus) ? emcclus->get_ecorr_sh(i) : -9999.9);
}

void EmcClusterLocalExtv1::set_ecorr_sh(const unsigned int iclus, const short i, const float rval)
{
  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  if (emcclus)
    {
      emcclus->set_ecorr_sh(rval, i);
    }
  else
    {
      cout << PHWHERE << "ERROR no EmcSnglClusterLocalExtv1 object found" << endl;
    }
  return ;
}

float EmcClusterLocalExtv1::get_e_sh(const unsigned int iclus, const short i) const
{
  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  return ((emcclus) ? emcclus->get_e_sh(i) : -9999.9);
}

void EmcClusterLocalExtv1::set_e_sh(const unsigned int iclus, const short i, const float rval)
{
  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  if (emcclus)
    {
      emcclus->set_e_sh(rval, i);
    }
  else
    {
      cout << PHWHERE << "ERROR no EmcSnglClusterLocalExtv1 object found" << endl;
    }
  return ;
}

float EmcClusterLocalExtv1::get_padisp(const unsigned int iclus, const short i) const
{
  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  return ((emcclus) ? emcclus->get_padisp(i) : -9999.9);
}

void EmcClusterLocalExtv1::set_padisp(const unsigned int iclus, const short i, const float rval)
{
  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  if (emcclus)
    {
      emcclus->set_padisp(rval, i);
    }
  else
    {
      cout << PHWHERE << "ERROR no EmcSnglClusterLocalExtv1 object found" << endl;
    }
  return ;
}

float EmcClusterLocalExtv1::get_partesum(const unsigned int iclus, const short i) const
{
  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  return ((emcclus) ? emcclus->get_partesum(i) : -9999.9);
}

void EmcClusterLocalExtv1::set_partesum(const unsigned int iclus, const short i, const float rval)
{
  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  if (emcclus)
    {
      emcclus->set_partesum(rval, i);
    }
  else
    {
      cout << PHWHERE << "ERROR no EmcSnglClusterLocalExtv1 object found" << endl;
    }
  return ;
}

float EmcClusterLocalExtv1::get_unitv(const unsigned int iclus, const short i) const
{
  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  return ((emcclus) ? emcclus->get_unitv(i) : -9999.9);
}

void EmcClusterLocalExtv1::set_unitv(const unsigned int iclus, const short i, const float rval)
{
  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  if (emcclus)
    {
      emcclus->set_unitv(rval, i);
    }
  else
    {
      cout << PHWHERE << "ERROR no EmcSnglClusterLocalExtv1 object found" << endl;
    }
  return ;
}

float EmcClusterLocalExtv1::get_xyz(const unsigned int iclus, const short i) const
{
  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  return ((emcclus) ? emcclus->get_xyz(i) : -9999.9);
}

void EmcClusterLocalExtv1::set_xyz(const unsigned int iclus, const short i, const float rval)
{
  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  if (emcclus)
    {
      emcclus->set_xyz(rval, i);
    }
  else
    {
      cout << PHWHERE << "ERROR no EmcSnglClusterLocalExtv1 object found" << endl;
    }
  return ;
}

float EmcClusterLocalExtv1::get_yz_cg(const unsigned int iclus, const short i) const
{

  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  return ((emcclus) ? emcclus->get_yz_cg(i) : -9999.9);
}

void EmcClusterLocalExtv1::set_yz_cg(const unsigned int iclus, const short i, const float rval)
{
  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  if (emcclus)
    {
      emcclus->set_yz_cg(rval, i);
    }
  else
    {
      cout << PHWHERE << "ERROR no EmcSnglClusterLocalExtv1 object found" << endl;
    }
  return ;
}

float EmcClusterLocalExtv1::get_dxyz_sh(const unsigned int iclus,
                                        const short i, const short j) const
{
  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
    return ((emcclus) ? emcclus->get_dxyz_sh(i, j) : -9999.9);
  }

void EmcClusterLocalExtv1::set_dxyz_sh(const unsigned int iclus, const
                                       short i, const short j, const float rval)
{
  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  if (emcclus)
    {
      emcclus->set_dxyz_sh(rval, i, j);
    }
  else
    {
      cout << PHWHERE << "ERROR no EmcSnglClusterLocalExtv1 object found" << endl;
    }
  return ;
}

float EmcClusterLocalExtv1::get_xyz_sh(const unsigned int iclus,
                                       const short i, const short j) const
  {
    EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
    return ((emcclus) ? emcclus->get_xyz_sh(i, j) : -9999.9);
  }

void EmcClusterLocalExtv1::set_xyz_sh(const unsigned int iclus, const
                                      short i, const short j, const float rval)
{
  EmcSnglClusterLocalExtv1 *emcclus = (EmcSnglClusterLocalExtv1 *) GetEmcClus()->UncheckedAt(iclus);
  if (emcclus)
    {
      emcclus->set_xyz_sh(rval, i, j);
    }
  else
    {
      cout << PHWHERE << "ERROR no EmcSnglClusterLocalExtv1 object found" << endl;
    }
  return ;
}

