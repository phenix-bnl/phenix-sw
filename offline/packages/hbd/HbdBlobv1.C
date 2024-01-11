
#include "HbdBlobv1.h"

ClassImp(HbdBlobv1)

using namespace std;

HbdBlobv1::HbdBlobv1()
{

  id = -9999;
  sector = -9999;
  charge = -9999.;
  size = -9999;
  blobx = -9999.;
  bloby = -9999.;
  blobz = -9999.;
  nlocalmax = -9999;
  parentid = -9999;
  bloby_local = -9999.0;
  blobz_local = -9999.0;

  for (int i=0; i<100; i++)
    {
      charge_pad[i] = -9999.;
      pady_local[i] = -9999.;
      padz_local[i] = -9999.;
      pad_sector[i] = -9999;
    }

  return;

}

HbdBlobv1::HbdBlobv1(HbdBlobv1 *blob)
{

  if (!blob) return;

  id = blob->get_id();
  sector = blob->get_sector();
  charge = blob->get_charge();
  size = blob->get_size();
  blobx = blob->get_blobx();
  bloby = blob->get_bloby();
  blobz = blob->get_blobz();
  nlocalmax = blob->get_nlocalmax();
  parentid = blob->get_parentid();
  bloby_local = blob->get_bloby_local();
  blobz_local = blob->get_blobz_local();

  for (int i=0; i<100; i++)
    {
      charge_pad[i] = blob->get_charge_pad(i);
      pady_local[i] = blob->get_pady_local(i);
      padz_local[i] = blob->get_padz_local(i);
      pad_sector[i] = blob->get_pad_sector(i);
    }

  return;

}

void HbdBlobv1::identify(ostream& os) const
{
  os << "identify yourself: HbdBlobv1 Object\n" << std::endl;
  return;
}

void HbdBlobv1::Reset()
{

  id = -9999;
  sector = -9999;
  charge = -9999.;
  size = -9999;
  blobx = -9999.;
  bloby = -9999.;
  blobz = -9999.;
  nlocalmax = -9999;
  parentid = -9999;
  bloby_local = -9999.;
  blobz_local = -9999.;

  for (int i=0; i<100; i++)
    {
      charge_pad[i] = -9999.;
      pady_local[i] = -9999.;
      padz_local[i] = -9999.;
      pad_sector[i] = -9999;
    }
  
  return;
}

int HbdBlobv1::isValid() const
{
  return 1;
}

