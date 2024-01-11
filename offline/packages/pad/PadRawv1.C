#include "dPadRawWrapper.h"
#include "PadRawv1.h"
#include "PadSnglRawv1.h"
#include "phool.h"
#include "TClonesArray.h"
#include <iostream>

ClassImp(PadRawv1)

using namespace std;

static const unsigned int PADNRAW = 200;

PadRawv1::PadRawv1()
{
  PadNRaw = 0;
  PcRaw = new TClonesArray("PadSnglRawv1",PADNRAW);
  return;
}

PadRawv1::~PadRawv1()
{
  if (PcRaw)
    {
      PcRaw->Clear();
      delete PcRaw;
    }
  return;
}

void PadRawv1::identify(ostream& os ) const
{
  os << "identify yourself: PadRawv1 Object" << endl;
  os << "No of Raws: " << PadNRaw << endl;
  return;
}

void PadRawv1::Reset()
{
 PcRaw->Clear();
 if (PadNRaw>PADNRAW)
   {
     PcRaw->Expand(PADNRAW);
   }
 PadNRaw = 0;
 return;
}

int PadRawv1::isValid() const
{
  return ((PadNRaw>0) ? 1 : 0);
}

void PadRawv1::FillFromWrapper(dPadRawWrapper *wrap)
{
  unsigned int iraw;
  if (wrap)
    {
      PadNRaw = wrap->RowCount();
      set_TClonesArraySize(wrap->RowCount());
      for (iraw = 0; iraw<wrap->RowCount();iraw++)
	{
          AddPadRaw(iraw);
	  set_arm(iraw,wrap->get_arm(iraw));
	  set_id(iraw,wrap->get_id(iraw));
	  set_padtype(iraw,wrap->get_padtype(iraw));
	  set_padx(iraw,wrap->get_padx(iraw));
	  set_padz(iraw,wrap->get_padz(iraw));
	  set_sector(iraw,wrap->get_sector(iraw));
	  set_side(iraw,wrap->get_side(iraw));
	}
    }
  return;
}

int PadRawv1::set_TClonesArraySize(const unsigned int nraw)
{
  if (nraw > PADNRAW)
    {
      PcRaw->Expand(nraw);
     }
  return nraw;
}

void PadRawv1::AddPadRaw(const unsigned int iraw)
{
  TClonesArray &padraw = *PcRaw;
  new(padraw[iraw]) PadSnglRawv1();
  return;
}

short PadRawv1::get_arm(const unsigned int iraw) const
{

  PadSnglRaw *padraw = (PadSnglRaw *) GetPcRaw()->UncheckedAt(iraw);
  return((padraw) ? padraw->get_arm() : -999);
}

void PadRawv1::set_arm(const unsigned int iraw, const short ival)
{
  PadSnglRaw *padraw = (PadSnglRaw *) GetPcRaw()->UncheckedAt(iraw);
  if (padraw)
    {
      padraw->set_arm(ival);
    }
else
  {
    cout << PHWHERE << "ERROR no PadSnglRaw object found" << endl;
  }
  return;
}

short PadRawv1::get_id(const unsigned int iraw) const
{

  PadSnglRaw *padraw = (PadSnglRaw *) GetPcRaw()->UncheckedAt(iraw);
  return((padraw) ? padraw->get_id() : -999);
}

void PadRawv1::set_id(const unsigned int iraw, const short ival)
{
  PadSnglRaw *padraw = (PadSnglRaw *) GetPcRaw()->UncheckedAt(iraw);
  if (padraw)
    {
      padraw->set_id(ival);
    }
else
  {
    cout << PHWHERE << "ERROR no PadSnglRaw object found" << endl;
  }
  return;
}

short PadRawv1::get_padtype(const unsigned int iraw) const
{

  PadSnglRaw *padraw = (PadSnglRaw *) GetPcRaw()->UncheckedAt(iraw);
  return((padraw) ? padraw->get_padtype() : -999);
}

void PadRawv1::set_padtype(const unsigned int iraw, const short ival)
{
  PadSnglRaw *padraw = (PadSnglRaw *) GetPcRaw()->UncheckedAt(iraw);
  if (padraw)
    {
      padraw->set_padtype(ival);
    }
else
  {
    cout << PHWHERE << "ERROR no PadSnglRaw object found" << endl;
  }
  return;
}

short PadRawv1::get_padx(const unsigned int iraw) const
{

  PadSnglRaw *padraw = (PadSnglRaw *) GetPcRaw()->UncheckedAt(iraw);
  return((padraw) ? padraw->get_padx() : -999);
}

void PadRawv1::set_padx(const unsigned int iraw, const short ival)
{
  PadSnglRaw *padraw = (PadSnglRaw *) GetPcRaw()->UncheckedAt(iraw);
  if (padraw)
    {
      padraw->set_padx(ival);
    }
else
  {
    cout << PHWHERE << "ERROR no PadSnglRaw object found" << endl;
  }
  return;
}

short PadRawv1::get_padz(const unsigned int iraw) const
{

  PadSnglRaw *padraw = (PadSnglRaw *) GetPcRaw()->UncheckedAt(iraw);
  return((padraw) ? padraw->get_padz() : -999);
}

void PadRawv1::set_padz(const unsigned int iraw, const short ival)
{
  PadSnglRaw *padraw = (PadSnglRaw *) GetPcRaw()->UncheckedAt(iraw);
  if (padraw)
    {
      padraw->set_padz(ival);
    }
else
  {
    cout << PHWHERE << "ERROR no PadSnglRaw object found" << endl;
  }
  return;
}

short PadRawv1::get_sector(const unsigned int iraw) const
{

  PadSnglRaw *padraw = (PadSnglRaw *) GetPcRaw()->UncheckedAt(iraw);
  return((padraw) ? padraw->get_sector() : -999);
}

void PadRawv1::set_sector(const unsigned int iraw, const short ival)
{
  PadSnglRaw *padraw = (PadSnglRaw *) GetPcRaw()->UncheckedAt(iraw);
  if (padraw)
    {
      padraw->set_sector(ival);
    }
else
  {
    cout << PHWHERE << "ERROR no PadSnglRaw object found" << endl;
  }
  return;
}

short PadRawv1::get_side(const unsigned int iraw) const
{

  PadSnglRaw *padraw = (PadSnglRaw *) GetPcRaw()->UncheckedAt(iraw);
  return((padraw) ? padraw->get_side() : -999);
}

void PadRawv1::set_side(const unsigned int iraw, const short ival)
{
  PadSnglRaw *padraw = (PadSnglRaw *) GetPcRaw()->UncheckedAt(iraw);
  if (padraw)
    {
      padraw->set_side(ival);
    }
else
  {
    cout << PHWHERE << "ERROR no PadSnglRaw object found" << endl;
  }
  return;
}




