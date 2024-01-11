#include <emcHitContainerv1.h>
#include <emcSnglTwrv1.h>
#include <TClonesArray.h>
#include <iostream>

ClassImp(emcHitContainerv1)

using namespace std;

  // First we implement the "standard functions"...
emcHitContainerv1::emcHitContainerv1()
{
  Pc = new TClonesArray("emcSnglTwrv1",400);
}


emcHitContainerv1::~emcHitContainerv1()
{
  if (Pc)
    {
      Pc->Clear();
    }
  delete Pc;
  return;
}

void emcHitContainerv1::identify(ostream& os) const
{
  os << "identify yourself: emcHitContainerv1 Object" << endl;
  os << "No of towers: " << Pc->GetEntries() << endl;
  return;
}

void emcHitContainerv1::Reset()
{

  //  cout << "Had " << padhitv1.size() << " hitv1s" << endl;
  Pc->Clear();
  if (Pc->GetSize() > 200)
    {
      Pc->Expand(200);
    }
 return;
}

int emcHitContainerv1::isValid() const
{
  return((Pc->GetEntries()>0) ? 1 : 0);
}

void
 emcHitContainerv1::AddTwr(const emcSnglTwr &twr)
{
//   cout << "size: " << Pc->GetSize() << endl;
//   cout << "entries: " << Pc->GetEntriesFast() << endl;
//   cout << "last: " << Pc->GetLast() << endl;

  if (Pc->GetSize() == Pc->GetEntriesFast())
    {
      Pc->Expand(Pc->GetSize()+400);
    }
  TClonesArray &towers = *Pc;
  new(towers[Pc->GetLast()+1]) emcSnglTwrv1(twr);
  return;
}

emcSnglTwr *
emcHitContainerv1::GetTwr(const int index)
{
  emcSnglTwr *twr = (emcSnglTwr *) Pc->At(index);
  return twr;
}

unsigned int emcHitContainerv1::multiplicity() const
{
    return Pc->GetLast()+1;
}
