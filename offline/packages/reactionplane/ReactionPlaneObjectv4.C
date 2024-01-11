#include <ReactionPlaneObjectv4.h>
#include <ReactionPlaneSnglv1.h>

#include <iomanip>

#include <TClass.h>
#include <TClonesArray.h>

ClassImp(ReactionPlaneObjectv4)


/**
 * @brief  The implementation v4 class for a storage of reaction plane 
 *
 * Created on  / /2012 by Hiroshi Nakagomi.
 */


using namespace std;

// default value of TClonesArray: 20
static const int NRPOUTS = 20;

ReactionPlaneObjectv4::ReactionPlaneObjectv4()
{
  ReactionPlaneSnglArray = new TClonesArray("ReactionPlaneSnglv1", NRPOUTS);
  m_vAryIndex.clear();
}

ReactionPlaneObjectv4::ReactionPlaneObjectv4(const ReactionPlaneObjectv4& source)
{
  ReactionPlaneSnglArray = NULL;
  source.copyTo(*this);
}

void
ReactionPlaneObjectv4::copyTo(ReactionPlaneObjectv4& dest) const
{
  if (! dest.ReactionPlaneSnglArray)
    {
      dest.ReactionPlaneSnglArray = new TClonesArray(ReactionPlaneSnglArray->GetClass()->GetName(), NRPOUTS);
    }
  else
    {
      dest.Reset();
    }
  dest.ReactionPlaneSnglArray->ExpandCreate(ReactionPlaneSnglArray->GetLast() + 1);
  for ( int i = 0; i <= ReactionPlaneSnglArray->GetLast(); ++i )
    {
      ReactionPlaneSngl *snglin  = static_cast<ReactionPlaneSngl *> (ReactionPlaneSnglArray->UncheckedAt(i));
      ReactionPlaneSngl *snglout = static_cast<ReactionPlaneSngl *> (dest.ReactionPlaneSnglArray->UncheckedAt(i));
      *snglout = *snglin;
    }
  dest.m_vAryIndex = m_vAryIndex;
}

ReactionPlaneObjectv4::~ReactionPlaneObjectv4()
{
  if (ReactionPlaneSnglArray)
    {
      ReactionPlaneSnglArray->Delete();
      delete ReactionPlaneSnglArray;
      ReactionPlaneSnglArray = NULL;
    }
  return ;
}

void ReactionPlaneObjectv4::identify(ostream& os) const
{
  os << "identify yourself: ReactionPlaneObjectv4 Object" << endl;
  identifyv4(os);
  return ;
}

void ReactionPlaneObjectv4::identifyv4(ostream& os) const
{
  os << "No of stored ReactionPlaneSngl: " << ReactionPlaneSnglArray->GetLast() + 1 << endl;
//  os << "No of stored ReactionPlaneSnglSumXYsize: " << m_vAryIndex.size() << endl;

  map<int, int>::const_iterator iter;
  for (iter = m_vAryIndex.begin(); iter != m_vAryIndex.end(); iter++)
    {
      ReactionPlaneSngl *tmp = dynamic_cast<ReactionPlaneSngl*> (ReactionPlaneSnglArray->UncheckedAt(iter->second));
      os << tmp->GetName()
	 << ": ID Psi"<<setw(8)<<hex<<tmp->GetIdCode()<<dec
	 << ", Psi "     << tmp->GetPsi()
         << endl;
    }
}

void ReactionPlaneObjectv4::Reset()
{
  ReactionPlaneSnglArray->Delete();
  // deflate TClonesArray again if size is larger than default
  if (ReactionPlaneSnglArray->GetSize() > NRPOUTS)
    {
      ReactionPlaneSnglArray->Expand(NRPOUTS);
    }

  m_vAryIndex.clear();
  return ;
}

int ReactionPlaneObjectv4::isValid() const
{
  return ((m_vAryIndex.size() > 0) ? 1 : 0);
}

//_____________________________________________________________
void ReactionPlaneObjectv4::AddReactionPlane(const char *name, const int id, const float rp)
{
  

  int ivtx = ReactionPlaneSnglArray->GetLast() + 1;
  
  // Expand Vertex TClonesArray if neccessary
  if (ivtx == ReactionPlaneSnglArray->GetSize())
    {
      ReactionPlaneSnglArray->Expand((ReactionPlaneSnglArray->GetSize() + NRPOUTS));
    }
  
  TClonesArray &rparray = *ReactionPlaneSnglArray;
  new(rparray[ivtx]) ReactionPlaneSnglv1(name, id, rp);
  
  // if order <= 0 the vertex does not take part in vertex selection
  m_vAryIndex[id] = ivtx;
  return ;
}

bool ReactionPlaneObjectv4::isReactionPlane(const char *name) const
{
  for (int i = 0; i <= ReactionPlaneSnglArray->GetLast(); i++)
    {
      ReactionPlaneSngl *tmp = dynamic_cast<ReactionPlaneSngl*> (ReactionPlaneSnglArray->UncheckedAt(i));
      if (!strcmp(tmp->GetName(), name)) { return true; }
    }
  return false;
}

int ReactionPlaneObjectv4::getnReactionPlane() const
{
  return ReactionPlaneSnglArray->GetEntries();
}

ReactionPlaneSngl* ReactionPlaneObjectv4::getReactionPlane(const int idcode) const
{
  map<int, int>::const_iterator itr = m_vAryIndex.find(idcode);
  if(itr!=m_vAryIndex.end()){
    int idx = itr->second;
    return dynamic_cast<ReactionPlaneSngl*> (ReactionPlaneSnglArray->UncheckedAt(idx));
  }
  return NULL;
}

void ReactionPlaneObjectv4::setValue(const char* name, const int idcode, const float val)
{
  ReactionPlaneSngl *tmp = getReactionPlane(idcode);
  if(tmp==NULL){
    AddReactionPlane(name, idcode,0);
    tmp = getReactionPlane(idcode);
  }
  tmp->SetPsi(val);
}

float ReactionPlaneObjectv4::getValue(const int idcode) const
{
  ReactionPlaneSngl *tmp = getReactionPlane(idcode);
  if(tmp==NULL){
    return -9999.0;
  }
  
  float ret = tmp->GetPsi();
  
  return ret;
}
