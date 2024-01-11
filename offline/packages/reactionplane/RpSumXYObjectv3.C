#include <RpSumXYObjectv3.h>
#include <RpSnglSumXYv1.h>

#include <iomanip>

#include <TClass.h>
#include <TClonesArray.h>

ClassImp(RpSumXYObjectv3)


/**
 * @brief  The implementation v3 class for a storage of reaction plane element 
 *
 * Created on 11/16/2011 by Takashi Hachiya.
 */


using namespace std;

// default value of TClonesArray: 20
static const int NRPOUTS = 20;

RpSumXYObjectv3::RpSumXYObjectv3()
{
  RpSnglArray = new TClonesArray("RpSnglSumXYv1", NRPOUTS);
  m_vAryIndex.clear();
}

RpSumXYObjectv3::RpSumXYObjectv3(const RpSumXYObjectv3& source)
{
  RpSnglArray = NULL;
  source.copyTo(*this);
}

void
RpSumXYObjectv3::copyTo(RpSumXYObjectv3& dest) const
{
  if (! dest.RpSnglArray)
    {
      dest.RpSnglArray = new TClonesArray(RpSnglArray->GetClass()->GetName(), NRPOUTS);
    }
  else
    {
      dest.Reset();
    }
  dest.RpSnglArray->ExpandCreate(RpSnglArray->GetLast() + 1);
  for ( int i = 0; i <= RpSnglArray->GetLast(); ++i )
    {
      RpSnglSumXY *snglin  = static_cast<RpSnglSumXY *> (RpSnglArray->UncheckedAt(i));
      RpSnglSumXY *snglout = static_cast<RpSnglSumXY *> (dest.RpSnglArray->UncheckedAt(i));
      *snglout = *snglin;
    }
  dest.m_vAryIndex = m_vAryIndex;
}

RpSumXYObjectv3::~RpSumXYObjectv3()
{
  if (RpSnglArray)
    {
      RpSnglArray->Delete();
      delete RpSnglArray;
      RpSnglArray = NULL;
    }
  return ;
}

void RpSumXYObjectv3::identify(ostream& os) const
{
  os << "identify yourself: RpSumXYObjectv3 Object" << endl;
  identifyv3(os);
  return ;
}

void RpSumXYObjectv3::identifyv3(ostream& os) const
{
  os << "No of stored RpSnglSumXY: " << RpSnglArray->GetLast() + 1 << endl;
//  os << "No of stored RpSnglSumXYsize: " << m_vAryIndex.size() << endl;

  map<int, int>::const_iterator iter;
  for (iter = m_vAryIndex.begin(); iter != m_vAryIndex.end(); iter++)
    {
      RpSnglSumXY *tmp = dynamic_cast<RpSnglSumXY*> (RpSnglArray->UncheckedAt(iter->second));
      os << tmp->Name()
	 << ": ID 0x"<<setw(8)<<hex<<tmp->IdCode()<<dec
	 << ", Qx "     << tmp->QVector(0)
	 << ", Qy "     << tmp->QVector(1)
	 << ", Weight " << tmp->Weight()
	 << endl;
    }
}

void RpSumXYObjectv3::Reset()
{
  RpSnglArray->Delete();
  // deflate TClonesArray again if size is larger than default
  if (RpSnglArray->GetSize() > NRPOUTS)
    {
      RpSnglArray->Expand(NRPOUTS);
    }

  m_vAryIndex.clear();
  return ;
}

int RpSumXYObjectv3::isValid() const
{
  return ((m_vAryIndex.size() > 0) ? 1 : 0);
}

//_____________________________________________________________
void RpSumXYObjectv3::AddRpSumXY(const char *name, const int id, const float qx, const float qy, const float w)
{
  int ivtx = RpSnglArray->GetLast() + 1;

  // Expand Vertex TClonesArray if neccessary
  if (ivtx == RpSnglArray->GetSize())
    {
      RpSnglArray->Expand((RpSnglArray->GetSize() + NRPOUTS));
    }

  TClonesArray &rparray = *RpSnglArray;
  new(rparray[ivtx]) RpSnglSumXYv1(name, id, qx, qy, w);

  // if order <= 0 the vertex does not take part in vertex selection
  m_vAryIndex[id] = ivtx;

  return ;
}

bool RpSumXYObjectv3::isRpSumXY(const char *name) const
{
  for (int i = 0; i <= RpSnglArray->GetLast(); i++)
  {
    RpSnglSumXY *tmp = dynamic_cast<RpSnglSumXY*> (RpSnglArray->UncheckedAt(i));
    if (!strcmp(tmp->Name(), name)) { return true; }
  }
  return false;
}

int RpSumXYObjectv3::getnRpSumXY() const
{
  return RpSnglArray->GetEntries();
}

RpSnglSumXY* RpSumXYObjectv3::getRpSumXY(const int idcode) const
{
  map<int, int>::const_iterator itr = m_vAryIndex.find(idcode);
  if(itr!=m_vAryIndex.end()){
    int idx = itr->second;
    return dynamic_cast<RpSnglSumXY*> (RpSnglArray->UncheckedAt(idx));
  }
  return NULL;
}

void RpSumXYObjectv3::setValue(const char* name, const int idcode, const int type, const float val)
{
  RpSnglSumXY *tmp = getRpSumXY(idcode);
  if(tmp==NULL){
    AddRpSumXY(name, idcode, 0, 0, 0);
    tmp = getRpSumXY(idcode);
  }
  
  switch(type){
    case 0: tmp->QVector(0, val); break;
    case 1: tmp->QVector(1, val); break;
    case 2: tmp->Weight(val); break;
    default: break;
  }
}

float RpSumXYObjectv3::getValue(const int idcode, const int type) const
{
  RpSnglSumXY *tmp = getRpSumXY(idcode);
  if(tmp==NULL){
    return -9999.0;
  }
  
  int ret = -9999.0;
  switch(type){
    case 0: ret = tmp->QVector(0); break;
    case 1: ret = tmp->QVector(1); break;
    case 2: ret = tmp->Weight(); break;
    default: break;
  }
  return ret;
}
