#include <SvxPixelRawHitList.h>

#include <SvxRawhitList.h>
#include <SvxRawhit.h>
#include <svxAddress.hh>

using namespace std;

ClassImp(SvxPixelRawHitList)

SvxPixelRawHitList::SvxPixelRawHitList(SvxPixelRawHitList *hitlist)
{
  for (short int i=0; i<479; i++)
    {
      vector<short> v = hitlist->get_hits(i);
      if (! v.empty())
	{
          add_chipHits(i,&v);
	}
    }
}

//============================================================================//
SvxPixelRawHitList::SvxPixelRawHitList(SvxRawhitList *hitlist)
{

  // loop over the rawhits and add them
  for (int ihit = 0; ihit < hitlist->get_nRawhits(); ihit++)
  {
    // add the rawhit.
    // NOTE: add_hit takes care of checking for validity
    add_hit(hitlist->get_Rawhit(ihit));
  }

}
//============================================================================//


//============================================================================//
void SvxPixelRawHitList::Reset()
{
  // clear the map
  m_chiphits.clear();
}
//============================================================================//


//============================================================================//
void SvxPixelRawHitList::identify(std::ostream& os) const
{
  os << "Identify yourself: SvxPixelRawHitList object:" << std::endl;
  map<short, std::vector<short> >::const_iterator iter;
  cout << "chipmap entries: " << m_chiphits.size() << endl;
  for (iter = m_chiphits.begin(); iter !=  m_chiphits.end(); ++iter)
    {
      cout << "chip id " << iter->first << ", entries " << iter->second.size() << endl;
    } 
}
//============================================================================//


//============================================================================//
void SvxPixelRawHitList::add_hit(SvxRawhit *hit)
{
  // make sure the hit pointer is valid
  if (!hit)
    return;

  // get the parameters of the hit
  int lyr = hit->get_layer();
  int ldr = hit->get_ladder();
  int sen = hit->get_sensor();
  int roc = hit->get_pixelROC();
  int chn = hit->get_channel();

  // get the chip index
  short chipID = get_chipID(lyr, ldr, sen, roc);

  // make sure it's a valid chip ID (i.e. pixel hit) then insert it
  if (chipID >= 0)
    add_hit(chipID, chn);

}
//============================================================================//


//============================================================================//
void SvxPixelRawHitList::add_hit(short chipID, short channel)
{

  // check if chip already exists and add it
  std::map<short, std::vector<short> >::iterator it = m_chiphits.find(chipID);
  if ( it != m_chiphits.end())
  {
    it->second.push_back(channel);
  }
  // else create and insert it
  else
  {
    std::vector<short> v;
    v.push_back(channel);
    m_chiphits.insert( std::pair< short, std::vector<short> >(chipID, v) );
  }
}
//============================================================================//


//============================================================================//
void SvxPixelRawHitList::add_chipHits(short chipID, std::vector<short> *hits)
{

  // check if the chip already exists and append
  std::map<short, std::vector<short> >::iterator it = m_chiphits.find(chipID);
  if ( it != m_chiphits.end())
  {
    it->second.insert( it->second.end(), (*hits).begin(), (*hits).end() );
  }
  // else insert it
  else
  {
    m_chiphits.insert( std::pair< short, std::vector<short> >(chipID, *hits) );
  }

}
//============================================================================//


//============================================================================//
std::vector<short> SvxPixelRawHitList::get_hits(short chipID) const
{

  // return hits if chip exists
  std::map<short, std::vector<short> >::const_iterator it = m_chiphits.find(chipID);
  if ( it != m_chiphits.end())
  {
    return it->second;
  }
  else
  {
    // return empty vector
    std::vector<short> v;
    return v;
  }
}
//============================================================================//


//============================================================================//
int SvxPixelRawHitList::get_nhits(short chipID)
{
  // return hits if chip exists
  std::map<short, std::vector<short> >::iterator it = m_chiphits.find(chipID);
  if ( it != m_chiphits.end())
  {
    return it->second.size();
  }
  else
  {
    return 0;
  }

}
//============================================================================//


//============================================================================//
int SvxPixelRawHitList::get_nhits()
{
  // iterate over all chips
  int nhits = 0;
  std::map<short, std::vector<short> >::iterator it;
  for ( it = m_chiphits.begin(); it != m_chiphits.end(); ++it)
  {
    nhits += it->second.size();
  }
  return nhits;
}
//============================================================================//


//============================================================================//
void SvxPixelRawHitList::reset_chip(short chipID)
{

  std::map<short, std::vector<short> >::iterator it = m_chiphits.find(chipID);
  if ( it != m_chiphits.end())
    {
     it->second.clear();
     m_chiphits.erase(it);
    }
}
//============================================================================//


//============================================================================//
int SvxPixelRawHitList::fill_rawhitlist(SvxRawhitList *hitlist,
                                        svxAddress *svx)
{

  // get around svxAddress problem by passing in a pointer to topNode,
  // and just require that svxAddress be retrieved from topNode?

  // make sure the input is a valid pointer
  if (!hitlist || !svx)
    return -1;

  // iterate over all chips
  int nhits = 0;
  std::map<short, std::vector<short> >::iterator it;
  for ( it = m_chiphits.begin(); it != m_chiphits.end(); ++it)
  {

    short chipID = it->first;
    int lyr = get_layer(chipID);
    int ldr = get_ladder(chipID);
    int sen = get_sensor(chipID);
    int roc = get_pixelROC(chipID);

    // iterate over all hit channels for this chip
    for (unsigned int ic = 0; ic < it->second.size(); ic++)
    {
      short chn = it->second.at(ic);

      // make the rawhit
      SvxRawhit *tmphit = hitlist->addRawhit();
      tmphit->set_svxSection(0);
      tmphit->set_layer( lyr );
      tmphit->set_ladder( ldr );
      tmphit->set_sensor( sen );
      tmphit->set_sensorSection( svx->getPixelSensorSection(roc, (int)chn) );
      tmphit->set_sensorReadout(0);
      tmphit->set_adc(1);
      tmphit->set_channel( chn );
      tmphit->set_pixelROC( roc );
      tmphit->set_pixelModule( svx->getPixelModuleID(lyr, ldr, sen) );

      nhits++;
    }
  }

  return nhits;
}
//============================================================================//


//============================================================================//
short SvxPixelRawHitList::get_chipID(int lyr, int ldr, int sen, int roc) const
{
  // double check input validity
  if (lyr > 1 || ldr > 19 || sen > 3 || roc > 7)
    return -1;

  // ladder index from 0 - 29
  // 10 ladders in B0
  // 20 ladders in B1
  int ladder = lyr == 0 ? ldr : ldr + 10;

  // A half-ladder has 8 chips, indexed 0-7
  // sensor 0/2: chips 0-3
  // sensor 1/3: chips 4-7
  int sen_chip = roc;
  if (sen == 1 || sen == 3)
    sen_chip -= 4;

  // There are:
  //   4   chips / sensor
  //   4 sensors / ladder
  return ladder * 4 * 4 + sen * 4 + sen_chip;
}
//============================================================================//


//============================================================================//
int SvxPixelRawHitList::get_layer(short chipID) const
{
  // check the input validity
  if (chipID < 0 || chipID >= 480)
    return -1;

  // get the ladder (0-29)
  int ldr = (int)chipID / 4 / 4;

  // get the layer for the given ladder
  // 0: 0-9, 1:10-29
  int lyr = ldr > 9 ? 1 : 0;

  return lyr;
}
//============================================================================//


//============================================================================//
int SvxPixelRawHitList::get_ladder(short chipID) const
{
  // check the input validity
  if (chipID < 0 || chipID >= 480)
    return -1;

  // get the ladder index (0-29)
  int ldr = (int)chipID / 4 / 4;

  // subtract off the layer
  ldr = ldr > 9 ? ldr - 10 : ldr;

  return ldr;
}
//============================================================================//


//============================================================================//
int SvxPixelRawHitList::get_sensor(short chipID) const
{
  // check the input validity
  if (chipID < 0 || chipID >= 480)
    return -1;

  return (int)chipID / 4 % 4;
}
//============================================================================//


//============================================================================//
int SvxPixelRawHitList::get_pixelROC(short chipID) const
{
  // check the input validity
  if (chipID < 0 || chipID >= 480)
    return -1;

  // get the sensor
  int sen = get_sensor(chipID);

  // get the chip index (0-3)
  int roc = (int)chipID % 4;

  // convert to roc (0-7)
  if (sen == 1 || sen == 3)
    roc += 4;

  return roc;
}
//============================================================================//

void
SvxPixelRawHitList::Print(const short chipID) const
{
  vector<short> hits = get_hits(chipID);
  cout << "chipID: " << chipID << " hist: " << hits.size() << endl;
  if (m_chiphits.find(chipID) != m_chiphits.end())
    {
      cout << "direct access: " << m_chiphits.find(chipID)->second.size() << endl;
    }
  return;
}
