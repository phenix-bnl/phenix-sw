#include "PHGlobalv4.h"

ClassImp(PHGlobalv4)

PHGlobalv4::PHGlobalv4() 
{
  Reset();
}


void PHGlobalv4::Reset() 
{
  run=0;
  seq=0;
  evt=0;
//trig=0;
  centclock=-999;
  centperp=-999;
  zdce0=0.;
  zdce1=0.;
  zdcz=-9999.;
  zdct0=-9999.;
  bbcn=0.;
  bbcs=0.;
  bbcqn=0.;
  bbcqs=0.;
  bbcz=-9999.;
  bbct0=-9999.;
  ndc=0;
  npc1=0;
  npc2=0;
  npc3=0;
  ntec=0;
  nemc=0;
  ntof=0;
  ncrk=0;
  etotw=0.;
  etote=0.;
  zvertex=-9999.; 
  ntcTimeZero	=-9999;
  ntcZVertex	=-9999;
  BunchNumber        =-9999;
  YellowBunchFlag    =-9999;
  BlueBunchFlag      =-9999;
  YellowPolarization =-9999;
  BluePolarization   =-9999;
  nMuoAllTracks         =-9999;
  nMuoGoodTracks     =-9999;
  for (int i=0; i<2; i++)
  {
    for (int j=0; j<3; j++)
    {
      MuoCalibCathodes[i][j]=-9999;
    }
  }
}

void PHGlobalv4::identify(std::ostream& os) const
{
  os << "identify yourself: PHGlobalv4 Object, Global Event Information." << std::endl;
}

int PHGlobalv4::isValid() const
{
  return ((run>0) ? 1 : 0);
}

float PHGlobalv4::getEmcEnergy() const 
{
    return (etote+etotw);
}

void PHGlobalv4::setZVertex(const float vtx) 
{
    zvertex=vtx;
}

float PHGlobalv4::getZVertex() const 
{
    return zvertex;
}

void PHGlobalv4::setRunNumber(const int crun) 
{
    run = crun;
}

int PHGlobalv4::getRunNumber() const 
{
    return run;
}

void PHGlobalv4::setRunSequence(const int cseq) 
{
  seq =cseq;
}

int PHGlobalv4::getRunSequence() const 
{
  return seq;
}

void PHGlobalv4::setEventNumber(const int cevt)
{
  evt = cevt;
}

int PHGlobalv4::getEventNumber() const 
{
  return evt;
}

/*
void PHGlobalv4::setTriggerWord(const int ctrig){
  trig = ctrig;
}
int PHGlobalv4::getTriggerWord() const {
  return trig;
}
*/

void PHGlobalv4::setZdcZVertex(const float czdcz){
  zdcz = czdcz;
}
float PHGlobalv4::getZdcZVertex() const {
  return zdcz;
}

void PHGlobalv4::setZdcEnergyNS(const float zdceNorth, const float zdceSouth){
  zdce0 = zdceNorth;
  zdce1 = zdceSouth;
}
float PHGlobalv4::getZdcEnergyN() const {
  return zdce0;
}
float PHGlobalv4::getZdcEnergyS() const {
  return zdce1;
}

void PHGlobalv4::setZdcTimeZero(const float czdct0) {
  zdct0 = czdct0;
}
float PHGlobalv4::getZdcTimeZero()  const {
  return zdct0;
}

void PHGlobalv4::setBbcMultNS(const short int  bbcNorth, const short int  bbcSouth) {
  bbcn = bbcNorth;
  bbcs = bbcSouth;
}

short int PHGlobalv4::getBbcMultN()  const {
  return (short int) bbcn;
}

short int PHGlobalv4::getBbcMultS()  const {
  return (short int) bbcs;
}

void PHGlobalv4::setBbcChargeNS(const float bbcqNorth, const float bbcqSouth) {
  bbcqn = bbcqNorth;
  bbcqs = bbcqSouth;
}
float PHGlobalv4::getBbcChargeN()  const {
  return bbcqn;
}
float PHGlobalv4::getBbcChargeS()  const {
  return bbcqs;
}

void PHGlobalv4::setBbcZVertex(const float cbbcz){
  bbcz = cbbcz;
}
float PHGlobalv4::getBbcZVertex() const {
  return bbcz;
}

void PHGlobalv4::setBbcTimeZero(const float cbbct0){
  bbct0 = cbbct0;
}
float PHGlobalv4::getBbcTimeZero() const {
  return bbct0;
}


void PHGlobalv4::setNumberDchTracks(const short int num){
  ndc = num;
}
short int PHGlobalv4::getNumberDchTracks() const {
  return ndc;
}

void PHGlobalv4::setNumberPC1Hits(const short int num) {
  npc1 = num;
}
short int PHGlobalv4::getNumberPC1Hits()  const {
  return npc1;
}

void PHGlobalv4::setNumberPC2Hits(const short int num) {
  npc2 = num;
}
short int PHGlobalv4::getNumberPC2Hits()  const {
  return npc2;
}

void PHGlobalv4::setNumberPC3Hits(const short int num) {
  npc3 = num;
}
short int PHGlobalv4::getNumberPC3Hits()  const {
  return npc3;
}

void PHGlobalv4::setNumberTecTracks(const short int num) {
  ntec = num;
}
short int PHGlobalv4::getNumberTecTracks()  const {
  return ntec;
}

void PHGlobalv4::setNumberEmcClusters(const short int num) {
  nemc = num;
}
short int PHGlobalv4::getNumberEmcClusters()  const {
  return nemc;
}

void PHGlobalv4::setNumberTofHits(const short int cntof){
  ntof = cntof;
}
short int PHGlobalv4::getNumberTofHits() const {
  return ntof;
}

void PHGlobalv4::setNumberCerenkovHits(const short int cncrk){
  ncrk = cncrk;
}
short int PHGlobalv4::getNumberCerenkovHits() const {
  return ncrk;
}

void PHGlobalv4::setEmcEnergyEW(const float east, const float west){
  etote = east;
  etotw = west;
}
float PHGlobalv4::getEmcEnergyW() const {
  return etotw;
}
float PHGlobalv4::getEmcEnergyE() const {
  return etote;
}

void PHGlobalv4::setCentralitybyClock(const int centvalue) {
  centclock=centvalue;
}

int PHGlobalv4::getCentralitybyClock() const {
  return centclock;
}

void PHGlobalv4::setCentralitybyPerp(const int centvalue) {
  centperp=centvalue;
}

int PHGlobalv4::getCentralitybyPerp() const {
  return centperp;
}

void PHGlobalv4::copy(PHCompositeNode* /*topNode*/)
{
  std::cout << "Error::The copy() function is not implemented for v4 PHGlobals." << std::endl;
}

