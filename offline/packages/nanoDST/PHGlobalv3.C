#include <iostream>
//INCLUDECHECKER: Removed this line: #include <cmath>
//INCLUDECHECKER: Removed this line: #include "PHTypedNodeIterator.h"
//INCLUDECHECKER: Removed this line: #include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHCompositeNode.h"

#include "PHGlobalv3.h"

ClassImp(PHGlobalv3)

PHGlobalv3::PHGlobalv3() 
{
  Reset();
}

void PHGlobalv3::Reset() 
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
}

void PHGlobalv3::identify(std::ostream& os) const
{
  os << "identify yourself: PHGlobalv3 Object, Global Event Information." << std::endl;
}

int PHGlobalv3::isValid() const 
{
  return ((run>0) ? 1 : 0);
}

float PHGlobalv3::getEmcEnergy() const 
{
    return (etote+etotw);
}

void PHGlobalv3::setZVertex(const float vtx) 
{
    zvertex=vtx;
}

float PHGlobalv3::getZVertex() const 
{
    return zvertex;
}

void PHGlobalv3::setRunNumber(const int crun) 
{
    run = crun;
}

int PHGlobalv3::getRunNumber() const 
{
    return run;
}

void PHGlobalv3::setRunSequence(const int cseq) 
{
  seq =cseq;
}

int PHGlobalv3::getRunSequence() const 
{
  return seq;
}

void PHGlobalv3::setEventNumber(const int cevt)
{
  evt = cevt;
}

int PHGlobalv3::getEventNumber() const 
{
  return evt;
}

/*
void PHGlobalv3::setTriggerWord(const int ctrig){
  trig = ctrig;
}
int PHGlobalv3::getTriggerWord() const {
  return trig;
}
*/

void PHGlobalv3::setZdcZVertex(const float czdcz){
  zdcz = czdcz;
}
float PHGlobalv3::getZdcZVertex() const {
  return zdcz;
}

void PHGlobalv3::setZdcEnergyNS(const float zdceNorth, const float zdceSouth){
  zdce0 = zdceNorth;
  zdce1 = zdceSouth;
}
float PHGlobalv3::getZdcEnergyN() const {
  return zdce0;
}
float PHGlobalv3::getZdcEnergyS() const {
  return zdce1;
}

void PHGlobalv3::setZdcTimeZero(const float czdct0) {
  zdct0 = czdct0;
}
float PHGlobalv3::getZdcTimeZero()  const {
  return zdct0;
}

void PHGlobalv3::setBbcMultNS(const short int  bbcNorth, const short int  bbcSouth) {
  bbcn = bbcNorth;
  bbcs = bbcSouth;
}

short int PHGlobalv3::getBbcMultN()  const {
  return (short int) bbcn;
}

short int PHGlobalv3::getBbcMultS()  const {
  return (short int) bbcs;
}

void PHGlobalv3::setBbcChargeNS(const float bbcqNorth, const float bbcqSouth) {
  bbcqn = bbcqNorth;
  bbcqs = bbcqSouth;
}
float PHGlobalv3::getBbcChargeN()  const {
  return bbcqn;
}
float PHGlobalv3::getBbcChargeS()  const {
  return bbcqs;
}

void PHGlobalv3::setBbcZVertex(const float cbbcz){
  bbcz = cbbcz;
}
float PHGlobalv3::getBbcZVertex() const {
  return bbcz;
}

void PHGlobalv3::setBbcTimeZero(const float cbbct0){
  bbct0 = cbbct0;
}
float PHGlobalv3::getBbcTimeZero() const {
  return bbct0;
}


void PHGlobalv3::setNumberDchTracks(const short int num){
  ndc = num;
}
short int PHGlobalv3::getNumberDchTracks() const {
  return ndc;
}

void PHGlobalv3::setNumberPC1Hits(const short int num) {
  npc1 = num;
}
short int PHGlobalv3::getNumberPC1Hits()  const {
  return npc1;
}

void PHGlobalv3::setNumberPC2Hits(const short int num) {
  npc2 = num;
}
short int PHGlobalv3::getNumberPC2Hits()  const {
  return npc2;
}

void PHGlobalv3::setNumberPC3Hits(const short int num) {
  npc3 = num;
}
short int PHGlobalv3::getNumberPC3Hits()  const {
  return npc3;
}

void PHGlobalv3::setNumberTecTracks(const short int num) {
  ntec = num;
}
short int PHGlobalv3::getNumberTecTracks()  const {
  return ntec;
}

void PHGlobalv3::setNumberEmcClusters(const short int num) {
  nemc = num;
}
short int PHGlobalv3::getNumberEmcClusters()  const {
  return nemc;
}

void PHGlobalv3::setNumberTofHits(const short int cntof){
  ntof = cntof;
}
short int PHGlobalv3::getNumberTofHits() const {
  return ntof;
}

void PHGlobalv3::setNumberCerenkovHits(const short int cncrk){
  ncrk = cncrk;
}
short int PHGlobalv3::getNumberCerenkovHits() const {
  return ncrk;
}

void PHGlobalv3::setEmcEnergyEW(const float east, const float west){
  etote = east;
  etotw = west;
}
float PHGlobalv3::getEmcEnergyW() const {
  return etotw;
}
float PHGlobalv3::getEmcEnergyE() const {
  return etote;
}

void PHGlobalv3::setCentralitybyClock(const int centvalue) {
  centclock=centvalue;
}

int PHGlobalv3::getCentralitybyClock() const {
  return centclock;
}

void PHGlobalv3::setCentralitybyPerp(const int centvalue) {
  centperp=centvalue;
}

int PHGlobalv3::getCentralitybyPerp() const {
  return centperp;
}

void PHGlobalv3::copy(PHCompositeNode* /*topNode*/)
{
  std::cout << "Error::The copy() function is not implemented for v3 PHGlobals." << std::endl;
}

