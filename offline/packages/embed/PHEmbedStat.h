#ifndef __PHEMBEDSTAT_H__
#define __PHEMBEDSTAT_H__
#include <vector>
class PHEmbedStat{
public:
  PHEmbedStat();
  std::vector<int>    dchitE;//dchitE.size() == dchit1->Entries();
  std::vector<int>    dchitStat;
  std::vector<int>    pc1rawE;
  std::vector<int>    pc1rawStat;
  std::vector<int>    pc2rawE;
  std::vector<int>    pc2rawStat;
  std::vector<int>    pc3rawE;
  std::vector<int>    pc3rawStat;
  std::vector<int>    pc1clusterE;
  std::vector<int>    pc1clusterStat;
  std::vector<int>    pc2clusterE;
  std::vector<int>    pc2clusterStat;
  std::vector<int>    pc3clusterE;
  std::vector<int>    pc3clusterStat;
  std::vector<int>    tofoutE;
  std::vector<int>    tofoutStat;
  std::vector<int>    crkhitE;
  std::vector<int>    crkhitStat;
  std::vector<int>    emctowerE;
  std::vector<int>    emctowerStat;
  std::vector<int>    emcclusterE;
  std::vector<int>    emcclusterStat;
  std::vector<int>    accrawE;
  std::vector<int>    accrawStat;
  std::vector<int>    tofwhitE;
  std::vector<int>    tofwhitStat;

  std::vector<int>&    get_dchitEmbed()      { return dchitE;}
  std::vector<int>&    get_dchitEmbedStat()  { return dchitStat;}
  std::vector<int>&    get_pc1clusterEmbed()   { return pc1clusterE;}
  std::vector<int>&    get_pc1clusterEmbedStat(){ return pc1clusterStat;}
  std::vector<int>&    get_pc2clusterEmbed()   { return pc2clusterE;}
  std::vector<int>&    get_pc2clusterEmbedStat() { return pc2clusterStat;}
  std::vector<int>&    get_pc3clusterEmbed()   { return pc3clusterE;}
  std::vector<int>&    get_pc3clusterEmbedStat(){ return pc3clusterStat;}
  std::vector<int>&    get_crkhitEmbed()     { return crkhitE;}
  std::vector<int>&    get_crkhitEmbedStat() { return crkhitStat;}
  std::vector<int>&    get_tofoutEmbed()     { return tofoutE;}
  std::vector<int>&    get_tofoutEmbedStat() { return tofoutStat;}
  std::vector<int>&    get_emctowerEmbed()     { return emctowerE;}
  std::vector<int>&    get_emctowerEmbedStat() { return emctowerStat;}
  std::vector<int>&    get_emcclusterEmbed()   { return emcclusterE;}
  std::vector<int>&    get_emcclusterEmbedStat() { return emcclusterStat;}
  std::vector<int>&    get_accrawEmbed()   { return accrawE;}
  std::vector<int>&    get_accrawEmbedStat() { return accrawStat;}
  std::vector<int>&    get_tofwhitEmbed()   { return tofwhitE;}
  std::vector<int>&    get_tofwhitEmbedStat() { return tofwhitStat;}
};
#endif
