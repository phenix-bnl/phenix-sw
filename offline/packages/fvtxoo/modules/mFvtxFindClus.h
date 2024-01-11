#ifndef __MFVTXFINDCLUS_HH__
#define __MFVTXFINDCLUS_HH__

#include<TFvtxClusMap.h>
#include<TFvtxHitMap.h>
#include<PHTimeServer.h>
#include<mFvtxFindClusPar.h>
//#include<MutCali.h>   *** need Fvtx equivalent
#include<FvtxGeom.h>
#include<string>
//#include<TFvtxGeo.h>   *** need this too!

#include <mFvtxModuleBase.h>

class PHCompositeNode;

namespace odbc
{
  class Connection;
}

class mFvtxFindClus : public mFvtxModuleBase
{
public:

  //constructor
  mFvtxFindClus();

  //destructor
  ~mFvtxFindClus();

  //event method
  PHBoolean event(PHCompositeNode*);
  void init(PHCompositeNode*){};
  void init_run(PHCompositeNode*); //! initialization and readin dead wedge database
  void end(PHCompositeNode*){};
private:

  //gets local pointer to useful nodes
  void
  set_interface_ptrs(PHCompositeNode* top_node);

  //find all clusters
  void
  find_clusters();

  bool
  find_clusters(const int& arm, const int& cage, const int& station,
      const int& sector, const int& column, const FvtxColumn * fvtx_column);

  void
  apply_cluster_cuts();

  bool
  excuse_strip(const FvtxColumn * fvtx_column, int strip);

  //module parameters
  const mFvtxFindClusPar* _mod_par;

  //hit map node
  TFvtxHitMap* _hit_map;

  //cluster map node
  TFvtxClusMap* _clus_map;

  //module timer
  PHTimeServer::timer _timer;

  // database connection
  odbc::Connection * _con;

  ////////////////////////////////////////////////////////
  //!@name handle dead channel maps, for historical compatibility - Obsolete, use TFvtxDeadMap instead
  //@{

  /*! \brief Decoder for the dead channel records in database - Obsolete, use TFvtxDeadMap instead
   *
   record is an integer, packed in the form abcddeffff,
   where a=arm, b=cage, c=station, dd=wedge, e=side, ffff=strip.
   Values that are 'too high', for example strip=9999 means 'all strips'.
   Thus the last number in the example is 1132189999,
   which is arm=1, cage=1, station=3, wedge=21, side=8 (both sides), and strip=9999 (all strips).
   */
  class dead_map_decoder
  {

  public:
    dead_map_decoder(int record);

    void
    print() const;

    int orig_record;
    int arm;
    int cage;
    int station;
    int sector;
    int side;
    int strip;
  };
  //! load the dead channel map at run starts - Obsolete, use TFvtxDeadMap instead
  void
  load_dead_chan_map(void);

  //! automatic select and load the dead channel map according to runnum - Obsolete, use TFvtxDeadMap instead
  void
  automatic_load_dead_chan_map(void);

  //! load the dead channel map with given name - Obsolete, use TFvtxDeadMap instead
  void
  load_dead_chan_map(const std::string &);

  //! load the dead channel map with given name - Obsolete, use TFvtxDeadMap instead
  void
  load_dead_chan(const mFvtxFindClus::dead_map_decoder &map, const int nrows);
  //@}
};

#endif /* __MFVTXFINDCLUS_HH__ */
