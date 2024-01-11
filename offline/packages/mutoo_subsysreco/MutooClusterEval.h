// $Id: MutooClusterEval.h,v 1.4 2010/09/10 16:37:52 abhisek Exp $
#ifndef MutooClusterEval_h
#define MutooClusterEval_h

//////////////////////////////////////////////////////////////
/*!
\file MutooClusterEval.h
\brief cluster evaluation ntuple
\author  Hugo Pereira
\version $Revision: 1.4 $
\date $Date: 2010/09/10 16:37:52 $
*/
//////////////////////////////////////////////////////////////

#include <string>
#include <SubsysReco.h>

#ifndef __CINT__
#include <TMutClusMap.h>
#include <TMutHitMap.h>
#include <boost/array.hpp>
#include <PHTimeServer.h>
#else
class TMutHitMap;
class TMutClusMap;
#endif


class PHCompositeNode;
class PHTimer;
class TTree;

//////////////////////////////////////////////////////////////
/*!
\class MutooClusterEval
\brief cluster evaluation ntuple
*/
//////////////////////////////////////////////////////////////

class MutooClusterEval: public SubsysReco
{
    public:

    //! constructor
    MutooClusterEval(
        const char* name = "MUTOOCLISTEREVAL",
        const char* file_name = "cluster_evaluation.root"
        );

    //! destructor
    virtual ~MutooClusterEval()
    {}

    //! run initialisation
    int InitRun(PHCompositeNode *topNode);

    //! unique initialisation
    int Init(PHCompositeNode *topNode);

    //! event processing
    int process_event(PHCompositeNode *topNode);

    //! full end
    int End(PHCompositeNode *topNode);

    //! supermodule name
    const char *Name() const{return ThisName.c_str(); }

    //! defines mutoo evaluation filename
    void set_file_name( const char* file )
    { if( file ) _file_name = std::string( file ); }

    // set extended tree
     void set_extended_tree( bool value )
     {  _extended_tree = value; }

    protected:

    //! retrieves all used maps from top_node_vtx_map
    void set_interface_ptrs( PHCompositeNode* top_node );

    //! fill tree for a given cathode
    void fill_tree( void );

    private:

    //! hit map
    TMutHitMap* _hit_map;

    //! cluster map
    TMutClusMap* _clus_map;

    //! mutoo evaluation file
    std::string _file_name;

    #ifndef __CINT__
    //! Timer
    PHTimeServer::timer _timer;
    #endif

    //! tree
    TTree* _eval_clusters;

    //!@name 'short' tree variables
    //@{

    //! arm
    int _arm;

    //! station
    int _station;

    //! octant
    int _octant;

    //! halfoctant
    int _half;

    //! gap
    int _gap;

    //! cathode
    int _cathode;

    //! number of hits in this event and this arm
    /*! this is usefull to make a "centrality-like" cut, in e.g. Au+Au */
    int _hit_mult;

    //! true if cluster is associated to a track
    bool _has_track;

    //! clusters flags
    bool _peak_bound;
    bool _low_charge;
    bool _high_charge;
    bool _has_attenuated_strip;
    bool _has_saturated_strip;
    bool _has_bad_strip;

    //! associated cluster size
    int _clus_size;

    //! number of coordinates associated to this cluster
    int _n_coords;

    //! associated cluster charge
    /*!
    it is the sum of the constituting hit charges,
    and _not_ the result of the mathieson fit
    */
    float _clus_q;

    //! this is the rms of the charge per hit for hits in the cluster
    float _clus_q_rms;

    //! associated cluster chisquare
    float _clus_chi2;

   // Store charge values
     float _Q[8];

   // store charge errors
     float _Qe[8];
 
   // have some more infos in extended tree
     bool _extended_tree; 
  
   // width of the coords
     float _w[4];

   // width error of the coords
     float _w_err[4];   

   // peak charges from coords
     float _q_peak[4];


    //@}

};

#endif
