// $Id: MutooHitEval.h,v 1.7 2010/08/25 22:21:15 hpereira Exp $
#ifndef MutooHitEval_h
#define MutooHitEval_h

//////////////////////////////////////////////////////////////
/*!
\file MutooHitEval.h
\brief single hit evaluation ntuple
\author  Hugo Pereira
\version $Revision: 1.7 $
\date $Date: 2010/08/25 22:21:15 $
*/
//////////////////////////////////////////////////////////////

#include <string>
#include <SubsysReco.h>

#ifndef __CINT__
#include <TMutHitMap.h>
#include <boost/array.hpp>
#include <PHTimeServer.h>
#else
class TMutHitMap;
#endif


class PHCompositeNode;
class PHTimer;
class TTree;

//////////////////////////////////////////////////////////////
/*!
\class MutooHitEval
\brief single hit evaluation ntuple
*/
//////////////////////////////////////////////////////////////

class MutooHitEval: public SubsysReco
{
    public:

    //! constructor
    MutooHitEval(
        const char* name = "MUTOOHITEVAL",
        const char* file_name = "hit_evaluation.root"
        );

    //! destructor
    virtual ~MutooHitEval()
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

    /*! \brief
    set extended_tree to true. That significanly increases the
    size of the output but contains more information
    */
    void set_extended_tree( bool value )
    { _extended_tree = value; }

    protected:

    //! retrieves all used maps from top_node_vtx_map
    void set_interface_ptrs( PHCompositeNode* top_node );

    //! fill tree for a given cathode
    void fill_tree( void );

    private:

    //! hit map
    TMutHitMap* _hit_map;

    //! mutoo evaluation file
    std::string _file_name;

    #ifndef __CINT__
    //! Timer
    PHTimeServer::timer _timer;
    #endif

    //! tree
    TTree* _eval_hits;

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

    //! strip
    int _strip;

    //! number of hits in this event and this arm
    /*! this is usefull to make a "centrality-like" cut, in e.g. Au+Au */
    int _hit_mult;

    //! true charge
    float _q;

    //! error on charge
    float _error;

    //! 1 if strip is saturated
    bool _saturated;

    //! 1 if strip is attenuated
    bool _attenuated;

    //! 1 if strip is peak strip in cluster
    bool _is_peak;

    //! true if hit is associated to a track
    bool _has_track;

    //! associated cluster size
    int _clus_size;

    //! associated cluster charge
    /*!
    it is the sum of the constituting hit charges,
    and _not_ the result of the mathieson fit
    */
    float _clus_q;

    //! associated cluster chisquare
    float _clus_chi2;

    //@}

    //! @name extended tree variables
    //@{

    //! strip gain
    float _calib_gain;

    //! strip calibration rms
    float _calib_rms;

    //! strip calibration pedestal
    float _calib_ped;

    #ifndef __CINT__
    //! hit samples
    boost::array<int,4> _samples;

    //! amu cells
    boost::array<int,4> _amu;

    //! hit samples (converted to charge)
    boost::array<float,4> _qsamples;
    #endif

    //! RMS between charge samples
    float _qsamples_rms;

    //! MC charge
    float _q_mc;

    //! total charge from the MC Hit
    float _q_tot_mc;

    //! packet id
    int _packet_id;

    //@}

    //! if true all variables are recorded
    bool _extended_tree;

};

#endif
