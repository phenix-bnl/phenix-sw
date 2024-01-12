#ifndef __VTXPEVNTMISALIGN_H__
#define __VTXPEVNTMISALIGN_H__

#include <string>
#include <utility>
#include <map>

class TTree;
class TLine;
class TH1F;

const unsigned int _misalign_max = 1000;

class VTXPEvntMisalign
{

  public:
    
    // Constructors/Destructor
    //
    VTXPEvntMisalign();
    VTXPEvntMisalign(TTree* tree, int ladder, int chip, std::string sys);
    ~VTXPEvntMisalign();

    // Member functions
    //
    int  add_shift(int x);
    int  add_shift(int* x, unsigned int n);
    int  remove_shift(int x);
    void make_corr_histos(std::string whichsys);
    bool run_search();
    void print_image(const char* outfileimage, bool draw_DoG);
    void save_histos(const char* outname, bool draw_DoG);
    //void print_data(const char* outfiledata);

    // Get/sets
    //
    TTree* get_tree() {return _tree;}
    void   set_tree(TTree* tree) {_tree = tree;}
    
    int    get_ladder() {return _ladder;}
    void   set_ladder(int ladder) {_ladder = ladder;}
    
    int    get_chip() {return _chip;}
    void   set_chip(int chip) {_chip = chip;}
    
    std::string   get_sys() {return _sys;}
    void   set_sys(std::string sys) {_sys = sys;}
    
    int    get_nbincorr() {return _nbincorr;}
    void   set_nbincorr(int nbincorr) {_nbincorr = nbincorr;}
    
    int    get_nrebin() {return _nrebin;}
    void   set_nrebin(int nrebin) {_nrebin = nrebin;}
    
    int    get_sigma() {return _sigma;}
    void   set_sigma(int sigma) {_sigma = sigma;}
    
    float  get_spike_threshold() {return _spike_threshold;}
    void   set_spike_threshold(float spike_threshold)
        {
          _spike_threshold = spike_threshold;
          _did_set_thresh = true;
        }
    
    float  get_died_threshold() {return _died_threshold;}
    void   set_died_threshold(float thresh){_died_threshold = thresh;}
    
    float  get_threshold_multiplier() {return _threshold_multiplier;}
    void   set_threshold_multiplier(float threshold_multiplier)
        {
          _threshold_multiplier = threshold_multiplier;
        }
    
    // Return whether or not the chip died
    bool   get_did_die()  {return _did_die;}
  
    // Return whether or not the chip had max allowed # of misalignments
    bool   get_did_misalign_max()  {return _did_misalign_max;}
  
    // Return the number of event maisalignments 
    int    get_n_misalignments()  {return _disco_map.size();}
    // Return the array of event maisalignments 
    // [nmisalign][0] = envent
    // [nmisalign][0] = shift from
    // [nmisalign][0] = shift to
    void  get_misalignment_array(int array[_misalign_max][3]);


  private:

    // Member variables
    //
    TTree* _tree;
    char*  _outfilename;
    int    _ladder;
    int    _chip;
    int    _nbincorr;
    int    _nrebin;
    int    _sigma;
    float  _spike_threshold;
    float  _died_threshold;
    float  _threshold_multiplier;
    bool   _did_make_corr_histos;
    bool   _did_make_DoG_histos;
    bool   _did_spike;
    bool   _did_die;
    bool   _did_set_thresh;
    bool   _did_misalign_max;
    // maps to hold shift (first) and histograms (second)
    std::map<int,TH1F*> _corrhists;
    std::map<int,TH1F*> _DoG_hists;
    // map to hold discontinuities event (first) and shift (second) which is 
    // itself a pair with the shift from (first) and the shift to (second)
    std::map< int, std::pair<int,int> >   _disco_map;
    std::string _sys;

    // Member functions
    //
    void make_DoG_histos();
    void calculate_thresh();
    int  arm(int ladder);
    void formatline(TLine* line, bool draw_DoG);
    void formathist(TH1F* h, bool draw_DoG);
};

#endif
