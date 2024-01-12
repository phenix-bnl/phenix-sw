#ifndef __VtxSimreco_H__
#define __VtxSimreco_H__

// $Id: VtxSimreco.h,v 1.5 2015/06/24 03:25:45 slash Exp $

/*! 
  \file VtxSimreco.h
  \brief get simulated vertex and store into BBC node
  \version $Revision: 1.5 $
  \date $Date: 2015/06/24 03:25:45 $
*/

#include <VtxOut.h>
#include <preco/VtxReco.h>
#include <vector>

#ifndef __CINT__
#include <gsl/gsl_rng.h>
#endif

class PHCompositeNode;
class TH1;

//! store simulated vertex position in VtxOut node 
/*!
  get simulated vertex (either from fixed value or from PISA header),
  possibly add some gaussian smearing to z and t0,
  and store into VtxOut node.
*/
class VtxSimreco: public VtxReco
{
 public:
  
  //! constructor
  VtxSimreco(const std::string &name = "VTXSIMRECO");
  
  //! destructor
  virtual ~VtxSimreco();
  
  //! input source enumeration
  enum VtxSource
  {
    
    //! use PISA header to get vertex
    PISA,
    
    //! use fixed value
    FIXED
  
  };
  
  //! initialization
  int Init( PHCompositeNode* );
  
  //! module initialization
  int InitRun(PHCompositeNode* );
  
  //! event mode
  int process_event(PHCompositeNode* );
  
  //!@name flags
  //@{
   
  //! tell if x is to be smeared when loading the simulated vertex
  void SmearX( bool value )
  { _smear_x = value; }
  
  //! tell if y is to be smeared when loading the simulated vertex
  void SmearY( bool value )
  { _smear_y = value; }
    
  //! tell if z is to be smeared when loading the simulated vertex
  void SmearZ( bool value )
  { _smear_z = value; }

  //! smear vertex according to the vertex error in VtxOut
  void set_use_vtxout_error( bool a)
  {_use_vtxout_error = a;}

  //! tell if z is to be smeared by a gaussian with sigma sampled from a histogram
  void SmearZfromDistribution( bool value )
  { _smear_z_dist = value; }
  
  //! vertex selection mode
  void UseVtx( VtxSource value )
  { _vtx_source = value; }

  //! tell if simulated x and y are to be used when loading the simulated vertex
  void UseXY( bool value )
  { _use_xy = value; }

  //@}
  
  //!@name default vertex API
  //@{

  //! set fixed X vertex value
  void XVertex(const float rval) 
  { _fixed_vtx[0] = rval; }
  
  //! set fixed Y vertex value
  void YVertex(const float rval) 
  { _fixed_vtx[1] = rval; }

  //! set fixed Z vertex value
  void ZVertex(const float rval) 
  { _fixed_vtx[2] = rval; }

  //! vertex error and smearing 
  /*! 
  used to assign manually the error on the vertex position
  and smear the value accordingly, provided that _smear_x is true (the default)
  */
  void XVertexSigma(const float rval) 
  { _vtx_sigma[0] = rval; }

  //! vertex smearing 
  /*! 
  used to assign manually the error on the vertex position
  and smear the value accordingly, provided that _smear_y is true (the default)
  */
  void YVertexSigma(const float rval) 
  { _vtx_sigma[1] = rval; }

  //! vertex smearing 
  /*! 
  used to assign manually the error on the vertex position
  and smear the value accordingly, provided that _smear_z is true (the default)
  */
  void ZVertexSigma(const float rval) 
  { _vtx_sigma[2] = rval; }

  //! vertex smearing 
  /*! 
  used to assign the distribution of errors on the vertex position.
  a random sigma can be sampled from the distribution for each event.
  */
  void ZVertexSigmaDistribution(TH1* h) 
  { _zvtx_sigma_histo = h; }

  //@}
   
  //! set to true if BBCOut contents should be overwritten by sim header
  /*! this is for compatibility with the old behavior */
  void OverwriteBBC( const bool value )
  { _overwrite_bbc = value; }
  
  //! set alternative name for the node from which PISA header is to be retrieved
  /*! 
  this is needed for the muon arms embedding when one wants to pick either the foreground
  or the background sim vertex, when they are not matched */
  void PisaHeaderNode( std::string value )
  { _pisa_header_node = value; }
  
  protected:
  
  //! source name matching enum for debugging
  std::string source_name( void )
  {
    switch( _vtx_source )
    {
      case PISA: return "PISA";
      case FIXED: return "FIXED";
      default: return "UNKNOWN";
    } 
  }
  
  //! load vertex from pisa node
  bool get_mc_vertex( PHCompositeNode* top_node );			

  //! smear vertex using gaussian of width the errors
  void smear_vertex( void );
  
  //! PISA header parent node name
  /*! 
  this is needed for the muon embedding when you want to look for the PISA header node in the SIGNAL input file, 
  and not in the background input file, or vice versa. Default is empty string meaning that the first header node
  encountered starting from top_node is used
  */
  std::string _pisa_header_node;
  
  //! vertex selection mode
  VtxSource _vtx_source;

  //! smearing along x (using _vtx_sigma.getX())
  bool _smear_x;
  
  //! smearing along y (using _vtx_sigma.getY())
  bool _smear_y;
  
  //! smearing along z (using _vtx_sigma.getZ())
  bool _smear_z;

  //! use VTxOut error to smear thrown particle  
  bool _use_vtxout_error;

  //! pointer to vtxOut
  VtxOut* vtxout;

  //! smearing along z by gaussian with sigma sampled from a distribution
  bool _smear_z_dist;

  /*! if true, loads also x and y from simulated vertex, instead of z only */
  bool _use_xy;
  
  //! set to true if BBCOut contents should be overwritten by sim header
  /*! this is for compatibility with the old behavior */
  bool _overwrite_bbc; 
  
  //! current vertex
  std::vector<float> _current_vtx;
  
  //! current error
  std::vector<float> _current_vtx_error;
  
  //! fixed vertex
  std::vector<float> _fixed_vtx;
  
  //! vertex smearing and error
  std::vector<float> _vtx_sigma;
  
  //! distribution of z-vertex values to sample from 
  TH1* _zvtx_sigma_histo;

#ifndef __CINT__
  gsl_rng *rng;
#endif

};

#endif /* __VtxSimreco_H__ */
