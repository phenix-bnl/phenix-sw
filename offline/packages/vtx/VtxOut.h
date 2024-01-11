#ifndef __VTXOUT_H
#define __VTXOUT_H

//!
#include <iostream>
#include <PHObject.h>
#include <VtxOrdercodes.h>

class PHPoint;

//!
class VtxOut: public PHObject
{
  public:
    
  //! dtor
  virtual ~VtxOut() {}

  //! clone
  virtual VtxOut* clone() const;

  //! isValid returns non zero if object contains Event vertex for reconstruction
  virtual int isValid() const;

  /*! identify Function from PHObject
      @param os Output Stream 
   */
  virtual void identify(std::ostream& out = std::cout) const;

  //!  Clear Event
  virtual void Reset();

  /*! copy data over from other VtxOut daughter class
      @param vtxout pointer to input VtxOut class
   */
  virtual void FillFromClass(const VtxOut *vtxout);

  //
  virtual int DeepCopy(const VtxOut *vtxout);

  /*! set Bbc ZVertex and ZVertex Error
      @param vtx Valid Bbc ZVertex 
      @param vtxerr Error on Valid Bbc ZVertex 
   */
  virtual int set_BbcVtx(const float vtx, const float vtxerr);

  /*! set Bbc ZVertex
      @param vtx Valid Bbc ZVertex 
   */
  virtual int set_BbcVtx(const float vtx);

  /*! set Mvd Vertex (3 components)
      @param vtx Valid Mvd Vertex (3 component PHPoint) 
      @param vtxerr Error on Valid Mvd Vertex (3 component PHPoint) 
   */
  virtual int set_MvdVtx(const PHPoint *vtx, const PHPoint *vtxerr);

  /*! set Mvd Vertex (3 components)
      @param vtx Valid Mvd Vertex (3 component PHPoint) 
   */
  virtual int set_MvdVtx(const PHPoint *vtx);

  /*! set Mvd Vertex (3 components)
      @param vtx Valid Mvd Vertex (3 component PHPoint) 
      @param vtxerr Error on Valid Mvd Vertex (3 component PHPoint) 
      @param conflevel confidence level of Valid Mvd Vertex
   */
  virtual int set_MvdVtx(const PHPoint *vtx, const PHPoint *vtxerr, const float conflevel);

  /*! set Muon ZVertex and ZVertex Error
      @param vtx Valid Muon ZVertex 
      @param vtxerr Error onValid Muon ZVertex 
   */
  virtual int set_MuonVtx(const float vtx, const float vtxerr);

  /*! set Muon ZVertex and ZVertex Error
      @param vtx Valid Muon ZVertex 
   */
  virtual int set_MuonVtx(const float vtx);

  /*! set Ntc ZVertex and ZVertex Error
      @param vtx Valid Ntc ZVertex 
      @param vtxerr Error on Valid Ntc ZVertex 
   */
  virtual int set_NtcVtx(const float vtx, const float vtxerr);

  /*! set Cgl ZVertex and ZVertex Error
      @param vtx Valid Cgl ZVertex 
      @param vtxerr Error on Valid Cgl ZVertex 
   */
  virtual int set_CglVtx(const float vtx, const float vtxerr);

  /*! set Pad ZVertex and ZVertex Error
      @param vtx Valid Pad ZVertex 
      @param vtxerr Error on Valid Pad ZVertex 
   */
  virtual int set_PadVtx(const float vtx, const float vtxerr);

  /*! set Zdc ZVertex and ZVertex Error
      @param vtx Valid Zdc ZVertex 
   */
  virtual int set_ZdcVtx(const float vtx);

  /*! set Zdc ZVertex and ZVertex Error
      @param vtx Valid Zdc ZVertex 
      @param vtxerr Error on Valid Zdc ZVertex 
   */
  virtual int set_ZdcVtx(const float vtx, const float vtxerr);


  /*! set Default Vertex and Vertex Error used if no vertex found
      @param vtx Default Vertex 
      @param vtxerr Default Error on Vertex 
   */
  virtual int set_DefaultVtx(const PHPoint *vtx, const PHPoint *vtxerr);

  /*! set Vertex used in reconstruction
      @param vtx reconstruction Vertex 
   */
  virtual int set_Vtx(const PHPoint *vtx);


  //! get Vertex used in reconstruction
  virtual PHPoint get_Vertex() const;
  virtual PHPoint get_Vertex(const char *name) const;

  //! get Error on Vertex used in reconstruction
  virtual PHPoint get_VertexError() const;
  virtual PHPoint get_VertexError(const char *name) const;

  //! get ZVertex used in reconstruction
  virtual float get_ZVertex() const;
  virtual float get_ZVertex(const char *name) const;

  //! get Error on ZVertex used in reconstruction
  virtual float get_ZVertexError() const;
  virtual float get_ZVertexError(const char *name) const;

  //! get Bbc ZVertex
  virtual float get_BbcVertex() const {return get_ZVertex("BBC");}

  //! get Bbc Error on ZVertex
  virtual float get_BbcVertexError() const {return get_ZVertexError("BBC");}

  //! get Muon ZVertex
  virtual float get_MuonVertex() const {return get_ZVertex("MUON");}
  //! get Muon Error on ZVertex
  virtual float get_MuonVertexError() const {return get_ZVertexError("MUON");}

  //!  get Mvd Vertex
  virtual PHPoint get_MvdVertex() const;
  //! get Mvd Error on ZVertex
  virtual PHPoint get_MvdVertexError() const;

  //! get Ntc ZVertex
  virtual float get_NtcVertex() const {return get_ZVertex("NTC");}
  //! get Ntc Error on ZVertex
  virtual float get_NtcVertexError() const {return get_ZVertexError("NTC");}

  //! get Pad ZVertex
  virtual float get_PadVertex() const {return get_ZVertex("PAD");}
  //! get Pad Error on ZVertex
  virtual float get_PadVertexError() const{return get_ZVertexError("PAD");}

  //! get Zdc ZVertex
  virtual float get_ZdcVertex() const {return get_ZVertex("ZDC");}
  //! get Zdc Error on ZVertex
  virtual float get_ZdcVertexError() const {return get_ZVertexError("ZDC");}

  virtual float get_CglVertex() const {return get_ZVertex("CGL");}
  virtual float get_CglVertexError() const{return get_ZVertexError("CGL");}

  //! get internal List of vertices
  virtual int get_VtxList() const;

  /*! override Vertex and Vertex Error used in reconstruction
      @param vtx Default Vertex 
      @param vtxerr Default Error on Vertex 
   */
  virtual int use_Vertex(const PHPoint *vtx, const PHPoint *vtxerr);

  /*! override Vertex and Vertex Error used in reconstruction
      @param subsystem use this subsystem vertex
   */
  virtual int use_Vertex(const char *subsystem);

  //! check for valid Vertex of subsystem (true -> vtx okay)
  virtual bool isVtx(const char *name) const;

  //! check for valid Bbc ZVertex (true -> vtx okay)
  virtual bool isBbcVtx() const {return isVtx("BBC");}

  //! check for valid Mvd ZVertex (true -> vtx okay)
  virtual bool isMvdVtx() const {return isVtx("MVD");}

  //! check for valid Muon ZVertex (true -> vtx okay)
  virtual bool isMuonVtx() const {return isVtx("MUON");}

  //! check for valid Ntc ZVertex (true -> vtx okay)
  virtual bool isNtcVtx() const {return isVtx("NTC");}

  //! check for valid Ntcp ZVertex (true -> vtx okay)
  virtual bool isNtcpVtx() const {return isVtx("NTCP");}

  //! check for valid Pad ZVertex (true -> vtx okay)
  virtual bool isPadVtx() const {return isVtx("PAD");}

  //! check for valid Zdc ZVertex (true -> vtx okay)
  virtual bool isZdcVtx() const {return isVtx("ZDC");}

  //! check for valid Cgl ZVertex (true -> vtx okay)
  virtual bool isCglVtx() const {return isVtx("CGL");}

  //! get character string identifying which subsystem provided Vtx
  virtual const char *which_Vtx() const;

  //! add vertex with a given name and with a given order
  virtual void AddVtx(const char *name, const float *vertex, const float *vertexerr, const short int order);
  virtual void AddVtx(const char *name, const float *vertex, const float *vertexerr, const VTX::Order order);

private:
  void virtual_warning(const char *funcname) const;
  void virtual_warning(const char *funcname, const char *param) const;

  ClassDef(VtxOut, 1)
};

#endif
