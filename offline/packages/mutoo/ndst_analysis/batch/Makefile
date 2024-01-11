PREFIX = .

CXXFLAGS = -O3
LDFLAGS =  -L$(OFFLINE_MAIN)/lib 

all : ManyDst
ManyDst : ManyDst.o 
	@echo "($(CXX)) creating $@"
	@$(CXX) $(CXXFLAGS) -o $@ $^ $(LDFLAGS) 
  
%.o : %.cxx 
	@echo "($(CXX)) compiling $*"
	@$(CXX) $(CXXFLAGS) -c $^ -o $@ 

clean: 
	rm -rf  *.o




