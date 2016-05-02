ICC=icc
CCFLAGS=-openmp -mkl
#DEPS = Shared.h DTime.h BackProp.h FileIO.hh
OBJ = BackProp.o NeuralNet.o DTime.o FileIO.o
phi: CCFLAGS += -O3 -mmic
phi: DNN copy clean
debug: CCFLAGS += -DDEBUG -g
debug: DNN 
cpu:  CCFLAGS += -O3
cpu: DNN
%.o: %.cpp
	$(ICC) -c -o $@ $< $(CCFLAGS)

DNN: $(OBJ)
	$(ICC) -o $@ $^ $(CCFLAGS)
clean:
	@echo "Removing *.o and executable files"
	rm -f *.o DNN 
