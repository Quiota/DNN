#ifndef backprop_h
#define backprop_h
//////////////////////////////////////////////
//	Fully connected multilayered feed		//
//	forward	artificial neural network using	//
//	Backpropogation	algorithm for training.	//
//////////////////////////////////////////////

#include "Shared.h"
//#include<cilk/cilk.h>

//using namespace std;

class CBackProp{

//	output of each neuron
	double **out;

//	delta error value for each neuron
	double **delta;

//	vector of weights for each neuron
	double **weight;

//	vector of min weight bounds for each neuron
	double **min_weight;

//	vector of max weight bounds for each neuron
	double **max_weight;
//
//	no of layers in net
//	including input layer
	int numl;
//	
//	Mini batch size
        int batch_size;	

//      Numbee of Symbols	
        int nSymbols;	
//
//	Number of data inputs
	int numi;

//	vector of numl elements for size 
//	of each layer
	int *lsize;

//	learning rate
	double beta;

//	momentum parameter
	double alpha;

//	storage for weight-change made
//	in previous epoch
	double **prevDwt;

//	squashing function
	double sigmoid(double in) const;

public:

	~CBackProp();

//	initializes and allocates memory
	CBackProp(int nl,int ni, int batch_size, int nSymbols, int *sz,double b,double a);

	int max(double*, int length) const;

        void change_depth(int depth);
//	Calculate the error rate from the confusion matrix
	double error_rate(const float* t, int test_size) const;

//	Enforces bounds on weights
	void filterWeights(int i,int j,int k);

//	backpropogates error for one set of input
	void bpgt(float *in,float *tgt);

//	feed forwards activations for one set of inputs
	void ffwd(float *in);

//	returns mean square error of the net
	double mse(float *tgt) const;	
	
//	returns i'th output of the net
	double Out(int i) const;
};

#endif
