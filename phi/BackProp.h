/* This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */
 // Author: Matthew Dixon, Diego Klabjan, Jin Hoon Bang
 // Description: This files declares the class that implements the back-propagation algorithm. This algorithm uses mini-batch stochastic gradient descent to minimize a cross-entropy function.
 // Reference: Please cite as M. Dixon, D. Klabjan, and J. H. Bang. "Implementing Deep Neural Networks for Financial Market Prediction on the Intel Xeon Phi" at the Eighth Workshop on High Performance Computational Finance (WHPCF'15), held in conjunction with Supercomputing 2015, Austin, TX, November 2015. Available at SSRN: http://ssrn.com/abstract=2627258 or http://dx.doi.org/10.2139/ssrn.2627258.
 // Dependencies: Intel MKL Random Number generator and Intel MKL BLAS
 // Revision: 1.0

//////////////////////////////////////////////////
//	Fully connected multilayered feed	//
//	forward	artificial neural network using	//
//	Backpropogation	algorithm for training.	//
//////////////////////////////////////////////////

#ifndef backprop_h
#define backprop_h
#include "Shared.h"


class CBackProp{

//	output of each neuron
	double *out;

//  array containing sizes of each DNN layer
	int *lsize_sums;

//  total sum of the sizes of each DNN layer
	int sz_sum;

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

//      Number of Symbols
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
	CBackProp(int nl,int ni, int batch_size, int nSymbols, int *sz, double b);
	void sum(double*, int length) const;

	double max(double*, int length, bool idx=true) const;

	double min(double*, int length, bool idx=true) const;

	double average(double*, int length) const;

        void summary(double* ar,int len, char* name) const;

        double fixNaN(double sum, double min, double max) const;

        void change_depth(int depth);

//	Calculate the error rate from the confusion matrix
	double error_rate(const float* t, int test_size) const;

//      Calculate the F1 measure (harmonic mean)
        double F1(const float *tgt, int length, char* avg_type) const;

//	Enforces bounds on weights
	void filterWeights(int i,int j,int k);

//	backpropogates error for one set of input
	void bpgt(float *in,float *tgt);

//	feed forwards activations for one set of inputs
	void ffwd(float *in, bool test =false);

//      implement the cross_entropy function
	double cross_entropy(float *tgt, int test_size) const;

//	returns mean square error of the net
	double mse(float *tgt) const;

//	returns i'th output of the net
	double Out(int i) const;
};

#endif
