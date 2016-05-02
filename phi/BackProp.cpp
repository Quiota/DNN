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
 // Description: This file defines the class that implements the back-propagation algorithm. This algorithm uses mini-batch stochastic gradient descent to minimize a cross-entropy function.
 // Reference: Please cite as M. Dixon, D. Klabjan, and J. H. Bang. "Implementing Deep Neural Networks for Financial Market Prediction on the Intel Xeon Phi" at the Eighth Workshop on High Performance Computational Finance (WHPCF'15), held in conjunction with Supercomputing 2015, Austin, TX, November 2015. Available at SSRN: http://ssrn.com/abstract=2627258 or http://dx.doi.org/10.2139/ssrn.2627258.
 // Dependencies: Intel MKL Random Number generator and Intel MKL BLAS
 // Revision: 1.0

#include "BackProp.h"
#include "DTime.h"

//initializes and allocates memory on heap
CBackProp::CBackProp(int nl,int ni,int batch_size_,int nSymbols_,int *sz, double b):numi(ni),batch_size(batch_size_),nSymbols(nSymbols_),beta(b)
{
	double tstart, tstop = 0.0;

	//set no of layers and their sizes
	numl=nl;
	lsize=new int[numl];
	lsize_sums = new int[numl];

    sz_sum = 0;
	for(int i=0;i<numl;i++){
		lsize[i]=sz[i];
		lsize_sums[i] = sz_sum * batch_size;
		sz_sum += sz[i];
	}

	//allocate memory for output of each neuron
	out = new double[numl * batch_size * sz_sum];

	//allocate memory for delta
	delta = new double*[numl];

	for(int i=1;i<numl;i++){
		delta[i]=new double[lsize[i]*batch_size];
	}

	//allocate memory for weights
	weight = new double*[numl];

	for(int i=1;i<numl;i++){
		weight[i]=new double[lsize[i]*(lsize[i-1]+1)];
	}

	//allocate memory for previous weights
	prevDwt = new double*[numl];

	for(int i=1;i<numl;i++){
		prevDwt[i]=new double[lsize[i]*(lsize[i-1]+1)];
	}

	//seed and assign random weights using MKL random number generator
        VSLStreamStatePtr stream;
        double* rnd_data;
        vslNewStream(&stream, VSL_BRNG_SFMT19937, 777);
	for(int i=1;i<numl;i++){
                int total = (lsize[i-1]+1)*lsize[i];
	        rnd_data = new double[total];
                tstart = dtime();
		vdRngUniform(VSL_RNG_METHOD_UNIFORM_STD, stream, total, rnd_data, -1.0, 1.0);
                tstop = dtime();
	        std::cout<<"Layer "<<i<<" rng generation: "<< (tstop-tstart)<<"(s)"<<std::endl;
		for(int j=0;j<lsize[i];j++){
                        #pragma omp parallel for
			for(int k=0;k<(lsize[i-1]+1);k++){
				int jidx = k*lsize[i] + j;
				weight[i][jidx]=rnd_data[j*(lsize[i-1]+1)+k];
 			}
			#pragma omp barrier
                }
        }
        vslDeleteStream(&stream);
        delete rnd_data;
	//initialize previous weights to 0 for first iteration
        tstart = dtime();
	for(int i=1;i<numl;i++)
		for(int j=0;j<lsize[i];j++)
                        #pragma omp parallel for
			for(int k=0;k<(lsize[i-1]+1);k++){
				int jidx = k*lsize[i] + j;
				prevDwt[i][jidx]=(double)0.0;
			}
			#pragma omp barrier

        tstop = dtime();
	std::cout<<"Prev Weight Initialization: "<< (tstop-tstart)<<"(s)"<<std::endl;
}

void CBackProp::filterWeights(int i, int j, int k){
        return;
     	if (weight[i][k*lsize[i]+j] > max_weight[i][k*lsize[i]+j])
	   weight[i][k*lsize[i]+j] = max_weight[i][k*lsize[i]+j];
     	else if (weight[i][k*lsize[i]+j] < min_weight[i][k*lsize[i]+j])
	   weight[i][k*lsize[i]+j] = min_weight[i][k*lsize[i]+j];
}

CBackProp::~CBackProp()
{
	//free out
	delete[] out;

	//free delta
	for(int i=1;i<numl;i++)
		delete[] delta[i];
	delete[] delta;

	//free weight
	for(int i=1;i<numl;i++){
		delete[] weight[i];
		delete[] min_weight[i];
		delete[] max_weight[i];
	}
	delete[] weight;
	delete[] min_weight;
	delete[] max_weight;

	//free prevDwt
	for(int i=1;i<numl;i++)
		delete[] prevDwt[i];
	delete[] prevDwt;

	//free layer info
	delete[] lsize;
}

//sigmoid function
double CBackProp::sigmoid(double in) const
{
      // truncate to avoid floating point overflow
      if (in < -30.0)
	 return(0.0);
      else if(in>30.0)
	 return(1.0);
      else
         return((double)(1.0/(1.0+exp(-in))));
}

//Calculate the F1 measure
double CBackProp::F1(const float *tgt, int length, char* avg_type) const
{
     double F1;
     double prec, recall;
     int TP =0; int FN =0; int TN=0; int FP =0;
     for (int i=0;i<length;i++){
       int k=0;
       for (int j=0;j<nSymbols*3;j+=3){
		int idx = i*lsize[numl-1] + j;
		int val =(int) max(&out[lsize_sums[numl-1] + idx],3)-1;
		if (val==tgt[i*nSymbols + k]){
                   TP+=1; TN+=2;
		}
		else{
		   FN+=1;FP+=1;TN+=1;
		}
		k++;
	}
     }
     std::cout<<"TP="<<TP<<", FP="<<FP<<", FN="<<FN<<std::endl;
     prec = (double)TP/(TP+FP); recall = (double)TP/(TP+FN);
     std::cout<<"prec="<<prec<<", recall="<<recall<<std::endl;

     F1 =0.5*prec*recall/(prec+recall);

     return(F1);
}

// Calculate error rate of confusion matrix
double CBackProp::error_rate(const float *tgt, int length) const{

   double error = 0.0;
   std::cout<<"In error_rate function"<<std::endl;
   //#pragma omp parallel for
   for (int i=0;i<length;i++){
       int k=0;
       for (int j=0;j<nSymbols*3;j+=3){
		int idx = i*lsize[numl-1]+ j;
		int val =(int) max(&out[lsize_sums[numl-1] + idx],3)-1;
		if (val==tgt[i*nSymbols + k])
                    error+=1.0;
		k++;
	}
    }
    double error_rate = error/((double)(length*nSymbols));
    //#pragma omp barrier

   return(error_rate);
}
//Find the maximum value in array ar
double CBackProp::max(double* ar, int len, bool idx) const
{
    int pos = 0;
    double max = 0.0;
    for (int i=0;i<len;i++){
      if (ar[i]>=max){
	max = ar[i];
	pos = i;
      }
    }
    if (idx==true)
      return(pos);
    else
      return(ar[pos]);
}

//Find the minimum value in array ar
double CBackProp::min(double* ar, int len, bool idx) const
{
    int pos = 0;
    double min = 9999999.9;
    for (int i=0;i<len;i++){
      if (ar[i]<=min){
	min = ar[i];
	pos = i;
      }
    }
    if (idx==true)
      return(pos);
    else
      return(ar[pos]);
}

//Find the average value in array ar
double CBackProp::average(double* ar, int len) const
{
    double avg = 0;
    for (int i=0;i<len;i++)
	avg+=ar[i];
    return(avg/len);
}
//Print summary properties of array ar for diagnostic purposes
void CBackProp::summary(double* ar,int len, char* name) const
{
    std::cout<<"max("<<name<<"):"<<max(ar,len,false)<<std::endl;
    std::cout<<"min("<<name<<"):"<<min(ar,len,false)<<std::endl;
    std::cout<<"average("<<name<<"):"<<average(ar,len)<<std::endl;
}

//Compute the mean square error
double CBackProp::mse(float *tgt) const
{
	double mse=0;
        for(int i=0;i<batch_size;i++)
	   for(int j=0;j<lsize[numl-1];j++){
		int idx = i*lsize[numl-1] + j;

		int out_idx = lsize_sums[numl - 1] + idx;
		mse+=(tgt[idx]-out[out_idx])*(tgt[idx]-out[out_idx]);
	}
	return mse/(2.0*batch_size);
}

//Compute the cross-entropy measure (for use with the softmax function in the final layer)
double CBackProp::cross_entropy(float *tgt, int test_size) const
{
	double entropy=0.0;
        for(int i=0;i<test_size;i++)
	   for(int j=0;j<lsize[numl-1];j++){
		int idx = i*lsize[numl-1] + j;
		entropy-=tgt[idx]*log(out[lsize_sums[numl - 1] + idx]);
	}
	return entropy;
}

//returns i'th output of the net
double CBackProp::Out(int i) const
{
	return out[lsize_sums[numl - 1] + i];
}
// resizes out (there is probably a more efficienct technique than this)
void CBackProp::change_depth(int depth){
    delete[] out;
    batch_size = depth;
    out = new double[numl * sz_sum * depth];
}

// feed forward one set of input
void CBackProp::ffwd(float *in, bool test)
{
	std::cout<<"In ffwd function"<<std::endl;
	//assign content to input layer

        #pragma omp parallel for //PROBLEM HERE
        for(int i=0;i<batch_size;i++)
	  for(int j=0;j<lsize[0];j++){
		if (std::isnan(in[i*lsize[0]+j]) || std::isinf(in[i*lsize[0]+j])){
	           std::cout<<"in["<<i*lsize[0]+j<<"]: "<<in[i*lsize[0]+j]<<std::endl;
		   in[i*lsize[0]+j]=0.0;
	       }
               out[i*lsize[0]+j]=in[i*lsize[0]+j];  // output_from_neuron(i,j) J^th neuron in I^th Layer
	  }
        #pragma omp barrier
	//assign output(activation) value
	//to each neuron usng sigmoid func
        double alpha = 1.0, beta = 1.0; /* Scaling factors */

	for(int i=1;i<numl;i++) {			// For each layer
             MKL_INT m = batch_size;
             MKL_INT n = lsize[i];
             MKL_INT k = lsize[i-1];
	     MKL_INT one = 1;

             double* sum = new double[m*n];
             #pragma omp parallel for
              for(int l=0;l<m;l++)
	       for (int j=0;j<n;j++)
                 sum[l*n+j] = weight[i][n*k + j];
             #pragma omp barrier
	//	assign output(activation) value
             cblas_dgemm(CblasRowMajor, CblasNoTrans, CblasNoTrans, m, n, k, alpha, &out[lsize_sums[i-1]], k, weight[i], n, beta, sum, n);
	     int u =0;

             #pragma omp parallel for
             for (int l=0;l<m;l++){
	        double factor = 0.0;
                for (int j=0;j<n;j++){
   	         if (i == (numl-1) && ( j%3==0 )){
	                factor = 0.0;
			double* val = new double[3];
		        for (int k=0;k<3;k++){
			    val[k]=exp(sum[l*n+j+k]);
			    // Fix numerical instabilty arising from floating point over or under flow
		            if (std::isnan(val[k]) || std::isinf(val[k]))
		               val[k]=fixNaN(val[k],1.0e-6,1.0e6);
		            factor+=val[k];
		        }
    		        double sum_check=0.0;
			//check that the probability weights sum to one (up to a tolerance)
		        for (int k=0;k<3;k++){
		          out[lsize_sums[i] + l * n + j + k]=val[k]/factor;
			  sum_check += out[lsize_sums[i] + l * n + j + k];
			}
			double eps =1.0e-3;
			if (sum_check>(1.0 +eps) || sum_check<(1.0-eps)){
		         for (int k=0;k<3;k++)
			  std::cout<<"out["<<k<<"]:"<<out[lsize_sums[i] + l * n + j + k]<<std::endl;
			  std::cout<<"sum_check="<<sum_check<<std::endl;
		        }
			delete [] val;
			u++;
		 }
  		 else if (i <(numl-1)){ //use a sigmoid or tanh function rather than a softmax function for all layers except the output layer
		        //double val=tanh(sum[l*n+j]);
		        double val=sigmoid(sum[l*n+j]);
		        if (std::isnan(val) || std::isinf(val))
		            val=fixNaN(sum[l*n+j],0.0, 1.0);
			out[lsize_sums[i] + l * n + j]= val;
		 } // else if
	     } //end j
	   } //end l
           #pragma omp barrier

	  delete [] sum;

	  } // end i
}
double CBackProp::fixNaN(double sum, double min_, double max_) const {
     double ret = 0.0;
     if (ret >0)
        return(max_);
     else
        return(0.0);
}

//Backpropagate errors from output
//layer until the first hidden layer
void CBackProp::bpgt(float* x,float *tgt)
{
        double tstart, tstop, ttime, stime = 0.0;

	//update output values for each neuron
	try{
  	  tstart = dtime();
	  ffwd(x);
          tstop = dtime();
          ttime += tstop - tstart;
          std::cout<<"> Feed forward network construction:"<<(tstop-tstart)<<"(s)"<<std::endl;
        }
	catch(std::exception& e){
          std::cerr<<"Error In ffwd: "<<e.what()<<std::endl;
	}
  	tstart = dtime();
	//find delta for output layer
        int m = lsize[numl-1];
        #pragma omp parallel for
        for(int i=0;i<batch_size; i++){
	   for(int j=0;j<lsize[numl-1];j++){
                delta[numl-1][i*m+j]=out[lsize_sums[numl - 1] + i * m + j]*
                (1-out[lsize_sums[numl - 1] + i * m + j])*(tgt[i*m+j]-out[lsize_sums[numl - 1] + i * m + j]);
	   }
	}
        #pragma omp barrier
        tstop = dtime();
        std::cout<<"delta for output later "<<(tstop-tstart)<<"(s)"<<std::endl;
	//find delta for hidden layers
  	tstart = dtime();
	for(int i=numl-2;i>0;i--){
         #pragma omp parallel for
         for(int l=0;l<batch_size;l++){
	    for(int j=0;j<lsize[i];j++){
		double sum=0.0;
		for(int k=0;k<lsize[i+1];k++){
		    int kidx = l*lsize[i+1] + k;
		    sum+=delta[i+1][kidx]*weight[i+1][j*lsize[i+1]+k];
		}
		int jidx = l*lsize[i] + j;
		delta[i][jidx]=out[lsize_sums[i] + jidx]*(1-out[lsize_sums[i] + jidx])*sum;
	    }
	 //output diagnostics for debugging code
         /*#ifdef _DEBUG_
	 for(int i=numl-1;i>0;i--){
             char* str = new char[8];
	     sprintf(str,"delta[%d]",i);
	     summary(delta[i], lsize[i]*batch_size, str);
	     sprintf(str,"out[%d]",i);
	     summary(out[i], lsize[i]*batch_size, str);
	     sprintf(str,"weight[%d]",i);
	     summary(weight[i],lsize[i]*(lsize[i-1]+1), str);
	 }
	 #endif*/
	 }
	#pragma omp barrier
        } //end i

        tstop = dtime();
        ttime += tstop - tstart;
        std::cout<<">delta propagation:"<<(tstop-tstart)<<"(s)"<<std::endl;



        double alpha_ = beta, beta_ = 1.0; // Scaling factors
        char transa = 'T', transb = 'N'; // Transposition options

        stime = 0.0;

        for(int i=1;i<numl;i++){
	  MKL_INT m = lsize[i-1] +1;
	  MKL_INT n = lsize[i];
	  MKL_INT k = batch_size;
          tstart = dtime();

/*-------------------------------------------------------------------------------------*/
          for (int st=0;st<10;st++){
             dgemm(&transa, &transb, &m, &n, &k, &alpha_, &out[lsize_sums[i-1]], &k, delta[i], &k, &beta_, weight[i], &m);
          }

          tstop = dtime();
          for (int u=0; u<(lsize[i]*(lsize[i-1]+1));u++){
             if (weight[i][u]<-100.0)
		weight[i][u]=-100.0;
             else if( weight[i][u] > 100.0)
		weight[i][u]=100.0;
	  }

        //Measure performance of DGEMM
        stime += (tstop - tstart)/10.0;
        ttime += (tstop - tstart)/10.0;
        //#ifdef _DIAGNOSTICS_
        double gflops = 10.0*(2.0e-9*m*n*k)/(tstop-tstart);

/*-----------------------------------------------------------------------------------------*/

        std::cout<<">dgemm for weights update in layer("<<i<<"): "<<(tstop-tstart)/10.0<<"(s)"<<std::endl;
	std::cout<<"GFlops/s: "<<gflops<<std::endl;
	//#endif
        }
        std::cout<<">total dgemm for weights updates"<<stime<<"(s)"<<std::endl;
        std::cout<<">Total time for bpgt: "<<ttime<<"(s)"<<std::endl;
}
