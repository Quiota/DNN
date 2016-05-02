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
 // Description: Header file containing links to all basic dependencies contained in "Shared.h". Also contains function prototypes which are implemented in FileIO.cpp.
 // Reference: Please cite as M. Dixon, D. Klabjan, and J. H. Bang. "Implementing Deep Neural Networks for Financial Market Prediction on the Intel Xeon Phi" at the Eighth Workshop on High Performance Computational Finance (WHPCF'15), held in conjunction with Supercomputing 2015, Austin, TX, November 2015. Available at SSRN: http://ssrn.com/abstract=2627258 or http://dx.doi.org/10.2139/ssrn.2627258.  
 // Dependencies: Intel MKL Random Number generator and Intel MKL BLAS
 // Revision: 1.0

#ifndef fileio_h
#define fileio_h
#include "Shared.h"

void loadBinaryFile(const std::string& filename, std::vector<double>& data, int& offset, int& nRows, int& nCols);
int loadFiles(const std::string& filename, std::vector<double>& data, std::vector<std::pair<int,int> >& vec_nInputs);

void loadBinaryFile(const std::string& filename, std::vector<double>& data, int& nRows, int& nCols);
int loadFiles(const std::string& filename, matrix& data, std::vector<std::pair<int,int> >& vec_nInputs);
int writeResults(float* r_hat, float* t_hat, int test_size, int nSymbols);
#endif
