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
 // Description: Same as FileIO.cpp, but has the try-catch error handling code removed from the two overloads of the method "loadFiles". Also, the writeResults method has been removed. This file implements code to read data for performing the Deep Neural Network Experiments.
 // Reference: Please cite as M. Dixon, D. Klabjan, and J. H. Bang. "Implementing Deep Neural Networks for Financial Market Prediction on the Intel Xeon Phi" at the Eighth Workshop on High Performance Computational Finance (WHPCF'15), held in conjunction with Supercomputing 2015, Austin, TX, November 2015. Available at SSRN: http://ssrn.com/abstract=2627258 or http://dx.doi.org/10.2139/ssrn.2627258.  
 // Dependencies: Intel MKL Random Number generator and Intel MKL BLAS
 // Revision: 1.0

#include "ReadFile.h"

void loadBinaryFile(const std::string& filename, std::vector<double>& data, int& offset, int& nRows, int& nCols){ 

  std::ifstream is(filename.c_str());
  // Determine the file length
  is.seekg (0, is.end);
  int size = is.tellg();
  //std::cout<<" Length: " <<size<<std::endl;
  is.seekg (0, is.beg);
  // Create a vector to store the data
  int sz= size/sizeof(double); 
  data.resize(offset+sz);
  // Load the data
  is.read((char*) &data[offset], size);
  //std::cout<<data[offset]<<","<<data[offset+1]<<","<<size/sizeof(double)<<std::endl;
  is.close();
  nRows = data[offset];
  nCols = data[offset+1];
  offset +=sz;
  //std::cout<<"nC:"<<nCols<<", nR:"<<nRows<<",nC*nR:"<<nCols*nRows<<",offset:"<<offset<<std::endl;

}

void loadBinaryFile(const std::string& filename, std::vector<double>& data, int& nRows, int& nCols){ 

  std::ifstream is(filename.c_str());
  // Determine the file length
  is.seekg (0, is.end);
  int size = is.tellg();
  //std::cout<<" Length: " <<size<<std::endl;
  is.seekg (0, is.beg);
  // Create a vector to store the data
  int sz= size/sizeof(double); 
  data.resize(sz);
  // Load the data
  is.read((char*) &data[0], size);
  //std::cout<<data[offset]<<","<<data[offset+1]<<","<<size/sizeof(double)<<std::endl;
  is.close();
  nRows = data[0];
  nCols = data[1];
  //std::cout<<"nC:"<<nCols<<", nR:"<<nRows<<",nC*nR:"<<nCols*nRows<<",offset:"<<offset<<std::endl;
}

int loadFiles(const std::string& filelistname, std::vector<double>& data,std::vector<std::pair<int,int> >& vec_nInputs)
{
  std::fstream file(filelistname.c_str(), std::ios::in);
  if(!file.is_open()){
    throw std::runtime_error("File not found!");
  }

  std::string filename;
  std::vector<std::string> filenames;
  while( std::getline(file, filename) ){
    filenames.push_back(filename);
    //std::vector<double> v;
    //data.push_back(v);
  }
  int nInputs = 0;
  int offset = 0;
  int nRows = 0;
  int nCols = 0;
  //#pragma omp parallel for
  for (int i=0;i<filenames.size();i++){
    //std::cout<<"Loading "<< filenames[i]<<std::endl;
    loadBinaryFile(filenames[i], data, offset, nRows, nCols);
    std::pair<int,int> pr = std::make_pair(nRows, nCols);
    vec_nInputs.push_back(pr);
    nInputs += nCols-3; // nrows, ncols, label
  }
  return(nInputs);
}

int loadFiles(const std::string& filelistname, matrix& data,std::vector<std::pair<int,int> >& vec_nInputs)
{
  std::fstream file(filelistname.c_str(), std::ios::in);
  if(!file.is_open()){
    throw std::runtime_error("File not found!");
  }

  std::string filename;
  std::vector<std::string> filenames;
  while( std::getline(file, filename) ){
    filenames.push_back(filename);
    std::vector<double> v;
    data.push_back(v);
    std::pair<int,int> pr = std::make_pair(0,0);
    vec_nInputs.push_back(pr);
  }
  int nInputs = 0;
  int nRows   = 0;
  int nCols   = 0;
  #pragma omp parallel for
  for (int i=0;i<filenames.size();i++){
    //std::cout<<"Loading "<< filenames[i]<<std::endl;
    loadBinaryFile(filenames[i], data[i], nRows, nCols);
    std::pair<int,int> pr = std::make_pair(nRows, nCols);
    vec_nInputs[i]=pr;
    nInputs += nCols-3; // nrows, ncols, label
  }
  #pragma omp barrier
  return(nInputs);
}
