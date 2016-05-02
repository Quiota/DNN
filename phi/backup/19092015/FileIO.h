#ifndef fileio_h
#define fileio_h
#include "Shared.h"

void loadBinaryFile(const std::string& filename, std::vector<double>& data, int& offset, int& nRows, int& nCols);
int loadFiles(const std::string& filename, std::vector<double>& data, std::vector<std::pair<int,int> >& vec_nInputs);

void loadBinaryFile(const std::string& filename, std::vector<double>& data, int& nRows, int& nCols);
int loadFiles(const std::string& filename, matrix& data, std::vector<std::pair<int,int> >& vec_nInputs);
#endif
