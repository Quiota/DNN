#include <iostream>
#include <vector>
#include <fstream>
using namespace std;

int main()
{
  vector< vector<double> > data;
  vector<double> array;
  
  for (int i=0;i< 10;i++){
    for (int j=0;j< 10;j++){
      array.push_back(1.0*j);
    }
    data.push_back(array);
   }

  for (int j=0;j< 10;j++)
    cout <<"data[0]["<<j<<"]:"<<data[0][j]<<endl;
  ofstream out("numbers", ios::out | ios::binary);
  if(!out) {
    cout << "Cannot open file.";
    return 1;
   }
  cout<<"NRows: "<<data.size()<<", NCols: "<<data[0].size()<<endl;
  for (vector<vector<double> >::iterator  it= data.begin(); it != data.end(); ++it)
     out.write((char*)&it->front(), it->size() * sizeof(double));

  out.close();

  for(int i=0; i<data.size(); i++) // clear array
    for(int j=0; j<data[0].size(); j++) // clear array
        data[i][j] = 0.0;

  ifstream in("numbers", ios::in | ios::binary);
  for (vector<vector<double> >::iterator  it= data.begin(); it != data.end(); ++it)
     in.read((char *)&it->front(), it->size() * sizeof(double));

  // see how many bytes have been read
  cout << in.gcount() << " bytes read\n";

   for(int i=0; i<data.size(); i++){ // show values read from file
     for(int j=0; j<data[0].size(); j++) // show values read from file
       cout << data[i][j] << " ";
     cout<<endl;
   }

   in.close();
  
   return 0;
  }
