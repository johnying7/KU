#include <iostream>
#include <fstream>
#include "Dfa.hpp"

using namespace std;

Dfa::Dfa(std::vector<int> _nfaStates, std::vector<char> _inputs, int _stateNum)
{
  stateNum = _stateNum;
  nfaStates = _nfaStates;
  inputs = _inputs;
  dfaMarked = false;
  for(unsigned int i = 0; i < inputs.size()-1; i++) {
    stateChange[i] = -1;
  }
}

Dfa::~Dfa() {

}


bool Dfa::isEmpty() {
  //IMPLEMENT ME
  //implMe();
  return false;
}
