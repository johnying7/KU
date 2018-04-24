#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include "State.hpp"

using namespace std;

State::State(int indexVal, std::vector<char> charInputs, std::vector<std::vector<int>> stateChange) {
  //IMPLEMENT ME
  //implMe();
  index = indexVal;
  inputs = charInputs;
  transitions = stateChange;
}

State::~State() {
}

bool State::isEmpty() {
  //IMPLEMENT ME
  //implMe();
  return false;
}
