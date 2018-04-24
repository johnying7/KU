#include <iostream>
#include <fstream>
#include "converter.hpp"
#include <stack>

using namespace std;

Converter::Converter(
  std::vector<State> _listOfStates,
  std::vector<char> _inputs,
  int _numFinalStates,
  std::vector<int> _finalStates,
  int _numTransitions,
  int _initialState,
  int _totalStates)
{
  //IMPLEMENT ME
  //implMe();
  listOfStates = _listOfStates;
  inputs = _inputs;
  numFinalStates = _numFinalStates;
  finalStates = _finalStates;
  numTransitions = _numTransitions;
  initialState = _initialState;
  totalStates = _totalStates;
}

Converter::~Converter() {

}

void Converter::convertNfaToDfa() {
  //std::vector<Dfa> DStates;
  return;
}

bool Converter::DStateHolds(std::vector<Dfa> dfa, std::vector<int> nfaStates) {
  return false;
}

//checks if the unmarked states exists in dfa
bool Converter::unmarked(std::vector<Dfa> dStates) {
  for(unsigned int i = 0; i < dStates.size(); i++) {
    if(!dStates[i].dfaMarked)
      return true;
  }
  return false;
}

//move from one state to another given the character input
vector<int> Converter::stateMove(vector<int> states, char charInput) {
  vector<int> pMoveStates;
  int charMove = getCharState(charInput);

  for(unsigned int i = 0; i < states.size(); i++) {
    vector<int> transitionStates = listOfStates[states].transitions[i][charMove];

    for (int j = 0; j < transitionStates.size(); j++) {
      if(find(pMoveStates.begin(), pMoveStates.end(), transitionStates[i]) == pMoveStates.end()) {
        pMoveStates.push_back(j);
      }
    }
  }
  sort(pMoveStates.begin(), pMoveStates.end());
  return pMoveStates;
}

int Converter::getCharState(char charInput) {
  for(unsigned int i = 0; i < inputs.size(); i++) {
    if(inputs[i] == charInput)
      return i;
  }
  return -1;
}

//executes E closure
std::vector<int> Converter::Eclosure(std::vector<int> states) {
  unsigned int copyNumtransitions = numTransitions;
  vector<int> closure = states;
  stack<int> stack;

  for(unsigned int i = 0; i < states.size(); i++) {
    stack.push(i);
  }

  while(!stack.empty()) {
    int state = stack.top();
    stack.pop();

    vector<int> eStates = listOfStates[states].transitions[state-1];
  }
}

int Converter::getFirstUnmarked(std::vector<Dfa> DStates) {
  return -1;
}

bool Converter::isEmpty() {
  //IMPLEMENT ME
  //implMe();
  return false;
}

void Converter::printDfa() {
  // std::cout << "Initial State: {"<< DFAinitial(DFA) << "}\n" << "Final States: ";
  // printVector(getDFAFinalStates(DFA, finalStates));
  // std::cout << "\nState  ";
  // for(int i = 0; i < inputCharacters.size()-1; i++) {
  //   std::cout<< inputCharacters[i] << "      ";
  // }
  // std::cout << "\n";
  // for(int i=0; i < DFA.size(); i++) {
  //   std::cout<< i+1 << "      ";
  //   for(int j=0; j < inputCharacters.size()-1; j++) {
  //     if(DFA[i].transitions[j] > 0) {
  //       std::cout<< "{" <<DFA[i].transitions[j]<< "}    ";
  //     }
  //     else {
  //       std::cout<< "{}     ";
  //     }
  //   }
  //   std::cout<< "\n";
  // }
}
