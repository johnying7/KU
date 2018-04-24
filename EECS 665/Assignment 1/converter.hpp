#ifndef CONVERTER_H
#define CONVERTER_H

#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include "State.hpp"
#include "Dfa.hpp"

class Converter {
  //Member fields
  //IMPLEMENT ME
private:


  //Member functions
 public:
  /**
   * Public constructor to construct the list
   */

  Converter(
    std::vector<State> _listOfStates,
    std::vector<char> _inputs,
    int _numFinalStates,
    std::vector<int> _finalStates,
    int _numTransitions,
    int _initialState,
    int _totalStates
  );

  std::vector<State> listOfStates;
  std::vector<char> inputs;
  int numFinalStates;
  std::vector<int> finalStates;
  int numTransitions;
  int initialState;
  int totalStates;
  /**
   * Destructor to destroy the list
   */
  ~Converter();

  void convertNfaToDfa();

  bool DStateHolds(std::vector<Dfa> dfa, std::vector<int> nfaStates);

  bool unmarked(std::vector<Dfa> Dstates);
  int getFirstUnmarked(std::vector<Dfa> DStates);
  /**
   * returns true if the list is empty or returns false
   */
  bool isEmpty();

  void printDfa();

  vector<int> stateMove(vector<int> states, char charInput);

  int getCharState(char charInput);

  std::vector<int> Eclosure(std::vector<int> states);

};

#endif //CONVERTER_H defined
