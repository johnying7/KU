#ifndef STATE_H
#define STATE_H

#include <iostream>
#include <fstream>
#include <string>
#include <vector>

using namespace std;

class State {
  //Member fields
  //IMPLEMENT ME
private:

  //Member functions
 public:
  std::vector<char> inputs;
  std::vector<std::vector<int>> transitions;
  int index;
  /**
   * Public constructor to construct the list
   */
  State(int indexVal, std::vector<char> inputs, std::vector<std::vector<int>> stateChange);

  /**
   * Destructor to destroy the list
   */
  ~State();

  /**
   * returns true if the list is empty or returns false
   */
  bool isEmpty();

};

#endif //STATE_H defined
