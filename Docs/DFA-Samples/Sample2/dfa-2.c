/* DFA implementation Sample 2
 * is licensed under the terms of the ZLIB license.
 * see http://www.gzip.org/zlib/zlib_license.html
 */
#include <stdio.h> 

#define TOTAL_STATES            4
#define FINAL_STATES            1
#define ALPHABET_CHARCTERS      2

#define UNKNOWN_SYMBOL_ERR      0
#define NOT_REACHED_FINAL_STATE 1
#define REACHED_FINAL_STATE     2

enum DFA_STATES {q0, q1, q2, q3};   // The set Q
enum input {_0, _1};

int  g_Accepted_states[FINAL_STATES] = { q0 };     // The set F
char g_alphabet[ALPHABET_CHARCTERS] = {'0', '1'};  // The set Sigma
int  g_Transition_Table[TOTAL_STATES][ALPHABET_CHARCTERS] = {}; // Transition function
int  g_Current_state = q0; // Start state of DFA

void SetDFA_Transitions()
{
  g_Transition_Table[q0][_0] = q2;
  g_Transition_Table[q0][_1] = q1;
  g_Transition_Table[q1][_0] = q3;
  g_Transition_Table[q1][_1] = q0;
  g_Transition_Table[q2][_0] = q0;
  g_Transition_Table[q2][_1] = q3;
  g_Transition_Table[q3][_0] = q1;
  g_Transition_Table[q3][_1] = q2;
}

int DFA(const char current_symbol)
{
  int i, pos;
  for (pos = 0; pos < ALPHABET_CHARCTERS; ++pos)
    if (current_symbol == g_alphabet[pos])   
      break;     // stops if any character other than 0 or 1
  if (ALPHABET_CHARCTERS == pos)
    return UNKNOWN_SYMBOL_ERR;
  for (i = 0; i < FINAL_STATES; ++i)
  {
    g_Current_state = g_Transition_Table[g_Current_state][pos];
    if (g_Current_state == g_Accepted_states[i])
      return REACHED_FINAL_STATE;
  }
  return NOT_REACHED_FINAL_STATE;
}

int main(void)
{
  char current_symbol;
  int  result;

  SetDFA_Transitions();    // Fill transition table

  printf("Enter a string with '0' s and '1's:\nPress Enter Key to stop\n");

  while ((current_symbol = getchar()) != '\n' && current_symbol != EOF)
  {
    result = DFA(current_symbol);
    if (REACHED_FINAL_STATE != result && NOT_REACHED_FINAL_STATE != result)
    {
      break;
    }
  }

  if (REACHED_FINAL_STATE == result)  
  {
    printf("Accepted");
  }
  else
  {
    printf("Rejected");
  }
  printf("\n\n");
  return 0;
}