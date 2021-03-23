/*! \file    rd.c
 *  \brief   Recursive desent parser.
 *
 *  \details Simplified recursive desent parser. Grammar rules:
 *           PROGRAM -> begin DECLIST comma STMTLIST end
 *           DECLIST -> d X
 *           X ->semi DECLIST | %epsilon
 *           STMTLIST -> s Y
 *           Y -> semi STMTLIST | %epsilon
 *
 *           %epsilon is a special notation for empty string.
 *
 *           \license Distributed under the terms of the Zlib license,
 *                    see http://www.gzip.org/zlib/zlib_license.html
 */

#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

/* Terminals */
#define begin 0
#define comma 1
#define end   2
#define semi  3
#define d     4
#define s     5

/* EOP */
#define EOP   6

#define UNDEF 7

/* Lexeme class */
int lexeme = 0;

/* Non terminals */
void PROGRAM(), DECLIST(), STMTLIST();
void X(), Y();

/* Reject input line */
void error();

/* Look ahead lexeme */
int get_token();

char g_inputBuffer[1024] = "";
char* g_prog = NULL;

int main(int argc, char* argv[])
{
  if (1 == argc)
  {
    printf("Enter your input line: ");
    gets(g_inputBuffer);
  }
  else
  {
    strcpy(g_inputBuffer, argv[1]);
  }

  g_prog = (char*)g_inputBuffer;

  lexeme = get_token();
  PROGRAM();
  if (lexeme == EOP)
    printf("Accept.\n");
  else
    printf("Reject.\n");
}

void PROGRAM()
{
    if (lexeme != begin)
        error();
    lexeme = get_token();
    DECLIST();
    if (lexeme != comma)
        error();
    lexeme = get_token();
    STMTLIST();
    if (lexeme != end)
        error();
    lexeme = get_token();
}

void DECLIST()
{
    if (lexeme != d)
        error();
    lexeme = get_token();
    X();
}

void X()
{
    if (lexeme == semi)
    {
        lexeme = get_token();
        DECLIST();
    }
    else if (lexeme != comma)
        error();
}

void STMTLIST()
{
    if (lexeme != s)
        error();
    lexeme = get_token();
    Y();
}

void Y()
{
    if (lexeme == semi)
    {
        lexeme = get_token();
        STMTLIST();
    }
    else if (lexeme != end)
       error();
}

int get_token()
{
  char token[132] = "";
  char* tok = token;

  /* Skip spaces */
  while (isspace(*g_prog))
    ++g_prog;

  /*End line marker*/
  if ((char*)NULL == (char*)*g_prog)
    return EOP;

  /* Keywords */
  if (isalpha(*g_prog))
  {
    while (isalpha(*g_prog))
    {
      *tok = *g_prog;
      ++tok;
      ++g_prog;
    }
    *tok = '\0';
    if (0 == strcmp(token, "begin"))
        return begin;
    if (0 == strcmp(token, "comma"))
        return comma;
    if (0 == strcmp(token, "end"))
        return end;
    if (0 == strcmp(token, "semi"))
        return semi;
    if (0 == strcmp(token, "d"))
        return d;
    if (0 == strcmp(token, "s"))
        return s;
  }
  return UNDEF;
}

void error()
{
  printf("Reject.\n");
  exit(EXIT_FAILURE);
}
