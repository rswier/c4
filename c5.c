// c5.c - C in five functions

// c4.c plus
//   abstract syntax tree creation
//   back-end code generator
//   parameters passed in correct order
//   various optimizations

// Written by Robert Swierczek

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <fcntl.h>
#ifdef _WIN32
#include "w32.h"
#endif

char *p, *lp, // current position in source code
     *data;   // data/bss pointer

int *e, *le,  // current position in emitted code
    *id,      // currently parsed identifier
    *n,       // current node in abstract syntax tree
    *sym,     // symbol table (simple list of identifiers)
    tk,       // current token
    ival,     // current token value
    ty,       // current expression type
    line,     // current line number
    src,      // print source and assembly flag
    debug;    // print executed instructions

// tokens and classes (operators last and in precedence order)
enum {
  Num = 128, Fun, Sys, Glo, Loc, Id, Load, Enter,
  Char, Else, Enum, If, Int, Return, Sizeof, While,
  Assign, Cond, Lor, Lan, Or, Xor, And, Eq, Ne, Lt, Gt, Le, Ge, Shl, Shr, Add, Sub, Mul, Div, Mod, Inc, Dec, Brak
};

// opcodes
enum { LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,
       OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,
       OPEN,READ,CLOS,PRTF,MALC,MSET,MCMP,MCPY,MMAP,DSYM,QSRT,EXIT };

// types
enum { CHAR, INT, PTR };

// identifier offsets (since we can't create an ident struct)
enum { Tk, Hash, Name, Class, Type, Val, HClass, HType, HVal, Idsz };

void next()
{
  char *pp;

  while (tk = *p) {
    ++p;
    if (tk == '\n') {
      if (src) {
        printf("%d: %.*s", line, p - lp, lp);
        lp = p;
        while (le < e) {
          printf("%8.4s", &"LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,"
                           "OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,"
                           "OPEN,READ,CLOS,PRTF,MALC,MSET,MCMP,MCPY,MMAP,DSYM,QSRT,EXIT,"[*++le * 5]);
          if (*le <= ADJ) printf(" %d\n", *++le); else printf("\n");
        }
      }
      ++line;
    }
    else if (tk == '#') {
      while (*p != 0 && *p != '\n') ++p;
    }
    else if ((tk >= 'a' && tk <= 'z') || (tk >= 'A' && tk <= 'Z') || tk == '_') {
      pp = p - 1;
      while ((*p >= 'a' && *p <= 'z') || (*p >= 'A' && *p <= 'Z') || (*p >= '0' && *p <= '9') || *p == '_')
        tk = tk * 147 + *p++;
      tk = (tk << 6) + (p - pp);
      id = sym;
      while (id[Tk]) {
        if (tk == id[Hash] && !memcmp((char *)id[Name], pp, p - pp)) { tk = id[Tk]; return; }
        id = id + Idsz;
      }
      id[Name] = (int)pp;
      id[Hash] = tk;
      tk = id[Tk] = Id;
      return;
    }
    else if (tk >= '0' && tk <= '9') {
      if (ival = tk - '0') { while (*p >= '0' && *p <= '9') ival = ival * 10 + *p++ - '0'; }
      else if (*p == 'x' || *p == 'X') {
        while ((tk = *++p) && ((tk >= '0' && tk <= '9') || (tk >= 'a' && tk <= 'f') || (tk >= 'A' && tk <= 'F')))
          ival = ival * 16 + (tk & 15) + (tk >= 'A' ? 9 : 0);
      }
      else { while (*p >= '0' && *p <= '7') ival = ival * 8 + *p++ - '0'; }
      tk = Num;
      return;
    }
    else if (tk == '/') {
      if (*p == '/') {
        ++p;
        while (*p != 0 && *p != '\n') ++p;
      }
      else {
        tk = Div;
        return;
      }
    }
    else if (tk == '\'' || tk == '"') {
      pp = data;
      while (*p != 0 && *p != tk) {
        if ((ival = *p++) == '\\') {
          if ((ival = *p++) == 'n') ival = '\n';
        }
        if (tk == '"') *data++ = ival;
      }
      ++p;
      if (tk == '"') ival = (int)pp; else tk = Num;
      return;
    }
    else if (tk == '=') { if (*p == '=') { ++p; tk = Eq; } else tk = Assign; return; }
    else if (tk == '+') { if (*p == '+') { ++p; tk = Inc; } else tk = Add; return; }
    else if (tk == '-') { if (*p == '-') { ++p; tk = Dec; } else tk = Sub; return; }
    else if (tk == '!') { if (*p == '=') { ++p; tk = Ne; } return; }
    else if (tk == '<') { if (*p == '=') { ++p; tk = Le; } else if (*p == '<') { ++p; tk = Shl; } else tk = Lt; return; }
    else if (tk == '>') { if (*p == '=') { ++p; tk = Ge; } else if (*p == '>') { ++p; tk = Shr; } else tk = Gt; return; }
    else if (tk == '|') { if (*p == '|') { ++p; tk = Lor; } else tk = Or; return; }
    else if (tk == '&') { if (*p == '&') { ++p; tk = Lan; } else tk = And; return; }
    else if (tk == '^') { tk = Xor; return; }
    else if (tk == '%') { tk = Mod; return; }
    else if (tk == '*') { tk = Mul; return; }
    else if (tk == '[') { tk = Brak; return; }
    else if (tk == '?') { tk = Cond; return; }
    else if (tk == '~' || tk == ';' || tk == '{' || tk == '}' || tk == '(' || tk == ')' || tk == ']' || tk == ',' || tk == ':') return;
  }
}

void expr(int lev)
{
  int t, *d, *b;

  if (!tk) { printf("%d: unexpected eof in expression\n", line); exit(-1); }
  else if (tk == Num) { *--n = ival; *--n = Num; next(); ty = INT; }
  else if (tk == '"') {
    *--n = ival; *--n = Num; next();
    while (tk == '"') next();
    data = (char *)((int)data + sizeof(int) & -sizeof(int)); ty = PTR;
  }
  else if (tk == Sizeof) {
    next(); if (tk == '(') next(); else { printf("%d: open paren expected in sizeof\n", line); exit(-1); }
    ty = INT; if (tk == Int) next(); else if (tk == Char) { next(); ty = CHAR; }
    while (tk == Mul) { next(); ty = ty + PTR; }
    if (tk == ')') next(); else { printf("%d: close paren expected in sizeof\n", line); exit(-1); }
    *--n = (ty == CHAR) ? sizeof(char) : sizeof(int); *--n = Num;
    ty = INT;
  }
  else if (tk == Id) {
    d = id; next();
    if (tk == '(') {
      if (d[Class] != Sys && d[Class] != Fun) { printf("%d: bad function call\n", line); exit(-1); }
      next();
      t = 0; b = 0;
      while (tk != ')') { expr(Assign); *--n = (int)b; b = n; ++t; if (tk == ',') next(); }
      next();
      *--n = t; *--n = d[Val]; *--n = (int)b; *--n = d[Class];
      ty = d[Type];
    }
    else if (d[Class] == Num) { *--n = d[Val]; *--n = Num; ty = INT; }
    else {
      if (d[Class] == Loc) { *--n = d[Val]; *--n = Loc; }
      else if (d[Class] == Glo) { *--n = d[Val]; *--n = Num; }
      else { printf("%d: undefined variable\n", line); exit(-1); }
      *--n = ty = d[Type]; *--n = Load;
    }
  }
  else if (tk == '(') {
    next();
    if (tk == Int || tk == Char) {
      t = (tk == Int) ? INT : CHAR; next();
      while (tk == Mul) { next(); t = t + PTR; }
      if (tk == ')') next(); else { printf("%d: bad cast\n", line); exit(-1); }
      expr(Inc);
      ty = t;
    }
    else {
      expr(Assign);
      if (tk == ')') next(); else { printf("%d: close paren expected\n", line); exit(-1); }
    }
  }
  else if (tk == Mul) {
    next(); expr(Inc);
    if (ty > INT) ty = ty - PTR; else { printf("%d: bad dereference\n", line); exit(-1); }
    *--n = ty; *--n = Load;
  }
  else if (tk == And) {
    next(); expr(Inc);
    if (*n == Load) n = n+2; else { printf("%d: bad address-of\n", line); exit(-1); }
    ty = ty + PTR;
  }
  else if (tk == '!') {
    next(); expr(Inc);
    if (*n == Num) n[1] = !n[1]; else { *--n = 0; *--n = Num; --n; *n = (int)(n+3); *--n = Eq; }
    ty = INT;
  }
  else if (tk == '~') {
    next(); expr(Inc);
    if (*n == Num) n[1] = ~n[1]; else { *--n = -1; *--n = Num; --n; *n = (int)(n+3); *--n = Xor; }
    ty = INT;
  }
  else if (tk == Add) { next(); expr(Inc); ty = INT; }
  else if (tk == Sub) {
    next(); expr(Inc);
    if (*n == Num) n[1] = -n[1]; else { *--n = -1; *--n = Num; --n; *n = (int)(n+3); *--n = Mul; }
    ty = INT;
  }
  else if (tk == Inc || tk == Dec) {
    t = tk; next(); expr(Inc);
    if (*n == Load) *n = t; else { printf("%d: bad lvalue in pre-increment\n", line); exit(-1); }
  }
  else { printf("%d: bad expression\n", line); exit(-1); }

  while (tk >= lev) { // "precedence climbing" or "Top Down Operator Precedence" method
    t = ty; b = n;
    if (tk == Assign) {
      next();
      if (*n != Load) { printf("%d: bad lvalue in assignment\n", line); exit(-1); }
      expr(Assign); *--n = (int)(b+2); *--n = ty = t; *--n = Assign;
    }
    else if (tk == Cond) {
      next();
      expr(Assign);
      if (tk == ':') next(); else { printf("%d: conditional missing colon\n", line); exit(-1); }
      d = n;
      expr(Cond);
      --n; *n = (int)(n+1); *--n = (int)d; *--n = (int)b; *--n = Cond;
    }
    else if (tk == Lor) { next(); expr(Lan); if (*n==Num && *b==Num) n[1] = b[1] || n[1]; else { *--n = (int)b; *--n = Lor; } ty = INT; }
    else if (tk == Lan) { next(); expr(Or);  if (*n==Num && *b==Num) n[1] = b[1] && n[1]; else { *--n = (int)b; *--n = Lan; } ty = INT; }
    else if (tk == Or)  { next(); expr(Xor); if (*n==Num && *b==Num) n[1] = b[1] |  n[1]; else { *--n = (int)b; *--n = Or;  } ty = INT; }
    else if (tk == Xor) { next(); expr(And); if (*n==Num && *b==Num) n[1] = b[1] ^  n[1]; else { *--n = (int)b; *--n = Xor; } ty = INT; }
    else if (tk == And) { next(); expr(Eq);  if (*n==Num && *b==Num) n[1] = b[1] &  n[1]; else { *--n = (int)b; *--n = And; } ty = INT; }
    else if (tk == Eq)  { next(); expr(Lt);  if (*n==Num && *b==Num) n[1] = b[1] == n[1]; else { *--n = (int)b; *--n = Eq;  } ty = INT; }
    else if (tk == Ne)  { next(); expr(Lt);  if (*n==Num && *b==Num) n[1] = b[1] != n[1]; else { *--n = (int)b; *--n = Ne;  } ty = INT; }
    else if (tk == Lt)  { next(); expr(Shl); if (*n==Num && *b==Num) n[1] = b[1] <  n[1]; else { *--n = (int)b; *--n = Lt;  } ty = INT; }
    else if (tk == Gt)  { next(); expr(Shl); if (*n==Num && *b==Num) n[1] = b[1] >  n[1]; else { *--n = (int)b; *--n = Gt;  } ty = INT; }
    else if (tk == Le)  { next(); expr(Shl); if (*n==Num && *b==Num) n[1] = b[1] <= n[1]; else { *--n = (int)b; *--n = Le;  } ty = INT; }
    else if (tk == Ge)  { next(); expr(Shl); if (*n==Num && *b==Num) n[1] = b[1] >= n[1]; else { *--n = (int)b; *--n = Ge;  } ty = INT; }
    else if (tk == Shl) { next(); expr(Add); if (*n==Num && *b==Num) n[1] = b[1] << n[1]; else { *--n = (int)b; *--n = Shl; } ty = INT; }
    else if (tk == Shr) { next(); expr(Add); if (*n==Num && *b==Num) n[1] = b[1] >> n[1]; else { *--n = (int)b; *--n = Shr; } ty = INT; }
    else if (tk == Add) {
      next(); expr(Mul);
      if ((ty = t) > PTR) { if (*n == Num) n[1] = n[1] * sizeof(int); else { *--n = sizeof(int); *--n = Num; --n; *n = (int)(n+3); *--n = Mul; } }
      if (*n == Num && *b == Num) n[1] = b[1] + n[1]; else { *--n = (int)b; *--n = Add; }
    }
    else if (tk == Sub) {
      next(); expr(Mul);
      if ((ty = t) > PTR) { if (*n == Num) n[1] = n[1] * sizeof(int); else { *--n = sizeof(int); *--n = Num; --n; *n = (int)(n+3); *--n = Mul; } }
      if (*n == Num && *b == Num) n[1] = b[1] - n[1]; else { *--n = (int)b; *--n = Sub; }
    }
    else if (tk == Mul) { next(); expr(Inc); if (*n==Num && *b==Num) n[1] = b[1] * n[1]; else { *--n = (int)b; *--n = Mul; } ty = INT; }
    else if (tk == Div) { next(); expr(Inc); if (*n==Num && *b==Num) n[1] = b[1] / n[1]; else { *--n = (int)b; *--n = Div; } ty = INT; }
    else if (tk == Mod) { next(); expr(Inc); if (*n==Num && *b==Num) n[1] = b[1] % n[1]; else { *--n = (int)b; *--n = Mod; } ty = INT; }
    else if (tk == Inc || tk == Dec) {
      if (*n == Load) *n = tk; else { printf("%d: bad lvalue in post-increment\n", line); exit(-1); }
      *--n = (ty > PTR) ? sizeof(int) : sizeof(char); *--n = Num;
      *--n = (int)b; *--n = (tk == Inc) ? Sub : Add;
      next();
    }
    else if (tk == Brak) {
      next(); expr(Assign);
      if (tk == ']') next(); else { printf("%d: close bracket expected\n", line); exit(-1); }
      if (t > PTR) { if (*n == Num) n[1] = n[1] * sizeof(int); else { *--n = sizeof(int); *--n = Num; --n; *n = (int)(n+3); *--n = Mul; } }
      else if (t < PTR) { printf("%d: pointer type expected\n", line); exit(-1); }
      if (*n == Num && *b == Num) n[1] = b[1] + n[1]; else { *--n = (int)b; *--n = Add; }
      *--n = ty = t - PTR; *--n = Load;
    }
    else { printf("%d: compiler error tk=%d\n", line, tk); exit(-1); }
  }
}

void stmt()
{
  int *a, *b, *c;

  if (tk == If) {
    next();
    if (tk == '(') next(); else { printf("%d: open paren expected\n", line); exit(-1); }
    expr(Assign); a = n;
    if (tk == ')') next(); else { printf("%d: close paren expected\n", line); exit(-1); }
    stmt(); b = n;
    if (tk == Else) { next(); stmt(); c = n; } else c = 0;
    *--n = (int)c; *--n = (int)b; *--n = (int)a; *--n = Cond;
  }
  else if (tk == While) {
    next();
    if (tk == '(') next(); else { printf("%d: open paren expected\n", line); exit(-1); }
    expr(Assign); a = n;
    if (tk == ')') next(); else { printf("%d: close paren expected\n", line); exit(-1); }
    stmt();
    *--n = (int)a; *--n = While;
  }
  else if (tk == Return) {
    next();
    if (tk != ';') { expr(Assign); a = n; } else a = 0;
    if (tk == ';') next(); else { printf("%d: semicolon expected\n", line); exit(-1); }
    *--n = (int)a; *--n = Return;
  }
  else if (tk == '{') {
    next();
    *--n = ';';
    while (tk != '}') { a = n; stmt(); *--n = (int)a; *--n = '{'; }
    next();
  }
  else if (tk == ';') {
    next(); *--n = ';';
  }
  else {
    expr(Assign);
    if (tk == ';') next(); else { printf("%d: semicolon expected\n", line); exit(-1); }
  }
}

void gen(int *n)
{
  int i, *a, *b;

  i = *n;
  if (i == Num) { *++e = IMM; *++e = n[1]; }
  else if (i == Loc) { *++e = LEA; *++e = n[1]; }
  else if (i == Load) { gen(n+2); *++e = (n[1] == CHAR) ? LC : LI; }
  else if (i == Assign) { gen((int *)n[2]); *++e = PSH; gen(n+3); *++e = (n[1] == CHAR) ? SC : SI; }
  else if (i == Inc || i == Dec) {
    gen(n+2);
    *++e = PSH; *++e = (n[1] == CHAR) ? LC : LI; *++e = PSH;
    *++e = IMM; *++e = (n[1] > PTR) ? sizeof(int) : sizeof(char);
    *++e = (i == Inc) ? ADD : SUB;
    *++e = (n[1] == CHAR) ? SC : SI;
  }  
  else if (i == Cond) {
    gen((int *)n[1]);
    *++e = BZ; b = ++e;
    gen((int *)n[2]);
    if (n[3]) { *b = (int)(e + 3); *++e = JMP; b = ++e; gen((int *)n[3]); }
    *b = (int)(e + 1);
  }
  else if (i == Lor) { gen((int *)n[1]); *++e = BNZ; b = ++e; gen(n+2); *b = (int)(e + 1); }
  else if (i == Lan) { gen((int *)n[1]); *++e = BZ;  b = ++e; gen(n+2); *b = (int)(e + 1); }
  else if (i == Or)  { gen((int *)n[1]); *++e = PSH; gen(n+2); *++e = OR; }
  else if (i == Xor) { gen((int *)n[1]); *++e = PSH; gen(n+2); *++e = XOR; }
  else if (i == And) { gen((int *)n[1]); *++e = PSH; gen(n+2); *++e = AND; }
  else if (i == Eq)  { gen((int *)n[1]); *++e = PSH; gen(n+2); *++e = EQ; }
  else if (i == Ne)  { gen((int *)n[1]); *++e = PSH; gen(n+2); *++e = NE; }
  else if (i == Lt)  { gen((int *)n[1]); *++e = PSH; gen(n+2); *++e = LT; }
  else if (i == Gt)  { gen((int *)n[1]); *++e = PSH; gen(n+2); *++e = GT; }
  else if (i == Le)  { gen((int *)n[1]); *++e = PSH; gen(n+2); *++e = LE; }
  else if (i == Ge)  { gen((int *)n[1]); *++e = PSH; gen(n+2); *++e = GE; }
  else if (i == Shl) { gen((int *)n[1]); *++e = PSH; gen(n+2); *++e = SHL; }
  else if (i == Shr) { gen((int *)n[1]); *++e = PSH; gen(n+2); *++e = SHR; }
  else if (i == Add) { gen((int *)n[1]); *++e = PSH; gen(n+2); *++e = ADD; }
  else if (i == Sub) { gen((int *)n[1]); *++e = PSH; gen(n+2); *++e = SUB; }
  else if (i == Mul) { gen((int *)n[1]); *++e = PSH; gen(n+2); *++e = MUL; }
  else if (i == Div) { gen((int *)n[1]); *++e = PSH; gen(n+2); *++e = DIV; }
  else if (i == Mod) { gen((int *)n[1]); *++e = PSH; gen(n+2); *++e = MOD; }
  else if (i == Sys || i == Fun) {
    b = (int *)n[1];
    while (b) { gen(b+1); *++e = PSH; b = (int *)*b; }
    if (i == Fun) *++e = JSR; *++e = n[2];
    if (n[3]) { *++e = ADJ; *++e = n[3]; }
  }
  else if (i == While) {
    *++e = JMP; b = ++e; gen(n+2); *b = (int)(e + 1);
    gen((int *)n[1]);
    *++e = BNZ; *++e = (int)(b + 1);
  }
  else if (i == Return) { if (n[1]) gen((int *)n[1]); *++e = LEV; }
  else if (i == '{') { gen((int *)n[1]); gen(n+2); }
  else if (i == Enter) { *++e = ENT; *++e = n[1]; gen(n+2); *++e = LEV; }
  else if (i != ';') { printf("%d: compiler error gen=%d\n", line, i); exit(-1); }
}

int main(int argc, char **argv)
{
  int fd, bt, ty, poolsz, *idmain, *ast;
  int *pc, *sp, *bp, a, cycle; // vm registers
  int i, *t; // temps

  --argc; ++argv;
  if (argc > 0 && **argv == '-' && (*argv)[1] == 's') { src = 1; --argc; ++argv; }
  if (argc > 0 && **argv == '-' && (*argv)[1] == 'd') { debug = 1; --argc; ++argv; }
  if (argc < 1) { printf("usage: c5 [-s] [-d] file ...\n"); return -1; }

  if ((fd = open(*argv, 0)) < 0) { printf("could not open(%s)\n", *argv); return -1; }

  poolsz = 256*1024; // arbitrary size
  if (!(sym = malloc(poolsz))) { printf("could not malloc(%d) symbol area\n", poolsz); return -1; }
  if (!(le = e = malloc(poolsz))) { printf("could not malloc(%d) text area\n", poolsz); return -1; }
  if (!(data = malloc(poolsz))) { printf("could not malloc(%d) data area\n", poolsz); return -1; }
  if (!(sp = malloc(poolsz))) { printf("could not malloc(%d) stack area\n", poolsz); return -1; }
  if (!(ast = malloc(poolsz))) { printf("could not malloc(%d) abstract syntax tree area\n", poolsz); return -1; }
  ast = (int *)((int)ast + poolsz); // abstract syntax tree is most efficiently built as a stack

  memset(sym,  0, poolsz);
  memset(e,    0, poolsz);
  memset(data, 0, poolsz);

  p = "char else enum if int return sizeof while "
      "open read close printf malloc memset memcmp memcpy mmap dlsym qsort exit void main";
  i = Char; while (i <= While) { next(); id[Tk] = i++; } // add keywords to symbol table
  i = OPEN; while (i <= EXIT) { next(); id[Class] = Sys; id[Type] = INT; id[Val] = i++; } // add library to symbol table
  next(); id[Tk] = Char; // handle void type
  next(); idmain = id; // keep track of main

  if (!(lp = p = malloc(poolsz))) { printf("could not malloc(%d) source area\n", poolsz); return -1; }
  if ((i = read(fd, p, poolsz-1)) <= 0) { printf("read() returned %d\n", i); return -1; }
  p[i] = 0;
  close(fd);

  // parse declarations
  line = 1;
  next();
  while (tk) {
    bt = INT; // basetype
    if (tk == Int) next();
    else if (tk == Char) { next(); bt = CHAR; }
    else if (tk == Enum) {
      next();
      if (tk != '{') next();
      if (tk == '{') {
        next();
        i = 0;
        while (tk != '}') {
          if (tk != Id) { printf("%d: bad enum identifier %d\n", line, tk); return -1; }
          next();
          if (tk == Assign) {
            next();
            n = ast; expr(Cond);
            if (*n != Num) { printf("%d: bad enum initializer\n", line); return -1; }
            i = n[1];
          }
          id[Class] = Num; id[Type] = INT; id[Val] = i++;
          if (tk == ',') next();
        }
        next();
      }
    }
    while (tk != ';' && tk != '}') {
      ty = bt;
      while (tk == Mul) { next(); ty = ty + PTR; }
      if (tk != Id) { printf("%d: bad global declaration\n", line); return -1; }
      if (id[Class]) { printf("%d: duplicate global definition\n", line); return -1; }
      next();
      id[Type] = ty;
      if (tk == '(') { // function
        id[Class] = Fun;
        id[Val] = (int)(e + 1);
        next(); i = 2;
        while (tk != ')') {
          ty = INT;
          if (tk == Int) next();
          else if (tk == Char) { next(); ty = CHAR; }
          while (tk == Mul) { next(); ty = ty + PTR; }
          if (tk != Id) { printf("%d: bad parameter declaration\n", line); return -1; }
          if (id[Class] == Loc) { printf("%d: duplicate parameter definition\n", line); return -1; }
          id[HClass] = id[Class]; id[Class] = Loc;
          id[HType]  = id[Type];  id[Type] = ty;
          id[HVal]   = id[Val];   id[Val] = i++;
          next();
          if (tk == ',') next();
        }
        next();
        if (tk != '{') { printf("%d: bad function definition\n", line); return -1; }
        i = 0;
        next();
        while (tk == Int || tk == Char) {
          bt = (tk == Int) ? INT : CHAR;
          next();
          while (tk != ';') {
            ty = bt;
            while (tk == Mul) { next(); ty = ty + PTR; }
            if (tk != Id) { printf("%d: bad local declaration\n", line); return -1; }
            if (id[Class] == Loc) { printf("%d: duplicate local definition\n", line); return -1; }
            id[HClass] = id[Class]; id[Class] = Loc;
            id[HType]  = id[Type];  id[Type] = ty;
            id[HVal]   = id[Val];   id[Val] = --i;
            next();
            if (tk == ',') next();
          }
          next();
        }
        n = ast;
        *--n = ';'; while (tk != '}') { t = n; stmt(); *--n = (int)t; *--n = '{'; }
        *--n = -i; *--n = Enter;
        gen(n);
        id = sym; // unwind symbol table locals
        while (id[Tk]) {
          if (id[Class] == Loc) {
            id[Class] = id[HClass];
            id[Type] = id[HType];
            id[Val] = id[HVal];
          }
          id = id + Idsz;
        }
      }
      else {
        id[Class] = Glo;
        id[Val] = (int)data;
        data = data + sizeof(int);
      }
      if (tk == ',') next();
    }
    next();
  }

  if (!(pc = (int *)idmain[Val])) { printf("main() not defined\n"); return -1; }
  if (src) return 0;

  // setup stack
  sp = (int *)((int)sp + poolsz);
  *--sp = EXIT; // call exit if main returns
  *--sp = PSH; t = sp;
  *--sp = (int)argv;
  *--sp = argc;
  *--sp = (int)t;

  // run...
  cycle = 0;
  while (1) {
    i = *pc++; ++cycle;
    if (debug) {
      printf("%d> %.4s", cycle,
        &"LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,"
         "OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,"
         "OPEN,READ,CLOS,PRTF,MALC,MSET,MCMP,MCPY,MMAP,DSYM,QSRT,EXIT,"[i * 5]);
      if (i <= ADJ) printf(" %d\n", *pc); else printf("\n");
    }
    if      (i == LEA) a = (int)(bp + *pc++);                             // load local address
    else if (i == IMM) a = *pc++;                                         // load global address or immediate
    else if (i == JMP) pc = (int *)*pc;                                   // jump
    else if (i == JSR) { *--sp = (int)(pc + 1); pc = (int *)*pc; }        // jump to subroutine
    else if (i == BZ)  pc = a ? pc + 1 : (int *)*pc;                      // branch if zero
    else if (i == BNZ) pc = a ? (int *)*pc : pc + 1;                      // branch if not zero
    else if (i == ENT) { *--sp = (int)bp; bp = sp; sp = sp - *pc++; }     // enter subroutine
    else if (i == ADJ) sp = sp + *pc++;                                   // stack adjust
    else if (i == LEV) { sp = bp; bp = (int *)*sp++; pc = (int *)*sp++; } // leave subroutine
    else if (i == LI)  a = *(int *)a;                                     // load int
    else if (i == LC)  a = *(char *)a;                                    // load char
    else if (i == SI)  *(int *)*sp++ = a;                                 // store int
    else if (i == SC)  a = *(char *)*sp++ = a;                            // store char
    else if (i == PSH) *--sp = a;                                         // push

    else if (i == OR)  a = *sp++ |  a;
    else if (i == XOR) a = *sp++ ^  a;
    else if (i == AND) a = *sp++ &  a;
    else if (i == EQ)  a = *sp++ == a;
    else if (i == NE)  a = *sp++ != a;
    else if (i == LT)  a = *sp++ <  a;
    else if (i == GT)  a = *sp++ >  a;
    else if (i == LE)  a = *sp++ <= a;
    else if (i == GE)  a = *sp++ >= a;
    else if (i == SHL) a = *sp++ << a;
    else if (i == SHR) a = *sp++ >> a;
    else if (i == ADD) a = *sp++ +  a;
    else if (i == SUB) a = *sp++ -  a;
    else if (i == MUL) a = *sp++ *  a;
    else if (i == DIV) a = *sp++ /  a;
    else if (i == MOD) a = *sp++ %  a;

    else if (i == OPEN) a = open((char *)*sp, sp[1]);
    else if (i == READ) a = read(*sp, (char *)sp[1], sp[2]);
    else if (i == CLOS) a = close(*sp);
    else if (i == PRTF) a = printf((char *)*sp, sp[1], sp[2], sp[3], sp[4], sp[5]);
    else if (i == MALC) a = (int)malloc(*sp);
    else if (i == MSET) a = (int)memset((char *)*sp, sp[1], sp[2]);
    else if (i == MCMP) a = memcmp((char *)*sp, (char *)sp[1], sp[2]);
    else if (i == MCPY) a = (int)memcpy((char *)*sp, (char *)sp[1], sp[2]);
    else if (i == MMAP) a = (int)mmap((char *)*sp, sp[1], sp[2], sp[3], sp[4], sp[5]);
    else if (i == DSYM) a = (int)dlsym((char *)*sp, (char *)sp[1]);
    else if (i == QSRT) qsort((char *)sp, sp[1], sp[2], (void *)sp[3]);
    else if (i == EXIT) { printf("exit(%d) cycle = %d\n", *sp, cycle); return *sp; }
    else { printf("unknown instruction = %d! cycle = %d\n", i, cycle); return -1; }
  }
}
