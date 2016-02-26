// c4.c - C in four functions

// char, int, structs, and pointer types
// if, while, return, switch, and expression statements
// just enough features to allow self-compilation and a bit more

// Written by Robert Swierczek

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <fcntl.h>

char *p, *lp, // current position in source code
     *data,   // data/bss pointer
     *ops;    // opcodes

int *e, *le,  // current position in emitted code
    *cas,     // case statement patch-up pointer
    *brk,     // break statement patch-up pointer
    *def,     // default statement patch-up pointer
    *tsize,   // array (indexed by type) of type sizes
    tnew,     // next available type
    tk,       // current token
    ival,     // current token value
    ty,       // current expression type
    loc,      // local variable offset
    line,     // current line number
    src,      // print source and assembly flag
    debug;    // print executed instructions

// identifier
struct ident_s {
  int tk;
  int hash;
  char *name;
  int class;
  int type;
  int val;
  int stype;
  int hclass;
  int htype;
  int hval;
} *id,  // currently parsed identifier
  *sym; // symbol table (simple list of identifiers)

struct member_s {
  struct ident_s *id;
  int offset;
  int type;
  struct member_s *next;
} **members; // array (indexed by type) of struct member lists

// tokens and classes (operators last and in precedence order)
enum {
  Num = 128, Fun, Sys, Glo, Loc, Id,
  Break, Case, Char, Default, Else, Enum, If, Int, Return, Sizeof, Struct, Switch, While,
  Assign, Cond, Lor, Lan, Or, Xor, And, Eq, Ne, Lt, Gt, Le, Ge, Shl, Shr, Add, Sub, Mul, Div, Mod, Inc, Dec, Dot, Arrow, Brak
};

// opcodes
enum { LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,
       OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,
       OPEN,READ,CLOS,PRTF,MALC,MSET,MCMP,EXIT };

// types
enum { CHAR, INT, PTR = 256, PTR2 = 512 };
       
void next()
{
  char *pp;

  while (tk = *p) {
    ++p;
    if ((tk >= 'a' && tk <= 'z') || (tk >= 'A' && tk <= 'Z') || tk == '_') {
      pp = p - 1;
      while ((*p >= 'a' && *p <= 'z') || (*p >= 'A' && *p <= 'Z') || (*p >= '0' && *p <= '9') || *p == '_')
        tk = tk * 147 + *p++;
      tk = (tk << 6) + (p - pp);
      id = sym;
      while (id->tk) {
        if (tk == id->hash && !memcmp(id->name, pp, p - pp)) { tk = id->tk; return; }
        id = id + 1;
      }
      id->name = pp;
      id->hash = tk;
      tk = id->tk = Id;
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
    switch (tk) {
    case '\n':
      if (src) {
        printf("%d: %.*s", line, p - lp, lp);
        lp = p;
        while (le < e) {
          printf("%8.4s", &ops[*++le * 5]);
          if (*le <= ADJ) printf(" %d\n", *++le); else printf("\n");
        }
      }
      ++line;
    case ' ': case '\t': case '\v': case '\f': case '\r':
      break;
    case '/':
      if (*p == '/') {
    case '#':
        while (*p != 0 && *p != '\n') ++p;
      } else {
        tk = Div;
        return;
      }
      break;
    case '\'':
    case '"':
      pp = data;
      while (*p != 0 && *p != tk) {
        if ((ival = *p++) == '\\') {
          switch (ival = *p++) {
          case 'n': ival = '\n'; break;
          case 't': ival = '\t'; break;
          case 'v': ival = '\v'; break;
          case 'f': ival = '\f'; break;
          case 'r': ival = '\r';
          }
        }
        if (tk == '"') *data++ = ival;
      }
      ++p;
      if (tk == '"') ival = (int)pp; else tk = Num;
      return;
    case '=': if (*p == '=') { ++p; tk = Eq; } else tk = Assign; return;
    case '+': if (*p == '+') { ++p; tk = Inc; } else tk = Add; return;
    case '-': if (*p == '-') { ++p; tk = Dec; } else if (*p == '>') { ++p; tk = Arrow; } else tk = Sub; return;
    case '!': if (*p == '=') { ++p; tk = Ne; } return;
    case '<': if (*p == '=') { ++p; tk = Le; } else if (*p == '<') { ++p; tk = Shl; } else tk = Lt; return;
    case '>': if (*p == '=') { ++p; tk = Ge; } else if (*p == '>') { ++p; tk = Shr; } else tk = Gt; return;
    case '|': if (*p == '|') { ++p; tk = Lor; } else tk = Or; return;
    case '&': if (*p == '&') { ++p; tk = Lan; } else tk = And; return;
    case '^': tk = Xor; return;
    case '%': tk = Mod; return;
    case '*': tk = Mul; return;
    case '[': tk = Brak; return;
    case '?': tk = Cond; return;
    case '.': tk = Dot; return;
    default: return;
    }
  }
}

void expr(int lev)
{
  int t, *b, sz;
  struct ident_s *d;
  struct member_s *m;

  switch (tk) {
  case 0: printf("%d: unexpected eof in expression\n", line); exit(-1);
  case Num: *++e = IMM; *++e = ival; next(); ty = INT; break;
  case '"':
    *++e = IMM; *++e = ival; next();
    while (tk == '"') next();
    data = (char *)((int)data + sizeof(int) & -sizeof(int)); ty = PTR;
    break;
  case Sizeof:
    next(); if (tk == '(') next(); else { printf("%d: open paren expected in sizeof\n", line); exit(-1); }
    ty = INT; if (tk == Int) next(); else if (tk == Char) { next(); ty = CHAR; }
    else if (tk == Struct) { next(); if (tk != Id) { printf("%d: bad struct type\n", line); exit(-1); } ty = id->stype; next(); }
    while (tk == Mul) { next(); ty = ty + PTR; }
    if (tk == ')') next(); else { printf("%d: close paren expected in sizeof\n", line); exit(-1); }
    *++e = IMM; *++e = ty >= PTR ? sizeof(int) : tsize[ty];
    ty = INT;
    break;
  case Id:
    d = id; next();
    if (tk == '(') {
      next();
      t = 0;
      while (tk != ')') { expr(Assign); *++e = PSH; ++t; if (tk == ',') next(); }
      next();
      if (d->class == Sys) *++e = d->val;
      else if (d->class == Fun) { *++e = JSR; *++e = d->val; }
      else { printf("%d: bad function call\n", line); exit(-1); }
      if (t) { *++e = ADJ; *++e = t; }
      ty = d->type;
    }
    else if (d->class == Num) { *++e = IMM; *++e = d->val; ty = INT; }
    else {
      if (d->class == Loc) { *++e = LEA; *++e = loc - d->val; }
      else if (d->class == Glo) { *++e = IMM; *++e = d->val; }
      else { printf("%d: undefined variable\n", line); exit(-1); }
      if ((ty = d->type) <= INT || ty >= PTR) *++e = (ty == CHAR) ? LC : LI;
    }
    break;
  case '(':
    next();
    if (tk == Int || tk == Char || tk == Struct) {
      if (tk == Int) { next(); t = INT; } else if (tk == Char) { next(); t = CHAR; }
      else { next(); if (tk != Id) { printf("%d: bad struct type\n", line); exit(-1); } t = id->stype; next(); }
      while (tk == Mul) { next(); t = t + PTR; }
      if (tk == ')') next(); else { printf("%d: bad cast\n", line); exit(-1); }
      expr(Inc);
      ty = t;
    }
    else {
      expr(Assign);
      if (tk == ')') next(); else { printf("%d: close paren expected\n", line); exit(-1); }
    }
    break;
  case Mul:
    next(); expr(Inc);
    if (ty > INT) ty = ty - PTR; else { printf("%d: bad dereference\n", line); exit(-1); }
    if (ty <= INT || ty >= PTR) *++e = (ty == CHAR) ? LC : LI;
    break;
  case And:
    next(); expr(Inc);
    if (*e == LC || *e == LI) --e; // XXX else { printf("%d: bad address-of\n", line); exit(-1); }
    ty = ty + PTR;
    break;
  case '!': next(); expr(Inc); *++e = PSH; *++e = IMM; *++e = 0; *++e = EQ; ty = INT; break;
  case '~': next(); expr(Inc); *++e = PSH; *++e = IMM; *++e = -1; *++e = XOR; ty = INT; break;
  case Add: next(); expr(Inc); ty = INT; break;
  case Sub:
    next(); *++e = IMM;
    if (tk == Num) { *++e = -ival; next(); } else { *++e = -1; *++e = PSH; expr(Inc); *++e = MUL; }
    ty = INT;
    break;
  case Inc: case Dec:
    t = tk; next(); expr(Inc);
    if (*e == LC) { *e = PSH; *++e = LC; }
    else if (*e == LI) { *e = PSH; *++e = LI; }
    else { printf("%d: bad lvalue in pre-increment\n", line); exit(-1); }
    *++e = PSH;
    *++e = IMM; *++e = ty >= PTR2 ? sizeof(int) : (ty >= PTR) ? tsize[ty - PTR] : 1;
    *++e = (t == Inc) ? ADD : SUB;
    *++e = (ty == CHAR) ? SC : SI;
    break;
  default: printf("%d: bad expression\n", line); exit(-1);
  }

  while (tk >= lev) { // "precedence climbing" or "Top Down Operator Precedence" method
    t = ty;
    switch (tk) {
    case Assign:
      next();
      if (*e == LC || *e == LI) *e = PSH; else { printf("%d: bad lvalue in assignment\n", line); exit(-1); }
      expr(Assign); *++e = ((ty = t) == CHAR) ? SC : SI;
      break;
    case Cond:
      next();
      *++e = BZ; b = ++e;
      expr(Assign);
      if (tk == ':') next(); else { printf("%d: conditional missing colon\n", line); exit(-1); }
      *b = (int)(e + 3); *++e = JMP; b = ++e;
      expr(Cond);
      *b = (int)(e + 1);
      break;
    case Lor: next(); *++e = BNZ; b = ++e; expr(Lan); *b = (int)(e + 1); ty = INT; break;
    case Lan: next(); *++e = BZ;  b = ++e; expr(Or);  *b = (int)(e + 1); ty = INT; break;
    case Or:  next(); *++e = PSH; expr(Xor); *++e = OR;  ty = INT; break;
    case Xor: next(); *++e = PSH; expr(And); *++e = XOR; ty = INT; break;
    case And: next(); *++e = PSH; expr(Eq);  *++e = AND; ty = INT; break;
    case Eq:  next(); *++e = PSH; expr(Lt);  *++e = EQ;  ty = INT; break;
    case Ne:  next(); *++e = PSH; expr(Lt);  *++e = NE;  ty = INT; break;
    case Lt:  next(); *++e = PSH; expr(Shl); *++e = LT;  ty = INT; break;
    case Gt:  next(); *++e = PSH; expr(Shl); *++e = GT;  ty = INT; break;
    case Le:  next(); *++e = PSH; expr(Shl); *++e = LE;  ty = INT; break;
    case Ge:  next(); *++e = PSH; expr(Shl); *++e = GE;  ty = INT; break;
    case Shl: next(); *++e = PSH; expr(Add); *++e = SHL; ty = INT; break;
    case Shr: next(); *++e = PSH; expr(Add); *++e = SHR; ty = INT; break;
    case Add:
      next(); *++e = PSH; expr(Mul);
      sz = (ty = t) >= PTR2 ? sizeof(int) : ty >= PTR ? tsize[ty - PTR] : 1;
      if (sz > 1) { *++e = PSH; *++e = IMM; *++e = sz; *++e = MUL;  }
      *++e = ADD;
      break;
    case Sub:
      next(); *++e = PSH; expr(Mul);
      sz = t >= PTR2 ? sizeof(int) : t >= PTR ? tsize[t - PTR] : 1;
      if (t == ty && sz > 1) { *++e = SUB; *++e = PSH; *++e = IMM; *++e = sz; *++e = DIV; ty = INT; }
      else if (sz > 1) { *++e = PSH; *++e = IMM; *++e = sz; *++e = MUL; *++e = SUB; }
      else *++e = SUB;
      ty = t;
      break;
    case Mul: next(); *++e = PSH; expr(Inc); *++e = MUL; ty = INT; break;
    case Div: next(); *++e = PSH; expr(Inc); *++e = DIV; ty = INT; break;
    case Mod: next(); *++e = PSH; expr(Inc); *++e = MOD; ty = INT; break;
    case Inc: case Dec:
      if (*e == LC) { *e = PSH; *++e = LC; }
      else if (*e == LI) { *e = PSH; *++e = LI; }
      else { printf("%d: bad lvalue in post-increment\n", line); exit(-1); }
      sz = ty >= PTR2 ? sizeof(int) : ty >= PTR ? tsize[ty - PTR] : 1;
      *++e = PSH; *++e = IMM; *++e = sz;
      *++e = (tk == Inc) ? ADD : SUB;
      *++e = (ty == CHAR) ? SC : SI;
      *++e = PSH; *++e = IMM; *++e = sz;
      *++e = (tk == Inc) ? SUB : ADD;
      next();
      break;
    case Dot:
      ty = ty + PTR;
    case Arrow:
      if (ty <= PTR+INT || ty >= PTR2) { printf("%d: structure expected\n", line); exit(-1); }
      next();
      if (tk != Id) { printf("%d: structure member expected\n", line); exit(-1); }
      m = members[ty - PTR]; while (m && m->id != id) m = m->next;
      if (!m) { printf("%d: structure member not found\n", line); exit(-1); }
      if (m->offset) { *++e = PSH; *++e = IMM; *++e = m->offset; *++e = ADD; }
      ty = m->type;
      if (ty <= INT || ty >= PTR) *++e = (ty == CHAR) ? LC : LI;
      next();
      break;
    case Brak:
      next(); *++e = PSH; expr(Assign);
      if (tk == ']') next(); else { printf("%d: close bracket expected\n", line); exit(-1); }
      if (t < PTR) { printf("%d: pointer type expected\n", line); exit(-1); }
      sz = (t = t - PTR) >= PTR ? sizeof(int) : tsize[t];
      if (sz > 1) { *++e = PSH; *++e = IMM; *++e = sz; *++e = MUL;  }
      *++e = ADD;
      if ((ty = t) <= INT || ty >= PTR) *++e = (ty == CHAR) ? LC : LI;
      break;
    default: printf("%d: compiler error tk=%d\n", line, tk); exit(-1);
    }
  }
}

void stmt()
{
  int *a, *b, *d, i;

  switch (tk) {
  case If:
    next();
    if (tk == '(') next(); else { printf("%d: open paren expected\n", line); exit(-1); }
    expr(Assign);
    if (tk == ')') next(); else { printf("%d: close paren expected\n", line); exit(-1); }
    *++e = BZ; b = ++e;
    stmt();
    if (tk == Else) {
      *b = (int)(e + 3); *++e = JMP; b = ++e;
      next();
      stmt();
    }
    *b = (int)(e + 1);
    return;
  case While:
    next();
    a = e + 1;
    if (tk == '(') next(); else { printf("%d: open paren expected\n", line); exit(-1); }
    expr(Assign);
    if (tk == ')') next(); else { printf("%d: close paren expected\n", line); exit(-1); }
    *++e = BZ; b = ++e;
    stmt();
    *++e = JMP; *++e = (int)a;
    *b = (int)(e + 1);
    return;
  case Switch:
    next();
    if (tk == '(') next(); else { printf("%d: open paren expected\n", line); exit(-1); }
    expr(Assign);
    if (tk == ')') next(); else { printf("%d: close paren expected\n", line); exit(-1); }
    a = cas; *++e = JMP; cas = ++e;
    b = brk; d = def; brk = def = 0;
    stmt();
    *cas = def ? (int)def : (int)(e + 1); cas = a; 
    while (brk) { a = (int *)*brk; *brk = (int)(e + 1); brk = a; }
    brk = b; def = d;
    return;
  case Case:
    *++e = JMP; ++e; *e = (int)(e + 7); *++e = PSH; i = *cas; *cas = (int)e; 
    next();
    expr(Or);
    if (e[-1] != IMM) { printf("%d: bad case immediate\n", line); exit(-1); }
    *e = *e - i; *++e = SUB; *++e = BNZ; cas = ++e; *e = i + e[-3];
    if (tk == ':') next(); else { printf("%d: colon expected\n", line); exit(-1); }
    stmt();
    return;
  case Break:
    next();
    if (tk == ';') next(); else { printf("%d: semicolon expected\n", line); exit(-1); }
    *++e = JMP; *++e = (int)brk; brk = e;
    return;
  case Default:
    next();
    if (tk == ':') next(); else { printf("%d: colon expected\n", line); exit(-1); }
    def = e + 1;
    stmt();
    return;
  case Return:
    next();
    if (tk != ';') expr(Assign);
    *++e = LEV;
    if (tk == ';') next(); else { printf("%d: semicolon expected\n", line); exit(-1); }
    return;
  case '{':
    next();
    while (tk != '}') stmt();
    next();
    return;
  case ';':
    next();
    return;
  default:
    expr(Assign);
    if (tk == ';') next(); else { printf("%d: semicolon expected\n", line); exit(-1); }
  }
}

int main(int argc, char **argv)
{
  int fd, bt, mbt, ty, poolsz;
  struct ident_s *idmain, *d;
  struct member_s *m;
  int *pc, *sp, *bp, a, cycle; // vm registers
  int i, *t; // temps

  --argc; ++argv;
  if (argc > 0 && **argv == '-' && (*argv)[1] == 's') { src = 1; --argc; ++argv; }
  if (argc > 0 && **argv == '-' && (*argv)[1] == 'd') { debug = 1; --argc; ++argv; }
  if (argc < 1) { printf("usage: c4 [-s] [-d] file ...\n"); return -1; }

  if ((fd = open(*argv, 0)) < 0) { printf("could not open(%s)\n", *argv); return -1; }

  poolsz = 256*1024; // arbitrary size
  if (!(sym = malloc(poolsz))) { printf("could not malloc(%d) symbol area\n", poolsz); return -1; }
  if (!(le = e = malloc(poolsz))) { printf("could not malloc(%d) text area\n", poolsz); return -1; }
  if (!(data = malloc(poolsz))) { printf("could not malloc(%d) data area\n", poolsz); return -1; }
  if (!(sp = malloc(poolsz))) { printf("could not malloc(%d) stack area\n", poolsz); return -1; }
  if (!(tsize = malloc(PTR * sizeof(int)))) { printf("could not malloc() tsize area\n"); return -1; }
  if (!(members = malloc(PTR * sizeof(struct member_s *)))) { printf("could not malloc() members area\n"); return -1; }

  memset(sym,  0, poolsz);
  memset(e,    0, poolsz);
  memset(data, 0, poolsz);
  memset(tsize,   0, PTR * sizeof(int));
  memset(members, 0, PTR * sizeof(struct member_s *));
  
  ops = "LEA  IMM  JMP  JSR  BZ   BNZ  ENT  ADJ  LEV  LI   LC   SI   SC   PSH  "
        "OR   XOR  AND  EQ   NE   LT   GT   LE   GE   SHL  SHR  ADD  SUB  MUL  DIV  MOD  "
        "OPEN READ CLOS PRTF MALC MSET MCMP EXIT ";
              
  p = "break case char default else enum if int return sizeof struct switch while "
      "open read close printf malloc memset memcmp exit void main";
  i = Break; while (i <= While) { next(); id->tk = i++; } // add keywords to symbol table
  i = OPEN; while (i <= EXIT) { next(); id->class = Sys; id->type = INT; id->val = i++; } // add library to symbol table
  next(); id->tk = Char; // handle void type
  next(); idmain = id; // keep track of main

  if (!(lp = p = malloc(poolsz))) { printf("could not malloc(%d) source area\n", poolsz); return -1; }
  if ((i = read(fd, p, poolsz-1)) <= 0) { printf("read() returned %d\n", i); return -1; }
  p[i] = 0;
  close(fd);

  // add primitive types
  tsize[tnew++] = sizeof(char);
  tsize[tnew++] = sizeof(int);
  
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
            if (tk != Num) { printf("%d: bad enum initializer\n", line); return -1; }
            i = ival;
            next();
          }
          id->class = Num; id->type = INT; id->val = i++;
          if (tk == ',') next();
        }
        next();
      }
    }
    else if (tk == Struct) {
      next();
      if (tk == Id) {
        if (!id->stype) id->stype = tnew++;
        bt = id->stype;
        next();
      } else { 
        bt = tnew++;
      }
      if (tk == '{') {
        next();
        if (members[bt]) { printf("%d: duplicate structure definition\n", line); return -1; }
        i = 0;
        while (tk != '}') {
          mbt = INT;
          if (tk == Int) next();
          else if (tk == Char) { next(); mbt = CHAR; }
          else if (tk == Struct) {
            next(); 
            if (tk != Id) { printf("%d: bad struct declaration\n", line); return -1; }
            mbt = id->stype;
            next();
          }
          while (tk != ';') {
            ty = mbt;
            while (tk == Mul) { next(); ty = ty + PTR; }
            if (tk != Id) { printf("%d: bad struct member definition\n", line); return -1; }
            m = malloc(sizeof(struct member_s));
            m->id = id;
            m->offset = i;
            m->type = ty;
            m->next = members[bt];
            members[bt] = m;
            i = i + (ty >= PTR ? sizeof(int) : tsize[ty]);
            i = (i + 3) & -4;
            next();
            if (tk == ',') next();
          }
          next();
        }
        next();
        tsize[bt] = i;
      }
    }
    while (tk != ';' && tk != '}') {
      ty = bt;
      while (tk == Mul) { next(); ty = ty + PTR; }
      if (tk != Id) { printf("%d: bad global declaration\n", line); return -1; }
      if (id->class) { printf("%d: duplicate global definition\n", line); return -1; }
      next();
      id->type = ty;
      if (tk == '(') { // function
        id->class = Fun;
        id->val = (int)(e + 1);
        next(); i = 0;
        while (tk != ')') {
          ty = INT;
          if (tk == Int) next();
          else if (tk == Char) { next(); ty = CHAR; }
          else if (tk == Struct) {
            next(); 
            if (tk != Id) { printf("%d: bad struct declaration\n", line); return -1; }
            ty = id->stype;
            next();
          }
          while (tk == Mul) { next(); ty = ty + PTR; }
          if (tk != Id) { printf("%d: bad parameter declaration\n", line); return -1; }
          if (id->class == Loc) { printf("%d: duplicate parameter definition\n", line); return -1; }
          id->hclass = id->class; id->class = Loc;
          id->htype  = id->type;  id->type = ty;
          id->hval   = id->val;   id->val = i++;
          next();
          if (tk == ',') next();
        }
        next();
        if (tk != '{') { printf("%d: bad function definition\n", line); return -1; }
        loc = ++i;
        next();
        while (tk == Int || tk == Char || tk == Struct) {
          if (tk == Int) bt = INT; else if (tk == Char) bt = CHAR; else {
            next(); 
            if (tk != Id) { printf("%d: bad struct declaration\n", line); return -1; }
            bt = id->stype;
          }
          next();
          while (tk != ';') {
            ty = bt;
            while (tk == Mul) { next(); ty = ty + PTR; }
            if (tk != Id) { printf("%d: bad local declaration\n", line); return -1; }
            if (id->class == Loc) { printf("%d: duplicate local definition\n", line); return -1; }
            id->hclass = id->class; id->class = Loc;
            id->htype  = id->type;  id->type = ty;
            id->hval   = id->val;   id->val = ++i;
            next();
            if (tk == ',') next();
          }
          next();
        }
        *++e = ENT; *++e = i - loc;
        while (tk != '}') stmt();
        *++e = LEV;
        id = sym; // unwind symbol table locals
        while (id->tk) {
          if (id->class == Loc) {
            id->class = id->hclass;
            id->type = id->htype;
            id->val = id->hval;
          }
          id = id + 1;
        }
      }
      else {
        id->class = Glo;
        id->val = (int)data;
        data = data + sizeof(int);
      }
      if (tk == ',') next();
    }
    next();
  }

  if (!(pc = (int *)idmain->val)) { printf("main() not defined\n"); return -1; }
  if (src) return 0;

  // setup stack
  sp = (int *)((int)sp + poolsz);
  *--sp = EXIT; // call exit if main returns
  *--sp = PSH; t = sp;
  *--sp = argc;
  *--sp = (int)argv;
  *--sp = (int)t;

  // run...
  cycle = 0;
  while (1) {
    i = *pc++; ++cycle;
    if (debug) {
      printf("%d> %.4s", cycle,
        &ops[i * 5]);
      if (i <= ADJ) printf(" %d\n", *pc); else printf("\n");
    }
    switch (i) {
    case LEA: a = (int)(bp + *pc++); break;                         // load local address
    case IMM: a = *pc++; break;                                     // load global address or immediate
    case JMP: pc = (int *)*pc; break;                               // jump
    case JSR: *--sp = (int)(pc + 1); pc = (int *)*pc; break;        // jump to subroutine
    case BZ:  pc = a ? pc + 1 : (int *)*pc; break;                  // branch if zero
    case BNZ: pc = a ? (int *)*pc : pc + 1; break;                  // branch if not zero
    case ENT: *--sp = (int)bp; bp = sp; sp = sp - *pc++; break;     // enter subroutine
    case ADJ: sp = sp + *pc++; break;                               // stack adjust
    case LEV: sp = bp; bp = (int *)*sp++; pc = (int *)*sp++; break; // leave subroutine
    case LI:  a = *(int *)a; break;                                 // load int
    case LC:  a = *(char *)a; break;                                // load char
    case SI:  *(int *)*sp++ = a; break;                             // store int
    case SC:  a = *(char *)*sp++ = a; break;                        // store char
    case PSH: *--sp = a; break;                                     // push

    case OR:  a = *sp++ |  a; break;
    case XOR: a = *sp++ ^  a; break;
    case AND: a = *sp++ &  a; break;
    case EQ:  a = *sp++ == a; break;
    case NE:  a = *sp++ != a; break;
    case LT:  a = *sp++ <  a; break;
    case GT:  a = *sp++ >  a; break;
    case LE:  a = *sp++ <= a; break;
    case GE:  a = *sp++ >= a; break;
    case SHL: a = *sp++ << a; break;
    case SHR: a = *sp++ >> a; break;
    case ADD: a = *sp++ +  a; break;
    case SUB: a = *sp++ -  a; break;
    case MUL: a = *sp++ *  a; break;
    case DIV: a = *sp++ /  a; break;
    case MOD: a = *sp++ %  a; break;

    case OPEN: a = open((char *)sp[1], *sp); break;
    case READ: a = read(sp[2], (char *)sp[1], *sp); break;
    case CLOS: a = close(*sp); break;
    case PRTF: t = sp + pc[1]; a = printf((char *)t[-1], t[-2], t[-3], t[-4], t[-5], t[-6]); break;
    case MALC: a = (int)malloc(*sp); break;
    case MSET: a = (int)memset((char *)sp[2], sp[1], *sp); break;
    case MCMP: a = memcmp((char *)sp[2], (char *)sp[1], *sp); break;
    case EXIT: printf("exit(%d) cycle = %d\n", *sp, cycle); return *sp;
    default: printf("unknown instruction = %d! cycle = %d\n", i, cycle); return -1;
    }
  }
}